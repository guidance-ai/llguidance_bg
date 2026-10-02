use std::{
    panic,
    sync::{
        atomic::{AtomicI32, Ordering},
        Arc, Condvar, Mutex, OnceLock,
    },
    time::{Duration, Instant},
};

use anyhow::{anyhow, bail, ensure, Result};
use llguidance::{
    panic_utils,
    toktrie::{SimpleVob, TokenId},
    CancellationHandle, TokenParser,
};

struct ConstraintInner {
    parser: TokenParser,
    error: Option<String>,
}

struct TicketInner {
    last_started_mask_ticket: MaskTicketId,
    last_done_mask_ticket: MaskTicketId,
    last_mask: Option<SimpleVob>,
    error: Option<String>,
}

pub trait MaskCallback: Send {
    fn mask_ready(&self, mask: &SimpleVob);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct MaskTicketId(pub i32);

impl ConstraintInner {
    fn check_error(&self) -> Result<()> {
        if let Some(e) = self.error.as_ref() {
            bail!("{}", e);
        }
        Ok(())
    }
}

impl TicketInner {
    fn check_error(&self) -> Result<()> {
        if let Some(e) = self.error.as_ref() {
            bail!("{}", e);
        }
        Ok(())
    }
}

struct ConstraintState {
    inner: Mutex<ConstraintInner>,
    ticket: Mutex<TicketInner>,
    mask_done_cond: Condvar,
    cancellation: OnceLock<CancellationHandle>,
    next_mask_ticket: AtomicI32,
    thread_pool: Arc<rayon::ThreadPool>,
}

// There's Constraint type already in llguidance library
// This one mostly just can do the mask computation in the background
pub struct BgConstraint {
    state: Arc<ConstraintState>,
}

#[derive(Clone)]
pub struct BgCancellationHandle {
    cancellation: CancellationHandle,
}

impl BgCancellationHandle {
    pub fn cancel(&self) {
        self.cancellation.cancel();
    }

    pub fn is_cancelled(&self) -> bool {
        self.cancellation.is_cancelled()
    }
}

impl BgConstraint {
    pub fn new(thread_pool: Arc<rayon::ThreadPool>, parser: TokenParser) -> Self {
        let cancellation = OnceLock::new();
        if let Some(handle) = parser.cancellation_handle() {
            cancellation
                .set(handle)
                .expect("cancellation handle is initialized only once");
        }
        BgConstraint {
            state: Arc::new(ConstraintState {
                inner: Mutex::new(ConstraintInner {
                    parser,
                    error: None,
                }),
                ticket: Mutex::new(TicketInner {
                    last_started_mask_ticket: MaskTicketId(0),
                    last_done_mask_ticket: MaskTicketId(0),
                    last_mask: None,
                    error: None,
                }),
                mask_done_cond: Condvar::new(),
                cancellation,
                next_mask_ticket: AtomicI32::new(1),
                thread_pool,
            }),
        }
    }

    fn clone_ref(&self) -> Self {
        BgConstraint {
            state: Arc::clone(&self.state),
        }
    }

    pub fn deep_clone(&self) -> Result<Self> {
        let inner = self.state.inner.lock().unwrap();
        inner.check_error()?;
        if self
            .state
            .cancellation
            .get()
            .is_some_and(CancellationHandle::is_cancelled)
        {
            bail!(llguidance::Cancelled);
        }
        let parser = inner.parser.deep_clone();
        if parser
            .cancellation_handle()
            .is_some_and(|handle| handle.is_cancelled())
        {
            bail!(llguidance::Cancelled);
        }
        let thread_pool = Arc::clone(&self.state.thread_pool);
        Ok(Self::new(thread_pool, parser))
    }

    pub fn cancellation_handle(&self) -> Result<BgCancellationHandle> {
        let mut inner = self.state.inner.lock().unwrap();
        inner.check_error()?;
        let cancellation = inner.parser.enable_cancellation();
        let cancellation = self.state.cancellation.get_or_init(|| cancellation).clone();
        Ok(BgCancellationHandle { cancellation })
    }

    fn with_inner<T>(&self, f: impl FnOnce(&mut ConstraintInner) -> Result<T>) -> Result<T> {
        let mut inner = self.state.inner.lock().unwrap();
        inner.check_error()?;
        // We catch any panics here and transform them into regular errors.
        // They shouldn't happen, but if they do, we don't want to crash the whole program.
        let r = panic_utils::catch_unwind(panic::AssertUnwindSafe(|| {
            if self
                .state
                .cancellation
                .get()
                .is_some_and(CancellationHandle::is_cancelled)
            {
                bail!(llguidance::Cancelled);
            }
            f(&mut inner)
        }));
        match r {
            Ok(r) => Ok(r),
            Err(e) => {
                if inner.error.is_none() {
                    inner.error = Some(e.to_string());
                }
                {
                    // Propagate error to self.state.ticket as well, so wait_mask_ready can see it
                    let mut tk = self.state.ticket.lock().unwrap();
                    if tk.error.is_none() {
                        tk.error = Some(e.to_string());
                        self.state.mask_done_cond.notify_all();
                    }
                }
                Err(e)
            }
        }
    }

    fn with_ticket<T>(&self, f: impl FnOnce(&mut TicketInner) -> Result<T>) -> Result<T> {
        let mut tk = self.state.ticket.lock().unwrap();
        tk.check_error()?;
        // We catch any panics here and transform them into regular errors.
        // They shouldn't happen, but if they do, we don't want to crash the whole program.
        let r = panic_utils::catch_unwind(panic::AssertUnwindSafe(|| f(&mut tk)));
        match r {
            Ok(r) => Ok(r),
            Err(e) => {
                if tk.error.is_none() {
                    tk.error = Some(e.to_string());
                }
                Err(e)
            }
        }
    }

    pub fn consume_tokens(&self, tokens: &[TokenId]) -> Result<()> {
        self.with_inner(|inner| {
            for &t in tokens {
                let bt = inner.parser.consume_token(t)?;
                ensure!(bt == 0, "unexpected backtracking");
            }
            Ok(())
        })
    }

    pub fn rollback(&self, num_tokens: usize) -> Result<()> {
        self.with_inner(|inner| inner.parser.rollback(num_tokens))
    }

    pub fn start_compute_mask(&self, cb: impl MaskCallback + 'static) -> MaskTicketId {
        let ticket = MaskTicketId(self.state.next_mask_ticket.fetch_add(1, Ordering::Relaxed));
        let self_copy = self.clone_ref();
        self.state.thread_pool.spawn(move || {
            let _ignore = self_copy.with_inner(|inner| {
                if self_copy.with_ticket(|tk| {
                    if ticket <= tk.last_started_mask_ticket {
                        return Ok(true);
                    }
                    tk.last_started_mask_ticket = ticket;
                    Ok(false)
                })? {
                    return Ok(());
                }

                let mask = inner.parser.compute_mask()?;
                cb.mask_ready(&mask);

                let _ignore = self_copy.with_ticket(|tk| {
                    if ticket > tk.last_done_mask_ticket {
                        tk.last_done_mask_ticket = ticket;
                        tk.last_mask = Some(mask);
                        self_copy.state.mask_done_cond.notify_all();
                    }
                    Ok(())
                });
                Ok(())
            });
        });
        ticket
    }

    /// Start mask computation unless the constraint has already been cancelled.
    pub fn try_start_compute_mask(&self, cb: impl MaskCallback + 'static) -> Result<MaskTicketId> {
        if self
            .state
            .cancellation
            .get()
            .is_some_and(CancellationHandle::is_cancelled)
        {
            let inner = self.state.inner.lock().unwrap();
            inner.check_error()?;
            bail!(llguidance::Cancelled);
        }
        Ok(self.start_compute_mask(cb))
    }

    pub fn wait_mask_ready(&self, ticket: MaskTicketId, duration: Duration) -> Result<bool> {
        let mut tk = self.state.ticket.lock().unwrap();

        if tk.last_done_mask_ticket >= ticket {
            tk.check_error()?;
            return Ok(true);
        }

        let deadline = Instant::now() + duration;

        while tk.last_done_mask_ticket < ticket {
            tk.check_error()?;

            let now = Instant::now();
            if now >= deadline {
                return Ok(false);
            }
            let timeout = deadline - now;

            let (guard, result) = self.state.mask_done_cond.wait_timeout(tk, timeout).unwrap();
            tk = guard;

            if result.timed_out() {
                return Ok(false);
            }
        }

        tk.check_error()?;
        Ok(true)
    }

    pub fn check_stop(&self) -> Result<bool> {
        self.with_inner(|inner| inner.parser.check_stop())
    }

    /// Return forced tokens, or an empty list if the operation fails.
    ///
    /// Use [`Self::try_compute_ff_tokens`] to distinguish cancellation and other errors from an
    /// empty result.
    pub fn compute_ff_tokens(&self) -> Vec<TokenId> {
        self.try_compute_ff_tokens().unwrap_or_else(|_| vec![])
    }

    /// Return forced tokens while preserving cancellation and parser errors.
    pub fn try_compute_ff_tokens(&self) -> Result<Vec<TokenId>> {
        self.with_inner(|inner| {
            let tokens = inner.parser.compute_ff_tokens();
            if inner
                .parser
                .cancellation_handle()
                .is_some_and(|handle| handle.is_cancelled())
            {
                bail!(llguidance::Cancelled);
            }
            Ok(tokens)
        })
    }

    pub fn try_consume_tokens(&self, tokens: &[TokenId]) -> Result<usize> {
        self.with_inner(|inner| {
            for (idx, &t) in tokens.iter().enumerate() {
                if !inner.parser.validate_token(t)? {
                    return Ok(idx);
                }
                let bt = inner.parser.consume_token(t)?;
                ensure!(bt == 0, "unexpected backtracking");
            }
            Ok(tokens.len())
        })
    }

    pub fn validate_tokens(&self, tokens: &[TokenId]) -> Result<usize> {
        self.with_inner(|inner| inner.parser.validate_tokens_raw(tokens))
    }

    pub fn get_error(&self) -> Option<String> {
        self.state.inner.lock().unwrap().error.clone()
    }

    pub fn with_last_mask<T>(&self, f: impl FnOnce(&SimpleVob) -> T) -> Result<T> {
        self.with_ticket(|tk| {
            Ok(f(tk
                .last_mask
                .as_ref()
                .ok_or_else(|| anyhow!("no mask"))?))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use llguidance::{api::TopLevelGrammar, toktrie::ApproximateTokEnv, ParserFactory};
    use std::sync::{
        atomic::{AtomicBool, Ordering},
        mpsc,
    };
    use std::thread;

    fn constraint_with_pool(thread_pool: Arc<rayon::ThreadPool>) -> BgConstraint {
        let tok_env = ApproximateTokEnv::single_byte_env();
        let factory = ParserFactory::new_simple(&tok_env).unwrap();
        let mut parser = factory
            .create_parser(TopLevelGrammar::from_lark(r#"start: "a""#.to_string()))
            .unwrap();
        parser.start_without_prompt();
        BgConstraint::new(thread_pool, parser)
    }

    fn constraint() -> BgConstraint {
        constraint_with_pool(Arc::new(
            rayon::ThreadPoolBuilder::new()
                .num_threads(1)
                .build()
                .unwrap(),
        ))
    }

    struct RecordCallback(Arc<AtomicBool>);

    impl MaskCallback for RecordCallback {
        fn mask_ready(&self, _: &SimpleVob) {
            self.0.store(true, Ordering::Relaxed);
        }
    }

    struct BlockingCallback {
        entered: mpsc::Sender<()>,
        release: mpsc::Receiver<()>,
    }

    impl MaskCallback for BlockingCallback {
        fn mask_ready(&self, _: &SimpleVob) {
            self.entered.send(()).unwrap();
            self.release.recv().unwrap();
        }
    }

    #[test]
    fn polling_timeout_does_not_enable_cancellation() {
        let constraint = constraint();

        assert!(!constraint
            .wait_mask_ready(MaskTicketId(1), Duration::ZERO)
            .unwrap());
        assert!(constraint.state.cancellation.get().is_none());
    }

    #[test]
    fn cancellation_is_opt_in_and_permanent() {
        let constraint = constraint();
        let first = constraint.cancellation_handle().unwrap();
        let second = constraint.cancellation_handle().unwrap();

        first.cancel();

        assert!(second.is_cancelled());
        assert!(constraint
            .try_start_compute_mask(RecordCallback(Arc::new(AtomicBool::new(false))))
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
        assert!(constraint
            .try_compute_ff_tokens()
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
    }

    #[test]
    fn inherits_existing_parser_cancellation() {
        let tok_env = ApproximateTokEnv::single_byte_env();
        let factory = ParserFactory::new_simple(&tok_env).unwrap();
        let mut parser = factory
            .create_parser(TopLevelGrammar::from_lark(r#"start: "a""#.to_string()))
            .unwrap();
        parser.start_without_prompt();
        let handle = parser.enable_cancellation();
        let constraint = BgConstraint::new(
            Arc::new(
                rayon::ThreadPoolBuilder::new()
                    .num_threads(1)
                    .build()
                    .unwrap(),
            ),
            parser,
        );

        handle.cancel();

        assert!(constraint
            .try_start_compute_mask(RecordCallback(Arc::new(AtomicBool::new(false))))
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
        assert!(constraint
            .try_compute_ff_tokens()
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
    }

    #[test]
    fn queued_computation_observes_cancellation() {
        let thread_pool = Arc::new(
            rayon::ThreadPoolBuilder::new()
                .num_threads(1)
                .build()
                .unwrap(),
        );
        let constraint = constraint_with_pool(thread_pool.clone());
        let handle = constraint.cancellation_handle().unwrap();
        let callback_called = Arc::new(AtomicBool::new(false));
        let (blocker_started_tx, blocker_started_rx) = mpsc::channel();
        let (release_blocker_tx, release_blocker_rx) = mpsc::channel();
        let (drained_tx, drained_rx) = mpsc::channel();

        thread_pool.spawn(move || {
            blocker_started_tx.send(()).unwrap();
            release_blocker_rx.recv().unwrap();
        });
        blocker_started_rx.recv().unwrap();

        let ticket = constraint.start_compute_mask(RecordCallback(callback_called.clone()));
        handle.cancel();
        release_blocker_tx.send(()).unwrap();
        thread_pool.spawn(move || drained_tx.send(()).unwrap());
        drained_rx.recv().unwrap();

        assert!(constraint
            .wait_mask_ready(ticket, Duration::ZERO)
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
        assert!(!callback_called.load(Ordering::Relaxed));
    }

    #[test]
    fn cancelled_start_waits_for_active_callback() {
        let constraint = constraint();
        let handle = constraint.cancellation_handle().unwrap();
        let (callback_entered_tx, callback_entered_rx) = mpsc::channel();
        let (release_callback_tx, release_callback_rx) = mpsc::channel();
        let (start_attempted_tx, start_attempted_rx) = mpsc::channel();
        let (start_done_tx, start_done_rx) = mpsc::channel();

        constraint.start_compute_mask(BlockingCallback {
            entered: callback_entered_tx,
            release: release_callback_rx,
        });
        callback_entered_rx.recv().unwrap();
        handle.cancel();

        let start_constraint = constraint.clone_ref();
        let start_thread = thread::spawn(move || {
            start_attempted_tx.send(()).unwrap();
            let result = start_constraint
                .try_start_compute_mask(RecordCallback(Arc::new(AtomicBool::new(false))));
            start_done_tx.send(result).unwrap();
        });
        start_attempted_rx.recv().unwrap();
        assert!(start_done_rx
            .recv_timeout(Duration::from_millis(20))
            .is_err());

        release_callback_tx.send(()).unwrap();
        assert!(start_done_rx
            .recv_timeout(Duration::from_secs(1))
            .unwrap()
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
        start_thread.join().unwrap();
    }

    #[test]
    fn cancellation_handle_outlives_constraint() {
        let handle = {
            let constraint = constraint();
            constraint.cancellation_handle().unwrap()
        };

        handle.cancel();
        assert!(handle.is_cancelled());
    }

    #[test]
    fn deep_clone_snapshots_cancellation_independently() {
        let constraint = constraint();
        let handle = constraint.cancellation_handle().unwrap();
        let cloned = constraint.deep_clone().unwrap();

        handle.cancel();

        assert!(constraint.check_stop().is_err());
        assert!(cloned.check_stop().is_ok());
    }
}
