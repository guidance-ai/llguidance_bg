use std::{
    cell::Cell,
    collections::VecDeque,
    panic,
    sync::{
        atomic::{AtomicI32, Ordering},
        Arc, Condvar, Mutex, OnceLock, Weak,
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

struct MaskJob {
    ticket: MaskTicketId,
    callback: Box<dyn MaskCallback>,
}

fn drop_mask_job(job: MaskJob) -> bool {
    match panic::catch_unwind(panic::AssertUnwindSafe(|| drop(job))) {
        Ok(()) => true,
        Err(payload) => {
            std::mem::forget(payload);
            false
        }
    }
}

#[derive(Default)]
struct WorkQueue {
    jobs: VecDeque<MaskJob>,
    worker_running: bool,
}

#[derive(Default)]
struct CallbackState {
    active: bool,
}

thread_local! {
    static MASK_CALLBACK_DEPTH: Cell<usize> = const { Cell::new(0) };
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
    mask_done_cond: Condvar, // under ticket mutex
    callback_state: Mutex<CallbackState>,
    callback_done_cond: Condvar,
    work_queue: Mutex<WorkQueue>,
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
    state: Weak<ConstraintState>,
}

impl BgCancellationHandle {
    pub fn cancel(&self) -> bool {
        self.cancellation.cancel();
        let mut callback_quiesced = true;
        if let Some(state) = self.state.upgrade() {
            let queued_jobs = {
                let mut queue = state.work_queue.lock().unwrap();
                std::mem::take(&mut queue.jobs)
            };
            let mut ticket = state.ticket.lock().unwrap();
            if ticket.error.is_none() {
                ticket.error = Some(llguidance::Cancelled.to_string());
            }
            state.mask_done_cond.notify_all();
            drop(ticket);

            for job in queued_jobs {
                drop_mask_job(job);
            }

            let caller_is_callback = MASK_CALLBACK_DEPTH.with(|depth| depth.get() > 0);
            let mut callback_state = state.callback_state.lock().unwrap();
            if callback_state.active && caller_is_callback {
                callback_quiesced = false;
            } else {
                while callback_state.active {
                    callback_state = state.callback_done_cond.wait(callback_state).unwrap();
                }
            }
        }
        callback_quiesced
    }

    pub fn is_cancelled(&self) -> bool {
        self.cancellation.is_cancelled()
    }
}

struct ActiveCallbackGuard<'a> {
    state: &'a ConstraintState,
}

impl<'a> ActiveCallbackGuard<'a> {
    fn begin(state: &'a ConstraintState) -> Result<Self> {
        let mut callback_state = state.callback_state.lock().unwrap();
        if state
            .cancellation
            .get()
            .is_some_and(CancellationHandle::is_cancelled)
        {
            bail!(llguidance::Cancelled);
        }
        callback_state.active = true;
        MASK_CALLBACK_DEPTH.with(|depth| depth.set(depth.get() + 1));
        Ok(Self { state })
    }
}

impl Drop for ActiveCallbackGuard<'_> {
    fn drop(&mut self) {
        let mut callback_state = self.state.callback_state.lock().unwrap();
        callback_state.active = false;
        self.state.callback_done_cond.notify_all();
        MASK_CALLBACK_DEPTH.with(|depth| {
            let current = depth.get();
            debug_assert!(current > 0);
            depth.set(current.saturating_sub(1));
        });
    }
}

impl BgConstraint {
    pub fn new(thread_pool: Arc<rayon::ThreadPool>, parser: TokenParser) -> Self {
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
                callback_state: Mutex::new(CallbackState::default()),
                callback_done_cond: Condvar::new(),
                work_queue: Mutex::new(WorkQueue::default()),
                cancellation: OnceLock::new(),
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
        // This lock also makes cancellation visible before any later job can inspect the state.
        let cancellation = inner.parser.enable_cancellation();
        let cancellation = self.state.cancellation.get_or_init(|| cancellation).clone();
        Ok(BgCancellationHandle {
            cancellation,
            state: Arc::downgrade(&self.state),
        })
    }

    fn with_inner<T>(&self, f: impl FnOnce(&mut ConstraintInner) -> Result<T>) -> Result<T> {
        let mut inner = self.state.inner.lock().unwrap();
        inner.check_error()?;
        if self
            .state
            .cancellation
            .get()
            .is_some_and(CancellationHandle::is_cancelled)
        {
            bail!(llguidance::Cancelled);
        }
        // We catch any panics here and transform them into regular errors.
        // They shouldn't happen, but if they do, we don't want to crash the whole program.
        let r = panic_utils::catch_unwind(panic::AssertUnwindSafe(|| f(&mut inner)));
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
        let job = MaskJob {
            ticket,
            callback: Box::new(cb),
        };
        if self.state.cancellation.get().is_none() {
            let self_copy = self.clone_ref();
            self.state
                .thread_pool
                .spawn(move || self_copy.run_mask_job_safely(job));
            return ticket;
        }

        // Retain ownership of pending callbacks so cancellation can synchronously release them.
        let should_spawn = {
            let mut queue = self.state.work_queue.lock().unwrap();
            queue.jobs.push_back(job);
            if queue.worker_running {
                false
            } else {
                queue.worker_running = true;
                true
            }
        };
        if should_spawn {
            let self_copy = self.clone_ref();
            self.state
                .thread_pool
                .spawn_fifo(move || self_copy.run_next_mask_job());
        }
        ticket
    }

    pub(crate) fn try_start_compute_mask(
        &self,
        cb: impl MaskCallback + 'static,
    ) -> Result<MaskTicketId> {
        if self
            .state
            .cancellation
            .get()
            .is_some_and(CancellationHandle::is_cancelled)
        {
            bail!(llguidance::Cancelled);
        }
        Ok(self.start_compute_mask(cb))
    }

    fn run_next_mask_job(&self) {
        let job = {
            let mut queue = self.state.work_queue.lock().unwrap();
            match queue.jobs.pop_front() {
                Some(job) => job,
                None => {
                    queue.worker_running = false;
                    return;
                }
            }
        };
        self.run_mask_job_safely(job);

        let should_reschedule = {
            let mut queue = self.state.work_queue.lock().unwrap();
            if queue.jobs.is_empty() {
                queue.worker_running = false;
                false
            } else {
                true
            }
        };
        if should_reschedule {
            // Reschedule one job at a time so unrelated pool work gets a chance to run.
            rayon::yield_now();
            let self_copy = self.clone_ref();
            self.state
                .thread_pool
                .spawn_fifo(move || self_copy.run_next_mask_job());
        }
    }

    fn record_error(&self, error: &str) {
        {
            let mut inner = self.state.inner.lock().unwrap();
            if inner.error.is_none() {
                inner.error = Some(error.to_string());
            }
        }
        let mut ticket = self.state.ticket.lock().unwrap();
        if ticket.error.is_none() {
            ticket.error = Some(error.to_string());
        }
        self.state.mask_done_cond.notify_all();
    }

    fn run_mask_job_safely(&self, job: MaskJob) {
        if let Err(payload) =
            panic::catch_unwind(panic::AssertUnwindSafe(|| self.run_mask_job(&job)))
        {
            std::mem::forget(payload);
            self.record_error("mask job panicked");
        }
        if !drop_mask_job(job) {
            self.record_error("mask callback drop panicked");
        }
    }

    fn run_mask_job(&self, job: &MaskJob) {
        let result = self.with_inner(|inner| {
            if self.with_ticket(|tk| {
                if job.ticket <= tk.last_started_mask_ticket {
                    return Ok(true);
                }
                tk.last_started_mask_ticket = job.ticket;
                Ok(false)
            })? {
                return Ok(());
            }

            // A handle can only be installed while holding `inner`, so an unguarded callback
            // cannot race a cancellation request.
            let cancellable = self.state.cancellation.get().is_some();
            let mask = inner.parser.compute_mask()?;

            let _callback_guard = if cancellable {
                Some(ActiveCallbackGuard::begin(&self.state)?)
            } else {
                None
            };
            if let Err(payload) =
                panic::catch_unwind(panic::AssertUnwindSafe(|| job.callback.mask_ready(&mask)))
            {
                let detail = panic_utils::mk_panic_error(&payload);
                std::mem::forget(payload);
                bail!("mask callback panicked: {detail}");
            }
            if cancellable
                && self
                    .state
                    .cancellation
                    .get()
                    .is_some_and(CancellationHandle::is_cancelled)
            {
                bail!(llguidance::Cancelled);
            }

            let _ignore = self.with_ticket(|tk| {
                if job.ticket > tk.last_done_mask_ticket {
                    tk.last_done_mask_ticket = job.ticket;
                    tk.last_mask = Some(mask);
                    self.state.mask_done_cond.notify_all();
                }
                Ok(())
            });
            Ok(())
        });
        if let Err(error) = result {
            let _ignore = self.with_ticket(|tk| {
                if tk.error.is_none() {
                    tk.error = Some(error.to_string());
                }
                self.state.mask_done_cond.notify_all();
                Ok(())
            });
        }
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

    pub fn compute_ff_tokens(&self) -> Vec<TokenId> {
        self.try_compute_ff_tokens().unwrap_or_else(|_| vec![])
    }

    pub(crate) fn try_compute_ff_tokens(&self) -> Result<Vec<TokenId>> {
        self.with_inner(|inner| Ok(inner.parser.compute_ff_tokens()))
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
    use llguidance::{
        api::TopLevelGrammar,
        earley::SlicedBiasComputer,
        toktrie::{ApproximateTokEnv, InferenceCapabilities},
        ParserFactory,
    };
    use std::{
        sync::{
            atomic::{AtomicBool, AtomicUsize, Ordering},
            mpsc, Barrier,
        },
        thread,
    };

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

    #[test]
    fn polling_timeout_does_not_cancel_constraint() {
        let constraint = constraint();

        assert!(!constraint
            .wait_mask_ready(MaskTicketId(1), Duration::ZERO)
            .unwrap());
        assert!(constraint.state.cancellation.get().is_none());
    }

    #[test]
    fn cancellation_is_opt_in() {
        let constraint = constraint();

        let first = constraint.cancellation_handle().unwrap();
        let second = constraint.cancellation_handle().unwrap();
        first.cancel();
        assert!(second.is_cancelled());
        assert!(constraint
            .wait_mask_ready(MaskTicketId(1), Duration::ZERO)
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
    }

    #[test]
    fn cancelled_constraint_rejects_new_mask_job() {
        let constraint = constraint();
        let handle = constraint.cancellation_handle().unwrap();
        handle.cancel();

        assert!(constraint
            .try_start_compute_mask(RecordCallback(Arc::new(AtomicBool::new(false))))
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
    }

    #[test]
    fn cancelled_constraint_reports_forced_token_error() {
        let constraint = constraint();
        let handle = constraint.cancellation_handle().unwrap();
        handle.cancel();

        assert!(constraint
            .try_compute_ff_tokens()
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
    }

    #[test]
    fn requesting_handle_preserves_parser_semantics() {
        let thread_pool = Arc::new(
            rayon::ThreadPoolBuilder::new()
                .num_threads(2)
                .build()
                .unwrap(),
        );
        let plain = constraint_with_pool(thread_pool.clone());
        let cancellable = constraint_with_pool(thread_pool);
        let _handle = cancellable.cancellation_handle().unwrap();

        plain.consume_tokens(&[b'a' as TokenId]).unwrap();
        cancellable.consume_tokens(&[b'a' as TokenId]).unwrap();

        let plain_callback = Arc::new(AtomicBool::new(false));
        let cancellable_callback = Arc::new(AtomicBool::new(false));
        let plain_ticket = plain.start_compute_mask(RecordCallback(plain_callback.clone()));
        let cancellable_ticket =
            cancellable.start_compute_mask(RecordCallback(cancellable_callback.clone()));

        assert!(plain
            .wait_mask_ready(plain_ticket, Duration::from_secs(1))
            .unwrap());
        assert!(cancellable
            .wait_mask_ready(cancellable_ticket, Duration::from_secs(1))
            .unwrap());
        assert!(plain_callback.load(Ordering::Relaxed));
        assert!(cancellable_callback.load(Ordering::Relaxed));
        assert_eq!(
            plain.with_last_mask(Clone::clone).unwrap(),
            cancellable.with_last_mask(Clone::clone).unwrap()
        );
        assert_eq!(
            plain.check_stop().unwrap(),
            cancellable.check_stop().unwrap()
        );
    }

    #[test]
    fn cancellation_supports_backtracking_parser() {
        let tok_env = ApproximateTokEnv::single_byte_env();
        let factory = ParserFactory::new(
            &tok_env,
            InferenceCapabilities {
                ff_tokens: true,
                backtrack: true,
                ..InferenceCapabilities::default()
            },
            &SlicedBiasComputer::general_slices(),
        )
        .unwrap();
        let mut parser = factory
            .create_parser(TopLevelGrammar::from_lark(r#"start: "a""#.to_string()))
            .unwrap();
        parser.start_without_prompt();
        let thread_pool = Arc::new(
            rayon::ThreadPoolBuilder::new()
                .num_threads(1)
                .build()
                .unwrap(),
        );
        let constraint = BgConstraint::new(thread_pool, parser);

        let handle = constraint.cancellation_handle().unwrap();
        assert!(constraint
            .with_inner(|inner| inner.parser.compute_mask().map(|_| ()))
            .is_ok());
        handle.cancel();
        assert!(constraint
            .with_inner(|inner| inner.parser.compute_mask().map(|_| ()))
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
    }

    #[test]
    fn concurrent_clone_never_returns_cancelled_constraint() {
        for _ in 0..100 {
            let constraint = constraint();
            let handle = constraint.cancellation_handle().unwrap();
            let barrier = Arc::new(Barrier::new(2));
            let cancel_barrier = barrier.clone();
            let cancel_thread = thread::spawn(move || {
                cancel_barrier.wait();
                handle.cancel();
            });

            barrier.wait();
            let cloned = constraint.deep_clone();
            cancel_thread.join().unwrap();

            if let Ok(cloned) = cloned {
                assert!(cloned.check_stop().is_ok());
            }
        }
    }

    #[test]
    fn cancellation_handle_wakes_waiters() {
        let constraint = constraint();
        let waiter = constraint.clone_ref();
        let handle = constraint.cancellation_handle().unwrap();
        let (started_tx, started_rx) = mpsc::channel();

        let waiter_thread = thread::spawn(move || {
            started_tx.send(()).unwrap();
            waiter.wait_mask_ready(MaskTicketId(1), Duration::from_secs(30))
        });

        started_rx.recv().unwrap();
        handle.cancel();

        assert!(waiter_thread
            .join()
            .unwrap()
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
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

    struct SignalCallback(mpsc::Sender<()>);

    impl MaskCallback for SignalCallback {
        fn mask_ready(&self, _: &SimpleVob) {
            self.0.send(()).unwrap();
        }
    }

    struct CancelCallback {
        handle: BgCancellationHandle,
        done: mpsc::Sender<bool>,
    }

    impl MaskCallback for CancelCallback {
        fn mask_ready(&self, _: &SimpleVob) {
            self.done.send(self.handle.cancel()).unwrap();
        }
    }

    struct CrossCancelCallback {
        barrier: Arc<Barrier>,
        target: BgCancellationHandle,
        result: mpsc::Sender<bool>,
    }

    impl MaskCallback for CrossCancelCallback {
        fn mask_ready(&self, _: &SimpleVob) {
            self.barrier.wait();
            self.result.send(self.target.cancel()).unwrap();
        }
    }

    struct DropCancelCallback {
        handle: BgCancellationHandle,
        result: mpsc::Sender<bool>,
    }

    impl MaskCallback for DropCancelCallback {
        fn mask_ready(&self, _: &SimpleVob) {}
    }

    impl Drop for DropCancelCallback {
        fn drop(&mut self) {
            self.result.send(self.handle.cancel()).unwrap();
        }
    }

    struct PanicDropCallback;

    impl MaskCallback for PanicDropCallback {
        fn mask_ready(&self, _: &SimpleVob) {}
    }

    impl Drop for PanicDropCallback {
        fn drop(&mut self) {
            panic!("callback drop panic");
        }
    }

    struct PanicOnDropPayload;

    impl Drop for PanicOnDropPayload {
        fn drop(&mut self) {
            panic!("panic payload drop");
        }
    }

    struct PanickingPayloadDropCallback(Arc<AtomicUsize>);

    impl MaskCallback for PanickingPayloadDropCallback {
        fn mask_ready(&self, _: &SimpleVob) {}
    }

    impl Drop for PanickingPayloadDropCallback {
        fn drop(&mut self) {
            self.0.fetch_add(1, Ordering::SeqCst);
            panic::panic_any(PanicOnDropPayload);
        }
    }

    struct PanickingPayloadCallback;

    impl MaskCallback for PanickingPayloadCallback {
        fn mask_ready(&self, _: &SimpleVob) {
            panic::panic_any(PanicOnDropPayload);
        }
    }

    struct PanickingCallback;

    impl MaskCallback for PanickingCallback {
        fn mask_ready(&self, _: &SimpleVob) {
            panic!("callback panic detail");
        }
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
    fn cancellation_waits_for_active_callback() {
        let constraint = constraint();
        let handle = constraint.cancellation_handle().unwrap();
        let (callback_entered_tx, callback_entered_rx) = mpsc::channel();
        let (release_callback_tx, release_callback_rx) = mpsc::channel();
        let (waiter_started_tx, waiter_started_rx) = mpsc::channel();
        let (waiter_done_tx, waiter_done_rx) = mpsc::channel();
        let (release_waiter_tx, release_waiter_rx) = mpsc::channel();
        let (cancel_started_tx, cancel_started_rx) = mpsc::channel();
        let (cancel_done_tx, cancel_done_rx) = mpsc::channel();

        let ticket = constraint.start_compute_mask(BlockingCallback {
            entered: callback_entered_tx,
            release: release_callback_rx,
        });
        callback_entered_rx.recv().unwrap();

        let waiter = constraint.clone_ref();
        let waiter_thread = thread::spawn(move || {
            waiter_started_tx.send(()).unwrap();
            let result = waiter.wait_mask_ready(ticket, Duration::from_secs(30));
            waiter_done_tx.send(result).unwrap();
            release_waiter_rx.recv().unwrap();
            release_callback_tx.send(()).unwrap();
        });
        waiter_started_rx.recv().unwrap();

        let cancel_thread = thread::spawn(move || {
            cancel_started_tx.send(()).unwrap();
            assert!(handle.cancel());
            cancel_done_tx.send(()).unwrap();
        });
        cancel_started_rx.recv().unwrap();
        assert!(cancel_done_rx
            .recv_timeout(Duration::from_millis(20))
            .is_err());

        assert!(waiter_done_rx
            .recv_timeout(Duration::from_secs(1))
            .unwrap()
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
        release_waiter_tx.send(()).unwrap();
        cancel_done_rx.recv_timeout(Duration::from_secs(1)).unwrap();
        waiter_thread.join().unwrap();
        cancel_thread.join().unwrap();
        assert!(constraint
            .wait_mask_ready(ticket, Duration::from_secs(1))
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
    }

    #[test]
    fn callback_can_cancel_its_own_constraint() {
        let constraint = constraint();
        let handle = constraint.cancellation_handle().unwrap();
        let (callback_done_tx, callback_done_rx) = mpsc::channel();

        let ticket = constraint.start_compute_mask(CancelCallback {
            handle,
            done: callback_done_tx,
        });

        assert!(!callback_done_rx
            .recv_timeout(Duration::from_secs(1))
            .unwrap());
        assert!(constraint
            .wait_mask_ready(ticket, Duration::from_secs(1))
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
    }

    #[test]
    fn callbacks_can_cross_cancel_without_deadlock() {
        let thread_pool = Arc::new(
            rayon::ThreadPoolBuilder::new()
                .num_threads(2)
                .build()
                .unwrap(),
        );
        let first = constraint_with_pool(thread_pool.clone());
        let second = constraint_with_pool(thread_pool);
        let first_handle = first.cancellation_handle().unwrap();
        let second_handle = second.cancellation_handle().unwrap();
        let barrier = Arc::new(Barrier::new(2));
        let (result_tx, result_rx) = mpsc::channel();

        let first_ticket = first.start_compute_mask(CrossCancelCallback {
            barrier: barrier.clone(),
            target: second_handle,
            result: result_tx.clone(),
        });
        let second_ticket = second.start_compute_mask(CrossCancelCallback {
            barrier,
            target: first_handle,
            result: result_tx,
        });

        let first_result = result_rx.recv_timeout(Duration::from_secs(1)).unwrap();
        let second_result = result_rx.recv_timeout(Duration::from_secs(1)).unwrap();
        assert!(!first_result || !second_result);
        assert!(first
            .wait_mask_ready(first_ticket, Duration::from_secs(1))
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
        assert!(second
            .wait_mask_ready(second_ticket, Duration::from_secs(1))
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
    }

    #[test]
    fn queued_callback_drop_can_cancel_without_deadlock() {
        let thread_pool = Arc::new(
            rayon::ThreadPoolBuilder::new()
                .num_threads(1)
                .build()
                .unwrap(),
        );
        let constraint = constraint_with_pool(thread_pool.clone());
        let handle = constraint.cancellation_handle().unwrap();
        let (blocker_started_tx, blocker_started_rx) = mpsc::channel();
        let (release_blocker_tx, release_blocker_rx) = mpsc::channel();
        let (drop_result_tx, drop_result_rx) = mpsc::channel();
        let (cancel_result_tx, cancel_result_rx) = mpsc::channel();

        thread_pool.spawn(move || {
            blocker_started_tx.send(()).unwrap();
            release_blocker_rx.recv().unwrap();
        });
        blocker_started_rx.recv().unwrap();
        constraint.start_compute_mask(DropCancelCallback {
            handle: handle.clone(),
            result: drop_result_tx,
        });

        let cancel_thread = thread::spawn(move || {
            cancel_result_tx.send(handle.cancel()).unwrap();
        });
        assert!(drop_result_rx.recv_timeout(Duration::from_secs(1)).unwrap());
        assert!(cancel_result_rx
            .recv_timeout(Duration::from_secs(1))
            .unwrap());
        cancel_thread.join().unwrap();
        release_blocker_tx.send(()).unwrap();
    }

    #[test]
    fn panicking_queued_callback_payloads_are_isolated() {
        let thread_pool = Arc::new(
            rayon::ThreadPoolBuilder::new()
                .num_threads(1)
                .build()
                .unwrap(),
        );
        let constraint = constraint_with_pool(thread_pool.clone());
        let handle = constraint.cancellation_handle().unwrap();
        let (blocker_started_tx, blocker_started_rx) = mpsc::channel();
        let (release_blocker_tx, release_blocker_rx) = mpsc::channel();

        thread_pool.spawn(move || {
            blocker_started_tx.send(()).unwrap();
            release_blocker_rx.recv().unwrap();
        });
        blocker_started_rx.recv().unwrap();
        let drops = Arc::new(AtomicUsize::new(0));
        let first_ticket =
            constraint.start_compute_mask(PanickingPayloadDropCallback(drops.clone()));
        let second_ticket =
            constraint.start_compute_mask(PanickingPayloadDropCallback(drops.clone()));

        assert!(handle.cancel());
        assert_eq!(drops.load(Ordering::SeqCst), 2);
        assert!(constraint
            .wait_mask_ready(first_ticket, Duration::ZERO)
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
        assert!(constraint
            .wait_mask_ready(second_ticket, Duration::ZERO)
            .unwrap_err()
            .to_string()
            .contains("operation cancelled"));
        release_blocker_tx.send(()).unwrap();
    }

    #[test]
    fn panicking_payload_does_not_escape_worker() {
        let constraint = constraint();
        let _cancellation = constraint.cancellation_handle().unwrap();
        let first_ticket = constraint.start_compute_mask(PanickingPayloadCallback);
        let second_ticket =
            constraint.start_compute_mask(RecordCallback(Arc::new(AtomicBool::new(false))));

        assert!(constraint
            .wait_mask_ready(first_ticket, Duration::from_secs(1))
            .is_err());
        assert!(constraint
            .wait_mask_ready(second_ticket, Duration::from_secs(1))
            .is_err());

        let deadline = Instant::now() + Duration::from_secs(1);
        loop {
            let queue = constraint.state.work_queue.lock().unwrap();
            if !queue.worker_running && queue.jobs.is_empty() {
                break;
            }
            drop(queue);
            assert!(Instant::now() < deadline, "worker queue remained wedged");
            thread::yield_now();
        }
    }

    #[test]
    fn panicking_payload_does_not_escape_direct_worker() {
        let constraint = constraint();
        let ticket = constraint.start_compute_mask(PanickingPayloadCallback);

        assert!(constraint
            .wait_mask_ready(ticket, Duration::from_secs(1))
            .is_err());

        let (done_tx, done_rx) = mpsc::channel();
        constraint
            .state
            .thread_pool
            .spawn(move || done_tx.send(()).unwrap());
        done_rx.recv_timeout(Duration::from_secs(1)).unwrap();
    }

    #[test]
    fn callback_panic_preserves_diagnostic() {
        let constraint = constraint();
        let ticket = constraint.start_compute_mask(PanickingCallback);

        assert!(constraint
            .wait_mask_ready(ticket, Duration::from_secs(1))
            .unwrap_err()
            .to_string()
            .contains("callback panic detail"));
    }

    #[test]
    fn panicking_active_callback_drop_does_not_wedge_worker() {
        let constraint = constraint();
        let _cancellation = constraint.cancellation_handle().unwrap();
        let first_ticket = constraint.start_compute_mask(PanicDropCallback);
        let second_ticket =
            constraint.start_compute_mask(RecordCallback(Arc::new(AtomicBool::new(false))));

        assert!(constraint
            .wait_mask_ready(second_ticket, Duration::from_secs(1))
            .is_err());
        assert!(constraint
            .wait_mask_ready(first_ticket, Duration::from_secs(1))
            .is_err());

        let deadline = Instant::now() + Duration::from_secs(1);
        loop {
            let queue = constraint.state.work_queue.lock().unwrap();
            if !queue.worker_running && queue.jobs.is_empty() {
                break;
            }
            drop(queue);
            assert!(Instant::now() < deadline, "worker queue remained wedged");
            thread::yield_now();
        }
    }

    #[test]
    fn one_constraint_does_not_occupy_other_pool_workers() {
        let thread_pool = Arc::new(
            rayon::ThreadPoolBuilder::new()
                .num_threads(2)
                .build()
                .unwrap(),
        );
        let blocked_constraint = constraint_with_pool(thread_pool.clone());
        let independent_constraint = constraint_with_pool(thread_pool);
        let _cancellation = blocked_constraint.cancellation_handle().unwrap();
        let (callback_entered_tx, callback_entered_rx) = mpsc::channel();
        let (release_callback_tx, release_callback_rx) = mpsc::channel();
        let (independent_done_tx, independent_done_rx) = mpsc::channel();

        blocked_constraint.start_compute_mask(BlockingCallback {
            entered: callback_entered_tx,
            release: release_callback_rx,
        });
        callback_entered_rx.recv().unwrap();

        let mut last_blocked_ticket = MaskTicketId(0);
        for _ in 0..8 {
            last_blocked_ticket = blocked_constraint
                .start_compute_mask(RecordCallback(Arc::new(AtomicBool::new(false))));
        }
        let independent_ticket =
            independent_constraint.start_compute_mask(SignalCallback(independent_done_tx));

        independent_done_rx
            .recv_timeout(Duration::from_secs(1))
            .unwrap();
        assert!(independent_constraint
            .wait_mask_ready(independent_ticket, Duration::from_secs(1))
            .unwrap());

        release_callback_tx.send(()).unwrap();
        assert!(blocked_constraint
            .wait_mask_ready(last_blocked_ticket, Duration::from_secs(1))
            .unwrap());
    }
}
