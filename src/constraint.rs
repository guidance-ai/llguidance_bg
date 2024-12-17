use std::{
    panic,
    sync::{
        atomic::{AtomicI32, Ordering},
        Arc, Condvar, Mutex,
    },
    time::{Duration, Instant},
};

use anyhow::{anyhow, bail, ensure, Result};
use llguidance::{
    panic_utils,
    toktrie::{SimpleVob, TokenId},
    TokenParser,
};

struct ConstraintInner {
    parser: TokenParser,
    error: Option<String>,
    curr_mask_ticket: MaskTicketId,
    last_mask: Option<SimpleVob>,
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

struct ConstraintState {
    inner: Mutex<ConstraintInner>,
    condvar: Condvar,
    next_mask_ticket: AtomicI32,
}

// There's Constraint type already in llguidance library
// This one mostly just can do the mask computation in the background
pub struct BgConstraint {
    state: Arc<ConstraintState>,
}

impl BgConstraint {
    pub fn new(mut parser: TokenParser) -> Self {
        parser.start_without_prompt();
        BgConstraint {
            state: Arc::new(ConstraintState {
                inner: Mutex::new(ConstraintInner {
                    parser,
                    error: None,
                    curr_mask_ticket: MaskTicketId(0),
                    last_mask: None,
                }),
                condvar: Condvar::new(),
                next_mask_ticket: AtomicI32::new(1),
            }),
        }
    }

    fn clone_ref(&self) -> Self {
        BgConstraint {
            state: Arc::clone(&self.state),
        }
    }

    fn with_inner<T>(&self, f: impl FnOnce(&mut ConstraintInner) -> Result<T>) -> Result<T> {
        let mut inner = self.state.inner.lock().unwrap();
        inner.check_error()?;
        // We catch any panics here and transform them into regular errors.
        // They shouldn't happen, but if they do, we don't want to crash the whole program.
        let r = panic_utils::catch_unwind(panic::AssertUnwindSafe(|| f(&mut inner)));
        match r {
            Ok(r) => Ok(r),
            Err(e) => {
                if inner.error.is_none() {
                    inner.error = Some(e.to_string());
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

    pub fn start_compute_mask(&self, cb: impl MaskCallback + 'static) -> MaskTicketId {
        let ticket = MaskTicketId(self.state.next_mask_ticket.fetch_add(1, Ordering::Relaxed));
        // This is only so that we can call with_inner() from the closure.
        let self_copy = self.clone_ref();
        rayon::spawn(move || {
            let _ignore = self_copy.with_inner(|inner| {
                let mask = inner.parser.compute_mask()?;
                cb.mask_ready(&mask);
                ensure!(inner.curr_mask_ticket < ticket, "mask computation race");
                inner.curr_mask_ticket = ticket;
                inner.last_mask = Some(mask);
                self_copy.state.condvar.notify_all();
                Ok(())
            });
        });
        ticket
    }

    pub fn wait_mask_ready(&self, ticket: MaskTicketId, duration: Duration) -> Result<bool> {
        let mut inner = self.state.inner.lock().unwrap();

        if inner.curr_mask_ticket >= ticket {
            inner.check_error()?;
            return Ok(true);
        }

        let deadline = Instant::now() + duration;

        while inner.curr_mask_ticket < ticket {
            inner.check_error()?;

            let now = Instant::now();
            if now >= deadline {
                return Ok(false);
            }
            let timeout = deadline - now;

            let (guard, result) = self.state.condvar.wait_timeout(inner, timeout).unwrap();
            inner = guard;

            if result.timed_out() {
                return Ok(false);
            }
        }

        inner.check_error()?;
        Ok(true)
    }

    pub fn check_stop(&self) -> Result<bool> {
        self.with_inner(|inner| inner.parser.check_stop())
    }

    pub fn compute_ff_tokens(&self) -> Vec<TokenId> {
        self.with_inner(|inner| Ok(inner.parser.compute_ff_tokens()))
            .unwrap_or_else(|_| vec![])
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
        self.with_inner(|inner| {
            Ok(f(inner
                .last_mask
                .as_ref()
                .ok_or_else(|| anyhow!("no mask"))?))
        })
    }
}
