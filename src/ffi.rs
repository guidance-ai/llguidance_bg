use anyhow::Result;
use llguidance::{
    api::TopLevelGrammar,
    earley::SlicedBiasComputer,
    ffi::{LlgConstraintInit, LlgToken, LlgTokenizer},
    toktrie::SimpleVob,
    ParserFactory,
};
use std::{
    ffi::{c_char, c_void},
    sync::Arc,
};

use crate::{constraint::MaskTicketId, BgConstraint, MaskCallback};

pub struct BllgConstraint {
    local_error: Option<String>,
    ff_tokens: Vec<LlgToken>,
    constraint: Option<BgConstraint>,
}

pub struct BllgConstraintMgr {
    factory: ParserFactory,
    init: LlgConstraintInit,
    tokenizer: LlgTokenizer,
    thread_pool: Arc<rayon::ThreadPool>,
}

pub type BllgMaskReady = Option<
    extern "C" fn(user_data: *const c_void, mask_ptr: *const u32, mask_byte_len: usize) -> usize,
>;

impl BllgConstraint {
    pub fn from_bg_constraint(c: Result<BgConstraint>) -> *mut BllgConstraint {
        let mut res = BllgConstraint {
            local_error: None,
            constraint: None,
            ff_tokens: vec![],
        };

        match c {
            Ok(constraint) => res.constraint = Some(constraint),
            Err(e) => res.set_error(&e.to_string()),
        };

        Box::into_raw(Box::new(res))
    }

    fn save_error<T>(&mut self, e: Result<T>) -> Option<T> {
        match e {
            Ok(r) => Some(r),
            Err(e) => {
                self.set_error(&e.to_string());
                None
            }
        }
    }

    fn get_error(&self) -> *const c_char {
        match &self.local_error {
            Some(e) => e.as_ptr() as *const c_char,
            None => std::ptr::null(),
        }
    }

    fn get_error_code(&self) -> i32 {
        if self.local_error.is_some() {
            -1
        } else {
            0
        }
    }

    pub(crate) fn set_error(&mut self, e: &str) {
        self.constraint = None;
        self.local_error = Some(format!("{e}\0"));
    }
}

impl BllgConstraintMgr {
    fn new_constraint(&self, grammar_json: &[u8]) -> Result<BgConstraint> {
        let grammar: TopLevelGrammar = serde_json::from_slice(grammar_json)
            .map_err(|e| anyhow::anyhow!("Invalid JSON in grammar_json: {e}"))?;
        let parser = self
            .init
            .build_parser_from_factory(&self.factory, grammar)?;
        Ok(BgConstraint::new(self.thread_pool.clone(), parser))
    }
}

/// Create a new constraint manager with given parameters.
/// # Safety
/// Should be called only from C code.
#[no_mangle]
pub unsafe extern "C" fn bllg_new_constraint_mgr(
    init: &LlgConstraintInit,
    num_threads: usize,
    slicesv: *const *const c_char,
    error_string: *mut c_char,
    error_string_len: usize,
) -> *mut BllgConstraintMgr {
    let mut slices = vec![];
    if slicesv.is_null() {
        // default slice
        slices = SlicedBiasComputer::general_slices();
    } else {
        let mut idx = 0;
        loop {
            let slice = unsafe { *slicesv.add(idx) };
            if slice.is_null() {
                break;
            }
            let slice = unsafe { std::ffi::CStr::from_ptr(slice) };
            slices.push(slice.to_str().unwrap().to_string());
            idx += 1;
        }
    }

    let tokenizer = unsafe { &(*init.tokenizer) };

    match BllgConstraintMgr::new(tokenizer, num_threads, init, slices) {
        Ok(r) => {
            // we keep track of the tokenizer locally
            let mut r = Box::new(r);
            r.init.tokenizer = &r.tokenizer;
            Box::into_raw(r)
        }
        Err(e) => {
            save_error(e.to_string(), error_string, error_string_len);
            std::ptr::null_mut()
        }
    }
}

impl BllgConstraintMgr {
    fn new(
        tokenizer: &LlgTokenizer,
        num_threads: usize,
        init: &LlgConstraintInit,
        slices: Vec<String>,
    ) -> Result<Self> {
        let num_threads = if num_threads == 0 {
            std::cmp::min(
                20,
                std::thread::available_parallelism()
                    .map(|p| p.get())
                    .unwrap_or(1),
            )
        } else {
            num_threads
        };
        let thread_pool = rayon::ThreadPoolBuilder::new()
            .num_threads(num_threads)
            .build()
            .map_err(|e| {
                anyhow::anyhow!("Failed to create thread pool with {num_threads} threads: {e}")
            })?;

        Ok(BllgConstraintMgr {
            tokenizer: tokenizer.clone(),
            factory: ParserFactory::new(
                &tokenizer.token_env,
                init.inference_capabilities(),
                &slices,
            )?,
            init: init.clone(),
            thread_pool: Arc::new(thread_pool),
        })
    }
}

/// Destroy the constraint manager.
/// # Safety
/// Should be called only from C code.
#[no_mangle]
pub unsafe extern "C" fn bllg_free_constraint_mgr(mgr: &mut BllgConstraintMgr) {
    unsafe {
        drop(Box::from_raw(mgr));
    }
}

/// Create a new constraint from a grammar JSON string
/// Always returns a non-null value. Call bllg_get_error() on the result to check for errors.
/// # Safety
/// Should be called only from C code.
#[no_mangle]
pub unsafe extern "C" fn bllg_new_constraint(
    mgr: &BllgConstraintMgr,
    grammar_data: *const u8,
    grammar_size: usize,
) -> *mut BllgConstraint {
    let grammar = unsafe { std::slice::from_raw_parts(grammar_data, grammar_size) };
    BllgConstraint::from_bg_constraint(mgr.new_constraint(grammar))
}

/// Get the error message from the constraint or null if there is no error.
/// After it returns a non-null value, it will always return it until the constraint is freed
/// using bllg_free_constraint() (at which point the pointer will be invalid).
#[no_mangle]
pub extern "C" fn bllg_get_error(cc: &BllgConstraint) -> *const c_char {
    cc.get_error()
}

/// Check if constraint is stopped (cannot be extended further).
/// Returns true also if the constraint is in an error state.
#[no_mangle]
pub extern "C" fn bllg_check_stop(cc: &BllgConstraint) -> bool {
    cc.constraint
        .as_ref().is_none_or(|c| c.check_stop().unwrap_or(true))
}

fn save_error(e: String, error_string: *mut c_char, error_string_len: usize) {
    if error_string_len > 1 {
        let e = e.as_bytes();
        let to_copy = std::cmp::min(e.len(), error_string_len - 1);
        unsafe {
            std::ptr::copy_nonoverlapping(e.as_ptr(), error_string as *mut u8, to_copy);
            std::ptr::write(error_string.add(to_copy) as *mut u8, 0);
        }
    }
}

/// Set maximum number of threads (cores) to use for mask computation.
/// Has to be called before starting mask computation, otherwise defaults will be used (usually all cores).
/// Returns 0 on success and -1 on error (which will be a string copied into error_string).
/// # Safety
/// Should be called only from C code.
#[no_mangle]
pub unsafe extern "C" fn bllg_set_num_threads(
    max_threads: usize,
    error_string: *mut c_char,
    error_string_len: usize,
) -> i32 {
    let r = rayon::ThreadPoolBuilder::new()
        .num_threads(max_threads)
        .build_global();
    match r {
        Ok(_) => 0,
        Err(e) => {
            // we only call that on error; if we called before .build_global()
            // it might build the default thread pool
            if rayon::current_num_threads() == max_threads {
                return 0;
            }
            if error_string_len > 1 {
                let e = e.to_string();
                let e = e.as_bytes();
                let to_copy = std::cmp::min(e.len(), error_string_len - 1);
                unsafe {
                    std::ptr::copy_nonoverlapping(e.as_ptr(), error_string as *mut u8, to_copy);
                    std::ptr::write(error_string.add(to_copy) as *mut u8, 0);
                }
            }
            -1
        }
    }
}

struct CMaskReady {
    cb: BllgMaskReady,
    cb_userdata: *const c_void,
}

unsafe impl Send for CMaskReady {}
impl MaskCallback for CMaskReady {
    fn mask_ready(&self, mask: &SimpleVob) {
        if let Some(cb) = self.cb {
            let mask_len = mask.len();
            let mask_ptr = mask.as_ptr();
            cb(self.cb_userdata, mask_ptr, mask_len * 4);
        }
    }
}

/// Start mask computation process.
/// The function will be called with mask data when ready.
/// Returns a positive "mask ticket" on success and -1 on error (use bllg_get_error() to get the exact error).
#[no_mangle]
pub extern "C" fn bllg_start_compute_mask(
    cc: &mut BllgConstraint,
    mask_ready: BllgMaskReady,
    mask_ready_userdata: *const c_void,
) -> i32 {
    if let Some(constraint) = &mut cc.constraint {
        let cb = CMaskReady {
            cb: mask_ready,
            cb_userdata: mask_ready_userdata,
        };
        let r = constraint.start_compute_mask(cb);
        return r.0;
    }
    cc.get_error_code()
}

/// Commit the token(s) sampled.
/// If any of the tokens is invalid (according to constraint),
/// the constraint will enter error state and never leave it.
/// Returns 0 on success and -1 on error (use bllg_get_error() to get the exact error).
/// # Safety
/// Should be called only from C code.
#[no_mangle]
pub unsafe extern "C" fn bllg_consume_tokens(
    cc: &mut BllgConstraint,
    tokens: *const LlgToken,
    tokens_len: usize,
) -> i32 {
    if let Some(constraint) = &mut cc.constraint {
        let tokens = unsafe { std::slice::from_raw_parts(tokens, tokens_len) };
        let r = constraint.consume_tokens(tokens);
        cc.save_error(r);
    }
    cc.get_error_code()
}

/// Commit as many of the tokens as the mask allows.
/// Returns the number of tokens consumed, or -1 on error (use bllg_get_error() to get the exact error).
/// The returned number can be 0.
/// # Safety
/// Should be called only from C code.
#[no_mangle]
pub unsafe extern "C" fn bllg_try_consume_tokens(
    cc: &mut BllgConstraint,
    tokens: *const LlgToken,
    tokens_len: usize,
) -> i32 {
    if let Some(constraint) = &mut cc.constraint {
        let tokens = unsafe { std::slice::from_raw_parts(tokens, tokens_len) };
        let r = constraint.try_consume_tokens(tokens);
        match cc.save_error(r) {
            Some(r) => r as i32,
            None => -1,
        }
    } else {
        cc.get_error_code()
    }
}

/// Undo the last N tokens consumed.
/// This will not get the constraint out of an error state,
/// but will get it out of the stop state.
/// EOS tokens are best not consumed and not rolled back.
/// Returns 0 on success and -1 on error (use bllg_get_error() to get the exact error).
#[no_mangle]
pub extern "C" fn bllg_rollback_tokens(cc: &mut BllgConstraint, num_tokens: usize) -> i32 {
    if let Some(constraint) = &mut cc.constraint {
        let r = constraint.rollback(num_tokens);
        cc.save_error(r);
    }
    cc.get_error_code()
}

/// Check how many of the tokens can be consumed, without actually consuming them.
/// Returns the number of tokens consumed, or -1 on error (use bllg_get_error() to get the exact error).
/// The returned number can be 0.
/// # Safety
/// Should be called only from C code.
#[no_mangle]
pub unsafe extern "C" fn bllg_validate_tokens(
    cc: &mut BllgConstraint,
    tokens: *const LlgToken,
    tokens_len: usize,
) -> i32 {
    if let Some(constraint) = &mut cc.constraint {
        let tokens = unsafe { std::slice::from_raw_parts(tokens, tokens_len) };
        let r = constraint.validate_tokens(tokens);
        match cc.save_error(r) {
            Some(r) => r as i32,
            None => -1,
        }
    } else {
        cc.get_error_code()
    }
}

/// Wait for the mask computation to be ready.
/// Returns 0 on success, 1 on timeout, and -1 on error (use bllg_get_error() to get the exact error).
/// The '1' result does not put the constraint into error state.
#[no_mangle]
pub extern "C" fn bllg_wait_mask_ready(
    cc: &mut BllgConstraint,
    ticket: i32,
    duration_us: u64,
) -> i32 {
    if ticket <= 0 {
        cc.set_error("Invalid mask ticket");
        return -1;
    }

    if let Some(constraint) = &mut cc.constraint {
        let r = constraint.wait_mask_ready(
            MaskTicketId(ticket),
            std::time::Duration::from_micros(duration_us),
        );
        match cc.save_error(r) {
            Some(true) => 0,
            Some(false) => 1,
            None => -1,
        }
    } else {
        cc.get_error_code()
    }
}

/// Copy data from the last computed mask into the provided buffer.
/// You need to call bllg_wait_mask_ready() first to ensure the mask is ready.
/// Returns 0 on success and -1 on error (use bllg_get_error() to get the exact error).
/// The buffer is filled with 0s if the mask is smaller than the buffer.
/// # Safety
/// Should be called only from C code.
#[no_mangle]
pub unsafe extern "C" fn bllg_get_last_mask(
    cc: &mut BllgConstraint,
    mask_ptr: *mut u32,
    mask_byte_len: usize,
) -> i32 {
    if let Some(constraint) = &mut cc.constraint {
        let r = constraint.with_last_mask(|mask| {
            let to_copy = std::cmp::min(mask.len(), mask_byte_len / 4);
            unsafe {
                std::ptr::copy_nonoverlapping(mask.as_ptr(), mask_ptr, to_copy);
            }
            let bytes_left = mask_byte_len - to_copy * 4;
            if bytes_left > 0 {
                unsafe {
                    std::ptr::write_bytes(mask_ptr.add(to_copy) as *mut u8, 0, bytes_left);
                }
            }
        });
        cc.save_error(r);
    }
    cc.get_error_code()
}

/// Return any forced tokens in the current state.
/// The returned pointer is valid until the next call to this function.
/// Returns the number of tokens (which can be 0), or -1 on error (use bllg_get_error() to get the exact error).
/// # Safety
/// Should be called only from C code.
#[no_mangle]
pub unsafe extern "C" fn bllg_compute_ff_tokens(
    cc: &mut BllgConstraint,
    tokens_out: *mut *const LlgToken,
) -> i32 {
    if let Some(constraint) = &mut cc.constraint {
        let tokens = constraint.compute_ff_tokens();
        cc.ff_tokens = tokens;
        unsafe {
            *tokens_out = cc.ff_tokens.as_ptr();
        }
        cc.ff_tokens.len() as i32
    } else {
        cc.get_error_code()
    }
}

/// Free the constraint
/// # Safety
/// Should be called only from C code.
#[no_mangle]
pub unsafe extern "C" fn bllg_free_constraint(cc: &mut BllgConstraint) {
    unsafe {
        drop(Box::from_raw(cc));
    }
}
