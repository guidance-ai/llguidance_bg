# Background LLGuidance

This wraps the crate [llguidance](https://github.com/microsoft/llguidance/tree/main/parser)
and adds ability to compute masks in background thread, as well as exposes additional C APIs.

## Cancelling mask computation

Cancellation directly exposes the cooperative cancellation mechanism provided by `llguidance`.
Obtain a thread-safe handle with `bllg_get_cancellation_handle()`, request cancellation with
`bllg_cancel()`, and release the handle with `bllg_free_cancellation_handle()`. Obtain the handle
before starting background work so the underlying parser is cancellation-aware. The handle remains
valid after the constraint is freed. The handle allocation must remain valid during cancellation
and status checks; do not free it while another thread is accessing it.

`bllg_cancel()` permanently sets the underlying cancellation flag. It does not wait for queued or
active work, wake mask waiters, or provide callback-lifetime synchronization. A mask computation
still returns a ticket when cancellation has already been requested. Use
`bllg_wait_mask_ready()` to wait for that background operation to finish and observe its
cancellation error. A timeout only stops waiting; it does not cancel the operation.
Operations that do not report `llguidance` errors retain their existing behavior. In particular,
`bllg_compute_ff_tokens()` returns an empty result after cancellation; use `bllg_is_cancelled()` if
that must be distinguished from a constraint with no forced tokens.

Cloning follows `llguidance` semantics: the clone receives an independent snapshot of the current
cancellation state. Clone before cancellation to create an unaffected constraint.

The build process for this crate creates the following files in `target/release`:
- `libllguidance_bg.a` - the static library to be linked into C++
- `llguidance.h` and `llguidance_bg.h` - the C header files to be included in C++ code
- `llguidance_bg_cpp.h` - a C++ single-header wrapper around the C APIs