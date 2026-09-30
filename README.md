# Background LLGuidance

This wraps the crate [llguidance](https://github.com/guidance-ai/llguidance/tree/main/parser)
and adds ability to compute masks in background thread, as well as exposes additional C APIs.

## Cancelling mask computation

Each constraint supports cooperative cancellation of queued or active mask computation. Obtain a
thread-safe handle with `bllg_get_cancellation_handle()`, call `bllg_cancel()` to stop computation,
and release the handle with `bllg_free_cancellation_handle()`. The handle remains valid if the
constraint is freed. Obtain the handle before calling `bllg_start_compute_mask()` so cancellation
is enabled before the work is queued.

`bllg_cancel()` waits for an active mask callback to finish and returns true when callback userdata
can be released. If it is called from any mask callback while the target constraint also has an
active callback, the function returns false instead of risking a callback-to-callback deadlock;
callback userdata must then remain valid until the target callback returns.

Cancellation is permanent for the associated constraint. Create a new constraint for subsequent
work, or clone the constraint before cancellation if its current state must be preserved.
`bllg_wait_mask_ready()` timeouts do not cancel computation.

The build process for this crate creates the following files in `target/release`:
- `libllguidance_bg.a` - the static library to be linked into C++
- `llguidance.h` and `llguidance_bg.h` - the C header files to be included in C++ code
- `llguidance_bg_cpp.h` - a C++ single-header wrapper around the C APIs