# LLGuidance BG

This wraps the crate [llguidance](https://github.com/microsoft/llguidance/tree/main/parser)
and adds ability to compute masks in background thread, as well as exposes additional C APIs.

The build process for this crate creates the following files in `target/release`:
- `libllguidance_bg.a` - the static library to be linked into C++
- `llguidance.h` and `llguidance_bg.h` - the C header files to be included in C++ code
