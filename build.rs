extern crate cbindgen;

use std::env;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    let config = cbindgen::Config {
        language: cbindgen::Language::C,
        cpp_compat: true,
        usize_is_size_t: true, // not exposed as .with_*() method
        ..Default::default()
    };

    println!("cargo:rerun-if-changed=src/ffi.rs");
    println!("cargo:rerun-if-changed=cpp/llguidance_bg_cpp.h");

    std::fs::copy(
        format!("{}/cpp/llguidance_bg_cpp.h", crate_dir),
        format!(
            "{}/../../../llguidance_bg_cpp.h",
            env::var("OUT_DIR").unwrap()
        ),
    )
    .expect("Failed to copy C++ header file");

    cbindgen::Builder::new()
        .with_config(config)
        .with_include_guard("LLGUIDANCE_BG_H")
        .with_crate(crate_dir)
        .rename_item("ParserLimits", "LlgParserLimits")
        .generate()
        .map_or_else(
            |error| match error {
                cbindgen::Error::ParseSyntaxError { .. } => {}
                e => panic!("{:?}", e),
            },
            |bindings| {
                bindings.write_to_file(format!(
                    "{}/../../../llguidance_bg.h",
                    env::var("OUT_DIR").unwrap()
                ));
            },
        );
}
