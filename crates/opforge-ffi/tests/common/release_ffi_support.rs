use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

pub const LIBRARY_PATH_ENV: &str = "OPFORGE_RELEASE_FFI_LIBRARY_PATH";
pub const SKIP_BUILD_ENV: &str = "OPFORGE_RELEASE_FFI_SKIP_BUILD";

pub fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("ffi crate parent")
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

pub fn release_ffi_library_path() -> PathBuf {
    if let Some(path) = env::var_os(LIBRARY_PATH_ENV) {
        return PathBuf::from(path);
    }

    let filename = if cfg!(target_os = "macos") {
        "libopforge.dylib"
    } else if cfg!(target_os = "windows") {
        "opforge.dll"
    } else {
        "libopforge.so"
    };

    workspace_root()
        .join("target")
        .join("release-ffi")
        .join(filename)
}

pub fn build_release_ffi_cdylib(with_panic_test_hooks: bool) {
    if env::var_os(SKIP_BUILD_ENV).is_some() {
        assert!(
            release_ffi_library_path().is_file(),
            "missing prebuilt cdylib artifact"
        );
        return;
    }

    let mut command = Command::new("cargo");
    command
        .current_dir(workspace_root())
        .args(["build", "-p", "ffi", "--profile", "release-ffi"]);
    if with_panic_test_hooks {
        command.args(["--features", "panic-test-hooks"]);
    }
    command.args(["--locked", "--lib"]);

    let status = command.status().expect("build release-ffi cdylib");
    assert!(status.success(), "release-ffi build failed");
    assert!(
        release_ffi_library_path().is_file(),
        "missing cdylib artifact"
    );
}

#[allow(dead_code)]
pub fn header_function_names_from_shipped_header() -> Vec<String> {
    let header_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("opforge.h");
    let header = fs::read_to_string(&header_path).expect("read opforge.h");
    parse_header_function_names(&header)
}

#[allow(dead_code)]
fn parse_header_function_names(header: &str) -> Vec<String> {
    let mut names = Vec::new();
    let mut statement = String::new();

    for line in header.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty()
            || trimmed.starts_with("/*")
            || trimmed.starts_with("*/")
            || trimmed.starts_with('*')
            || trimmed.starts_with("//")
            || trimmed.starts_with('#')
        {
            continue;
        }

        if !statement.is_empty() {
            statement.push(' ');
        }
        statement.push_str(trimmed);

        if !trimmed.ends_with(';') {
            continue;
        }

        let candidate = statement.trim().to_string();
        statement.clear();

        if candidate.starts_with("typedef")
            || candidate.contains("(*")
            || !candidate.ends_with(");")
        {
            continue;
        }

        let Some(open_paren) = candidate.find('(') else {
            continue;
        };
        let Some(name) = candidate[..open_paren].split_whitespace().last() else {
            continue;
        };
        if name.starts_with("opforge_") {
            names.push(name.to_string());
        }
    }

    names.sort();
    names.dedup();
    names
}
