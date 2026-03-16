use libloading::Library;
use opforge::{
    OpforgeAsmDiagnosticsOptions, OpforgeAsmExecutionOptions, OpforgeAsmOutputOptions,
    OpforgeAsmRequest, OpforgeAsmSourceOptions, OpforgeStatus, OpforgeStringList,
    OPFORGE_DEFAULT_OUTPUTS_DISABLE, OPFORGE_EXECUTION_MODE_VM,
    OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT, OPFORGE_OUTPUT_FORMAT_TEXT,
};
use std::env;
use std::ffi::{CStr, CString};
use std::fs;
use std::os::raw::c_char;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

const CHILD_ENV: &str = "OPFORGE_RELEASE_PANIC_BOUNDARY_CHILD";
const CHILD_MODE_ENV: &str = "OPFORGE_RELEASE_PANIC_BOUNDARY_MODE";
const LIBRARY_PATH_ENV: &str = "OPFORGE_RELEASE_FFI_LIBRARY_PATH";
const SKIP_BUILD_ENV: &str = "OPFORGE_RELEASE_FFI_SKIP_BUILD";

const MODE_PANIC: &str = "panic";
const MODE_SMOKE: &str = "smoke";

type AssembleFileWithRequest =
    unsafe extern "C" fn(*const OpforgeAsmRequest) -> *mut std::ffi::c_void;
type ReportStatus = unsafe extern "C" fn(*const std::ffi::c_void) -> OpforgeStatus;
type ReportMessage = unsafe extern "C" fn(*const std::ffi::c_void) -> *const c_char;
type ReportFree = unsafe extern "C" fn(*mut std::ffi::c_void);
type ForceNextPanic = unsafe extern "C" fn();

fn make_temp_dir(name: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock before epoch")
        .as_nanos();
    let path = env::temp_dir().join(format!(
        "libopforge-release-panic-{name}-{}-{nanos}",
        std::process::id()
    ));
    fs::create_dir_all(&path).expect("create temp dir");
    path
}

fn empty_string_list() -> OpforgeStringList {
    OpforgeStringList {
        items: std::ptr::null(),
        count: 0,
    }
}

fn basic_request(root_path: *const c_char) -> OpforgeAsmRequest {
    OpforgeAsmRequest {
        source: OpforgeAsmSourceOptions {
            root_path,
            output_base: std::ptr::null(),
            defines: empty_string_list(),
            include_paths: empty_string_list(),
            module_paths: empty_string_list(),
            pp_macro_depth: 0,
        },
        execution: OpforgeAsmExecutionOptions {
            execution_mode: OPFORGE_EXECUTION_MODE_VM,
            cpu_override: std::ptr::null(),
            max_loop_iterations: 0,
            opasm_package_path: std::ptr::null(),
        },
        output: OpforgeAsmOutputOptions {
            out_dir: std::ptr::null(),
            emit_outputs: OPFORGE_DEFAULT_OUTPUTS_DISABLE,
            output_format: OPFORGE_OUTPUT_FORMAT_TEXT,
            go_addr: std::ptr::null(),
            bin_specs: empty_string_list(),
            fill_byte: 0,
            fill_byte_set: 0,
            labels_file: std::ptr::null(),
            label_output_format: OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT,
            dependency_output_path: std::ptr::null(),
            dependency_append: 0,
            dependency_make_phony: 0,
            outfile_override: std::ptr::null(),
            list_name_override: std::ptr::null(),
            hex_name_override: std::ptr::null(),
            header_title: std::ptr::null(),
            no_outputs: 0,
        },
        diagnostics: OpforgeAsmDiagnosticsOptions {
            debug_conditionals: 0,
            tab_size: 0,
        },
    }
}

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("ffi crate parent")
        .parent()
        .expect("workspace root")
        .to_path_buf()
}

fn release_ffi_library_path() -> PathBuf {
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

fn build_release_ffi_cdylib() {
    if env::var_os(SKIP_BUILD_ENV).is_some() {
        assert!(
            release_ffi_library_path().is_file(),
            "missing prebuilt cdylib artifact"
        );
        return;
    }

    let status = Command::new("cargo")
        .current_dir(workspace_root())
        .args([
            "build",
            "-p",
            "ffi",
            "--profile",
            "release-ffi",
            "--features",
            "panic-test-hooks",
            "--locked",
            "--lib",
        ])
        .status()
        .expect("build release-ffi cdylib");
    assert!(status.success(), "release-ffi build failed");
    assert!(
        release_ffi_library_path().is_file(),
        "missing cdylib artifact"
    );
}

fn assemble_ok_main() {
    let library_path = release_ffi_library_path();
    let work_dir = make_temp_dir("smoke");
    let source_path = work_dir.join("main.asm");
    fs::write(&source_path, ".module main\n    nop\n.endmodule\n").expect("write source");
    let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
    let request = basic_request(root.as_ptr());

    unsafe {
        let library = Library::new(&library_path).expect("load release-ffi cdylib");
        let assemble = library
            .get::<AssembleFileWithRequest>(b"opforge_asm_assemble_file_with_request\0")
            .expect("load assemble entrypoint");
        let report_status = library
            .get::<ReportStatus>(b"opforge_asm_report_status\0")
            .expect("load report status");
        let report_message = library
            .get::<ReportMessage>(b"opforge_asm_report_message\0")
            .expect("load report message");
        let report_free = library
            .get::<ReportFree>(b"opforge_asm_report_free\0")
            .expect("load report free");

        let report = assemble(&request);
        assert!(
            !report.is_null(),
            "expected report handle from release smoke call"
        );
        assert_eq!(report_status(report), OpforgeStatus::Ok);
        assert!(
            report_message(report).is_null(),
            "expected no report message on successful smoke call"
        );

        report_free(report);
    }
}

fn panic_boundary_main() {
    let library_path = release_ffi_library_path();
    let work_dir = make_temp_dir("child");
    let source_path = work_dir.join("main.asm");
    fs::write(&source_path, ".module main\n    nop\n.endmodule\n").expect("write source");
    let root = CString::new(source_path.to_string_lossy().as_bytes()).expect("root cstr");
    let request = basic_request(root.as_ptr());

    unsafe {
        let library = Library::new(&library_path).expect("load release-ffi cdylib");
        let force_panic = library
            .get::<ForceNextPanic>(b"opforge_test_force_next_assemble_file_with_request_panic\0")
            .expect("load panic test hook");
        let assemble = library
            .get::<AssembleFileWithRequest>(b"opforge_asm_assemble_file_with_request\0")
            .expect("load assemble entrypoint");
        let report_status = library
            .get::<ReportStatus>(b"opforge_asm_report_status\0")
            .expect("load report status");
        let report_message = library
            .get::<ReportMessage>(b"opforge_asm_report_message\0")
            .expect("load report message");
        let report_free = library
            .get::<ReportFree>(b"opforge_asm_report_free\0")
            .expect("load report free");

        force_panic();
        let report = assemble(&request);
        assert!(
            !report.is_null(),
            "expected report handle after forced panic"
        );
        assert_eq!(report_status(report), OpforgeStatus::AssembleError);

        let message_ptr = report_message(report);
        assert!(!message_ptr.is_null(), "expected panic report message");
        let message = CStr::from_ptr(message_ptr)
            .to_str()
            .expect("panic report utf8");
        assert!(message.contains("internal libopforge panic"), "{message}");
        assert!(
            message.contains("opforge_asm_assemble_file_with_request"),
            "{message}"
        );

        report_free(report);
    }
}

fn child_main() {
    match env::var(CHILD_MODE_ENV).as_deref() {
        Ok(MODE_PANIC) => panic_boundary_main(),
        Ok(MODE_SMOKE) => assemble_ok_main(),
        Ok(other) => panic!("unexpected child mode: {other}"),
        Err(_) => panic!("missing child mode"),
    }
}

fn run_child(mode: &str) -> std::process::ExitStatus {
    Command::new(env::current_exe().expect("current test executable"))
        .args([
            "--exact",
            match mode {
                MODE_PANIC => "release_profile_catches_forced_ffi_panic",
                MODE_SMOKE => "release_profile_loads_and_assembles_smoke",
                _ => panic!("unexpected mode"),
            },
            "--nocapture",
        ])
        .env(CHILD_ENV, "1")
        .env(CHILD_MODE_ENV, mode)
        .status()
        .expect("run child release-ffi test")
}

#[test]
fn release_profile_catches_forced_ffi_panic() {
    if env::var_os(CHILD_ENV).is_some() {
        child_main();
        return;
    }

    build_release_ffi_cdylib();

    let status = run_child(MODE_PANIC);

    assert!(
        status.success(),
        "release-ffi child process did not survive forced panic"
    );
}

#[test]
fn release_profile_loads_and_assembles_smoke() {
    if env::var_os(CHILD_ENV).is_some() {
        child_main();
        return;
    }

    build_release_ffi_cdylib();

    let status = run_child(MODE_SMOKE);

    assert!(
        status.success(),
        "release-ffi child process did not complete smoke assembly successfully"
    );
}
