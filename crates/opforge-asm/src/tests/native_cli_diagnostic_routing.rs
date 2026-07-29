//! Native CLI diagnostic-routing contract tests.

use super::*;

#[test]
fn native_cli_error_output_routing_contract() {
    // Proof level B. This test proves the native CLI has a distinct
    // ErrorOutput adapter, composed diagnostics keep every fragment on that
    // sink, and the real-CLI batch harness captures stdout/stderr separately.
    // This test does not prove real AmigaOS execution or diagnostic semantics.
    let repo_root = workspace_root();
    let constants = fs::read_to_string(
        repo_root.join("native/motorola68000/amigaos/opforge-cli/constants.asm"),
    )
    .expect("read native CLI constants");
    let dos =
        fs::read_to_string(repo_root.join("native/motorola68000/amigaos/opforge-cli/dos.asm"))
            .expect("read native CLI DOS adapter");
    let run =
        fs::read_to_string(repo_root.join("native/motorola68000/amigaos/opforge-cli/run.asm"))
            .expect("read native CLI run orchestration");
    let report =
        fs::read_to_string(repo_root.join("native/motorola68000/amigaos/opforge-cli/report.asm"))
            .expect("read native CLI report renderer");
    let event_report = fs::read_to_string(
        repo_root.join("native/motorola68000/amigaos/opforge-cli/opasm_event_report.asm"),
    )
    .expect("read native CLI opasm event renderer");
    let text_output = fs::read_to_string(
        repo_root.join("native/motorola68000/amigaos/opforge-cli/text_output.asm"),
    )
    .expect("read native CLI text output helpers");
    let harness = fs::read_to_string(repo_root.join("crates/opforge-asm/src/fs_uae_smoke.rs"))
        .expect("read FS-UAE native CLI harness");

    for required in ["PR_COS", "PR_CES", "FPUTS"] {
        assert!(
            constants.contains(required),
            "native CLI constants must declare {required} for ErrorOutput routing"
        );
    }
    for required in [
        "putErrStr",
        "constants.PR_CES",
        "constants.PR_COS",
        "constants.FPUTS",
    ] {
        assert!(
            dos.contains(required),
            "native CLI DOS adapter must contain {required}"
        );
    }

    for diagnostic in [
        "InputOpenErrorText",
        "NativeHunkNotImplementedText",
        "HunkRequiredText",
        "TokenizerFailureText",
        "NativePassFailureText",
        "NativeOutputFailureText",
        "EmitterStubText",
    ] {
        let routed = format!("move.l #strings.{diagnostic}, d1\n\tjsr dos.putErrStr");
        assert!(
            run.contains(&routed),
            "run-level diagnostic {diagnostic} must use ErrorOutput"
        );
    }

    let parse_error = report
        .split_once("opforgeNativeCliReportParseError\t.block")
        .and_then(|(_, tail)| tail.split_once("\t.bend  ; opforgeNativeCliReportParseError"))
        .map(|(body, _)| body)
        .expect("parse-error renderer body");
    assert!(
        !parse_error.contains("jsr dos.putStr"),
        "every parse-error fragment must use ErrorOutput"
    );
    assert!(
        parse_error.contains("jsr dos.putErrStr"),
        "parse-error renderer must emit through ErrorOutput"
    );

    for required in [
        "opforgeNativeCliPutErrU16Decimal",
        "opforgeNativeCliPutErrSpace",
    ] {
        assert!(
            text_output.contains(required),
            "composed diagnostic helper {required} must exist"
        );
        assert!(
            event_report.contains(required),
            "opasm diagnostics must use {required}"
        );
    }
    assert!(
        event_report.contains("jsr dos.putErrStr"),
        "opasm diagnostic strings must use ErrorOutput"
    );

    for required in [
        "stderr_path: PathBuf",
        "FS_UAE_OPFORGE_NATIVE_CLI_CASE_STDERR_FILE",
        "batch_script.push_str(\" *>\")",
        "pub(crate) exit_code: Option<i32>",
    ] {
        assert!(
            harness.contains(required),
            "native CLI batch harness must contain {required}"
        );
    }
}
