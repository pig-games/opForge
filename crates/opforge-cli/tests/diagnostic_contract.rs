// SPDX-License-Identifier: GPL-3.0-or-later

use std::fs;
use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use serde_json::Value;

static TEMP_DIR_SEQ: AtomicU64 = AtomicU64::new(1);

fn unique_temp_dir(prefix: &str) -> PathBuf {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock")
        .as_nanos();
    let pid = std::process::id();
    let seq = TEMP_DIR_SEQ.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("{prefix}-{pid}-{now}-{seq}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

fn opforge(args: &[String]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_opforge"))
        .args(args)
        .output()
        .expect("run opforge")
}

fn arg(path: &std::path::Path) -> String {
    path.to_string_lossy().to_string()
}

fn assert_failed_with_output(output: &Output) -> String {
    assert!(!output.status.success(), "command unexpectedly succeeded");
    let stderr = String::from_utf8(output.stderr.clone()).expect("stderr is UTF-8");
    let stdout = String::from_utf8(output.stdout.clone()).expect("stdout is UTF-8");
    assert!(
        !stderr.is_empty() || !stdout.is_empty(),
        "failed command produced no output"
    );
    format!("{stderr}{stdout}")
}

fn write_source(temp_dir: &std::path::Path, name: &str, source: &str) -> PathBuf {
    let input = temp_dir.join(name);
    fs::write(&input, source).expect("write source");
    input
}

#[test]
fn missing_input_reports_the_path_and_not_found() {
    let temp_dir = unique_temp_dir("opforge-cli-missing-input");
    let missing = temp_dir.join("missing.asm");
    let output = opforge(&["--infile".to_string(), arg(&missing)]);
    let rendered = assert_failed_with_output(&output);
    assert!(
        rendered.contains(missing.to_string_lossy().as_ref()),
        "{rendered}"
    );
    assert!(
        rendered.contains("Input source file not found"),
        "{rendered}"
    );
}

#[test]
fn unsupported_extension_reports_the_accepted_extension() {
    let temp_dir = unique_temp_dir("opforge-cli-unsupported-extension");
    let input = temp_dir.join("input.txt");
    fs::write(&input, "nop\n").expect("write input");
    let output = opforge(&["--infile".to_string(), arg(&input)]);
    let rendered = assert_failed_with_output(&output);
    assert!(rendered.contains("source extensions: .asm"), "{rendered}");
}

#[test]
fn input_directory_without_main_reports_the_folder_rule() {
    let temp_dir = unique_temp_dir("opforge-cli-empty-project");
    let project = temp_dir.join("project");
    fs::create_dir_all(&project).expect("create project");
    let output = opforge(&["--infile".to_string(), arg(&project)]);
    let rendered = assert_failed_with_output(&output);
    assert!(
        rendered.contains("Input folder must contain exactly one main.* root module"),
        "{rendered}"
    );
}

#[test]
fn input_directory_with_multiple_main_files_reports_ambiguity() {
    let temp_dir = unique_temp_dir("opforge-cli-ambiguous-project");
    let project = temp_dir.join("project");
    fs::create_dir_all(&project).expect("create project");
    fs::write(project.join("main.asm"), "nop\n").expect("write first main");
    fs::write(project.join("main.inc"), "nop\n").expect("write second main");
    let output = opforge(&["--infile".to_string(), arg(&project)]);
    let rendered = assert_failed_with_output(&output);
    assert!(
        rendered.contains("Input folder contains multiple main.* root modules"),
        "{rendered}"
    );
}

#[test]
fn quiet_still_reports_a_terminal_failure() {
    let temp_dir = unique_temp_dir("opforge-cli-quiet-fatal");
    let missing = temp_dir.join("missing.asm");
    let output = opforge(&["--quiet".to_string(), "--infile".to_string(), arg(&missing)]);
    let rendered = assert_failed_with_output(&output);
    assert!(
        rendered.contains("Input source file not found"),
        "{rendered}"
    );
}

#[test]
fn json_fatal_failure_is_a_valid_error_record() {
    let temp_dir = unique_temp_dir("opforge-cli-json-fatal");
    let missing = temp_dir.join("missing.asm");
    let output = opforge(&[
        "--format".to_string(),
        "json".to_string(),
        "--infile".to_string(),
        arg(&missing),
    ]);
    assert!(!output.status.success(), "command unexpectedly succeeded");
    let stderr = String::from_utf8(output.stderr).expect("stderr is UTF-8");
    let payload: Value = serde_json::from_str(stderr.trim()).expect("valid fatal JSON");
    assert_eq!(payload["severity"], "error");
    assert_eq!(payload["message"], "Input source file not found");
    assert_eq!(payload["file"], missing.to_string_lossy().as_ref());
}

#[test]
fn later_multi_input_failure_names_the_terminal_input() {
    let temp_dir = unique_temp_dir("opforge-cli-multi-input-failure");
    let first = temp_dir.join("first.asm");
    let missing = temp_dir.join("missing.asm");
    fs::write(&first, ".module first\nnop\n.endmodule\n").expect("write source");
    let output = opforge(&[
        "--list".to_string(),
        "--infile".to_string(),
        arg(&first),
        "--infile".to_string(),
        arg(&missing),
    ]);
    let rendered = assert_failed_with_output(&output);
    assert!(
        rendered.contains(missing.to_string_lossy().as_ref()),
        "{rendered}"
    );
}

#[test]
fn no_error_intentionally_suppresses_failure_output() {
    let temp_dir = unique_temp_dir("opforge-cli-no-error");
    let missing = temp_dir.join("missing.asm");
    let output = opforge(&[
        "--no-error".to_string(),
        "--infile".to_string(),
        arg(&missing),
    ]);
    assert!(!output.status.success(), "command unexpectedly succeeded");
    assert!(
        output.stdout.is_empty(),
        "unexpected stdout: {:?}",
        output.stdout
    );
    assert!(
        output.stderr.is_empty(),
        "unexpected stderr: {:?}",
        output.stderr
    );
}

#[test]
fn source_failures_render_bounded_context_in_default_and_classic_styles() {
    let temp_dir = unique_temp_dir("opforge-cli-source-context");
    let tokenizer = write_source(
        &temp_dir,
        "tokenizer.asm",
        ".cpu \"68020\"\n.org $1000\n.bogus\nnop\n.end\n",
    );
    let parser = write_source(
        &temp_dir,
        "parser.asm",
        ".cpu \"68020\"\n.org $1000\nmove ???\nnop\n.end\n",
    );
    let semantic = write_source(
        &temp_dir,
        "semantic.asm",
        ".cpu \"68020\"\n.org $1000\n.byte missing_symbol\nnop\n.end\n",
    );

    let tokenizer_rendered =
        assert_failed_with_output(&opforge(&["--infile".to_string(), arg(&tokenizer)]));
    assert!(
        tokenizer_rendered.contains("    3 | .bogus"),
        "{tokenizer_rendered}"
    );
    assert!(
        tokenizer_rendered.contains("      |       ^"),
        "{tokenizer_rendered}"
    );
    assert!(
        tokenizer_rendered.contains("    5 | .end"),
        "{tokenizer_rendered}"
    );

    let parser_rendered =
        assert_failed_with_output(&opforge(&["--infile".to_string(), arg(&parser)]));
    assert!(
        parser_rendered.contains("    3 | move ???"),
        "{parser_rendered}"
    );
    assert!(parser_rendered.contains("      | ^"), "{parser_rendered}");
    assert!(
        parser_rendered.contains("    5 | .end"),
        "{parser_rendered}"
    );

    let semantic_rendered = assert_failed_with_output(&opforge(&[
        "--diagnostics-style".to_string(),
        "classic".to_string(),
        "--infile".to_string(),
        arg(&semantic),
    ]));
    assert!(
        semantic_rendered.contains("    3 | .byte missing_symbol"),
        "{semantic_rendered}"
    );
    assert!(
        semantic_rendered.contains("      |       ^"),
        "{semantic_rendered}"
    );
    assert!(
        semantic_rendered.contains("    5 | .end"),
        "{semantic_rendered}"
    );
}

#[cfg(unix)]
#[test]
fn output_open_failure_reports_the_output_target() {
    let temp_dir = unique_temp_dir("opforge-cli-output-failure");
    let source = temp_dir.join("main.asm");
    fs::write(&source, ".byte 1\n").expect("write source");
    let output = opforge(&[
        "--list".to_string(),
        "/dev/null/opforge-output.lst".to_string(),
        "--infile".to_string(),
        arg(&source),
    ]);
    let rendered = assert_failed_with_output(&output);
    assert!(
        rendered.contains("/dev/null/opforge-output.lst"),
        "{rendered}"
    );
}

#[cfg(target_os = "linux")]
#[test]
fn failed_diagnostics_file_write_falls_back_with_the_original_error() {
    let temp_dir = unique_temp_dir("opforge-cli-diagnostics-fallback");
    let missing = temp_dir.join("missing.asm");
    let output = opforge(&[
        "--error".to_string(),
        "/dev/full".to_string(),
        "--infile".to_string(),
        arg(&missing),
    ]);
    let rendered = assert_failed_with_output(&output);
    assert!(
        rendered.contains("diagnostics sink write failed"),
        "{rendered}"
    );
    assert!(
        rendered.contains("Input source file not found"),
        "{rendered}"
    );
}
