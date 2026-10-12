// SPDX-License-Identifier: GPL-3.0-or-later
//! External-template body ownership and cursor component controls.
//! Shared Rust statement assembly supplies numeric/text substitution oracles.
use super::native_iterable_loops::oracle;
use std::path::{Path, PathBuf};
fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}
struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}
fn assemble() -> (Scratch, Vec<u8>, String) {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let repo = root();
    let path = std::env::temp_dir().join(format!(
        "opforge-statement-templates-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&path).unwrap();
    let scratch = Scratch(path);
    let source = std::fs::read_to_string(repo.join(
        "native/motorola68000/amigaos/test-harnesses/experimental/stmt_templates_harness.asm",
    ))
    .unwrap();
    let entry = scratch.0.join("entry.asm");
    std::fs::write(&entry, &source).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_owned(),
        entry.to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        repo.join("native/motorola68000")
            .to_string_lossy()
            .into_owned(),
        "-I".into(),
        repo.join("native/motorola68000/amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(scratch.0.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "native statement templates: {error}; diagnostics: {:?}",
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("native statement templates: {error}")
        }
        _ => panic!("native statement templates warnings"),
    });
    let image = std::fs::read(scratch.0.join("statement-templates.hunk")).unwrap();
    (scratch, image, source)
}
fn expected() -> Vec<u8> {
    let directory = super::native_iterable_loops::scratch();
    let source = |body: &str| format!(".module app\n.cpu 68020\n{body}.endmodule\n");
    let numeric = oracle(
        &directory.0,
        &source(".statement T byte:x\n\n.byte .x\n.endstatement\nlabel T 0B8H\n"),
    )
    .unwrap();
    assert_eq!(numeric, [184]);
    let textual = oracle(
        &directory.0,
        &source(".statement T str:x\n.byte \".x:@1\"\n.endstatement\nlabel T \"0B8H\"\n"),
    )
    .unwrap();
    assert_eq!(textual, b"0B8H:0B8H");
    let empty = oracle(
        &directory.0,
        &source(".statement T str:x\n.byte \".x:@1\"\n.endstatement\nlabel T \"\"\n"),
    )
    .unwrap();
    assert_eq!(empty, b":");
    let label = [0, 0, 102, 0, 5];
    let record = |payload: &[u8]| {
        let mut bytes = vec![(8 + payload.len()) as u8, 0, 0, 77];
        bytes.extend(label);
        bytes.extend(payload);
        bytes
    };
    let mut output = Vec::new();
    for value in [6_u32, 5, 1] {
        output.extend(value.to_be_bytes());
    }
    let mut text_token = vec![3, textual.len() as u8];
    text_token.extend(textual);
    for row in [
        record(&[2, 0, 0, 0, numeric[0]]),
        record(&[]),
        record(&[]),
        record(&text_token),
        record(&[]),
        record(&[3, 1, empty[0]]),
    ] {
        output.extend((row.len() as u32).to_be_bytes());
        output.extend(row);
    }
    output
}
#[test]
fn native_statement_templates_live_oracles_and_fixture_export() {
    let bytes = expected();
    let directory = root().join("target/native-statement-templates");
    std::fs::create_dir_all(&directory).unwrap();
    std::fs::write(directory.join("statement-templates-expected.bin"), bytes).unwrap();
    eprintln!("STATEMENT_TEMPLATES cases=6 overloads=true nine-bound=true blank-label=true empty-capture=true");
}
#[test]
fn native_statement_templates_host_assembles() {
    assert!(!assemble().1.is_empty());
}
#[test]
#[ignore = "requires configured FS-UAE; fresh external-template body/cursor proof"]
fn native_statement_templates_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let expected = expected();
    let (_scratch, image, source) = assemble();
    let case = OpforgeNativeCliParityCase {
        name: "statement-templates-external-owner",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/statement-templates-results.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root(), &case, &image)
        .unwrap_or_else(|error| panic!("fresh template body/cursor proof: {error}"));
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
    eprintln!(
        "STATEMENT_TEMPLATES cases=6 exact=true seconds={:?}",
        runs[0].start_to_done_host_seconds
    );
}
