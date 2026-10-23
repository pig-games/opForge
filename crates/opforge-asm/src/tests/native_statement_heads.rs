// SPDX-License-Identifier: GPL-3.0-or-later
//! Native acquisition head adapter at the owned TKVM record boundary.
//! The live Rust A8 oracle classifies roles; body execution is separate.
use std::path::{Path, PathBuf};
#[path = "native_statement_heads/fixtures.rs"]
mod fixtures;
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
fn assemble(input: &[u8]) -> (Scratch, Vec<u8>, String) {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let repo = root();
    let path = std::env::temp_dir().join(format!(
        "opforge-statement-heads-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&path).unwrap();
    let scratch = Scratch(path);
    std::fs::write(scratch.0.join("statement-heads-cases.bin"), input).unwrap();
    let source = std::fs::read_to_string(
        repo.join("native/motorola68000/amigaos/tests/assembler/statement_heads_harness.asm"),
    )
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
        repo.join("native/motorola68000/amigaos")
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
            "native statement heads: {error}; diagnostics: {:?}",
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => panic!("native statement heads: {error}"),
        _ => panic!("native statement heads warnings"),
    });
    let image = std::fs::read(scratch.0.join("statement-heads.hunk")).unwrap();
    (scratch, image, source)
}
#[test]
fn native_statement_heads_live_oracles_and_fixture_export() {
    let (input, expected) = fixtures::batch();
    let count = u32::from_be_bytes(input[..4].try_into().unwrap());
    assert!(count >= 20 && expected.len() == count as usize * 8);
    let directory = root().join("target/native-statement-heads");
    std::fs::create_dir_all(&directory).unwrap();
    std::fs::write(directory.join("statement-heads-cases.bin"), input).unwrap();
    std::fs::write(directory.join("statement-heads-expected.bin"), expected).unwrap();
    eprintln!("STATEMENT_HEADS owned-token/live-oracle cases={count}");
}
#[test]
fn native_statement_heads_host_assembles() {
    let (input, _) = fixtures::batch();
    assert!(!assemble(&input).1.is_empty());
}
#[test]
#[ignore = "requires configured FS-UAE; fresh owned-token statement acquisition proof"]
fn native_statement_heads_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let (input, expected) = fixtures::batch();
    let (_scratch, image, source) = assemble(&input);
    let case = OpforgeNativeCliParityCase {
        name: "statement-heads-owned-token-adapter",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/statement-heads-results.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root(), &case, &image)
        .unwrap_or_else(|error| panic!("fresh statement head adapter proof: {error}"));
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
    eprintln!(
        "STATEMENT_HEADS cases={} exact=true owned-record-boundary=true seconds={:?}",
        u32::from_be_bytes(input[..4].try_into().unwrap()),
        runs[0].start_to_done_host_seconds
    );
}
