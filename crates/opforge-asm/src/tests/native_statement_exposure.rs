// SPDX-License-Identifier: GPL-3.0-or-later
//! Module-flat statement source facets and exposure metadata controls.
//! Live Rust source probes establish visible-key behavior; direct metadata controls
//! exercise the acquired native descriptor ABI, not the .use source parser.
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
        "opforge-statement-exposure-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&path).unwrap();
    let scratch = Scratch(path);
    let source = std::fs::read_to_string(repo.join(
        "native/motorola68000/amigaos/test-harnesses/experimental/statement_exposure_harness.asm",
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
            "native statement exposure: {error}; diagnostics: {:?}",
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("native statement exposure: {error}")
        }
        _ => panic!("native statement exposure warnings"),
    });
    let image = std::fs::read(scratch.0.join("statement-exposure.hunk")).unwrap();
    (scratch, image, source)
}
fn expected() -> Vec<u8> {
    let directory = super::native_iterable_loops::scratch();
    let library = ".module foo.bar\n.pub\n.statement PUSH byte:x\n.byte 7\n.endstatement\n.priv\n.statement PUSH str:x\n.byte 8\n.endstatement\n.endmodule\n";
    std::fs::create_dir_all(directory.0.join("foo")).unwrap();
    std::fs::write(directory.0.join("foo/bar.asm"), library).unwrap();
    let source = |body: &str| format!(".module app\n.cpu 68020\n{body}.endmodule\n");
    // Each successful invocation independently proves a caller-visible key.
    for (use_line, head) in [
        (".use foo.bar", "foo.bar.PUSH"),
        (".use foo.bar as bar", "bar.PUSH"),
        (".use foo.bar(*)", "PUSH"),
        (".use foo.bar(PUSH as X)", "X"),
        (".use foo.bar(PUSH) as bar", "bar.PUSH"),
    ] {
        assert_eq!(
            oracle(
                &directory.0,
                &source(&format!("{use_line}\n    {head} 1\n"))
            )
            .unwrap(),
            [7]
        );
    }
    let hidden = oracle(&directory.0, &source(".use foo.bar\n    bar.PUSH 1\n")).unwrap_err();
    assert!(
        hidden.contains("No instruction found for BAR.PUSH"),
        "{hidden}"
    );
    let private = oracle(&directory.0, &source(".use foo.bar(*)\n    PUSH \"xx\"\n")).unwrap_err();
    assert!(
        private.contains("unsupported Motorola 68000 operand form"),
        "{private}"
    );
    assert_eq!(oracle(&directory.0, &source(".use foo.bar(*)\n.priv\n.statement PUSH str:x\n.byte 8\n.endstatement\n    PUSH 1\n    PUSH \"xx\"\n")).unwrap(), [7,8]);
    // Actual Rust block definitions share one module keyword vector.
    let ambiguous = oracle(&directory.0, &source("a .block\n.statement T byte:x\n.byte 1\n.endstatement\n.bend\nb .block\n.statement T byte:y\n.byte 2\n.endstatement\n    T 1\n.bend\n")).unwrap_err();
    assert!(
        ambiguous.contains("Ambiguous statement signature"),
        "{ambiguous}"
    );
    assert_eq!(oracle(&directory.0, &source(".statement T byte:x\n.byte 1\n.endstatement\nT .macro x\n.byte 2\n.endmacro\n    T 7\n    .T 7\n")).unwrap(), [1,2]);
    std::fs::write(
        directory.0.join("other.asm"),
        ".module other\n.pub\n.statement PUSH byte:x\n.byte 9\n.endstatement\n.endmodule\n",
    )
    .unwrap();
    assert_eq!(
        oracle(
            &directory.0,
            &source(".use foo.bar(*)\n.use other(*)\n    PUSH 1\n")
        )
        .unwrap(),
        [9]
    );
    let mut output = Vec::new();
    // Native source-facet metadata: independent template + statement declared/public.
    output.extend(15_u32.to_be_bytes());
    output.extend(1_u32.to_be_bytes());
    for (key, ordinal) in [
        ("foo.bar.PUSH", 1_u32),
        ("foo.bar.PUSH", 1),
        ("bar.PUSH", 1),
        ("PUSH", 1),
        ("X", 1),
        ("bar.PUSH", 1),
        ("PUSH", 0),
        ("PUSH", 1),
        ("PUSH", 2),
    ] {
        output.extend((key.len() as u32).to_be_bytes());
        output.extend(ordinal.to_be_bytes());
        output.extend(key.as_bytes());
    }
    output
}
#[test]
fn native_statement_exposure_live_oracles_and_fixture_export() {
    let bytes = expected();
    let directory = root().join("target/native-statement-exposure");
    std::fs::create_dir_all(&directory).unwrap();
    std::fs::write(directory.join("statement-exposure-expected.bin"), bytes).unwrap();
    eprintln!("STATEMENT_EXPOSURE scenarios=8 rows=9 live_namespace=true acquired_metadata=true");
}
#[test]
fn native_statement_exposure_host_assembles() {
    assert!(!assemble().1.is_empty());
}
#[test]
#[ignore = "requires configured FS-UAE; fresh statement exposure metadata proof"]
fn native_statement_exposure_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let expected = expected();
    let (_scratch, image, source) = assemble();
    let case = OpforgeNativeCliParityCase {
        name: "statement-exposure-owner",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/statement-exposure-results.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root(), &case, &image)
        .unwrap_or_else(|error| panic!("fresh statement exposure metadata proof: {error}"));
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
    eprintln!(
        "STATEMENT_EXPOSURE scenarios=8 rows=9 exact=true seconds={:?}",
        runs[0].start_to_done_host_seconds
    );
}
