//! Direct owner proof for complete macro-plan cloning and transactional rejection.
//! The current Rust assembler builds the harness from current native sources;
//! execution checks offset semantics before publishing its one-byte success file.
use super::native_iterable_loops::scratch;
use super::*;

fn clone_image(dir: &Path) -> (Vec<u8>, String) {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let source = fs::read_to_string(
        root.join("native/motorola68000/amigaos/test-harnesses/experimental/macro_plan_clone.asm"),
    )
    .unwrap();
    let entry = dir.join("entry.asm");
    fs::write(&entry, &source).unwrap();
    let cli = Cli::parse_from([
        "opForge".into(),
        entry.to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        root.join("native/motorola68000")
            .to_string_lossy()
            .into_owned(),
        "-I".into(),
        root.join("native/motorola68000/amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.into());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "macro plan clone harness: {error}; diagnostics: {:?}",
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("macro plan clone harness: {error}")
        }
        _ => panic!("macro plan clone harness warnings"),
    });
    (fs::read(dir.join("macro-plan-clone.hunk")).unwrap(), source)
}

#[test]
fn native_macro_plan_clone_host_assembles() {
    let dir = scratch();
    let image = clone_image(&dir.0).0;
    assert!(!image.is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; macro plan recipe rebasing, rejection and source retirement"]
fn native_macro_plan_clone_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let (image, source) = clone_image(&dir.0);
    let expected = [0x5a];
    let case = OpforgeNativeCliParityCase {
        name: "macro-plan-clone-ownership",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/macro-plan-clone.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .unwrap_or_else(|error| panic!("fresh macro plan clone proof: {error}"));
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
}
