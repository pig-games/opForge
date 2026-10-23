//! Direct native binding plan producer/consumer growth beyond one MiB.
use super::native_iterable_loops::scratch;
use super::*;

fn growth_image(dir: &Path) -> (Vec<u8>, String) {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let source = fs::read_to_string(
        root.join("native/motorola68000/amigaos/tests/assembler/binding_plan_growth.asm"),
    )
    .unwrap();
    let entry = dir.join("entry.asm");
    fs::write(&entry, &source).unwrap();
    let source_root = std::env::var_os("OPFORGE_BINDING_PLAN_SOURCE_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|| root.join("native/motorola68000"));
    let cli = Cli::parse_from([
        "opForge".into(),
        entry.to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        source_root.to_string_lossy().into_owned(),
        "-I".into(),
        source_root.join("amigaos").to_string_lossy().into_owned(),
        "-I".into(),
        source_root
            .join("amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.into());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "binding plan growth harness: {error}; diagnostics: {:?}",
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("binding plan growth harness: {error}")
        }
        _ => panic!("binding plan growth harness warnings"),
    });
    (
        fs::read(dir.join("binding-plan-growth.hunk")).unwrap(),
        source,
    )
}

#[test]
fn native_binding_plan_growth_host_assembles() {
    let dir = scratch();
    let image = growth_image(&dir.0).0;
    assert!(!image.is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; actual binding plan growth rejection and ABI"]
fn native_binding_plan_growth_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let (image, source) = growth_image(&dir.0);
    let expected = [0x5a];
    let case = OpforgeNativeCliParityCase {
        name: "binding-plan-word-boundary",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/binding-plan-growth.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .unwrap_or_else(|error| panic!("fresh binding plan growth proof: {error}"));
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
}
