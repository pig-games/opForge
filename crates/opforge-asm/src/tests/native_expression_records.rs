//! Typed records retain values and carry relocation provenance through shared ExprVM.
use crate::fs_uae_smoke::FsUaeSmokeOutcome;
use std::path::{Path, PathBuf};

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn oracle() -> Vec<u8> {
    let cases: [[u32; 6]; 17] = [
        [0, 0, 0, 4, 0, 0],          // retained compact record alias
        [0, 0, 0, 4, 0, 0],          // typed list index retains record
        [0, 7, 0, 0, 0, 0],          // numeric member identity
        [0, 44, 0, 0, 2, 0],         // section-relative member
        [0, 45, 0, 0, 2, 0],         // record default plus absolute
        [0, 45, 0, 0, 2, 0],         // absolute plus record default
        [0, 35, 0, 0, 2, 0],         // record default minus absolute
        [0, 20, 0, 0, 0, 0],         // same-section difference cancels provenance
        [0, 100, 0, 0, u32::MAX, 0], // unsupported address algebra
        [1, 0, 0, 0, 0, 0],          // absent member fails explicitly
        [0, 80, 0, 0, 2, 0],         // current PC provenance
        [0, 40, 0, 0, 2, 0],         // explicit scalar boundary projects default
        [0, 45, 0, 0, 2, 0],         // compact arithmetic projects record default
        [0, 0, 0, 0, u32::MAX, 0],   // two unsafe tags cannot cancel provenance
        [0, 7, 0, 0, 0, 0],          // canonical undefined symbol numeric binding
        [0, 44, 0, 0, 2, 0],         // compact undefined symbol numeric binding
        [0, 45, 0, 0, 2, 0],         // compact numeric binding plus absolute
    ];
    cases
        .into_iter()
        .flatten()
        .chain([0, 40, 0, 2, 1, 0x10012, 0, 2, 1]) // invalid bare record descriptor
        .chain([0, 1, 1, 1, 1]) // nested equality: equal, changed, provenance, truncated, cycle
        .flat_map(u32::to_be_bytes)
        .collect()
}

struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}
fn assemble(root: &Path) -> (Scratch, Vec<u8>, String) {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let path = std::env::temp_dir().join(format!(
        "opforge-expression-records-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&path).unwrap();
    let scratch = Scratch(path);
    let source = std::fs::read_to_string(root.join("native/motorola68000/amigaos/test-harnesses/experimental/binary_expression_records_harness.asm")).unwrap();
    let entry = scratch.0.join("entry.asm");
    std::fs::write(&entry, &source).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_owned(),
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
    config.out_dir = Some(scratch.0.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => {
            panic!(
                "native EXVM component: {error}; diagnostics: {:?}",
                error.diagnostics()
            )
        }
        cli_core::CliRunError::Workflow { error, .. } => panic!("native EXVM component: {error}"),
        _ => panic!("native EXVM component warnings"),
    });
    let image = std::fs::read(scratch.0.join("expression-records.hunk")).unwrap();
    (scratch, image, source)
}

#[test]
fn native_expression_records_host_assembles() {
    assert_eq!(oracle().len(), 17 * 24 + 36 + 5 * 4);
    assert!(!assemble(&workspace_root()).1.is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; fresh typed record, projection and provenance component proof"]
fn native_expression_records_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let root = workspace_root();
    let expected = oracle();
    let (_scratch, image, source) = assemble(&root);
    let case = OpforgeNativeCliParityCase {
        name: "expression-records",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/expression-records.bin",
            rust_oracle: &expected,
        },
    };
    let result = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh typed record completion");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
}
