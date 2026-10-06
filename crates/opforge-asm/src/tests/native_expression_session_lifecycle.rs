//! Session switching must restore EXVM grammar and finish must invalidate it.
use super::*;
use crate::fs_uae_smoke::FsUaeSmokeOutcome;
use std::path::{Path, PathBuf};
use vm::runtime_model_core::RuntimeModelCore;

fn packages() -> (Vec<u8>, Vec<u8>) {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let a = crate::binary_source_experiment::prepare_package(&core, &resolved).unwrap();
    let mut b = a.clone();
    let plan = u32::from_be_bytes(a[200..204].try_into().unwrap()) as usize;
    // B accepts one number only; A accepts the complete arithmetic expression.
    b[plan..plan + 3].copy_from_slice(&[
        package::ExvmOpcode::BuildNumber as u8,
        package::ExvmOpcode::Advance as u8,
        package::ExvmOpcode::End as u8,
    ]);
    assert_ne!(a, b);
    (a, b)
}

fn oracle() -> Vec<u8> {
    // begin A, compile A, prepare A statement, begin B, compile B, finish B, invalid compile,
    // activate A, compile A, finish A, invalid compile.
    [0u32, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1]
        .into_iter()
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
        "opforge-expression-session-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&path).unwrap();
    let scratch = Scratch(path);
    let (a, b) = packages();
    std::fs::write(scratch.0.join("package-a.bin"), a).unwrap();
    std::fs::write(scratch.0.join("package-b.bin"), b).unwrap();
    let source = std::fs::read_to_string(root.join("native/motorola68000/amigaos/test-harnesses/experimental/binary_expression_session_harness.asm")).unwrap();
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
    let image = std::fs::read(scratch.0.join("expression-session.hunk")).unwrap();
    (scratch, image, source)
}

#[test]
fn native_expression_session_lifecycle_host_assembles() {
    assert_eq!(oracle().len(), 44);
    assert!(!assemble(&workspace_root()).1.is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; fresh two-session grammar lifecycle proof"]
fn native_expression_session_lifecycle_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let root = workspace_root();
    let expected = oracle();
    let (_scratch, image, source) = assemble(&root);
    let case = OpforgeNativeCliParityCase {
        name: "expression-session-lifecycle",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/expression-session.bin",
            rust_oracle: &expected,
        },
    };
    let result = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh session lifecycle completion");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
}
