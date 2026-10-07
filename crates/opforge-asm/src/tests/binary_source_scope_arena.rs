//! Real-native scope storage observation, not assembler or self-host parity.
use super::*;
use std::path::{Path, PathBuf};

const NAME_BYTES: u32 = 128;
const NAME_COUNT: u32 = 10_000;
const BEFORE_ARENA_BYTES: u32 = 1 << 20;
const HARNESS: &str =
    "native/motorola68000/amigaos/test-harnesses/experimental/binary_scope_arena_harness.asm";

struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn assemble(root: &Path) -> Vec<u8> {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let scratch = Scratch(std::env::temp_dir().join(format!(
        "opforge-scope-arena-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    )));
    std::fs::create_dir(&scratch.0).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_owned(),
        root.join(HARNESS).to_string_lossy().into_owned(),
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
            panic!("assemble scope arena component: {:?}", error.diagnostics())
        }
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("assemble scope arena component: {error}")
        }
        _ => panic!("assemble scope arena component: warnings treated as errors"),
    });
    let image = std::fs::read(scratch.0.join("scope-arena-probe.hunk")).unwrap();
    if let Some(path) = std::env::var_os("OPFORGE_SCOPE_ARENA_SAVE_IMAGE") {
        std::fs::write(path, &image).unwrap();
    }
    image
}

fn oracle(expect_limit: bool) -> Vec<u8> {
    let observation = if expect_limit {
        [1, BEFORE_ARENA_BYTES / NAME_BYTES, BEFORE_ARENA_BYTES]
    } else {
        [0, NAME_COUNT, NAME_COUNT * NAME_BYTES]
    };
    observation.into_iter().flat_map(u32::to_be_bytes).collect()
}

#[test]
fn compact_scope_arena_component_host_assembly() {
    assert!(!assemble(&workspace_root()).is_empty());
    assert_eq!(oracle(false).len(), 12);
    assert_eq!(oracle(true).len(), 12);
}

#[test]
#[ignore = "requires configured FS-UAE; scope arena component observation only"]
fn compact_scope_arena_component_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let root = std::env::var_os("OPFORGE_COMPARE_NATIVE_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(workspace_root);
    let source = std::fs::read(root.join(HARNESS)).unwrap();
    let image = match std::env::var_os("OPFORGE_SCOPE_ARENA_IMAGE") {
        Some(path) => std::fs::read(path).unwrap(),
        None => assemble(&root),
    };
    let expect_limit = match std::env::var("OPFORGE_SCOPE_ARENA_EXPECT_LIMIT").as_deref() {
        Err(std::env::VarError::NotPresent) | Ok("0") => false,
        Ok("1") => true,
        value => panic!("OPFORGE_SCOPE_ARENA_EXPECT_LIMIT must be 0 or 1: {value:?}"),
    };
    // This selector affects only the test's live exact-artifact oracle.
    let expected = oracle(expect_limit);
    let case = OpforgeNativeCliParityCase {
        name: "scope-arena-component",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(&source),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/scope-arena.bin",
            rust_oracle: &expected,
        },
    };
    let result = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh native scope arena component completion");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    assert!(runs[0].success);
    eprintln!(
        "SCOPE_ARENA expected_limit={expect_limit} seconds={:?} image_bytes={} observation={expected:02x?}",
        runs[0].start_to_done_host_seconds,
        image.len()
    );
}
