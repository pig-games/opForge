//! Level B package checks and opt-in Level D native binary-source proof.
use super::*;
use crate::binary_source_experiment::prepare_package;
use crate::fs_uae_smoke::FsUaeSmokeOutcome;
use vm::runtime_model_core::RuntimeModelCore;

#[test]
fn binary_source_packages_prepare() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000"] {
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let bytes = prepare_package(&core, &resolved).unwrap();
        assert_eq!(&bytes[..4], b"BSP1");
        assert_eq!(
            u32::from_be_bytes(bytes[4..8].try_into().unwrap()) as usize,
            bytes.len()
        );
        assert_eq!(bytes, prepare_package(&core, &resolved).unwrap());
    }
}

#[test]
#[ignore = "requires explicit source/CPU and configured FS-UAE"]
fn binary_source_fs_uae() {
    let source = fs::read_to_string(std::env::var("OPFORGE_COMPARE_SOURCE").unwrap()).unwrap();
    let cpu = std::env::var("OPFORGE_COMPARE_CPU").unwrap();
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(&cpu, None).unwrap();
    let preparation_started = std::time::Instant::now();
    let mut input = prepare_package(&core, &resolved).unwrap();
    let package_preparation_seconds = preparation_started.elapsed().as_secs_f64();
    let package_bytes = input.len();
    input.extend_from_slice(source.as_bytes());
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live Rust source oracle");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let oracle: Vec<u8> = entries.into_iter().map(|(_, byte)| byte).collect();
    let result =
        crate::fs_uae_smoke::run_binary_source_harness_from_env(&workspace_root(), &input, &oracle)
            .expect("completed native binary-source comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("explicit comparison requires real native execution");
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.success && run.protocol_completed);
    assert_eq!(run.exit_code, Some(0));
    let image = run
        .captured_artifacts
        .get(&PathBuf::from("Work/build/binary_source_harness"))
        .expect("fresh native image capture");
    eprintln!(
        "BINARY_SOURCE_COMPARISON {}",
        serde_json::json!({
            "cpu": cpu, "source_bytes": source.len(), "runtime_package_bytes": package_bytes,
            "host_package_preparation_seconds": package_preparation_seconds,
            "native_image_bytes": image.len(),
            "guest_start_to_done_host_seconds": run.start_to_done_host_seconds,
            "native_image_digest": run.native_image_digest,
            "exact_output": oracle, "guest_exit": run.exit_code,
            "source_and_dictionary_erased_before_passes": true,
        })
    );
}

#[test]
#[ignore = "requires explicit source/CPU and configured FS-UAE"]
fn binary_source_rejection_fs_uae() {
    let source = fs::read_to_string(std::env::var("OPFORGE_COMPARE_SOURCE").unwrap()).unwrap();
    let cpu = std::env::var("OPFORGE_COMPARE_CPU").unwrap();
    let oracle =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true);
    assert!(
        match &oracle {
            Err(_) => true,
            Ok((_, diagnostics)) => !diagnostics.is_empty(),
        },
        "negative case must be rejected by the live Rust assembler",
    );
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(&cpu, None).unwrap();
    let mut input = prepare_package(&core, &resolved).unwrap();
    input.extend_from_slice(source.as_bytes());
    let result =
        crate::fs_uae_smoke::run_binary_source_rejection_from_env(&workspace_root(), &input)
            .expect("fresh completed native rejection with diagnostic");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("explicit rejection contract requires native execution");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
}
