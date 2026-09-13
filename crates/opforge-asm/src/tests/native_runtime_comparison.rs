//! Opt-in receipt for small, complete Rust/native comparisons.
use super::*;
use crate::fs_uae_smoke::{
    FsUaeSmokeOutcome, OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase,
    OpforgeNativeCliProof,
};

#[test]
#[ignore = "requires explicit source/package and configured FS-UAE"]
fn native_runtime_comparison_fs_uae() {
    let source = fs::read_to_string(std::env::var("OPFORGE_COMPARE_SOURCE").unwrap()).unwrap();
    let package = fs::read(std::env::var("OPFORGE_COMPARE_PACKAGE").unwrap()).unwrap();
    assert_eq!(
        package,
        build_hierarchy_package_from_registry(&default_registry()).unwrap(),
        "native and live Rust oracle must use the same current package"
    );
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live Rust source oracle");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let oracle: Vec<u8> = entries.into_iter().map(|(_, byte)| byte).collect();
    let profile = std::env::var("OPFORGE_COMPARE_PROFILE").unwrap_or_else(|_| "off".into());
    assert!(matches!(profile.as_str(), "off" | "all" | "runtime"));
    let defines: &[&str] = if profile == "all" {
        &[
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_PROGRESS_WORK_COUNTERS",
            "OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS",
            "OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL",
            "OPFORGE_PROGRESS_RUNTIME_COUNTERS",
            "OPFORGE_PROGRESS_PLATFORM_COUNTERS",
            "OPFORGE_PROGRESS_EXPORT_RECORDS",
        ]
    } else if profile == "runtime" {
        &[
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_PROGRESS_RUNTIME_COUNTERS",
            "OPFORGE_PROGRESS_EXPORT_RECORDS",
        ]
    } else {
        &[]
    };
    let case = OpforgeNativeCliParityCase {
        name: "runtime-comparison",
        cpu_override: "68020",
        extra_assembly_defines: defines,
        source_override: Some(source.as_bytes()),
        command_template: Some("{input} --bin {bin} --cpu m6502 --opasm-package {package}"),
        package_mode: OpforgeNativeCliPackageMode::Explicit(&package),
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle: &oracle,
        },
    };
    let native_root = std::env::var_os("OPFORGE_COMPARE_NATIVE_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(workspace_root);
    let result =
        crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&native_root, &[case])
            .expect("completed native comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("explicit comparison requires real native execution");
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.success && run.protocol_completed);
    assert_eq!(run.exit_code, Some(0));
    let counters = (profile != "off").then(|| {
        use super::native_harness_evidence::{
            native_harness_decode_exported_profile_with_projection, NativeProfileProjection,
        };
        native_harness_decode_exported_profile_with_projection(
            run,
            true,
            if profile == "runtime" {
                NativeProfileProjection::Runtime
            } else {
                NativeProfileProjection::All
            },
        )
    });
    eprintln!(
        "RUNTIME_COMPARISON {}",
        serde_json::json!({
            "guest_start_to_done_host_seconds": run.start_to_done_host_seconds.expect("single-case timing"),
            "native_image_digest": run.native_image_digest,
            "native_image_bytes": captured_fs_uae_artifact(run, "Work/build/opforge_cli").len(),
            "exact_output": oracle, "guest_exit": run.exit_code,
            "profile": profile, "counters": counters,
        })
    );
}
