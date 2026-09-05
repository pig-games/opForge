//! Step14 complete module-reader refill proof.

use super::*;

#[test]
fn native_module_buffer_refill_boundaries_fs_uae() {
    // Level D: complete module discovery/slicing across 8 KiB refill boundaries.
    // Level C covers synthetic DOS short/error reads; this case uses real DOS.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    fn pad_comments(bytes: &mut Vec<u8>, end: usize) {
        while bytes.len() < end {
            let remaining = end - bytes.len();
            if remaining < 3 {
                bytes.push(b'\n');
            } else {
                let count = remaining.min(120);
                bytes.push(b';');
                bytes.extend(std::iter::repeat_n(b'x', count - 3));
                bytes.extend_from_slice(b"\r\n");
            }
        }
    }
    let source = b".module boundary.app\n.cpu 65c02\n.use target.lib (VALUE)\n.org 0\n.byte VALUE\n.endmodule\n.end\n";
    let mut modules = b".module decoy.lib\n.pub\nVALUE .const $11\n.endmodule\n".to_vec();
    pad_comments(&mut modules, 8191);
    modules.extend_from_slice(b"\r\n.module target.lib\r\n.pub\r\nVALUE .const $22\r\n");
    pad_comments(&mut modules, 16380);
    modules.extend_from_slice(b".endmodule\r\n");
    assert_eq!(&modules[8191..8193], b"\r\n");
    assert_eq!(&modules[16380..16390], b".endmodule");
    let unrelated = b".module unrelated.lib\n.pub\nOTHER .const $44\n.endmodule\n";
    let support = [
        Item7StagedGuestFile {
            relative_path: "modules.asm".into(),
            bytes: modules.clone(),
        },
        Item7StagedGuestFile {
            relative_path: "unrelated.asm".into(),
            bytes: unrelated.to_vec(),
        },
    ];
    let oracle =
        item7_live_rust_cli_binary_oracle("module-buffer-refill", source, &support, "65c02", &[]);
    assert_eq!(oracle, [0x22]);
    let mode = std::env::var("OPFORGE_MODULE_SCAN_READ").unwrap_or("buffered".into());
    assert!(matches!(mode.as_str(), "buffered" | "byte"));
    let profile = std::env::var("OPFORGE_MODULE_SCAN_PROFILE").unwrap_or("off".into());
    assert!(matches!(profile.as_str(), "off" | "all" | "platform"));
    let mut defines = vec![crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_65C02_OUTPUT_DEFINE];
    if mode == "byte" {
        defines.push("OPFORGE_MODULE_SCAN_BYTE_READ_REFERENCE");
    }
    if profile == "all" {
        defines.extend([
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_PROGRESS_WORK_COUNTERS",
            "OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS",
            "OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL",
            "OPFORGE_PROGRESS_RUNTIME_COUNTERS",
            "OPFORGE_PROGRESS_PLATFORM_COUNTERS",
            "OPFORGE_PROGRESS_EXPORT_RECORDS",
        ]);
    }
    if profile == "platform" {
        defines.extend([
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_PROGRESS_PLATFORM_COUNTERS",
            "OPFORGE_PROGRESS_EXPORT_RECORDS",
        ]);
    }
    let package = item6_mos_package_bytes();
    let files = [
        crate::fs_uae_smoke::OpforgeNativeCliGuestFile {
            relative_path: "modules.asm",
            bytes: &modules,
        },
        crate::fs_uae_smoke::OpforgeNativeCliGuestFile {
            relative_path: "unrelated.asm",
            bytes: unrelated,
        },
    ];
    let case = crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "module-buffer-refill",
        cpu_override: "68020",
        extra_assembly_defines: &defines,
        source_override: Some(source),
        command_template: Some(
            "{input} --bin {bin} --cpu 65c02 -I {guest_work_dir} -M {guest_work_dir}",
        ),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
        extra_guest_files: &files,
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle: &oracle,
        },
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &[case],
    )
    .expect("module buffer refill parity")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(run.success && run.protocol_completed);
            assert_eq!(run.exit_code, Some(0));
            assert_eq!(verified_fs_uae_output(run), oracle);
            let guest_stdout = captured_fs_uae_artifact(
                run,
                "Work/case_artifacts/case_0000/opforge_fsuae_smoke.stdout",
            );
            let guest_stderr = captured_fs_uae_artifact(
                run,
                "Work/case_artifacts/case_0000/opforge_fsuae_smoke.stderr",
            );
            assert!(
                guest_stdout.is_empty(),
                "unexpected guest stdout: {}",
                String::from_utf8_lossy(guest_stdout)
            );
            assert!(
                guest_stderr.is_empty(),
                "unexpected guest stderr: {}",
                String::from_utf8_lossy(guest_stderr)
            );
            let counters = (profile == "all").then(|| {
                super::super::native_harness_evidence::native_harness_decode_exported_profile(
                    run, true,
                )
            });
            let platform_records = (profile == "platform").then(|| {
                serde_json::json!({
                    "ofpr": captured_fs_uae_artifact(run, "Work/opforge-profile.ofpr"),
                    "ofio": captured_fs_uae_artifact(run, "Work/opforge-profile.ofio"),
                })
            });
            eprintln!(
                "MODULE_BUFFER_RESULT {}",
                serde_json::json!({
                    "mode": mode, "profile": profile, "complete": run.protocol_completed,
                    "exit_code": run.exit_code, "native_image_digest": run.native_image_digest,
                    "start_to_done_host_seconds": run.start_to_done_host_seconds,
                    "source": source.as_slice(), "modules": modules, "unrelated": unrelated.as_slice(),
                    "exact_output": oracle, "stdout": String::from_utf8_lossy(guest_stdout), "stderr": String::from_utf8_lossy(guest_stderr), "counters": counters, "platform_records": platform_records,
                })
            );
        }
    }
}
