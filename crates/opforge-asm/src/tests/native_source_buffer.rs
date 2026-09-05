//! Step19 source-reader refill and suspended-parent proof.

use super::*;

#[test]
fn native_source_buffer_refill_boundaries_fs_uae() {
    // Level D: real DOS, source refills, CRLF, module slicing and include resume.
    // This does not inject DOS errors or prove arbitrary recursion depth.
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
    let mut source =
        b".module boundary.app\n.cpu 65c02\n.use target.lib (VALUE)\n.org 0\n".to_vec();
    pad_comments(&mut source, 511);
    source.extend_from_slice(b"\r\n.include \"defs.inc\"\r\n.byte VALUE\r\n.endmodule\r\n.end");
    assert_eq!(&source[511..513], b"\r\n");
    let include = b".byte $11\r\n";
    let mut modules = b".module decoy.lib\n.pub\nVALUE .const $11\n.endmodule\n".to_vec();
    pad_comments(&mut modules, 511);
    modules.extend_from_slice(b"\r\n.module target.lib\r\n.pub\r\nVALUE .const $22\r\n");
    pad_comments(&mut modules, 1020);
    modules.extend_from_slice(b".endmodule\r\n");
    modules.extend_from_slice(b".module after.lib\n.org $20\n.byte $ee\n.endmodule\n");
    assert_eq!(&modules[511..513], b"\r\n");
    assert_eq!(&modules[1020..1030], b".endmodule");
    let unrelated = b".module unrelated.lib\n.pub\nOTHER .const $44\n.endmodule\n";
    let support = [
        Item7StagedGuestFile {
            relative_path: "defs.inc".into(),
            bytes: include.to_vec(),
        },
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
        item7_live_rust_cli_binary_oracle("source-buffer-refill", &source, &support, "65c02", &[]);
    assert_eq!(oracle, [0x11, 0x22]);
    let mode = std::env::var("OPFORGE_SOURCE_READ").unwrap_or("buffered".into());
    assert!(matches!(mode.as_str(), "buffered" | "byte"));
    let profile = std::env::var("OPFORGE_SOURCE_PROFILE").unwrap_or("off".into());
    assert!(matches!(profile.as_str(), "off" | "all" | "platform"));
    let mut defines = vec![crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_65C02_OUTPUT_DEFINE];
    if mode == "byte" {
        defines.push("OPFORGE_SOURCE_READ_BYTE_REFERENCE");
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
            relative_path: "defs.inc",
            bytes: include,
        },
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
        name: "source-buffer-refill",
        cpu_override: "68020",
        extra_assembly_defines: &defines,
        source_override: Some(&source),
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
    .expect("source buffer refill parity")
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
                "SOURCE_BUFFER_RESULT {}",
                serde_json::json!({
                    "mode": mode, "profile": profile, "complete": run.protocol_completed,
                    "exit_code": run.exit_code, "native_image_digest": run.native_image_digest,
                    "start_to_done_host_seconds": run.start_to_done_host_seconds,
                    "source": source.as_slice(), "include": include.as_slice(), "modules": modules, "unrelated": unrelated.as_slice(),
                    "exact_output": oracle, "stdout": String::from_utf8_lossy(guest_stdout), "stderr": String::from_utf8_lossy(guest_stderr), "counters": counters, "platform_records": platform_records,
                })
            );
        }
    }
}

#[test]
fn native_source_buffer_debug_include_fs_uae() {
    // Level D: real native include execution retains INCLUDE-LINE in explicit
    // debug mode while producing the exact live Rust CLI binary.
    // This does not prove normal-mode output isolation or refill boundaries.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    let source = b".cpu 65c02\n.org 0\n.include \"debug.inc\"\n.end\n";
    let include = b".byte $11\n";
    let support = [Item7StagedGuestFile {
        relative_path: "debug.inc".into(),
        bytes: include.to_vec(),
    }];
    let oracle = item7_live_rust_cli_binary_oracle(
        "source-buffer-debug-include",
        source,
        &support,
        "65c02",
        &[],
    );
    assert_eq!(oracle, [0x11]);
    let package = item6_mos_package_bytes();
    let files = [crate::fs_uae_smoke::OpforgeNativeCliGuestFile {
        relative_path: "debug.inc",
        bytes: include,
    }];
    let defines = [crate::fs_uae_smoke::FS_UAE_OPFORGE_NATIVE_CLI_65C02_OUTPUT_DEFINE];
    let case = crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "source-buffer-debug-include",
        cpu_override: "68020",
        extra_assembly_defines: &defines,
        source_override: Some(source),
        command_template: Some(
            "{input} --bin {bin} --cpu 65c02 -I {guest_work_dir} --native-debug",
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
    .expect("source buffer debug include parity")
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
            assert_eq!(
                String::from_utf8_lossy(guest_stdout)
                    .lines()
                    .filter(|line| *line == "INCLUDE-LINE 1 1")
                    .count(),
                1,
                "expected one include-line debug record in stdout: {}",
                String::from_utf8_lossy(guest_stdout)
            );
            let guest_stderr = captured_fs_uae_artifact(
                run,
                "Work/case_artifacts/case_0000/opforge_fsuae_smoke.stderr",
            );
            assert!(
                guest_stderr.is_empty(),
                "unexpected guest stderr: {}",
                String::from_utf8_lossy(guest_stderr)
            );
        }
    }
}
