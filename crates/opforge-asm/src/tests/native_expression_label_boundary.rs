//! Native parenthesized label-term boundary parity proofs.

use super::*;

const LABEL_BOUNDARY_SOURCE: &[u8] = b"        .org $1000\njump_tab .byte 1,2,3\nentries .const ($-jump_tab) / 3\n        .byte entries\n        .word (jump_tab)\n";

const UNRESOLVED_LABEL_BOUNDARY_SOURCE: &[u8] = b"        .org 0\n        .byte (missing_label)\n";

fn rust_label_boundary_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(LABEL_BOUNDARY_SOURCE).expect("label-boundary fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust label-boundary authority");
    assert!(
        diagnostics.is_empty(),
        "Rust label-boundary diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_label_boundary_rust_oracle() {
    // Proof level A. This proves live Rust evaluates current-address
    // subtraction from an underscore-bearing label inside a group, continues
    // with division, and accepts a directly parenthesized label term. It does
    // not prove native token consumption or guest execution.
    assert_eq!(
        rust_label_boundary_bytes(),
        vec![0x01, 0x02, 0x03, 0x01, 0x00, 0x10]
    );
}

#[test]
fn native_expression_label_boundary_source_contract() {
    // Proof level B. This proves the native label scanner stops before the
    // closing parenthesis that compileSingleTerm's group parser must consume.
    // It does not execute native code or prove an emitted artifact.
    let bridge = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm"),
    )
    .expect("read native expression bridge");
    let start = bridge.find("termLength\t.block").expect("termLength block");
    let block = &bridge[start..];
    let end = block.find("\t.bend  ; termLength").expect("termLength end");
    let block = &block[..end];
    assert!(source_contains_in_order(
        block,
        &["cmpi.b #'-', d1", "cmpi.b #')', d1", "beq.s done"]
    ));

    let single_start = bridge
        .find("compileSingleTerm\t.block")
        .expect("compileSingleTerm block");
    let single = &bridge[single_start..];
    let single_end = single
        .find("\t.bend  ; compileSingleTerm")
        .expect("compileSingleTerm end");
    let single = &single[..single_end];
    assert!(source_contains_in_order(
        single,
        &["bsr.w compileHighLow", "cmpi.b #')', (a0)", "addq.l #1, a0",]
    ));
    assert!(source_contains_in_order(
        single,
        &[
            "jsr (a1)",
            "tst.l d0",
            "bne.s labelSnapshot",
            "labelSnapshot",
            "move.l d2, d0",
            "bsr.w resolveLabelIndex",
        ]
    ));
}

#[test]
fn native_expression_label_boundary_fs_uae() {
    // Proof level D. This proves the real native CLI emits the exact Rust
    // artifact for resolved grouped labels and completes with diagnostics for
    // an actually unresolved grouped label.
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let rust_bytes = rust_label_boundary_bytes();
    let cases = [
        crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
            name: "expression-label-boundary",
            cpu_id: "65c02",
            source: LABEL_BOUNDARY_SOURCE,
            package_bytes: package.as_slice(),
            proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(
                rust_bytes.as_slice(),
            ),
        },
        crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
            name: "expression-label-boundary-unresolved",
            cpu_id: "65c02",
            source: UNRESOLVED_LABEL_BOUNDARY_SOURCE,
            package_bytes: package.as_slice(),
            proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExpectedFailureWithDiagnostic,
        },
    ];
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &cases)
        .expect("expression label-boundary FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), cases.len());
            eprintln!(
                "proof expression-label-boundary: protocol_completed={} exit={:?} success={} bytes={:02x?}",
                runs[0].protocol_completed,
                runs[0].exit_code,
                runs[0].success,
                verified_fs_uae_output(&runs[0]),
            );
            eprintln!(
                "proof expression-label-boundary-unresolved: protocol_completed={} exit={:?} success={} diagnostic={:?}",
                runs[1].protocol_completed,
                runs[1].exit_code,
                runs[1].success,
                runs[1].stderr.trim(),
            );
            assert_eq!(verified_fs_uae_output(&runs[0]), rust_bytes.as_slice());
            assert!(runs[0].success);
            assert!(runs[1].protocol_completed);
            assert_ne!(runs[1].exit_code, Some(0));
            assert!(!runs[1].stderr.trim().is_empty());
        }
    }
}
