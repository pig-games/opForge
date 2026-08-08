//! Native non-emitting `.ds` reservation parity proofs.

use super::*;

const DS_RESERVATION_SOURCE: &[u8] = b".org 0\n.byte 1\ngap .ds 3\nafter .byte 2\n.word after\n";
const INVALID_DS_RESERVATION_SOURCE: &[u8] = b".org 0\n.ds missing_size\n";

fn rust_ds_reservation_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(DS_RESERVATION_SOURCE).expect("ds fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust ds authority");
    assert!(
        diagnostics.is_empty(),
        "Rust ds diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_ds_reservation_rust_oracle() {
    // Proof level A. This proves live Rust omits the reserved gap from its
    // emitted entries while the following label retains address 4. It does
    // not prove native sizing, emission, or guest execution.
    assert_eq!(rust_ds_reservation_bytes(), vec![0x01, 0x02, 0x04, 0x00]);
}

#[test]
fn native_ds_reservation_source_contract() {
    // Proof level B. This proves native `.ds` evaluates and advances the PC in
    // the sizing path, while its emission branch appends no bytes. It does not
    // execute native code or prove the final artifact.
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    assert!(source_contains_in_order(
        &driver,
        &[
            "ds\n\tmoveq #2, d5",
            "bsr.w readOperandValueForStatement",
            "advanceLayoutD3",
            "jsr eng.opasmEngineAdvancePcBySizeV1",
        ]
    ));
    let emit_start = driver.find("\nemitDs\n").expect("emitDs branch") + 1;
    let emit = &driver[emit_start..];
    let emit_end = emit.find("emitFill\n").expect("emitDs branch end");
    let emit = &emit[..emit_end];
    assert!(emit.contains("moveq #0, d0"));
    assert!(!emit.contains("appendRepeatedByte"));
    assert!(!emit.contains("opasmEngineAppendImageBytesV1"));
}

#[test]
fn native_ds_reservation_fs_uae() {
    // Proof level D. This proves the real native CLI emits exact live Rust
    // sparse-entry bytes and completes an unresolved reservation with a
    // diagnostic after the exact fresh guest protocol.
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let rust_bytes = rust_ds_reservation_bytes();
    let cases = [
        crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
            name: "ds-reservation",
            cpu_id: "65c02",
            source: DS_RESERVATION_SOURCE,
            package_bytes: package.as_slice(),
            proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(
                rust_bytes.as_slice(),
            ),
        },
        crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
            name: "ds-reservation-unresolved",
            cpu_id: "65c02",
            source: INVALID_DS_RESERVATION_SOURCE,
            package_bytes: package.as_slice(),
            proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExpectedFailureWithDiagnostic,
        },
    ];
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &cases)
        .expect("ds reservation FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), cases.len());
            assert_eq!(verified_fs_uae_output(&runs[0]), rust_bytes.as_slice());
            assert!(runs[0].success);
            assert!(runs[1].protocol_completed);
            assert_ne!(runs[1].exit_code, Some(0));
            assert!(!runs[1].stderr.trim().is_empty());
        }
    }
}
