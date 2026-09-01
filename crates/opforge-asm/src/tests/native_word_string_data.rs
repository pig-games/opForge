//! Native quoted `.word` string-data parity proofs.

use super::*;

const WORD_STRING_SOURCE: &[u8] =
    b".org 0\n.word $4142\n.word 'AB'\n.word \"AB\"\n.word 'A'*256 | 'B'\n";
const INVALID_WORD_STRING_SOURCE: &[u8] = b".org 0\n.word '\\x1'\n";

fn rust_word_string_bytes() -> Vec<u8> {
    let text = std::str::from_utf8(WORD_STRING_SOURCE).expect("word-string fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust word-string authority");
    assert!(
        diagnostics.is_empty(),
        "Rust word-string diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_word_string_data_rust_oracle() {
    // Proof level A. This proves live Rust emits pure quoted `.word` operands
    // in decoded source order while numeric forms retain little-endian word
    // packing. It does not prove native routing or execution.
    assert_eq!(
        rust_word_string_bytes(),
        vec![0x42, 0x41, 0x41, 0x42, 0x41, 0x42, 0x42, 0x41]
    );
}

#[test]
fn native_word_string_data_source_contract() {
    // Proof level B. This proves native `.word` sizing and emission try the
    // quoted-data owner before the numeric-data fallback. It does not execute
    // native code or prove emitted bytes.
    let driver = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native opasm driver");
    assert!(source_contains_in_order(
        &driver,
        &[
            "word\n\tmoveq #0, d5",
            "bsr.w parseTextDirectiveForStatement",
            "move.l OpasmTextScratchLen, d3",
            "wordNumeric",
            "moveq #2, d5",
            "bsr.w dataDirectiveSizeForStatement",
        ]
    ));
    assert!(source_contains_in_order(
        &driver,
        &[
            "emitWord\n\tmove.l d6, d7",
            "moveq #0, d5",
            "bsr.w parseTextDirectiveForStatement",
            "jsr eng.opasmEngineAppendImageBytesV1",
            "emitWordNumeric",
            "moveq #2, d5",
            "bsr.w emitDataDirectiveForStatement",
        ]
    ));
}

#[test]
fn native_word_string_data_fs_uae() {
    // Proof level D. This proves the real native CLI emits exact live Rust
    // bytes for the complete focused quoted/numeric matrix and completes a
    // malformed quoted word with diagnostics.
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let rust_bytes = rust_word_string_bytes();
    let cases = [
        crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
            name: "word-string-data",
            cpu_id: "65c02",
            source: WORD_STRING_SOURCE,
            package_bytes: package.as_slice(),
            proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(
                rust_bytes.as_slice(),
            ),
        },
        crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
            name: "word-string-data-invalid",
            cpu_id: "65c02",
            source: INVALID_WORD_STRING_SOURCE,
            package_bytes: package.as_slice(),
            proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExpectedFailureWithDiagnostic,
        },
    ];
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &cases)
        .expect("word-string FS-UAE helper")
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
