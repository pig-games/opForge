//! Native implicit-origin parity proofs.

use super::*;

const IMPLICIT_ORIGIN_SOURCE: &[u8] =
    b"        .word target-1\n        nop\n        jmp exec\nexec    nop\ntarget  .byte $aa\n";

const EXPLICIT_ORIGIN_SOURCE: &[u8] = b"        .org $1200\n        .word target-1\n        nop\n        jmp exec\nexec    nop\ntarget  .byte $aa\n";

fn rust_origin_bytes(source: &[u8]) -> Vec<u8> {
    let text = std::str::from_utf8(source).expect("origin fixture UTF-8");
    let mut lines = vec![".cpu 65c02"];
    lines.extend(text.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble Rust origin authority");
    assert!(
        diagnostics.is_empty(),
        "Rust origin diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_default_origin_rust_oracle() {
    // Proof level A. This proves live Rust begins an implicit session at zero
    // and honors an explicit nonzero .org in both label arithmetic and an
    // absolute instruction operand. It does not prove native initialization.
    assert_eq!(
        rust_origin_bytes(IMPLICIT_ORIGIN_SOURCE),
        vec![0x06, 0x00, 0xea, 0x4c, 0x06, 0x00, 0xea, 0xaa]
    );
    assert_eq!(
        rust_origin_bytes(EXPLICIT_ORIGIN_SOURCE),
        vec![0x06, 0x12, 0xea, 0x4c, 0x06, 0x12, 0xea, 0xaa]
    );
}

#[test]
fn native_default_origin_source_contract() {
    // Proof level B. This proves pass one initializes the native session at
    // zero and pass two restarts from the retained origin. It does not execute
    // native code or prove an emitted artifact.
    let engine = fs::read_to_string(
        workspace_root().join("native/motorola68000/amigaos/opasm/opasm_engine.asm"),
    )
    .expect("read native opasm engine");
    let pass_one_start = engine
        .find("opasmEngineBeginPassOneV1\t.block")
        .expect("pass-one block");
    let pass_one = &engine[pass_one_start..];
    let pass_one_end = pass_one
        .find("\t.bend  ; opasmEngineBeginPassOneV1")
        .expect("pass-one end");
    let pass_one = &pass_one[..pass_one_end];
    assert!(source_contains_in_order(
        pass_one,
        &[
            "clr.l OpasmEngineSessionOrigin.l",
            "move.l OpasmEngineSessionOrigin.l, d1",
            "move.l d1, OpasmEngineSessionCurrentPc.l",
        ]
    ));

    let pass_two_start = engine
        .find("opasmEngineBeginPassTwoV1\t.block")
        .expect("pass-two block");
    let pass_two = &engine[pass_two_start..];
    let pass_two_end = pass_two
        .find("\t.bend  ; opasmEngineBeginPassTwoV1")
        .expect("pass-two end");
    let pass_two = &pass_two[..pass_two_end];
    assert!(source_contains_in_order(
        pass_two,
        &[
            "move.l OpasmEngineSessionOrigin.l, d1",
            "move.l d1, OpasmEngineSessionCurrentPc.l",
        ]
    ));
}

#[test]
fn native_default_origin_fs_uae() {
    // Proof level D. This proves the real native CLI matches live Rust for
    // both implicit zero origin and an explicit nonzero origin.
    let root = workspace_root();
    let package = item6_mos_package_bytes();
    let implicit_rust = rust_origin_bytes(IMPLICIT_ORIGIN_SOURCE);
    let explicit_rust = rust_origin_bytes(EXPLICIT_ORIGIN_SOURCE);
    let cases = [
        crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
            name: "implicit-origin-zero",
            cpu_id: "65c02",
            source: IMPLICIT_ORIGIN_SOURCE,
            package_bytes: package.as_slice(),
            proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(
                implicit_rust.as_slice(),
            ),
        },
        crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
            name: "explicit-origin-1200",
            cpu_id: "65c02",
            source: EXPLICIT_ORIGIN_SOURCE,
            package_bytes: package.as_slice(),
            proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(
                explicit_rust.as_slice(),
            ),
        },
    ];
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &cases)
        .expect("default-origin FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), cases.len());
            for (run, expected) in runs.iter().zip([implicit_rust, explicit_rust]) {
                assert!(
                    run.success,
                    "native origin fixture failed\nstdout:\n{}\nstderr:\n{}",
                    run.stdout, run.stderr
                );
                assert_eq!(verified_fs_uae_output(run), expected.as_slice());
            }
        }
    }
}
