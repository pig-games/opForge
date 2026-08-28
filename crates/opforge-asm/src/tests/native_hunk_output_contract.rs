//! Native executable-Hunk structural contract proofs.

use super::*;

#[test]
fn native_item35_hunk_output_contract() {
    // Proof level B. This binds the native writer and routing surface to the
    // Rust relocation-free Hunk record order and memory-bit constants. It does
    // not execute native code or prove artifact bytes; the Item 35 Level D
    // guests provide those proofs.
    let root = workspace_root();
    let rust = fs::read_to_string(root.join("crates/opforge-vm/src/output_hunk.rs"))
        .expect("read Rust Hunk authority");
    for required in [
        "const HUNK_HEADER: u32 = 0x0000_03f3;",
        "const HUNK_CODE: u32 = 0x0000_03e9;",
        "const HUNK_DATA: u32 = 0x0000_03ea;",
        "const HUNK_BSS: u32 = 0x0000_03eb;",
        "const HUNK_END: u32 = 0x0000_03f2;",
        "if input.segments[0].kind != SectionKind::Code",
        "bytes.resize(",
        "push_be_u32(&mut bytes, HUNK_END);",
    ] {
        assert!(rust.contains(required), "Rust authority lacks {required:?}");
    }

    let native =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/hunk_output.asm"))
            .expect("read native Hunk writer");
    for required in [
        "HUNK_HEADER = $000003f3",
        "HUNK_CODE = $000003e9",
        "HUNK_DATA = $000003ea",
        "HUNK_BSS = $000003eb",
        "HUNK_END = $000003f2",
        "ori.l #$40000000, d4",
        "ori.l #$80000000, d4",
    ] {
        assert!(
            native.contains(required),
            "native writer lacks {required:?}"
        );
    }
    assert!(source_contains_in_order(
        &native,
        &[
            "countLoop",
            "tst.w d7",
            "cmpi.l #layout.OPASM_LAYOUT_SECTION_KIND_CODE, d2",
            "move.l #HUNK_HEADER, (a3)+",
            "allocationLoop",
            "tst.l d1",
            "beq.s allocationNext",
            "segmentBegin",
            "segmentLoop",
            "move.l #HUNK_END, (a3)+",
        ]
    ));
    assert!(source_contains_in_order(
        &native,
        &[
            "move.l #HUNK_HEADER, (a2)+",
            "move.l #HUNK_CODE, (a2)+",
            "copyImage",
            "padLoop",
            "move.l #HUNK_END, (a2)+",
        ]
    ));

    let output =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/output.asm"))
            .expect("read native output router");
    assert!(source_contains_in_order(
        &output,
        &[
            "tst.w state.NativeCliSourceOutputSectionCount",
            "jsr hunk_output.buildSelectedSectionsV1",
            "buildDefaultArtifact",
            "jsr hunk_output.buildFlatCodeV1",
            "lea state.NativeCliHunkPath, a0",
        ]
    ));
}
