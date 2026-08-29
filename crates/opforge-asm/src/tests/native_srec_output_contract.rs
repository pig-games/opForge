//! Native Motorola S-record structural contract proofs.

use super::*;

#[test]
fn native_item37_srec_writer_contract() {
    // Proof level B. This binds the native writer to the current Rust S-record
    // contract. It does not execute native code or prove artifact bytes; the
    // directed Level D guest provides that proof.
    let root = workspace_root();
    let rust = fs::read_to_string(root.join("crates/opforge-types/src/image.rs"))
        .expect("read Rust S-record authority");
    for required in [
        "pub fn write_srec_file<W: Write>",
        "const LINE_LIMIT: usize = 32;",
        "write_srec_record(&mut out, data_record, address_bytes, line_addr, &line_data)?;",
        "write_srec_record(&mut out, termination_record, address_bytes, start_addr, &[])?;",
        "let checksum = 0xffu8.wrapping_sub(sum);",
    ] {
        assert!(rust.contains(required), "Rust authority lacks {required:?}");
    }

    let native =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/srec_output.asm"))
            .expect("read native S-record writer");
    for required in [
        "NATIVE_SREC_LINE_DATA_BYTES = 32",
        "buildFlatV1\t.block",
        "addi.b #'/', d3",
        "moveq #11, d3",
        "sub.w d4, d3",
        "moveq #-1, d0",
        "sub.b d7, d0",
        ".byte \"0123456789ABCDEF\"",
    ] {
        assert!(
            native.contains(required),
            "native writer lacks {required:?}"
        );
    }
    assert!(source_contains_in_order(
        &native,
        &[
            "moveq #2, d4",
            "cmpi.l #$0000ffff, d7",
            "moveq #3, d4",
            "cmpi.l #$00ffffff, d7",
            "moveq #4, d4",
            "recordLoop",
            "tst.b (a4)",
            "recordStart",
            "moveq #0, d1",
            "cmpi.l #NATIVE_SREC_LINE_DATA_BYTES, d1",
            "termination",
        ]
    ));

    let engine =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_engine.asm"))
            .expect("read native image owner");
    for required in [
        "OpasmEngineImagePresentBuffer",
        "clearImagePresentV1",
        "move.b #1, (a2)+",
        "opasmEngineGetImagePresentBufferPtrV1",
    ] {
        assert!(
            engine.contains(required),
            "native image owner lacks {required:?}"
        );
    }

    // The current Rust writer emits data plus termination records only. S0
    // headers and S5/S6 count records are intentionally not native additions.
    assert!(!native.contains("emitHeaderRecord"));
    assert!(!native.contains("emitCountRecord"));

    let args = fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/args.asm"))
        .expect("read native CLI parser");
    assert!(source_contains_in_order(
        &args,
        &[
            "lea strings.FlagSrecShort, a1",
            "lea strings.FlagSrecLong, a1",
            "srec",
            "move.w #1, state.NativeCliSrecRequested",
            "move.w #constants.NATIVE_OUTPUT_FORMAT_SREC, state.NativeCliOutputFormat",
        ]
    ));
    assert!(source_contains_in_order(
        &args,
        &[
            "lea strings.FlagGoShort, a1",
            "lea strings.FlagGoLong, a1",
            "goAddress",
            "opforgeNativeCliParseGoAddressValue",
            "cmpi.l #8, d2",
            "cmpi.l #4, d2",
            "move.l d1, state.NativeCliGoAddr",
            "opforgeNativeCliDeriveSrecPath",
            "move.b #'s', (a2)+",
            "move.b #'r', (a2)+",
            "move.b #'e', (a2)+",
            "move.b #'c', (a2)+",
        ]
    ));

    let output =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/output.asm"))
            .expect("read native output router");
    assert!(source_contains_in_order(
        &output,
        &[
            "buildSrec",
            "jsr srec_output.buildFlatV1",
            "payloadSrecPath",
            "lea state.NativeCliSrecPath, a0",
        ]
    ));
    assert!(source_contains_in_order(
        &output,
        &[
            "opforgeNativeCliWriteRequestedOutputs\t.block",
            "tst.w state.NativeCliFlatBinRequested",
            "tst.w state.NativeCliSrecRequested",
            "move.w #constants.NATIVE_OUTPUT_FORMAT_SREC, state.NativeCliOutputFormat",
            "tst.w state.NativeCliHunkRequested",
            "move.w #constants.NATIVE_OUTPUT_FORMAT_HUNK, state.NativeCliOutputFormat",
            "tst.w state.NativeCliLstRequested",
            "move.w #constants.NATIVE_OUTPUT_FORMAT_LST, state.NativeCliOutputFormat",
        ]
    ));
}
