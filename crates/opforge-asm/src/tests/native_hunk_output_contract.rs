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

#[test]
fn native_item36_hunk_abs32_transport_contract() {
    // Proof level B. Rust's package fixup VM remains the semantic authority;
    // native may only transport its neutral absolute-fixup fields into the
    // section model and HUNK_RELOC32 writer.
    let root = workspace_root();
    let rust_fixup = fs::read_to_string(root.join("crates/opforge-vm/src/fixup_vm.rs"))
        .expect("read Rust fixup authority");
    for required in [
        "if step.relocation == PortableRelocationKind::Absolute",
        "offset,",
        "width: step.width,",
        "kind: PortableOutputFixupKind::Absolute,",
        "encoded_addend: value as u32,",
    ] {
        assert!(
            rust_fixup.contains(required),
            "Rust fixup authority lacks {required:?}"
        );
    }

    let rust_hunk = fs::read_to_string(root.join("crates/opforge-vm/src/output_hunk.rs"))
        .expect("read Rust Hunk authority");
    for required in [
        "const HUNK_RELOC32: u32 = 0x0000_03ec;",
        "grouped_entries.sort_by_key",
        "offsets.sort_unstable();",
        "push_be_u32(bytes, HUNK_RELOC32);",
    ] {
        assert!(
            rust_hunk.contains(required),
            "Rust Hunk authority lacks {required:?}"
        );
    }

    let native_fixup = fs::read_to_string(
        root.join("native/motorola68000/amigaos/tkpkg/tkpkg_encode_service.asm"),
    )
    .expect("read native fixup transport");
    assert!(source_contains_in_order(
        &native_fixup,
        &["cmpi.w #1, 20(sp)", "tkpkgRecordOutputFixupV1"]
    ));
    assert!(source_contains_in_order(
        &native_fixup,
        &["tkpkgSemanticLoadFixupInputV4", "cmpi.w #7, d2"]
    ));
    assert!(source_contains_in_order(
        &native_fixup,
        &[
            "bsr.w tkpkgNormalizeOutputFixupLengthV1",
            "tkpkgNormalizeOutputFixupLengthV1",
            "lea buffers.SemanticOutputFixupOffsets, a0",
            "lea buffers.SemanticOutputFixupWidths, a0",
            "cmpi.l #buffers.LAST_ERROR_BUFFER_CAPACITY, d4",
            "move.w d4, d1",
        ]
    ));

    let driver = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native assembly driver");
    assert!(source_contains_in_order(
        &driver,
        &[
            "jsr tkpkg.adaptSelectedEncodeRequestV1",
            "recordSelectedOutputFixupsV1",
            "jsr layout.recordAbsoluteOutputFixupV1",
            "jsr eng.opasmEngineAppendImageBytesV1",
        ]
    ));
    let selected_fixup = driver
        .split("recordSelectedOutputFixupsV1\t.block")
        .nth(1)
        .and_then(|tail| tail.split(".bend  ; recordSelectedOutputFixupsV1").next())
        .expect("selected-fixup routine body");
    let directive_fixup = driver
        .split("recordDirectiveOutputFixupsV1\t.block")
        .nth(1)
        .and_then(|tail| tail.split(".bend  ; recordDirectiveOutputFixupsV1").next())
        .expect("directive-fixup routine body");
    assert!(
        !selected_fixup.contains("move.l d3, 0(a0, d1.l)"),
        "generic instruction assembly must retain absolute BIN bytes"
    );
    assert!(selected_fixup.contains("move.l d4, 0(a0, d1.l)"));
    assert!(
        !directive_fixup.contains("move.l d3, 0(a0, d5.l)"),
        "generic directive assembly must retain absolute BIN bytes"
    );
    assert!(directive_fixup.contains("move.l d4, 0(a0, d5.l)"));

    let hunk =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/hunk_output.asm"))
            .expect("read native Hunk writer");
    for required in [
        "HUNK_RELOC32 = $000003ec",
        "applyFlatRelocationAddendsV1",
        "applySelectedRelocationAddendsV1",
        "move.l d4, 0(a1, d3.l)",
        "move.l d4, 0(a2, d3.l)",
        "appendSelectedRelocationsV1",
        "jsr layout.getOutputFixupV1",
        "move.l #HUNK_RELOC32, (a3)+",
        "clr.l (a3)+",
    ] {
        assert!(
            hunk.contains(required),
            "native Hunk writer lacks {required:?}"
        );
    }
}

#[test]
fn native_item36_hunk_implicit_abs32_contract() {
    // Proof level B. Rust's missing-sections path owns one implicit CODE
    // segment; native retains the same source/target ownership with its
    // architecture-neutral NONE sentinel and emits target segment zero.
    // This does not execute native code or prove artifact bytes.
    let root = workspace_root();
    let rust_engine = fs::read_to_string(root.join("crates/opforge-asm/src/engine.rs"))
        .expect("read Rust implicit Hunk authority");
    for required in [
        "sections.contains_key(IMPLICIT_HUNK_CODE_SECTION_NAME)",
        "implicit_section_names = [IMPLICIT_HUNK_CODE_SECTION_NAME.to_string()]",
        "Self::hunk_output_relocation_disposition(output, &self.sections)",
    ] {
        assert!(
            rust_engine.contains(required),
            "Rust implicit-Hunk authority lacks {required:?}"
        );
    }

    let layout =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_layout.asm"))
            .expect("read native implicit-fixup projection");
    assert!(source_contains_in_order(
        &layout,
        &[
            "recordOutputFixupFlatSource",
            "cmpi.w #OPASM_LAYOUT_INDEX_NONE, d5",
            "jsr eng.opasmEngineGetSessionOriginV1",
            "recordOutputFixupStore",
        ]
    ));

    let hunk =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/hunk_output.asm"))
            .expect("read native implicit Hunk writer");
    assert!(source_contains_in_order(
        &hunk,
        &[
            "buildFlatCodeV1",
            "bsr.w appendFlatRelocationsV1",
            "move.l #HUNK_END, (a2)+",
        ]
    ));
    for required in [
        "appendFlatRelocationsV1",
        "move.l #HUNK_RELOC32, (a3)+",
        "clr.l (a3)+",
    ] {
        assert!(
            hunk.contains(required),
            "native implicit writer lacks {required:?}"
        );
    }
}

#[test]
fn native_item36_hunk_multi_group_contract() {
    // Proof level B. Rust sorts relocation groups by emitted target index and
    // offsets within each group. Native traverses the declared emitted-section
    // order and the pass-two fixup stream, which is monotonically increasing
    // by source offset. Level D proves the resulting bytes with interleaved
    // targets; this test only binds the two structural loops.
    let root = workspace_root();
    let rust = fs::read_to_string(root.join("crates/opforge-vm/src/output_hunk.rs"))
        .expect("read Rust relocation ordering authority");
    for required in ["grouped_entries.sort_by_key", "offsets.sort_unstable();"] {
        assert!(
            rust.contains(required),
            "Rust ordering authority lacks {required:?}"
        );
    }

    let native =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/hunk_output.asm"))
            .expect("read native grouped relocation writer");
    assert!(source_contains_in_order(
        &native,
        &[
            "relocTargetLoop",
            "selectedSectionInfoV1",
            "relocCountLoop",
            "move.l #HUNK_RELOC32, (a3)+",
            "relocOffsetLoop",
            "move.l d3, (a3)+",
            "relocTargetAdvance",
        ]
    ));
}

#[test]
fn native_item36_hunk_empty_target_index_contract() {
    // Proof level B. Rust removes empty initialized sections before assigning
    // relocation target indices. Native skips the same sections without
    // advancing its emitted-target counter. This does not execute native code
    // or prove the resulting artifact bytes.
    let root = workspace_root();
    let rust = fs::read_to_string(root.join("crates/opforge-vm/src/output_hunk.rs"))
        .expect("read Rust emitted-segment authority");
    assert!(source_contains_in_order(
        &rust,
        &[
            "if section.kind != SectionKind::Bss && section.bytes.is_empty()",
            "continue;",
            "segments.push(HunkSegmentInput",
            ".position(|target| target.name.eq_ignore_ascii_case(target_section))",
        ]
    ));

    let native =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/hunk_output.asm"))
            .expect("read native emitted-target traversal");
    assert!(source_contains_in_order(
        &native,
        &[
            "relocTargetLoop",
            "tst.l d1",
            "beq.w relocTargetNext",
            "relocTargetEmitted",
            "move.w d5, 2(sp)",
            "relocTargetAdvance",
            "addq.w #1, 4(sp)",
            "relocTargetNext",
        ]
    ));
}

#[test]
fn native_item36_hunk_directive_long_package_contract() {
    // Proof level B. Rust's CPU mode owns directive byte order and its Hunk
    // path turns target-backed `.long` values into absolute relocations.
    // Native requests the package's opaque absolute-32 semantic role and
    // transports its bytes/fixups without embedding a CPU endian decision.
    let root = workspace_root();
    let rust_line = fs::read_to_string(root.join("crates/opforge-asm/src/line.rs"))
        .expect("read Rust directive authority");
    for required in [
        "self.current_cpu_little_endian()",
        "OutputFixupRecord::hunk_abs32",
    ] {
        assert!(
            rust_line.contains(required),
            "Rust directive authority lacks {required:?}"
        );
    }
    let rust_eval = fs::read_to_string(root.join("crates/opforge-asm/src/asmline_eval.rs"))
        .expect("read Rust relocation-expression authority");
    assert!(rust_eval.contains("fn absolute_relocation(&self, expr: &Expr)"));

    let service = fs::read_to_string(
        root.join("native/motorola68000/amigaos/tkpkg/tkpkg_encode_service.asm"),
    )
    .expect("read native named-semantic service");
    assert!(source_contains_in_order(
        &service,
        &[
            "executeNamedSemanticProgramV1",
            "tkpkgEncodeFindAndExecuteSemanticProgramV2",
            "tkpkgNormalizeOutputFixupLengthV1",
            "lea buffers.LastErrorBuffer, a0",
        ]
    ));

    let driver = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native directive-long owner");
    for required in [
        ".byte \"fix.abs32\"",
        "emitPackageLongDirectiveForStatement",
        "jsr tkpkg.executeNamedSemanticProgramV1",
        "recordDirectiveOutputFixupsV1",
        "jsr layout.recordAbsoluteOutputFixupV1",
    ] {
        assert!(
            driver.contains(required),
            "native directive path lacks {required:?}"
        );
    }
}

#[test]
fn native_item36_hunk_emit_long_contract() {
    // Proof level B. Native recognizes Rust's typed `.emit long` surface,
    // excludes the type selector from sizing/operand iteration, and delegates
    // every value to the same package-owned absolute-32 semantic program.
    let root = workspace_root();
    let router = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opasm/opasm_directive_router.asm"),
    )
    .expect("read native directive router");
    for required in [
        "OPASM_DIRECTIVE_EMIT",
        "DirectiveEmitText",
        ".byte \"emit\", 0",
    ] {
        assert!(
            router.contains(required),
            "native `.emit` router lacks {required:?}"
        );
    }

    let driver = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native typed-long path");
    assert!(source_contains_in_order(
        &driver,
        &[
            "emitTypedLong",
            "emitDirectiveIsLongV1",
            "moveq #1, d4",
            "emitPackageLongDirectiveForStatement",
        ]
    ));
    assert!(driver.contains("subq.l #4, d3"));
}

#[test]
fn native_item36_hunk_symbolic_long_expression_contract() {
    // Proof level B. This binds native's bounded-source provenance projection
    // to Rust's frozen `.long`/`.emit long` expression decisions and Hunk-only
    // diagnostic boundary. It does not execute native code or prove bytes.
    let root = workspace_root();
    let rust = fs::read_to_string(root.join("crates/opforge-asm/src/line.rs"))
        .expect("read Rust symbolic-long authority");
    for required in [
        "fn hunk_abs32_target_section_for_expr(&self, expr: &Expr)",
        "fn hunk_abs32_target_section_for_data_expr(&self, expr: &Expr)",
        "fn expr_is_absolute_constant_symbol_expr(&self, expr: &Expr)",
        "format=hunk does not support this symbolic {directive} expression in v0.3",
        "format=hunk does not support this symbolic .emit long expression in v0.2",
    ] {
        assert!(rust.contains(required), "Rust authority lacks {required:?}");
    }

    let operand_eval =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_operand_eval.asm"))
            .expect("read native expression provenance owner");
    for required in [
        "classifyAbsoluteRelocationExpressionV1",
        "jsr eng.opasmEngineLastResolvedLabelIsTargetReferenceV1",
        "jsr eng.opasmEngineLabelIsAbsoluteConstantV1",
        "tst.w RelocScanRootMinusCount.l",
        "tst.w RelocScanAbsoluteSymbolCount.l",
    ] {
        assert!(
            operand_eval.contains(required),
            "native provenance classifier lacks {required:?}"
        );
    }

    let driver = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native symbolic-long driver");
    assert!(source_contains_in_order(
        &driver,
        &[
            "refreshAbsoluteConstantLabelsForPassTwo",
            "classifyAbsoluteRelocationExpressionV1",
            "opasmEngineSetLabelAbsoluteConstantV1",
            "emitPackageLongDirectiveForStatement",
            "layout.recordUnsupportedHunkFixupV1",
        ]
    ));

    let output =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/output.asm"))
            .expect("read native Hunk diagnostic facade");
    assert!(source_contains_in_order(
        &output,
        &[
            "rejectUnsupportedHunkFixupV1",
            "layout.getUnsupportedHunkFixupV1",
            "NativeHunkUnsupportedLongFixupText",
            "NativeHunkSectionNameScratch",
        ]
    ));
}
