// SPDX-License-Identifier: GPL-3.0-or-later
use super::*;

fn native_api_reference_listing(module: &str, routines: &[&str], label: &str) -> String {
    let out_dir = create_temp_dir(label);
    let input = out_dir.join("api_probe.asm");
    let listing = out_dir.join("api_probe.lst");
    // A callable API is retained through an actual address reference, rather
    // than assuming every public dependency block survives a module import.
    let mut source =
        format!(".module proof.api\n.cpu 68020\n.use {module} as api\n.section refs, kind=data\n");
    for routine in routines {
        source.push_str(&format!(" .long api.{routine}\n"));
    }
    source.push_str(".endsection\n.endmodule\n");
    write_file(&input, &source);
    let mut args = vec![
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--list".to_string(),
        listing.to_string_lossy().into_owned(),
    ];
    let amigaos = workspace_root().join("native/motorola68000/amigaos");
    for directory in [
        "tkpkg", "tkvm", "prvm", "exprvm", "opcore", "opasm", "debug",
    ] {
        args.extend([
            "-M".into(),
            amigaos.join(directory).to_string_lossy().into_owned(),
        ]);
    }
    args.extend([
        "-I".into(),
        amigaos.join("debug").to_string_lossy().into_owned(),
    ]);
    run_with_cli_with_context(&Cli::parse_from(args)).expect("assemble native API address probe");
    fs::read_to_string(listing).expect("read native API address probe listing")
}

#[test]
fn motorola68020_tkpkg_module_surface_assembles_composed_runtime_boundary() {
    let repo_root = workspace_root();
    let asm_path =
        repo_root.join("native/motorola68000/amigaos/test-harnesses/tkpkg/tkpkg_entry.asm");
    let out_dir = create_temp_dir("m68000-tkpkg-entry");
    let entry_source = tkpkg_amigaos_source("tkpkg_entry.asm");

    if let Err(err) = assemble_example(&asm_path, &out_dir, false) {
        let detail = assemble_example_error(&asm_path).unwrap_or_else(|| err.clone());
        panic!("assemble tkpkg entry example: {detail}");
    }

    assert!(source_contains_in_order(
        &entry_source,
        &[
            "tkpkgEntryDispatchV1 .block",
            "JSR service.dispatchV1",
            "RTS"
        ]
    ));
    assert!(source_contains_in_order(
        &entry_source,
        &[
            "tkpkgEntryBootstrapV1 .block",
            "lea buffers.ControlBlockV1, a0",
            "moveq #abi.ENTRY_ORD_INIT, d0",
            "jsr service.dispatchV1",
            "rts",
        ]
    ));

    let listing = fs::read_to_string(out_dir.join("tkpkg_entry.lst")).expect("read tkpkg listing");
    assert!(listing.contains(".cpu 68020"));
    assert!(listing.contains("main.tkpkgEntryDispatchV1"));
    assert!(listing.contains("main.tkpkgEntryBootstrapV1"));
    assert!(listing.contains("tkpkg.amigaos.service.dispatchV1"));
    assert!(listing.contains("tkpkg.amigaos.service.tkpkgServicePrepareRequestV1"));
    let api_listing = native_api_reference_listing(
        "tkpkg.amigaos.service_status",
        &["setRuntimeErrorV1"],
        "tkpkg-runtime-error-api",
    );
    // Runtime status is now owned by the status service; the facade delegate is private.
    assert!(api_listing.contains("tkpkg.amigaos.service_status.setRuntimeErrorV1"));
    assert!(listing.contains("tkpkg.amigaos.runtime_context.getAbiVersionV1"));
    let symbol_listing = native_api_reference_listing(
        "tkpkg.amigaos.engine_context_adapter",
        &["lookupSymbolV1"],
        "tkpkg-symbol-lookup-api",
    );
    assert!(symbol_listing.contains("tkpkg.amigaos.engine_context_adapter.lookupSymbolV1"));
    assert!(listing.contains("tkpkg.amigaos.buffers.ControlBlockV1"));
    assert!(listing.contains("tkpkg.amigaos.buffers.LastErrorBuffer"));
    assert!(listing.contains("tkpkg.amigaos.buffers.StoredLastErrorLen"));
    assert!(listing.contains("tkpkg.amigaos.package_loader.tkpkgPackageLoaderLoadV1"));
    assert!(listing.contains("tkpkg.amigaos.package_loader.validateToc"));
    assert!(listing.contains("tkpkg.amigaos.pipeline.tkpkgPipelineSetActiveV1"));
    assert!(listing.contains("tkpkg.amigaos.pipeline.resolveTokenizerVmLocatorV1"));
    assert!(listing.contains("tkpkg.amigaos.state_service.initializeActiveV1"));
    assert!(listing.contains("tkpkg.amigaos.state_service.resetActiveV1"));
    let state_listing = native_api_reference_listing(
        "tkpkg.amigaos.state_service",
        &["applyDirectiveV1", "getFlagV1", "requirementAllowsV1"],
        "tkpkg-state-api",
    );
    assert!(state_listing.contains("tkpkg.amigaos.state_service.applyDirectiveV1"));
    assert!(state_listing.contains("tkpkg.amigaos.state_service.getFlagV1"));
    assert!(state_listing.contains("tkpkg.amigaos.state_service.requirementAllowsV1"));
    assert!(listing.contains("tkpkg.amigaos.buffers.StvmChunkOffsetLo"));
    assert!(listing.contains("tkpkg.amigaos.buffers.ActiveStateProgramPtr"));
    assert!(listing.contains("tkpkg.amigaos.token_policy.resolveLocatorV1"));
    assert!(listing.contains("tkpkg.amigaos.tokenizer_vm.tkpkgTokenizerVmTokenizeLineV1"));
    assert!(listing.contains("tkvm.amigaos.runtime.tkvmRun68000"));

    let payload_path = example_output_payload_path(&out_dir, "tkpkg_entry", "hunk");
    let payload = fs::read(payload_path).expect("read tkpkg hunk payload");
    assert!(
        payload
            .windows("OPFORGE-TKPKG-ABI-V1".len())
            .any(|window| window == b"OPFORGE-TKPKG-ABI-V1"),
        "expected ABI marker string in tkpkg Hunk payload"
    );
    assert!(
        payload
            .windows("TKPKG-WIRE-CONTRACT-V1".len())
            .any(|window| window == b"TKPKG-WIRE-CONTRACT-V1"),
        "expected wire contract marker string in tkpkg Hunk payload"
    );
    assert!(
        payload
            .windows("OTR002: bad request".len())
            .any(|window| window == b"OTR002: bad request"),
        "expected bad request error marker in tkpkg Hunk payload"
    );
    assert!(
        payload
            .windows("OTR003: bad control".len())
            .any(|window| window == b"OTR003: bad control"),
        "expected bad control error marker in tkpkg Hunk payload"
    );
    assert!(
        payload
            .windows("OTR901: unimplemented".len())
            .any(|window| window == b"OTR901: unimplemented"),
        "expected runtime error marker in tkpkg Hunk payload"
    );
    assert!(
        payload
            .windows("OPC001: invalid package magic".len())
            .any(|window| window == b"OPC001: invalid package magic"),
        "expected invalid package magic marker in tkpkg Hunk payload"
    );
    assert!(
        payload
            .windows("OPC006: missing required chunk".len())
            .any(|window| window == b"OPC006: missing required chunk"),
        "expected missing required chunk marker in tkpkg Hunk payload"
    );
    assert!(
        payload
            .windows("OTR001: set_pipeline requires load_package".len())
            .any(|window| window == b"OTR001: set_pipeline requires load_package"),
        "expected missing package marker in tkpkg Hunk payload"
    );
    assert!(
        payload
            .windows("OTR003: missing tokenizer policy".len())
            .any(|window| window == b"OTR003: missing tokenizer policy"),
        "expected missing tokenizer policy marker in tkpkg Hunk payload"
    );
    assert!(
        payload
            .windows("OTR004: unresolved package cpu id".len())
            .any(|window| window == b"OTR004: unresolved package cpu id"),
        "expected unresolved cpu marker in tkpkg Hunk payload"
    );
    assert!(
        payload
            .windows("OTR004: unresolved package family".len())
            .any(|window| window == b"OTR004: unresolved package family"),
        "expected unresolved family marker in tkpkg Hunk payload"
    );
    assert!(
        payload
            .windows("OTR004: unresolved package dialect".len())
            .any(|window| window == b"OTR004: unresolved package dialect"),
        "expected unresolved dialect marker in tkpkg Hunk payload"
    );
    assert!(
        payload
            .windows("OTR001: missing tokenizer VM program".len())
            .any(|window| window == b"OTR001: missing tokenizer VM program"),
        "expected missing tokenizer VM marker in tkpkg Hunk payload"
    );
}

#[test]
fn motorola68020_tkpkg_tokenizer_parity_module_surface_assembles_entry_runtime() {
    let buffers_source = tkpkg_amigaos_source("tkpkg_buffers.asm");
    let tokenizer_source = tkpkg_amigaos_source("tkpkg_tokenizer_vm.asm");
    let local_tokvm_source = tokvm_amigaos_source("tkvm_runtime.asm");
    let char_predicates_source = tokvm_amigaos_source("tkvm_char_predicates.asm");
    let listing = tokvm_interpreter_listing("m68000-tkpkg-tokenizer-runtime-surface");
    let control_listing = native_api_reference_listing(
        "tkvm.amigaos.control",
        &["tkvmSetProgramStateTable68000", "tkvmReadLastFailure68000"],
        "tkvm-tokenizer-control-api",
    );

    assert!(tkpkg_source_contains(
        &buffers_source,
        "LAST_ERROR_BUFFER_CAPACITY = 4096"
    ));
    assert!(tkpkg_source_contains(
        &buffers_source,
        "TOKEN_BUFFER_CAPACITY = 64"
    ));
    assert!(tkpkg_source_contains(
        &tokenizer_source,
        ".use tkvm.amigaos.runtime"
    ));
    assert!(tkpkg_source_contains(
        &tokenizer_source,
        ".use tkvm.amigaos.control"
    ));
    assert!(tkpkg_source_contains(
        &tokenizer_source,
        "MOVEQ #0,D1\n        MOVE.W #buffers.TOKEN_BUFFER_CAPACITY,D1"
    ));
    assert!(tkpkg_source_contains(
        &tokenizer_source,
        "MOVEQ #0,D2\n        MOVE.W #buffers.TOKEN_SCRATCH_CAPACITY,D2"
    ));
    assert!(tkpkg_source_contains(
        &tokenizer_source,
        "ActiveTokenizerVmStartStateLo"
    ));
    assert!(tkpkg_source_contains(
        &tokenizer_source,
        "ActiveTokenizerVmStartStateHi"
    ));
    assert!(tkpkg_source_contains(
        &tokenizer_source,
        "ActiveTokenizerVmStateCountHi"
    ));
    assert!(tkpkg_source_contains(
        &tokenizer_source,
        "ActiveTokenizerVmMaxErrorsPerLine"
    ));
    assert!(tkpkg_source_contains(
        &tokenizer_source,
        "readStringIntoSlot\t.block"
    ));
    assert!(tkpkg_source_contains(&tokenizer_source, "statusVmFailure:"));
    assert!(tokvm_source_contains(
        &local_tokvm_source,
        "opcodeScanIdentifier"
    ));
    assert!(control_listing.contains("tkvm.amigaos.control.tkvmSetProgramStateTable68000"));
    assert!(tokvm_source_contains(&local_tokvm_source, "opcodeSetState"));
    assert!(local_tokvm_source.contains(
        "move.l state.TkvmProgramStateTablePtr, d1\n\tbeq.w invalidProgramAtCursor\n\tmovea.l d1, a1\n\tadd.l d0, d0\n\tadd.l d0, d0\n\tadda.l d0, a1\n\tmove.l (a1), d0"
    ));
    assert!(char_predicates_source.contains("tkvmIsWhitespace\t.block"));
    assert!(listing.contains("tkvm.amigaos.scanner.commit"));
    assert!(control_listing.contains("tkvm.amigaos.control.tkvmReadLastFailure68000"));
}

#[test]
fn motorola68020_tokvm_interpreter_supports_configured_state_table_entrypoints() {
    let listing = tokvm_interpreter_listing("m68000-tokvm-state-surface");
    let control_listing = native_api_reference_listing(
        "tkvm.amigaos.control",
        &["tkvmSetProgramStateTable68000"],
        "tkvm-state-control-api",
    );

    assert!(listing.contains("tkvm.amigaos.demo_program.DemoStateEntryOffsets"));
    assert!(listing.contains("tkvm.amigaos.state.TkvmProgramStateTablePtr"));
    assert!(listing.contains("tkvm.amigaos.state.TkvmProgramStateCount"));
    assert!(listing.contains("tkvm.amigaos.state.TkvmProgramStartState"));
    assert!(control_listing.contains("tkvm.amigaos.control.tkvmSetProgramStateTable68000"));
    assert!(listing.contains("tkvm.amigaos.runtime.tkvmRun68000.newlineScanDone"));
    assert!(listing.contains("tkvm.amigaos.runtime.tkvmRun68000.opcodeSetState"));
}

#[test]
fn motorola68020_tokvm_interpreter_records_operand_aware_vm_failures() {
    let source = tokvm_amigaos_source("tkvm_runtime.asm");
    let listing = tokvm_interpreter_listing("m68000-tokvm-failure-record-surface");
    let control_listing = native_api_reference_listing(
        "tkvm.amigaos.control",
        &["tkvmReadLastFailure68000"],
        "tkvm-failure-control-api",
    );

    assert!(
        tokvm_source_contains(
            &source,
            "TK_VM_FAILURE_KIND_NONE         = 0\nTK_VM_FAILURE_KIND_FAIL         = 1\nTK_VM_FAILURE_KIND_EMIT_DIAG    = 2"
        ),
        "expected tokvm to publish symbolic failure-kind ids for tkpkg-side error formatting"
    );
    assert!(control_listing.contains("tkvm.amigaos.control.tkvmReadLastFailure68000"));
    assert!(listing.contains("tkvm.amigaos.state.TkvmLastFailureKind"));
    assert!(listing.contains("tkvm.amigaos.state.TkvmLastFailureOperand"));
    assert!(
        tokvm_source_contains(
            &source,
            ".long opcodeJumpIfClass\n        .long opcodeFail\n        .long opcodeEmitFailure"
        ),
        "expected the dispatch table to route Fail and EmitDiag into dedicated native handlers"
    );
    assert!(
        tokvm_source_contains(
            &source,
            "opcodeFail:\n        MOVE.L A0,D0\n        SUB.L A3,D0\n        ADDQ.L #1,D0\n        CMP.L D7,D0\n        BHI invalidProgramAtCursor"
        ),
        "expected Fail to read one inline reason operand and report VM failure"
    );
    assert!(
        tokvm_source_contains(
            &source,
            "opcodeEmitFailure:\n        MOVE.L A0,D0\n        SUB.L A3,D0\n        ADDQ.L #1,D0\n        CMP.L D7,D0\n        BHI invalidProgramAtCursor"
        ),
        "expected EmitDiag to read one inline diagnostic-slot operand and report VM failure"
    );
    assert!(
        source_contains_in_order(
            &source,
            &["tkvmRun68000", ".block", "movem.l d4-d7/a4-a6, -(sp)"]
        ),
        "expected tkvmRun68000 to remain the stable entrypoint for the failure-recording path"
    );
    assert!(
        source.contains("TkvmLastFailureKind"),
        "expected tokvm to retain a dedicated last-failure kind slot"
    );
    assert!(
        source.contains("TkvmLastFailureOperand"),
        "expected tokvm to retain a dedicated last-failure operand slot"
    );
    assert!(
        source.contains("clr.w state.TkvmLastFailureKind")
            && source.contains("clr.w state.TkvmLastFailureOperand"),
        "expected tkvmRun68000 to clear stale failure metadata before each execution"
    );
}
