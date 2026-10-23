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
    args.extend(["-M".into(), amigaos.to_string_lossy().into_owned()]);
    args.extend([
        "-I".into(),
        amigaos.join("debug").to_string_lossy().into_owned(),
    ]);
    run_with_cli_with_context(&Cli::parse_from(args)).expect("assemble native API address probe");
    fs::read_to_string(listing).expect("read native API address probe listing")
}

#[test]
fn motorola68020_tokvm_interpreter_supports_configured_state_table_entrypoints() {
    let listing = tokvm_interpreter_listing("m68000-tokvm-state-surface");
    let control_listing = native_api_reference_listing(
        "opforge.vm.tkvm.control",
        &["tkvmSetProgramStateTable68000"],
        "tkvm-state-control-api",
    );

    assert!(listing.contains("opforge.vm.tkvm.demo_program.DemoStateEntryOffsets"));
    assert!(listing.contains("opforge.vm.tkvm.state.TkvmProgramStateTablePtr"));
    assert!(listing.contains("opforge.vm.tkvm.state.TkvmProgramStateCount"));
    assert!(listing.contains("opforge.vm.tkvm.state.TkvmProgramStartState"));
    assert!(control_listing.contains("opforge.vm.tkvm.control.tkvmSetProgramStateTable68000"));
    assert!(listing.contains("opforge.vm.tkvm.runtime.tkvmRun68000.newlineScanDone"));
    assert!(listing.contains("opforge.vm.tkvm.runtime.tkvmRun68000.opcodeSetState"));
}

#[test]
fn motorola68020_tokvm_interpreter_records_operand_aware_vm_failures() {
    let source = tokvm_amigaos_source("vm/tkvm/runtime.asm");
    let listing = tokvm_interpreter_listing("m68000-tokvm-failure-record-surface");
    let control_listing = native_api_reference_listing(
        "opforge.vm.tkvm.control",
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
    assert!(control_listing.contains("opforge.vm.tkvm.control.tkvmReadLastFailure68000"));
    assert!(listing.contains("opforge.vm.tkvm.state.TkvmLastFailureKind"));
    assert!(listing.contains("opforge.vm.tkvm.state.TkvmLastFailureOperand"));
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
