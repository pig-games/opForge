use super::*;

const RUNTIME_TELEMETRY_INCLUDE: &str = "native/motorola68000/amigaos/debug/telemetry_macros.i";
const MEMORY_TELEMETRY_INCLUDE: &str = "native/motorola68000/amigaos/debug/memory_telemetry.i";

fn runtime_telemetry_source(with_stub: bool, with_sites: bool) -> String {
    let mut stub = String::new();
    if with_stub {
        stub.push_str(
            ".module debug.amigaos.runtime_profile\n.cpu 68020\n.pub\n\
             compactPrepare = 1\ncompactLookup = 2\n\
             .section code, kind=code\n",
        );
        for name in [
            "EnterVm",
            "RecordOpcode",
            "LeaveVm",
            "EnterService",
            "LeaveService",
            "RecordCandidate",
        ] {
            stub.push_str(&format!(
                "opforgeRuntimeProfile{name}V1 .block\n    rts\n.bend\n"
            ));
        }
        stub.push_str("recordCompact .block\n    rts\n.bend\n");
        stub.push_str(".endsection\n.endmodule\n");
    }
    let sites = if with_sites {
        r#"
    .TELEMETRY_VM_ENTER 1, 2
    .TELEMETRY_VM_OPCODE 1, 2
    .TELEMETRY_CANDIDATE 1
    .TELEMETRY_SERVICE_ENTER 1
    .TELEMETRY_SERVICE_LEAVE
    .TELEMETRY_VM_LEAVE
    .TELEMETRY_COMPACT runtime_profile.compactPrepare, d0
    .TELEMETRY_COMPACT runtime_profile.compactLookup, d1
    .TELEMETRY_COMPACT_WORD runtime_profile.compactLookup, d0
"#
    } else {
        ""
    };
    format!(
        r#".module telemetry.test
.cpu 68020
.region ram, 0, $ffff
.include "telemetry_macros.i"
.section code, kind=code
start .block
    moveq #7, d0
{sites}
    rts
.bend
.endsection
.place code in ram
.output "build/telemetry-test", format=bin, sections=code
.endmodule
{stub}.end
"#
    )
}

fn assemble_runtime_telemetry_case(label: &str, source: &str, defines: &[String]) -> Vec<u8> {
    let root = workspace_root();
    let temp = create_temp_dir(label);
    let source_path = temp.join("telemetry_test.asm");
    fs::write(&source_path, source).expect("write telemetry test source");
    fs::write(
        temp.join("telemetry_macros.i"),
        fs::read(root.join(RUNTIME_TELEMETRY_INCLUDE)).expect("read telemetry macro include"),
    )
    .expect("stage telemetry macro include");
    assemble_example_with_base_and_defines(&source_path, &temp, "telemetry_test", false, defines)
        .unwrap_or_else(|error| panic!("assemble {label}: {error}"));
    let bytes = fs::read(temp.join("build/telemetry-test")).expect("read telemetry test output");
    fs::remove_dir_all(temp).expect("remove telemetry test scratch");
    bytes
}

fn memory_telemetry_source(with_stub: bool, with_sites: bool) -> String {
    let stub = if with_stub {
        r#"
.module debug.amigaos.memory_profile
.cpu 68020
.section code, kind=code
.pub
allocate .block
    rts
.bend
release .block
    rts
.bend
failure .block
    rts
.bend
progress .block
    rts
.bend
phase .block
    rts
.bend
save .block
    rts
.bend
layout .block
    rts
.bend
work .block
    rts
.bend
clock .block
    rts
.bend
stage .block
    rts
.bend
detailBegin .block
    rts
.bend
detailEnd .block
    rts
.bend
bindSampleBegin .block
    rts
.bend
bindSampleEnd .block
    rts
.bend
templateWork .block
    rts
.bend
inputBegin .block
    rts
.bend
inputEnd .block
    rts
.bend
inputRead .block
    rts
.bend
tokenBegin .block
    rts
.bend
tokenOpcode .block
    rts
.bend
tokenWork .block
    rts
.bend
tokenScopeBegin .block
    rts
.bend
tokenScopeEnd .block
    rts
.bend
tokenScopeClose .block
    rts
.bend
.endsection
.endmodule
"#
    } else {
        ""
    };
    let sites = if with_sites {
        r#"
    .MEMORY_ALLOC d1
    .MEMORY_FREE d2
    .MEMORY_FAILURE #128, d3, d4, d5
    .MEMORY_PROGRESS a1, #1, d1, d2, d3
    .MEMORY_PHASE #2
    .MEMORY_SAVE a1
    .MEMORY_LAYOUT d3, d4, #4096
    .MEMORY_WORK #1, d0
    .MEMORY_CLOCK a1, #2
    .MEMORY_STAGE #2
    .MEMORY_DETAIL_BEGIN #0
    .MEMORY_BIND_SAMPLE_BEGIN
    .MEMORY_BIND_SAMPLE_END
    .MEMORY_DETAIL_END #0
    .MEMORY_TEMPLATE_WORK #2, d1
    .MEMORY_INPUT_BEGIN d2
    .MEMORY_INPUT_READ
    .MEMORY_INPUT_END d2
    .TOKEN_BEGIN d0
    .TOKEN_OPCODE d1
    .TOKEN_WORK #3, d2
    .TOKEN_SCOPE_BEGIN #0
    .TOKEN_SCOPE_END #0
    .TOKEN_SCOPE_CLOSE #0
"#
    } else {
        ""
    };
    format!(
        r#".module memory.telemetry.test
.cpu 68020
.region ram, 0, $ffff
.include "memory_telemetry.i"
.section code, kind=code
start .block
    moveq #7, d0
{sites}
    rts
.bend
.endsection
.place code in ram
.output "build/memory-telemetry-test", format=bin, sections=code
.endmodule
{stub}.end
"#
    )
}

fn assemble_memory_telemetry_case(label: &str, source: &str, defines: &[String]) -> Vec<u8> {
    let root = workspace_root();
    let temp = create_temp_dir(label);
    let source_path = temp.join("memory_telemetry_test.asm");
    fs::write(&source_path, source).expect("write memory telemetry test source");
    fs::write(
        temp.join("memory_telemetry.i"),
        fs::read(root.join(MEMORY_TELEMETRY_INCLUDE)).expect("read memory telemetry macro include"),
    )
    .expect("stage memory telemetry macro include");
    assemble_example_with_base_and_defines(
        &source_path,
        &temp,
        "memory_telemetry_test",
        false,
        defines,
    )
    .unwrap_or_else(|error| panic!("assemble {label}: {error}"));
    let bytes = fs::read(temp.join("build/memory-telemetry-test"))
        .expect("read memory telemetry test output");
    fs::remove_dir_all(temp).expect("remove memory telemetry test scratch");
    bytes
}

#[test]
fn native_telemetry_macros_are_gated_and_byte_transparent_when_disabled() {
    let baseline = assemble_runtime_telemetry_case(
        "native-telemetry-baseline",
        &runtime_telemetry_source(false, false),
        &[],
    );
    for defines in [
        vec![],
        vec!["OPFORGE_DEBUG_CONTRACTS".to_string()],
        vec!["OPFORGE_PROGRESS_RUNTIME_COUNTERS".to_string()],
    ] {
        let disabled = assemble_runtime_telemetry_case(
            "native-telemetry-disabled",
            &runtime_telemetry_source(false, true),
            &defines,
        );
        assert_eq!(
            disabled, baseline,
            "incomplete telemetry gates must emit zero bytes"
        );
    }

    let enabled = assemble_runtime_telemetry_case(
        "native-telemetry-enabled",
        &runtime_telemetry_source(true, true),
        &[
            "OPFORGE_DEBUG_CONTRACTS".to_string(),
            "OPFORGE_PROGRESS_RUNTIME_COUNTERS".to_string(),
        ],
    );
    assert_ne!(
        enabled, baseline,
        "enabled telemetry sites must assemble calls"
    );
}

#[test]
fn native_memory_telemetry_macros_are_byte_transparent_without_both_gates() {
    let baseline = assemble_memory_telemetry_case(
        "native-memory-telemetry-baseline",
        &memory_telemetry_source(false, false),
        &[],
    );
    for defines in [
        vec![],
        vec!["OPFORGE_DEBUG_CONTRACTS".to_string()],
        vec!["OPFORGE_MEMORY_TELEMETRY".to_string()],
        vec!["OPFORGE_BINDING_DETAIL_TELEMETRY".to_string()],
        vec!["OPFORGE_TEMPLATE_WORK_TELEMETRY".to_string()],
    ] {
        let disabled = assemble_memory_telemetry_case(
            "native-memory-telemetry-disabled",
            &memory_telemetry_source(false, true),
            &defines,
        );
        assert_eq!(
            disabled, baseline,
            "each incomplete memory-telemetry gate must emit exactly zero bytes"
        );
    }
}

#[test]
fn native_memory_telemetry_macros_assemble_against_passive_profile_api() {
    let baseline = assemble_memory_telemetry_case(
        "native-memory-telemetry-enabled-baseline",
        &memory_telemetry_source(true, false),
        &[
            "OPFORGE_DEBUG_CONTRACTS".to_string(),
            "OPFORGE_MEMORY_TELEMETRY".to_string(),
            "OPFORGE_PREPARATION_PROGRESS".to_string(),
        ],
    );
    let enabled = assemble_memory_telemetry_case(
        "native-memory-telemetry-enabled",
        &memory_telemetry_source(true, true),
        &[
            "OPFORGE_DEBUG_CONTRACTS".to_string(),
            "OPFORGE_MEMORY_TELEMETRY".to_string(),
            "OPFORGE_PREPARATION_PROGRESS".to_string(),
        ],
    );
    assert!(
        enabled.len() > baseline.len(),
        "enabled memory telemetry macros must emit preservation and profile-call code"
    );
}

#[test]
fn native_binding_detail_macros_are_independently_gated() {
    let source = memory_telemetry_source(true, true);
    let without_detail = source
        .lines()
        .filter(|line| !line.contains(".MEMORY_DETAIL_") && !line.contains(".MEMORY_BIND_SAMPLE_"))
        .collect::<Vec<_>>()
        .join("\n");
    let coarse = assemble_memory_telemetry_case(
        "native-binding-detail-coarse",
        &source,
        &[
            "OPFORGE_DEBUG_CONTRACTS".to_string(),
            "OPFORGE_MEMORY_TELEMETRY".to_string(),
        ],
    );
    let omitted = assemble_memory_telemetry_case(
        "native-binding-detail-omitted",
        &without_detail,
        &[
            "OPFORGE_DEBUG_CONTRACTS".to_string(),
            "OPFORGE_MEMORY_TELEMETRY".to_string(),
        ],
    );
    assert_eq!(coarse, omitted, "disabled detail probes emit no bytes");
    let detailed = assemble_memory_telemetry_case(
        "native-binding-detail-enabled",
        &source,
        &[
            "OPFORGE_DEBUG_CONTRACTS".to_string(),
            "OPFORGE_MEMORY_TELEMETRY".to_string(),
            "OPFORGE_BINDING_DETAIL_TELEMETRY".to_string(),
        ],
    );
    assert!(detailed.len() > coarse.len());
}

#[test]
fn native_template_work_macro_is_independently_gated() {
    let source = memory_telemetry_source(true, true);
    let without_counter = source.replace("    .MEMORY_TEMPLATE_WORK #2, d1\n", "");
    let common = [
        "OPFORGE_DEBUG_CONTRACTS".to_string(),
        "OPFORGE_MEMORY_TELEMETRY".to_string(),
    ];
    let disabled = assemble_memory_telemetry_case("template-work-disabled", &source, &common);
    let omitted =
        assemble_memory_telemetry_case("template-work-omitted", &without_counter, &common);
    assert_eq!(disabled, omitted);
    let mut enabled_defines = common.to_vec();
    enabled_defines.push("OPFORGE_TEMPLATE_WORK_TELEMETRY".to_string());
    let enabled =
        assemble_memory_telemetry_case("template-work-enabled", &source, &enabled_defines);
    assert!(enabled.len() > disabled.len());
}

#[test]
fn native_memory_progress_requires_its_extra_gate() {
    let with_progress = memory_telemetry_source(true, true);
    let without_progress = with_progress.replace("    .MEMORY_PROGRESS a1, #1, d1, d2, d3\n", "");
    let defines = [
        "OPFORGE_DEBUG_CONTRACTS".to_string(),
        "OPFORGE_MEMORY_TELEMETRY".to_string(),
    ];
    assert_eq!(
        assemble_memory_telemetry_case("memory-progress-disabled", &with_progress, &defines),
        assemble_memory_telemetry_case("memory-progress-absent", &without_progress, &defines),
    );
}

fn assembly_position_source(with_site: bool) -> String {
    let site = if with_site {
        r#"
    .ASSEMBLY_POSITION_CLEAR Position
    .ASSEMBLY_POSITION Position, d7, d5, SectionWords, 0, 2, 4
    .MEMORY_PROGRESS_BLOCK a1, #26, Position, AssemblyPosition.Pass, AssemblyPosition.Sweep, AssemblyPosition.Section
    .MEMORY_PROGRESS_RECORDS a1, #27, d1, d2, Position, AssemblyPosition.Count
"#
    } else {
        ""
    };
    format!(
        r#".module memory.assembly.position.test
.cpu 68020
.region ram, 0, $ffff
.include "memory_telemetry.i"
.section code, kind=code
start .block
    moveq #7, d0
{site}
    rts
.bend
.endsection
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
.section bss, kind=bss
Position .res byte, AssemblyPosition.Count+4
.endsection
.section data, kind=data
SectionWords .word 5, 2, 4
.endsection
.endif
.endif
.endif
.place code in ram
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
.place bss in ram
.place data in ram
.endif
.endif
.endif
.output "build/memory-telemetry-test", format=bin, sections=code
.endmodule
.module debug.amigaos.memory_profile
.cpu 68020
.pub
.section code, kind=code
progress .block
    rts
.bend
.endsection
.endmodule
.end
"#
    )
}

#[test]
fn native_assembly_position_capture_is_triple_gated_and_byte_transparent() {
    let with_site = assembly_position_source(true);
    for defines in [
        vec![],
        vec!["OPFORGE_DEBUG_CONTRACTS".to_string()],
        vec!["OPFORGE_MEMORY_TELEMETRY".to_string()],
        vec!["OPFORGE_PREPARATION_PROGRESS".to_string()],
        vec![
            "OPFORGE_DEBUG_CONTRACTS".to_string(),
            "OPFORGE_MEMORY_TELEMETRY".to_string(),
        ],
        vec![
            "OPFORGE_DEBUG_CONTRACTS".to_string(),
            "OPFORGE_PREPARATION_PROGRESS".to_string(),
        ],
        vec![
            "OPFORGE_MEMORY_TELEMETRY".to_string(),
            "OPFORGE_PREPARATION_PROGRESS".to_string(),
        ],
    ] {
        let baseline = assemble_memory_telemetry_case(
            "assembly-position-baseline",
            &assembly_position_source(false),
            &defines,
        );
        assert_eq!(
            assemble_memory_telemetry_case("assembly-position-disabled", &with_site, &defines,),
            baseline,
            "capture and storage must emit no bytes unless all three gates are present"
        );
    }

    let enabled_defines = [
        "OPFORGE_DEBUG_CONTRACTS".to_string(),
        "OPFORGE_MEMORY_TELEMETRY".to_string(),
        "OPFORGE_PREPARATION_PROGRESS".to_string(),
    ];
    let baseline = assemble_memory_telemetry_case(
        "assembly-position-enabled-baseline",
        &assembly_position_source(false),
        &enabled_defines,
    );
    let enabled =
        assemble_memory_telemetry_case("assembly-position-enabled", &with_site, &enabled_defines);
    assert!(
        enabled.len() > baseline.len(),
        "enabled capture must assemble its preservation and field-store instructions"
    );
}

#[test]
fn native_input_macros_are_independently_gated() {
    let source = memory_telemetry_source(true, true);
    let without_input = source
        .lines()
        .filter(|line| !line.contains(".MEMORY_INPUT_"))
        .collect::<Vec<_>>()
        .join("\n");
    let common = [
        "OPFORGE_DEBUG_CONTRACTS".to_string(),
        "OPFORGE_MEMORY_TELEMETRY".to_string(),
    ];
    let disabled = assemble_memory_telemetry_case("input-disabled", &source, &common);
    let omitted = assemble_memory_telemetry_case("input-omitted", &without_input, &common);
    assert_eq!(disabled, omitted);
    let mut enabled = common.to_vec();
    enabled.push("OPFORGE_INPUT_TELEMETRY".to_string());
    assert!(
        assemble_memory_telemetry_case("input-enabled", &source, &enabled).len() > disabled.len()
    );
}

fn selection_position_source(sites: &str) -> String {
    assembly_position_source(false)
        .replace("    moveq #7, d0", &format!("    moveq #7, d0\n{sites}"))
        .replace(
            "Position .res byte, AssemblyPosition.Count+4",
            "Position .res byte, SelectionPosition.Projection+4",
        )
        .replace(
            "SectionWords .word 5, 2, 4",
            "SectionWords .word $fedc\n    .byte $ab, $cd",
        )
}

#[test]
fn native_selection_position_is_triple_gated_and_preserves_setup_frame() {
    let sites = r#"
    .SELECTION_POSITION_CLEAR Position
    .SELECTION_POSITION_CANDIDATE Position, (a0), 2(a0)
    .SELECTION_POSITION_PROJECTION Position, 3(a0)
"#;
    let gates = [
        "OPFORGE_DEBUG_CONTRACTS",
        "OPFORGE_MEMORY_TELEMETRY",
        "OPFORGE_PREPARATION_PROGRESS",
    ];
    for mask in 0..7 {
        let defines = gates
            .iter()
            .enumerate()
            .filter(|(index, _)| mask & (1 << index) != 0)
            .map(|(_, gate)| (*gate).to_string())
            .collect::<Vec<_>>();
        let disabled = assemble_memory_telemetry_case(
            "selection-position-disabled",
            &selection_position_source(sites),
            &defines,
        );
        let omitted = assemble_memory_telemetry_case(
            "selection-position-omitted",
            &selection_position_source(""),
            &defines,
        );
        assert_eq!(
            disabled, omitted,
            "incomplete gates must emit no snapshot bytes"
        );
    }

    // Host assembly proof: every generated instruction matches a bounded,
    // balanced preservation sequence, including CCR saved before argument setup.
    // This does not execute the native code or qualify guest parity.
    let expected = r#"
    move.w ccr, -(sp)
    movem.l d0/a0, -(sp)
    moveq #-1, d0
    lea Position, a0
    move.l d0, 0(a0)
    move.l d0, 4(a0)
    move.l d0, 8(a0)
    movem.l (sp)+, d0/a0
    move.w (sp)+, ccr
    move.w ccr, -(sp)
    movem.l d0-d1/a0, -(sp)
    moveq #0, d0
    moveq #0, d1
    move.w (a0), d0
    move.b 2(a0), d1
    lea Position, a0
    move.l d0, 0(a0)
    move.l d1, 4(a0)
    moveq #-1, d0
    move.l d0, 8(a0)
    movem.l (sp)+, d0-d1/a0
    move.w (sp)+, ccr
    move.w ccr, -(sp)
    movem.l d0/a0, -(sp)
    moveq #0, d0
    move.b 3(a0), d0
    lea Position, a0
    move.l d0, 8(a0)
    movem.l (sp)+, d0/a0
    move.w (sp)+, ccr
"#;
    let defines = gates.map(str::to_string);
    assert_eq!(
        assemble_memory_telemetry_case(
            "selection-position-enabled",
            &selection_position_source(sites),
            &defines
        ),
        assemble_memory_telemetry_case(
            "selection-position-preservation",
            &selection_position_source(expected),
            &defines
        ),
        "enabled snapshot must match the full passive frame and base-relative stores"
    );
}
