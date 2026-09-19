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
    .MEMORY_PHASE #2
    .MEMORY_SAVE a1
    .MEMORY_LAYOUT d3, d4, #4096
    .MEMORY_WORK #1, d0
    .MEMORY_CLOCK a1, #2
    .MEMORY_STAGE #2
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
        ],
    );
    let enabled = assemble_memory_telemetry_case(
        "native-memory-telemetry-enabled",
        &memory_telemetry_source(true, true),
        &[
            "OPFORGE_DEBUG_CONTRACTS".to_string(),
            "OPFORGE_MEMORY_TELEMETRY".to_string(),
        ],
    );
    assert!(
        enabled.len() > baseline.len(),
        "enabled memory telemetry macros must emit preservation and profile-call code"
    );
}
