use super::*;

const TELEMETRY_INCLUDE: &str = "native/motorola68000/amigaos/debug/telemetry_macros.i";

fn telemetry_source(with_stub: bool, with_sites: bool) -> String {
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

fn assemble_telemetry_case(label: &str, source: &str, defines: &[String]) -> Vec<u8> {
    let root = workspace_root();
    let temp = create_temp_dir(label);
    let source_path = temp.join("telemetry_test.asm");
    fs::write(&source_path, source).expect("write telemetry test source");
    fs::write(
        temp.join("telemetry_macros.i"),
        fs::read(root.join(TELEMETRY_INCLUDE)).expect("read telemetry macro include"),
    )
    .expect("stage telemetry macro include");
    assemble_example_with_base_and_defines(&source_path, &temp, "telemetry_test", false, defines)
        .unwrap_or_else(|error| panic!("assemble {label}: {error}"));
    {
        let bytes =
            fs::read(temp.join("build/telemetry-test")).expect("read telemetry test output");
        fs::remove_dir_all(temp).expect("remove telemetry test scratch");
        bytes
    }
}

#[test]
fn native_telemetry_macros_are_gated_and_byte_transparent_when_disabled() {
    let baseline = assemble_telemetry_case(
        "native-telemetry-baseline",
        &telemetry_source(false, false),
        &[],
    );
    for defines in [
        vec![],
        vec!["OPFORGE_DEBUG_CONTRACTS".to_string()],
        vec!["OPFORGE_PROGRESS_RUNTIME_COUNTERS".to_string()],
    ] {
        let disabled = assemble_telemetry_case(
            "native-telemetry-disabled",
            &telemetry_source(false, true),
            &defines,
        );
        assert_eq!(
            disabled, baseline,
            "incomplete telemetry gates must emit zero bytes"
        );
    }

    let enabled = assemble_telemetry_case(
        "native-telemetry-enabled",
        &telemetry_source(true, true),
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
