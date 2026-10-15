//! Source-selected deferred features must retain zero overhead with partial gates.
use super::*;

#[test]
fn native_deferred_telemetry_source_flags_require_complete_observer_gates() {
    let root = workspace_root();
    let temp = create_temp_dir("deferred-source-telemetry");
    for include in ["deferred_plan_telemetry.i", "deferred_binding_report.i"] {
        fs::copy(
            root.join("native/motorola68000/amigaos/debug")
                .join(include),
            temp.join(include),
        )
        .expect("stage deferred telemetry include");
    }
    let gates = [
        "OPFORGE_DEBUG_CONTRACTS",
        "OPFORGE_MEMORY_TELEMETRY",
        "OPFORGE_PREPARATION_PROGRESS",
    ];
    for (acquire, lines) in [(0, 0), (1, 0), (1, 1)] {
        // Deliberately absent State fields, tag constants, frontend routines and
        // MEMORY_PROGRESS prove disabled helpers do not resolve their arguments.
        let source = format!(
            r#".module deferred.telemetry.test
.cpu 68020
.region ram, 0, $ffff
DEFER_MACRO_PLANS = 1
ACQUIRE_MACRO_CANDIDATES = {acquire}
DEFER_MACRO_LINES = {lines}
.include "deferred_plan_telemetry.i"
.include "deferred_binding_report.i"
.section code, kind=code
start .block
    moveq #7, d0
    .DEFERRED_COUNTER a0, Missing.Field, Missing.Amount
    .DEFERRED_CLEAR a0
    .DEFERRED_ACQUISITION_COUNTER a0, Missing.Acquired, Missing.Tag
    .DEFERRED_LINE_COUNTER a0, Missing.Lines, Missing.Tag
    .DEFERRED_BINDING_REPORT Missing.Dos, Missing.Session
    .DEFERRED_REPORT_PAIR Missing.Dos, Missing.Session, Missing.Routine, Missing.Phase
    rts
.bend
.endsection
.place code in ram
.output "build/deferred-test", format=bin, sections=code
.endmodule
.end
"#
        );
        let source_path = temp.join("deferred_test.asm");
        fs::write(&source_path, source).expect("write deferred telemetry test");
        for mask in 0..7 {
            let defines = gates
                .iter()
                .enumerate()
                .filter(|(bit, _)| mask & (1 << bit) != 0)
                .map(|(_, gate)| (*gate).to_string())
                .collect::<Vec<_>>();
            assemble_example_with_base_and_defines(
                &source_path,
                &temp,
                "deferred_test",
                false,
                &defines,
            )
            .unwrap_or_else(|error| {
                panic!("deferred telemetry acquire={acquire} lines={lines} gates={mask}: {error}")
            });
            let bytes = fs::read(temp.join("build/deferred-test")).expect("read deferred output");
            assert_eq!(
                bytes,
                [0x70, 0x07, 0x4e, 0x75],
                "partial gates must emit only moveq/rts: acquire={acquire} lines={lines} gates={mask}"
            );
        }
    }
    fs::remove_dir_all(temp).expect("remove deferred telemetry scratch");
}
