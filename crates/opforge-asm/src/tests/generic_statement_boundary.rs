use super::*;
use types::target_callbacks::{self as audit, Mode};

#[test]
fn unknown_dot_statements_never_fall_through_to_cpu_processing() {
    for cpu in [m6502_cpu_id, z80_cpu_id, m68000_cpu_id] {
        for source in [".not_a_directive (1+2)", ".lda 1", ".move (1+2)"] {
            let _audit = audit::install(Mode::Refuse);
            let (status, diagnostic) =
                assemble_line_diagnostic_with_runtime_mode(cpu, source, true);
            assert_eq!(status, LineStatus::Error);
            let diagnostic = diagnostic.expect("shared directive diagnostic");
            assert!(
                diagnostic.message().contains("Unknown directive"),
                "{diagnostic:?}"
            );
            assert!(diagnostic.fixits().is_empty());
            audit::check().unwrap();
            assert!(audit::snapshot().unwrap()["attempts"]
                .as_array()
                .unwrap()
                .is_empty());
        }
    }
}

#[test]
fn generic_directive_expressions_agree_in_rust_vm_lockstep() {
    for cpu in [m6502_cpu_id, z80_cpu_id, m68000_cpu_id] {
        for source in [".byte (1+2)", ".byte #1", ".word (1+2)*3"] {
            let _audit = audit::install(Mode::Refuse);
            let (status, diagnostic) =
                assemble_line_diagnostic_with_runtime_mode(cpu, source, true);
            assert_eq!(status, LineStatus::Ok, "{source}: {diagnostic:?}");
            audit::check().unwrap();
            assert!(audit::snapshot().unwrap()["attempts"]
                .as_array()
                .unwrap()
                .is_empty());
        }
    }
}
