//! Immutable match-arm ownership participates in layout replay like if guards.
use super::*;

fn bytes(cpu: &str, body: &str) -> Vec<u8> {
    let source = format!(".cpu {cpu}\n{body}");
    let assembler = run_passes(&source.lines().collect::<Vec<_>>());
    assembler
        .image()
        .entries()
        .unwrap()
        .into_iter()
        .map(|(_, byte)| byte)
        .collect()
}

#[test]
fn match_guard_replay_selects_forward_case_and_default_loop_owners() {
    for cpu in ["m6502", "m68000", "m68020"] {
        for (value, expected) in [(1, vec![7, 7]), (2, vec![8, 8, 8])] {
            let source = format!(".match later\n.case 1\ncount=2\n.for count\n.byte 7\n.endfor\n.default\ncount=3\n.for count\n.byte 8\n.endfor\n.endmatch\nlater={value}");
            assert_eq!(bytes(cpu, &source), expected);
        }
        assert_eq!(bytes(cpu, ".match 2\n.case later\ncount=2\n.for count\n.byte 7\n.endfor\n.default\ncount=3\n.for count\n.byte 8\n.endfor\n.endmatch\nlater=2"), [7, 7]);
    }
}

#[test]
fn match_guard_replay_short_circuits_cases_and_retains_scalar_projection() {
    for cpu in ["m6502", "m68000", "m68020"] {
        assert_eq!(bytes(cpu, ".match later\n.case 1,missing\n.for 2\n.byte 7\n.endfor\n.case missing\n.byte missing\n.default\n.for 3\n.byte 8\n.endfor\n.endmatch\nlater=1"), [7, 7]);
        assert_eq!(bytes(cpu, ".match later\n.case 0\n.for 2\n.byte 7\n.endfor\n.default\n.for 3\n.byte 8\n.endfor\n.endmatch\nlater=((1<<16)<<16)"), [7, 7]);
    }
}

#[test]
fn match_guard_replay_contextual_selectors_do_not_certify_layout_changes() {
    for declaration in ["later .var 1", "later:"] {
        let source = format!(".byte 1\n.match later\n.case 1,3,4\n.for 2\n.byte 7\n.endfor\n.default\n.for 3\n.byte 8\n.endfor\n.endmatch\n{declaration}");
        let assembler = run_pass1(&source.lines().collect::<Vec<_>>());
        assert!(
            assembler.diagnostics.iter().any(|diagnostic| diagnostic
                .error
                .message()
                .contains("loop iteration count changed between passes")),
            "contextual selector accepted: {declaration}"
        );
    }
}

#[test]
fn match_control_labels_publish_under_enclosing_activity() {
    for separator in [":", ""] {
        let input = format!("opening{separator} .match opening\narm{separator} .case opening\n.byte arm\nending{separator} .endmatch\n.byte ending");
        assert_eq!(bytes("m68020", &input), [0, 1]);
    }
    let assembler = run_passes(&[
        ".if 0",
        "hidden: .match missing",
        "arm: .case missing",
        ".byte missing",
        "ending: .endmatch",
        ".endif",
        ".byte 7",
    ]);
    for label in ["hidden", "arm", "ending"] {
        assert_eq!(
            assembler.symbols().lookup(label),
            None,
            "inactive control published {label}"
        );
    }
}
