//! Runtime directives and instruction guards share package-owned numeric state.
use super::*;

#[path = "binary_source_state_guards.rs"]
mod guards;

#[path = "binary_source_fpu_operands.rs"]
mod fpu_operands;

#[path = "binary_source_immediate_operands.rs"]
mod immediate_operands;

const CASES: &[(&str, &str)] = &[
    (
        "m68020",
        ".cpu m68020\n.fpu 68881\n fnop\n.fpu none\n nop\n.fpu \"68882\"\n.if 0\n.fpu none\n.endif\n fnop\n.cpu m68020\n.fpu 68881\n fnop\n",
    ),
    (
        "m68040",
        ".cpu m68040\n.fpu 68040\n fnop\n fmove fp0,fp1\n.fpu none\n nop\n",
    ),
    (
        "m68080",
        ".cpu m68080\non = 42\n fnop\n.apollo on\n mov3q #1,d0\n movs.b d0,d1\n.apollo off\n nop\n.apollo 1\n mov3q #-1,d2\n.byte on\n",
    ),
    (
        "m68020",
        ".cpu m68020\nconfigure .macro\n.fpu 68882\n.endmacro\n.configure\n fnop\n",
    ),
];

fn source(body: &str) -> String {
    format!(".module app\n{body}.endmodule\n.end\n")
}

const REJECTIONS: &[(&str, &str)] = &[
    (
        "m68020",
        ".cpu m68020\n.section scratch,kind=bss\n.res fpu,2\n.endsection\n",
    ),
    ("m68020", ".cpu m68020\n fnop\n"),
    ("m68020", ".cpu m68020\n.fpu 68881\n.fpu none\n fnop\n"),
    ("m68020", ".cpu m68020\n.fpu 68881\n.cpu m68020\n fnop\n"),
    ("m68000", ".cpu m68000\n.fpu 68881\n nop\n"),
    ("m68020", ".cpu m68020\n.apollo on\n nop\n"),
    ("m68080", ".cpu m68080\n mov3q #1,d0\n"),
    (
        "m68080",
        ".cpu m68080\n.apollo on\n.apollo off\n mov3q #1,d0\n",
    ),
    ("m68080", ".cpu m68080\n.apollo 2\n nop\n"),
    ("m68040", ".cpu m68040\n.fpu m68040\n nop\n"),
    ("m68080", ".cpu m68080\n.fpu m68080\n nop\n"),
    ("m68020", ".cpu m68020\n.fpu 68881,68882\n nop\n"),
    ("m68020", ".cpu m68020\n.fpu\n nop\n"),
];

#[test]
fn compact_state_rust_oracles() {
    for &(cpu, body) in CASES {
        let source = source(body);
        let expected = oracle(&[("main.asm", &source)]).unwrap();
        assert!(!expected.is_empty(), "{cpu}: {body}");
    }
    for &(cpu, body) in REJECTIONS {
        let source = source(body);
        assert!(oracle(&[("main.asm", &source)]).is_err(), "{cpu}: {body}");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; package state transitions and guarded encodings"]
fn compact_state_transitions_fs_uae() {
    let mut failed = Vec::new();
    for &(cpu, body) in CASES {
        let source = source(body);
        let expected = oracle(&[("main.asm", &source)]).unwrap();
        eprintln!("COMPACT_STATE_POSITIVE cpu={cpu} source={source:?}");
        let result = std::panic::catch_unwind(|| {
            compact_cli_cpu(
                &[("main.asm", &source)],
                &[],
                &[],
                Some(&expected),
                false,
                cpu,
            )
        });
        if result.is_err() {
            failed.push((cpu, body));
        }
    }
    assert!(failed.is_empty(), "native state cases failed: {failed:?}");
}

#[test]
#[ignore = "requires configured FS-UAE; illegal states and disabled instruction guards"]
fn compact_state_rejections_fs_uae() {
    let mut failed = Vec::new();
    for &(cpu, body) in REJECTIONS {
        let source = source(body);
        assert!(oracle(&[("main.asm", &source)]).is_err(), "{cpu}: {body}");
        eprintln!("COMPACT_STATE_NEGATIVE cpu={cpu} source={source:?}");
        let result = std::panic::catch_unwind(|| {
            compact_cli_cpu(&[("main.asm", &source)], &[], &[], None, false, cpu);
        });
        if result.is_err() {
            failed.push((cpu, body));
        }
    }
    assert!(
        failed.is_empty(),
        "native rejection cases failed: {failed:?}"
    );
}

const MAPPED: &[(&str, &str)] = &[
    ("main.asm", ".module app\n.cpu m68020\n.use dep map { code -> app_code }\n.fpu 68881\n.for 0\n.fpu none\n.endfor\n.region image,$1000,$10ff,align=1\n.section app_code,kind=code,align=1\n fnop\n.word dep.entry\n.endsection\n.place app_code in image\n.endmodule\n.end\n"),
    ("dep.asm", ".module dep\n.cpu m68020\n.fpu 68881\n.pub\n.section code,kind=code,logical,align=1\nentry .block\n fnop\n.bend\n.endsection\n.endmodule\n.end\n"),
];

#[test]
fn compact_state_mapped_loop_rust_oracle() {
    assert_eq!(
        oracle(MAPPED).unwrap(),
        [0xf2, 0x80, 0, 0, 0x10, 6, 0xf2, 0x80, 0, 0]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; skipped loops also govern mapped state replay"]
fn compact_state_mapped_loop_fs_uae() {
    let expected = oracle(MAPPED).unwrap();
    compact_cli_cpu(MAPPED, &[], &[], Some(&expected), false, "m68020");
}
