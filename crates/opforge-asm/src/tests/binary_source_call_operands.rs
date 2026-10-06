//! Three operands and indirect call children retain canonical package semantics.
use super::*;

const EXAMPLE: &str = include_str!("../../../../examples/motorola68000/68030_carry_forward.asm");

// This deliberately transformed native input probes numeric execution only.
// Rust cannot parse the indirect call children directly from this spelling:
// its family normalization creates them from colon pairs. This is not
// unchanged-source parity; the original example remains unchanged.
fn transport_example() -> String {
    EXAMPLE.replace(
        "D0:D1,D2:D3,(A0):(A1)",
        ".pair(D0,D1),.pair(D2,D3),.pair((A0),(A1))",
    )
}

const VALID: &str = " cas2.w .pair(d0,d1),.pair(d2,d3),.pair((a0),(a1))\n cas2.l .pair(d7,d6),.pair(d5,d4),.pair((a7),(a6))\n pack d0,d1,#1\n pack -(a0),-(a1),#-1\n unpk d2,d3,#2\n";
const INVALID: &[&str] = &[
    "cas2.w .pair(a0,d1),.pair(d2,d3),.pair((a0),(a1))",
    "cas2.w .pair(d0,d1),.pair(d2,d3),.pair((d0),(a1))",
    "cas2.w .pair(d0,d1),.pair(d2,d3),.pair(a0,a1)",
    "cas2.w .pair(d0),.pair(d2,d3),.pair((a0),(a1))",
    "cas2.w .pair(d0,d1),.pair(d2,d3),.pair((a0))",
    "cas2.w .pair(d0,d1),.pair(d2,d3),.pair((a0),(a1)),d0",
    "pack d0,d1,#65536",
];

const REGISTER_IMMEDIATE: &str = " link.w a6,#-8\n link.l a6,#-8\n link.l a0,#$12345678\n";
const REGISTER_IMMEDIATE_INVALID: &[(&str, &str)] = &[
    ("m68020", "link.l d0,#-8"),
    ("m68020", "link.l a6,-8"),
    ("m68020", "sub.w d0,#1"),
    ("m68000", "link.l a6,#-8"),
];

const REGISTER_DIRECT: &str = " cas.b d0,d1,(a0)\n cas.w d2,d3,(a1)\n cas.l d4,d5,(a2)\n";
const REGISTER_DIRECT_INVALID: &[(&str, &str)] = &[
    ("m68020", "cas.w a0,d1,(a2)"),
    ("m68020", "cas.w d0,d1,d2"),
    ("m68000", "cas.w d0,d1,(a0)"),
];

#[test]
fn compact_register_direct_rust_oracles() {
    let text = source(&format!(".cpu m68020\n{REGISTER_DIRECT}"));
    assert_eq!(oracle(&[("main.asm", &text)]).unwrap().len(), 12);
    for (cpu, body) in REGISTER_DIRECT_INVALID {
        let text = source(&format!(".cpu {cpu}\n {body}\n"));
        assert!(oracle(&[("main.asm", &text)]).is_err(), "{cpu}: {body}");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; package-owned three-operand byte, word and long encoding"]
fn compact_register_direct_parity_fs_uae() {
    let text = source(&format!(".cpu m68020\n{REGISTER_DIRECT}"));
    let expected = oracle(&[("main.asm", &text)]).unwrap();
    compact_cli_cpu(
        &[("main.asm", &text)],
        &[],
        &[],
        Some(&expected),
        false,
        "m68020",
    );
}

#[test]
#[ignore = "requires configured FS-UAE; three-operand class, destination and capability barriers"]
fn compact_register_direct_rejections_fs_uae() {
    for (cpu, body) in REGISTER_DIRECT_INVALID {
        let text = source(&format!(".cpu {cpu}\n {body}\n"));
        compact_cli_cpu(&[("main.asm", &text)], &[], &[], None, false, cpu);
    }
}

#[test]
fn compact_register_immediate_rust_oracles() {
    let text = source(&format!(".cpu m68020\n{REGISTER_IMMEDIATE}"));
    assert_eq!(
        oracle(&[("main.asm", &text)]).unwrap(),
        [
            0x4e, 0x56, 0xff, 0xf8, 0x48, 0x0e, 0xff, 0xff, 0xff, 0xf8, 0x48, 0x08, 0x12, 0x34,
            0x56, 0x78
        ]
    );
    for (cpu, body) in REGISTER_IMMEDIATE_INVALID {
        let text = source(&format!(".cpu {cpu}\n {body}\n"));
        assert!(oracle(&[("main.asm", &text)]).is_err(), "{cpu}: {body}");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; package-owned register/immediate word and long encoding"]
fn compact_register_immediate_parity_fs_uae() {
    let text = source(&format!(".cpu m68020\n{REGISTER_IMMEDIATE}"));
    let expected = oracle(&[("main.asm", &text)]).unwrap();
    compact_cli_cpu(
        &[("main.asm", &text)],
        &[],
        &[],
        Some(&expected),
        false,
        "m68020",
    );
}

#[test]
#[ignore = "requires configured FS-UAE; register class, immediate form and package capability barriers"]
fn compact_register_immediate_rejections_fs_uae() {
    for (cpu, body) in REGISTER_IMMEDIATE_INVALID {
        let text = source(&format!(".cpu {cpu}\n {body}\n"));
        compact_cli_cpu(&[("main.asm", &text)], &[], &[], None, false, cpu);
    }
}

#[test]
fn compact_call_operand_rust_oracles() {
    assert!(!oracle(&[("main.asm", EXAMPLE)]).unwrap().is_empty());
    let text = source(".cpu m68030\n pack d0,d1,#1\n pack -(a0),-(a1),#-1\n unpk d2,d3,#2\n");
    assert!(!oracle(&[("main.asm", &text)]).unwrap().is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; transformed call input; execution probe, not unchanged-source parity"]
fn compact_call_operand_transport_probe_fs_uae() {
    let text = transport_example();
    let expected = oracle(&[("main.asm", EXAMPLE)]).unwrap();
    compact_cli_cpu(
        &[("main.asm", &text)],
        &[],
        &[],
        Some(&expected),
        false,
        "m68030",
    );
}

#[test]
#[ignore = "requires configured FS-UAE; transformed indirect calls and same-source three operand controls; execution probe"]
fn compact_call_operand_variations_fs_uae() {
    let text = source(&format!(".cpu m68030\n{VALID}"));
    let rust_text = source(".cpu m68030\n cas2.w d0:d1,d2:d3,(a0):(a1)\n cas2.l d7:d6,d5:d4,(a7):(a6)\n pack d0,d1,#1\n pack -(a0),-(a1),#-1\n unpk d2,d3,#2\n");
    let expected = oracle(&[("main.asm", &rust_text)]).unwrap();
    compact_cli_cpu(
        &[("main.asm", &text)],
        &[],
        &[],
        Some(&expected),
        false,
        "m68030",
    );
}

#[test]
#[ignore = "requires configured FS-UAE; package class, indirectness, arity and range controls"]
fn compact_call_operand_rejections_fs_uae() {
    for body in INVALID {
        let text = source(&format!(".cpu m68030\n {body}\n"));
        compact_cli_cpu(&[("main.asm", &text)], &[], &[], None, false, "m68030");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; same-source register and memory three-operand parity"]
fn compact_call_operand_pack_parity_fs_uae() {
    let text = source(".cpu m68030\n pack d0,d1,#1\n pack -(a0),-(a1),#-1\n unpk d2,d3,#2\n");
    let expected = oracle(&[("main.asm", &text)]).unwrap();
    compact_cli_cpu(
        &[("main.asm", &text)],
        &[],
        &[],
        Some(&expected),
        false,
        "m68030",
    );
}
