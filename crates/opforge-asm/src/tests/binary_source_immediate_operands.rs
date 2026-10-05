//! Package projections require immediate wrappers independently of CPU encodings.
use super::*;

const CASES: &[(&str, &str, &[u8])] = &[
    (
        "m68030",
        "mask = 3\n pflush #0,#0\n pflush #7,#7\n pflush #(2+1),#mask\n",
        &[
            0xf0, 0, 0x30, 0x10, 0xf0, 0, 0x30, 0xf7, 0xf0, 0, 0x30, 0x73,
        ],
    ),
    (
        "m68040",
        " pflush (a0)\n pflush (a7)\n",
        &[0xf5, 8, 0xf5, 15],
    ),
];

const REJECTIONS: &[(&str, &str)] = &[
    ("m68030", " pflush #0,0\n"),
    ("m68030", " pflush #0,#8\n"),
    ("m68030", " pflush #0,#-1\n"),
    ("m68030", " pflush #8,#0\n"),
    ("m68030", " pflush #0\n"),
    ("m68020", " pflush #0,#0\n"),
    ("m68000", " pflush #0,#0\n"),
    ("m68040", " pflush #0,#0\n"),
    ("m68040", " pflush (d0)\n"),
];

fn input(cpu: &str, body: &str) -> String {
    source(&format!(".cpu {cpu}\n{body}"))
}

#[test]
fn compact_immediate_operand_rust_oracles() {
    for &(cpu, body, bytes) in CASES {
        assert_eq!(oracle(&[("main.asm", &input(cpu, body))]).unwrap(), bytes);
    }
    for &(cpu, body) in REJECTIONS {
        assert!(
            oracle(&[("main.asm", &input(cpu, body))]).is_err(),
            "{cpu}: {body}"
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; package-selected immediate expressions"]
fn compact_immediate_operands_fs_uae() {
    for &(cpu, body, _) in CASES {
        let text = input(cpu, body);
        let expected = oracle(&[("main.asm", &text)]).unwrap();
        compact_cli_cpu(
            &[("main.asm", &text)],
            &[],
            &[],
            Some(&expected),
            false,
            cpu,
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; immediate shape, bounds and CPU rejection"]
fn compact_immediate_operand_rejections_fs_uae() {
    for &(cpu, body) in REJECTIONS {
        let text = input(cpu, body);
        assert!(oracle(&[("main.asm", &text)]).is_err(), "{cpu}: {body}");
        compact_cli_cpu(&[("main.asm", &text)], &[], &[], None, false, cpu);
    }
}
