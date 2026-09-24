//! `.use with` values are module-local compile-time symbols.
use super::*;

const PARAMETERIZED_BLOCK: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (entry) with (FEATURE=1, COUNT=2)\n.word entry\n.endmodule\n.end\n",
    ),
    (
        "library/dep.asm",
        ".module dep\n.cpu m6502\n.org $1000\n.pub\n.if FEATURE\nentry .block\n.byte $11\n.bend\n.else\nentry .block\n.byte $22\n.bend\n.endif\n.for COUNT\n.byte $33\n.endfor\n.endmodule\n.end\n",
    ),
];

const PARAMETERIZED_BYTE: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (entry) with (FEATURE=7)\n.word entry\n.endmodule\n.end\n",
    ),
    (
        "library/dep.asm",
        ".module dep\n.cpu m6502\n.org $1000\n.pub\nentry .block\n.byte FEATURE\n.bend\n.endmodule\n.end\n",
    ),
];

#[test]
fn import_parameter_is_visible_to_module_data() {
    assert_eq!(
        oracle_with_roots(PARAMETERIZED_BYTE, &["library"]).unwrap(),
        [7, 0x00, 0x10]
    );
}

#[test]
fn import_parameters_drive_module_conditionals_and_loops() {
    assert_eq!(
        oracle_with_roots(PARAMETERIZED_BLOCK, &["library"]).unwrap(),
        [0x11, 0x33, 0x33, 0x00, 0x10]
    );
}

#[test]
fn import_parameter_expressions_use_earlier_caller_constants() {
    let main = PARAMETERIZED_BLOCK[0].1.replace(
        ".use dep (entry) with (FEATURE=1, COUNT=2)",
        "BASE = 2\n.use dep (entry) with (FEATURE=BASE-1, COUNT=BASE)",
    );
    assert_eq!(
        oracle_with_roots(&[("main.asm", &main), PARAMETERIZED_BLOCK[1]], &["library"]).unwrap(),
        [0x11, 0x33, 0x33, 0x00, 0x10]
    );
}

#[test]
fn import_parameter_expressions_use_const_directive() {
    let main = PARAMETERIZED_BYTE[0].1.replace(
        ".use dep (entry) with (FEATURE=7)",
        "BASE .const 7\n.use dep (entry) with (FEATURE=BASE)",
    );
    assert_eq!(
        oracle_with_roots(&[("main.asm", &main), PARAMETERIZED_BYTE[1]], &["library"]).unwrap(),
        [7, 0, 0x10]
    );
}

#[test]
fn mutable_import_parameter_source_is_rejected() {
    let main = PARAMETERIZED_BYTE[0].1.replace(
        ".use dep (entry) with (FEATURE=7)",
        "BASE := 7\n.use dep (entry) with (FEATURE=BASE)",
    );
    assert!(
        oracle_with_roots(&[("main.asm", &main), PARAMETERIZED_BYTE[1]], &["library"]).is_err()
    );
}

#[test]
fn block_local_constant_is_not_an_import_parameter_source() {
    let main = PARAMETERIZED_BYTE[0].1.replace(
        ".use dep (entry) with (FEATURE=7)",
        "local .block\nBASE = 7\n.bend\n.use dep (entry) with (FEATURE=BASE)",
    );
    assert!(
        oracle_with_roots(&[("main.asm", &main), PARAMETERIZED_BYTE[1]], &["library"]).is_err()
    );
}

#[test]
fn import_parameter_expressions_use_incoming_parameters() {
    let files = &[
        (
            "main.asm",
            ".module main\n.cpu m6502\n.use middle with (BASE=2)\n.byte $aa\n.endmodule\n.end\n",
        ),
        (
            "library/middle.asm",
            ".module middle\n.cpu m6502\n.use dep with (FEATURE=BASE+1)\n.byte $bb\n.endmodule\n.end\n",
        ),
        (
            "library/dep.asm",
            ".module dep\n.cpu m6502\n.byte FEATURE\n.endmodule\n.end\n",
        ),
    ];
    assert_eq!(
        oracle_with_roots(files, &["library"]).unwrap(),
        [3, 0xbb, 0xaa]
    );
}

#[test]
fn forward_import_parameter_values_are_rejected() {
    let main = PARAMETERIZED_BYTE[0]
        .1
        .replace("with (FEATURE=7)", "with (FEATURE=LATER)\nLATER = 7");
    assert!(
        oracle_with_roots(&[("main.asm", &main), PARAMETERIZED_BYTE[1]], &["library"]).is_err()
    );
}

#[test]
fn import_parameter_remains_private_to_its_module() {
    let main = PARAMETERIZED_BYTE[0]
        .1
        .replace(".word entry", ".word dep.FEATURE");
    assert!(
        oracle_with_roots(&[("main.asm", &main), PARAMETERIZED_BYTE[1]], &["library"]).is_err()
    );
}

#[test]
fn same_file_dependency_can_receive_parameters_from_later_module() {
    let files = &[(
        "main.asm",
        ".module dep\n.cpu m6502\n.byte FEATURE\n.endmodule\n.module main\n.cpu m6502\n.use dep with (FEATURE=7)\n.byte $aa\n.endmodule\n.end\n",
    )];
    assert_eq!(oracle(files).unwrap(), [7, 0xaa]);
}

const CONDITIONAL_DEPENDENCY: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (entry) with (FEATURE=1)\n.word entry\n.endmodule\n.end\n",
    ),
    (
        "library/dep.asm",
        ".module dep\n.cpu m6502\n.org $1000\n.pub\n.if FEATURE\n.use leaf (payload)\n.endif\nentry .block\n.word payload\n.bend\n.endmodule\n.end\n",
    ),
    (
        "library/leaf.asm",
        ".module leaf\n.cpu m6502\n.pub\npayload .block\n.byte $44\n.bend\n.endmodule\n.end\n",
    ),
];

#[test]
fn import_parameter_controls_conditional_dependency_discovery() {
    assert_eq!(
        oracle_with_roots(CONDITIONAL_DEPENDENCY, &["library"]).unwrap(),
        [0x44, 0x00, 0x00, 0x00, 0x10]
    );
}
