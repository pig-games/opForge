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

const TWO_LITERAL_PARAMETERS: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (entry) with (FIRST=1, SECOND=2)\n.word entry\n.endmodule\n.end\n",
    ),
    (
        "library/dep.asm",
        ".module dep\n.cpu m6502\n.org $1000\n.pub\nentry .block\n.byte FIRST, SECOND\n.bend\n.endmodule\n.end\n",
    ),
];

const INCOMING_PARAMETER_CHAIN: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\nBASE = 9\n.use middle with (BASE=2)\n.byte $aa\n.endmodule\n.end\n",
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

#[test]
fn two_import_parameters_are_visible_in_module_data() {
    assert_eq!(
        oracle_with_roots(TWO_LITERAL_PARAMETERS, &["library"]).unwrap(),
        [1, 2, 0, 0x10]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; two native scalar literal parameters"]
fn compact_cli_two_literal_parameters_fs_uae() {
    let expected = oracle_with_roots(TWO_LITERAL_PARAMETERS, &["library"]).unwrap();
    compact_cli(
        TWO_LITERAL_PARAMETERS,
        &["library"],
        &[],
        Some(&expected),
        false,
    );
}

#[test]
fn import_parameter_is_visible_to_module_data() {
    assert_eq!(
        oracle_with_roots(PARAMETERIZED_BYTE, &["library"]).unwrap(),
        [7, 0x00, 0x10]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; native scalar parameter binding"]
fn compact_cli_scalar_parameter_literal_fs_uae() {
    let expected = oracle_with_roots(PARAMETERIZED_BYTE, &["library"]).unwrap();
    compact_cli(
        PARAMETERIZED_BYTE,
        &["library"],
        &[],
        Some(&expected),
        false,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; importer-site constant parameter evaluation"]
fn compact_cli_caller_constant_parameter_fs_uae() {
    let main = PARAMETERIZED_BYTE[0].1.replace(
        ".use dep (entry) with (FEATURE=7)",
        "BASE = 7\n.use dep (entry) with (FEATURE=BASE)",
    );
    let files = &[("main.asm", main.as_str()), PARAMETERIZED_BYTE[1]];
    let expected = oracle_with_roots(files, &["library"]).unwrap();
    compact_cli(files, &["library"], &[], Some(&expected), false);
}

#[test]
#[ignore = "requires configured FS-UAE; importer-site scalar expression evaluation"]
fn compact_cli_caller_expression_parameter_fs_uae() {
    let main = PARAMETERIZED_BYTE[0].1.replace(
        ".use dep (entry) with (FEATURE=7)",
        "BASE = 3\n.use dep (entry) with (FEATURE=(BASE+2)*2-1)",
    );
    let files = &[("main.asm", main.as_str()), PARAMETERIZED_BYTE[1]];
    let expected = oracle_with_roots(files, &["library"]).unwrap();
    assert_eq!(expected, [9, 0, 0x10]);
    compact_cli(files, &["library"], &[], Some(&expected), false);
}

#[test]
#[ignore = "requires configured FS-UAE; unrelated forward constants remain valid"]
fn compact_cli_unrelated_forward_constant_with_import_fs_uae() {
    let main = PARAMETERIZED_BYTE[0].1.replace(
        ".use dep (entry) with (FEATURE=7)",
        "EARLY = LATER+1\nLATER = 6\n.use dep (entry) with (FEATURE=7)",
    );
    let files = &[("main.asm", main.as_str()), PARAMETERIZED_BYTE[1]];
    let expected = oracle_with_roots(files, &["library"]).unwrap();
    compact_cli(files, &["library"], &[], Some(&expected), false);
}

#[test]
#[ignore = "requires configured FS-UAE; isolate forward constant resolution"]
fn compact_cli_forward_constant_without_import_fs_uae() {
    let files = &[(
        "main.asm",
        ".module main\n.cpu m6502\nEARLY = LATER+1\nLATER = 6\n.byte 7\n.endmodule\n.end\n",
    )];
    let expected = oracle_with_roots(files, &[]).unwrap();
    assert_eq!(expected, [7]);
    compact_cli(files, &[], &[], Some(&expected), false);
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
    assert_eq!(
        oracle_with_roots(INCOMING_PARAMETER_CHAIN, &["library"]).unwrap(),
        [3, 0xbb, 0xaa]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; incoming parameter feeds nested import"]
fn compact_cli_incoming_parameter_expression_fs_uae() {
    let expected = oracle_with_roots(INCOMING_PARAMETER_CHAIN, &["library"]).unwrap();
    assert_eq!(expected, [3, 0xbb, 0xaa]);
    compact_cli(
        INCOMING_PARAMETER_CHAIN,
        &["library"],
        &[],
        Some(&expected),
        false,
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
#[ignore = "requires configured FS-UAE; no forward value at import site"]
fn compact_cli_rejects_forward_parameter_value_fs_uae() {
    let main = PARAMETERIZED_BYTE[0]
        .1
        .replace("with (FEATURE=7)", "with (FEATURE=LATER)\nLATER = 7");
    let files = &[("main.asm", main.as_str()), PARAMETERIZED_BYTE[1]];
    assert!(oracle_with_roots(files, &["library"]).is_err());
    compact_cli(files, &["library"], &[], None, false);
}

#[test]
#[ignore = "requires configured FS-UAE; label addresses are assembly-time values"]
fn compact_cli_rejects_label_parameter_value_fs_uae() {
    let main = PARAMETERIZED_BYTE[0].1.replace(
        ".use dep (entry) with (FEATURE=7)",
        "here:\n.use dep (entry) with (FEATURE=here)",
    );
    let files = &[("main.asm", main.as_str()), PARAMETERIZED_BYTE[1]];
    assert!(oracle_with_roots(files, &["library"]).is_err());
    compact_cli(files, &["library"], &[], None, false);
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
#[ignore = "requires configured FS-UAE; native parameter privacy"]
fn compact_cli_rejects_private_parameter_reference_fs_uae() {
    let main = PARAMETERIZED_BYTE[0]
        .1
        .replace(".word entry", ".word dep.FEATURE");
    let files = &[("main.asm", main.as_str()), PARAMETERIZED_BYTE[1]];
    compact_cli(files, &["library"], &[], None, false);
}

#[test]
fn conflicting_values_for_one_module_are_rejected() {
    let main = PARAMETERIZED_BYTE[0].1.replace(
        ".use dep (entry) with (FEATURE=7)\n.word entry",
        ".use dep as first with (FEATURE=7)\n.use dep as second with (FEATURE=8)\n.word first.entry",
    );
    assert!(
        oracle_with_roots(&[("main.asm", &main), PARAMETERIZED_BYTE[1]], &["library"]).is_err()
    );
}

#[test]
#[ignore = "requires configured FS-UAE; conflicting native module parameters"]
fn compact_cli_rejects_conflicting_parameter_values_fs_uae() {
    let main = PARAMETERIZED_BYTE[0].1.replace(
        ".use dep (entry) with (FEATURE=7)\n.word entry",
        ".use dep as first with (FEATURE=7)\n.use dep as second with (FEATURE=8)\n.word first.entry",
    );
    let files = &[("main.asm", main.as_str()), PARAMETERIZED_BYTE[1]];
    compact_cli(files, &["library"], &[], None, false);
}

#[test]
fn repeated_import_alias_is_rejected_even_with_identical_parameters() {
    let main = PARAMETERIZED_BYTE[0].1.replace(
        ".use dep (entry) with (FEATURE=7)\n.word entry",
        ".use dep with (FEATURE=7)\n.use dep with (FEATURE=7)\n.word dep.entry",
    );
    assert!(
        oracle_with_roots(&[("main.asm", &main), PARAMETERIZED_BYTE[1]], &["library"]).is_err()
    );
}

#[test]
#[ignore = "requires configured FS-UAE; repeated native import alias rejection"]
fn compact_cli_rejects_repeated_import_alias_fs_uae() {
    let main = PARAMETERIZED_BYTE[0].1.replace(
        ".use dep (entry) with (FEATURE=7)\n.word entry",
        ".use dep with (FEATURE=7)\n.use dep with (FEATURE=7)\n.word dep.entry",
    );
    let files = &[("main.asm", main.as_str()), PARAMETERIZED_BYTE[1]];
    compact_cli(files, &["library"], &[], None, false);
}

#[test]
fn identical_parameters_allow_distinct_aliases_for_one_module() {
    let main = PARAMETERIZED_BYTE[0].1.replace(
        ".use dep (entry) with (FEATURE=7)\n.word entry",
        ".use dep as first with (FEATURE=7)\n.use dep as second with (FEATURE=7)\n.word first.entry",
    );
    assert_eq!(
        oracle_with_roots(&[("main.asm", &main), PARAMETERIZED_BYTE[1]], &["library"]).unwrap(),
        [7, 0, 0x10]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; two aliases share one configured module"]
fn compact_cli_scalar_parameter_two_aliases_fs_uae() {
    let main = PARAMETERIZED_BYTE[0].1.replace(
        ".use dep (entry) with (FEATURE=7)\n.word entry",
        ".use dep as first with (FEATURE=7)\n.use dep as second with (FEATURE=7)\n.word first.entry",
    );
    let files = &[("main.asm", main.as_str()), PARAMETERIZED_BYTE[1]];
    let expected = oracle_with_roots(files, &["library"]).unwrap();
    compact_cli(files, &["library"], &[], Some(&expected), false);
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
