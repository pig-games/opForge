//! Iterator bindings must shadow declarations in evaluation and reachability.
use super::*;

fn bytes(source: &str) -> Vec<u8> {
    let assembler = run_passes(&source.lines().collect::<Vec<_>>());
    assembler
        .image()
        .entries()
        .unwrap()
        .into_iter()
        .map(|(_, byte)| byte)
        .collect()
}

fn imported_source(body: &str) -> String {
    format!(
        ".module dep\n.pub\nhelper .block\n.byte $a5\n.bend\n.endmodule\n.module main\n.use dep (helper)\n{body}\n.endmodule"
    )
}

#[test]
fn iterable_lexical_bindings_do_not_retain_shadowed_imported_blocks() {
    assert_eq!(
        bytes(&imported_source(
            ".for helper in {2,4}\n.byte helper,helper+1\n.endfor"
        )),
        [2, 3, 4, 5]
    );
}

#[test]
fn iterable_lexical_bindings_keep_qualified_imported_references() {
    assert_eq!(
        bytes(&imported_source(
            ".for helper in {2,4}\n.byte helper\n.word dep.helper\n.endfor"
        )),
        [0xa5, 2, 0, 0, 4, 0, 0]
    );
}

#[test]
fn iterable_lexical_bindings_evaluate_rhs_before_binding_and_restore_nested_values() {
    assert_eq!(
        bytes("item = {2,4}\n.for item in item\n.byte item\n.for item in {item+1}\n.byte item\n.endfor\n.byte item\n.endfor\n.byte item[0],.len(item)"),
        [2, 3, 2, 4, 5, 4, 2, 2]
    );
}

#[test]
fn iterable_lexical_bindings_shadow_scalar_symbols_in_compound_expressions() {
    assert_eq!(
        bytes("item = 7\n.for item in {2,4}\n.byte item+1\n.endfor\n.byte item"),
        [3, 5, 7]
    );
}

#[test]
fn iterable_lexical_bindings_do_not_fall_back_to_shadowed_list_for_indexing() {
    let (_, diagnostics) = assemble_source_entries_with_runtime_mode(
        &[
            "item = {8,9}",
            ".for item in {2}",
            ".byte item[0]",
            ".endfor",
        ],
        true,
    )
    .unwrap();
    assert!(
        !diagnostics.is_empty(),
        "indexing an iterator scalar must fail"
    );
}

#[test]
fn iterable_lexical_bindings_do_not_fall_back_to_shadowed_struct_for_members() {
    assert_eq!(
        bytes("Point .struct\nx .byte ?\ny .byte ?\n.endstruct\nitem .var Point\n.byte item.y"),
        [1]
    );
    let (_, diagnostics) = assemble_source_entries_with_runtime_mode(
        &[
            "Point .struct",
            "x .byte ?",
            "y .byte ?",
            ".endstruct",
            "item .var Point",
            ".for item in {2}",
            ".byte item.y",
            ".endfor",
        ],
        true,
    )
    .unwrap();
    assert!(
        !diagnostics.is_empty(),
        "a scalar iterator has no struct field"
    );
}
