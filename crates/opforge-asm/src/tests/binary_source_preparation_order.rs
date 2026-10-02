//! Discovery/configuration must precede dependency-ordered semantic preparation.
use super::*;

#[path = "binary_source_preparation_order/character_literals.rs"]
mod character_literals;

#[path = "binary_source_preparation_order/imported_labels.rs"]
mod imported_labels;

#[path = "binary_source_preparation_order/binding_diagnostics.rs"]
mod binding_diagnostics;

const IMPORTER: &str = ".module app\n.cpu m68020\n.use owner as dep with (COUNT=5)\nFrame .struct\nBody .res dep.Span\nTail .byte ?\n.endstruct\n.byte Frame.Tail\n.endmodule\n";
const OWNER: &str = ".module owner\n.cpu m68020\n.pub\nSpan .struct\nBody .res COUNT\nTail .byte ?\n.endstruct\n.endmodule\n";

fn same_file(importer_first: bool) -> String {
    if importer_first {
        format!("{IMPORTER}{OWNER}")
    } else {
        format!("{OWNER}{IMPORTER}")
    }
}

fn configured_chain(choice: u8) -> Vec<(&'static str, String)> {
    vec![
        (
            "main.asm",
            format!(".module main\n.cpu m68020\n.use middle as dep with (SELECT={choice}, N=5)\nFrame .struct\nBody .res dep.Span\n.endstruct\n.byte Frame\n.endmodule\n"),
        ),
        (
            "library/middle.asm",
            ".module middle\n.cpu m68020\n.if SELECT\n.use chosen as child with (COUNT=N+1)\n.else\n.use alternative as child with (COUNT=N+2)\n.endif\n.pub\nSpan .struct\nBody .res child.Span\n.endstruct\n.endmodule\n".into(),
        ),
        (
            "library/chosen.asm",
            ".module chosen\n.cpu m68020\n.pub\nSpan .struct\nBody .res COUNT\n.endstruct\n.endmodule\n".into(),
        ),
        (
            "library/alternative.asm",
            ".module alternative\n.cpu m68020\n.pub\nSpan .struct\nBody .res COUNT\n.endstruct\n.endmodule\n".into(),
        ),
    ]
}

fn imported_entry_configuration(importer_first: bool) -> Vec<(&'static str, String)> {
    let owner = OWNER.replace(
        ".pub\nSpan .struct\nBody .res COUNT",
        ".if COUNT\n.use chosen as child\n.else\n.use alternative as child\n.endif\n.pub\nSpan .struct\nBody .res child.Span",
    );
    let root = if importer_first {
        format!("{IMPORTER}{owner}")
    } else {
        format!("{owner}{IMPORTER}")
    };
    vec![
        ("main.asm", root),
        (
            "library/chosen.asm",
            ".module chosen\n.cpu m68020\n.pub\nSpan = 5\n.endmodule\n".into(),
        ),
        (
            "library/alternative.asm",
            ".module alternative\n.cpu m68020\n.pub\nSpan = 9\n.endmodule\n".into(),
        ),
    ]
}

const EMPTY_DEPENDENCY: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m68020\n.use dep\n.byte 1\n.endmodule\n",
    ),
    ("library/dep.asm", ""),
];

#[test]
fn compact_preparation_order_rust_oracles() {
    for importer_first in [true, false] {
        let text = same_file(importer_first);
        assert_eq!(oracle_with_roots(&[("main.asm", &text)], &[]).unwrap(), [6]);
    }
    for (choice, size) in [(1, 6), (0, 7)] {
        let sources = configured_chain(choice);
        let files: Vec<_> = sources
            .iter()
            .map(|(name, text)| (*name, text.as_str()))
            .collect();
        assert_eq!(oracle_with_roots(&files, &["library"]).unwrap(), [size]);
    }
    for importer_first in [true, false] {
        let sources = imported_entry_configuration(importer_first);
        let files: Vec<_> = sources
            .iter()
            .map(|(name, text)| (*name, text.as_str()))
            .collect();
        assert_eq!(oracle_with_roots(&files, &["library"]).unwrap(), [6]);
    }
    assert_eq!(
        oracle_with_roots(EMPTY_DEPENDENCY, &["library"]).unwrap(),
        [1]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; same-file importer and parameterized layout owner"]
fn compact_preparation_order_same_file_fs_uae() {
    for importer_first in [true, false] {
        let text = same_file(importer_first);
        let files = [("main.asm", text.as_str())];
        let expected = oracle_with_roots(&files, &[]).unwrap();
        compact_cli_cpu(&files, &[], &[], Some(&expected), false, "m68020");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; parameter-selected dependency layout and incoming-value chain"]
fn compact_preparation_order_configured_chain_fs_uae() {
    for choice in [1, 0] {
        let sources = configured_chain(choice);
        let files: Vec<_> = sources
            .iter()
            .map(|(name, text)| (*name, text.as_str()))
            .collect();
        let expected = oracle_with_roots(&files, &["library"]).unwrap();
        compact_cli_cpu(&files, &["library"], &[], Some(&expected), false, "m68020");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; incoming parameters precede imported entry configuration"]
fn compact_preparation_order_imported_entry_fs_uae() {
    for importer_first in [true, false] {
        let sources = imported_entry_configuration(importer_first);
        let files: Vec<_> = sources
            .iter()
            .map(|(name, text)| (*name, text.as_str()))
            .collect();
        let expected = oracle_with_roots(&files, &["library"]).unwrap();
        compact_cli_cpu(&files, &["library"], &[], Some(&expected), false, "m68020");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; zero-length file-derived dependency"]
fn compact_preparation_order_empty_dependency_fs_uae() {
    let expected = oracle_with_roots(EMPTY_DEPENDENCY, &["library"]).unwrap();
    compact_cli_cpu(
        EMPTY_DEPENDENCY,
        &["library"],
        &[],
        Some(&expected),
        false,
        "m68020",
    );
}

#[test]
#[ignore = "requires configured FS-UAE; captured dependency cycles and missing targets reject"]
fn compact_preparation_order_graph_rejections_fs_uae() {
    for files in [CYCLE, SELF, MISSING] {
        assert!(oracle_with_roots(files, &[]).is_err());
        compact_cli_cpu(files, &[], &[], None, false, "m68020");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; real self-host ABI reservation declared after importer"]
fn compact_preparation_order_real_abi_fs_uae() {
    let abi = include_str!("../../../../native/motorola68000/amigaos/prvm/prvm_abi.asm");
    let text = format!(".module app\n.cpu m68020\n.use prvm.amigaos.abi as abi\nFrame .struct\nRequest .res abi.PRVM_REQUEST_FRAME_SIZE\nResult .res 32\n.endstruct\n.byte Frame.Result\n.endmodule\n{abi}");
    let files = [("main.asm", text.as_str())];
    let expected = oracle_with_roots(&files, &[]).unwrap();
    assert_eq!(expected, [112]);
    compact_cli_cpu(&files, &[], &[], Some(&expected), false, "m68020");
}
