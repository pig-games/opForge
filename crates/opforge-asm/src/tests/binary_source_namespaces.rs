//! F5: namespaces and canonical bare labels bind during preparation.
use super::*;

const COPY: &str = include_str!("../../fixtures/binary-source/namespaced-copy.asm");
const CONTROL: &str = include_str!("../../fixtures/binary-source/namespaced-control.asm");
const MIXED: &str = r#".cpu m6502
.org $1000
root = 9
start
.namespace pool
.byte value
value = 3
work .block
.namespace local
value = 7
data .byte value,pool.value,root
.endn
.byte value
.bend
.endnamespace
pool = 12
.namespace pool
other = value+1
.byte other
.endn
anchor .namespace otherSpace
.byte pool,pool.other
.endn
labelSpace .namespace
.byte 1
.endnamespace
.word anchor,labelSpace,start,pool.work.local.data
.end
"#;

fn bytes(source: &str) -> Vec<u8> {
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live Rust oracle");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn binary_namespaces_routine_oracles() {
    assert_eq!(
        bytes(COPY),
        [
            0x20, 7, 0x10, 0x20, 0x13, 0x10, 0x60, 0xa2, 0, 0xb5, 0x20, 0x95, 0x30, 0xe8, 0xe0, 4,
            0xd0, 0xf7, 0x60, 0xa2, 0, 0xa9, 0, 0x55, 0x30, 0xe8, 0xe0, 4, 0xd0, 0xf9, 0x29, 15,
            0x60, 9, 0x10, 0x17, 0x10, 15, 0,
        ]
    );
    assert_eq!(
        bytes(CONTROL),
        [
            2, 0x80, 0, 0, 0, 0xff, 0, 0x80, 0, 0, 0x3a, 0, 0x4e, 0x75, 2, 0x80, 0, 0, 0xff, 0,
            0x0a, 0x80, 0, 0, 0x3a, 0, 0x4e, 0x75, 0x10, 0, 0x10, 14, 0, 14, 0, 14, 0, 0xff, 0xff,
            0,
        ]
    );
}

#[test]
fn binary_namespaces_mixed_oracle() {
    assert_eq!(
        bytes(MIXED),
        [3, 7, 3, 9, 3, 4, 12, 4, 1, 6, 0x10, 8, 0x10, 0, 0x10, 1, 0x10]
    );
}

// True marks valid Rust forms deliberately outside the native subset.
const REJECTIONS: &[(&str, &str, bool)] = &[
    ("indented_label", " label: .byte 1", false),
    ("indented_unknown", " label", false),
    ("reserved_label", "nop\n.byte 1", true),
    ("missing", ".namespace", false),
    ("operand", ".namespace 4", false),
    ("extra", ".namespace one,two", false),
    ("block_close", ".namespace space\n.endblock", false),
    ("namespace_close", "space .block\n.endn", false),
    ("underflow", ".endnamespace", false),
    ("unclosed", ".namespace space\n.byte 1", false),
    (
        "duplicate",
        ".namespace space\nvalue = 1\n.endn\n.namespace space\nvalue = 2\n.endn",
        false,
    ),
    (
        "no_label",
        ".namespace space\n.byte 1\n.endn\n.word space",
        false,
    ),
    ("close_operands", ".namespace space\n.endn 1", false),
    ("dotted", ".namespace outer.inner\n.byte 1\n.endn", true),
];
fn rejection_source(body: &str) -> String {
    format!(".cpu m6502\n.org $1000\n{body}\n.end\n")
}
#[test]
fn binary_namespaces_rejection_oracles() {
    for (name, body, expected) in REJECTIONS {
        let source = rejection_source(body);
        let result =
            assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true);
        let accepted = matches!(result, Ok((_, ref diagnostics)) if diagnostics.is_empty());
        assert_eq!(accepted, *expected, "{name}");
    }
}
#[test]
#[ignore = "requires configured FS-UAE; namespaced copy/fold"]
fn binary_namespaces_copy_fs_uae() {
    assert_binary_source(COPY.into(), "m6502".into());
}
#[test]
#[ignore = "requires configured FS-UAE; namespaced control word"]
fn binary_namespaces_control_fs_uae() {
    assert_binary_source(CONTROL.into(), "m68000".into());
}
#[test]
#[ignore = "requires configured FS-UAE; reopening and mixed naming"]
fn binary_namespaces_mixed_fs_uae() {
    assert_binary_source(MIXED.into(), "m6502".into());
}
#[test]
#[ignore = "requires OPFORGE_NAMESPACE_REJECTION and configured FS-UAE"]
fn binary_namespaces_rejection_fs_uae() {
    let key = std::env::var("OPFORGE_NAMESPACE_REJECTION").unwrap();
    let (_, body, _) = REJECTIONS.iter().find(|(name, _, _)| *name == key).unwrap();
    assert_native_rejection(&rejection_source(body), "m6502");
}
