//! F4: scoped source bindings disappear before binary assembly starts.
use super::*;

const COPY: &str = include_str!("../../fixtures/binary-source/scoped-copy.asm");
const COPY_FLAT: &str = include_str!("../../fixtures/binary-source/flat-copy.asm");
const CONTROL: &str = include_str!("../../fixtures/binary-source/scoped-control.asm");
const CONTROL_FLAT: &str = include_str!("../../fixtures/binary-source/flat-control.asm");
const NESTED: &str = r#".cpu m6502
.org $1000
reference = outer.inner.value+1
 .byte reference
value = 9
outer .block
 .byte value
value = 3
parentValue = limit+1
limit = 4
inner .block
 .byte value,parentValue
value = 7
result = parentValue+1
 .byte value,outer.value,result
.endblock
 .byte value
.endblock
.byte value,OuTeR.InNeR.VaLuE
.long $01020304
.word $2034
.byte ((~value)&15)^1|2,(value<<2)>>1
.byte -value+10,value*2-value
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
fn binary_scopes_copy_oracle() {
    let expected = [
        0x20, 7, 0x10, 0x20, 0x13, 0x10, 0x60, 0xa2, 0, 0xb5, 0x20, 0x95, 0x30, 0xe8, 0xe0, 4,
        0xd0, 0xf7, 0x60, 0xa2, 0, 0xa9, 0, 0x55, 0x30, 0xe8, 0xe0, 4, 0xd0, 0xf9, 0x29, 15, 0x60,
        9, 0x10, 0x17, 0x10, 15, 0,
    ];
    assert_eq!(bytes(COPY), expected);
    assert_eq!(bytes(COPY_FLAT), expected);
}

#[test]
fn binary_scopes_control_oracle() {
    let expected = [
        2, 0x80, 0, 0, 0, 0xff, 0, 0x80, 0, 0, 0x3a, 0, 0x4e, 0x75, 2, 0x80, 0, 0, 0xff, 0, 0x0a,
        0x80, 0, 0, 0x3a, 0, 0x4e, 0x75, 0x10, 0, 0x10, 14, 0, 14, 0, 14, 0, 0xff, 0xff, 0,
    ];
    assert_eq!(bytes(CONTROL), expected);
    assert_eq!(bytes(CONTROL_FLAT), expected);
}

#[test]
fn binary_scopes_nested_oracle() {
    assert_eq!(
        bytes(NESTED),
        [8, 3, 7, 5, 7, 3, 6, 3, 9, 7, 4, 3, 2, 1, 0x34, 0x20, 7, 18, 1, 9]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; scoped copy/fold routines"]
fn binary_scopes_copy_fs_uae() {
    assert_binary_source(COPY.into(), "m6502".into());
}

#[test]
#[ignore = "requires configured FS-UAE; scoped control-word routines"]
fn binary_scopes_control_fs_uae() {
    assert_binary_source(CONTROL.into(), "m68000".into());
}

#[test]
#[ignore = "requires configured FS-UAE; nested lookup and forward local shadowing"]
fn binary_scopes_nested_fs_uae() {
    assert_binary_source(NESTED.into(), "m6502".into());
}

// True marks valid Rust constructs deliberately outside this native increment.
const REJECTIONS: &[(&str, &str, bool)] = &[
    ("cross_cycle", "left .block\nvalue = right.value+1\n.endblock\nright .block\nvalue = left.value+1\n.endblock", false),
    ("unclosed", "open .block\n.byte 1", false),
    ("underflow", ".endblock", false),
    ("duplicate", "scope .block\nvalue = 1\nvalue = 2\n.endblock", false),
    ("sibling_leak", "left .block\nvalue = 1\n.endblock\nright .block\n.byte value\n.endblock", false),
    ("qualified_missing", "scope .block\nvalue = 1\n.endblock\n.byte absent.value", false),
    ("close_operands", "scope .block\n.endblock 1", false),
    ("anonymous", ".block\n.byte 1\n.endblock", true),
    ("dotted_block", "outer.inner .block\n.byte 1\n.endblock", true),
];

fn rejection_source(body: &str) -> String {
    format!(".cpu m6502\n.org $1000\n{body}\n.end\n")
}

#[test]
fn binary_scopes_rejection_oracles() {
    let actual: Vec<_> = REJECTIONS
        .iter()
        .map(|(name, body, _)| {
            let source = rejection_source(body);
            let result = assemble_source_entries_with_runtime_mode(
                &source.lines().collect::<Vec<_>>(),
                true,
            );
            (
                *name,
                matches!(result,Ok((_,ref diagnostics)) if diagnostics.is_empty()),
            )
        })
        .collect();
    let expected: Vec<_> = REJECTIONS
        .iter()
        .map(|(name, _, accepted)| (*name, *accepted))
        .collect();
    assert_eq!(actual, expected);
}

#[test]
#[ignore = "requires OPFORGE_SCOPE_REJECTION and configured FS-UAE"]
fn binary_scopes_rejection_fs_uae() {
    let key = std::env::var("OPFORGE_SCOPE_REJECTION").unwrap();
    let (_, body, _) = REJECTIONS.iter().find(|(name, _, _)| *name == key).unwrap();
    assert_native_rejection(&rejection_source(body), "m6502");
}

#[test]
#[ignore = "requires configured FS-UAE; equivalent flat copy/fold comparison"]
fn binary_scopes_copy_flat_fs_uae() {
    assert_binary_source(COPY_FLAT.into(), "m6502".into());
}

#[test]
#[ignore = "requires configured FS-UAE; equivalent flat control-word comparison"]
fn binary_scopes_control_flat_fs_uae() {
    assert_binary_source(CONTROL_FLAT.into(), "m68000".into());
}
