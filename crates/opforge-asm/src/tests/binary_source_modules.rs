//! F6: module ownership and visibility finish during preparation.
use super::*;

const COPY: &str = include_str!("../../fixtures/binary-source/modules-copy.asm");
const CONTROL: &str = include_str!("../../fixtures/binary-source/modules-control.asm");
const MIXED: &str = r#"header .module demo
.cpu m6502
.org $1000
.pub
value = 3
.namespace local
.priv
hidden = 7
.byte hidden
.endn
after = 5
visible .priv
.endmodule
.module demo.child
.byte value,demo.after
.endmodule
.module client
.pub
.byte demo.value,demo.after
finish .endmodule
.module trailer
.word header,demo.visible,client.finish
.endmodule
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
fn binary_modules_routine_oracles() {
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
fn binary_modules_mixed_oracle() {
    assert_eq!(bytes(MIXED), [7, 3, 5, 3, 5, 0, 0, 1, 0x10, 5, 0x10]);
}

const REJECTIONS: &[(&str, &str, bool)] = &[
    ("label_visibility", ".module owner\n.cpu m6502\nmark .pub\n.endmodule\n.module other\n.word owner.mark\n.endmodule", false),
    ("private", ".module owner\n.cpu m6502\n.org $1000\nsecret = 7\n.byte secret\n.endmodule\n.module other\n.byte owner.secret\n.endmodule", false),
    ("private_forward", ".module other\n.cpu m6502\n.org $1000\n.byte owner.secret\n.endmodule\n.module owner\nsecret = 7\n.byte secret\n.endmodule", false),
    ("prefix_private", ".module demo\n.cpu m6502\nvalue = 7\n.endmodule\n.module demo.child\n.byte value\n.endmodule", false),
    ("restore_private", ".module owner\n.cpu m6502\nroutine .block\n.pub\ninside = 1\n.bend\nhidden = 7\n.endmodule\n.module other\n.byte owner.hidden\n.endmodule", false),
    ("duplicate", ".module demo\n.endmodule\n.module DEMO\n.endmodule", false),
    ("nested", ".module outer\n.module inner\n.endmodule\n.endmodule", false),
    ("module_in_scope", ".namespace outer\n.module inner\n.endmodule\n.endn", false),
    ("unclosed", ".module demo\n.cpu m6502\n.byte 1", false),
    ("open_child", ".module demo\n.namespace child\n.endmodule", false),
    ("underflow", ".endmodule", false),
    ("before", ".cpu m6502\n.module demo\n.endmodule", false),
    ("between", ".module demo\n.endmodule\n.byte 1\n.module other\n.endmodule", false),
    ("bad_id", ".module demo..child\n.endmodule", false),
    ("pub_operands", ".module demo\n.pub 1\n.endmodule", false),
    ("close_operands", ".module demo\n.endmodule 1", false),
];

fn rejection_source(body: &str) -> String {
    format!("{body}\n.end\n")
}
#[test]
fn binary_modules_rejection_oracles() {
    for (name, body, expected) in REJECTIONS {
        let source = rejection_source(body);
        let result =
            assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true);
        let accepted = matches!(result, Ok((_, ref diagnostics)) if diagnostics.is_empty());
        assert_eq!(accepted, *expected, "{name}");
    }
}
#[test]
#[ignore = "requires configured FS-UAE; module copy/fold"]
fn binary_modules_copy_fs_uae() {
    assert_binary_source(COPY.into(), "m6502".into());
}
#[test]
#[ignore = "requires configured FS-UAE; module control word"]
fn binary_modules_control_fs_uae() {
    assert_binary_source(CONTROL.into(), "m68000".into());
}
#[test]
#[ignore = "requires configured FS-UAE; module ownership and visibility"]
fn binary_modules_mixed_fs_uae() {
    assert_binary_source(MIXED.into(), "m6502".into());
}
#[test]
#[ignore = "requires OPFORGE_MODULE_REJECTION and configured FS-UAE"]
fn binary_modules_rejection_fs_uae() {
    let key = std::env::var("OPFORGE_MODULE_REJECTION").unwrap();
    let (_, body, _) = REJECTIONS.iter().find(|(name, _, _)| *name == key).unwrap();
    assert_native_rejection(&rejection_source(body), "m6502");
}
