//! Qualified imports and compound members retain their precedence in struct scopes.
use super::native_active_includes::live_oracle;
use super::native_iterable_loops::{build_image, oracle, scratch};
use super::native_source_cases::{self, Case};

fn cases() -> Vec<Case> {
    let mut cases = Vec::new();
    for field in ["Pack", "pAcK", "Storage"] {
        for imported in [false, true] {
            let (uses, extent) = if imported {
                (".use layout_defs as arguments\n", "arguments.FRAME_BYTES")
            } else {
                ("", "84")
            };
            let mut files = vec![(
                "entry.asm",
                format!(".module app\n.cpu 68020\n{uses}Local .struct\n{field} .res {extent}\nTail .byte ?\n.endstruct\n.org $1000\n.word Local.{field}, Local.Tail\n.endmodule\n"),
            )];
            if imported {
                files.push((
                    "layout_defs.asm",
                    ".module layout_defs\n.cpu 68020\n.pub\nFRAME_BYTES = 84\n.endmodule\n".into(),
                ));
            }
            cases.push(Case {
                name: format!("struct-field-{field}-imported-{imported}"),
                cpu: "68020".into(),
                files,
                expected: Some(vec![0, 0, 0, 84]),
            });
        }
    }
    for field in ["Pack", "pAcK", "Storage"] {
        cases.push(Case {
            name: format!("struct-field-{field}-import-alias-shadowed"),
            cpu: "68020".into(),
            files: vec![
                (
                    "entry.asm",
                    format!(".module app\n.cpu 68020\n.use layout_defs as arguments\nLocal .struct\nArguments .res 8\n{field} .res arguments.FRAME_BYTES\nTail .byte ?\n.endstruct\n.org $1000\n.word Local.{field}, Local.Tail\n.endmodule\n"),
                ),
                (
                    "layout_defs.asm",
                    ".module layout_defs\n.cpu 68020\n.pub\nFRAME_BYTES = 84\n.endmodule\n".into(),
                ),
            ],
            expected: Some(vec![0, 8, 0, 92]),
        });
    }
    cases.push(Case {
        name: "struct-extent-compound-member-control".into(),
        cpu: "68020".into(),
        files: vec![("entry.asm", ".module app\n.cpu 68020\nSizes .struct\nFRAME_BYTES .byte ?\n.endstruct\narguments=app.Sizes{FRAME_BYTES:84}\nLocal .struct\nPack .res arguments.FRAME_BYTES\nTail .byte ?\n.endstruct\n.org $1000\n.word Local.Pack,Local.Tail\n.endmodule\n".into())],
        expected: Some(vec![0,0,0,84]),
    });
    for (name, declarations, extent) in [
        (
            "alias-missing-member",
            ".use layout_defs as arguments\n",
            "arguments.MISSING",
        ),
        ("scalar-missing-member", "", "Arguments.MISSING"),
    ] {
        cases.push(Case {
            name: format!("struct-extent-{name}-rejection"),
            cpu: "68020".into(),
            files: vec![("entry.asm", format!(".module layout_defs\n.cpu 68020\n.pub\nFRAME_BYTES=84\n.endmodule\n.module app\n.cpu 68020\n{declarations}Local .struct\nArguments .res 8\nPack .res {extent}\n.endstruct\n.org $1000\n.byte 1\n.endmodule\n"))],
            expected: None,
        });
    }
    for (name, exported, expected) in [
        ("alias-and-compound", "FRAME_BYTES=84", Some(vec![84])),
        ("alias-missing-compound-present", "OTHER=84", Some(vec![3])),
        (
            "alias-private-compound-present",
            ".priv\nFRAME_BYTES=84",
            None,
        ),
    ] {
        cases.push(Case {
            name: format!("struct-extent-{name}"),
            cpu: "68020".into(),
            files: vec![("entry.asm", format!(".module layout_defs\n.cpu 68020\n.pub\n{exported}\n.endmodule\n.module app\n.cpu 68020\n.use layout_defs as arguments\nPoint .struct\nFRAME_BYTES .byte ?\n.endstruct\narguments=app.Point{{FRAME_BYTES:3}}\n.org $1000\n.byte arguments.FRAME_BYTES\n.endmodule\n"))],
            expected,
        });
    }
    cases.push(Case {
        name: "qualified-module-compound-control".into(),
        cpu: "68020".into(),
        files: vec![("entry.asm", ".module layout_defs\n.cpu 68020\n.pub\nPoint .struct\nFRAME_BYTES .byte ?\n.endstruct\ndata=layout_defs.Point{FRAME_BYTES:3}\n.endmodule\n.module app\n.cpu 68020\n.use layout_defs\n.org $1000\n.byte layout_defs.data.FRAME_BYTES\n.endmodule\n".into())],
        expected: Some(vec![3]),
    });
    let alternate = cases
        .iter()
        .find(|case| case.name == "struct-field-Pack-import-alias-shadowed")
        .unwrap();
    let files = alternate
        .files
        .iter()
        .map(|(path, source)| (*path, source.replace(".cpu 68020", ".cpu 6502")))
        .collect();
    cases.push(Case {
        name: "struct-import-alias-shadowed-6502".into(),
        cpu: "6502".into(),
        files,
        expected: Some(vec![8, 0, 92, 0]),
    });
    cases
}

#[test]
fn struct_field_name_extent_rust_oracles() {
    for case in cases() {
        let dir = scratch();
        if let Some(expected) = case.expected {
            assert_eq!(live_oracle(&dir.0, &case.files), expected, "{}", case.name);
        } else {
            assert!(oracle(&dir.0, &case.files[0].1).is_err(), "{}", case.name);
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; labeled struct fields and imported extents"]
fn struct_field_name_extent_fs_uae() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = build_image(&root, &dir.0);
    native_source_cases::run_source_language_batch(&root, &image, &cases(), 17);
}
