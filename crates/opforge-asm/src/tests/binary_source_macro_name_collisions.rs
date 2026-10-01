//! A macro name may also name a structural declaration or ordinary label.
use super::*;

fn hunk_source(code: &str) -> String {
    format!(
        ".module macro_name_probe\n.cpu m68020\n.section code,kind=code\n{code}.endsection\n.section bss,kind=bss\n.res byte,12\n.endsection\n.section data,kind=data\n.byte $aa\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule\n"
    )
}

fn evaluator_source() -> String {
    hunk_source(
        ".priv\nEVALUATE .macro saved\n movem.l .saved,-(sp)\n moveq #7,d0\n movem.l (sp)+,.saved\n tst.l d0\n rts\n.endmacro\n.pub\nevaluate .block\n .EVALUATE d3-d7/a1-a6\n.bend\nevaluateWithSymbols .block\n .eVaLuAtE d3/d5-d7/a1-a6\n.bend\n",
    )
}

fn ordinary_label_source() -> String {
    hunk_source(
        ".priv\nENTRY .macro value\n.byte .value,$5a\n.endmacro\n.pub\nentry nop\n .ENTRY $a5\n rts\n",
    )
}

fn renamed_ordinary_label_source() -> String {
    ordinary_label_source()
        .replace("ENTRY .macro", "ENTRY_BODY .macro")
        .replace(" .ENTRY ", " .ENTRY_BODY ")
}

fn colon_ordinary_label_source() -> String {
    ordinary_label_source().replace("entry nop", "entry: nop")
}

fn renamed_evaluator_source() -> String {
    evaluator_source()
        .replace("EVALUATE .macro", "EVALUATE_BODY .macro")
        .replace(" .EVALUATE ", " .EVALUATE_BODY ")
        .replace(" .eVaLuAtE ", " .EVALUATE_BODY ")
}

fn local_facets_source(template_first: bool) -> String {
    let value = "SHARED = 3\n";
    let template = "SHARED .macro arg\n.byte .arg\n.endmacro\n";
    let declarations = if template_first {
        format!("{template}{value}")
    } else {
        format!("{value}{template}")
    };
    hunk_source(&format!("{declarations}.byte SHARED\n .sHaReD 7\n"))
}

fn local_facet_cases() -> [(&'static str, String); 3] {
    [
        ("value_first", local_facets_source(false)),
        ("template_first", local_facets_source(true)),
        (
            "forward_numeric",
            hunk_source(
                ".byte SHARED\nSHARED .macro arg\n.byte .arg\n.endmacro\nSHARED = 3\n .SHARED 7\n",
            ),
        ),
    ]
}

fn imported_facets_source(
    import: &str,
    value_public: bool,
    template_public: bool,
    body: &str,
) -> String {
    // Both shared runners stage input.asm; preserve the caller as the root module
    // while defining imported templates before their calls.
    let caller = hunk_source(body)
        .replacen(".module macro_name_probe\n", ".module input\n", 1)
        .replacen(".cpu m68020\n", &format!(".cpu m68020\n{import}\n"), 1);
    let value_visibility = if value_public { ".pub" } else { ".priv" };
    let template_visibility = if template_public { ".pub" } else { ".priv" };
    format!(
        ".module dual\n.cpu m68020\n{value_visibility}\nSHARED = 3\n{template_visibility}\nSHARED .macro arg\n.byte .arg\n.endmacro\n.endmodule\n{caller}"
    )
}

fn imported_template_only_source(body: &str) -> String {
    imported_facets_source(".use dual (SHARED as picked)", true, true, body)
        .replace("SHARED = 3\n", "")
}

fn imported_positive_cases() -> [(&'static str, String, [u8; 4]); 7] {
    [
        (
            "qualified",
            imported_facets_source(
                ".use dual",
                true,
                true,
                ".byte dual.SHARED\n .dual.SHARED 7\n",
            ),
            [3, 7, 0, 0],
        ),
        (
            "alias",
            imported_facets_source(
                ".use dual as d",
                true,
                true,
                ".byte d.SHARED\n .d.SHARED 7\n",
            ),
            [3, 7, 0, 0],
        ),
        (
            "public_value",
            imported_facets_source(".use dual as d", true, false, ".byte d.SHARED,$a5\n"),
            [3, 0xa5, 0, 0],
        ),
        (
            "public_template",
            imported_facets_source(".use dual as d", false, true, " .d.SHARED 7\n.byte $a5\n"),
            [7, 0xa5, 0, 0],
        ),
        (
            "selected_public_value",
            imported_facets_source(
                ".use dual (SHARED) as d",
                true,
                false,
                ".byte d.SHARED,$a5\n",
            ),
            [3, 0xa5, 0, 0],
        ),
        (
            "selected_alias",
            imported_facets_source(
                ".use dual (SHARED as picked)",
                true,
                true,
                ".byte picked,$a5\n",
            ),
            [3, 0xa5, 0, 0],
        ),
        (
            "selected_template_original",
            imported_template_only_source(" .SHARED 7\n.byte $a5\n"),
            [7, 0xa5, 0, 0],
        ),
    ]
}

fn negative_facet_cases() -> [(&'static str, String); 10] {
    [
        ("duplicate_value", hunk_source("SHARED = 3\nSHARED .macro\n.byte 7\n.endmacro\nSHARED = 4\n")),
        ("duplicate_template", hunk_source("SHARED = 3\nSHARED .macro\n.byte 7\n.endmacro\nSHARED .macro\n.byte 8\n.endmacro\n")),
        ("private_value", imported_facets_source(".use dual as d", false, true, ".byte d.SHARED\n")),
        ("private_template", imported_facets_source(".use dual as d", true, false, " .d.SHARED 7\n")),
        ("selected_private_value", imported_facets_source(".use dual (SHARED) as d", false, true, ".byte $a5\n")),
        ("selected_private_template", imported_facets_source(".use dual (SHARED) as d", true, false, " .d.SHARED 7\n")),
        ("template_only_numeric", hunk_source("SHARED .macro arg\n.byte .arg\n.endmacro\n.byte SHARED\n")),
        ("selected_alias_private_value", imported_facets_source(".use dual (SHARED as picked)", false, true, ".byte $a5\n")),
        ("selected_alias_template", imported_facets_source(".use dual (SHARED as picked)", true, true, " .picked 7\n")),
        ("selected_alias_template_only", imported_template_only_source(" .picked 7\n")),
    ]
}

fn assert_facet_code(source: &str, expected: &[u8]) {
    let oracle = hunk_sections::rust_hunk_source(source);
    let sections = hunk::segments(&oracle).expect("valid value/template namespace Hunk");
    assert_eq!(sections[0].payload, expected);
    assert!(sections[0].relocations.is_empty());
}

fn rust_facet_rejection(source: &str) -> Option<String> {
    let dir = create_temp_dir("macro-name-facet-rejection");
    let input = dir.join("input.asm");
    fs::write(&input, source).expect("write namespace rejection source");
    let error = assemble_example_error(&input);
    fs::remove_dir_all(dir).expect("remove namespace rejection input");
    error
}

#[test]
fn compact_macro_name_block_collision_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&evaluator_source());
    let sections = hunk::segments(&oracle).expect("valid evaluator macro Hunk");
    assert_eq!(
        sections[0].payload,
        [
            0x48, 0xe7, 0x1f, 0x7e, 0x70, 7, 0x4c, 0xdf, 0x7e, 0xf8, 0x4a, 0x80, 0x4e, 0x75, 0x48,
            0xe7, 0x17, 0x7e, 0x70, 7, 0x4c, 0xdf, 0x7e, 0xe8, 0x4a, 0x80, 0x4e, 0x75,
        ]
    );
    assert!(sections[0].relocations.is_empty());
}

#[test]
fn compact_macro_name_ordinary_label_collision_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&ordinary_label_source());
    let sections = hunk::segments(&oracle).expect("valid ordinary label macro Hunk");
    assert_eq!(
        sections[0].payload,
        [0x4e, 0x71, 0xa5, 0x5a, 0x4e, 0x75, 0, 0]
    );
    assert!(sections[0].relocations.is_empty());
}

#[test]
fn compact_macro_name_renamed_ordinary_label_rust_oracle() {
    assert_eq!(
        hunk_sections::rust_hunk_source(&renamed_ordinary_label_source()),
        hunk_sections::rust_hunk_source(&ordinary_label_source())
    );
}

#[test]
fn compact_macro_name_colon_ordinary_label_rust_oracle() {
    assert_eq!(
        hunk_sections::rust_hunk_source(&colon_ordinary_label_source()),
        hunk_sections::rust_hunk_source(&ordinary_label_source())
    );
}

#[test]
fn compact_macro_name_renamed_evaluator_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&renamed_evaluator_source());
    let sections = hunk::segments(&oracle).expect("valid renamed evaluator macro Hunk");
    assert_eq!(sections[0].payload.len(), 28);
    assert_eq!(oracle, hunk_sections::rust_hunk_source(&evaluator_source()));
}

#[test]
fn compact_macro_name_local_value_template_rust_oracle() {
    for (name, source) in local_facet_cases() {
        eprintln!("VALUE_TEMPLATE_CASE={name}");
        assert_facet_code(&source, &[3, 7, 0, 0]);
    }
}

#[test]
fn compact_macro_name_imported_value_template_rust_oracle() {
    for (name, source, expected) in imported_positive_cases() {
        eprintln!("VALUE_TEMPLATE_CASE={name}");
        assert_facet_code(&source, &expected);
    }
}

#[test]
fn compact_macro_name_value_template_rejections_rust_oracle() {
    for (name, source) in negative_facet_cases() {
        let error =
            rust_facet_rejection(&source).unwrap_or_else(|| panic!("{name} unexpectedly accepted"));
        assert!(
            error.to_ascii_lowercase().contains("shared"),
            "{name}: {error}"
        );
        if name.starts_with("duplicate") {
            assert!(
                error.to_ascii_lowercase().contains("already")
                    && error.to_ascii_lowercase().contains("defined"),
                "{name}: {error}"
            );
        } else {
            let expected = match name {
                "private_value" | "selected_private_value" | "selected_alias_private_value" => {
                    "Symbol is private"
                }
                "private_template"
                | "selected_private_template"
                | "selected_alias_template"
                | "selected_alias_template_only" => "Unknown directive",
                "template_only_numeric" => "Label not found",
                _ => unreachable!("unknown facet rejection"),
            };
            assert!(error.contains(expected), "{name}: {error}");
        }
        eprintln!("VALUE_TEMPLATE_REJECTION={name}: {error}");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; public block name also names a private macro"]
fn compact_macro_name_block_collision_fs_uae() {
    hunk_sections::native_hunk_source(&evaluator_source());
}

#[test]
#[ignore = "requires configured FS-UAE; ordinary instruction label also names a private macro"]
fn compact_macro_name_ordinary_label_collision_fs_uae() {
    hunk_sections::native_hunk_source(&ordinary_label_source());
}

#[test]
#[ignore = "requires configured FS-UAE; ordinary-label control with renamed macro"]
fn compact_macro_name_renamed_ordinary_label_fs_uae() {
    hunk_sections::native_hunk_source(&renamed_ordinary_label_source());
}

#[test]
#[ignore = "requires configured FS-UAE; ordinary-label collision with explicit colon"]
fn compact_macro_name_colon_ordinary_label_fs_uae() {
    hunk_sections::native_hunk_source(&colon_ordinary_label_source());
}

#[test]
#[ignore = "requires configured FS-UAE; evaluator macro renamed while routine stays evaluate"]
fn compact_macro_name_renamed_evaluator_fs_uae() {
    hunk_sections::native_hunk_source(&renamed_evaluator_source());
}

#[test]
#[ignore = "requires configured FS-UAE; value-first and template-first shared lexical names"]
fn compact_macro_name_local_value_template_fs_uae() {
    for (name, source) in local_facet_cases() {
        eprintln!("VALUE_TEMPLATE_CASE={name}");
        hunk_sections::native_hunk_source(&source);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; imported value/template facets and independent visibility"]
fn compact_macro_name_imported_value_template_fs_uae() {
    for (name, source, _) in imported_positive_cases() {
        eprintln!("VALUE_TEMPLATE_CASE={name}");
        hunk_sections::native_hunk_source(&source);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; duplicate facets, visibility and selected value precedence"]
fn compact_macro_name_value_template_rejections_fs_uae() {
    for (name, source) in negative_facet_cases() {
        eprintln!("VALUE_TEMPLATE_REJECTION={name}");
        assert_native_rejection(&source, "m68020");
    }
}
