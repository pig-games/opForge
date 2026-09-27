//! Substitution precedes string decoding in the authoritative Rust language.
use super::*;

fn source(body: &str, argument: &str) -> String {
    format!(".module app\n.cpu m6502\nemit .macro name\n.byte {body}\n.endmacro\n.emit {argument}\n.endmodule\n")
}

#[test]
fn text_recipe_ordering_rust_oracles() {
    for (body, argument, expected) in [
        (r#""x@1""#, "A", &b"xA"[..]),
        (r#""literal""#, "A", &b"literal"[..]),
        ("7", "A", &[7][..]),
        (r#""\x401""#, "A", &[b'@', b'1'][..]),
        (r#""\x2ename""#, "A", &b".name"[..]),
        (r#""@1""#, r#"A",7,"B"#, &[b'A', 7, b'B'][..]),
    ] {
        let input = source(body, argument);
        assert_eq!(
            graph::oracle_with_roots(&[("input.asm", &input)], &[]).unwrap(),
            expected
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; ordinary substitution baseline"]
fn text_recipe_control_substitution_fs_uae() {
    native(source(r#""x@1""#, "A"));
}

#[test]
#[ignore = "requires configured FS-UAE; literal macro baseline"]
fn text_recipe_control_literal_fs_uae() {
    native(source(r#""literal""#, "A"));
}

#[test]
#[ignore = "requires configured FS-UAE; numeric macro baseline"]
fn text_recipe_control_numeric_fs_uae() {
    native(source("7", "A"));
}

fn native(input: String) {
    native_cpu(input, "m6502");
}

fn native_cpu(input: String, cpu: &str) {
    let expected = graph::oracle_with_roots(&[("input.asm", &input)], &[]).unwrap();
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        input.as_bytes(),
        Some(&expected),
    )
    .expect("fresh full CLI substitution comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real native proof required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        super::macro_calls::check_memory(
            &runs[0].captured_artifacts[&PathBuf::from("Work/memory.bin")],
            0,
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; isolate CPU package from macro baseline"]
fn text_recipe_numeric_m68020_fs_uae() {
    native_cpu(source("7", "A").replace("m6502", "m68020"), "m68020");
}

#[test]
#[ignore = "requires configured FS-UAE; isolate argument spelling from macro baseline"]
fn text_recipe_numeric_immediate_fs_uae() {
    native(source("7", "#0"));
}

#[test]
#[ignore = "requires configured FS-UAE; isolate explicit module from macro baseline"]
fn text_recipe_numeric_implicit_module_fs_uae() {
    native(
        source("7", "A")
            .replace(".module app\n", "")
            .replace(".endmodule\n", ""),
    );
}

#[test]
#[ignore = "requires configured FS-UAE; escaped marker must not become a placeholder"]
fn text_recipe_escaped_positional_fs_uae() {
    native(source(r#""\x401""#, "A"));
}

#[test]
#[ignore = "requires configured FS-UAE; escaped dot must remain literal"]
fn text_recipe_escaped_named_fs_uae() {
    native(source(r#""\x2ename""#, "A"));
}

#[test]
#[ignore = "requires configured FS-UAE; binary expansion must preserve Rust quote structure"]
fn text_recipe_quote_structure_fs_uae() {
    native(source(r#""@1""#, r#"A",7,"B"#));
}

// Keep several lexical state changes in one bounded native assembly.
fn ordering_batch() -> String {
    let mut text = String::from(".module app\n.cpu m6502\n");
    for (index, body, argument) in [
        (0, r#""\x401""#, "A"),
        (1, r#""\x2ename""#, "A"),
        (2, r#""@1""#, r#"A",7,"B"#),
        (3, r#""@1",9"#, r#"A";ignored"B"#),
        (4, r#""\@1""#, "x41"),
        (5, r#"".@""#, "A"),
        (6, r#"".{name}""#, "B"),
        (7, r#"".unknown""#, "C"),
        (8, r#""@1",2+3*4"#, "D"),
    ] {
        text.push_str(&format!(
            "emit{index} .macro name\n.byte {body}\n.endmacro\n.emit{index} {argument}\n"
        ));
    }
    text.push_str(&format!(
        "longline .macro name\n.byte \"{}@1\"\n.endmacro\n.longline Q\n",
        "\\x41".repeat(70)
    ));
    text.push_str(&format!(
        "repeated .macro name\n.byte \"{}\"\n.endmacro\n.repeated R\n",
        "@1".repeat(20)
    ));
    text.push_str(
        "defaulted .macro name=Z\n.byte \"@1\"\n.endmacro\n.defaulted\n.defaulted Y\n.endmodule\n",
    );
    text
}

#[test]
fn text_recipe_complete_line_rust_oracle() {
    let mut expected = b"@1.nameA\x07BAAAB.unknownD\x0e".to_vec();
    expected.extend(std::iter::repeat_n(b'A', 70));
    expected.push(b'Q');
    expected.extend(std::iter::repeat_n(b'R', 20));
    expected.extend(b"ZY");
    assert_eq!(
        graph::oracle_with_roots(&[("input.asm", &ordering_batch())], &[]).unwrap(),
        expected
    );
}

#[test]
#[ignore = "requires configured FS-UAE; substitution before whole-line tokenization"]
fn text_recipe_complete_line_fs_uae() {
    native(ordering_batch());
}
