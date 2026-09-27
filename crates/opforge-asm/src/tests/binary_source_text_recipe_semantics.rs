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
