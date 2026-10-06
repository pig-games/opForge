//! Qualified paths work relative to scopes, including CLI-created modules.
use super::*;

fn image_bytes(assembler: &Assembler) -> Vec<u8> {
    assembler
        .image()
        .entries()
        .unwrap()
        .into_iter()
        .map(|(_, byte)| byte)
        .collect()
}

fn cli_example(name: &str, expected: &[u8]) {
    let dir = create_temp_dir("qualified-scope-cli");
    let output = dir.join("output.bin");
    let input = workspace_root().join(format!("examples/opcore/{name}.asm"));
    let cli = Cli::parse_from([
        "opforge".to_string(),
        input.to_string_lossy().into_owned(),
        "--bin".to_string(),
        output.to_string_lossy().into_owned(),
    ]);
    let config = validate_cli(&cli).unwrap();
    let result = run_with_validated_cli_with_context(&cli, &config);
    let bytes = fs::read(&output);
    fs::remove_dir_all(dir).unwrap();
    result.unwrap_or_else(|error| panic!("{name}: {error:?}"));
    assert_eq!(bytes.unwrap(), expected, "{name}");
}

#[test]
fn qualified_scope_cli_blocks() {
    cli_example("scopes", &[2, 0, 1, 0, 5, 0]);
}

#[test]
fn qualified_scope_cli_namespaces() {
    cli_example("scopes_namespace", &[1, 0, 5, 0, 9, 0]);
}

#[test]
fn qualified_scope_cli_macro_invocation() {
    cli_example(
        "macro_invocation_native",
        &[0xa5, 0x12, 0x85, 0x34, 1, 2, 3, 3, 0, 9, 0],
    );
}

#[test]
fn qualified_scope_cli_macro_syntax() {
    cli_example(
        "macro_syntax",
        &[0x3a, 0x12, 0, 0x32, 0x34, 0, 1, 2, 3, 3, 0, 7, 9, 0],
    );
}

#[test]
fn qualified_scope_wrapped_and_unwrapped_paths() {
    let body = ".cpu 6502\n.namespace outer\n.namespace inner\nvalue .const 5\nvalues = {7,9}\n.endnamespace\n.word inner.value\n.endnamespace\n.word outer.inner.value,outer.inner.values[1]\n";
    for source in [body.to_string(), format!(".module app\n{body}.endmodule\n")] {
        let assembled = run_passes(&source.lines().collect::<Vec<_>>());
        assert_eq!(image_bytes(&assembled), [5, 0, 5, 0, 9, 0]);
    }
}

#[test]
fn qualified_scope_relative_binding_precedes_global_literal() {
    let assembled = run_passes(&[
        ".module globals",
        ".cpu 6502",
        "branch.value .const 1",
        ".word branch.value",
        ".endmodule",
        ".module app",
        ".namespace branch",
        "value .const 2",
        ".endnamespace",
        ".word branch.value,app.branch.value",
        ".endmodule",
    ]);
    assert_eq!(image_bytes(&assembled), [1, 0, 2, 0, 2, 0]);
}

#[test]
fn qualified_scope_import_alias_precedes_relative_and_literal_bindings() {
    let assembled = run_passes(&[
        ".module globals",
        ".cpu 6502",
        "lib.value .const 1",
        ".endmodule",
        ".module dependency",
        ".pub",
        "value .const 3",
        ".endmodule",
        ".module app",
        ".use dependency as lib",
        ".namespace lib",
        "value .const 2",
        ".endnamespace",
        ".word lib.value,dependency.value,app.lib.value",
        ".endmodule",
    ]);
    assert_eq!(image_bytes(&assembled), [3, 0, 3, 0, 2, 0]);
}

fn assert_error(lines: &[&str], message: &str) {
    let mut assembler = Assembler::new();
    let lines = lines
        .iter()
        .map(|line| line.to_string())
        .collect::<Vec<_>>();
    assembler.pass1(&lines);
    let mut output = Vec::new();
    let mut listing = ListingWriter::new(&mut output, false);
    assembler.pass2(&lines, &mut listing).unwrap();
    assert!(
        assembler.diagnostics.iter().any(|diagnostic| {
            diagnostic.severity == Severity::Error && diagnostic.error.message().contains(message)
        }),
        "expected {message}: {:?}",
        assembler.diagnostics
    );
}

#[test]
fn qualified_scope_missing_import_target_does_not_fall_back_to_local() {
    assert_error(
        &[
            ".module dependency",
            ".pub",
            "other .const 3",
            ".endmodule",
            ".module app",
            ".use dependency as lib",
            ".namespace lib",
            "value .const 2",
            ".endnamespace",
            ".word lib.value",
            ".endmodule",
        ],
        "Label not found",
    );
}

#[test]
fn qualified_scope_private_import_does_not_fall_back_to_local() {
    assert_error(
        &[
            ".module dependency",
            "value .const 3",
            ".endmodule",
            ".module app",
            ".use dependency as lib",
            ".namespace lib",
            "value .const 2",
            ".endnamespace",
            ".word lib.value",
            ".endmodule",
        ],
        "Symbol is private",
    );
}

#[test]
fn qualified_scope_forward_relative_reference_retains_whole_imported_block() {
    let assembled = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "entry .block",
        ".word helper.inside",
        ".bend",
        "helper .block",
        ".byte $11",
        "inside",
        ".byte $22",
        ".bend",
        "unused .block",
        ".byte $33",
        ".bend",
        ".endsection",
        ".endmodule",
        ".module app",
        ".cpu 68000",
        ".section app_code, kind=code",
        ".endsection",
        ".use dep (entry) as d map { code -> app_code }",
        ".section refs, kind=data",
        ".word d.entry",
        ".endsection",
        ".endmodule",
    ]);
    assert_eq!(assembled.sections()["app_code"].bytes, [0, 3, 0x11, 0x22]);
    assert!(assembled.symbols.entry("dep.helper").is_some());
    assert!(assembled.symbols.entry("dep.helper.inside").is_some());
    assert!(assembled.symbols.entry("dep.unused").is_none());
}

#[test]
fn qualified_scope_forward_import_alias_tracks_the_imported_block() {
    let assembled = run_passes(&[
        ".module app",
        ".cpu 68000",
        ".section app_code, kind=code",
        ".endsection",
        ".use dep (entry) as d map { code -> app_code }",
        ".section refs, kind=data",
        ".word d.entry",
        "d.entry .const $1234",
        ".endsection",
        ".endmodule",
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "entry .block",
        ".byte $42",
        ".bend",
        "unused .block",
        ".byte $33",
        ".bend",
        ".endsection",
        ".endmodule",
    ]);
    assert_eq!(assembled.sections()["app_code"].bytes, [0x42]);
    assert_eq!(assembled.sections()["refs"].bytes, [0, 0]);
    assert!(assembled.symbols.entry("dep.entry").is_some());
    assert!(assembled.symbols.entry("dep.unused").is_none());
}
