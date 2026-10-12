//! Shared metadata completion and ownership controls for the native block slice.
use super::*;

fn assemble_metadata(lines: &[&str]) -> Assembler {
    let mut assembler = Assembler::new();
    assembler.set_runtime_line_router(Some(make_test_runtime_line_router(ExecutionMode::Rust)));
    assembler.root_metadata.root_module_id = Some("main".into());
    assembler.pass1(
        &lines
            .iter()
            .map(|line| (*line).to_string())
            .collect::<Vec<_>>(),
    );
    assembler
}

#[test]
fn metadata_blocks_reject_unclosed_implicit_blocks_at_source_boundary() {
    for (lines, expected) in [
        (vec![".meta"], "Found .meta without .endmeta"),
        (vec![".meta", ".output"], "Found .output without .endoutput"),
        (
            vec![".meta", ".output", ".z80"],
            "Found .z80 without .endz80",
        ),
    ] {
        let mut assembler = assemble_metadata(&lines);
        assert!(
            assembler
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.error.message() == expected),
            "missing completion diagnostic {expected:?}: {:?}",
            assembler.diagnostics
        );
        assembler.clear_diagnostics();
        let owned = lines
            .iter()
            .map(|line| (*line).to_string())
            .collect::<Vec<_>>();
        let mut listing = ListingWriter::new(Vec::new(), false);
        let counts = assembler.pass2(&owned, &mut listing).unwrap();
        assert!(counts.errors > 0);
        assert!(assembler
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.error.message() == expected));
    }
}

#[test]
fn metadata_blocks_keep_active_values_and_canonical_short_cpu_overrides() {
    let assembler = assemble_metadata(&[
        ".module main",
        ".meta",
        ".name \"Description\"",
        ".output",
        ".name \"default\"",
        ".bin",
        ".if 0",
        ".name \"inactive\"",
        ".z80",
        ".name \"inactive-target\"",
        ".endz80",
        ".else",
        ".Z80",
        ".name \"target\"",
        ".bin \"0000:0003\"",
        ".endZ80",
        ".endif",
        ".endoutput",
        ".endmeta",
        ".endmodule",
    ]);
    assert!(
        assembler.diagnostics.is_empty(),
        "{:?}",
        assembler.diagnostics
    );
    assert_eq!(assembler.root_metadata.name.as_deref(), Some("Description"));
    let default = assembler.root_metadata.output_config_for_cpu("i8085");
    let target = assembler.root_metadata.output_config_for_cpu("z80");
    assert_eq!(default.name.as_deref(), Some("default"));
    assert!(default.bin_specs[0].range.is_none());
    assert_eq!(target.name.as_deref(), Some("target"));
    assert_eq!(target.bin_specs.len(), 1, "target vector replaces defaults");
    assert_eq!(target.bin_specs[0].range.as_ref().unwrap().end, 3);
}

#[test]
fn metadata_blocks_reject_nonroot_nested_and_malformed_controls() {
    for lines in [
        vec![".module imported", ".meta", ".output", ".z80"],
        vec![".module main", ".block", ".meta"],
        vec![".module main", ".meta", ".output", ".z80", ".end6502"],
        vec![".module main", ".meta", ".output", ".z80", ".endoutput"],
        vec![".module main", ".meta", ".output", ".z80", ".z80"],
        vec![".module main", ".meta", ".output", ".output.name \"bad\""],
        vec![".module main", ".meta", ".output", ".byte 1"],
    ] {
        let assembler = assemble_metadata(&lines);
        assert!(
            !assembler.diagnostics.is_empty(),
            "invalid metadata accepted: {lines:?}"
        );
    }
    let inactive = assemble_metadata(&[".if 0", ".meta", ".output", ".z80", ".endif"]);
    assert!(
        inactive.diagnostics.is_empty(),
        "inactive block delimiters must not change metadata state: {:?}",
        inactive.diagnostics
    );
}

#[test]
fn metadata_blocks_preserve_current_fixture_metadata_and_emitted_bytes() {
    for (fixture, target_name) in [
        ("module_metadata_output.asm", Some("meta-demo-6502")),
        ("module_metadata_block.asm", Some("meta-block-demo-6502")),
        ("module_metadata_outputs.asm", None),
    ] {
        let path = workspace_root().join("examples/opcore").join(fixture);
        let root_lines = expand_source_file(&path, &[], &[], 64).unwrap();
        let graph = load_module_graph(&path, root_lines, &[], &[], &[], 64).unwrap();
        let owned = graph.lines;
        let mut assembler = Assembler::new();
        assembler.set_runtime_line_router(Some(make_test_runtime_line_router(ExecutionMode::Rust)));
        assembler.root_metadata.root_module_id = Some("main".into());
        assembler.module_macro_names = graph.module_macro_names;
        assembler.module_template_seeds = graph.module_template_seeds;
        assembler.module_parameters = graph.module_parameters;
        assembler.set_source_resources(graph.source_map);
        assert_eq!(assembler.pass1(&owned).errors, 0);
        assert!(
            assembler.diagnostics.is_empty(),
            "{fixture}: {:?}",
            assembler.diagnostics
        );
        let target = assembler.root_metadata.output_config_for_cpu("m6502");
        assert_eq!(target.name.as_deref(), target_name);
        if target_name.is_none() {
            assert_eq!(target.list_name.as_deref(), Some(""));
            assert_eq!(target.hex_name.as_deref(), Some("meta-hex"));
            assert_eq!(target.bin_specs[0].range.as_ref().unwrap().end, 3);
        }
        let mut listing = ListingWriter::new(Vec::new(), false);
        assert_eq!(assembler.pass2(&owned, &mut listing).unwrap().errors, 0);
        assert_eq!(
            assembler.image().entries().unwrap(),
            vec![(0, 1), (1, 2), (2, 3), (3, 4)]
        );
    }
}
