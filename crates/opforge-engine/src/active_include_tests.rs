// SPDX-License-Identifier: GPL-3.0-or-later
use crate::*;

fn assemble(
    provider: &MemorySourceProvider,
    defines: &[String],
) -> (Assembler, Vec<String>, String) {
    assemble_with_depth(provider, defines, 32)
}

fn assemble_with_depth(
    provider: &MemorySourceProvider,
    defines: &[String],
    depth: usize,
) -> (Assembler, Vec<String>, String) {
    let prepared = prepare_assembly_session(AssemblyPreparationRequest {
        root_path: Path::new("/virtual/main.asm"),
        defines,
        include_roots: &[PathBuf::from("/virtual")],
        module_paths: &[],
        pp_macro_depth: depth,
        registry: build_default_asm_registry(),
        cpu_override: None,
        default_cpu: CpuType::new("8085"),
        max_loop_iterations: 1000,
        source_provider: Some(provider),
    })
    .expect("prepare sources");
    let mut assembler = Assembler::new();
    assembler.root_metadata.root_module_id = Some("main".into());
    assembler.module_macro_names = prepared.module_macro_names.clone();
    assembler.module_template_seeds = prepared.module_template_seeds.clone();
    assembler.module_parameters = prepared.module_parameters.clone();
    assembler.set_source_resources(prepared.source_map.clone());
    let lines = prepared.expanded_lines;
    let pass1 = assembler.pass1(&lines);
    let mut listing = Vec::new();
    if pass1.errors == 0 {
        assembler
            .pass2(
                &lines,
                &mut asm::listing::ListingWriter::new(&mut listing, false),
            )
            .unwrap();
    }
    (assembler, lines, String::from_utf8(listing).unwrap())
}

#[test]
fn active_include_and_macro_templates_survive_unreferenced_block_relayout() {
    let provider = MemorySourceProvider::default()
        .with_file(
            "/virtual/main.asm",
            ".module main\n.use provider\nSCAN .macro suffix\n.for 1\n.if 0\n.byte provider.routine.saved@1\n.endif\n.include \"leaf.inc\"\n.endfor\n.endmacro\n.SCAN 1\n.endmodule\n",
        )
        .with_file(
            "/virtual/provider.asm",
            ".module provider\n.pub\nroutine .block\nsaved1 .const 66\n.byte 99\n.bend\n.endmodule\n",
        )
        .with_file("/virtual/leaf.inc", ".byte 60\n");
    let (assembler, _, _) = assemble(&provider, &[]);
    assert!(
        assembler.diagnostics.is_empty(),
        "{:?}",
        assembler.diagnostics
    );
    assert_eq!(assembler.image().entries().unwrap(), vec![(0, 60)]);
    assert_eq!(
        assembler.binary_resource_dependencies(),
        vec![PathBuf::from("/virtual/leaf.inc")]
    );
}

#[test]
fn active_include_executes_only_selected_statements_and_introduces_templates_and_constants() {
    let provider = MemorySourceProvider::default()
        .with_file("/virtual/main.asm", ".module main\n.use provider\n.org 0\nEMIT .macro value\n.byte .value\n.endmacro\n.include \"defs/templates.inc\"\n.LOAD \"data.inc\"\n.dynamic 9\n.if VALUE\n.include \"loop.inc\"\n.endif\n.for 2\n.include \"loop.inc\"\n.endfor\n.if 0\n.include \"missing-if.inc\"\n.endif\n.for 0\n.include \"missing-for.inc\"\n.endfor\n.while 0\n.include \"missing-while.inc\"\n.endwhile\n.endmodule\n")
        .with_file("/virtual/provider.asm", ".module provider\n.pub\nunused .block\n.byte 99\n.bend\n.endmodule\n")
        .with_file("/virtual/defs/templates.inc", "LOAD .segment file\n.include .file\n.endsegment\nUNUSED .macro\n.include \"missing-unused.inc\"\n.endmacro\n")
        .with_file("/virtual/defs/data.inc", "VALUE .const 1\n.emit 7\nDYNAMIC .macro value\n.byte .value\n.endmacro\n")
        .with_file("/virtual/loop.inc", ".byte 3\n");
    let (assembler, _, _) = assemble(&provider, &[]);
    assert!(
        assembler.diagnostics.is_empty(),
        "{:?}",
        assembler.diagnostics
    );
    assert_eq!(
        assembler.image().entries().unwrap(),
        vec![(0, 7), (1, 9), (2, 3), (3, 3), (4, 3)]
    );
    let mut dependencies = assembler.binary_resource_dependencies();
    dependencies.sort();
    assert_eq!(
        dependencies,
        vec![
            PathBuf::from("/virtual/defs/data.inc"),
            PathBuf::from("/virtual/loop.inc")
        ]
    );
}

#[test]
fn active_include_preserves_cli_defines_and_mixed_conditional_ownership() {
    let provider = MemorySourceProvider::default()
        .with_file("/virtual/main.asm", ".module main\n.ifdef ENABLE\n.if 0\n.include \"missing.inc\"\n.endif\n.for 0\n.ifdef ENABLE\n.include \"missing-loop.inc\"\n.endif\n.endfor\n.if 1\n.include \"enabled.inc\"\n.endif\n.endif\n.endmodule\n")
        .with_file("/virtual/enabled.inc", ".ifdef ENABLE\n.byte 5\n.else\n.byte 99\n.endif\n");
    let (assembler, _, _) = assemble(&provider, &["ENABLE".into()]);
    assert!(
        assembler.diagnostics.is_empty(),
        "{:?}",
        assembler.diagnostics
    );
    assert_eq!(assembler.image().entries().unwrap(), vec![(0, 5)]);
}

#[test]
fn active_include_retains_diagnostic_origin_and_listing_source() {
    let provider = MemorySourceProvider::default()
        .with_file(
            "/virtual/main.asm",
            ".module main\n.if 1\n.include \"bad.inc\"\n.endif\n.endmodule\n",
        )
        .with_file("/virtual/bad.inc", "; first\n.byte missing_symbol\n");
    let (assembler, _, listing) = assemble(&provider, &[]);
    let diagnostic = assembler
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.file.as_deref() == Some("/virtual/bad.inc"))
        .expect("included diagnostic origin");
    assert_eq!(diagnostic.line, 2);
    assert_eq!(diagnostic.source.as_deref(), Some(".byte missing_symbol"));
    assert!(listing.contains(".byte missing_symbol"), "{listing}");
}

#[test]
fn active_include_retains_preprocessor_error_origin() {
    let provider = MemorySourceProvider::default()
        .with_file(
            "/virtual/main.asm",
            ".module main\n.if 1\n.include \"bad.inc\"\n.endif\n.endmodule\n",
        )
        .with_file("/virtual/bad.inc", "; first\n.ifdef\n.endif\n");
    let (assembler, _, _) = assemble(&provider, &[]);
    let diagnostic = assembler
        .diagnostics
        .iter()
        .find(|diagnostic| diagnostic.file.as_deref() == Some("/virtual/bad.inc"))
        .expect("acquired preprocessor error origin");
    assert_eq!(diagnostic.line, 2);
    assert_eq!(diagnostic.source.as_deref(), Some(".ifdef"));
}

#[test]
fn active_include_rejects_cycles_and_module_discovery_changes() {
    for (body, message) in [
        (".include \"main.asm\"\n", "cycle"),
        (
            ".module introduced\n.endmodule\n",
            "module discovery support",
        ),
    ] {
        let provider = MemorySourceProvider::default()
            .with_file(
                "/virtual/main.asm",
                ".module main\n.if 1\n.include \"child.inc\"\n.endif\n.endmodule\n",
            )
            .with_file("/virtual/child.inc", body);
        let (assembler, _, _) = assemble(&provider, &[]);
        assert!(
            assembler
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.error.message().contains(message)),
            "{:?}",
            assembler.diagnostics
        );
    }
}

#[test]
fn active_include_preserves_lexical_scopes_and_imported_template_aliases() {
    let provider = MemorySourceProvider::default()
        .with_file("/virtual/main.asm", ".module main\n.use dep (EMIT as imported)\n.namespace ns\n.block\n.if 1\n.include \"scoped.inc\"\n.endif\n.local 8\n.endblock\n.endnamespace\n.byte 9\nitems .bfor 0\n.include \"missing-labeled.inc\"\n.endfor\n.endmodule\n")
        .with_file("/virtual/dep.asm", ".module dep\n.pub\nEMIT .macro value\n.byte .value\n.endmacro\n.endmodule\n")
        .with_file("/virtual/scoped.inc", ".imported 7\nLOCAL .segment value\n.byte .value\n.endsegment\n");
    let (assembler, _, _) = assemble(&provider, &[]);
    assert!(
        assembler.diagnostics.is_empty(),
        "{:?}",
        assembler.diagnostics
    );
    assert_eq!(
        assembler.image().entries().unwrap(),
        vec![(0, 7), (1, 8), (2, 9)]
    );
}

#[test]
fn active_include_substitutes_statement_body_filename() {
    let provider = MemorySourceProvider::default()
        .with_file("/virtual/main.asm", ".module main\n.statement LOAD byte:value\n.include .value\n.endstatement\n    LOAD 7\n.endmodule\n")
        .with_file("/virtual/7", ".byte 4\n");
    let (assembler, _, _) = assemble(&provider, &[]);
    assert!(
        assembler.diagnostics.is_empty(),
        "{:?}",
        assembler.diagnostics
    );
    assert_eq!(assembler.image().entries().unwrap(), vec![(0, 4)]);
}

#[test]
fn active_include_streams_namespace_state_across_file_and_macro_boundaries() {
    for main in [
        ".module main\nF .macro\n.byte 1\n.endmacro\n.if 1\n.include \"opens.inc\"\n.endif\n.endnamespace\n.F\n.endmodule\n",
        ".module main\nF .macro\n.byte 1\n.endmacro\n.include \"templates.inc\"\n.if 1\n.CROSS\n.endif\n.F\n.endmodule\n",
    ] {
        let provider = MemorySourceProvider::default().with_file("/virtual/main.asm", main)
            .with_file("/virtual/opens.inc", ".namespace N\nF .macro\n.byte 2\n.endmacro\n")
            .with_file("/virtual/templates.inc", "CROSS .macro\n.namespace N\n.include \"defines.inc\"\n.F\n.endnamespace\n.endmacro\n")
            .with_file("/virtual/defines.inc", "F .macro\n.byte 2\n.endmacro\n");
        let (assembler, _, _) = assemble(&provider, &[]);
        assert!(assembler.diagnostics.is_empty(), "{:?}", assembler.diagnostics);
        let entries = assembler.image().entries().unwrap();
        assert_eq!(entries.last(), Some(&(entries.len() as u32 - 1, 1)));
    }
}

#[test]
fn active_include_rejects_unresolved_activity_before_acquisition() {
    for condition in ["later == 0", "later"] {
        let provider = MemorySourceProvider::default()
            .with_file("/virtual/main.asm", &format!(".module main\n.if {condition}\n.include \"unread.inc\"\n.endif\nlater = 1\n.endmodule\n"))
            .with_file("/virtual/unread.inc", ".byte 4\n");
        let (assembler, _, _) = assemble(&provider, &[]);
        assert!(
            assembler.diagnostics.iter().any(|diagnostic| diagnostic
                .error
                .message()
                .contains("forward-dependent source acquisition")),
            "{:?}",
            assembler.diagnostics
        );
        assert!(assembler.binary_resource_dependencies().is_empty());
    }
}

#[test]
fn active_include_does_not_acquire_after_invalid_or_short_circuited_guards() {
    for main in [
        ".module main\n.if 1\n.byte 1\n.elseif missing\n.include \"missing.inc\"\n.endif\n.endmodule\n",
        ".module main\n.match 1\n.case 1\n.byte 1\n.case missing\n.include \"missing.inc\"\n.endmatch\n.endmodule\n",
    ] {
        let provider = MemorySourceProvider::default().with_file("/virtual/main.asm", main);
        let (assembler, _, _) = assemble(&provider, &[]);
        assert!(assembler.diagnostics.is_empty(), "{:?}", assembler.diagnostics);
        assert!(assembler.binary_resource_dependencies().is_empty());
    }
    let provider = MemorySourceProvider::default()
        .with_file(
            "/virtual/main.asm",
            ".module main\n.if 1/0\n.include \"unread.inc\"\n.endif\n.endmodule\n",
        )
        .with_file("/virtual/unread.inc", ".byte 9\n");
    let (assembler, _, _) = assemble(&provider, &[]);
    assert!(!assembler.diagnostics.is_empty());
    assert!(assembler.binary_resource_dependencies().is_empty());
}

#[test]
fn active_include_rejects_expanded_module_changes_but_ignores_inactive_changes() {
    let provider = MemorySourceProvider::default()
        .with_file("/virtual/main.asm", ".module main\n.if 1\n.include \"defs.inc\"\n.endif\n.CHANGE .module\n.endmodule\n")
        .with_file("/virtual/defs.inc", "CHANGE .segment directive\n.directive other\n.endsegment\n.if 0\n.module unused\n.endmodule\n.endif\n");
    let (assembler, _, _) = assemble(&provider, &[]);
    assert!(
        assembler.diagnostics.iter().any(|diagnostic| diagnostic
            .error
            .message()
            .contains("module discovery support")),
        "{:?}",
        assembler.diagnostics
    );
}

#[test]
fn active_include_streamed_template_bodies_do_not_close_live_loops() {
    let provider = MemorySourceProvider::default()
        .with_file("/virtual/main.asm", ".module main\n.if 1\n.include \"empty.inc\"\n.endif\n.for 2\nUNUSED .macro\n.endfor\n.endmacro\n.byte 7\n.endfor\n.endmodule\n")
        .with_file("/virtual/empty.inc", "; empty\n");
    let (assembler, _, _) = assemble(&provider, &[]);
    assert!(
        assembler.diagnostics.is_empty(),
        "{:?}",
        assembler.diagnostics
    );
    assert_eq!(assembler.image().entries().unwrap(), vec![(0, 7), (1, 7)]);
}

#[test]
fn active_include_requires_repetition_delimiters_to_stay_in_one_source() {
    let provider = MemorySourceProvider::default()
        .with_file(
            "/virtual/main.asm",
            ".module main\n.if 1\n.include \"opener.inc\"\n.byte 7\n.endfor\n.endif\n.endmodule\n",
        )
        .with_file("/virtual/opener.inc", ".for 2\n");
    let (assembler, _, _) = assemble(&provider, &[]);
    assert!(
        assembler
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.error.message().contains("unterminated .for")),
        "{:?}",
        assembler.diagnostics
    );
}

#[test]
fn active_include_prefix_templates_ignore_inactive_anonymous_block_ordinals() {
    let provider = MemorySourceProvider::default()
        .with_file("/virtual/main.asm", ".module main\n.if 0\n.block\n.endblock\n.endif\n.block\nF .macro\n.byte 7\n.endmacro\n.if 1\n.include \"calls.inc\"\n.endif\n.endblock\n.endmodule\n")
        .with_file("/virtual/calls.inc", ".F\n");
    let (assembler, _, _) = assemble(&provider, &[]);
    assert!(
        assembler.diagnostics.is_empty(),
        "{:?}",
        assembler.diagnostics
    );
    assert_eq!(assembler.image().entries().unwrap(), vec![(0, 7)]);
}

#[test]
fn active_include_cannot_call_templates_defined_by_later_source() {
    let provider = MemorySourceProvider::default()
        .with_file("/virtual/main.asm", ".module main\n.if 1\n.include \"early.inc\"\n.endif\n.include \"future.inc\"\n.endmodule\n")
        .with_file("/virtual/early.inc", ".FUTURE\n")
        .with_file("/virtual/future.inc", "FUTURE .macro\n.endmacro\n");
    let (assembler, _, _) = assemble(&provider, &[]);
    assert!(
        assembler
            .diagnostics
            .iter()
            .any(
                |diagnostic| diagnostic.file.as_deref() == Some("/virtual/early.inc")
                    && diagnostic.error.message().contains("Unknown directive")
            ),
        "{:?}",
        assembler.diagnostics
    );
}

#[test]
fn active_include_depth_counts_the_originating_root() {
    let provider = MemorySourceProvider::default()
        .with_file(
            "/virtual/main.asm",
            ".module main\n.if 1\n.include \"unread.inc\"\n.endif\n.endmodule\n",
        )
        .with_file("/virtual/unread.inc", ".byte 4\n");
    let (assembler, _, _) = assemble_with_depth(&provider, &[], 1);
    assert!(
        assembler
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.error.message().contains("maximum depth (1)")),
        "{:?}",
        assembler.diagnostics
    );
    assert!(assembler.binary_resource_dependencies().is_empty());
}
