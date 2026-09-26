//! Numeric native dependency ordering; real files supply a live Rust graph oracle.
use super::*;

#[path = "binary_source_map_measurement.rs"]
mod measurement;

#[path = "binary_source_implicit_modules.rs"]
mod implicit_modules;
#[path = "binary_source_include_parent.rs"]
mod include_parent;
#[path = "binary_source_parameters.rs"]
mod parameters;
#[path = "binary_source_wildcard.rs"]
mod wildcard;

const ROOT: &str =
    ".module main\n.cpu m6502\n.use alpha\n.use beta\n.pub\nentry\n.byte 4\n.endmodule\n.end\n";
const A: &str = ".module alpha\n.cpu m6502\n.use shared\n.pub\nentry\n.byte 2\n.endmodule\n.end\n";
const B: &str = ".module beta\n.cpu m6502\n.use shared\n.pub\nentry\n.byte 3\n.endmodule\n.end\n";
const SHARED: &str =
    ".module shared\n.cpu m6502\n.org $1000\n.pub\nentry\n.byte 1\n.endmodule\n.end\n";
const DIAMOND: &[(&str, &str)] = &[
    ("main.asm", ROOT),
    ("beta.asm", B),
    ("shared.asm", SHARED),
    ("alpha.asm", A),
];
const ENTRY_SIBLINGS: &[(&str, &str)] = &[("main.asm", ".module main\n.cpu m6502\n.use helper\n.byte 2\n.endmodule\n.module helper\n.cpu m6502\n.org $1000\n.byte 1\n.endmodule\n.end\n")];
const UNUSED: &[(&str, &str)] = &[
    ("main.asm", ".module main\n.cpu m6502\n.use chosen\n.byte 2\n.endmodule\n.end\n"),
    ("library.asm", ".module unused\n.use absent\n.byte missing\n.endmodule\n.module chosen\n.cpu m6502\n.org $1000\n.byte 1\n.endmodule\n.end\n"),
];

const BLOCK_BOUNDARIES: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep\n.word dep.entry.inside\n.endmodule\n.end\n",
    ),
    (
        "dep.asm",
        ".module dep\n.cpu m6502\n.org $1000\n.pub\nentry .block\n.byte $11\ninside:\n.byte $22\n.bend\n.endmodule\n.end\n",
    ),
];

// Native's current single-PC experiment selects whole blocks before its two
// passes. Rust's mapped-section linker is the semantic reference, but this
// unsectioned source cannot be compared as an identical Rust output yet.
const BLOCK_REACHABILITY: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep\n.word dep.entry.inside\n.endmodule\n.end\n",
    ),
    (
        "dep.asm",
        ".module dep\n.cpu m6502\n.org $1000\n.pub\nentry .block\n.byte $11\ninside:\n.byte $22\n.bend\nunused .block\n.byte $99\n.bend\n.endmodule\n.end\n",
    ),
];

const BLOCK_TRANSITIVE: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep\n.word dep.entry\n.endmodule\n.end\n",
    ),
    (
        "dep.asm",
        ".module dep\n.cpu m6502\n.org $1000\n.pub\nentry .block\n.word dep.next\n.bend\nnext .block\n.byte $22\n.bend\nunused .block\n.byte $99\n.bend\n.endmodule\n.end\n",
    ),
];

const BLOCK_PRECEDING_LABEL: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep\n.word dep.before\n.endmodule\n.end\n",
    ),
    (
        "dep.asm",
        ".module dep\n.cpu m6502\n.org $1000\n.pub\nbefore:\nentry .block\n.byte $11\n.bend\nunused .block\n.byte $99\n.bend\n.endmodule\n.end\n",
    ),
];

const BLOCK_ENTRY_ROOT: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep\nroot .block\n.byte $11\n.bend\n.word dep.entry\n.endmodule\n.end\n",
    ),
    (
        "dep.asm",
        ".module dep\n.cpu m6502\n.org $1000\n.pub\nentry .block\n.byte $22\n.bend\nunused .block\n.byte $99\n.bend\n.endmodule\n.end\n",
    ),
];

const SELECTED_UNUSED: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (entry)\n.byte $aa\n.endmodule\n.end\n",
    ),
    (
        "dep.asm",
        ".module dep\n.cpu m6502\n.pub\nentry .block\n.byte $11\n.bend\nunused .block\n.byte $99\n.bend\n.endmodule\n.end\n",
    ),
];

const SELECTED_REFERENCED: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (entry)\n.word entry\n.endmodule\n.end\n",
    ),
    SELECTED_UNUSED[1],
];

const SELECTED_ALIASED: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (entry) as d\n.word d.entry\n.endmodule\n.end\n",
    ),
    SELECTED_UNUSED[1],
];

const SELECTED_ITEM_ALIAS: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.region rom, $1000, $10ff\n.use dep (entry as chosen)\n.section code\n.word chosen\n.endsection\n.place code in rom\n.endmodule\n.end\n",
    ),
    (
        "library/dep.asm",
        ".module dep\n.cpu m6502\n.pub\n.section code, logical\nentry .block\n.byte $11\n.bend\nunused .block\n.byte $99\n.bend\n.endsection\n.endmodule\n.end\n",
    ),
];

#[test]
fn binary_graph_selected_item_alias_rust_oracle() {
    assert_eq!(
        oracle_address_ordered_with_roots(SELECTED_ITEM_ALIAS, &["library"]).unwrap(),
        [0x11, 0x00, 0x10]
    );
    let qualified = SELECTED_ITEM_ALIAS[0]
        .1
        .replace(
            ".use dep (entry as chosen)",
            ".use dep (entry as chosen) as d",
        )
        .replace(".word chosen", ".word d.chosen");
    assert!(oracle_address_ordered_with_roots(
        &[("main.asm", &qualified), SELECTED_ITEM_ALIAS[1]],
        &["library"],
    )
    .unwrap_err()
    .contains("Qualified selective imports cannot use per-item aliases"));

    let multiple = SELECTED_ITEM_ALIAS[0]
        .1
        .replace(
            ".use dep (entry as chosen)",
            ".use dep (entry as chosen, unused as sibling)",
        )
        .replace(".word chosen", ".word chosen, sibling");
    assert_eq!(
        oracle_address_ordered_with_roots(
            &[("main.asm", &multiple), SELECTED_ITEM_ALIAS[1]],
            &["library"],
        )
        .unwrap(),
        [0x11, 0x99, 0x00, 0x10, 0x01, 0x10]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; direct selective item alias"]
fn binary_graph_selected_item_alias_fs_uae() {
    let expected = oracle_address_ordered_with_roots(SELECTED_ITEM_ALIAS, &["library"]).unwrap();
    native(
        SELECTED_ITEM_ALIAS,
        "m6502",
        Some(&expected),
        Some(&["library"]),
    );
}

#[test]
#[ignore = "requires configured FS-UAE; direct selective item alias through compact CLI"]
fn compact_cli_selected_item_alias_fs_uae() {
    let expected = oracle_address_ordered_with_roots(SELECTED_ITEM_ALIAS, &["library"]).unwrap();
    compact_cli(
        SELECTED_ITEM_ALIAS,
        &["library"],
        &[],
        Some(&expected),
        false,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; multiple direct item aliases"]
fn compact_cli_multiple_item_aliases_fs_uae() {
    let multiple = SELECTED_ITEM_ALIAS[0]
        .1
        .replace(
            ".use dep (entry as chosen)",
            ".use dep (entry as chosen, unused as sibling)",
        )
        .replace(".word chosen", ".word chosen, sibling");
    let files = &[("main.asm", multiple.as_str()), SELECTED_ITEM_ALIAS[1]];
    let expected = oracle_address_ordered_with_roots(files, &["library"]).unwrap();
    compact_cli(files, &["library"], &[], Some(&expected), false);
}

#[test]
#[ignore = "requires configured FS-UAE; qualified item alias rejects like Rust"]
fn binary_graph_qualified_item_alias_rejection_fs_uae() {
    let qualified = SELECTED_ITEM_ALIAS[0]
        .1
        .replace(
            ".use dep (entry as chosen)",
            ".use dep (entry as chosen) as d",
        )
        .replace(".word chosen", ".word d.chosen");
    native(
        &[("main.asm", &qualified), SELECTED_ITEM_ALIAS[1]],
        "m6502",
        None,
        Some(&["library"]),
    );
}

const SELECTED_MISSING: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (missing)\n.byte $aa\n.endmodule\n.end\n",
    ),
    SELECTED_UNUSED[1],
];

const SELECTED_LOCAL_SHADOW: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (entry)\nentry:\n.byte $aa\n.word entry\n.endmodule\n.end\n",
    ),
    SELECTED_UNUSED[1],
];

const SELECTED_PRIVATE: &[(&str, &str)] = &[
    SELECTED_UNUSED[0],
    (
        "dep.asm",
        ".module dep\n.cpu m6502\n.priv\nentry .block\n.byte $11\n.bend\n.endmodule\n.end\n",
    ),
];

const MULTI_SELECTED_DEP: &str = ".module dep\n.cpu m6502\n.pub\nentry .block\n.byte $11\n.bend\nother .block\n.byte $22\n.bend\nunused .block\n.byte $99\n.bend\n.endmodule\n.end\n";

const MULTI_SELECTED_ONE_REFERENCE: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (entry, other)\n.word other\n.endmodule\n.end\n",
    ),
    ("dep.asm", MULTI_SELECTED_DEP),
];

const MULTI_SELECTED_BOTH_REFERENCES: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (entry, other)\n.word other\n.word entry\n.endmodule\n.end\n",
    ),
    ("dep.asm", MULTI_SELECTED_DEP),
];

const MULTI_SELECTED_ALIASED: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (entry, other) as d\n.word d.other\n.endmodule\n.end\n",
    ),
    ("dep.asm", MULTI_SELECTED_DEP),
];

const MULTI_SELECTED_MISSING: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (entry, missing)\n.byte $aa\n.endmodule\n.end\n",
    ),
    ("dep.asm", MULTI_SELECTED_DEP),
];

const MULTI_SELECTED_REPEAT: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.use dep (entry, other, entry)\n.word entry\n.endmodule\n.end\n",
    ),
    ("dep.asm", MULTI_SELECTED_DEP),
];

fn oracle(files: &[(&str, &str)]) -> Result<Vec<u8>, String> {
    oracle_with_roots(files, &[])
}

pub(super) fn oracle_with_roots(files: &[(&str, &str)], roots: &[&str]) -> Result<Vec<u8>, String> {
    oracle_with_search_roots(files, roots, &[])
}

fn oracle_with_search_roots(
    files: &[(&str, &str)],
    module_roots: &[&str],
    include_roots: &[&str],
) -> Result<Vec<u8>, String> {
    oracle_entries_with_search_roots(files, module_roots, include_roots)
        .map(|entries| entries.into_iter().map(|(_, byte)| byte).collect())
}

fn oracle_address_ordered_with_roots(
    files: &[(&str, &str)],
    roots: &[&str],
) -> Result<Vec<u8>, String> {
    let mut entries = oracle_entries_with_search_roots(files, roots, &[])?;
    entries.sort_unstable_by_key(|(address, _)| *address);
    if entries
        .windows(2)
        .any(|pair| pair[0].0.checked_add(1) != Some(pair[1].0))
    {
        return Err("Rust image is not one contiguous, nonoverlapping binary".into());
    }
    Ok(entries.into_iter().map(|(_, byte)| byte).collect())
}

fn oracle_entries_with_search_roots(
    files: &[(&str, &str)],
    module_roots: &[&str],
    include_roots: &[&str],
) -> Result<Vec<(u32, u8)>, String> {
    let dir = create_temp_dir("binary-module-graph");
    for (name, source) in files {
        let path = dir.join(name);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, source).unwrap();
    }
    let root = dir.join(files[0].0);
    let result = if module_roots.is_empty() && include_roots.is_empty() {
        assemble_example_entries_with_runtime_mode(&root, true)
    } else {
        let module_roots = module_roots
            .iter()
            .map(|root| dir.join(root))
            .collect::<Vec<_>>();
        let include_roots = include_roots
            .iter()
            .map(|root| dir.join(root))
            .collect::<Vec<_>>();
        let root_lines = expand_source_file(&root, &[], &include_roots, 64)
            .map_err(|error| format!("{error:?}"))?;
        let root_module_id =
            root_module_id_from_lines(&root, &root_lines).map_err(|error| format!("{error:?}"))?;
        let graph = load_module_graph(&root, root_lines, &[], &include_roots, &module_roots, 64)
            .map_err(|error| format!("{error:?}"))?;
        let mut assembler = Assembler::new();
        assembler.root_metadata.root_module_id = Some(root_module_id);
        assembler.set_runtime_line_router(Some(make_test_runtime_line_router(
            runtime_enabled_execution_mode(true),
        )));
        assembler.module_macro_names = graph.module_macro_names;
        let pass1 = assembler.pass1(&graph.lines);
        let mut listing_out = Vec::new();
        let mut listing = ListingWriter::new(&mut listing_out, false);
        let pass2 = assembler
            .pass2(&graph.lines, &mut listing)
            .map_err(|error| format!("{error:?}"))?;
        let entries = assembler
            .image()
            .entries()
            .map_err(|error| format!("{error:?}"))?;
        let diagnostics = assembler
            .diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.severity == Severity::Error)
            .map(|diagnostic| diagnostic.error.message().to_owned())
            .collect();
        if pass1.errors > 0 || pass2.errors > 0 {
            Ok((entries, diagnostics))
        } else {
            Ok((entries, Vec::new()))
        }
    }
    .map_err(|error| format!("{error:?}"))
    .and_then(|(entries, diagnostics)| {
        if diagnostics.is_empty() {
            Ok(entries)
        } else {
            Err(format!("{diagnostics:?}"))
        }
    });
    fs::remove_dir_all(dir).unwrap();
    result
}

fn native(files: &[(&str, &str)], cpu: &str, expected: Option<&[u8]>, roots: Option<&[&str]>) {
    native_with_diagnostic(files, cpu, expected, roots, None);
}

fn native_with_diagnostic(
    files: &[(&str, &str)],
    cpu: &str,
    expected: Option<&[u8]>,
    roots: Option<&[&str]>,
    diagnostic: Option<&str>,
) {
    native_with_roots(files, cpu, expected, roots, &[], diagnostic);
}

fn native_with_roots(
    files: &[(&str, &str)],
    cpu: &str,
    expected: Option<&[u8]>,
    roots: Option<&[&str]>,
    include_roots: &[&str],
    diagnostic: Option<&str>,
) {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let sources = files
        .iter()
        .map(|(path, source)| (*path, source.as_bytes()))
        .collect::<Vec<_>>();
    let result = if let Some(roots) = roots {
        crate::fs_uae_smoke::run_binary_discovery_with_includes_from_env(
            &workspace_root(),
            &package,
            &sources,
            expected,
            diagnostic,
            roots,
            include_roots,
        )
    } else {
        crate::fs_uae_smoke::run_binary_graph_from_env(
            &workspace_root(),
            &package,
            &sources,
            expected,
            diagnostic,
        )
    }
    .expect("fresh native graph completion");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(
        runs[0].protocol_completed,
        "exit={:?} stdout={} stderr={}",
        runs[0].exit_code, runs[0].stdout, runs[0].stderr
    );
    if expected.is_some() {
        assert!(runs[0].success);
    }
    assert_eq!(
        runs[0].exit_code,
        Some(if expected.is_some() { 0 } else { 20 })
    );
    let image = runs[0]
        .captured_artifacts
        .get(&PathBuf::from("Work/build/binary_source_harness"))
        .unwrap();
    let allocation = hunk::allocation(image).unwrap();
    eprintln!(
        "BINARY_GRAPH_REPORT seconds={:?} image={:?} image_bytes={} linked_reserved_bytes={}",
        runs[0].start_to_done_host_seconds,
        runs[0].native_image_digest,
        image.len(),
        allocation.total()
    );
}

fn compact_cli(
    files: &[(&str, &str)],
    module_roots: &[&str],
    include_roots: &[&str],
    expected: Option<&[u8]>,
    bare_entry: bool,
) {
    compact_cli_cpu(
        files,
        module_roots,
        include_roots,
        expected,
        bare_entry,
        "m6502",
    );
}

fn compact_cli_cpu(
    files: &[(&str, &str)],
    module_roots: &[&str],
    include_roots: &[&str],
    expected: Option<&[u8]>,
    bare_entry: bool,
    cpu: &str,
) {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let sources = files
        .iter()
        .map(|(path, source)| (*path, source.as_bytes()))
        .collect::<Vec<_>>();
    let result = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        &sources,
        module_roots,
        include_roots,
        expected,
        bare_entry,
    )
    .expect("fresh compact CLI source-set completion");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].success, expected.is_some());
    assert_eq!(
        runs[0].exit_code,
        Some(if expected.is_some() { 0 } else { 20 })
    );
    let image = runs[0]
        .captured_artifacts
        .get(&PathBuf::from("Work/build/opforge_compact"))
        .expect("fresh compact CLI Hunk");
    let allocation = hunk::allocation(image).expect("valid compact CLI Hunk");
    assert!(allocation.total() < 2 * 1024 * 1024);
    eprintln!(
        "COMPACT_CLI_SOURCE_SET seconds={:?} image_bytes={} linked_reserved_bytes={}",
        runs[0].start_to_done_host_seconds,
        image.len(),
        allocation.total()
    );
}

#[test]
fn binary_graph_rust_ordering() {
    assert_eq!(oracle(DIAMOND).unwrap(), [1, 2, 3, 4]);
    assert_eq!(oracle(ENTRY_SIBLINGS).unwrap(), [1, 2]);
    assert_eq!(oracle(UNUSED).unwrap(), [1, 2]);
    assert_eq!(oracle(BLOCK_BOUNDARIES).unwrap(), [0x11, 0x22, 1, 0x10]);
}

const SINGLE_MAPPED_SECTION: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.region rom, $1000, $10ff\n.use dep (entry) as d\n.section code\n.word d.entry\n.endsection\n.place code in rom\n.endmodule\n.end\n",
    ),
    (
        "library/dep.asm",
        ".module dep\n.cpu m6502\n.pub\n.section code, logical\nentry .block\n.byte $11\n.bend\nunused .block\n.byte $99\n.bend\n.endsection\n.endmodule\n.end\n",
    ),
];

#[test]
fn binary_graph_single_mapped_section_rust_oracle() {
    assert_eq!(
        oracle_with_roots(SINGLE_MAPPED_SECTION, &["library"]).unwrap(),
        [0x11, 0x00, 0x10]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; bounded same-name section placement"]
fn compact_cli_single_mapped_section_fs_uae() {
    let expected =
        oracle_with_roots(SINGLE_MAPPED_SECTION, &["library"]).expect("live Rust section oracle");
    compact_cli(
        SINGLE_MAPPED_SECTION,
        &["library"],
        &[],
        Some(&expected),
        false,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; typed logical and concrete mapped sections"]
fn compact_cli_typed_mapped_section_fs_uae() {
    let root = SINGLE_MAPPED_SECTION[0]
        .1
        .replace(".section code\n", ".section code, kind=code\n");
    let imported = SINGLE_MAPPED_SECTION[1].1.replace(
        ".section code, logical",
        ".section code, logical, kind=code",
    );
    let files = [
        ("main.asm", root.as_str()),
        ("library/dep.asm", imported.as_str()),
    ];
    let expected =
        oracle_with_roots(&files, &["library"]).expect("live Rust mapped-section oracle");
    compact_cli(&files, &["library"], &[], Some(&expected), false);
}

const EXPLICIT_MAPPED_SECTION: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.region rom, $1000, $10ff\n.use dep (entry) as d map { code -> app_code }\nwanted = d.entry\n.section app_code\n.endsection\n.place app_code in rom\n.endmodule\n.end\n",
    ),
    SINGLE_MAPPED_SECTION[1],
];

#[test]
fn binary_graph_explicit_mapped_section_rust_oracle() {
    assert_eq!(
        oracle_with_roots(EXPLICIT_MAPPED_SECTION, &["library"]).unwrap(),
        [0x11]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; one explicit imported section map"]
fn compact_cli_explicit_mapped_section_fs_uae() {
    let expected = oracle_with_roots(EXPLICIT_MAPPED_SECTION, &["library"])
        .expect("live Rust explicit-map oracle");
    compact_cli(
        EXPLICIT_MAPPED_SECTION,
        &["library"],
        &[],
        Some(&expected),
        false,
    );
}

const EXPLICIT_MAPPED_BODY: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.region rom, $1000, $10ff\n.use dep (entry) as d map { code -> app_code }\n.section app_code\n.byte $22\n.word d.entry\n.byte $33\n.endsection\n.place app_code in rom\n.endmodule\n.end\n",
    ),
    (
        "library/dep.asm",
        ".module dep\n.cpu m6502\n.pub\n.section code, logical\n.byte $b0\nentry .block\n.byte $11\n.bend\nunused .block\n.byte $99\n.bend\n.byte $b1\n.endsection\n.endmodule\n.end\n",
    ),
];

#[test]
fn binary_graph_explicit_mapped_section_body_rust_oracle() {
    assert_eq!(
        oracle_with_roots(EXPLICIT_MAPPED_BODY, &["library"]).unwrap(),
        [0x22, 0x05, 0x10, 0x33, 0xb0, 0x11, 0xb1]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; concrete body precedes imported mapped content"]
fn compact_cli_explicit_mapped_section_body_fs_uae() {
    let expected = oracle_with_roots(EXPLICIT_MAPPED_BODY, &["library"])
        .expect("live Rust mapped-body oracle");
    compact_cli(
        EXPLICIT_MAPPED_BODY,
        &["library"],
        &[],
        Some(&expected),
        false,
    );
}

const TWO_MAPPED_SECTIONS: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.region rom, $1000, $10ff\n.use dep_a (entry) as left map { code_a -> app_a }\n.use dep_b (entry) as right map { code_b -> app_b }\n.section app_a\n.byte $a0\n.word left.entry\n.endsection\n.section app_b\n.byte $a1\n.word right.entry\n.endsection\n.place app_a in rom\n.place app_b in rom\n.endmodule\n.end\n",
    ),
    (
        "library/dep_a.asm",
        ".module dep_a\n.cpu m6502\n.pub\n.section code_a, logical\n.byte $b0\nentry .block\n.byte $10\n.bend\nunused .block\n.byte $99\n.bend\n.endsection\n.endmodule\n.end\n",
    ),
    (
        "library/dep_b.asm",
        ".module dep_b\n.cpu m6502\n.pub\n.section code_b, logical\n.byte $c0\nentry .block\n.byte $20\n.bend\nunused .block\n.byte $98\n.bend\n.endsection\n.endmodule\n.end\n",
    ),
];

#[test]
fn binary_graph_two_mapped_sections_overlap_rejected() {
    assert!(oracle_with_roots(TWO_MAPPED_SECTIONS, &["library"])
        .unwrap_err()
        .contains("Mapped section overlaps the next placed section"));
}

#[test]
#[ignore = "requires configured FS-UAE; two maps cannot share one region"]
fn compact_cli_two_mapped_sections_overlap_fs_uae() {
    assert!(oracle_with_roots(TWO_MAPPED_SECTIONS, &["library"]).is_err());
    compact_cli(TWO_MAPPED_SECTIONS, &["library"], &[], None, false);
}

const TWO_MAPPED_REGIONS: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.region rom_a, $1000, $1004\n.region rom_b, $1005, $10ff\n.use dep_a (entry) as left map { code_a -> app_a }\n.use dep_b (entry) as right map { code_b -> app_b }\n.section app_a\n.byte $a0\n.word left.entry\n.endsection\n.section app_b\n.byte $a1\n.word right.entry\n.endsection\n.place app_a in rom_a\n.place app_b in rom_b\n.endmodule\n.end\n",
    ),
    TWO_MAPPED_SECTIONS[1],
    TWO_MAPPED_SECTIONS[2],
];

#[test]
fn binary_graph_two_mapped_regions_rust_oracle() {
    assert_eq!(
        oracle_address_ordered_with_roots(TWO_MAPPED_REGIONS, &["library"]).unwrap(),
        [0xa0, 0x04, 0x10, 0xb0, 0x10, 0xa1, 0x09, 0x10, 0xc0, 0x20]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; two imported maps in adjacent regions"]
fn compact_cli_two_mapped_regions_fs_uae() {
    let expected = oracle_address_ordered_with_roots(TWO_MAPPED_REGIONS, &["library"])
        .expect("live Rust two-map oracle");
    compact_cli(
        TWO_MAPPED_REGIONS,
        &["library"],
        &[],
        Some(&expected),
        false,
    );
}

const TWO_CONCRETE_SECTIONS: &[(&str, &str)] = &[(
    "main.asm",
    ".module main\n.cpu m6502\n.region rom_a, $1000, $1002\n.region rom_b, $1003, $10ff\n.section app_a\n.byte $a0\n.word b_entry\n.endsection\n.section app_b\nb_entry:\n.byte $b0, $b1\n.endsection\n.place app_a in rom_a\n.place app_b in rom_b\n.endmodule\n.end\n",
)];

fn typed_concrete_sections() -> String {
    TWO_CONCRETE_SECTIONS[0]
        .1
        .replace(".section app_a", ".section app_a, kind=code")
        .replace(".section app_b", ".section app_b, kind=data")
}

#[test]
fn binary_graph_two_concrete_sections_rust_oracle() {
    assert_eq!(
        oracle(TWO_CONCRETE_SECTIONS).unwrap(),
        [0xa0, 0x03, 0x10, 0xb0, 0xb1]
    );
}

#[test]
fn binary_graph_typed_concrete_sections_rust_oracle() {
    let source = typed_concrete_sections();
    assert_eq!(
        oracle(&[("main.asm", &source)]).unwrap(),
        [0xa0, 0x03, 0x10, 0xb0, 0xb1]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; numeric code/data section kinds"]
fn compact_cli_typed_concrete_sections_fs_uae() {
    let source = typed_concrete_sections();
    let expected = oracle(&[("main.asm", &source)]).expect("live Rust typed-section oracle");
    compact_cli(&[("main.asm", &source)], &[], &[], Some(&expected), false);
}

#[test]
#[ignore = "requires configured FS-UAE; BSS must reject initialized bytes"]
fn compact_cli_bss_section_rejects_initialized_bytes_fs_uae() {
    let source = typed_concrete_sections().replace("kind=data", "kind=bss");
    assert!(oracle(&[("main.asm", &source)]).is_err());
    compact_cli(&[("main.asm", &source)], &[], &[], None, false);
}

#[test]
#[ignore = "requires configured FS-UAE; two adjacent concrete placements"]
fn compact_cli_two_concrete_sections_fs_uae() {
    let expected = oracle(TWO_CONCRETE_SECTIONS).expect("live Rust two-section oracle");
    compact_cli(TWO_CONCRETE_SECTIONS, &[], &[], Some(&expected), false);
}

#[test]
#[ignore = "requires configured FS-UAE; flat writer rejects a gap between sections"]
fn compact_cli_two_concrete_sections_gap_fs_uae() {
    let source = TWO_CONCRETE_SECTIONS[0]
        .1
        .replace(".region rom_b, $1003", ".region rom_b, $1004");
    compact_cli(&[("main.asm", &source)], &[], &[], None, false);
}

#[test]
#[ignore = "requires configured FS-UAE; numeric block markers through graph ordering"]
fn binary_graph_block_boundaries_fs_uae() {
    let expected = oracle(BLOCK_BOUNDARIES).unwrap();
    native(BLOCK_BOUNDARIES, "m6502", Some(&expected), None);
}

#[test]
#[ignore = "requires configured FS-UAE; experimental native block reachability smoke"]
fn binary_graph_block_reachability_fs_uae() {
    native(
        BLOCK_REACHABILITY,
        "m6502",
        Some(&[0x11, 0x22, 0x01, 0x10]),
        None,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; transitive native block reachability smoke"]
fn binary_graph_block_transitive_fs_uae() {
    native(
        BLOCK_TRANSITIVE,
        "m6502",
        Some(&[0x02, 0x10, 0x22, 0x00, 0x10]),
        None,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; native block preceded by referenced label"]
fn binary_graph_block_preceding_label_fs_uae() {
    native(
        BLOCK_PRECEDING_LABEL,
        "m6502",
        Some(&[0x11, 0x00, 0x10]),
        None,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; entry-file blocks remain roots"]
fn binary_graph_block_entry_root_fs_uae() {
    native(
        BLOCK_ENTRY_ROOT,
        "m6502",
        Some(&[0x22, 0x11, 0x00, 0x10]),
        None,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; selected imports are available but not output roots"]
fn binary_graph_selected_import_fs_uae() {
    native(SELECTED_UNUSED, "m6502", Some(&[0xaa]), None);
    native(
        SELECTED_REFERENCED,
        "m6502",
        Some(&[0x11, 0x00, 0x00]),
        None,
    );
    native(SELECTED_ALIASED, "m6502", Some(&[0x11, 0x00, 0x00]), None);
    native(SELECTED_MISSING, "m6502", None, None);
}

#[test]
#[ignore = "requires configured FS-UAE; selected name validation and local precedence"]
fn binary_graph_selected_import_binding_fs_uae() {
    native(
        SELECTED_LOCAL_SHADOW,
        "m6502",
        Some(&[0xaa, 0x00, 0x00]),
        None,
    );
    native(SELECTED_PRIVATE, "m6502", None, None);
}

#[test]
#[ignore = "requires configured FS-UAE; multiple selected names and reference-driven output"]
fn binary_graph_multi_selected_import_fs_uae() {
    native(
        MULTI_SELECTED_ONE_REFERENCE,
        "m6502",
        Some(&[0x22, 0x00, 0x00]),
        None,
    );
    native(
        MULTI_SELECTED_BOTH_REFERENCES,
        "m6502",
        Some(&[0x11, 0x22, 0x01, 0x00, 0x00, 0x00]),
        None,
    );
    native(
        MULTI_SELECTED_ALIASED,
        "m6502",
        Some(&[0x22, 0x00, 0x00]),
        None,
    );
    native(MULTI_SELECTED_MISSING, "m6502", None, None);
}

#[test]
#[ignore = "requires configured FS-UAE; repeated names share one numeric selection"]
fn binary_graph_multi_selected_repeat_fs_uae() {
    native(
        MULTI_SELECTED_REPEAT,
        "m6502",
        Some(&[0x11, 0x00, 0x00]),
        None,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; bounded numeric graph parity"]
fn binary_graph_diamond_fs_uae() {
    let expected = oracle(DIAMOND).unwrap();
    assert_eq!(expected, [1, 2, 3, 4]);
    native(DIAMOND, "m6502", Some(&expected), None);
}

#[test]
#[ignore = "requires configured FS-UAE; bounded entry-file module ordering"]
fn binary_graph_entry_siblings_fs_uae() {
    let expected = oracle(ENTRY_SIBLINGS).unwrap();
    assert_eq!(expected, [1, 2]);
    native(ENTRY_SIBLINGS, "m6502", Some(&expected), None);
}

#[test]
#[ignore = "requires configured FS-UAE; unused sibling does not join graph"]
fn binary_graph_unused_fs_uae() {
    let expected = oracle(UNUSED).unwrap();
    native(UNUSED, "m6502", Some(&expected), None);
}

const CYCLE: &[(&str, &str)] = &[
    ("main.asm", ".module main\n.use helper\n.endmodule\n.end\n"),
    (
        "helper.asm",
        ".module helper\n.use main\n.endmodule\n.end\n",
    ),
];
const SELF: &[(&str, &str)] = &[("main.asm", ".module main\n.use main\n.endmodule\n.end\n")];
const MISSING: &[(&str, &str)] = &[("main.asm", ".module main\n.use absent\n.endmodule\n.end\n")];

#[test]
fn binary_graph_rust_rejections() {
    for files in [CYCLE, SELF, MISSING] {
        assert!(oracle(files).is_err());
    }
}

#[test]
#[ignore = "requires configured FS-UAE; fresh negative proofs"]
fn binary_graph_rejections_fs_uae() {
    for files in [CYCLE, SELF, MISSING] {
        eprintln!("BINARY_GRAPH_NEGATIVE {}", files[0].1);
        assert!(oracle(files).is_err());
        native(files, "m6502", None, None);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; native directory discovery"]
fn binary_discovery_diamond_fs_uae() {
    let expected = oracle(DIAMOND).unwrap();
    native(DIAMOND, "m6502", Some(&expected), Some(&[]));
}

const SEARCH_ROOTS: &[(&str, &str)] = &[
    ("entry/main.asm", ROOT),
    ("library/beta.asm", B),
    ("library/nested/shared.inc", SHARED),
    ("library/alpha.asm", A),
];

#[test]
#[ignore = "requires configured FS-UAE; entry directory plus configured recursive root"]
fn binary_discovery_search_roots_fs_uae() {
    let expected = oracle_with_roots(SEARCH_ROOTS, &["library", "library/nested"]).unwrap();
    native(
        SEARCH_ROOTS,
        "m6502",
        Some(&expected),
        Some(&["library", "library/nested"]),
    );
}

#[test]
#[ignore = "requires configured FS-UAE; compact CLI search roots and graph order"]
fn compact_cli_search_roots_fs_uae() {
    let roots = &["library", "library/nested"];
    let expected = oracle_with_roots(SEARCH_ROOTS, roots).unwrap();
    assert_eq!(expected, [1, 2, 3, 4]);
    compact_cli(SEARCH_ROOTS, roots, &[], Some(&expected), false);
}

const SELECTIVE_CANDIDATES: &[(&str, &str)] = &[
    (
        "entry/main.asm",
        ".module main\n.cpu m6502\n.use chosen\n.byte 2\n.endmodule\n.end\n",
    ),
    (
        "library/chosen.asm",
        ".module chosen\n.cpu m6502\n.org $1000\n.byte 1\n.endmodule\n.end\n",
    ),
    (
        "library/unused.asm",
        ".module unused\n.invalid_directive\n.endmodule\n",
    ),
    ("library/duplicate-a.asm", ".module duplicate\n.endmodule\n"),
    ("library/duplicate-b.asm", ".module duplicate\n.endmodule\n"),
    ("library/fragment.inc", ".include \"missing.inc\"\n"),
];

#[test]
fn binary_discovery_rust_selective() {
    assert_eq!(
        oracle_with_roots(SELECTIVE_CANDIDATES, &["library"]).unwrap(),
        [1, 2]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; only the dependency closure is lowered"]
fn binary_discovery_selective_fs_uae() {
    let expected = oracle_with_roots(SELECTIVE_CANDIDATES, &["library"]).unwrap();
    assert_eq!(expected, [1, 2]);
    native(
        SELECTIVE_CANDIDATES,
        "m6502",
        Some(&expected),
        Some(&["library"]),
    );
}

const SELECTED_SIBLING: &[(&str, &str)] = &[
    (
        "entry/main.asm",
        ".module main\n.cpu m6502\n.use chosen\n.byte 2\n.endmodule\n.end\n",
    ),
    (
        "library/mixed.asm",
        ".module unused\nbroken_instruction\n.invalid_directive\n.endmodule\n.module chosen\n.cpu m6502\n.org $1000\n.byte 1\n.endmodule\n.end\n",
    ),
];

const TWO_SELECTED_FROM_FILE: &[(&str, &str)] = &[
    (
        "entry/main.asm",
        ".module main\n.cpu m6502\n.use alpha\n.use beta\n.byte 3\n.endmodule\n.end\n",
    ),
    (
        "library/mixed.asm",
        ".module ALPHA\n.cpu m6502\n.org $1000\n.byte 1\n.endmodule\n.module unused\nbroken_instruction\n.invalid_directive\n.endmodule\n.module beta\n.cpu m6502\n.byte 2\n.endmodule\n.end\n",
    ),
];

#[test]
fn binary_discovery_selected_modules_rust() {
    assert_eq!(
        oracle_with_roots(SELECTED_SIBLING, &["library"]).unwrap(),
        [1, 2]
    );
    assert_eq!(
        oracle_with_roots(TWO_SELECTED_FROM_FILE, &["library"]).unwrap(),
        [1, 2, 3]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; selected module bodies in one candidate file"]
fn binary_discovery_selected_modules_fs_uae() {
    for files in [SELECTED_SIBLING, TWO_SELECTED_FROM_FILE] {
        let expected = oracle_with_roots(files, &["library"]).unwrap();
        native(files, "m6502", Some(&expected), Some(&["library"]));
    }
}

const SELECTED_FILE_MISSING_INCLUDE: &[(&str, &str)] = &[
    (
        "entry/main.asm",
        ".module main\n.cpu m6502\n.use chosen\n.byte 2\n.endmodule\n.end\n",
    ),
    (
        "library/mixed.asm",
        ".module unused\n.include \"missing.inc\"\n.endmodule\n.module chosen\n.cpu m6502\n.org $1000\n.byte 1\n.endmodule\n.end\n",
    ),
];
const SELECTED_FILE_DUPLICATE_MODULE: &[(&str, &str)] = &[
    (
        "entry/main.asm",
        ".module main\n.cpu m6502\n.use chosen\n.byte 2\n.endmodule\n.end\n",
    ),
    (
        "library/mixed.asm",
        ".module chosen\n.cpu m6502\n.org $1000\n.byte 1\n.endmodule\n.module CHOSEN\n.byte 3\n.endmodule\n.end\n",
    ),
];

#[test]
fn binary_discovery_selected_file_rejections_rust() {
    for files in [
        SELECTED_FILE_MISSING_INCLUDE,
        SELECTED_FILE_DUPLICATE_MODULE,
    ] {
        assert!(oracle_with_roots(files, &["library"]).is_err());
    }
}

#[test]
#[ignore = "requires configured FS-UAE; selected-file include and duplicate rejection"]
fn binary_discovery_selected_file_rejections_fs_uae() {
    for files in [
        SELECTED_FILE_MISSING_INCLUDE,
        SELECTED_FILE_DUPLICATE_MODULE,
    ] {
        native(files, "m6502", None, Some(&["library"]));
    }
}

const INCLUDED_GRAPH: &[(&str, &str)] = &[
    (
        "entry/main.asm",
        ".module main\n.cpu m6502\n.use chosen\n.byte 3\n.endmodule\n.end\n",
    ),
    (
        "library/chosen.asm",
        ".module chosen\n.cpu m6502\n.include \"detail/outer.inc\"\n.endmodule\n.end\n",
    ),
    (
        "library/detail/outer.inc",
        ".include \"inner.inc\"\n.byte 2\n",
    ),
    (
        "library/detail/inner.inc",
        ".use helper\nmark\n.byte helper.value - $1000\n",
    ),
    (
        "library/helper.asm",
        ".module helper\n.cpu m6502\n.org $1000\n.pub\nvalue\n.byte 1\n.endmodule\n.end\n",
    ),
    (
        "library/unused.asm",
        ".module unused\n.include \"missing.inc\"\n.endmodule\n",
    ),
];

#[test]
fn binary_discovery_nested_include_rust() {
    assert_eq!(
        oracle_with_roots(INCLUDED_GRAPH, &["library"]).unwrap(),
        [1, 0, 2, 3]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; selected nested includes and numeric graph"]
fn binary_discovery_nested_include_fs_uae() {
    let expected = oracle_with_roots(INCLUDED_GRAPH, &["library"]).unwrap();
    native(INCLUDED_GRAPH, "m6502", Some(&expected), Some(&["library"]));
}

const INLINED_GRAPH: &[(&str, &str)] = &[
    INCLUDED_GRAPH[0],
    (
        "library/chosen.asm",
        ".module chosen\n.cpu m6502\n.use helper\nmark\n.byte helper.value - $1000\n.byte 2\n.endmodule\n.end\n",
    ),
    INCLUDED_GRAPH[2],
    INCLUDED_GRAPH[3],
    INCLUDED_GRAPH[4],
    INCLUDED_GRAPH[5],
];

#[test]
#[ignore = "requires configured FS-UAE; bounded include versus manual inlining"]
fn binary_discovery_include_inline_comparison_fs_uae() {
    let expected = oracle_with_roots(INCLUDED_GRAPH, &["library"]).unwrap();
    assert_eq!(
        oracle_with_roots(INLINED_GRAPH, &["library"]).unwrap(),
        expected
    );
    eprintln!("BINARY_INCLUDE_COMPARISON mode=include");
    native(INCLUDED_GRAPH, "m6502", Some(&expected), Some(&["library"]));
    eprintln!("BINARY_INCLUDE_COMPARISON mode=inline");
    native(INLINED_GRAPH, "m6502", Some(&expected), Some(&["library"]));
}

const INCLUDED_FROM_ROOT: &[(&str, &str)] = &[
    (
        "entry/main.asm",
        ".module main\n.cpu m6502\n.use chosen\n.byte 2\n.endmodule\n.end\n",
    ),
    (
        "library/chosen.asm",
        ".module chosen\n.cpu m6502\n.include \"defs.inc\"\n.endmodule\n.end\n",
    ),
    ("common/defs.inc", ".org $1000\n.byte 1\n"),
];

#[test]
fn binary_discovery_include_root_rust() {
    assert_eq!(
        oracle_with_search_roots(INCLUDED_FROM_ROOT, &["library"], &["common"]).unwrap(),
        [1, 2]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; selected include search root"]
fn binary_discovery_include_root_fs_uae() {
    let expected = oracle_with_search_roots(INCLUDED_FROM_ROOT, &["library"], &["common"]).unwrap();
    native_with_roots(
        INCLUDED_FROM_ROOT,
        "m6502",
        Some(&expected),
        Some(&["library"]),
        &["common"],
        None,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; compact CLI selected include search"]
fn compact_cli_include_root_fs_uae() {
    let expected = oracle_with_search_roots(INCLUDED_FROM_ROOT, &["library"], &["common"]).unwrap();
    assert_eq!(expected, [1, 2]);
    compact_cli(
        INCLUDED_FROM_ROOT,
        &["library"],
        &["common"],
        Some(&expected),
        false,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; bare entry anchors current-directory discovery"]
fn compact_cli_bare_entry_fs_uae() {
    let files = &[
        ("main.asm", ROOT),
        ("library/alpha.asm", A),
        ("library/beta.asm", B),
        ("library/shared.asm", SHARED),
    ];
    let expected = oracle_with_search_roots(files, &[], &["library"]).unwrap();
    compact_cli(files, &[], &["library"], Some(&expected), true);
}

const INCLUDE_ASSEMBLY_FAILURE: &[(&str, &str)] = &[
    (
        "entry/main.asm",
        ".module main\n.cpu m6502\n.use chosen\n.byte 2\n.endmodule\n.end\n",
    ),
    (
        "library/chosen.asm",
        ".module chosen\n.cpu m6502\n.include \"part.inc\"\n.endmodule\n.end\n",
    ),
    ("library/part.inc", ".org $1000\n.long $+2147483647\n"),
];

#[test]
#[ignore = "requires configured FS-UAE; included record origin after graph ordering"]
fn binary_discovery_include_origin_fs_uae() {
    native_with_diagnostic(
        INCLUDE_ASSEMBLY_FAILURE,
        "m6502",
        None,
        Some(&["library"]),
        Some("[file 00000004, line 00000002]"),
    );
}

const INCLUDE_MISSING: &[(&str, &str)] = &[(
    "entry/main.asm",
    ".module main\n.cpu m6502\n.include \"missing.inc\"\n.endmodule\n.end\n",
)];
const INCLUDE_CYCLE: &[(&str, &str)] = &[
    (
        "entry/main.asm",
        ".module main\n.cpu m6502\n.include \"a.inc\"\n.endmodule\n.end\n",
    ),
    ("entry/a.inc", ".include \"b.inc\"\n"),
    ("entry/b.inc", ".include \"a.inc\"\n"),
];

#[test]
fn binary_discovery_include_rejections_rust() {
    assert!(oracle(INCLUDE_MISSING).is_err());
    assert!(oracle(INCLUDE_CYCLE).is_err());
}

#[test]
#[ignore = "requires configured FS-UAE; bounded missing and cyclic include failures"]
fn binary_discovery_include_rejections_fs_uae() {
    for files in [INCLUDE_MISSING, INCLUDE_CYCLE] {
        native(files, "m6502", None, Some(&[]));
    }
}

const ORDERED_DIAMOND: &[(&str, &str)] = &[
    ("shared.asm", SHARED),
    ("alpha.asm", A),
    ("beta.asm", B),
    ("main.asm", ROOT),
];

#[test]
#[ignore = "requires configured FS-UAE; bounded F7/F8 same-source comparison"]
fn binary_graph_cost_fs_uae() {
    let expected = oracle(DIAMOND).unwrap();
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let run = |files: &[(&str, &str)], graph: bool| {
        let sources = files
            .iter()
            .map(|(path, text)| (*path, text.as_bytes()))
            .collect::<Vec<_>>();
        let started = std::time::Instant::now();
        let result = if graph {
            crate::fs_uae_smoke::run_binary_graph_from_env(
                &workspace_root(),
                &package,
                &sources,
                Some(&expected),
                None,
            )
        } else {
            crate::fs_uae_smoke::run_binary_source_harness_from_env(
                &workspace_root(),
                &package,
                &sources,
                &expected,
            )
        }
        .expect("fresh native comparison");
        let elapsed = started.elapsed().as_secs_f64();
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("native execution required")
        };
        assert_eq!(runs.len(), 1);
        let run = &runs[0];
        assert!(run.success && run.protocol_completed);
        assert_eq!(run.exit_code, Some(0));
        let image = run
            .captured_artifacts
            .get(&PathBuf::from("Work/build/binary_source_harness"))
            .unwrap();
        let allocation = hunk::allocation(image).unwrap();
        let memory = run
            .captured_artifacts
            .get(&PathBuf::from("Work/memory.bin"))
            .map(|bytes| {
                let words = bytes
                    .chunks_exact(4)
                    .map(|chunk| u32::from_be_bytes(chunk.try_into().unwrap()))
                    .collect::<Vec<_>>();
                assert_eq!(words[0], 0x4d454d35);
                assert_eq!(words[1], 0);
            assert_eq!(words[3], words[4]);
            assert_eq!(words[11], 0);
            let stamp = |offset: usize| -> u64 {
                u64::from(words[offset]) * 24 * 60 * 60 * 50
                    + u64::from(words[offset + 1]) * 60 * 50
                    + u64::from(words[offset + 2])
            };
            let preparation_seconds = stamp(22).checked_sub(stamp(19)).unwrap() as f64 / 50.0;
            let assembly_seconds = stamp(25).checked_sub(stamp(22)).unwrap() as f64 / 50.0;
            let stage_ticks: u64 = (0..6)
                .map(|index| {
                    (u64::from(words[30 + index * 2]) << 32)
                        | u64::from(words[31 + index * 2])
                })
                .sum();
            serde_json::json!({
                "peak_owned_bytes": words[2], "retained_after_preparation_bytes": words[5],
                "runtime_prefix_bytes": words[13], "packed_source_bytes": words[14],
                "source_bytes": words[15], "instrumented_preparation_seconds": preparation_seconds,
                "instrumented_assembly_seconds": assembly_seconds,
                "preparation_stage_seconds": stage_ticks as f64 / f64::from(words[28]),
            })
            });
        serde_json::json!({
            "mode": if graph { "numeric_graph" } else { "explicit_order" },
            "native_start_to_done_seconds": run.start_to_done_host_seconds,
            "invocation_seconds": elapsed,
            "image_bytes": image.len(), "linked_reserved_bytes": allocation.total(),
            "memory": memory,
        })
    };
    let ordered = run(ORDERED_DIAMOND, false);
    let graph = run(DIAMOND, true);
    eprintln!("BINARY_GRAPH_COST ordered={ordered} graph={graph}");
}

const DISCOVERY_MISSING: &[(&str, &str)] = &[(
    "entry/main.asm",
    ".module main\n.use absent\n.endmodule\n.end\n",
)];
const DISCOVERY_AMBIGUOUS: &[(&str, &str)] = &[
    (
        "entry/main.asm",
        ".module main\n.use duplicate\n.endmodule\n.end\n",
    ),
    ("first/one.asm", ".module duplicate\n.endmodule\n.end\n"),
    ("second/two.inc", ".module DUPLICATE\n.endmodule\n.end\n"),
];

#[test]
fn binary_discovery_rust_rejections() {
    assert!(oracle(DISCOVERY_MISSING).is_err());
    assert!(oracle_with_roots(DISCOVERY_AMBIGUOUS, &["first", "second"]).is_err());
}

#[test]
#[ignore = "requires configured FS-UAE; native search failure proofs"]
fn binary_discovery_rejections_fs_uae() {
    native(DISCOVERY_MISSING, "m6502", None, Some(&[]));
    native(
        DISCOVERY_AMBIGUOUS,
        "m6502",
        None,
        Some(&["first", "second"]),
    );
}
