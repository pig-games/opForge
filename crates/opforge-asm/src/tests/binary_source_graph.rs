//! Numeric native dependency ordering; real files supply a live Rust graph oracle.
use super::*;

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

fn oracle(files: &[(&str, &str)]) -> Result<Vec<u8>, String> {
    oracle_with_roots(files, &[])
}

fn oracle_with_roots(files: &[(&str, &str)], roots: &[&str]) -> Result<Vec<u8>, String> {
    oracle_with_search_roots(files, roots, &[])
}

fn oracle_with_search_roots(
    files: &[(&str, &str)],
    module_roots: &[&str],
    include_roots: &[&str],
) -> Result<Vec<u8>, String> {
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
            Ok(entries.into_iter().map(|(_, byte)| byte).collect())
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

#[test]
fn binary_graph_rust_ordering() {
    assert_eq!(oracle(DIAMOND).unwrap(), [1, 2, 3, 4]);
    assert_eq!(oracle(ENTRY_SIBLINGS).unwrap(), [1, 2]);
    assert_eq!(oracle(UNUSED).unwrap(), [1, 2]);
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
