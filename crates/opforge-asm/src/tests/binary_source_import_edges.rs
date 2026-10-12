//! Import-edge capacity is independent of module and named-block capacity.
use super::*;
use std::fmt::Write as _;

fn sources(importers: usize) -> Vec<(String, String)> {
    let mut main = String::from(".module main\n.cpu m6502\n.org $1000\n");
    let mut files = Vec::new();
    for index in 0..importers {
        writeln!(main, ".use importer{index}").unwrap();
        files.push((
            format!("library/importer{index}.asm"),
            format!(".module importer{index}\n.cpu m6502\n.use left\n.use right\n.pub\nvalue = left.value + right.value\n.endmodule\n"),
        ));
    }
    for index in 0..importers {
        writeln!(main, ".byte importer{index}.value").unwrap();
    }
    main.push_str(".endmodule\n.end\n");
    files.insert(0, ("main.asm".into(), main));
    for (name, value) in [("left", 17), ("right", 34)] {
        files.push((
            format!("library/{name}.asm"),
            format!(".module {name}\n.cpu m6502\n.pub\nvalue = {value}\n.endmodule\n"),
        ));
    }
    files
}

#[test]
fn import_edges_above_512_rust_oracle() {
    for importers in [170, 172] {
        let sources = sources(importers);
        let files = sources
            .iter()
            .map(|(path, source)| (path.as_str(), source.as_str()))
            .collect::<Vec<_>>();
        let mut entries = oracle_entries_with_search_roots(&files, &["library"], &[]).unwrap();
        entries.sort_unstable_by_key(|(address, _)| *address);
        assert_eq!(
            entries,
            (0x1000..).zip(vec![17 + 34; importers]).collect::<Vec<_>>()
        );
    }
}

#[test]
#[ignore = "requires fresh native import-edge capacity proof; no named blocks, under 512 modules"]
fn compact_cli_import_edge_capacity_fs_uae() {
    let importers = std::env::var("OPFORGE_IMPORT_EDGE_IMPORTERS")
        .map(|value| value.parse::<usize>().expect("importer count"))
        .unwrap_or(172);
    assert!(matches!(importers, 170 | 172));
    let sources = sources(importers);
    let files = sources
        .iter()
        .map(|(path, source)| (path.as_str(), source.as_str()))
        .collect::<Vec<_>>();
    let expected = oracle_address_ordered_with_roots(&files, &["library"]).unwrap();
    assert_eq!(expected, vec![51; importers]);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let source_bytes = files
        .iter()
        .map(|(path, source)| (*path, source.as_bytes()))
        .collect::<Vec<_>>();
    // Explicit frozen native roots allow before/after owner comparison without
    // changing the source-under-test, oracle, or package contract.
    let native_root = std::env::var_os("OPFORGE_IMPORT_EDGE_NATIVE_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(workspace_root);
    let result = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &native_root,
        &package,
        &source_bytes,
        &["library"],
        &[],
        Some(&expected),
        false,
    )
    .expect("fresh import-edge native proof");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert!(runs[0].success);
    assert_eq!(runs[0].exit_code, Some(0));
    eprintln!(
        "IMPORT_EDGE_CAPACITY modules={} edges={} bytes={} fresh_success=true",
        importers + 3,
        importers * 3,
        expected.len()
    );
}
