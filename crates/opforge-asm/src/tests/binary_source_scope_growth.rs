//! Closing a live block must preserve its metadata across identity-table growth.
use super::*;

const COUNTS: [usize; 3] = [520, 1032, 2056];

fn many_blocks_source(count: usize) -> String {
    let mut source = String::from(".module app\n.cpu m68020\n");
    for index in 0..count {
        source.push_str(&format!("b{index} .block\n.byte 1\n.bend\n"));
    }
    source.push_str(".byte 7\n.endmodule\n.end\n");
    source
}

#[test]
fn compact_block_index_capacity_rust_oracle() {
    let output = oracle(&many_blocks_source(513));
    assert_eq!(output.len(), 514);
    assert!(output[..513].iter().all(|byte| *byte == 1));
    assert_eq!(output[513], 7);
}

#[test]
#[ignore = "requires configured FS-UAE; 513 live blocks exceed the former index cap"]
fn compact_block_index_capacity_fs_uae() {
    let source = many_blocks_source(513);
    let expected = oracle(&source);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let outcome = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        &[("main.asm", source.as_bytes())],
        &[],
        &[],
        Some(&expected),
        false,
    )
    .expect("fresh 513-block comparison");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
}

fn many_imported_blocks(count: usize) -> Vec<(&'static str, String)> {
    let last = count - 1;
    let caller =
        format!(".module app\n.cpu m68020\n.use dep (b{last})\n.word b{last}\n.endmodule\n.end\n");
    let mut provider = String::from(".module dep\n.cpu m68020\n.pub\n");
    for index in 0..count {
        provider.push_str(&format!("b{index} .block\n.byte 1\n.bend\n"));
    }
    provider.push_str(".endmodule\n.end\n");
    vec![("main.asm", caller), ("library/dep.asm", provider)]
}

#[test]
fn compact_imported_block_index_capacity_rust_oracle() {
    assert_eq!(wide_import_oracle(&many_imported_blocks(513)), [1, 0, 0]);
}

#[test]
#[ignore = "requires configured FS-UAE; selection from 513 imported blocks"]
fn compact_imported_block_index_capacity_fs_uae() {
    native_import(many_imported_blocks(513));
}

#[test]
#[ignore = "requires configured FS-UAE; instrumented 128/513 imported-block scaling"]
fn compact_imported_block_index_scaling_fs_uae() {
    for count in [128, 513] {
        eprintln!("COMPACT_BLOCK_SCALE count={count}");
        native_import(many_imported_blocks(count));
    }
}

fn source(count: usize) -> String {
    let mut source = String::from(".module app\n.cpu m68020\nentry .block\n");
    for index in 0..count {
        source.push_str(&format!("v{index} = {index}\n"));
    }
    source.push_str(&format!(
        ".word v0,v{},v{}\n.bend\n.long entry\n.endmodule\n.end\n",
        count / 2,
        count - 1
    ));
    // Even fully scoped native spellings (app.entry.vN) fit the arena.
    let spelling_bytes: usize = (0..count)
        .map(|index| format!("app.entry.v{index}").len())
        .sum();
    assert!(spelling_bytes + 1024 < 65535);
    source
}

fn oracle(source: &str) -> Vec<u8> {
    graph::oracle_with_roots(&[("main.asm", source)], &[]).unwrap()
}

#[test]
fn compact_scope_growth_rust_oracles() {
    for count in COUNTS {
        let expected: Vec<u8> = [0u16, (count / 2) as u16, (count - 1) as u16]
            .into_iter()
            .flat_map(u16::to_be_bytes)
            .chain([0, 0, 0, 0])
            .collect();
        assert_eq!(oracle(&source(count)), expected, "count={count}");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; live block closes after 512/1024/2048 identities"]
fn compact_scope_growth_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    for count in COUNTS {
        let source = source(count);
        let expected = oracle(&source);
        let outcome = crate::fs_uae_smoke::run_compact_cli_files_from_env(
            &workspace_root(),
            &package,
            &[("main.asm", source.as_bytes())],
            &[],
            &[],
            Some(&expected),
            false,
        )
        .expect("fresh scope metadata growth comparison");
        let FsUaeSmokeOutcome::Completed { runs } = outcome else {
            panic!("real native execution required; count={count}");
        };
        assert_eq!(runs.len(), 1, "count={count}");
        assert!(
            runs[0].success && runs[0].protocol_completed,
            "count={count}"
        );
        assert_eq!(runs[0].exit_code, Some(0), "count={count}");
    }
}

fn wide_import_sources(count: usize) -> Vec<(&'static str, String)> {
    let filler: String = (0..count)
        .map(|index| format!("Fill{index:03}_{} = {index}\n", "x".repeat(200)))
        .collect();
    if count == 320 {
        assert!(filler.len() > 65535);
    }
    vec![
        ("main.asm", ".module app\n.cpu m68020\n.use dep (*)\n.word entry\n.endmodule\n.end\n".into()),
        ("library/dep.asm", format!(".module dep\n.cpu m68020\n.pub\n{filler}entry .block\n.byte 7\n.bend\nunused .block\n.byte 99\n.bend\n.endmodule\n.end\n")),
    ]
}

fn wide_import_oracle(files: &[(&str, String)]) -> Vec<u8> {
    let files: Vec<_> = files
        .iter()
        .map(|(path, source)| (*path, source.as_str()))
        .collect();
    graph::oracle_with_roots(
        &files,
        if files.iter().any(|(path, _)| path.starts_with("library/")) {
            &["library"]
        } else {
            &[]
        },
    )
    .unwrap()
}

#[test]
fn compact_scope_wide_import_rust_oracle() {
    assert_eq!(wide_import_oracle(&wide_import_sources(320)), [7, 0, 0]);
}

#[test]
#[ignore = "requires configured FS-UAE; wildcard imports and retained block beyond 64 KiB"]
fn compact_scope_wide_import_fs_uae() {
    native_import(wide_import_sources(320));
}

#[test]
#[ignore = "known native readiness gap: multiple explicit modules with provider first"]
fn compact_scope_provider_first_readiness_fs_uae() {
    let provider = ".module dep\n.cpu m68020\n.pub\nShape .struct\nfield .long ?\n.endstruct\npayload = Shape.field+7\nemit .macro x\n.word .x\n.endmacro\nroutine .block\n.word payload\n.bend\n.endmodule\n";
    let caller = ".module app\n.cpu m68020\n.use dep as lib\n.lib.emit lib.payload\n.long lib.routine\n.endmodule\n.end\n";
    native_import(vec![("main.asm", format!("{provider}{caller}"))]);
}

fn native_import(files: Vec<(&str, String)>) {
    let expected = wide_import_oracle(&files);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let inputs: Vec<_> = files
        .iter()
        .map(|(path, source)| (*path, source.as_bytes()))
        .collect();
    let outcome = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        &inputs,
        if files.iter().any(|(path, _)| path.starts_with("library/")) {
            &["library"]
        } else {
            &[]
        },
        &[],
        Some(&expected),
        false,
    )
    .expect("fresh wide preparation import comparison");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.success && run.protocol_completed);
    assert_eq!(run.exit_code, Some(0));
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let words: Vec<_> = run.captured_artifacts[&PathBuf::from("Work/memory.bin")]
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect();
        assert_eq!(words[0], 0x4d454d38);
        assert_eq!(words[1], 0);
        assert_eq!(words[3], words[4]);
        assert_eq!(words[11], 0);
        assert_eq!(words[29], 0);
        let finalization_ticks = (u64::from(words[40]) << 32) | u64::from(words[41]);
        eprintln!(
            "COMPACT_IMPORT_MEMORY peak_owned_bytes={} finalization_seconds={:.3}",
            words[2],
            finalization_ticks as f64 / f64::from(words[28])
        );
    }
}
