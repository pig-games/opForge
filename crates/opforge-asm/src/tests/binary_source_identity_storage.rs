//! Owned preparation tables must preserve identities across growth and imports.
use super::*;

fn constants() -> String {
    (0..600)
        .map(|index| format!("I{index:03} = {index}\n"))
        .collect()
}

fn sources(imported: bool) -> Vec<(&'static str, String)> {
    let body = format!(
        "{}.if I599 - 598\n.byte I000,I511 & 255,I599 & 255\n.else\n.byte 99\n.endif\n.word I520\n",
        constants()
    );
    if imported {
        vec![
            ("main.asm", ".module main\n.cpu m68020\n.use dep (*)\n.word entry\n.endmodule\n.end\n".into()),
            ("library/dep.asm", format!(".module dep\n.cpu m68020\n.pub\n{body}entry .block\n.byte 7\n.bend\nunused .block\n.byte 99\n.bend\n.endmodule\n.end\n")),
        ]
    } else {
        vec![(
            "main.asm",
            format!(".module root.a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t\n.cpu m68020\n{body}.endmodule\n.end\n"),
        )]
    }
}

fn oracle(files: &[(&str, String)], imported: bool) -> Vec<u8> {
    let files = files
        .iter()
        .map(|(path, source)| (*path, source.as_str()))
        .collect::<Vec<_>>();
    graph::oracle_with_roots(&files, if imported { &["library"] } else { &[] }).unwrap()
}

#[test]
fn compact_identity_storage_rust_oracles() {
    assert_eq!(oracle(&sources(false), false), [0, 255, 87, 2, 8]);
    assert_eq!(oracle(&sources(true), true), [0, 255, 87, 2, 8, 7, 0, 5]);
}

fn native(imported: bool) {
    let files = sources(imported);
    let expected = oracle(&files, imported);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let inputs = files
        .iter()
        .map(|(path, source)| (*path, source.as_bytes()))
        .collect::<Vec<_>>();
    let outcome = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        &inputs,
        if imported { &["library"] } else { &[] },
        &[],
        Some(&expected),
        false,
    )
    .expect("fresh growable identity storage comparison");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.success && run.protocol_completed);
    assert_eq!(run.exit_code, Some(0));
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let record = &run.captured_artifacts[&PathBuf::from("Work/memory.bin")];
        assert_eq!(record.len(), 1756);
        let words = record
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(words[0], 0x4d454d35);
        assert_eq!(words[1], 0, "all owned blocks released");
        assert_eq!(words[3], words[4], "allocation/free accounting balances");
        assert_eq!(words[11], 0);
        assert_eq!(words[29], 0);
        eprintln!(
            "COMPACT_IDENTITY_MEMORY imported={imported} peak_owned_bytes={}",
            words[2]
        );
    }
    let image = &run.captured_artifacts[&PathBuf::from("Work/build/opforge_compact")];
    eprintln!("COMPACT_IDENTITY_STORAGE imported={imported} source_bytes={} output_bytes={} seconds={:?} image_bytes={} linked_reserved_bytes={} package_bytes={}", files.iter().map(|(_, source)| source.len()).sum::<usize>(), expected.len(), run.start_to_done_host_seconds, image.len(), hunk::allocation(image).unwrap().total(), package.len());
}

#[test]
#[ignore = "requires configured FS-UAE; more than 512 identities and conditional constants"]
fn compact_identity_storage_local_fs_uae() {
    native(false);
}

#[test]
#[ignore = "requires configured FS-UAE; per-ID import metadata and live block selection after growth"]
fn compact_identity_storage_imported_fs_uae() {
    native(true);
}

// The graph still keys its bounded nodes by binding ID. A late module must
// reject explicitly rather than treating newly growable identities as nodes.
#[test]
#[ignore = "requires configured FS-UAE; independent graph bound after identity growth"]
fn compact_identity_storage_graph_bound_fs_uae() {
    let main = format!(
        ".module main\n.cpu m68020\n{}.use dep (*)\n.word entry\n.endmodule\n.end\n",
        constants()
    );
    let dep = ".module dep\n.cpu m68020\n.pub\nentry .block\n.byte 7\n.bend\n.endmodule\n.end\n"
        .to_owned();
    let files = [("main.asm", main), ("library/dep.asm", dep)];
    assert_eq!(oracle(&files, true), [7, 0, 0]);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let inputs = files
        .iter()
        .map(|(path, source)| (*path, source.as_bytes()))
        .collect::<Vec<_>>();
    let outcome = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        &inputs,
        &["library"],
        &[],
        None,
        false,
    )
    .expect("fresh explicit graph-bound rejection");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let words = runs[0].captured_artifacts[&PathBuf::from("Work/memory.bin")]
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(words[0], 0x4d454d35);
        assert_eq!(words[1], 0);
        assert_eq!(words[3], words[4]);
        assert_eq!(words[11], 0);
        assert_eq!(words[29], 16);
        eprintln!("COMPACT_IDENTITY_GRAPH_BOUND peak_owned_bytes={}", words[2]);
    }
}
