//! Package-defined tuple projections and semantic sequences over packed operands.
use super::*;

const BODY: &str = r#"tst.b 0(a0,d3.w)
move.b d0,0(a0,d1.l)
cmpi.b #13,-1(a1,d0.l)
move.w 0(a0,d2.w),d3
move.l 0(a1,d2.l),d4
tst.w 0(a1,a2.w)
lea 0(a0,d2.w),a3
cmp.l 0(a1,d3.l),d4
andi.w #$7fff,0(a0,d1.w)
ori.w #1,0(a1,d2.l)
move.b #1,0(a0,d6.l)
clr.b 0(a0,d6.l)
tst.b (a0,d0.l)
tst.b -128(a2,d7.w)
tst.b 127(a3,a6.l)
"#;

fn source(repetitions: usize) -> String {
    format!(
        ".cpu m68020\n.org 0\n{}\n.end\n",
        BODY.lines()
            .map(|line| format!(" {line}\n"))
            .collect::<String>()
            .repeat(repetitions)
    )
}

fn oracle(source: &str) -> Vec<u8> {
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live package-backed Rust oracle");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn binary_indexed_rust_oracle() {
    let bytes = oracle(&source(1));
    assert_eq!(
        &bytes[..14],
        [0x4a, 0x30, 0x30, 0, 0x11, 0x80, 0x18, 0, 0x0c, 0x31, 0, 13, 0x08, 0xff]
    );
    assert_eq!(oracle(&source(24)), bytes.repeat(24));
}

#[test]
fn binary_indexed_rejection_oracles() {
    for body in [
        "tst.b 0(d0,d1.w)",
        "tst.b 0(a0,d1.b)",
        "cmpi.b #13,128(a0,d1.w)",
    ] {
        let source = format!(".cpu m68020\n {body}\n.end\n");
        let result =
            assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true);
        assert!(
            !matches!(result, Ok((_, ref diagnostics)) if diagnostics.is_empty()),
            "accepted {body}"
        );
    }
}

fn native_source(source: String) {
    let expected = oracle(&source);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let outcome = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        Some(&expected),
    )
    .expect("fresh indexed operand parity");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    let image = &runs[0].captured_artifacts[&PathBuf::from("Work/build/opforge_compact")];
    eprintln!("COMPACT_INDEXED source_bytes={} output_bytes={} package_bytes={} image_bytes={} linked_reserved_bytes={} seconds={:?}", source.len(), expected.len(),package.len(),image.len(),hunk::allocation(image).unwrap().total(),runs[0].start_to_done_host_seconds);
}

#[test]
#[ignore = "requires configured FS-UAE; package-driven indexed operand forms"]
fn binary_indexed_native_parity_fs_uae() {
    native_source(source(1));
}

#[test]
#[ignore = "requires configured FS-UAE; repeated representative indexed workload"]
fn binary_indexed_workload_fs_uae() {
    native_source(source(24));
}

#[test]
#[ignore = "requires configured FS-UAE; invalid indexed base register"]
fn binary_indexed_bad_base_fs_uae() {
    compact_rejection(".cpu m68020\n tst.b 0(d0,d1.w)\n.end\n");
}

#[test]
#[ignore = "requires configured FS-UAE; indexed displacement outside package range"]
fn binary_indexed_bad_range_fs_uae() {
    compact_rejection(".cpu m68020\n cmpi.b #13,128(a0,d1.w)\n.end\n");
}

#[test]
#[ignore = "requires configured FS-UAE; signed tuple versus immediate sequence"]
fn binary_indexed_signed_sequence_fs_uae() {
    native_source(
        ".cpu m68020\n tst.b -1(a1,d0.l)\n cmpi.b #13,0(a1,d0.l)\n cmpi.b #13,-1(a1,d0.l)\n.end\n"
            .into(),
    );
}

fn compact_rejection(source: &str) {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let outcome = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        &[("input.asm", source.as_bytes())],
        &[],
        &[],
        None,
        false,
    )
    .expect("fresh compact CLI rejection");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
    assert!(runs[0]
        .stdout
        .contains("binary source: unsupported or invalid input"));
}
