//! Connected named-block capacity, independent of imports and module count.
use super::*;
use std::fmt::Write as _;

fn source(count: usize) -> String {
    let mut source = String::from(".module main\n.cpu m68020\n.org $1000\n");
    // Every block is referenced from live root data, with no import edges.
    for index in 0..count {
        writeln!(source, ".word block{index}").unwrap();
    }
    for index in 0..count {
        writeln!(source, "block{index} .block\n.byte {}\n.bend", index % 251).unwrap();
    }
    source.push_str(".endmodule\n.end\n");
    source
}

fn expected(count: usize) -> Vec<u8> {
    let body_start = 0x1000 + count * 2;
    let mut bytes = Vec::with_capacity(count * 3);
    for index in 0..count {
        bytes.extend_from_slice(&u16::try_from(body_start + index).unwrap().to_be_bytes());
    }
    bytes.extend((0..count).map(|index| (index % 251) as u8));
    bytes
}

#[test]
fn connected_block_capacity_1024_rust_oracle() {
    for count in [1023, 1025] {
        let source = source(count);
        let mut entries =
            oracle_entries_with_search_roots(&[("main.asm", &source)], &[], &[]).unwrap();
        entries.sort_unstable_by_key(|(address, _)| *address);
        assert_eq!(entries, (0x1000..).zip(expected(count)).collect::<Vec<_>>());
    }
}

#[test]
#[ignore = "requires fresh native connected 1024-block index capacity proof"]
fn compact_cli_connected_block_capacity_1024_fs_uae() {
    let count = std::env::var("OPFORGE_BLOCK_CAPACITY_COUNT")
        .map(|value| value.parse::<usize>().expect("block count"))
        .unwrap_or(1025);
    assert!(matches!(count, 1023 | 1025));
    let source = source(count);
    let expected = expected(count);
    assert_eq!(
        oracle_address_ordered_with_roots(&[("main.asm", &source)], &[]).unwrap(),
        expected
    );
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let native_root = std::env::var_os("OPFORGE_BLOCK_CAPACITY_NATIVE_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(workspace_root);
    let result = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &native_root,
        &package,
        &[("main.asm", source.as_bytes())],
        &[],
        &[],
        Some(&expected),
        false,
    )
    .expect("fresh connected block-capacity proof");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed && runs[0].success);
    assert_eq!(runs[0].exit_code, Some(0));
    eprintln!(
        "CONNECTED_BLOCK_CAPACITY modules=1 imports=0 blocks={count} bytes={} fresh_success=true",
        expected.len()
    );
}
