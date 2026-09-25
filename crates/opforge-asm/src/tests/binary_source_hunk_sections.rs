//! A live Rust Hunk oracle and opt-in compact-native comparison for sections.
use super::*;

const SOURCE: &str = ".module hunk_probe\n.cpu m68020\n.section code, kind=code\nentry: .long payload\n RTS\n.align 4\n.endsection\n.section data, kind=data\npayload: .byte $aa,$bb,$cc\n.endsection\n.section bss, kind=bss\n.res byte, 1\n.align 4\nreserved: .res byte, 5\n.endsection\n.output \"build/sections.hunk\", format=hunk, sections=code,bss,data\n.endmodule\n";

fn rust_hunk_oracle() -> Vec<u8> {
    let dir = create_temp_dir("compact-hunk-sections-rust-oracle");
    fs::create_dir_all(dir.join("build")).expect("create output directory");
    let input = dir.join("input.asm");
    fs::write(&input, SOURCE).expect("write Hunk source");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "68020".to_string(),
    ]);
    let mut config = validate_cli(&cli).expect("validate live Rust Hunk oracle");
    config.out_dir = Some(dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("assemble Hunk source with Rust");
    let oracle = fs::read(dir.join("build/sections.hunk")).expect("read Rust Hunk oracle");
    fs::remove_dir_all(&dir).expect("remove Rust oracle directory");
    let allocation = hunk::allocation(&oracle).expect("valid Rust Hunk");
    assert_eq!(allocation.segments, 3);
    assert_eq!(
        allocation.bss, 12,
        "BSS reservation and alignment round to Hunk words"
    );
    assert!(
        oracle
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect::<Vec<_>>()
            .windows(4)
            .any(|words| words == [0x3ec, 1, 2, 0]),
        "CODE must relocate its .long to the reordered DATA segment"
    );
    oracle
}

#[test]
fn compact_hunk_sections_live_rust_oracle() {
    let oracle = rust_hunk_oracle();
    assert!(oracle.starts_with(&[0, 0, 3, 0xf3]));
}

#[test]
#[ignore = "requires configured FS-UAE; compact native Hunk section output"]
fn compact_hunk_sections_fs_uae() {
    let oracle = rust_hunk_oracle();
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        SOURCE.as_bytes(),
        Some(&oracle),
    )
    .expect("fresh compact CLI exact Hunk comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    let image = runs[0]
        .captured_artifacts
        .get(&PathBuf::from("Work/build/opforge_compact"))
        .expect("fresh compact executable");
    eprintln!(
        "COMPACT_HUNK_SECTIONS seconds={:?} image_bytes={} linked_reserved_bytes={} output_bytes={}",
        runs[0].start_to_done_host_seconds,
        image.len(),
        hunk::allocation(image).expect("valid compact executable").total(),
        oracle.len()
    );
}

#[test]
#[ignore = "requires configured FS-UAE; unsupported Hunk expression must fail closed"]
fn compact_hunk_expression_relocation_rejects_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    for unsupported in [".long payload+1", "move.l #payload,d0"] {
        let source = SOURCE.replace(".long payload", unsupported);
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            None,
        )
        .unwrap_or_else(|error| panic!("fresh native rejection for {unsupported}: {error}"));
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].protocol_completed);
        assert_ne!(runs[0].exit_code, Some(0));
    }
}
