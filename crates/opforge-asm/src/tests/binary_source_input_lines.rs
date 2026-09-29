//! Physical source lines at the compact reader's 4096-byte buffer boundaries.
use super::*;

const LINE_BYTES: usize = 4096;

fn sources() -> Vec<(&'static str, String)> {
    let mut main = String::new();
    // LF is the last byte of the first refill, then the first byte after a
    // full-capacity line. Both comments are real physical source lines.
    main.push(';');
    main.push_str(&"x".repeat(LINE_BYTES - 2));
    main.push('\n');
    main.push(';');
    main.push_str(&"y".repeat(LINE_BYTES - 1));
    main.push('\n');
    main.push_str(".cpu m68020\n");
    // Put the 'o' in MOVEQ at the first byte of a refill. The physical
    // instruction, not just a comment, must be assembled across that read.
    let instruction_start = 3 * LINE_BYTES - 2;
    let padding = instruction_start - main.len() - 2;
    main.push(';');
    main.push_str(&"p".repeat(padding));
    main.push('\n');
    main.push_str("\tmoveq #7,d0\n.byte $11\n.include \"detail.inc\"\n\n\n.byte $33");

    let mut detail = String::new();
    // CR ends the refill and LF starts the next one. The copied line is
    // exactly LINE_BYTES, including the CR retained by the native collector.
    detail.push(';');
    detail.push_str(&"z".repeat(LINE_BYTES - 2));
    detail.push_str("\r\n");
    detail.push_str("\tnop\n.byte $22\n\n\n.byte $23");

    vec![("main.asm", main), ("detail.inc", detail)]
}

fn rust_cli_oracle(files: &[(&str, String)]) -> Vec<u8> {
    let dir = create_temp_dir("compact-physical-lines-oracle");
    for (name, source) in files {
        fs::write(dir.join(name), source).expect("stage Rust CLI source");
    }
    let output = dir.join("oracle.bin");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        dir.join(files[0].0).to_string_lossy().into_owned(),
        "--bin".to_string(),
        output.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "m68020".to_string(),
    ]);
    run_with_cli_with_context(&cli).expect("assemble staged Rust CLI oracle");
    let bytes = fs::read(&output).expect("read Rust CLI output");
    fs::remove_dir_all(dir).expect("remove Rust CLI oracle scratch");
    bytes
}

fn check_memory(run: &crate::fs_uae_smoke::FsUaeSmokeRun, bytes: usize, rejected: bool) {
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() != Ok("1") {
        return;
    }
    let record = &run.captured_artifacts[&PathBuf::from("Work/memory.bin")];
    assert_eq!(record.len(), 2280);
    let words = record
        .chunks_exact(4)
        .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
        .collect::<Vec<_>>();
    assert_eq!(words[0], 0x4d454d44);
    assert_eq!(words[1], 0, "cleanup releases all tracked allocations");
    assert_eq!(words[3], words[4], "allocated and freed capacity balances");
    assert_eq!(
        words[29],
        if rejected { 16 } else { 0 },
        "input clock must close even on overflow"
    );
    if std::env::var("OPFORGE_INPUT_DETAIL").as_deref() == Ok("1") {
        assert_eq!(
            words[568] as usize, bytes,
            "collection counts physical input bytes"
        );
        assert!(words[567] > 0 && words[569] > 0);
    }
}

#[test]
fn compact_physical_line_boundaries_rust_cli_oracle() {
    let files = sources();
    let main = files[0].1.as_bytes();
    let detail = files[1].1.as_bytes();
    assert_eq!(main[LINE_BYTES - 1], b'\n');
    assert_eq!(main[2 * LINE_BYTES], b'\n');
    assert_eq!(
        &main[3 * LINE_BYTES - 2..3 * LINE_BYTES + 10],
        b"\tmoveq #7,d0"
    );
    assert_eq!(&detail[LINE_BYTES - 1..=LINE_BYTES], b"\r\n");
    assert_eq!(main.last(), Some(&b'3'));
    assert_eq!(detail.last(), Some(&b'3'));
    assert_eq!(
        rust_cli_oracle(&files),
        [0x70, 0x07, 0x11, 0x4e, 0x71, 0x22, 0x23, 0x33]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; refill, CRLF, include EOF and parent resume"]
fn compact_physical_line_boundaries_fs_uae() {
    let files = sources();
    let expected = rust_cli_oracle(&files);
    assert_eq!(expected, [0x70, 0x07, 0x11, 0x4e, 0x71, 0x22, 0x23, 0x33]);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let inputs = files
        .iter()
        .map(|(name, source)| (*name, source.as_bytes()))
        .collect::<Vec<_>>();
    let outcome = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        &inputs,
        &[],
        &[],
        Some(&expected),
        false,
    )
    .expect("fresh compact physical-line parity");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    check_memory(
        &runs[0],
        files.iter().map(|(_, source)| source.len()).sum(),
        false,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; 4097-byte physical line exceeds native capacity"]
fn compact_physical_line_overflow_fs_uae() {
    let source = format!(
        ";{}\n.cpu m68020\n.byte $11\n.end\n",
        "x".repeat(LINE_BYTES)
    );
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let outcome = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        &[("main.asm", source.as_bytes())],
        &[],
        &[],
        None,
        false,
    )
    .expect("fresh compact line-capacity rejection");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
    check_memory(&runs[0], LINE_BYTES + 1, true);
    assert!(
        runs[0].stdout.contains("[file 00000001, line 00000001]"),
        "overflow must be attributed to the first physical source line: {}",
        runs[0].stdout
    );
}
