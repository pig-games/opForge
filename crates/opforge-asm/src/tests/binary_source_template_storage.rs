//! Growable template pools preserve bodies and identities across relocation.
use super::*;

const TELEMETRY: &str =
    include_str!("../../../../native/motorola68000/amigaos/debug/memory_telemetry.i");
const ENTRY: &str = ".include \"provider.i\"\n.module app\n.cpu m68020\n.use provider (EMIT)\n.include \"memory_telemetry.i\"\n.MEMORY_PHASE 0\n.MEMORY_LAYOUT 1,2,3\n.EMIT\n.endmodule\n";
const PROVIDER: &str = ".module provider\n.include \"memory_telemetry.i\"\n.pub\nEMIT .macro value=7\n.byte .value\n.endmacro\n.endmodule\n";
const PAYLOAD: &str =
    "01234567890123456789012345678901234567890123456789012345678901234567890123456789";

fn oracle(sources: &[(&str, &[u8])], cpu: &str) -> Vec<u8> {
    let dir = create_temp_dir("compact-template-storage-oracle");
    fs::create_dir_all(dir.join("debug")).unwrap();
    for (path, bytes) in sources {
        let target = dir.join(path);
        fs::create_dir_all(target.parent().unwrap()).unwrap();
        fs::write(target, bytes).unwrap();
    }
    let input = dir.join(sources[0].0);
    let output = dir.join("output.bin");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        cpu.to_string(),
        "--bin".to_string(),
        output.to_string_lossy().into_owned(),
        "-I".to_string(),
        dir.join("debug").to_string_lossy().into_owned(),
    ]);
    let config = validate_cli(&cli).unwrap();
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    let bytes = fs::read(output).unwrap();
    fs::remove_dir_all(dir).unwrap();
    bytes
}

fn growth_source() -> String {
    let mut source = ".cpu m6502\n".to_string();
    // Many definitions and aggregate bodies/defaults exceed the old independent
    // pool bounds; calls use both early and late definitions after relocation.
    for index in 0..120 {
        source.push_str(&format!("M{index:03} .macro value={index}+1\n"));
        for _ in 0..7 {
            source.push_str(&format!(".byte \"{PAYLOAD}\",.value\n"));
        }
        source.push_str(".endmacro\n");
    }
    source.push_str("WRAP .macro value=4\n.M119 .value\n.endmacro\n.M000\n.M119\n.WRAP\n.end\n");
    source
}

#[test]
fn compact_template_storage_rust_oracles() {
    let sources = [
        ("input.asm", ENTRY.as_bytes()),
        ("provider.i", PROVIDER.as_bytes()),
        ("debug/memory_telemetry.i", TELEMETRY.as_bytes()),
    ];
    assert_eq!(oracle(&sources, "68020"), [7]);
    let source = growth_source();
    // String tokens alone exceed 64 KiB, before directive/parameter tokens:
    // this exercises widened body cursors rather than just larger old caps.
    assert!(120 * 7 * (PAYLOAD.len() + 2) > usize::from(u16::MAX));
    let mut expected = Vec::new();
    for value in [1, 120, 4] {
        for _ in 0..7 {
            expected.extend(PAYLOAD.as_bytes());
            expected.push(value);
        }
    }
    assert_eq!(
        oracle(&[("input.asm", source.as_bytes())], "6502"),
        expected
    );
}

fn native(sources: &[(&str, &[u8])], cpu: &str) {
    let expected = oracle(sources, cpu);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let include_roots: &[&str] = if sources.iter().any(|(name, _)| name.starts_with("debug/")) {
        &["debug"]
    } else {
        &[]
    };
    let result = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        sources,
        &[],
        include_roots,
        Some(&expected),
        false,
    )
    .expect("fresh template-storage exact native comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    let image = &runs[0].captured_artifacts[&PathBuf::from("Work/build/opforge_compact")];
    eprintln!("COMPACT_TEMPLATE_STORAGE cpu={cpu} source_bytes={} output_bytes={} seconds={:?} image_bytes={} linked_reserved_bytes={}", sources.iter().map(|(_, bytes)| bytes.len()).sum::<usize>(), expected.len(), runs[0].start_to_done_host_seconds, image.len(), hunk::allocation(image).unwrap().total());
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let record = &runs[0].captured_artifacts[&PathBuf::from("Work/memory.bin")];
        let words = record
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(words[0], 0x4d454d35);
        assert_eq!(
            words[1], 0,
            "template pools and other owned blocks must be released"
        );
        assert_eq!(words[3], words[4], "all tracked allocations must be freed");
        eprintln!(
            "COMPACT_TEMPLATE_STORAGE_INSTRUMENTED peak_owned_bytes={}",
            words[2]
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; repeated telemetry includes and retained exported macro"]
fn compact_template_storage_modules_fs_uae() {
    native(
        &[
            ("input.asm", ENTRY.as_bytes()),
            ("provider.i", PROVIDER.as_bytes()),
            ("debug/memory_telemetry.i", TELEMETRY.as_bytes()),
        ],
        "m68020",
    );
}

#[test]
#[ignore = "requires configured FS-UAE; definition/default/body growth and nested expansion"]
fn compact_template_storage_growth_fs_uae() {
    let source = growth_source();
    native(&[("input.asm", source.as_bytes())], "m6502");
}
