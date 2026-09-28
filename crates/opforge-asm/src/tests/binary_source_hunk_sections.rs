//! A live Rust Hunk oracle and opt-in compact-native comparison for sections.
use super::*;

const SOURCE: &str = ".module hunk_probe\n.cpu m68020\n.section code, kind=code\nentry: .long payload\n RTS\n.align 4\n.endsection\n.section data, kind=data\npayload: .byte $aa,$bb,$cc\n.endsection\n.section bss, kind=bss\n.res byte, 1\n.align 4\nreserved: .res byte, 5\n.endsection\n.output \"build/sections.hunk\", format=hunk, sections=code,bss,data\n.endmodule\n";

fn rust_hunk_oracle() -> Vec<u8> {
    rust_hunk_source(SOURCE)
}

fn rust_hunk_source(source: &str) -> Vec<u8> {
    let dir = create_temp_dir("compact-hunk-sections-rust-oracle");
    fs::create_dir_all(dir.join("build")).expect("create output directory");
    let input = dir.join("input.asm");
    fs::write(&input, source).expect("write Hunk source");
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
    oracle
}

fn contains_hunk_reloc(oracle: &[u8], target: u32, offset: u32) -> bool {
    oracle
        .chunks_exact(4)
        .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
        .collect::<Vec<_>>()
        .windows(4)
        .any(|words| words == [0x3ec, 1, target, offset])
}

#[test]
fn compact_hunk_sections_live_rust_oracle() {
    let oracle = rust_hunk_oracle();
    assert!(oracle.starts_with(&[0, 0, 3, 0xf3]));
    assert!(
        contains_hunk_reloc(&oracle, 2, 0),
        "CODE must relocate its .long to the reordered DATA segment"
    );
}

#[test]
#[ignore = "requires configured FS-UAE; compact native Hunk section output"]
fn compact_hunk_sections_fs_uae() {
    native_hunk_source(SOURCE);
}

fn instruction_reference_source() -> String {
    SOURCE.replace("entry: .long payload\n RTS", "entry: LEA payload,a1\n RTS")
}

#[test]
fn compact_hunk_instruction_relocation_rust_oracle() {
    let oracle = rust_hunk_source(&instruction_reference_source());
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric =
        vm::binary_source_package::BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let lea = numeric.names.iter().position(|name| name == "lea").unwrap() as u16;
    let wire = prepare_package(&core, &resolved).unwrap();
    let rows = u32::from_be_bytes(wire[16..20].try_into().unwrap()) as usize;
    let count = u32::from_be_bytes(wire[20..24].try_into().unwrap()) as usize;
    assert!((0..count).any(|index| {
        let row = rows + index * 32;
        u16::from_be_bytes(wire[row..row + 2].try_into().unwrap()) == lea
            && u16::from_be_bytes(wire[row + 6..row + 8].try_into().unwrap()) == 78
            && wire[row + 5] == 9
    }));
    assert!(oracle.windows(2).any(|bytes| bytes == [0x43, 0xf9]));
    assert!(
        contains_hunk_reloc(&oracle, 2, 2),
        "LEA's absolute-long extension must relocate to DATA"
    );
}

#[test]
#[ignore = "requires configured FS-UAE; package instruction output relocation"]
fn compact_hunk_instruction_relocation_fs_uae() {
    native_hunk_source(&instruction_reference_source());
}

fn bss_instruction_reference_source() -> String {
    SOURCE.replace(
        "entry: .long payload\n RTS",
        "entry: MOVE.L reserved,d0\n RTS",
    )
}

#[test]
fn compact_hunk_bss_instruction_relocation_rust_oracle() {
    let oracle = rust_hunk_source(&bss_instruction_reference_source());
    assert!(oracle.windows(2).any(|bytes| bytes == [0x20, 0x39]));
    assert!(
        contains_hunk_reloc(&oracle, 1, 2),
        "MOVE.L's absolute-long source must relocate to BSS"
    );
}

#[test]
#[ignore = "known compact-native Hunk MOVE.L failure; requires FS-UAE"]
fn compact_hunk_bss_instruction_relocation_fs_uae() {
    native_hunk_source(&bss_instruction_reference_source());
}

fn self_host_constants_source() -> String {
    SOURCE.replace(
        ".cpu m68020\n",
        ".cpu m68020\nPATH_BYTES = 256\nMODULE_ROOT_LIMIT = 8\nINCLUDE_ROOT_LIMIT = 16\nOPEN_LIBRARY = -552\nCLOSE_LIBRARY = -414\nGET_ARG_STR = -534\nPUT_STR = -948\n",
    )
    .replace("entry: .long payload", "entry: .long payload\n .word GET_ARG_STR")
}

#[test]
fn compact_hunk_self_host_constants_rust_oracle() {
    let oracle = rust_hunk_source(&self_host_constants_source());
    assert!(oracle.windows(2).any(|bytes| bytes == [0xfd, 0xea]));
}

#[test]
#[ignore = "requires configured FS-UAE; negative constants preceding Hunk sections"]
fn compact_hunk_self_host_constants_fs_uae() {
    native_hunk_source(&self_host_constants_source());
}

fn native_hunk_source(source: &str) {
    let oracle = rust_hunk_source(source);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
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

fn reserved_segment_source() -> String {
    SOURCE.replace(".section bss, kind=bss\n.res byte, 1\n.align 4\nreserved: .res byte, 5\n.endsection",
        "RESERVE .segment amount,boundary\n .res byte,.amount\n .align .2\n.endsegment\n.section bss, kind=bss\n .RESERVE 1,4\nreserved .RESERVE 5,1\n.endsection")
}

#[test]
fn compact_hunk_reserved_segment_rust_oracle() {
    assert_eq!(
        rust_hunk_source(&reserved_segment_source()),
        rust_hunk_oracle()
    );
}

#[test]
#[ignore = "requires configured FS-UAE; core BSS directives substituted by a labeled segment"]
fn compact_hunk_reserved_segment_fs_uae() {
    native_hunk_source(&reserved_segment_source());
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
