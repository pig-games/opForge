//! A live Rust Hunk oracle and opt-in compact-native comparison for sections.
use super::*;

const SOURCE: &str = ".module hunk_probe\n.cpu m68020\n.section code, kind=code\nentry: .long payload\n RTS\n.align 4\n.endsection\n.section data, kind=data\npayload: .byte $aa,$bb,$cc\n.endsection\n.section bss, kind=bss\n.res byte, 1\n.align 4\nreserved: .res byte, 5\n.endsection\n.output \"build/sections.hunk\", format=hunk, sections=code,bss,data\n.endmodule\n";

fn rust_hunk_oracle() -> Vec<u8> {
    rust_hunk_source(SOURCE)
}

fn rust_hunk_bytes(source: &str) -> Vec<u8> {
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
    oracle
}

pub(super) fn rust_hunk_source(source: &str) -> Vec<u8> {
    rust_hunk_source_with_allocation(source, 12)
}

pub(super) fn rust_hunk_source_with_allocation(source: &str, expected_bss: u64) -> Vec<u8> {
    let oracle = rust_hunk_bytes(source);
    let allocation = hunk::allocation(&oracle).expect("valid Rust Hunk");
    assert_eq!(allocation.segments, 3);
    assert_eq!(
        allocation.bss, expected_bss,
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

fn reopened_concrete_sections_source() -> String {
    let caller = SOURCE
        .replace(".module hunk_probe\n", ".module hunk_probe\n.use dep\n")
        .replace("entry: .long payload", "entry: .long dep.tail");
    format!(
        "{caller}.module dep\n.cpu m68020\n.pub\n.section code, kind=code\ntail: .byte $dd\n.endsection\n.section data, kind=data\nextra: .byte $ee\n.endsection\n.endmodule\n"
    )
}

#[test]
fn compact_hunk_reopened_concrete_sections_rust_oracle() {
    let hunk = rust_hunk_source(&reopened_concrete_sections_source());
    assert!(hunk.contains(&0xdd) && hunk.contains(&0xee));
}

#[test]
#[ignore = "requires configured FS-UAE; code and data sections reopened by imported module"]
fn compact_hunk_reopened_concrete_sections_fs_uae() {
    native_hunk_source(&reopened_concrete_sections_source());
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

fn pc_dispatch_reference_source() -> String {
    SOURCE.replace(
        "entry: .long payload\n RTS",
        "entry: LEA dispatchTable(PC),A1\n RTS\ndispatchTable: .long payload",
    )
}

#[test]
fn compact_hunk_pc_dispatch_reference_rust_oracle() {
    let oracle = rust_hunk_source(&pc_dispatch_reference_source());
    assert!(oracle
        .windows(6)
        .any(|bytes| bytes == [0x43, 0xfa, 0, 4, 0x4e, 0x75]));
    assert!(contains_hunk_reloc(&oracle, 2, 6));
}

#[test]
#[ignore = "requires configured FS-UAE; PC-relative code target and absolute DATA table entry"]
fn compact_hunk_pc_dispatch_reference_fs_uae() {
    native_hunk_source(&pc_dispatch_reference_source());
}

fn pc_literal_reference_source() -> String {
    SOURCE.replace(
        "entry: .long payload\n RTS",
        "entry: LEA 4(PC),A1\n RTS\n .long payload",
    )
}

fn pc_absolute_constant_source() -> String {
    SOURCE.replace(
        "entry: .long payload\n RTS",
        "OFFSET = 4\nentry: LEA OFFSET(PC),A1\n RTS\n .long payload",
    )
}

#[test]
fn compact_hunk_pc_offset_controls_rust_oracle() {
    for source in [pc_literal_reference_source(), pc_absolute_constant_source()] {
        let oracle = rust_hunk_source(&source);
        assert!(oracle
            .windows(6)
            .any(|bytes| bytes == [0x43, 0xfa, 0, 4, 0x4e, 0x75]));
        assert!(contains_hunk_reloc(&oracle, 2, 6));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; PC numeric and absolute constant offsets"]
fn compact_hunk_pc_offset_controls_fs_uae() {
    native_hunk_source(&pc_literal_reference_source());
    native_hunk_source(&pc_absolute_constant_source());
}

fn pc_and_absolute_destination_source() -> String {
    SOURCE.replace(
        "entry: .long payload\n RTS",
        "entry: MOVE.W dispatchTable(PC),payload\n RTS\ndispatchTable: .word 1",
    )
}

#[test]
fn compact_hunk_mixed_pc_absolute_rust_oracle() {
    let oracle = rust_hunk_source(&pc_and_absolute_destination_source());
    assert!(oracle.windows(2).any(|bytes| bytes == [0x33, 0xfa]));
    assert!(contains_hunk_reloc(&oracle, 2, 4));
}

#[test]
#[ignore = "requires configured FS-UAE; mixed PC and absolute Hunk fixups fail closed"]
fn compact_hunk_mixed_pc_absolute_barrier_fs_uae() {
    let source = pc_and_absolute_destination_source();
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        &[("input.asm", source.as_bytes())],
        &[],
        &[],
        None,
        false,
    )
    .expect("fresh mixed-fixup rejection");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
    assert!(runs[0]
        .stdout
        .contains("binary source: unsupported or invalid input"));
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
#[ignore = "requires configured FS-UAE; package MOVE.L relocation to BSS"]
fn compact_hunk_bss_instruction_relocation_fs_uae() {
    native_hunk_source(&bss_instruction_reference_source());
}

fn bss_addq_reference_source() -> String {
    SOURCE.replace(
        "entry: .long payload\n RTS",
        "entry: ADDQ.L #1,reserved\n RTS",
    )
}

#[test]
fn compact_hunk_bss_addq_rust_oracle() {
    let oracle = rust_hunk_source(&bss_addq_reference_source());
    assert!(oracle.windows(2).any(|bytes| bytes == [0x52, 0xb9]));
    assert!(contains_hunk_reloc(&oracle, 1, 2));
}

#[test]
#[ignore = "requires configured FS-UAE; ADDQ.L absolute BSS target"]
fn compact_hunk_bss_addq_fs_uae() {
    native_hunk_source(&bss_addq_reference_source());
}

#[test]
#[ignore = "requires configured FS-UAE; ADDQ.L absolute DATA target"]
fn compact_hunk_data_addq_fs_uae() {
    let source = SOURCE.replace(
        "entry: .long payload\n RTS",
        "entry: ADDQ.L #1,payload\n RTS",
    );
    native_hunk_source(&source);
}

fn immediate_data_reference_source() -> String {
    SOURCE.replace(
        "entry: .long payload\n RTS",
        "entry: MOVE.L #payload,d1\n RTS",
    )
}

#[test]
fn compact_hunk_immediate_data_rust_oracle() {
    let oracle = rust_hunk_source(&immediate_data_reference_source());
    assert!(oracle.windows(2).any(|bytes| bytes == [0x22, 0x3c]));
    assert!(contains_hunk_reloc(&oracle, 2, 2));
}

#[test]
fn compact_hunk_immediate_data_package_projection() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric =
        vm::binary_source_package::BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let candidate = numeric
        .candidates
        .iter()
        .find(|candidate| {
            numeric.names[usize::from(candidate.mnemonic)] == "move"
                && numeric.names[usize::from(candidate.shape)] == "immediate_register"
                && candidate
                    .qualifier
                    .is_some_and(|id| numeric.qualifiers[usize::from(id)] == "l")
                && candidate.priority == 76
        })
        .unwrap();
    let wire = prepare_package(&core, &resolved).unwrap();
    let long =
        |offset: usize| u32::from_be_bytes(wire[offset..offset + 4].try_into().unwrap()) as usize;
    let word = |offset: usize| u16::from_be_bytes(wire[offset..offset + 2].try_into().unwrap());
    let rows = long(16);
    let row = (0..long(20))
        .map(|index| rows + index * 32)
        .find(|&row| {
            word(row) == candidate.mnemonic
                && wire[row + 2] == candidate.qualifier.unwrap() as u8 + 1
                && wire[row + 3] == 3 // immediate_register
                && word(row + 6) == candidate.priority
        })
        .expect("serialized immediate target candidate");
    assert_eq!(wire[row + 5], 9);
    let match_stage = long(row + 12);
    let match_inputs = long(match_stage + 8);
    assert_eq!(wire[match_inputs], 17);
}

#[test]
#[ignore = "requires configured FS-UAE; immediate DATA address in MOVE.L"]
fn compact_hunk_immediate_data_fs_uae() {
    native_hunk_source(&immediate_data_reference_source());
}

#[test]
#[ignore = "requires configured FS-UAE; numeric immediate remains relocation-free"]
fn compact_hunk_immediate_numeric_fs_uae() {
    let source = SOURCE.replace("entry: .long payload\n RTS", "entry: MOVE.L #8,d1\n RTS");
    native_hunk_source(&source);
}

#[test]
#[ignore = "requires configured FS-UAE; Hunk numeric MOVE.L must not require relocation"]
fn compact_hunk_numeric_move_fs_uae() {
    let source = SOURCE.replace("entry: .long payload\n RTS", " MOVE.L 8,d0\n RTS");
    native_hunk_source(&source);
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

pub(super) fn native_hunk_source(source: &str) {
    native_hunk_source_with_allocation(source, 12);
}

pub(super) fn native_hunk_source_with_allocation(source: &str, expected_bss: u64) {
    let oracle = rust_hunk_source_with_allocation(source, expected_bss);
    native_hunk_bytes(source, &oracle);
}

fn native_hunk_bytes(source: &str, oracle: &[u8]) {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let native_root = std::env::var_os("OPFORGE_COMPARE_NATIVE_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(workspace_root);
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &native_root,
        &package,
        source.as_bytes(),
        Some(oracle),
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
    let source = SOURCE.replace(".long payload", ".long payload+1");
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        None,
    )
    .expect("fresh native rejection for unsupported expression relocation");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_ne!(runs[0].exit_code, Some(0));
}
