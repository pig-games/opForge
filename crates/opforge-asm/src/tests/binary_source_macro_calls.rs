//! Macro arguments and lexical lookup use the same packed execution path.
use super::*;

const DEFINITIONS: &str = r#".module app
.cpu m68020
Frame .struct
Value .long ?
.endstruct
SourceBytes = 10
emit .macro value
.byte 1
.endmacro
args .macro first,second,third
.byte 2
.endmacro
zero .macro
.byte 3
.endmacro
"#;

fn source(nested: bool) -> String {
    let calls = ".emit #0\n.args Frame.Value(a4),d1,SourceBytes\n.zero\n";
    if nested {
        format!("{DEFINITIONS}routine .block\n.namespace inner\n{calls}.endnamespace\n.bend\n.byte routine\n.endmodule\n")
    } else {
        format!("{DEFINITIONS}{calls}.endmodule\n")
    }
}

fn oracle(source: &str) -> Vec<u8> {
    let dir = create_temp_dir("compact-macro-call-oracle");
    let input = dir.join("input.asm");
    let output = dir.join("output.bin");
    fs::write(&input, source).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "m68020".to_string(),
        "--bin".to_string(),
        output.to_string_lossy().into_owned(),
    ]);
    let config = validate_cli(&cli).unwrap();
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    let bytes = fs::read(output).unwrap();
    fs::remove_dir_all(dir).unwrap();
    bytes
}

#[test]
fn compact_macro_call_rust_oracles() {
    assert_eq!(oracle(&source(false)), [1, 2, 3]);
    assert_eq!(oracle(&source(true)), [1, 2, 3, 0]);
}

fn native_source(source: String) {
    let expected = oracle(&source);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        Some(&expected),
    )
    .expect("fresh packed macro call comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let record = &runs[0].captured_artifacts[&PathBuf::from("Work/memory.bin")];
        check_memory(record, 0);
    }
    let image = &runs[0].captured_artifacts[&PathBuf::from("Work/build/opforge_compact")];
    eprintln!("COMPACT_MACRO_CALL source_bytes={} output_bytes={} seconds={:?} image_bytes={} linked_reserved_bytes={}", source.len(), expected.len(), runs[0].start_to_done_host_seconds, image.len(), hunk::allocation(image).unwrap().total());
}

#[test]
#[ignore = "requires configured FS-UAE; generic macro argument forms"]
fn compact_macro_call_root_fs_uae() {
    native_source(source(false));
}

#[test]
#[ignore = "requires configured FS-UAE; ancestor macro calls in block/namespace"]
fn compact_macro_call_nested_fs_uae() {
    native_source(source(true));
}

// The invocation-local gate name exceeds the former 63-byte composed-name cap.
// Keep the real disabled telemetry definitions: no telemetry code may be emitted.
fn telemetry_source() -> String {
    let telemetry =
        include_str!("../../../../native/motorola68000/amigaos/debug/memory_telemetry.i");
    format!(".module experimental.amigaos.binary_app\n.cpu m68020\n{telemetry}\nexecute .block\n.MEMORY_PHASE #0\n.byte 1\n.bend\n.byte execute\n.endmodule\n")
}

#[test]
fn compact_macro_telemetry_rust_oracle() {
    assert_eq!(oracle(&telemetry_source()), [1, 0]);
}

#[test]
#[ignore = "requires configured FS-UAE; telemetry expansion in a qualified block"]
fn compact_macro_telemetry_fs_uae() {
    native_source(telemetry_source());
}

fn telemetry_family_source() -> String {
    let telemetry =
        include_str!("../../../../native/motorola68000/amigaos/debug/memory_telemetry.i");
    let calls = [
        ".MEMORY_ALLOC #16",
        ".MEMORY_WORK #0,#1",
        ".MEMORY_CLOCK a6,#0",
        ".MEMORY_FREE #16",
        ".MEMORY_PHASE #0",
        ".MEMORY_SAVE a6",
        ".MEMORY_LAYOUT #1,#2,#3",
        ".MEMORY_STAGE #0",
        ".TOKEN_BEGIN #1",
        ".TOKEN_OPCODE #4",
        ".TOKEN_SCOPE_BEGIN #0",
        ".TOKEN_SCOPE_END #0",
        ".TOKEN_WORK #0,#1",
        ".TOKEN_SCOPE_CLOSE #0",
    ];
    let body = calls
        .iter()
        .enumerate()
        .map(|(index, call)| format!("{call}\n.byte {}\n", index + 1))
        .collect::<String>();
    format!(".module experimental.amigaos.binary_app\n.cpu m68020\n{telemetry}\nexecute .block\n{body}.bend\n.byte execute\n.endmodule\n")
}

#[test]
fn compact_macro_telemetry_family_rust_oracle() {
    let mut expected = (1..=14).collect::<Vec<u8>>();
    expected.push(0);
    assert_eq!(oracle(&telemetry_family_source()), expected);
}

#[test]
#[ignore = "requires configured FS-UAE; complete disabled telemetry macro family"]
fn compact_macro_telemetry_family_fs_uae() {
    native_source(telemetry_family_source());
}

fn telemetry_arena_source(count: usize, width: usize) -> String {
    let mut source = telemetry_family_source();
    let filler = (0..count)
        .map(|index| format!("Fill{index:03}_{} = {index}\n", "x".repeat(width)))
        .collect::<String>();
    // Distinct module-level names consume spelling storage while remaining
    // ordinary constants. No fixture identity selects a production path.
    source = source.replacen("execute .block", &format!("{filler}execute .block"), 1);
    source
}

fn telemetry_growth_source() -> String {
    // Prefix binding crosses an allocation boundary while opening this module.
    // Subsequent long sibling names grow the spelling arena beyond 16 KiB.
    let module = [
        format!("root{}", "x".repeat(45)),
        format!("middle{}", "y".repeat(43)),
        format!("leaf{}", "z".repeat(45)),
    ]
    .join(".");
    telemetry_arena_source(180, 72).replacen("experimental.amigaos.binary_app", &module, 1)
}

#[test]
fn compact_macro_arena_growth_rust_oracle() {
    assert_eq!(
        oracle(&telemetry_growth_source()),
        oracle(&telemetry_family_source())
    );
    assert_eq!(
        oracle(&telemetry_arena_source(280, 200)),
        oracle(&telemetry_family_source())
    );
}

#[test]
#[ignore = "requires configured FS-UAE; spelling arena growth with packed macro expansion"]
fn compact_macro_arena_growth_fs_uae() {
    native_source(telemetry_growth_source());
}

#[test]
#[ignore = "requires configured FS-UAE; explicit 16-bit spelling-offset bound"]
fn compact_macro_arena_bound_fs_uae() {
    let source = telemetry_arena_source(280, 200);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let outcome = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        None,
    )
    .expect("fresh bounded spelling rejection");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
    assert!(runs[0]
        .stdout
        .contains("binary source: unsupported or invalid input"));
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let record = &runs[0].captured_artifacts[&PathBuf::from("Work/memory.bin")];
        check_memory(record, 16);
    }
}

fn check_memory(record: &[u8], expected_errors: u32) {
    assert_eq!(record.len(), 1756);
    let words = record
        .chunks_exact(4)
        .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap()))
        .collect::<Vec<_>>();
    assert_eq!(words[0], 0x4d454d35);
    assert_eq!(words[1], 0, "all owned blocks released");
    assert_eq!(words[3], words[4], "allocation/free accounting balances");
    assert_eq!(words[11], 0);
    assert_eq!(words[29], expected_errors);
    eprintln!(
        "COMPACT_MACRO_MEMORY peak_owned_bytes={} profiling_errors={}",
        words[2], words[29]
    );
}
