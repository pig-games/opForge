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
