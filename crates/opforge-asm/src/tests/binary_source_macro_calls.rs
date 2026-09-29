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

fn inactive_header_source() -> String {
    ".cpu m68020\n.if 0\nignored .macro value=\n.byte 99\n.endmacro\n.endif\nemit .macro value\n.if .value\n.byte 1\n.else\n.byte 2\n.endif\n.endmacro\n.emit 1\n.emit 0\n.end\n".into()
}

#[test]
fn compact_macro_inactive_header_rust_oracle() {
    assert_eq!(oracle(&inactive_header_source()), [1, 2]);
}

#[test]
#[ignore = "requires configured FS-UAE; inactive headers and both conditional branches"]
fn compact_macro_inactive_header_fs_uae() {
    native_source(inactive_header_source());
}

fn native_source(source: String) {
    native_source_for_cpu(source, "m68020");
}

fn native_source_for_cpu(source: String, cpu: &str) {
    let expected = oracle(&source);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
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

fn assembly_telemetry_source() -> String {
    let telemetry =
        include_str!("../../../../native/motorola68000/amigaos/debug/memory_telemetry.i");
    format!(".module app\n.cpu m68020\n{telemetry}\nMissing=0\n.ASSEMBLY_FAILURE_STAGE Missing,#1\n.byte 1\n.endmodule\n")
}

#[test]
fn compact_assembly_telemetry_definitions_rust_oracle() {
    assert_eq!(oracle(&assembly_telemetry_source()), [1]);
}

#[test]
#[ignore = "requires configured FS-UAE; compact frontend reads disabled trace macros"]
fn compact_assembly_telemetry_definitions_fs_uae() {
    native_source(assembly_telemetry_source());
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

fn inactive_dotted_operand_source() -> String {
    r#".module experimental.amigaos.binary_app
.cpu m68020
MEMORY_PHASE .macro value
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.l .value, d0
.endif
.endif
.endmacro
execute .block
.MEMORY_PHASE #0
.byte 1
.bend
.byte execute
.endmodule
"#
    .into()
}

#[test]
fn compact_macro_inactive_operand_rust_oracle() {
    assert_eq!(oracle(&inactive_dotted_operand_source()), [1, 0]);
}

#[test]
#[ignore = "requires configured FS-UAE; inactive macro expression operand"]
fn compact_macro_inactive_operand_fs_uae() {
    native_source(inactive_dotted_operand_source());
}

fn forward_labeled_call_source() -> String {
    r#".module app
.cpu m68020
OUTER .macro value
inside .INNER .value
.endmacro
INNER .macro value
.byte .value
.endmacro
.OUTER 7
.endmodule
"#
    .into()
}

#[test]
fn compact_macro_forward_labeled_call_rust_oracle() {
    assert_eq!(oracle(&forward_labeled_call_source()), [7]);
}

#[test]
#[ignore = "requires configured FS-UAE; forward nested call with canonical label"]
fn compact_macro_forward_labeled_call_fs_uae() {
    native_source(forward_labeled_call_source());
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
#[ignore = "requires configured FS-UAE; disabled telemetry beyond 64 KiB of spellings"]
fn compact_macro_arena_wide_fs_uae() {
    native_source(telemetry_arena_source(280, 200));
}

pub(super) fn check_memory(record: &[u8], expected_errors: u32) {
    assert_eq!(record.len(), 2212);
    let words = record
        .chunks_exact(4)
        .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap()))
        .collect::<Vec<_>>();
    assert_eq!(words[0], 0x4d454d42);
    assert_eq!(words[1], 0, "all owned blocks released");
    assert_eq!(words[3], words[4], "allocation/free accounting balances");
    assert_eq!(words[11], 0);
    assert_eq!(words[29], expected_errors);
    eprintln!(
        "COMPACT_MACRO_MEMORY peak_owned_bytes={} profiling_errors={}",
        words[2], words[29]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; package-owned formal names retain selected spelling"]
fn compact_macro_package_formal_spelling_fs_uae() {
    for cpu in ["m6502", "m68020"] {
        native_source_for_cpu(format!(".cpu {cpu}\n.org $2000\nPAIR .macro a, b=2\n .byte .a, .b\n .byte \".a,.b\"\n.endmacro\n .PAIR 1\n .PAIR(3, 4)\n.end\n"), cpu);
    }
}

fn core_body_source(cpu: &str) -> String {
    format!(".cpu {cpu}\n.org $2000\nINNER .macro n\n .byte .n\n.endmacro\nPAD .macro a=4\n .byte \".a\"\n .align .1\n .INNER(.a)\n.endmacro\nentry .PAD 4\n .PAD\n.word entry\n.end\n")
}

#[test]
fn compact_macro_core_body_rust_oracle() {
    let expected = oracle(&core_body_source("m6502"));
    assert!(!expected.is_empty());
    assert_eq!(expected[0], b'4');
    assert_eq!(&expected[expected.len() - 2..], [0, 0x20]);
}

#[test]
#[ignore = "requires configured FS-UAE; core body substitutions followed by a nested invocation"]
fn compact_macro_core_body_fs_uae() {
    for cpu in ["m6502", "m68020"] {
        native_source_for_cpu(core_body_source(cpu), cpu);
    }
}

fn fragment_call_source(cpu: &str) -> String {
    format!(
        r#".cpu {cpu}
INNER .macro left,right
 .byte .left,.right
.endmacro
TEXT .macro n
 .byte ".n"
.endmacro
OUTER .macro a,b=2
 .INNER .a,.{{b}}
 .INNER @1,.2
 .TEXT .unknown
.endmacro
FORWARD .macro x,y
 .INNER .@
.endmacro
 .OUTER 3,4
 .OUTER 5
 .FORWARD($06 ,  $07)
.end
"#
    )
}

#[test]
fn compact_macro_fragment_call_rust_oracle() {
    assert_eq!(
        oracle(&fragment_call_source("m6502")),
        [
            vec![3, 4, 3, 4],
            b".unknown".to_vec(),
            vec![5, 2, 5, 2],
            b".unknown".to_vec(),
            vec![6, 7]
        ]
        .concat()
    );
}

#[test]
#[ignore = "requires configured FS-UAE; cached call fragments, defaults and unresolved markers"]
fn compact_macro_fragment_call_fs_uae() {
    for cpu in ["m6502", "m68020"] {
        native_source_for_cpu(fragment_call_source(cpu), cpu);
    }
}

fn nested_string_source(body: &str, argument: &str) -> String {
    format!(
        ".module app\n.cpu m6502\nINNER .macro a,b,c\n.byte .a,.b,.c\n.endmacro\nOUTER .macro value\n.INNER {body}\n.endmacro\n.OUTER {argument}\n.endmodule\n"
    )
}

#[test]
fn compact_macro_nested_string_rust_oracles() {
    for (body, argument, expected) in [
        (r#""\x401",7,"B""#, "A", &b"@1\x07B"[..]),
        (r#""@1""#, r#"A",7,"B"#, &b"A\x07B"[..]),
    ] {
        assert_eq!(oracle(&nested_string_source(body, argument)), expected);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; nested string substitution and token boundaries"]
fn compact_macro_nested_string_fs_uae() {
    for (body, argument) in [(r#""\x401",7,"B""#, "A"), (r#""@1""#, r#"A",7,"B"#)] {
        native_source_for_cpu(nested_string_source(body, argument), "m6502");
    }
}

fn segment_nested_string_source() -> String {
    r#".module app
.cpu m6502
INNER .macro a,b,c
.byte .a,.b,.c
.endmacro
OUTER .segment value
.INNER "\x401",7,"B"
.endsegment
.OUTER A
.endmodule
"#
    .into()
}

#[test]
fn compact_macro_segment_nested_string_rust_oracle() {
    assert_eq!(oracle(&segment_nested_string_source()), b"@1\x07B");
}

#[test]
#[ignore = "requires configured FS-UAE; segment forwards nested string call"]
fn compact_macro_segment_nested_string_fs_uae() {
    native_source_for_cpu(segment_nested_string_source(), "m6502");
}

fn empty_nested_call_source() -> String {
    r#".module app
.cpu m6502
INNER .macro
.byte 7
.endmacro
OUTER .macro
.INNER
.endmacro
.OUTER
.endmodule
"#
    .into()
}

#[test]
fn compact_macro_empty_nested_call_rust_oracle() {
    assert_eq!(oracle(&empty_nested_call_source()), [7]);
}

#[test]
#[ignore = "requires configured FS-UAE; zero-argument generated call"]
fn compact_macro_empty_nested_call_fs_uae() {
    native_source_for_cpu(empty_nested_call_source(), "m6502");
}
