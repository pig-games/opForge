//! Component proof for numeric text-owner APIs. This does not qualify the
//! compact source parser or directive execution integration.
use super::native_iterable_loops::{oracle, scratch};
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

const HARNESS: &str =
    "native/motorola68000/amigaos/test-harnesses/experimental/binary_text_harness.asm";
const ORACLE: &str = r#".module main
.cpu 68020
.enc petscii
.byte "Az09"
.encode game
.cdef "A","Z",1
.tdef "xy",$40
.tdef "!?",$80,$81
.edef "{cr}",13
.edef "{erase}",""
.edef "{long-pattern-over-sixteen}","replacement-over-sixteen"
.edef "{1}",1
.edef "{2}",2
.edef "{3}",3
.edef "{4}",4
.edef "{5}",5
.edef "{6}",6
.edef "{7}",7
.edef "{8}",8
.edef "{9}",9
.edef "{cr}",10
.edef "A!",$ff
.endencode
.enc game
.byte "A{cr}xy!?{erase}{long-pattern-over-sixteen}{9}A!"
.encode cloned,game
.edef "{cr}",11
.endencode
.enc cloned
.byte "A{cr}xy{9}"
.enc game
.byte "A{cr}xy{9}"
.encode shifted,petscii
.edef "{home}",19
.endencode
.enc shifted
.byte "Az{home}"
.endmodule
.module second
.byte "Az"
.enc game
.byte "A{cr}xy{9}"
.endmodule
"#;

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

fn image(root: &Path, scratch: &Path) -> Vec<u8> {
    image_named(root, scratch, HARNESS, "text-component.hunk")
}

fn image_named(root: &Path, scratch: &Path, harness: &str, artifact: &str) -> Vec<u8> {
    let output = scratch.join("component");
    fs::create_dir_all(&output).unwrap();
    let dir = output.as_path();
    // The common Rust CLI resolves the real owner modules. All generated
    // outputs, including the source .output request, stay in disposable scratch.
    let source = fs::read_to_string(root.join(harness)).unwrap();
    let input = dir.join("component.asm");
    fs::write(&input, source).unwrap();
    let amiga = root.join("native/motorola68000/amigaos");
    let cli = Cli::parse_from([
        "opForge".to_owned(),
        input.to_string_lossy().into_owned(),
        "-M".into(),
        amiga.to_string_lossy().into_owned(),
        "-I".into(),
        amiga.join("include").to_string_lossy().into_owned(),
        "-I".into(),
        amiga.join("debug").to_string_lossy().into_owned(),
    ]);
    let mut config = validate_cli(&cli).expect("validate text component assembly");
    config.out_dir = Some(dir.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config).expect("assemble real text owner modules");
    fs::read(dir.join(artifact)).expect("component Hunk in scratch")
}

#[test]
fn native_text_encoding_component_host_contract() {
    let dir = scratch();
    let expected = oracle(&dir.0, ORACLE).expect("live Rust text oracle");
    assert_eq!(&expected[..4], &[0xc1, 0x5a, b'0', b'9']);
    assert_eq!(
        &expected[expected.len() - 7..],
        &[b'A', b'z', 1, 10, 0x40, 0x41, 9]
    );
    assert!(expected
        .windows(24)
        .any(|bytes| bytes == b"replacement-over-sixteen"));
    assert!(!image(&workspace_root(), &dir.0).is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; fresh text owner component artifact proof"]
fn native_text_encoding_component_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let dir = scratch();
    let root = workspace_root();
    let expected = oracle(&dir.0, ORACLE).expect("case-owned live Rust text oracle");
    let executable = image(&root, &dir.0);
    let case = OpforgeNativeCliParityCase {
        name: "text-owner-component",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(ORACLE.as_bytes()),
        // The component deliberately accepts and ignores CLI arguments. The
        // ordinary source still binds the fresh challenge and direct oracle.
        command_template: Some("--cpu 68020 Work:main.asm"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/build/text-results.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &executable)
        .expect("fresh text component completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    assert!(runs[0].success, "{}\n{}", runs[0].stdout, runs[0].stderr);
}

const GRAMMAR_HARNESS: &str =
    "native/motorola68000/amigaos/test-harnesses/experimental/binary_text_grammar_harness.asm";
fn grammar_oracle() -> Vec<u8> {
    use package::{packed_text_program, PARSER_VM_MACRO_VERSION, PARSER_VM_PACKED_TEXT_ENTRY};
    use vm::packed_source_layout as layout;
    let program = packed_text_program([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    // Same immutable .edef record as the native request-frame component:
    // delimiter-looking pattern bytes, empty replacement, scalar replacement.
    let mut record = vec![
        0,
        layout::FLAG_INDENT,
        0,
        1,
        layout::TOKEN_DOT,
        layout::TOKEN_IDENTIFIER,
        0,
        7,
        0,
        layout::TOKEN_STRING,
        3,
        layout::TOKEN_COMMA,
        layout::TOKEN_OPEN_BRACKET,
        layout::TOKEN_CLOSE_PAREN,
        layout::TOKEN_COMMA,
        layout::TOKEN_STRING,
        0,
        layout::TOKEN_COMMA,
        layout::TOKEN_NUMBER,
        0,
        0,
        0,
        9,
    ];
    record[layout::RECORD_LENGTH] = (record.len() - 1) as u8;
    let mut bytes = Vec::new();
    for flag in [0, layout::FLAG_MUTABLE_SNAPSHOT, layout::FLAG_PLAN] {
        record[layout::RECORD_FLAGS] = layout::FLAG_INDENT | flag;
        let rows = vm::packed_text_vm::execute(
            PARSER_VM_PACKED_TEXT_ENTRY,
            PARSER_VM_MACRO_VERSION,
            &program,
            &record,
            128,
            100,
        )
        .expect("live shared text grammar oracle");
        assert_eq!(rows.len(), 4);
        bytes.extend(rows.into_iter().flatten());
    }
    bytes
}
fn grammar_oracle_source(expected: &[u8]) -> String {
    let mut source = String::from(".cpu 68020\n");
    // Keep each generated statement within the normal tokenizer budget.
    for row in expected.chunks(32) {
        source.push_str(".byte ");
        source.push_str(&row.iter().map(u8::to_string).collect::<Vec<_>>().join(","));
        source.push('\n');
    }
    source
}

#[test]
fn native_text_grammar_component_host_contract() {
    let dir = scratch();
    let expected = grammar_oracle();
    assert_eq!(expected.len(), 384);
    assert_eq!(
        oracle(&dir.0, &grammar_oracle_source(&expected)).unwrap(),
        expected
    );
    assert!(!image_named(
        &workspace_root(),
        &dir.0,
        GRAMMAR_HARNESS,
        "text-grammar.hunk"
    )
    .is_empty());
}
#[test]
#[ignore = "requires configured FS-UAE; fresh shared text grammar component proof"]
fn native_text_grammar_component_fs_uae() {
    let dir = scratch();
    let root = workspace_root();
    let expected = grammar_oracle();
    let source = grammar_oracle_source(&expected);
    assert_eq!(oracle(&dir.0, &source).unwrap(), expected);
    let executable = image_named(&root, &dir.0, GRAMMAR_HARNESS, "text-grammar.hunk");
    let case = OpforgeNativeCliParityCase {
        name: "text-grammar-component",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some("--cpu 68020 Work:main.asm"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/build/text-grammar.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &executable)
        .expect("fresh text grammar component completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    assert!(runs[0].success, "{}\n{}", runs[0].stdout, runs[0].stderr);
}
