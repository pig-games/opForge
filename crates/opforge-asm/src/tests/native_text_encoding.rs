//! Component and compact CLI proofs for shared numeric text encoding.
//! Focused controls do not establish complete source-language parity.
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

// Full compact-source integration. Each native case is compared with its own
// current Rust execution, including module selection and decoded-string policy.
const INTEGRATED_POSITIVE: &[(&str, &str)] = &[
    ("nested-definitions-close-in-module", ".module main\n.cpu 68020\n.encode outer\n.cdef \"A\",\"A\",1\n.encode inner\n.cdef \"B\",\"B\",2\n.endencode\n.cdef \"C\",\"C\",3\n.endencode\n.enc outer\n.text \"AC\"\n.enc inner\n.text \"B\"\n.endmodule\n"),
    ("registry-replay-and-modules", ORACLE),
    ("forward-definition-values", ".module main\n.cpu 68020\n.encode ranges\n.cdef \"A\",\"B\",CODE\n.endencode\n.enc ranges\n.text \"AB\"\n.encode tables\n.tdef \"AB\",CODE\n.endencode\n.enc tables\n.text \"AB\"\n.encode escapes\n.edef \"{x}\",CODE\n.endencode\n.enc escapes\n.text \"{x}\"\nCODE .const 7\n.endmodule\n"),
    ("scalar-data-and-padding", ".module main\n.cpu 68020\n.emit byte,0,255\n.emit word,$1234\n.emit long,$12345678\n.emit 3,$abcdef\n.emit 8,$12345678\n.endmodule\n"),
    (
        "encoded-data-policy",
        r#".module main
.cpu 68020
.enc petscii
.byte "A"
.word "a"
.long "AB"
.text "a", "A"
.text ""
.null ""
.ptext "ab"
.emit byte, "a"
.emit word, "AB"
.endmodule
"#,
    ),
    (
        "escapes-scalar-and-empty",
        r#".module main
.cpu 68020
.encode custom
.cdef "A", "B", 1+1
.edef "{empty}", ""
.edef "{one}", 7
.edef "{two}", 1, 2
.edef "{raw}", "az", 0
.endencode
.enc custom
.byte "{one}"
.word "{one}"
.long "{two}"
.null "{empty}"
.ptext "{empty}"
.emit word, "{two}"
.text "AB{raw}"
.endmodule
"#,
    ),
    (
        "inactive-controls-and-labels",
        r#".module dep
.cpu 68020
.pub
entry .block
.byte 7
.bend
unused .block
.enc petscii
.bend
.text "A"
.endmodule
.module main
.cpu 68020
.use dep (entry)
.if 0
.enc petscii
.encode ignored
.cdef "A", "A", 1
.endencode
.endif
first: .text "A"
.null "a"
.word first
.word dep.entry
.endmodule
"#,
    ),
    (
        "normalized-global-names",
        r#".module main
.cpu 68020
petscii .const 17
.encoding " PeTsCiI "
.byte "A"
.encode " GaMe "
.cdef "A", "A", 9
.endencode
.enc "game"
.text "A"
.byte petscii
.endmodule
"#,
    ),
    (
        "bare-encoding-names-in-macros",
        r#".module main
.cpu 68020
select .macro
.enc petscii
.endmacro
create .macro
.encode custom
.cdef "A","B",7
.endencode
.endmacro
.create
.enc custom
.text "AB"
.select
.text "A"
.endmodule
"#,
    ),
    (
        "parameterized-encoding-definitions",
        r#".module main
.cpu 68020
create .macro name,start
.encode .name
.cdef "A","B",.start
.endencode
.endmacro
select .macro name
.enc .name
.endmacro
.namespace scoped
.create "game",7
.select "game"
.text "AB"
.endnamespace
.endmodule
"#,
    ),
    (
        "encoded-expression-values",
        r#".module main
.cpu 68020
.enc petscii
saved .const "A"
.byte saved, "a"+1
.word "AB"+1
.enc ascii
.byte "A",saved
.endmodule
"#,
    ),
    (
        "encoding-names-through-macro-namespace",
        r#".module main
.cpu 68020
choose .macro
.enc " petscii "
.endmacro
.namespace scoped
petscii .const 17
.choose
.text "A"
.byte petscii
.endnamespace
.text "a"
.endmodule
"#,
    ),
];
const INTEGRATED_NEGATIVE: &[(&str, &str)] = &[
    ("unmatched-endencode", ".endencode\n"),
    ("unclosed-encode-at-module-end", ".encode custom\n.cdef \"A\",\"A\",1\n"),
    ("unclosed-encode-before-next-module", ".encode custom\n.cdef \"A\",\"A\",1\n.endmodule\n.module next\n.byte 1\n"),
    ("unknown-profile", ".enc missing\n.text \"A\"\n"),
    ("definition-outside-scope", ".cdef \"A\",\"B\",1\n"),
    ("duplicate-character", ".encode custom\n.tdef \"aa\",1\n.endencode\n"),
    ("unmapped-character", ".encode custom\n.cdef \"A\",\"A\",1\n.endencode\n.enc custom\n.text \"B\"\n"),
    ("empty-data", ".byte \"\"\n"),
    ("non-string-text", ".text 1\n"),
    ("empty-text-in-bss", ".region image,0,15,align=1\n.section data,kind=bss\n.text \"\"\n.endsection\n.place data in image\n"),
    ("null-zero-payload", ".null \"\\0\"\n"),
    ("null-zero-mapping", ".encode custom\n.cdef \"a\",\"a\",0\n.endencode\n.enc custom\n.null \"a\"\n"),
    ("oversized-string-scalar", ".emit byte,\"ABC\"\n"),
    ("replacement-byte-range", ".encode custom\n.edef \"{x}\",256\n.endencode\n"),
];

#[test]
fn native_text_integration_live_rust_controls() {
    let dir = scratch();
    for (name, source) in INTEGRATED_POSITIVE {
        let actual = oracle(&dir.0, source)
            .unwrap_or_else(|error| panic!("positive control {name}: {error}"));
        let pinned: Option<&[u8]> = match *name {
            "forward-definition-values" => Some(&[7, 8, 7, 8, 7]),
            "nested-definitions-close-in-module" => Some(&[1, 3, 2]),
            "scalar-data-and-padding" => Some(&[
                0, 255, 0x12, 0x34, 0x12, 0x34, 0x56, 0x78, 0xab, 0xcd, 0xef, 0, 0, 0, 0, 0x12,
                0x34, 0x56, 0x78,
            ]),
            "encoded-data-policy" => {
                Some(&[193, 0, 65, 193, 194, 65, 193, 0, 2, 65, 66, 65, 193, 194])
            }
            "escapes-scalar-and-empty" => Some(&[7, 0, 7, 1, 2, 0, 0, 1, 2, 2, 3, 97, 122, 0]),
            "inactive-controls-and-labels" => Some(&[7, 65, 65, 97, 0, 0, 2, 0, 0]),
            "normalized-global-names" => Some(&[193, 9, 17]),
            "encoding-names-through-macro-namespace" => Some(&[193, 17, 65]),
            "encoded-expression-values" => Some(&[193, 66, 193, 195, 65, 193]),
            "parameterized-encoding-definitions" => Some(&[7, 8]),
            "bare-encoding-names-in-macros" => Some(&[7, 8, 193]),
            _ => None,
        };
        if let Some(expected) = pinned {
            assert_eq!(actual, expected, "pinned control {name}");
        }
    }
    for (name, body) in INTEGRATED_NEGATIVE {
        let source = format!(".module main\n.cpu 68020\n{body}.endmodule\n");
        assert!(oracle(&dir.0, &source).is_err(), "negative control {name}");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; fresh compact text integration proof"]
fn native_text_integration_fs_uae() {
    use super::native_iterable_loops::{build_image, run_case};
    use super::native_source_cases::{self, Case};
    let dir = scratch();
    let root = workspace_root();
    let executable = std::env::var_os("OPFORGE_TEXT_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let selected_case = std::env::var("OPFORGE_TEXT_CASE").ok();
    let selected_cpu = std::env::var("OPFORGE_TEXT_CPU").ok();
    let mut cases = Vec::new();
    for cpu in ["68020", "6502"] {
        if selected_cpu
            .as_deref()
            .is_some_and(|selected| selected != cpu)
        {
            continue;
        }
        for (name, source) in INTEGRATED_POSITIVE {
            if selected_case
                .as_deref()
                .is_some_and(|selected| selected != *name)
            {
                continue;
            }
            let source = source.replace(".cpu 68020", &format!(".cpu {cpu}"));
            let expected = oracle(&dir.0, &source).expect("current positive text oracle");
            cases.push(Case {
                name: format!("{name}-{cpu}"),
                cpu: cpu.into(),
                files: vec![("entry.asm", source)],
                expected: Some(expected),
            });
        }
        for (name, body) in INTEGRATED_NEGATIVE {
            if selected_case
                .as_deref()
                .is_some_and(|selected| selected != *name)
            {
                continue;
            }
            let source = format!(".module main\n.cpu {cpu}\n{body}.endmodule\n");
            cases.push(Case {
                name: format!("{name}-{cpu}"),
                cpu: cpu.into(),
                files: vec![("entry.asm", source)],
                expected: None,
            });
        }
    }
    assert!(!cases.is_empty(), "text localization selected no cases");
    if std::env::var("OPFORGE_ITERABLE_MEMORY").as_deref() == Ok("1") {
        // Keep isolated diagnostic/timing runs separate from correctness batches.
        for case in cases {
            run_case(
                &root,
                &dir.0,
                &executable,
                &case.name,
                &case.cpu,
                &case.files[0].1,
                case.expected.is_none(),
            );
        }
    } else {
        native_source_cases::run(&root, &executable, &cases);
    }
}

#[test]
fn native_text_forward_definition_live_controls() {
    let dir = scratch();
    for (name, body, expected) in [
        ("cdef", ".encode custom\n.cdef \"A\",\"B\",CODE\n.endencode\n.enc custom\n.text \"AB\"\nCODE .const 7\n", &[7, 8][..]),
        ("tdef", ".encode custom\n.tdef \"AB\",CODE\n.endencode\n.enc custom\n.text \"AB\"\nCODE .const 7\n", &[7, 8][..]),
        ("edef", ".encode custom\n.edef \"{x}\",CODE\n.endencode\n.enc custom\n.text \"{x}\"\nCODE .const 7\n", &[7][..]),
    ] {
        let source = format!(".module main\n.cpu 68020\n{body}.endmodule\n");
        assert_eq!(oracle(&dir.0, &source).unwrap(), expected, "forward {name}");
    }
}
