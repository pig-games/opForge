// SPDX-License-Identifier: GPL-3.0-or-later
//! PRVM-selected capture transport through shared native TKVM and packed writer.
use std::path::{Path, PathBuf};
fn fixture() -> (Vec<u8>, Vec<u8>) {
    use opcore::tokenizer::{Span, TokenKind, Tokenizer};
    use vm::packed_statement_vm::{compile, execute_match, prepare_tokens};
    let tokens = |text: &str| {
        let mut lexer = Tokenizer::new(text, 1);
        let mut result = Vec::new();
        loop {
            let token = lexer.next_token().unwrap();
            if matches!(token.kind, TokenKind::End) {
                break;
            }
            result.push(token);
        }
        result
    };
    let span = Span {
        line: 1,
        col_start: 1,
        col_end: 1,
    };
    let plan = compile(
        &package::statement_program::compile_program(),
        &tokens("byte:x str:y str:z str:e"),
        span,
    )
    .unwrap();
    let (views, arena) = prepare_tokens(&tokens("0B8H \"1+2\" \"symbol\" \"\""));
    let captures = execute_match(
        &package::statement_program::match_program(),
        &plan,
        &views,
        &arena,
        span,
    )
    .unwrap()
    .unwrap();
    assert_eq!(captures.len(), 4);
    let core = vm::runtime_model_core::RuntimeModelCore::from_registry(&super::default_registry())
        .unwrap();
    let program = core
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap()
        .program;
    let mut input = Vec::new();
    for count in [views.len(), captures.len(), arena.len(), program.len()] {
        input.extend((count as u32).to_be_bytes());
    }
    for view in views {
        input.extend(view.encode());
    }
    for capture in captures {
        input.extend(capture.encode());
    }
    input.extend(&arena);
    if arena.len() % 2 != 0 {
        input.push(0);
    }
    input.extend(program);
    // Wire oracle: normalized number, retokenized expression, numeric source
    // identity, and an empty present capture. This checks transport, not output
    // instruction semantics or connected compact-CLI statement expansion.
    let packed = [
        2, 0, 0, 0, 184, 2, 0, 0, 0, 1, 18, 2, 0, 0, 0, 2, 0, 0, 100, 0,
    ];
    let full = b"0B8H, 1+2, symbol, ";
    let mut expected = Vec::new();
    for value in [0, full.len() as u32, packed.len() as u32] {
        expected.extend(value.to_be_bytes());
    }
    for (packed, spelling) in [(5u16, 4u16), (11, 3), (4, 6), (0, 0)] {
        expected.extend(packed.to_be_bytes());
        expected.extend(spelling.to_be_bytes());
    }
    expected.extend(packed);
    expected.extend(full);
    (input, expected)
}

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}
struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}
fn assemble(input: &[u8]) -> (Scratch, Vec<u8>, String) {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let repo = root();
    let path = std::env::temp_dir().join(format!(
        "opforge-stmt-argument-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&path).unwrap();
    let scratch = Scratch(path);
    std::fs::write(scratch.0.join("stmt-argument-fixture.bin"), input).unwrap();
    let source = std::fs::read_to_string(repo.join(
        "native/motorola68000/amigaos/test-harnesses/experimental/stmt_arguments_harness.asm",
    ))
    .unwrap();
    let entry = scratch.0.join("entry.asm");
    std::fs::write(&entry, &source).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_owned(),
        entry.to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        repo.join("native/motorola68000")
            .to_string_lossy()
            .into_owned(),
        "-I".into(),
        repo.join("native/motorola68000/amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(scratch.0.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "native statement captures: {error}; diagnostics: {:?}",
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("native statement captures: {error}")
        }
        _ => panic!("native statement captures warnings"),
    });
    let image = std::fs::read(scratch.0.join("stmt-arguments.hunk")).unwrap();
    (scratch, image, source)
}
#[test]
fn native_statement_arguments_live_oracles_and_fixture_export() {
    let (input, expected) = fixture();
    assert!(!input.is_empty() && expected.len() > 32);
    let directory = root().join("target/native-stmt-argument");
    std::fs::create_dir_all(&directory).unwrap();
    std::fs::write(directory.join("stmt-argument-fixture.bin"), input).unwrap();
    std::fs::write(directory.join("stmt-argument-expected.bin"), expected).unwrap();
    eprintln!("STATEMENT_ARGUMENTS captures=4 empty-present=true decoded-expression=true");
}
#[test]
fn native_statement_arguments_host_assembles() {
    let (input, _) = fixture();
    assert!(!assemble(&input).1.is_empty());
}
#[test]
#[ignore = "requires configured FS-UAE; fresh owned-token statement adapter proof"]
fn native_statement_arguments_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let (input, expected) = fixture();
    let (_scratch, image, source) = assemble(&input);
    let case = OpforgeNativeCliParityCase {
        name: "stmt-argument-owned-token-adapter",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/stmt-argument-results.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root(), &case, &image)
        .unwrap_or_else(|error| panic!("fresh statement capture adapter proof: {error}"));
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
    eprintln!(
        "STATEMENT_ARGUMENTS inputs={} exact=true capture-transport-only=true seconds={:?}",
        u32::from_be_bytes(input[..4].try_into().unwrap()),
        runs[0].start_to_done_host_seconds
    );
}
