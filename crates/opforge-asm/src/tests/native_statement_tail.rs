// SPDX-License-Identifier: GPL-3.0-or-later
//! Owned PRVM argument-view transport: residual TKVM plus unchanged tail facets.
use std::path::{Path, PathBuf};
fn fixture() -> (Vec<u8>, Vec<u8>) {
    use opcore::tokenizer::{TokenKind, Tokenizer};
    use vm::packed_statement_vm::prepare_tokens;
    let lex = |text: &str| {
        let mut lexer = Tokenizer::new(text, 1);
        let mut tokens = Vec::new();
        loop {
            let t = lexer.next_token().unwrap();
            if matches!(t.kind, TokenKind::End) {
                break;
            }
            tokens.push(t);
        }
        tokens
    };
    let registry = engine::build_default_asm_registry();
    let core = vm::runtime_model_core::RuntimeModelCore::from_registry(&registry).unwrap();
    let resolved = core.resolve_pipeline("68020", None).unwrap();
    let package = crate::native_runtime_package::prepare_package(&core, &resolved).unwrap();
    let mut cases = vec![
        ("    N7 \"1+2\"".to_owned(), 0usize, 1usize),
        ("    N0B8H \"\"".to_owned(), 0, 1),
        ("    NXalpha \"two words\"".to_owned(), 0, 2),
        ("here: Nfoo 8".to_owned(), 2, 1),
        ("    N \"not code ; .byte\"".to_owned(), 0, 1),
        ("    N".to_owned(), 0, 1),
    ];
    cases.push((format!("    N7 {}", vec!["1"; 63].join(" ")), 0, 1));
    cases.push((format!("    Nfoo.bar {}", vec!["1"; 63].join(" ")), 0, 4));
    let mut input = Vec::new();
    let mut expected = Vec::new();
    input.extend((cases.len() as u32).to_be_bytes());
    input.extend((package.len() as u32).to_be_bytes());
    input.extend(package);
    while input.len() % 4 != 0 {
        input.push(0)
    }
    for (source, head, prefix) in cases {
        let original = lex(&source);
        let (views, arena) = prepare_tokens(&original);
        let spelling = match &original[head].kind {
            TokenKind::Identifier(s) => s,
            _ => panic!("head"),
        };
        let span = opcore::tokenizer::Span {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        let policy = vm::statement_invocation::decode_program(
            &package::statement_program::match_program(),
            span,
        )
        .unwrap();
        let scope = [types::statement_invocation::ScopeRow {
            keyword_offset: 0,
            keyword_bytes: prefix as u32,
            plan_id: 0,
            template_id: 0,
        }];
        let routed = vm::statement_invocation::route(
            &policy,
            &views,
            &arena,
            &scope,
            &spelling.as_bytes()[..prefix],
            span,
        )
        .unwrap()
        .unwrap();
        assert_eq!(routed.head_token, head as u32);
        assert_eq!(routed.prefix_bytes, prefix as u32);
        let suffix = lex(&spelling[prefix..]);
        let (mut args, suffix_arena) = prepare_tokens(&suffix);
        let origin = original[head].span.col_start + prefix - 1;
        let mut tail = original[head + 1..].to_vec();
        for token in &mut tail {
            token.span.col_start -= origin;
            token.span.col_end -= origin;
        }
        // Prepare tail separately only to obtain translated columns; preserve
        // original lexical payload offsets and all original semantic facets.
        let (tail_columns, _) = prepare_tokens(&tail);
        for view in &mut args {
            view.literal_offset += arena.len() as u32;
        }
        for (old, columns) in views[head + 1..].iter().zip(tail_columns) {
            let mut view = *old;
            view.col_start = columns.col_start;
            view.col_end = columns.col_end;
            args.push(view);
        }
        for n in [views.len(), arena.len(), head, prefix] {
            input.extend((n as u32).to_be_bytes());
        }
        for view in views {
            input.extend(view.encode());
        }
        input.extend(&arena);
        while input.len() % 4 != 0 {
            input.push(0)
        }
        if args.len() > 64 {
            for n in [7u32, 0, 0] {
                expected.extend(n.to_be_bytes());
            }
            continue;
        }
        for n in [0, args.len(), arena.len() + suffix_arena.len()] {
            expected.extend((n as u32).to_be_bytes());
        }
        for view in args {
            expected.extend(view.encode());
        }
        expected.extend(arena);
        expected.extend(suffix_arena);
    }
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
        "opforge-stmt-tail-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&path).unwrap();
    let scratch = Scratch(path);
    std::fs::write(scratch.0.join("stmt-tail-fixture.bin"), input).unwrap();
    let source = std::fs::read_to_string(
        repo.join("native/motorola68000/amigaos/tests/assembler/statement_tail_harness.asm"),
    )
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
        repo.join("native/motorola68000/amigaos")
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
            "native statement tail: {error}; diagnostics: {:?}",
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => panic!("native statement tail: {error}"),
        _ => panic!("native statement tail warnings"),
    });
    let image = std::fs::read(scratch.0.join("stmt-tail.hunk")).unwrap();
    (scratch, image, source)
}
#[test]
fn native_statement_tail_live_oracles_and_fixture_export() {
    let (input, expected) = fixture();
    assert!(!input.is_empty() && expected.len() > 32);
    let directory = root().join("target/native-stmt-tail");
    std::fs::create_dir_all(&directory).unwrap();
    std::fs::write(directory.join("stmt-tail-fixture.bin"), input).unwrap();
    std::fs::write(directory.join("stmt-tail-expected.bin"), expected).unwrap();
    eprintln!("STATEMENT_TAIL cases=8 decoded-str-preserved=true residual-lexing=true");
}
#[test]
fn native_statement_tail_host_assembles() {
    let (input, _) = fixture();
    assert!(!assemble(&input).1.is_empty());
}
#[test]
#[ignore = "requires configured FS-UAE; fresh owned-token statement adapter proof"]
fn native_statement_tail_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let (input, expected) = fixture();
    let (_scratch, image, source) = assemble(&input);
    let case = OpforgeNativeCliParityCase {
        name: "stmt-tail-owned-token-adapter",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/stmt-tail-results.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root(), &case, &image)
        .unwrap_or_else(|error| panic!("fresh statement tail proof: {error}"));
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
    eprintln!(
        "STATEMENT_TAIL inputs={} exact=true argument-views-only=true seconds={:?}",
        u32::from_be_bytes(input[..4].try_into().unwrap()),
        runs[0].start_to_done_host_seconds
    );
}
