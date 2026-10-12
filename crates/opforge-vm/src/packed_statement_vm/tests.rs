// SPDX-License-Identifier: GPL-3.0-or-later
use super::*;
use opcore::tokenizer::{Token, TokenKind};
fn span(col_start: usize, col_end: usize) -> Span {
    Span {
        line: 1,
        col_start,
        col_end,
    }
}
#[test]
fn parse_statement_signature_from_tokens_parses_boundary_capture_sequence() {
    let tokens = vec![
        Token {
            kind: TokenKind::Identifier("byte".to_string()),
            span: span(1, 4),
        },
        Token {
            kind: TokenKind::Colon,
            span: span(5, 5),
        },
        Token {
            kind: TokenKind::Identifier("lhs".to_string()),
            span: span(6, 8),
        },
        Token {
            kind: TokenKind::String(opcore::tokenizer::StringLiteral {
                raw: ",".to_string(),
                bytes: vec![b','],
            }),
            span: span(10, 12),
        },
        Token {
            kind: TokenKind::OpenBracket,
            span: span(14, 14),
        },
        Token {
            kind: TokenKind::OpenBrace,
            span: span(15, 15),
        },
        Token {
            kind: TokenKind::Identifier("word".to_string()),
            span: span(16, 19),
        },
        Token {
            kind: TokenKind::Colon,
            span: span(20, 20),
        },
        Token {
            kind: TokenKind::Identifier("rhs".to_string()),
            span: span(21, 23),
        },
        Token {
            kind: TokenKind::CloseBrace,
            span: span(24, 24),
        },
        Token {
            kind: TokenKind::CloseBracket,
            span: span(25, 25),
        },
    ];
    let mut cursor = 0;
    let signature =
        parse_statement_signature_from_tokens(&tokens, &mut cursor, false, span(25, 25))
            .expect("statement signature should parse");

    assert_eq!(signature.atoms().len(), 3);
    assert!(matches!(
        signature.atoms()[0],
        SignatureAtom::Capture { .. }
    ));
    assert!(matches!(signature.atoms()[1], SignatureAtom::Literal(_, _)));
    assert!(matches!(
        signature.atoms()[2],
        SignatureAtom::Boundary { .. }
    ));
}

#[test]
fn parse_statement_definition_from_line_parses_keyword_and_signature() {
    let def =
        parse_statement_definition_from_line(".statement LDA [{ byte:value }] \",\" \"#\"", 1)
            .expect("statement definition should parse");

    assert_eq!(def.keyword, "LDA");
    assert_eq!(def.signature.atoms().len(), 3);
}

#[test]
fn parse_statement_definition_from_line_accepts_long_capture_type() {
    let def = parse_statement_definition_from_line(".statement ADDI \"#\" [{ long:value }]", 1)
        .expect("statement definition with long capture should parse");

    assert_eq!(def.keyword, "ADDI");
    assert_eq!(def.signature.atoms().len(), 2);
    assert!(matches!(
        def.signature.atoms()[1],
        SignatureAtom::Boundary { .. }
    ));
}

#[test]
fn parse_statement_definition_from_line_rejects_unknown_capture_type() {
    let err = parse_statement_definition_from_line(".statement MOVE [{ quad:value }]", 1)
        .expect_err("unknown capture type should fail");

    assert_eq!(err.message, "Unknown statement capture type: quad");
}

fn tokens(text: &str) -> Vec<Token> {
    let mut tokenizer = Tokenizer::new(text, 1);
    let mut out = Vec::new();
    loop {
        let token = tokenizer.next_token().unwrap();
        if matches!(token.kind, TokenKind::End) {
            return out;
        }
        out.push(token);
    }
}
fn oracle_signature(text: &str) -> Result<StatementSignature, ParseError> {
    let mut parser = opcore::parser::Parser::from_line(text, 1)?;
    match parser.parse_compat_mixed_line()? {
        opcore::parser::LineAst::StatementDef(def) => Ok(def.signature),
        _ => panic!("expected statement definition"),
    }
}

#[test]
fn prvm_statement_compilation_matches_independent_core_grammar() {
    for signature in [
        ".statement MOVE char:dst \",\" char:src",
        ".statement LDA \"[\"[{byte:value}]\"],y\"",
        ".statement N [{[{word:x}]}]",
        ".statement N \"\"",
        ".statement N [{ }]",
        ".statement N byte:x,word:y",
        ".statement N quad:x",
        ".statement N byte x",
        ".statement N byte:",
        ".statement N [{byte:x}",
        ".statement N [{byte:x",
        ".statement N 7",
    ] {
        let actual = parse_statement_definition_from_line(signature, 1).map(|d| d.signature);
        let expected = oracle_signature(signature);
        match (actual, expected) {
            (Ok(actual), Ok(expected)) => {
                assert_eq!(actual, expected, "{signature}");
                assert!(actual.prepared_plan().is_some());
            }
            (Err(actual), Err(expected)) => {
                assert_eq!(actual.message, expected.message, "{signature}");
                assert_eq!(actual.span, expected.span, "{signature}");
            }
            other => panic!("{signature}: {other:?}"),
        }
    }
}

#[test]
fn prvm_statement_matching_matches_independent_core_oracle() {
    let groups = [
        vec!["byte:x"],
        vec!["word:x"],
        vec!["long:x"],
        vec!["char:x"],
        vec!["str:x"],
        vec!["\"[\"[{byte:x}]\"],y\""],
        vec!["char:x[{byte:y}]"],
        vec!["[{[{char:x}]}] \"!\""],
        vec!["byte:x", "word:x"],
        vec!["byte:x", "\"7\""],
        vec!["\"\""],
        vec!["[{ }]"],
    ];
    for group in groups {
        let lines: Vec<_> = group.iter().map(|s| format!(".statement N {s}")).collect();
        let actual: Vec<_> = lines
            .iter()
            .map(|s| {
                parse_statement_definition_from_line(s, 1)
                    .unwrap()
                    .signature
            })
            .collect();
        let expected: Vec<_> = lines.iter().map(|s| oracle_signature(s).unwrap()).collect();
        for input in [
            "",
            "7",
            "255",
            "256",
            "65535",
            "65536",
            "4294967295",
            "4294967296",
            "$ff",
            "0ffh",
            "%1010",
            "0B8H",
            "0AB",
            "'a'",
            "'ab'",
            "'abc'",
            "''",
            "name",
            "d",
            "d0",
            "d 0",
            "[$05],y",
            "[ $05],y",
            "[$05 ],y",
            "[$05], y",
            "d!",
            "d !",
            "7+1",
        ] {
            let input_tokens = tokens(input);
            let got = select_and_match_statement_signature_texts(&actual, &input_tokens);
            let want = opcore::parser::select_and_match_statement_signature_texts(
                &expected,
                &input_tokens,
            );
            assert_eq!(
                format!("{got:?}"),
                format!("{want:?}"),
                "{group:?}: {input}"
            );
        }
    }
}

#[test]
fn prvm_statement_plan_cache_shares_clones_and_invalidates_on_replacement() {
    let mut signature = parse_statement_definition_from_line(".statement N byte:x", 1)
        .unwrap()
        .signature;
    let cloned = signature.clone();
    assert!(std::sync::Arc::ptr_eq(
        signature.prepared_plan().unwrap(),
        cloned.prepared_plan().unwrap()
    ));
    signature.replace_atoms(vec![]);
    assert!(signature.prepared_plan().is_none());
    assert!(cloned.prepared_plan().is_some());
    assert_eq!(signature, StatementSignature::new(vec![]));
    assert_eq!(
        format!("{cloned:?}"),
        format!("{:?}", StatementSignature::new(cloned.atoms().to_vec()))
    );
}

#[test]
fn prvm_statement_plan_and_program_corruption_reject_before_matching() {
    let signature = parse_statement_definition_from_line(".statement N [{byte:x}]", 1)
        .unwrap()
        .signature;
    let plan = lower_signature(&signature);
    let (views, arena) = prepare_tokens(&tokens("7"));
    let program = statement_program::match_program();
    for offset in [
        wire::EXTENT,
        wire::ROW_COUNT,
        wire::LITERAL_SCORE,
        wire::ATOM_SCORE,
        wire::HEADER_BYTES + wire::KIND,
        wire::HEADER_BYTES + wire::PAIRED_ROW,
        wire::HEADER_BYTES + 2,
    ] {
        let mut bytes = plan.0.to_vec();
        bytes[offset] ^= 0x80;
        assert!(
            execute_match(&program, &Plan(bytes.into()), &views, &arena, span(0, 0)).is_err(),
            "offset {offset}"
        );
    }
    for length in 0..program.len() {
        assert!(execute_match(&program[..length], &plan, &views, &arena, span(0, 0)).is_err());
    }
}

#[test]
fn prvm_statement_numeric_compiler_facets_match_core_token_adapter() {
    for signature in [
        "byte:x",
        "\"[\"[{byte:x}]\"],y\"",
        "char:x[{byte:y}]",
        "[{[{word:x}]}]",
        "\"\"",
        "[{ }]",
        "byte:x,word:y",
        "quad:x",
    ] {
        let input = tokens(signature);
        let (views, arena) = prepare_tokens(&input);
        let expected = compile(
            &statement_program::compile_program(),
            &input,
            span(1, signature.len() + 1),
        );
        let actual = compile_views(
            &statement_program::compile_program(),
            &views,
            &arena,
            span(1, signature.len() + 1),
        );
        assert_eq!(
            format!("{actual:?}"),
            format!("{expected:?}"),
            "{signature}"
        );
    }
}

#[test]
fn prvm_statement_head_selection_is_numeric_and_keeps_signature_unvalidated() {
    let program = statement_program::head_program([41, 42]);
    for (source, ids, action, keyword, signature) in [
        (".statement N malformed", vec![0, 41, 0, 0], 1, 2, 3),
        (".endstatement", vec![0, 42], 2, 2, 3),
        ("label .endstatement", vec![0, 0, 42], 2, 3, 4),
        ("label: .endstatement", vec![0, 0, 0, 42], 2, 4, 5),
    ] {
        let (views, _) = prepare_tokens(&tokens(source));
        assert_eq!(
            select_head(&program, wire::HEAD_COMPLETE, &views, &ids, span(1, 1)).unwrap(),
            Some(wire::Head {
                action,
                keyword_token: keyword,
                signature_token: signature
            })
        );
    }
    for source in ["label .statement N byte:x", "label: .statement N byte:x"] {
        let input = tokens(source);
        let ids: Vec<_> = input
            .iter()
            .map(|t| {
                u16::from(matches!(&t.kind, TokenKind::Identifier(s) if s == "statement")) * 41
            })
            .collect();
        let (views, _) = prepare_tokens(&input);
        assert_eq!(
            select_head(&program, wire::HEAD_COMPLETE, &views, &ids, span(1, 1))
                .unwrap_err()
                .message,
            "Expected .statement definition"
        );
    }
}

#[test]
fn prvm_statement_canonical_facets_reject_contradictions_and_normalize_equal() {
    let mut views_and_arena = prepare_tokens(&tokens("."));
    views_and_arena.0[0].literal_bytes = 3;
    assert_eq!(
        compile_views(
            &statement_program::compile_program(),
            &views_and_arena.0,
            b"abc",
            span(1, 2)
        )
        .unwrap_err()
        .message,
        "Invalid statement canonical token facet"
    );
    let (views, arena) = prepare_tokens(&tokens("="));
    assert_eq!(views[0].kind, 34);
    assert_eq!(arena, b"==");
    let literal = parse_statement_definition_from_line(".statement N \"==\"", 1)
        .unwrap()
        .signature;
    assert!(
        select_and_match_statement_signature_texts(&[literal], &tokens("="))
            .unwrap()
            .is_some()
    );
}

#[test]
fn prvm_statement_lossy_literals_keep_original_decoded_capture_width() {
    let input = [Token {
        kind: TokenKind::String(opcore::tokenizer::StringLiteral {
            raw: "'invalid'".into(),
            bytes: vec![0xff],
        }),
        span: span(1, 4),
    }];
    let (views, arena) = prepare_tokens(&input);
    assert_eq!(views[0].decoded_bytes, 1);
    assert_eq!(arena, "\u{fffd}".as_bytes());
    let expected = compile(&statement_program::compile_program(), &input, span(1, 4)).unwrap();
    assert_eq!(
        compile_views(
            &statement_program::compile_program(),
            &views,
            &arena,
            span(1, 4)
        )
        .unwrap(),
        expected
    );
    let signature = parse_statement_definition_from_line(".statement N char:x", 1)
        .unwrap()
        .signature;
    let plan = lower_signature(&signature);
    assert!(execute_match(
        &statement_program::match_program(),
        &plan,
        &views,
        &arena,
        span(1, 4)
    )
    .unwrap()
    .is_some());
    let mut invalid = views;
    invalid[0].decoded_bytes = 4;
    assert!(execute_match(
        &statement_program::match_program(),
        &plan,
        &invalid,
        &arena,
        span(1, 4)
    )
    .is_err());
}

#[test]
fn prvm_statement_compiler_capture_ids_come_from_program_rows() {
    let mut program = statement_program::compile_program();
    program[9] = wire::WORD;
    let second = 9 + 2 + "byte".len();
    program[second] = wire::BYTE;
    let plan = compile(&program, &tokens("byte:x"), span(1, 7)).unwrap();
    assert_eq!(plan.0[wire::HEADER_BYTES + wire::CAPTURE_TYPE], wire::WORD);
    let (views, arena) = prepare_tokens(&tokens("256"));
    assert!(execute_match(
        &statement_program::match_program(),
        &plan,
        &views,
        &arena,
        span(1, 3)
    )
    .unwrap()
    .is_some());
}

#[test]
fn prvm_statement_unknown_wire_kind_rejects_even_matching_literal_bytes() {
    let signature = parse_statement_definition_from_line(".statement N \"7\"", 1)
        .unwrap()
        .signature;
    let plan = lower_signature(&signature);
    let (mut views, arena) = prepare_tokens(&tokens("7"));
    views[0].kind = 41;
    views[0].numeric_status = 0;
    views[0].numeric_value = 0;
    assert!(execute_match(
        &statement_program::match_program(),
        &plan,
        &views,
        &arena,
        span(1, 1)
    )
    .is_err());
    assert!(compile_views(
        &statement_program::compile_program(),
        &views,
        &arena,
        span(1, 1)
    )
    .is_err());
}
