// SPDX-License-Identifier: GPL-3.0-or-later

use super::*;
use crate::packed_statement_vm::{compile, prepare_tokens};
use opcore::tokenizer::{Token, TokenKind, Tokenizer};
fn span() -> Span {
    Span {
        line: 1,
        col_start: 1,
        col_end: 1,
    }
}
fn policy() -> Policy {
    Policy::decode(&[0xa9, 7, b'A', b'Z', b'a', b'z', b'0', b'9'], span()).unwrap()
}
#[test]
fn current_match_program_requires_complete_invocation_policy() {
    let program = package::statement_program::match_program();
    assert_eq!(program.len(), 133);
    decode_program(&program, span()).unwrap();
    let mut superseded = program.clone();
    superseded.drain(123..131);
    assert!(decode_program(&superseded, span()).is_err());
    for offset in [123, 124, 125, 127, 129, 131, 132] {
        let mut malformed = program.clone();
        malformed[offset] = 255;
        assert!(
            decode_program(&malformed, span()).is_err(),
            "offset {offset}"
        );
    }
}
fn tokens(source: &str) -> Vec<Token> {
    let mut lexer = Tokenizer::new(source, 1);
    let mut out = Vec::new();
    loop {
        let token = lexer.next_token().unwrap();
        if matches!(token.kind, TokenKind::End) {
            return out;
        }
        out.push(token);
    }
}
fn plan(signature: &str) -> Plan {
    compile(
        &package::statement_program::compile_program(),
        &tokens(signature),
        span(),
    )
    .unwrap()
}
fn scope(ids: &[u32]) -> Vec<ScopeRow> {
    ids.iter()
        .map(|id| ScopeRow {
            keyword_offset: 0,
            keyword_bytes: 1,
            plan_id: *id,
            template_id: *id + 100,
        })
        .collect()
}
#[test]
fn numeric_route_preserves_column_one_label_and_longest_prefix() {
    let rows = [
        ScopeRow {
            keyword_offset: 0,
            keyword_bytes: 1,
            plan_id: 0,
            template_id: 0,
        },
        ScopeRow {
            keyword_offset: 1,
            keyword_bytes: 2,
            plan_id: 1,
            template_id: 1,
        },
    ];
    for (source, label, head) in [
        (" nXmore 7", NONE, 0),
        ("label nXmore 7", 0, 1),
        ("label: nXmore 7", 0, 2),
    ] {
        let (views, arena) = prepare_tokens(&tokens(source));
        assert_eq!(
            route(&policy(), &views, &arena, &rows, b"NNX", span()).unwrap(),
            Some(Route {
                label_token: label,
                head_token: head,
                prefix_bytes: 2
            })
        );
    }
    let (views, arena) = prepare_tokens(&tokens("N"));
    assert!(route(&policy(), &views, &arena, &rows, b"NNX", span())
        .unwrap()
        .is_none());
    let (views, arena) = prepare_tokens(&tokens(" .N 7"));
    assert!(route(&policy(), &views, &arena, &rows, b"NNX", span())
        .unwrap()
        .is_none());
    let (views, arena) = prepare_tokens(&tokens("label : N 7"));
    assert!(route(&policy(), &views, &arena, &rows, b"NNX", span())
        .unwrap()
        .is_none());
}
#[test]
fn normal_match_precedes_fallback_and_ambiguity_is_terminal() {
    let plans = [plan("byte:x"), plan("char:x[{byte:y}]")];
    let (views, arena) = prepare_tokens(&tokens("d0"));
    let route = Route {
        label_token: NONE,
        head_token: 0,
        prefix_bytes: 1,
    };
    let program = package::statement_program::match_program();
    let (selected, fallback) = select_overloads(
        &policy(),
        &program,
        &ScopedPlans {
            rows: &scope(&[0, 1]),
            keywords: b"N",
            plans: &plans,
        },
        &MatchInput {
            route,
            head: b"N",
            views: &views,
            arena: &arena,
            span: span(),
        },
    )
    .unwrap();
    assert_eq!(selected.unwrap().plan_id, 0);
    assert!(fallback.is_empty());
    let (selected, fallback) = select_overloads(
        &policy(),
        &program,
        &ScopedPlans {
            rows: &scope(&[1]),
            keywords: b"N",
            plans: &plans,
        },
        &MatchInput {
            route,
            head: b"N",
            views: &views,
            arena: &arena,
            span: span(),
        },
    )
    .unwrap();
    assert!(selected.unwrap().used_fallback);
    assert_eq!(fallback.len(), 2);
    assert_eq!(fallback[0].col_end, fallback[1].col_start);
    assert_eq!(fallback[1].numeric_value, 0);
    assert_eq!(
        select_overloads(
            &policy(),
            &program,
            &ScopedPlans {
                rows: &scope(&[0, 0]),
                keywords: b"N",
                plans: &plans
            },
            &MatchInput {
                route,
                head: b"N",
                views: &views,
                arena: &arena,
                span: span()
            }
        )
        .unwrap_err()
        .message,
        "Ambiguous statement signature"
    );
}
#[test]
fn fallback_preserves_register_kind_spelling_and_numeric_overflow() {
    let mut input = tokens("D18446744073709551616");
    let TokenKind::Identifier(name) = input[0].kind.clone() else {
        panic!()
    };
    input[0].kind = TokenKind::Register(name);
    let (views, arena) = prepare_tokens(&input);
    let fallback = fallback_views(&policy(), &views, &arena, span()).unwrap();
    assert_eq!(fallback[0].kind, 1);
    assert_eq!(&arena[..1], b"D");
    assert_eq!(fallback[1].kind, 2);
    assert_eq!(fallback[1].numeric_status, 0);
    assert_eq!(fallback[1].numeric_value, 0);
}
