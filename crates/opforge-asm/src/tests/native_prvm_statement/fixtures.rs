// SPDX-License-Identifier: GPL-3.0-or-later
//! Live shared PRVM oracles; caller-envelope failures are distinct from syntax.
use opcore::tokenizer::{Span, Token, TokenKind, Tokenizer};
use package::statement_program as policy;
use types::packed_statement::{self as wire, TokenView};
use vm::packed_statement_vm::{compile_views, execute_match, prepare_tokens, select_head, Plan};

use types::statement_invocation::{Route, ScopeRow};
use vm::statement_invocation::{
    decode_program, fallback_views, longest_prefix, route, select_overloads, MatchInput,
    ScopedPlans,
};
#[derive(Clone, Default)]
struct InvocationData {
    scope: Vec<ScopeRow>,
    keywords: Vec<u8>,
    plans: Vec<Plan>,
    head: Vec<u8>,
}

const BUDGET: u32 = 100_000;
const OUTPUT: u32 = 8192;
const WORKSPACE: u32 = 16384;
#[derive(Clone)]
struct Case {
    mode: u32,
    budget: u32,
    output: u32,
    workspace: u32,
    program: Vec<u8>,
    plan: Vec<u8>,
    tokens: Vec<TokenView>,
    arena: Vec<u8>,
    names: Vec<u16>,
    rejection: Option<u32>,
    invocation: InvocationData,
}
fn span() -> Span {
    Span {
        line: 1,
        col_start: 1,
        col_end: 1,
    }
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
fn compile_case(text: &str, rejection: Option<u32>) -> Case {
    let (tokens, arena) = prepare_tokens(&tokens(text));
    Case {
        mode: 1,
        budget: BUDGET,
        output: OUTPUT,
        workspace: WORKSPACE,
        program: policy::compile_program(),
        plan: vec![],
        tokens,
        arena,
        names: vec![],
        rejection,
        invocation: InvocationData::default(),
    }
}
fn match_case(signature: &str, invocation: &str) -> Case {
    let definition = compile_case(signature, None);
    let plan = compile_views(
        &definition.program,
        &definition.tokens,
        &definition.arena,
        span(),
    )
    .unwrap()
    .0
    .to_vec();
    let (tokens, arena) = prepare_tokens(&tokens(invocation));
    Case {
        mode: 2,
        plan,
        tokens,
        arena,
        program: policy::match_program(),
        ..definition
    }
}
fn head_case(text: &str, rejection: Option<u32>) -> Case {
    let input = tokens(text);
    let names = input
        .iter()
        .map(|t| match &t.kind {
            TokenKind::Identifier(s) | TokenKind::Register(s) if s == "statement" => 41,
            TokenKind::Identifier(s) | TokenKind::Register(s) if s == "endstatement" => 42,
            _ => 0,
        })
        .collect();
    let (tokens, arena) = prepare_tokens(&input);
    Case {
        mode: 3,
        budget: BUDGET,
        output: OUTPUT,
        workspace: WORKSPACE,
        program: policy::head_program([41, 42]),
        plan: vec![],
        tokens,
        arena,
        names,
        rejection,
        invocation: InvocationData::default(),
    }
}
fn lossy_case(input: &[u8]) -> Case {
    Case {
        mode: 4,
        budget: BUDGET,
        output: OUTPUT,
        workspace: WORKSPACE,
        program: vec![],
        plan: vec![],
        tokens: vec![],
        arena: input.to_vec(),
        names: vec![],
        rejection: None,
        invocation: InvocationData::default(),
    }
}
fn word(bytes: &mut Vec<u8>, value: u32) {
    bytes.extend(value.to_be_bytes());
}
fn padded(bytes: &mut Vec<u8>, payload: &[u8]) {
    bytes.extend(payload);
    if !payload.len().is_multiple_of(2) {
        bytes.push(0);
    }
}
fn result(status: u32, primary: u32, secondary: u32, tertiary: u32, payload: &[u8]) -> Vec<u8> {
    let mut out = Vec::new();
    for value in [status, primary, secondary, tertiary, payload.len() as u32] {
        word(&mut out, value);
    }
    out.extend(payload);
    out
}
impl Case {
    fn oracle(&self) -> Vec<u8> {
        let actual = match self.mode {
            1 => compile_views(&self.program, &self.tokens, &self.arena, span()).map(|plan| {
                let rows = u32::from_be_bytes(
                    plan.0[wire::ROW_COUNT..wire::ROW_COUNT + 4]
                        .try_into()
                        .unwrap(),
                );
                (plan.0.len() as u32, rows, 0, plan.0.to_vec())
            }),
            2 => execute_match(
                &self.program,
                &Plan(self.plan.clone().into()),
                &self.tokens,
                &self.arena,
                span(),
            )
            .map(|captures| {
                let Some(captures) = captures else {
                    return (0, 0, 0, vec![]);
                };
                let mut bytes = Vec::new();
                for capture in &captures {
                    bytes.extend(capture.encode());
                }
                (1, captures.len() as u32, 0, bytes)
            }),
            3 | 5 => select_head(
                &self.program,
                if self.mode == 3 {
                    wire::HEAD_CLASSIFY
                } else {
                    wire::HEAD_COMPLETE
                },
                &self.tokens,
                &self.names,
                span(),
            )
            .map(|head| {
                head.map_or((0, 0, 0, vec![]), |h| {
                    (
                        u32::from(h.action),
                        h.keyword_token,
                        h.signature_token,
                        vec![],
                    )
                })
            }),
            4 => {
                let bytes = String::from_utf8_lossy(&self.arena)
                    .into_owned()
                    .into_bytes();
                Ok((bytes.len() as u32, 0, 0, bytes))
            }
            6 => decode_program(&self.program, span())
                .and_then(|p| {
                    route(
                        &p,
                        &self.tokens,
                        &self.arena,
                        &self.invocation.scope,
                        &self.invocation.keywords,
                        span(),
                    )
                })
                .map(|route| {
                    route.map_or((0, 0, 0, vec![]), |r| {
                        (1, r.label_token, r.head_token, r.encode().to_vec())
                    })
                }),
            7 => decode_program(&self.program, span())
                .and_then(|p| fallback_views(&p, &self.tokens, &self.arena, span()))
                .map(|views| {
                    let mut bytes = Vec::new();
                    for v in &views {
                        bytes.extend(v.encode());
                    }
                    (views.len() as u32, bytes.len() as u32, 0, bytes)
                }),
            8 => decode_program(&self.program, span())
                .and_then(|p| {
                    let prefix = longest_prefix(
                        &p,
                        &self.invocation.head,
                        &self.invocation.scope,
                        &self.invocation.keywords,
                        span(),
                    )?;
                    select_overloads(
                        &p,
                        &self.program,
                        &ScopedPlans {
                            rows: &self.invocation.scope,
                            keywords: &self.invocation.keywords,
                            plans: &self.invocation.plans,
                        },
                        &MatchInput {
                            route: Route {
                                label_token: wire::NONE,
                                head_token: 0,
                                prefix_bytes: prefix,
                            },
                            head: &self.invocation.head,
                            views: &self.tokens,
                            arena: &self.arena,
                            span: span(),
                        },
                    )
                })
                .map(|(selected, fallback)| {
                    let Some(selected) = selected else {
                        return (0, 0, 0, vec![]);
                    };
                    let mut bytes = selected.encode_header().to_vec();
                    for c in &selected.captures {
                        bytes.extend(c.encode());
                    }
                    let fallback_count = if selected.used_fallback {
                        for v in &fallback {
                            bytes.extend(v.encode());
                        }
                        fallback.len() as u32
                    } else {
                        0
                    };
                    (1, selected.captures.len() as u32, fallback_count, bytes)
                }),
            _ => unreachable!(),
        };
        if let Some(status) = self.rejection {
            assert!(
                actual.is_err(),
                "expected live PRVM rejection, mode {}",
                self.mode
            );
            return result(status, 0, 0, 0, &[]);
        }
        let (primary, secondary, tertiary, payload) = actual.unwrap();
        // These are caller-frame limits, not source-language diagnostics or a
        // claim that Rust/native count intermediate steps identically.
        if self.budget == 0 {
            return result(12, 0, 0, 0, &[]);
        }
        // Selection and derived views publish into independently bounded
        // buffers; the artifact concatenates them only after successful return.
        let required_output = if self.mode == 8 && primary != 0 {
            (types::statement_invocation::SELECTION_BYTES
                + secondary as usize * wire::CAPTURE_BYTES)
                .max(tertiary as usize * wire::TOKEN_BYTES)
        } else {
            payload.len()
        };
        if required_output > self.output as usize
            || (self.workspace == 0
                && matches!(self.mode, 1 | 2 | 7 | 8)
                && (!payload.is_empty() || self.mode == 1))
        {
            return result(7, 0, 0, 0, &[]);
        }
        result(0, primary, secondary, tertiary, &payload)
    }
    fn encode(&self, out: &mut Vec<u8>) {
        for value in [
            self.mode,
            self.budget,
            self.output,
            self.workspace,
            self.program.len() as u32,
            self.plan.len() as u32,
            self.tokens.len() as u32,
            self.arena.len() as u32,
            self.names.len() as u32,
            self.invocation.scope.len() as u32,
            self.invocation.keywords.len() as u32,
            self.invocation.plans.len() as u32,
            self.invocation.head.len() as u32,
        ] {
            word(out, value);
        }
        padded(out, &self.program);
        padded(out, &self.plan);
        for token in &self.tokens {
            out.extend(token.encode());
        }
        padded(out, &self.arena);
        for name in &self.names {
            out.extend(name.to_be_bytes());
        }
        for row in &self.invocation.scope {
            out.extend(row.encode());
        }
        padded(out, &self.invocation.keywords);
        for plan in &self.invocation.plans {
            word(out, plan.0.len() as u32);
            padded(out, &plan.0);
        }
        padded(out, &self.invocation.head);
    }
}

pub(super) fn batch() -> (Vec<u8>, Vec<u8>) {
    let mut cases = Vec::new();
    for signature in [
        "",
        "byte:x",
        "word:x",
        "long:x",
        "char:x",
        "str:x",
        "\"[\"[{byte:x}]\"],y\"",
        "char:x[{byte:y}] \",\" char:z[{byte:w}]",
        "\"#\"[{long:x}]\",\"char:y[{byte:z}]",
        "[{[{byte:x}]}]",
        "[{ }]",
        "\"\"",
        "\"==\"",
        "\"é\"",
    ] {
        cases.push(compile_case(signature, None));
    }
    for signature in [
        "byte:x,word:y",
        "quad:x",
        "byte x",
        "byte:",
        "[{byte:x}",
        "[{byte:x",
        "7",
        "}",
        "byte:1",
    ] {
        cases.push(compile_case(signature, Some(5)));
    }
    for signature in ["byte:x", "word:x", "long:x", "char:x", "str:x"] {
        for invocation in [
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
            "name",
            "d",
            "'a'",
            "'ab'",
            "'abc'",
            "''",
            "7+1",
        ] {
            cases.push(match_case(signature, invocation));
        }
    }
    for (signature, invocations) in [
        (
            "\"[\"[{byte:x}]\"],y\"",
            vec!["[$05],y", "[ $05],y", "[$05 ],y", "[$05], y"],
        ),
        ("char:x[{byte:y}]", vec!["d0", "d 0"]),
        ("\"==\"", vec!["=", "=="]),
        ("[{[{char:x}]}] \"!\"", vec!["d!", "d !"]),
        ("\"\"", vec!["", "7"]),
        ("[{ }]", vec!["", "7"]),
    ] {
        for invocation in invocations {
            cases.push(match_case(signature, invocation));
        }
    }
    // Signed numeric facets are prepared once, rather than repeatedly parsed by
    // each candidate matcher. Ordinary '-1' remains two invocation tokens.
    for value in [-129i64, -128, -32769, -32768, -2147483649, -2147483648] {
        for signature in ["byte:x", "word:x", "long:x"] {
            let mut case = match_case(signature, "1");
            case.tokens[0].numeric_value = value;
            case.arena = value.to_string().into_bytes();
            case.tokens[0].literal_bytes = case.arena.len() as u32;
            case.tokens[0].col_end = case.tokens[0].col_start + case.arena.len() as u32;
            cases.push(case);
        }
    }
    for (source, status) in [
        (".statement N malformed", None),
        (".endstatement", None),
        ("label .endstatement", None),
        ("label: .endstatement", None),
        ("label .statement N", None),
        ("label: .statement N", None),
        (".ordinary N", None),
    ] {
        cases.push(head_case(source, status));
    }
    for (source, status) in [
        (".statement N malformed", None),
        (".statement", Some(5)),
        (".statement 7", Some(5)),
        ("label .statement N", Some(5)),
        ("label: .statement N", Some(5)),
        ("label .endstatement", None),
    ] {
        let mut case = head_case(source, status);
        case.mode = 5;
        cases.push(case);
    }
    for input in [
        &b""[..],
        &b"ascii"[..],
        "é😀".as_bytes(),
        &[0xff],
        &[0xe2, 0x82],
        &[0xf0, 0x28, 0x8c, 0xbc],
        &[0xc0, 0xaf],
    ] {
        cases.push(lossy_case(input));
    }
    let mut bad_program = compile_case("byte:x", Some(6));
    bad_program.program.pop();
    cases.push(bad_program);
    let mut bad_program = match_case("byte:x", "7");
    bad_program.program[0] ^= 0x80;
    bad_program.rejection = Some(6);
    cases.push(bad_program);
    let mut bad_plan = match_case("[{byte:x}]", "7");
    bad_plan.plan[wire::HEADER_BYTES + wire::PAIRED_ROW] ^= 0x80;
    bad_plan.rejection = Some(4);
    cases.push(bad_plan);
    let mut bad_span = compile_case("byte:x", Some(4));
    bad_span.tokens[0].col_end = 0;
    cases.push(bad_span);
    let mut bad_extent = match_case("byte:x", "7");
    bad_extent.tokens[0].literal_offset = u32::MAX;
    bad_extent.rejection = Some(4);
    cases.push(bad_extent);
    let mut bad_canonical = compile_case(".", Some(5));
    bad_canonical.arena = b"abc".to_vec();
    bad_canonical.tokens[0].literal_bytes = 3;
    cases.push(bad_canonical);
    let mut bad_facet = match_case("char:x", "'a'");
    bad_facet.tokens[0].decoded_bytes = 4;
    bad_facet.rejection = Some(5);
    cases.push(bad_facet);
    for mode in 1..=4 {
        let base = match mode {
            1 => compile_case("byte:x", None),
            2 => match_case("byte:x", "7"),
            3 => head_case(".statement N byte:x", None),
            4 => lossy_case(b"abc"),
            _ => unreachable!(),
        };
        let mut budget = base.clone();
        budget.budget = 0;
        cases.push(budget);
        if mode != 3 {
            let mut short = base.clone();
            short.output = 0;
            cases.push(short);
            let mut workspace = base;
            workspace.workspace = 0;
            cases.push(workspace);
        }
    }
    cases.extend(invocation_cases());
    let mut input = Vec::new();
    word(&mut input, cases.len() as u32);
    let mut expected = Vec::new();
    for case in cases {
        case.encode(&mut input);
        expected.extend(case.oracle());
    }
    (input, expected)
}

fn invocation_case(mode: u32, input: &str, head: &str, definitions: &[(&str, &str)]) -> Case {
    let mut case = match_case("byte:x", input);
    case.mode = mode;
    case.invocation.head = head.as_bytes().to_vec();
    for (index, (keyword, signature)) in definitions.iter().enumerate() {
        let definition = compile_case(signature, None);
        let plan = compile_views(
            &definition.program,
            &definition.tokens,
            &definition.arena,
            span(),
        )
        .unwrap();
        case.invocation.scope.push(ScopeRow {
            keyword_offset: case.invocation.keywords.len() as u32,
            keyword_bytes: keyword.len() as u32,
            plan_id: index as u32,
            template_id: index as u32 + 100,
        });
        case.invocation.keywords.extend(keyword.as_bytes());
        case.invocation.plans.push(plan);
    }
    case
}
fn invocation_cases() -> Vec<Case> {
    let mut out = Vec::new();
    let defs = [("N", "byte:x"), ("NX", "char:x[{byte:y}]")];
    for input in [
        " nXmore 7",
        "label nXmore 7",
        "label: nXmore 7",
        "label : nXmore 7",
        "N",
        " .N 7",
        " Unknown 7",
        " n 7",
    ] {
        out.push(invocation_case(6, input, "", &defs));
    }
    for input in [
        "d0",
        "D123",
        "d 0",
        "xy",
        "d_0",
        "d18446744073709551616",
        "d4294967295",
        "7",
        "'d0'",
        "d0,d2",
    ] {
        out.push(invocation_case(7, input, "", &[]));
    }
    for definitions in [
        vec![("N", "byte:x"), ("N", "char:x[{byte:y}]")],
        vec![("N", "char:x[{byte:y}]")],
        vec![("N", "byte:x"), ("N", "byte:y")],
        vec![("N", "byte:x"), ("N", "\"7\"")],
    ] {
        for input in [
            "d0",
            "d 0",
            "7",
            "256",
            "d4294967296",
            "d18446744073709551616",
        ] {
            let mut case = invocation_case(8, input, "N", &definitions);
            if definitions == vec![("N", "byte:x"), ("N", "byte:y")]
                && ["d0", "7", "d4294967296", "d18446744073709551616"].contains(&input)
            {
                case.rejection = Some(5);
            }
            out.push(case);
        }
    }
    for mode in 6..=8 {
        let mut base = invocation_case(
            mode,
            if mode == 6 { " N d0" } else { "d0" },
            "N",
            &[("N", "char:x[{byte:y}]")],
        );
        let mut budget = base.clone();
        budget.budget = 0;
        out.push(budget);
        let mut capacity = base.clone();
        capacity.output = 0;
        out.push(capacity);
        if mode != 6 {
            base.workspace = 0;
            out.push(base);
        }
    }
    for (signature, capacities) in [
        ("byte:x", &[31, 32][..]),
        ("char:x[{byte:y}]", &[43, 63, 64][..]),
    ] {
        for capacity in capacities {
            let mut case = invocation_case(8, "d0", "N", &[("N", signature)]);
            case.output = *capacity;
            out.push(case);
        }
    }
    out
}
