// SPDX-License-Identifier: GPL-3.0-or-later
//! Owned-token transport controls. Numeric metadata is supplied explicitly at
//! the capture contract; these cases do not claim full TKVM scanner coverage.
use opcore::tokenizer::{NumberLiteral, Span, StringLiteral, Token, TokenKind, Tokenizer};
use vm::{packed_statement_vm::prepare_tokens, runtime_model_core::RuntimeModelCore};

const SENTINEL: u32 = 0xa5a5a5a5;
const VIEWS: u32 = 64 * 32;
const NAMES: u32 = 64 * 2;
const ARENA: u32 = 8192;
const WORKSPACE: u32 = 16384;
#[derive(Clone, Copy)]
enum Number {
    Valid(u64),
    Malformed,
    Overflow,
}
#[derive(Clone)]
struct Case {
    tokens: Vec<Token>,
    records: Vec<u8>,
    lexemes: Vec<u8>,
    ids: Vec<u16>,
    fault: u32,
    budget: u32,
    views: u32,
    names: u32,
    arena: u32,
    workspace: u32,
    token_bytes: u32,
    ids_bytes: u32,
    rejection: Option<u32>,
}
fn word(out: &mut Vec<u8>, value: u32) {
    out.extend(value.to_be_bytes());
}
fn padded(out: &mut Vec<u8>, bytes: &[u8]) {
    out.extend(bytes);
    if !bytes.len().is_multiple_of(2) {
        out.push(0);
    }
}
fn tokenize(source: &str) -> Vec<Token> {
    let mut lexer = Tokenizer::new(source, 1);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token().unwrap();
        if matches!(token.kind, TokenKind::End) {
            return tokens;
        }
        tokens.push(token);
    }
}
fn token(kind: TokenKind) -> Token {
    Token {
        kind,
        span: Span {
            line: 1,
            col_start: 1,
            col_end: 9,
        },
    }
}
fn number(spelling: &str, metadata: Number) -> Case {
    Case::new(
        vec![token(TokenKind::Number(NumberLiteral {
            text: spelling.into(),
            base: 10,
        }))],
        Some(metadata),
    )
}
fn string(bytes: &[u8]) -> Case {
    Case::new(
        vec![token(TokenKind::String(StringLiteral {
            raw: String::from_utf8_lossy(bytes).into_owned(),
            bytes: bytes.to_vec(),
        }))],
        None,
    )
}
impl Case {
    fn new(tokens: Vec<Token>, numeric: Option<Number>) -> Self {
        let (views, _) = prepare_tokens(&tokens);
        let mut records = Vec::new();
        let mut lexemes = Vec::new();
        let mut ids = Vec::new();
        for (index, (token, view)) in tokens.iter().zip(&views).enumerate() {
            let metadata = match &token.kind {
                TokenKind::Number(n) => Some(numeric.unwrap_or_else(|| {
                    opcore::expr::parse_number(&n.text)
                        .map(|n| Number::Valid(u64::try_from(n).unwrap()))
                        .unwrap_or(Number::Malformed)
                })),
                _ => None,
            };
            let literal = match &token.kind {
                TokenKind::String(s) => s.bytes.clone(),
                TokenKind::Identifier(s) | TokenKind::Register(s) => s.as_bytes().to_vec(),
                TokenKind::Number(n) => n.text.as_bytes().to_vec(),
                _ => vec![], // Canonical punctuation comes from package render rows.
            };
            let offset = lexemes.len() as u32;
            lexemes.extend(&literal);
            let reserved = match metadata {
                Some(Number::Valid(value)) => {
                    lexemes.extend(value.to_be_bytes());
                    1u16
                }
                Some(Number::Malformed) => 2,
                Some(Number::Overflow) => 3,
                None => 0,
            };
            records.extend(u16::from(view.kind).to_be_bytes());
            records.extend(reserved.to_be_bytes());
            for value in [
                token.span.col_start as u32,
                token.span.col_end as u32,
                offset,
                literal.len() as u32,
            ] {
                word(&mut records, value);
            }
            ids.push(if view.kind <= 1 {
                200 + index as u16
            } else {
                u16::MAX
            });
        }
        Self {
            token_bytes: records.len() as u32,
            ids_bytes: (ids.len() * 2) as u32,
            tokens,
            records,
            lexemes,
            ids,
            fault: 0,
            budget: 100_000,
            views: VIEWS,
            names: NAMES,
            arena: ARENA,
            workspace: WORKSPACE,
            rejection: None,
        }
    }
    fn rejected(mut self, status: u32) -> Self {
        self.rejection = Some(status);
        self
    }
    fn oracle(&self) -> Vec<u8> {
        let (views, arena) = prepare_tokens(&self.tokens);
        let mut out = Vec::new();
        if let Some(status) = self.rejection {
            for value in [status, SENTINEL, SENTINEL] {
                word(&mut out, value);
            }
            return out;
        }
        for value in [0, views.len() as u32, arena.len() as u32] {
            word(&mut out, value);
        }
        for view in &views {
            out.extend(view.encode());
        }
        for (view, id) in views.iter().zip(&self.ids) {
            out.extend(if view.kind <= 1 { *id } else { u16::MAX }.to_be_bytes());
        }
        out.extend(arena);
        out
    }
    fn encode(&self, out: &mut Vec<u8>) {
        for value in [
            self.fault,
            self.budget,
            self.views,
            self.names,
            self.arena,
            self.workspace,
            self.tokens.len() as u32,
            self.token_bytes,
            self.lexemes.len() as u32,
            self.ids_bytes,
        ] {
            word(out, value);
        }
        out.extend(&self.records);
        padded(out, &self.lexemes);
        for id in &self.ids {
            out.extend(id.to_be_bytes());
        }
    }
}
pub(super) fn batch() -> (Vec<u8>, Vec<u8>) {
    let mut cases = vec![Case::new(vec![], None)];
    for source in [
        "alpha",
        ".statement LOAD byte:x",
        "=",
        "==",
        "alpha[{7}],beta",
    ] {
        cases.push(Case::new(tokenize(source), None));
    }
    cases.push(Case::new(
        vec![token(TokenKind::Register("neutral_name".into()))],
        None,
    ));
    let mut ids = Case::new(tokenize("alpha beta"), None);
    ids.ids = vec![1, 65534];
    cases.push(ids);
    for (_, literal) in package::statement_program::TOKEN_RENDER_SPELLINGS {
        cases.push(Case::new(tokenize(literal), None));
    }
    for (spelling, value) in [
        ("0", 0),
        ("$ff", 255),
        ("%1010", 10),
        ("0B8H", 184),
        ("9B", 9),
        ("101B", 5),
        ("1_2_3", 123),
        ("0o77", 63),
        ("77Q", 63),
        ("55D", 55),
        ("$7FFFFFFFFFFFFFFF", i64::MAX as u64),
        ("$8000000000000000", 1u64 << 63),
        ("$FFFFFFFFFFFFFFFF", u64::MAX),
    ] {
        cases.push(number(spelling, Number::Valid(value)));
    }
    cases.push(number("18446744073709551616", Number::Overflow));
    cases.push(number("12foo", Number::Malformed));
    cases.push(Case::new(tokenize("-1"), None));
    for bytes in [
        &b""[..],
        &b"ascii"[..],
        "é😀".as_bytes(),
        &[0xff],
        &[0xe2, 0x82],
        &[0xe2, 0x28, 0xa1],
        &[0xf0, 0x90, 0x80],
        &[0xed, 0xa0, 0x80],
        &[0, b'a'],
    ] {
        cases.push(string(bytes));
    }
    let mut exact = string(&[0xff]);
    exact.views = 32;
    exact.names = 2;
    exact.arena = 3;
    exact.workspace = 34 + exact.arena;
    cases.push(exact.clone());
    exact.arena = 2;
    cases.push(exact.rejected(7));
    let baseline = Case::new(tokenize("alpha"), None);
    for variant in 0..5 {
        let mut case = baseline.clone();
        match variant {
            0 => case.views = 31,
            1 => case.names = 1,
            2 => case.arena = 4,
            3 => case.workspace = 34 + ARENA - 1,
            _ => case.workspace = 0,
        }
        cases.push(case.rejected(7));
    }
    for fault in 1..=5 {
        let mut case = baseline.clone();
        case.fault = fault;
        cases.push(case.rejected(4));
    }
    for budget in [0, 1] {
        let mut case = baseline.clone();
        case.budget = budget;
        cases.push(case.rejected(12));
    }
    let mut negative_budget = baseline.clone();
    negative_budget.budget = u32::MAX;
    cases.push(negative_budget.rejected(4));
    for invalid in 0..6 {
        let mut case = baseline.clone();
        match invalid {
            0 => case.token_bytes = 19,
            1 => case.ids_bytes = 1,
            2 => case.records[12..16].copy_from_slice(&u32::MAX.to_be_bytes()),
            3 => case.records[4..8].fill(0),
            4 => case.records[16..20].copy_from_slice(&1000u32.to_be_bytes()),
            _ => case.records[..2].copy_from_slice(&41u16.to_be_bytes()),
        }
        cases.push(case.rejected(if invalid < 2 { 4 } else { 5 }));
    }
    let mut invalid_number = number("1", Number::Valid(1));
    invalid_number.records[2..4].copy_from_slice(&4u16.to_be_bytes());
    cases.push(invalid_number.rejected(5));
    let mut missing_metadata = number("1", Number::Valid(1));
    missing_metadata.lexemes.pop();
    cases.push(missing_metadata.rejected(5));
    let mut invalid_name = baseline.clone();
    invalid_name.lexemes[0] = 0xff;
    cases.push(invalid_name.rejected(5));
    cases.push(Case::new(
        vec![token(TokenKind::Identifier("x".into())); 64],
        None,
    ));
    cases.push(Case::new(vec![token(TokenKind::Identifier("x".into())); 65], None).rejected(4));
    let registry = engine::build_default_asm_registry();
    let core = RuntimeModelCore::from_registry(&registry).unwrap();
    let resolved = core.resolve_pipeline("68020", None).unwrap();
    let package = crate::native_runtime_package::prepare_package(&core, &resolved).unwrap();
    let mut input = Vec::new();
    word(&mut input, cases.len() as u32);
    word(&mut input, package.len() as u32);
    padded(&mut input, &package);
    let mut expected = Vec::new();
    for case in &cases {
        case.encode(&mut input);
        expected.extend(case.oracle());
    }
    (input, expected)
}
#[test]
fn fixture_numeric_metadata_and_live_facets_cover_signed_transport_boundary() {
    for (text, value, valid) in [
        ("$7FFFFFFFFFFFFFFF", i64::MAX as u64, 1),
        ("$8000000000000000", 1u64 << 63, 0),
        ("$FFFFFFFFFFFFFFFF", u64::MAX, 0),
    ] {
        let case = number(text, Number::Valid(value));
        let (views, _) = prepare_tokens(&case.tokens);
        assert_eq!(views[0].numeric_status, valid);
        assert_eq!(
            u16::from_be_bytes(case.records[2..4].try_into().unwrap()),
            1
        );
        assert_eq!(
            u64::from_be_bytes(case.lexemes[text.len()..].try_into().unwrap()),
            value
        );
    }
}
