// SPDX-License-Identifier: GPL-3.0-or-later
//! Acquisition adapter inputs use retained TKVM fields and current package IDs.
use opcore::tokenizer::{Span, TokenKind, Tokenizer};
use vm::{
    packed_statement_vm::{prepare_tokens, select_head},
    runtime_model_core::RuntimeModelCore,
};
fn word(out: &mut Vec<u8>, value: u32) {
    out.extend(value.to_be_bytes());
}
fn padded(out: &mut Vec<u8>, bytes: &[u8]) {
    out.extend(bytes);
    if !bytes.len().is_multiple_of(2) {
        out.push(0);
    }
}
pub(super) fn batch() -> (Vec<u8>, Vec<u8>) {
    let registry = engine::build_default_asm_registry();
    let core = RuntimeModelCore::from_registry(&registry).unwrap();
    let resolved = core.resolve_pipeline("68020", None).unwrap();
    let package = crate::native_runtime_package::prepare_package(&core, &resolved).unwrap();
    let offset = u32::from_be_bytes(
        package[crate::native_runtime_package::STATEMENT_HEAD..][..4]
            .try_into()
            .unwrap(),
    ) as usize;
    let program = &package[offset..offset + 8];
    let definition = u16::from_be_bytes(program[2..4].try_into().unwrap());
    let end = u16::from_be_bytes(program[4..6].try_into().unwrap());
    let cases = [
        "",
        "label",
        ".statement LOAD byte:x",
        "  .StAtEmEnT move.b char:x",
        ".statement",
        "label .statement LOAD",
        "label: .statement LOAD",
        "label: .statement",
        ".endstatement",
        "label .ENDSTATEMENT ignored",
        "label: .endstatement ignored",
        "  LOAD 7",
        ".include \"missing.i\"",
        ".macro X",
        "X .macro",
        ".cpu 68020",
        ".byte 1",
        ".statement LOAD byte:x word:y long:z str:s",
        "statement .byte 1",
        "endstatement: .byte 2",
        ".statementx LOAD",
        ".endstatementx",
        "  .endstatement more [tail]",
        "label:  .statement LOAD byte:x,word:y",
    ];
    let mut input = Vec::new();
    word(&mut input, cases.len() as u32);
    word(&mut input, package.len() as u32);
    padded(&mut input, &package);
    let mut expected = Vec::new();
    for source in cases {
        let mut lexer = Tokenizer::new(source, 1);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token().unwrap();
            if matches!(token.kind, TokenKind::End) {
                break;
            }
            tokens.push(token);
        }
        let (views, _) = prepare_tokens(&tokens);
        let names = tokens
            .iter()
            .map(|t| match &t.kind {
                TokenKind::Identifier(s) | TokenKind::Register(s)
                    if s.eq_ignore_ascii_case("statement") =>
                {
                    definition
                }
                TokenKind::Identifier(s) | TokenKind::Register(s)
                    if s.eq_ignore_ascii_case("endstatement") =>
                {
                    end
                }
                _ => u16::MAX,
            })
            .collect::<Vec<_>>();
        let span = Span {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        let role = select_head(program, 0, &views, &names, span)
            .unwrap()
            .map_or(0, |head| head.action);
        word(&mut expected, 0);
        word(&mut expected, role as u32);
        let mut records = Vec::new();
        let mut lexemes = Vec::new();
        for (token, view) in tokens.iter().zip(&views) {
            let bytes = match &token.kind {
                TokenKind::Identifier(s) | TokenKind::Register(s) => s.as_bytes().to_vec(),
                TokenKind::Number(n) => n.text.as_bytes().to_vec(),
                TokenKind::String(s) => s.bytes.clone(),
                _ => Vec::new(),
            };
            records.extend(u16::from(view.kind).to_be_bytes());
            records.extend(0u16.to_be_bytes());
            for value in [
                token.span.col_start as u32,
                token.span.col_end as u32,
                lexemes.len() as u32,
                bytes.len() as u32,
            ] {
                word(&mut records, value);
            }
            lexemes.extend(bytes);
        }
        word(&mut input, tokens.len() as u32);
        word(&mut input, lexemes.len() as u32);
        input.extend(records);
        padded(&mut input, &lexemes);
    }
    (input, expected)
}
