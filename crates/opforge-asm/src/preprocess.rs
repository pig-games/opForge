// SPDX-License-Identifier: GPL-3.0-or-later

use opcore::macro_processor::{
    substitute_macro_line_with_named_args, CompileTimeVisibility, MacroError, MacroExports,
    MacroProcessor, NativeStatementExport, StatementDirectiveKind, StatementSupportHooks,
};
use opcore::parser::{
    select_and_match_statement_signature_texts, StatementCaptureText, StatementSignature,
};
use opcore::text_utils::{is_ident_start, to_upper, Cursor};
use opcore::tokenizer::{NumberLiteral, Span, Token, TokenKind, Tokenizer};
use registry::parse_statement_definition_from_line;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use types::processing::{LineProcessingTrace, ProcessingRequestKind};

pub type AsmMacroError = MacroError;

#[derive(Debug, Clone, Default)]
pub struct AsmMacroExports {
    core: MacroExports,
    statements: Vec<NativeStatementExport>,
}

impl AsmMacroExports {
    pub fn names(&self) -> HashSet<String> {
        let mut names = self.core.names();
        names.extend(self.statements.iter().map(|def| def.keyword().to_string()));
        names
    }

    pub fn visibility_index(&self) -> HashMap<String, CompileTimeVisibility> {
        let mut vis = self.core.visibility_index();
        for def in &self.statements {
            vis.entry(def.keyword().to_string())
                .and_modify(|entry| {
                    if matches!(def.visibility(), CompileTimeVisibility::Public) {
                        *entry = CompileTimeVisibility::Public;
                    }
                })
                .or_insert(def.visibility());
        }
        vis
    }
}

pub struct AsmMacroProcessor {
    inner: MacroProcessor,
    runtime_processing_traces: Vec<(u8, u32, LineProcessingTrace)>,
}

thread_local! {
    static PREPROCESS_RUNTIME_PROCESSING_TRACES: RefCell<Vec<(u8, u32, LineProcessingTrace)>> =
        const { RefCell::new(Vec::new()) };
}

impl AsmMacroProcessor {
    pub fn new(max_depth: usize) -> Self {
        Self {
            inner: MacroProcessor::new()
                .with_max_depth(max_depth)
                .with_statement_support_hooks(StatementSupportHooks {
                    classify_directive: asm_classify_statement_directive,
                    parse_definition: asm_parse_statement_definition,
                    expand_invocation: asm_expand_statement_invocation,
                }),
            runtime_processing_traces: Vec::new(),
        }
    }

    pub fn expand(&mut self, lines: &[String]) -> Result<Vec<String>, AsmMacroError> {
        clear_preprocess_runtime_processing_traces();
        let result = self.inner.expand(lines);
        self.runtime_processing_traces
            .extend(take_preprocess_runtime_processing_traces());
        result
    }

    pub fn inject_from(&mut self, exports: &AsmMacroExports, names: &[String]) {
        self.inner.inject_from(&exports.core, names);
        self.inner
            .inject_statement_defs_from(&exports.statements, names);
    }

    pub fn inject_all(&mut self, exports: &AsmMacroExports) {
        self.inner.inject_all(&exports.core);
        self.inner.inject_all_statement_defs(&exports.statements);
    }

    pub fn inject_qualified(&mut self, exports: &AsmMacroExports, qualifier: &str) {
        self.inner.inject_qualified(&exports.core, qualifier);
        self.inner
            .inject_qualified_statement_defs(&exports.statements, qualifier);
    }

    pub fn take_native_exports(&mut self) -> AsmMacroExports {
        AsmMacroExports {
            core: self.inner.take_native_exports(),
            statements: self.inner.take_native_statement_defs(),
        }
    }

    pub fn take_runtime_processing_traces(&mut self) -> Vec<(u8, u32, LineProcessingTrace)> {
        std::mem::take(&mut self.runtime_processing_traces)
    }
}

fn clear_preprocess_runtime_processing_traces() {
    PREPROCESS_RUNTIME_PROCESSING_TRACES.with(|traces| traces.borrow_mut().clear());
}

fn take_preprocess_runtime_processing_traces() -> Vec<(u8, u32, LineProcessingTrace)> {
    PREPROCESS_RUNTIME_PROCESSING_TRACES.with(|traces| std::mem::take(&mut *traces.borrow_mut()))
}

fn record_preprocess_runtime_trace(line_num: u32, kind: &str) {
    let mut trace = LineProcessingTrace::default();
    trace.push(ProcessingRequestKind::Processor {
        processor: "asm".to_string(),
        kind: kind.to_string(),
    });
    PREPROCESS_RUNTIME_PROCESSING_TRACES
        .with(|traces| traces.borrow_mut().push((0, line_num, trace)));
}

fn asm_classify_statement_directive(code: &str) -> Option<StatementDirectiveKind> {
    let mut cursor = Cursor::new(code);
    cursor.skip_ws();
    if let Some(label) = cursor.take_ident() {
        if cursor.peek() == Some(b':') {
            cursor.next();
            cursor.skip_ws();
        } else {
            let _ = label;
        }
    }
    if cursor.peek() != Some(b'.') {
        return None;
    }
    cursor.next();
    cursor.skip_ws();
    let directive = cursor.take_ident()?.to_ascii_uppercase();
    match directive.as_str() {
        "STATEMENT" => Some(StatementDirectiveKind::Def),
        "ENDSTATEMENT" => Some(StatementDirectiveKind::End),
        _ => None,
    }
}

fn asm_parse_statement_definition(
    code: &str,
    line_num: u32,
) -> Result<(String, StatementSignature), MacroError> {
    record_preprocess_runtime_trace(line_num, "preprocess.statement_definition");
    let def = parse_statement_definition_from_line(code, line_num)
        .map_err(|err| MacroError::new(err.message, Some(line_num), Some(err.span.col_start)))?;
    Ok((def.keyword, def.signature))
}

fn asm_expand_statement_invocation(
    code: &str,
    line_num: u32,
    depth: usize,
    processor: &mut MacroProcessor,
) -> Result<Option<Vec<String>>, AsmMacroError> {
    record_preprocess_runtime_trace(line_num, "preprocess.statement_invocation");
    let defs = processor.statement_defs();
    if defs.is_empty() {
        return Ok(None);
    }

    let mut cursor = Cursor::new(code);
    cursor.skip_ws();
    if cursor.peek().is_none() || cursor.peek() == Some(b'.') {
        return Ok(None);
    }

    let Some((label, mnemonic_text, mnemonic_end)) = scan_statement_invocation(code) else {
        return Ok(None);
    };

    let mnemonic_upper = mnemonic_text.to_ascii_uppercase();
    let mut best_keyword: Option<&str> = None;
    for def in &defs {
        let keyword = def.keyword();
        let keyword_upper = keyword.to_ascii_uppercase();
        if !mnemonic_upper.starts_with(&keyword_upper) {
            continue;
        }
        match best_keyword {
            None => best_keyword = Some(keyword),
            Some(current) if keyword.len() > current.len() => best_keyword = Some(keyword),
            _ => {}
        }
    }

    let Some(keyword_upper) = best_keyword else {
        return Ok(None);
    };

    let matching_defs: Vec<&NativeStatementExport> = defs
        .iter()
        .filter(|def| def.keyword().eq_ignore_ascii_case(keyword_upper))
        .collect();
    let remainder = &mnemonic_text[keyword_upper.len()..];
    let tail = code.get(mnemonic_end..).unwrap_or("");
    let match_text = format!("{}{}", remainder, tail);

    let match_tokens = tokenize_line(&match_text, line_num)?;
    let signatures: Vec<StatementSignature> = matching_defs
        .iter()
        .map(|def| def.signature().clone())
        .collect();
    let (selection, captures) =
        match select_and_match_statement_signature_texts(&signatures, &match_tokens)
            .map_err(|err| MacroError::new(err.message, Some(line_num), Some(err.span.col_start)))?
        {
            Some(result) => result,
            None => {
                let split_tokens = split_single_letter_digit_tokens(&match_tokens);
                let Some(result) =
                    select_and_match_statement_signature_texts(&signatures, &split_tokens)
                        .map_err(|err| {
                            MacroError::new(err.message, Some(line_num), Some(err.span.col_start))
                        })?
                else {
                    return Ok(None);
                };
                result
            }
        };

    let (positional, named, full_list) = build_statement_args(&captures);
    let mut expanded = Vec::new();
    for line in matching_defs[selection].body() {
        expanded.push(substitute_macro_line_with_named_args(
            line,
            &positional,
            &named,
            &full_list,
        ));
    }

    if let Some(label) = &label {
        attach_label_to_expansion(label, &mut expanded);
    }

    let nested = processor.expand_nested_statement_lines(&expanded, depth + 1)?;
    Ok(Some(nested))
}

fn scan_statement_invocation(line: &str) -> Option<(Option<String>, String, usize)> {
    let mut cursor = Cursor::new(line);
    cursor.skip_ws();
    let at_col1 = cursor.pos() == 0;
    let first = cursor.peek()?;
    if first == b'.' || first == b';' || first == b'#' || first == b'*' {
        return None;
    }
    if !is_ident_start(first) {
        return None;
    }

    let mut label = None;
    if at_col1 {
        let name = cursor.take_ident()?;
        if cursor.peek() == Some(b':') {
            cursor.next();
        }
        label = Some(name);
        cursor.skip_ws();
        if cursor.peek().is_none() || !is_ident_start(cursor.peek()?) {
            return None;
        }
    }

    let mnemonic = cursor.take_ident()?;
    let mnemonic_end = cursor.pos();
    Some((label, mnemonic, mnemonic_end))
}

fn tokenize_line(line: &str, line_num: u32) -> Result<Vec<Token>, AsmMacroError> {
    let mut tokenizer = Tokenizer::new(line, line_num);
    let mut tokens = Vec::new();
    loop {
        let token = tokenizer.next_token().map_err(|err| {
            MacroError::new(err.message, Some(line_num), Some(err.span.col_start))
        })?;
        if matches!(token.kind, TokenKind::End) {
            break;
        }
        tokens.push(token);
    }
    Ok(tokens)
}

fn split_single_letter_digit_tokens(tokens: &[Token]) -> Vec<Token> {
    let mut out = Vec::with_capacity(tokens.len());
    for token in tokens {
        match &token.kind {
            TokenKind::Identifier(name) | TokenKind::Register(name) => {
                if name.len() >= 2 {
                    let mut chars = name.chars();
                    if let Some(first) = chars.next() {
                        let rest: String = chars.collect();
                        if first.is_ascii_alphabetic()
                            && !rest.is_empty()
                            && rest.chars().all(|c| c.is_ascii_digit())
                        {
                            let first_kind = if matches!(token.kind, TokenKind::Register(_)) {
                                TokenKind::Register(first.to_string())
                            } else {
                                TokenKind::Identifier(first.to_string())
                            };
                            let first_start = token.span.col_start;
                            let first_end = first_start + 1;
                            let second_start = first_end;
                            let second_end = token.span.col_end;
                            out.push(Token {
                                kind: first_kind,
                                span: Span {
                                    line: token.span.line,
                                    col_start: first_start,
                                    col_end: first_end,
                                },
                            });
                            out.push(Token {
                                kind: TokenKind::Number(NumberLiteral {
                                    text: rest,
                                    base: 10,
                                }),
                                span: Span {
                                    line: token.span.line,
                                    col_start: second_start,
                                    col_end: second_end,
                                },
                            });
                            continue;
                        }
                    }
                }
                out.push(token.clone());
            }
            _ => out.push(token.clone()),
        }
    }
    out
}

fn build_statement_args(
    captures: &[StatementCaptureText],
) -> (Vec<String>, HashMap<String, String>, String) {
    let mut positional = Vec::new();
    let mut named = HashMap::new();
    for capture in captures {
        positional.push(capture.text.clone());
        named.insert(to_upper(&capture.name), capture.text.clone());
    }
    let full_list = positional.join(", ");
    (positional, named, full_list)
}

fn attach_label_to_expansion(label: &str, expanded: &mut Vec<String>) {
    if let Some(first) = expanded.first_mut() {
        let trimmed = first.trim_start();
        if trimmed.is_empty() {
            *first = label.to_string();
        } else {
            *first = format!("{label} {trimmed}");
        }
    } else {
        expanded.push(label.to_string());
    }
}

#[cfg(test)]
mod tests {
    use opcore::macro_processor::CompileTimeVisibility;

    use super::AsmMacroProcessor;

    #[test]
    fn asm_macro_processor_expands_statement_support() {
        let mut processor = AsmMacroProcessor::new(64);
        let lines = vec![
            ".statement LOAD byte:val".to_string(),
            "    .byte .val".to_string(),
            ".endstatement".to_string(),
            "    LOAD 7".to_string(),
        ];

        let out = processor.expand(&lines).expect("expand");
        assert!(out.contains(&"    .byte 7".to_string()));
    }

    #[test]
    fn asm_macro_exports_preserve_statement_defs_for_import() {
        let mut producer = AsmMacroProcessor::new(64);
        let lines = vec![
            ".pub".to_string(),
            ".statement LOAD byte:val".to_string(),
            "    .byte .val".to_string(),
            ".endstatement".to_string(),
        ];
        let _ = producer.expand(&lines).expect("expand");
        let exports = producer.take_native_exports();

        let mut consumer = AsmMacroProcessor::new(64);
        consumer.inject_all(&exports);
        let out = consumer
            .expand(&["    LOAD 7".to_string()])
            .expect("expand");

        assert!(out.contains(&"    .byte 7".to_string()));
        assert_eq!(
            exports.visibility_index().get("LOAD"),
            Some(&CompileTimeVisibility::Public)
        );
    }

    #[test]
    fn asm_macro_processor_expands_statement_with_boundary_signature() {
        let mut processor = AsmMacroProcessor::new(64);
        let lines = vec![
            ".statement lda \"[\"[{byte:val}]\"],y\"".to_string(),
            "    .byte .val".to_string(),
            ".endstatement".to_string(),
            "    lda [$05],y".to_string(),
        ];

        let out = processor.expand(&lines).expect("expand");
        assert!(out.contains(&"    .byte $05".to_string()));
    }

    #[test]
    fn asm_macro_processor_expands_dotted_statement_with_split_register_digit_args() {
        let mut processor = AsmMacroProcessor::new(64);
        let lines = vec![
            ".statement move.b char:dst[{byte:dstnum}] \",\" char:src[{byte:srcnum}]".to_string(),
            "    .byte 'b'".to_string(),
            "    .byte '.dst', .dstnum".to_string(),
            "    .byte '.src', .srcnum".to_string(),
            ".endstatement".to_string(),
            "    move.b d0,d2".to_string(),
        ];

        let out = processor.expand(&lines).expect("expand");
        assert!(out.contains(&"    .byte 'b'".to_string()));
        assert!(out.contains(&"    .byte 'd', 0".to_string()));
        assert!(out.contains(&"    .byte 'd', 2".to_string()));
    }
}
