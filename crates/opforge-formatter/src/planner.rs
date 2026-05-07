// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::{
    symbol_context::{
        build_inline_context, effective_label_case, DocumentFormatContext, LabelRole,
    },
    CaseStyle, FormatterConfig, IndentChar, LabelCaseStyle, LabelColonStyle, SurfaceDocument,
    SurfaceLine, SurfaceLineKind, SurfaceParsedDocument, SurfaceParsedLine,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlannedLine {
    pub line_number: usize,
    pub output: Vec<SurfaceLine>,
    pub changed: bool,
    pub preserved_original: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FormatPlan {
    pub lines: Vec<PlannedLine>,
}

impl FormatPlan {
    pub fn changed_line_count(&self) -> usize {
        self.lines.iter().filter(|line| line.changed).count()
    }

    pub fn render(&self) -> String {
        let mut out = String::new();
        for line in &self.lines {
            for surface in &line.output {
                out.push_str(&surface.render());
            }
        }
        out
    }
}

pub fn plan_document(
    doc: &SurfaceDocument,
    parsed: &SurfaceParsedDocument,
    config: &FormatterConfig,
) -> FormatPlan {
    let source = doc.render();
    let context = build_inline_context(&source, parsed, config);
    plan_document_with_context(doc, parsed, config, &context)
}

pub(crate) fn plan_document_with_context(
    doc: &SurfaceDocument,
    parsed: &SurfaceParsedDocument,
    config: &FormatterConfig,
    context: &DocumentFormatContext,
) -> FormatPlan {
    let mut plan = FormatPlan {
        lines: Vec::with_capacity(doc.lines.len()),
    };
    let mut blank_run = 0usize;

    for (idx, line) in doc.lines.iter().enumerate() {
        let line_number = idx + 1;
        let original = line.render();
        let is_blank = line.code.is_empty() && line.comment.is_none();
        if is_blank {
            blank_run += 1;
        } else {
            blank_run = 0;
        }

        if is_blank && blank_run > config.max_consecutive_blank_lines {
            plan.lines.push(PlannedLine {
                line_number,
                output: Vec::new(),
                changed: true,
                preserved_original: false,
            });
            continue;
        }

        let parsed_line = parsed.lines.get(idx);
        let (output, preserved_original) = if let Some(parsed_line) = parsed_line {
            if parsed_line.is_fallback() {
                (vec![line.clone()], true)
            } else {
                (
                    normalize_line(line, parsed_line, config, context, idx),
                    false,
                )
            }
        } else {
            (vec![line.clone()], true)
        };
        let changed = render_surface_lines(&output) != original;

        plan.lines.push(PlannedLine {
            line_number,
            output,
            changed,
            preserved_original,
        });
    }

    plan
}

fn normalize_line(
    line: &SurfaceLine,
    parsed: &SurfaceParsedLine,
    config: &FormatterConfig,
    context: &DocumentFormatContext,
    line_index: usize,
) -> Vec<SurfaceLine> {
    match parsed.kind {
        SurfaceLineKind::Assignment => {
            return vec![normalize_assignment_line(
                line, parsed, config, context, line_index,
            )];
        }
        SurfaceLineKind::Directive | SurfaceLineKind::Instruction => {}
        SurfaceLineKind::CommentOnly => {
            if line.indent.is_empty() {
                return vec![line.clone()];
            }
            return vec![SurfaceLine {
                indent: indent_fill(config),
                code: String::new(),
                comment: line.comment.clone(),
                line_ending: line.line_ending,
            }];
        }
        SurfaceLineKind::LabelOnly => {
            if config.label_colon_style == LabelColonStyle::Keep
                && label_case_for_line(context, config, line_index) == LabelCaseStyle::Keep
            {
                return vec![line.clone()];
            }
            return vec![normalize_label_only_line(
                line, parsed, config, context, line_index,
            )];
        }
        _ => return vec![line.clone()],
    }

    let Some(raw_head) = parsed.head.as_deref() else {
        return vec![line.clone()];
    };

    let mut indent = line.indent.clone();
    let mut label_token = None;
    let mut code = String::new();
    if let Some(label) = parsed.label.as_deref() {
        indent.clear();
        let current_label_token = format_label_token(label, parsed, config, context, line_index);
        code.push_str(&current_label_token);
        code.push_str(&spacing_after_label(&current_label_token, config));
        label_token = Some(current_label_token);
    } else if parsed.kind == SurfaceLineKind::Directive
        || parsed.kind == SurfaceLineKind::Assignment
    {
        if !line.indent.is_empty() {
            indent = indent_fill(config);
        }
    } else if parsed.kind == SurfaceLineKind::Instruction && config.align_unlabeled_instructions {
        indent = indent_fill(config);
    }
    let head = if parsed.kind == SurfaceLineKind::Instruction {
        config.mnemonic_case.apply(raw_head)
    } else if parsed.kind == SurfaceLineKind::Directive {
        config.directive_case.apply(raw_head)
    } else {
        raw_head.to_string()
    };
    code.push_str(&head);

    let mut tail = normalize_operand_tail(&parsed.tail);
    tail = rewrite_tail(parsed, &tail, context, line_index);
    if parsed.kind == SurfaceLineKind::Instruction {
        tail = apply_register_case(&tail, config.register_case);
    }
    tail = apply_hex_literal_case(&tail, config.hex_literal_case);
    if !tail.is_empty() {
        code.push(' ');
        code.push_str(&tail);
    }

    let comment = line
        .comment
        .as_deref()
        .map(|comment| comment.trim_start_matches([' ', '\t']).to_string());

    if parsed.kind == SurfaceLineKind::Instruction
        && config.split_long_label_instructions
        && config.indent_char == IndentChar::Space
        && label_token
            .as_ref()
            .is_some_and(|token| token.len() >= config.label_alignment_column)
    {
        let mut instruction_code = head;
        if !tail.is_empty() {
            instruction_code.push(' ');
            instruction_code.push_str(&tail);
        }
        if comment.is_some() {
            instruction_code = instruction_code.trim_end_matches([' ', '\t']).to_string();
            instruction_code.push_str("  ");
        }
        let Some(label_code) = label_token else {
            return vec![SurfaceLine {
                indent,
                code,
                comment,
                line_ending: line.line_ending,
            }];
        };
        return vec![
            SurfaceLine {
                indent: String::new(),
                code: label_code,
                comment: None,
                line_ending: split_inserted_line_ending(line.line_ending),
            },
            SurfaceLine {
                indent: indent_fill(config),
                code: instruction_code,
                comment,
                line_ending: line.line_ending,
            },
        ];
    }

    if let Some(comment) = comment {
        code = code.trim_end_matches([' ', '\t']).to_string();
        if !code.is_empty() {
            code.push_str("  ");
        }
        return vec![SurfaceLine {
            indent,
            code,
            comment: Some(comment),
            line_ending: line.line_ending,
        }];
    }

    vec![SurfaceLine {
        indent,
        code,
        comment: None,
        line_ending: line.line_ending,
    }]
}

fn normalize_assignment_line(
    line: &SurfaceLine,
    parsed: &SurfaceParsedLine,
    config: &FormatterConfig,
    context: &DocumentFormatContext,
    line_index: usize,
) -> SurfaceLine {
    let Some(operator) = parsed.head.as_deref() else {
        return line.clone();
    };

    let indent = if line.indent.is_empty() {
        String::new()
    } else {
        indent_fill(config)
    };
    let raw_code = parsed.raw_code.trim_end_matches([' ', '\t']);
    let search_start = parsed.label.as_deref().map(str::len).unwrap_or(0);
    let Some(relative_operator_index) = raw_code[search_start..].find(operator) else {
        return SurfaceLine {
            indent,
            code: line.code.clone(),
            comment: line.comment.clone(),
            line_ending: line.line_ending,
        };
    };
    let operator_index = search_start + relative_operator_index;
    let mut code = String::new();

    if let Some(label) = parsed.label.as_deref() {
        let label_token =
            apply_case_to_label(label, label_case_for_line(context, config, line_index));
        code.push_str(&label_token);
        code.push_str(&assignment_spacing_before_operator(
            raw_code,
            operator_index,
            label,
            &label_token,
        ));
    } else if operator_index > 0 {
        code.push_str(&raw_code[..operator_index]);
    }

    code.push_str(operator);

    let mut tail = raw_code[operator_index + operator.len()..].to_string();
    tail = rewrite_identifier_tokens(&tail, &context.reference_renames);
    tail = apply_hex_literal_case(&tail, config.hex_literal_case);
    code.push_str(&tail);

    SurfaceLine {
        indent,
        code,
        comment: line.comment.clone(),
        line_ending: line.line_ending,
    }
}

fn normalize_label_only_line(
    line: &SurfaceLine,
    parsed: &SurfaceParsedLine,
    config: &FormatterConfig,
    context: &DocumentFormatContext,
    line_index: usize,
) -> SurfaceLine {
    let Some(label) = parsed.label.as_deref() else {
        return line.clone();
    };
    let mut code = apply_case_to_label(label, label_case_for_line(context, config, line_index));
    if label_should_have_colon(
        raw_label_has_colon(&parsed.raw_code, label),
        config.label_colon_style,
    ) {
        code.push(':');
    }
    SurfaceLine {
        indent: line.indent.clone(),
        code,
        comment: line.comment.clone(),
        line_ending: line.line_ending,
    }
}

fn render_surface_lines(lines: &[SurfaceLine]) -> String {
    let mut out = String::new();
    for line in lines {
        out.push_str(&line.render());
    }
    out
}

fn split_inserted_line_ending(
    source: crate::formatter::LineEnding,
) -> crate::formatter::LineEnding {
    match source {
        crate::formatter::LineEnding::Crlf => crate::formatter::LineEnding::Crlf,
        _ => crate::formatter::LineEnding::Lf,
    }
}

fn format_label_token(
    label: &str,
    parsed: &SurfaceParsedLine,
    config: &FormatterConfig,
    context: &DocumentFormatContext,
    line_index: usize,
) -> String {
    let mut label_token =
        apply_case_to_label(label, label_case_for_line(context, config, line_index));
    if label_should_have_colon(
        raw_label_has_colon(&parsed.raw_code, label),
        config.label_colon_style,
    ) {
        label_token.push(':');
    }
    label_token
}

fn apply_case_to_label(label: &str, case: LabelCaseStyle) -> String {
    if label == "*" {
        return label.to_string();
    }
    case.apply(label)
}

fn label_case_for_line(
    context: &DocumentFormatContext,
    config: &FormatterConfig,
    line_index: usize,
) -> LabelCaseStyle {
    let role = context
        .label_roles
        .get(line_index)
        .and_then(|role| *role)
        .unwrap_or(LabelRole::Generic);
    effective_label_case(config, role)
}

fn indent_fill(config: &FormatterConfig) -> String {
    config.indent_char.fill(config.label_alignment_column)
}

fn spacing_after_label(label_token: &str, config: &FormatterConfig) -> String {
    match config.indent_char {
        IndentChar::Space => {
            let spacing = if config.label_alignment_column > label_token.len() {
                config.label_alignment_column - label_token.len()
            } else {
                1
            };
            " ".repeat(spacing.max(1))
        }
        IndentChar::Tab => "\t".repeat(config.label_alignment_column.max(1)),
    }
}

fn assignment_spacing_before_operator(
    raw_code: &str,
    operator_index: usize,
    original_label: &str,
    styled_label: &str,
) -> String {
    let original_spacing = &raw_code[original_label.len()..operator_index];
    if styled_label.len() == original_label.len() {
        return original_spacing.to_string();
    }

    let spacing = operator_index.saturating_sub(styled_label.len()).max(1);
    " ".repeat(spacing)
}

fn label_should_have_colon(has_colon: bool, style: LabelColonStyle) -> bool {
    match style {
        LabelColonStyle::Keep => has_colon,
        LabelColonStyle::With => true,
        LabelColonStyle::Without => false,
    }
}

fn raw_label_has_colon(raw_code: &str, label: &str) -> bool {
    raw_code
        .as_bytes()
        .get(label.len())
        .is_some_and(|byte| *byte == b':')
}

fn rewrite_tail(
    parsed: &SurfaceParsedLine,
    tail: &str,
    context: &DocumentFormatContext,
    line_index: usize,
) -> String {
    match parsed.kind {
        SurfaceLineKind::Instruction | SurfaceLineKind::Assignment => {
            rewrite_identifier_tokens(tail, &context.reference_renames)
        }
        SurfaceLineKind::Directive => {
            let Some(head) = parsed.head.as_deref() else {
                return tail.to_string();
            };
            if head.eq_ignore_ascii_case(".module") {
                return tail.to_string();
            }
            if head.eq_ignore_ascii_case(".use") {
                let renames = context
                    .selective_import_renames
                    .get(line_index)
                    .cloned()
                    .unwrap_or_default();
                return rewrite_use_tail(tail, &renames);
            }
            rewrite_identifier_tokens(tail, &context.reference_renames)
        }
        _ => tail.to_string(),
    }
}

fn rewrite_identifier_tokens(
    input: &str,
    renames: &std::collections::HashMap<String, String>,
) -> String {
    if renames.is_empty() {
        return input.to_string();
    }

    let chars: Vec<char> = input.chars().collect();
    let mut out = String::with_capacity(input.len());
    let mut idx = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;

    while idx < chars.len() {
        let ch = chars[idx];
        if escaped {
            out.push(ch);
            escaped = false;
            idx += 1;
            continue;
        }
        if ch == '\\' && (in_single || in_double) {
            out.push(ch);
            escaped = true;
            idx += 1;
            continue;
        }
        if ch == '\'' && !in_double {
            in_single = !in_single;
            out.push(ch);
            idx += 1;
            continue;
        }
        if ch == '"' && !in_single {
            in_double = !in_double;
            out.push(ch);
            idx += 1;
            continue;
        }
        if in_single || in_double {
            out.push(ch);
            idx += 1;
            continue;
        }

        if ch.is_ascii_alphabetic() || ch == '_' {
            let start = idx;
            idx += 1;
            while idx < chars.len() && is_identish(chars[idx]) {
                idx += 1;
            }
            let token: String = chars[start..idx].iter().collect();
            if let Some(styled) = renames.get(&token) {
                out.push_str(styled);
            } else {
                out.push_str(&token);
            }
            continue;
        }

        out.push(ch);
        idx += 1;
    }

    out
}

fn rewrite_use_tail(input: &str, renames: &std::collections::HashMap<String, String>) -> String {
    if renames.is_empty() {
        return input.to_string();
    }

    let chars: Vec<char> = input.chars().collect();
    let mut out = String::with_capacity(input.len());
    let mut idx = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;
    let mut seen_items_paren = false;
    let mut paren_depth = 0usize;
    let mut skip_next_alias = false;

    while idx < chars.len() {
        let ch = chars[idx];
        if escaped {
            out.push(ch);
            escaped = false;
            idx += 1;
            continue;
        }
        if ch == '\\' && (in_single || in_double) {
            out.push(ch);
            escaped = true;
            idx += 1;
            continue;
        }
        if ch == '\'' && !in_double {
            in_single = !in_single;
            out.push(ch);
            idx += 1;
            continue;
        }
        if ch == '"' && !in_single {
            in_double = !in_double;
            out.push(ch);
            idx += 1;
            continue;
        }
        if in_single || in_double {
            out.push(ch);
            idx += 1;
            continue;
        }
        if ch == '(' {
            if !seen_items_paren {
                seen_items_paren = true;
                paren_depth = 1;
            } else if paren_depth > 0 {
                paren_depth += 1;
            }
            out.push(ch);
            idx += 1;
            continue;
        }
        if ch == ')' {
            paren_depth = paren_depth.saturating_sub(1);
            out.push(ch);
            idx += 1;
            continue;
        }
        if ch.is_ascii_alphabetic() || ch == '_' {
            let start = idx;
            idx += 1;
            while idx < chars.len() && is_identish(chars[idx]) {
                idx += 1;
            }
            let token: String = chars[start..idx].iter().collect();
            if paren_depth > 0 {
                if token.eq_ignore_ascii_case("as") {
                    skip_next_alias = true;
                    out.push_str(&token);
                } else if skip_next_alias {
                    skip_next_alias = false;
                    out.push_str(&token);
                } else if let Some(styled) = renames.get(&token) {
                    out.push_str(styled);
                } else {
                    out.push_str(&token);
                }
            } else {
                out.push_str(&token);
            }
            continue;
        }

        out.push(ch);
        idx += 1;
    }

    out
}

fn normalize_operand_tail(tail: &str) -> String {
    let trimmed = tail.trim();
    if trimmed.is_empty() {
        return String::new();
    }

    let mut out = String::with_capacity(trimmed.len());
    let mut chars = trimmed.chars().peekable();
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;

    while let Some(ch) = chars.next() {
        if escaped {
            out.push(ch);
            escaped = false;
            continue;
        }
        if ch == '\\' && (in_single || in_double) {
            out.push(ch);
            escaped = true;
            continue;
        }
        if ch == '\'' && !in_double {
            in_single = !in_single;
            out.push(ch);
            continue;
        }
        if ch == '"' && !in_single {
            in_double = !in_double;
            out.push(ch);
            continue;
        }
        if ch == ',' && !in_single && !in_double {
            trim_trailing_space(&mut out);
            out.push(',');
            while matches!(chars.peek(), Some(' ' | '\t')) {
                chars.next();
            }
            if chars.peek().is_some() {
                out.push(' ');
            }
            continue;
        }
        out.push(ch);
    }

    out
}

fn apply_register_case(input: &str, case: CaseStyle) -> String {
    if case == CaseStyle::Keep {
        return input.to_string();
    }

    let chars: Vec<char> = input.chars().collect();
    let mut out = String::with_capacity(input.len());
    let mut idx = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;

    while idx < chars.len() {
        let ch = chars[idx];
        if escaped {
            out.push(ch);
            escaped = false;
            idx += 1;
            continue;
        }
        if ch == '\\' && (in_single || in_double) {
            out.push(ch);
            escaped = true;
            idx += 1;
            continue;
        }
        if ch == '\'' && !in_double {
            in_single = !in_single;
            out.push(ch);
            idx += 1;
            continue;
        }
        if ch == '"' && !in_single {
            in_double = !in_double;
            out.push(ch);
            idx += 1;
            continue;
        }
        if in_single || in_double {
            out.push(ch);
            idx += 1;
            continue;
        }

        if ch.is_ascii_alphabetic() || ch == '_' {
            let token_start = idx;
            idx += 1;
            while idx < chars.len() && is_identish(chars[idx]) {
                idx += 1;
            }
            let token: String = chars[token_start..idx].iter().collect();
            if should_case_register_token(&chars, token_start, &token) {
                out.push_str(&case.apply(&token));
            } else {
                out.push_str(&token);
            }
            continue;
        }

        out.push(ch);
        idx += 1;
    }

    out
}

fn should_case_register_token(chars: &[char], token_start: usize, token: &str) -> bool {
    if token_start > 0 && chars[token_start - 1] == '$' {
        return false;
    }
    is_known_register_token(token)
}

fn is_known_register_token(token: &str) -> bool {
    let upper = token.to_ascii_uppercase();
    matches!(
        upper.as_str(),
        // Intel 8080/8085 register names and pairs.
        "A"
            | "B"
            | "C"
            | "D"
            | "E"
            | "H"
            | "L"
            | "M"
            | "BC"
            | "DE"
            | "HL"
            | "SP"
            | "PSW"
            // Z80 register names and pairs.
            | "AF"
            | "AF'"
            | "I"
            | "R"
            | "IX"
            | "IY"
            | "IXH"
            | "IXL"
            | "IYH"
            | "IYL"
            // MOS-family register names.
            | "X"
            | "Y"
            | "S"
            // Motorola 68000-family control registers commonly used in sources.
            | "CCR"
            | "SR"
            | "USP"
    ) || is_m68k_general_register(&upper)
}

fn is_m68k_general_register(token: &str) -> bool {
    let bytes = token.as_bytes();
    if bytes.len() != 2 {
        return false;
    }
    matches!(bytes[0], b'D' | b'A') && bytes[1].is_ascii_digit() && bytes[1] <= b'7'
}

fn apply_hex_literal_case(input: &str, case: CaseStyle) -> String {
    if case == CaseStyle::Keep {
        return input.to_string();
    }

    let chars: Vec<char> = input.chars().collect();
    let mut out = String::with_capacity(input.len());
    let mut idx = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;

    while idx < chars.len() {
        let ch = chars[idx];
        if escaped {
            out.push(ch);
            escaped = false;
            idx += 1;
            continue;
        }
        if ch == '\\' && (in_single || in_double) {
            out.push(ch);
            escaped = true;
            idx += 1;
            continue;
        }
        if ch == '\'' && !in_double {
            in_single = !in_single;
            out.push(ch);
            idx += 1;
            continue;
        }
        if ch == '"' && !in_single {
            in_double = !in_double;
            out.push(ch);
            idx += 1;
            continue;
        }
        if in_single || in_double {
            out.push(ch);
            idx += 1;
            continue;
        }

        if ch == '$' {
            out.push(ch);
            idx += 1;
            let start = idx;
            while idx < chars.len() && is_hex_digit_or_underscore(chars[idx]) {
                idx += 1;
            }
            if idx > start {
                out.push_str(&apply_case_to_hex_digits(
                    &chars[start..idx].iter().collect::<String>(),
                    case,
                ));
            }
            continue;
        }

        if is_hex_digit_or_underscore(ch) && is_hex_token_boundary_before(&chars, idx) {
            let start = idx;
            idx += 1;
            while idx < chars.len() && is_hex_digit_or_underscore(chars[idx]) {
                idx += 1;
            }
            if idx < chars.len()
                && (chars[idx] == 'h' || chars[idx] == 'H')
                && is_hex_token_boundary_after(&chars, idx + 1)
            {
                out.push_str(&apply_case_to_hex_digits(
                    &chars[start..idx].iter().collect::<String>(),
                    case,
                ));
                out.push(match case {
                    CaseStyle::Upper => 'H',
                    CaseStyle::Lower => 'h',
                    CaseStyle::Keep => chars[idx],
                });
                idx += 1;
                continue;
            }
            out.push_str(&chars[start..idx].iter().collect::<String>());
            continue;
        }

        out.push(ch);
        idx += 1;
    }

    out
}

fn apply_case_to_hex_digits(input: &str, case: CaseStyle) -> String {
    input
        .chars()
        .map(|ch| match case {
            CaseStyle::Upper => {
                if ch.is_ascii_hexdigit() {
                    ch.to_ascii_uppercase()
                } else {
                    ch
                }
            }
            CaseStyle::Lower => {
                if ch.is_ascii_hexdigit() {
                    ch.to_ascii_lowercase()
                } else {
                    ch
                }
            }
            CaseStyle::Keep => ch,
        })
        .collect()
}

fn is_hex_digit_or_underscore(ch: char) -> bool {
    ch.is_ascii_hexdigit() || ch == '_'
}

fn is_identish(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

fn is_hex_token_boundary_before(chars: &[char], idx: usize) -> bool {
    if idx == 0 {
        true
    } else {
        !is_identish(chars[idx - 1])
    }
}

fn is_hex_token_boundary_after(chars: &[char], idx: usize) -> bool {
    if idx >= chars.len() {
        true
    } else {
        !is_identish(chars[idx])
    }
}

fn trim_trailing_space(out: &mut String) {
    while matches!(out.chars().last(), Some(' ' | '\t')) {
        out.pop();
    }
}

#[cfg(test)]
mod tests {
    use super::plan_document;
    use crate::formatter::{
        parse_document, tokenize_source, CaseStyle, FormatterConfig, IndentChar, LabelCaseStyle,
        LabelColonStyle,
    };

    #[test]
    fn planner_normalizes_intel_spacing_with_label_and_comment() {
        let source = "start:   mvi a,1 ;c\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(&doc, &parsed, &FormatterConfig::default());
        assert_eq!(plan.render(), "start:  mvi a, 1  ;c\n");
        assert_eq!(plan.changed_line_count(), 1);
    }

    #[test]
    fn planner_normalizes_mos_spacing_and_preserves_mnemonic_case() {
        let source = "    Lda $20,x ; note\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(&doc, &parsed, &FormatterConfig::default());
        assert_eq!(plan.render(), "        Lda $20, x  ; note\n");
        assert_eq!(plan.changed_line_count(), 1);
    }

    #[test]
    fn planner_preserves_unparsed_fallback_lines() {
        let source = "    .+bad ; keep\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(&doc, &parsed, &FormatterConfig::default());
        assert_eq!(plan.render(), source);
        assert!(plan.lines[0].preserved_original);
    }

    #[test]
    fn planner_collapses_blank_runs_to_configured_max() {
        let source = "nop\n\n\t\n\nlda #1\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(&doc, &parsed, &FormatterConfig::default());
        assert_eq!(plan.render(), "nop\n\n        lda #1\n");
        assert_eq!(plan.changed_line_count(), 3);
    }

    #[test]
    fn planner_is_noop_when_line_already_matches_policy() {
        let source = "label:  lda #1  ; c\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(&doc, &parsed, &FormatterConfig::default());
        assert_eq!(plan.render(), source);
        assert_eq!(plan.changed_line_count(), 0);
    }

    #[test]
    fn planner_applies_opt_in_lowercase_style_and_colonless_labels() {
        let source = "Start: LDA #$ABCD, 1AFH ; note\n    STA $20\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(
            &doc,
            &parsed,
            &FormatterConfig {
                align_unlabeled_instructions: true,
                label_colon_style: LabelColonStyle::Without,
                label_case: LabelCaseStyle::Lower,
                mnemonic_case: CaseStyle::Lower,
                hex_literal_case: CaseStyle::Lower,
                ..FormatterConfig::default()
            },
        );
        assert_eq!(
            plan.render(),
            "start   lda #$abcd, 1afh  ; note\n        sta $20\n"
        );
        assert_eq!(plan.changed_line_count(), 2);
    }

    #[test]
    fn planner_applies_opt_in_label_only_colonless_style() {
        let source = "Entry:\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(
            &doc,
            &parsed,
            &FormatterConfig {
                label_colon_style: LabelColonStyle::Without,
                label_case: LabelCaseStyle::Lower,
                ..FormatterConfig::default()
            },
        );
        assert_eq!(plan.render(), "entry\n");
        assert_eq!(plan.changed_line_count(), 1);
    }

    #[test]
    fn planner_applies_opt_in_directive_and_register_case() {
        let source = ".CpU z80\nLoop ld a,(ix+1)\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(
            &doc,
            &parsed,
            &FormatterConfig {
                directive_case: CaseStyle::Lower,
                register_case: CaseStyle::Upper,
                ..FormatterConfig::default()
            },
        );
        assert_eq!(plan.render(), ".cpu z80\nLoop    ld A, (IX+1)\n");
        assert_eq!(plan.changed_line_count(), 2);
    }

    #[test]
    fn planner_aligns_indented_unlabeled_directives_to_code_column() {
        let source = ".cpu 8085\n            .foo 1\n            .byte 2\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(&doc, &parsed, &FormatterConfig::default());
        assert_eq!(
            plan.render(),
            ".cpu 8085\n        .foo 1\n        .byte 2\n"
        );
        assert_eq!(plan.changed_line_count(), 2);
    }

    #[test]
    fn planner_aligns_indented_comment_only_lines_to_code_column() {
        let source = "; top\n            ; indented\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(&doc, &parsed, &FormatterConfig::default());
        assert_eq!(plan.render(), "; top\n        ; indented\n");
        assert_eq!(plan.changed_line_count(), 1);
    }

    #[test]
    fn planner_splits_long_instruction_label_when_configured() {
        let source = "VeryLongLabel: lda #1 ; c\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(
            &doc,
            &parsed,
            &FormatterConfig {
                label_alignment_column: 8,
                split_long_label_instructions: true,
                ..FormatterConfig::default()
            },
        );
        assert_eq!(plan.render(), "VeryLongLabel:\n        lda #1  ; c\n");
        assert_eq!(plan.changed_line_count(), 1);
    }

    #[test]
    fn planner_uses_tabs_for_instruction_indent_and_label_spacing() {
        let source = "Start: MOVE.L D0,D1\n    BRA Start\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(
            &doc,
            &parsed,
            &FormatterConfig {
                indent_char: IndentChar::Tab,
                label_alignment_column: 1,
                label_colon_style: LabelColonStyle::Without,
                label_case: LabelCaseStyle::LowerCamel,
                mnemonic_case: CaseStyle::Lower,
                register_case: CaseStyle::Lower,
                ..FormatterConfig::default()
            },
        );
        assert_eq!(plan.render(), "start\tmove.l d0, d1\n\tbra start\n");
    }

    #[test]
    fn planner_preserves_assignment_alignment_by_default() {
        let source = "TK_KIND_OP_POWER                = 21\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(&doc, &parsed, &FormatterConfig::default());
        assert_eq!(plan.render(), source);
        assert_eq!(plan.changed_line_count(), 0);
    }

    #[test]
    fn planner_preserves_assignment_operator_column_when_label_case_changes() {
        let source = "myValue = OtherValue + $ab\n";
        let doc = tokenize_source(source);
        let parsed = parse_document(&doc);
        let plan = plan_document(
            &doc,
            &parsed,
            &FormatterConfig {
                constant_label_case: LabelCaseStyle::UpperSnake,
                hex_literal_case: CaseStyle::Upper,
                ..FormatterConfig::default()
            },
        );
        assert_eq!(plan.render(), "MY_VALUE = OtherValue + $AB\n");
        assert_eq!(plan.changed_line_count(), 1);
    }
}
