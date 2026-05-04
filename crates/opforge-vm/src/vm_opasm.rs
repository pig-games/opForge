// SPDX-License-Identifier: GPL-3.0-or-later

//! `.opasm` VM surface.
//!
//! This groups assembler-oriented VM functionality, including statement
//! parsing, selector/encoding helpers, and CPU-family VM helpers.

use opcore::parser::{BinaryOp, Expr, LineAst, ParseError, UnaryOp};
use opcore::tokenizer::{OperatorKind, Span, Token, TokenKind};
use registry::family::AssemblerContext;
use registry::registry::OperandSet;
use registry::syntax::RegisterChecker;
use std::io;
use std::path::PathBuf;
use types::artifacts::{LabelOutputFormat, OutputFormat};
use types::image::ImageStore;
use types::symbol::SymbolTable;

pub use crate::builder;
pub use crate::execution_model::apply_token_policy_to_token;
pub use crate::intel8080_vm;
pub use crate::listing::{ListingLine, ListingWriter};
pub use crate::native6502;
pub use crate::native6502_abi;
pub use crate::output_artifacts::{
    build_export_sections_payloads, build_linker_output_payload, build_mapfile_text,
    ArtifactBuildError,
};
pub use crate::output_model::{
    parse_bin_output_arg, parse_bin_range_str, resolve_bin_path, resolve_output_base,
    resolve_output_path, BinOutputSpec, BinRange, ExportSectionsDirective, ExportSectionsFormat,
    ExportSectionsInclude, HunkMemoryType, HunkOutputInput, HunkSegmentInput,
    LinkerOutputDirective, LinkerOutputFormat, LinkerOutputRelocationDisposition, MapFileDirective,
    MapSymbolsMode, OutputConfig, PlacedSectionInfo, PlacementDirective, RegionState, RootMetadata,
    SectionKind, SectionOptions, SectionState,
};
pub use crate::selector_encoding_utils;
pub use crate::vm_core::HierarchyExecutionModel;
pub use crate::vm_opasm_parse::{DynExprProcessingHandler, ExprProcessingHandler};

/// Runnable `.opasm` VM stage: tokenize an assembler line for parser-VM
/// processing using the active CPU pipeline.
pub fn tokenize_statement_line_with_model(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
    register_checker: &RegisterChecker,
) -> Result<(Vec<Token>, Span, Option<String>), ParseError> {
    crate::vm_opasm_parse::tokenize_parser_tokens_with_model(
        model,
        cpu_id,
        dialect_override,
        line,
        line_num,
        register_checker,
    )
}

/// Runnable `.opasm` VM stage: parse an assembler line using the active
/// parser-VM program and optional expression hand-off.
pub fn parse_statement_line_with_model(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
    register_checker: &RegisterChecker,
) -> Result<(LineAst, Span, Option<String>), ParseError> {
    crate::vm_opasm_parse::parse_line_with_model(
        model,
        cpu_id,
        dialect_override,
        line,
        line_num,
        register_checker,
    )
}

/// Runnable `.opasm` VM stage: parse an assembler line while delegating nested
/// expression requests through the engine-managed expression handler.
pub fn parse_statement_line_with_model_and_expr_handler<'a>(
    model: &'a HierarchyExecutionModel,
    cpu_id: &'a str,
    dialect_override: Option<&'a str>,
    line: &'a str,
    line_num: u32,
    register_checker: &RegisterChecker,
    expr_handler: Option<DynExprProcessingHandler<'a>>,
) -> Result<(LineAst, Span, Option<String>), ParseError> {
    crate::vm_opasm_parse::parse_line_with_model_with_expr_handler(
        model,
        cpu_id,
        dialect_override,
        line,
        line_num,
        register_checker,
        expr_handler,
    )
}

/// Runnable `.opasm` VM stage: parse a portable assembler line through the
/// active VM-backed CPU pipeline.
pub fn parse_portable_line_for_assembler(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
) -> Result<crate::portable_contract::PortableLineAst, ParseError> {
    crate::vm_opasm_parse::parse_portable_line_for_assembler(
        model,
        cpu_id,
        dialect_override,
        line,
        line_num,
    )
}

/// Runnable `.opasm` VM stage: map portable runtime tokens into core-token
/// form for assembler parsing.
pub fn map_runtime_tokens_to_core_tokens(
    tokens: &[crate::portable_contract::PortableToken],
    register_checker: &RegisterChecker,
) -> Result<Vec<Token>, ParseError> {
    crate::tokenizer_runtime_utils::runtime_tokens_to_core_tokens(tokens, None, register_checker)
}

/// Runnable `.opasm` VM stage: encode an instruction from resolved operands
/// through the VM-backed selector/encoder pipeline.
pub fn encode_instruction(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    mnemonic: &str,
    operands: &dyn OperandSet,
) -> Result<Option<Vec<u8>>, crate::runtime_error::RuntimeBridgeError> {
    model.encode_instruction(cpu_id, dialect_override, mnemonic, operands)
}

/// Runnable `.opasm` VM stage: resolve selector candidates from expressions
/// and encode through the VM-backed selector/encoder pipeline.
pub fn encode_instruction_from_exprs(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    mnemonic: &str,
    operands: &[Expr],
    ctx: &dyn AssemblerContext,
) -> Result<Option<Vec<u8>>, crate::runtime_error::RuntimeBridgeError> {
    model.encode_instruction_from_exprs(cpu_id, dialect_override, mnemonic, operands, ctx)
}

pub fn expr_resolution_is_strict_for_family(
    model: &HierarchyExecutionModel,
    family_id: &str,
) -> bool {
    model.expr_resolution_is_strict_for_family(family_id)
}

pub fn defer_native_diagnostics_on_expr_none(
    model: &HierarchyExecutionModel,
    family_id: &str,
) -> bool {
    model.defer_native_diagnostics_on_expr_none(family_id)
}

pub fn selector_gate_only_expr_runtime_for_cpu(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
) -> bool {
    model.selector_gate_only_expr_runtime_for_cpu(cpu_id)
}

/// Runnable `.opasm` VM stage: emit Intel HEX payload bytes from the current
/// assembled image.
pub fn build_hex_output_payload(image: &ImageStore, go_addr: Option<&str>) -> io::Result<Vec<u8>> {
    let mut out = Vec::new();
    image.write_hex_file(&mut out, go_addr)?;
    Ok(out)
}

/// Runnable `.opasm` VM stage: emit Motorola S-record payload bytes from the
/// current assembled image.
pub fn build_srec_output_payload(image: &ImageStore, go_addr: Option<&str>) -> io::Result<Vec<u8>> {
    let mut out = Vec::new();
    image.write_srec_file(&mut out, go_addr)?;
    Ok(out)
}

/// Runnable `.opasm` VM stage: emit raw binary payload bytes from the current
/// assembled image for the requested range.
pub fn build_bin_output_payload(
    image: &ImageStore,
    start: u32,
    end: u32,
    fill_byte: u8,
) -> io::Result<Vec<u8>> {
    let mut out = Vec::new();
    image.write_bin_file(&mut out, start, end, fill_byte)?;
    Ok(out)
}

/// Runnable `.opasm` VM stage: render label output text or JSON from the
/// current assembled symbol table.
pub fn render_labels(
    format: LabelOutputFormat,
    output_format: OutputFormat,
    symbols: &SymbolTable,
) -> String {
    types::artifacts::render_labels(format, output_format, symbols)
}

/// Runnable `.opasm` VM stage: render dependency output for produced targets.
pub fn render_dependencies(
    output_format: OutputFormat,
    targets: &[String],
    dependencies: &[PathBuf],
    make_phony: bool,
) -> Option<String> {
    types::artifacts::render_dependencies(output_format, targets, dependencies, make_phony)
}

#[derive(Clone, Copy, Default)]
pub(crate) struct OperandExprParseHints<'a> {
    pub(crate) mnemonic: Option<&'a str>,
    pub(crate) operand_index: usize,
}

#[derive(Clone, Default)]
pub(crate) struct OperandExprBoundary {
    pub(crate) end_span: Span,
    pub(crate) end_token_text: Option<String>,
}

type ExprSliceParser<'a> =
    dyn for<'tokens> FnMut(&'tokens [Token], Span, Option<String>) -> Result<Expr, ParseError> + 'a;

pub(crate) fn parse_operand_expr_range(
    tokens: &[Token],
    start: usize,
    end: usize,
    boundary: OperandExprBoundary,
    hints: OperandExprParseHints<'_>,
    expr_parse_ctx: &crate::vm_opasm_parse::VmExprParseContext<'_>,
    operands: &mut Vec<Expr>,
) -> Result<(), ParseError> {
    if start >= end {
        let span = tokens
            .get(start)
            .map(|token| token.span)
            .unwrap_or(boundary.end_span);
        operands.push(Expr::Error("Expected expression".to_string(), span));
        return Ok(());
    }
    expr_parse_ctx
        .model
        .ensure_parser_vm_v2_expr_subcall_contract_for_assembler(
            expr_parse_ctx.cpu_id,
            expr_parse_ctx.dialect_override,
        )
        .map_err(|err| {
            crate::runtime_parse_utils::runtime_bridge_error_to_parse_error(err, boundary.end_span)
        })?;
    let boundary_token = tokens.get(end);
    let expr_end_span = boundary_token
        .map(|token| token.span)
        .unwrap_or(boundary.end_span);
    let mut parse_inner =
        |inner_tokens: &[Token], inner_end_span: Span, inner_end_token_text: Option<String>| {
            parse_expr_slice(
                expr_parse_ctx,
                inner_tokens,
                inner_end_span,
                inner_end_token_text,
            )
        };
    if let Some(expr) = parse_generic_operand_wrapper(
        &tokens[start..end],
        expr_end_span,
        boundary.end_token_text.clone(),
        &mut parse_inner,
    ) {
        operands.push(expr);
        return Ok(());
    }
    let family_id = resolve_operand_family_id(expr_parse_ctx, expr_end_span)?;
    if family_allows_m6800_indexed_register_postfix(family_id.as_str()) {
        if let Some(expr) = parse_indexed_register_postfix_operand(&tokens[start..end]) {
            operands.push(expr);
            return Ok(());
        }
    }
    if family_allows_m68k_operand_shapes(family_id.as_str()) {
        if let Some(expr) = parse_m68k_texture_operand(
            &tokens[start..end],
            hints.mnemonic,
            hints.operand_index,
            expr_parse_ctx,
            expr_end_span,
            boundary.end_token_text.clone(),
        )? {
            operands.push(expr);
            return Ok(());
        }
        if let Some(expr) = parse_m68k_postincrement_operand(
            &tokens[start..end],
            expr_end_span,
            boundary.end_token_text.clone(),
            |inner_tokens, inner_end_span, inner_end_token_text| {
                parse_expr_slice(
                    expr_parse_ctx,
                    inner_tokens,
                    inner_end_span,
                    inner_end_token_text,
                )
            },
        ) {
            operands.push(expr);
            return Ok(());
        }
        if let Some(expr) = parse_m68k_predecrement_operand(
            &tokens[start..end],
            expr_end_span,
            boundary.end_token_text.clone(),
            |inner_tokens, inner_end_span, inner_end_token_text| {
                parse_expr_slice(
                    expr_parse_ctx,
                    inner_tokens,
                    inner_end_span,
                    inner_end_token_text,
                )
            },
        ) {
            operands.push(expr);
            return Ok(());
        }
        if let Some(expr) = parse_m68k_postfix_tuple_operand(
            &tokens[start..end],
            |inner_tokens, inner_end_span, inner_end_token_text| {
                parse_expr_slice(
                    expr_parse_ctx,
                    inner_tokens,
                    inner_end_span,
                    inner_end_token_text,
                )
            },
        ) {
            operands.push(expr);
            return Ok(());
        }
        if let Some(expr) = parse_bitfield_suffix_operand(
            &tokens[start..end],
            hints.mnemonic,
            hints.operand_index,
            expr_parse_ctx,
            expr_end_span,
            boundary.end_token_text.clone(),
            boundary_token,
        )? {
            operands.push(expr);
            return Ok(());
        }
        if let Some(expr) = parse_register_pair_operand(
            &tokens[start..end],
            hints.mnemonic,
            hints.operand_index,
            expr_parse_ctx,
            expr_end_span,
            boundary.end_token_text.clone(),
            boundary_token,
        )? {
            operands.push(expr);
            return Ok(());
        }
    }
    match crate::vm_opcore::parse_expr_with_authoritative_exvm_contract_and_boundary(
        expr_parse_ctx,
        &tokens[start..end],
        expr_end_span,
        boundary.end_token_text,
        boundary_token,
    ) {
        Ok(expr) => operands.push(expr),
        Err(err) => operands.push(Expr::Error(err.message, err.span)),
    }
    Ok(())
}

fn resolve_operand_family_id(
    expr_parse_ctx: &crate::vm_opasm_parse::VmExprParseContext<'_>,
    end_span: Span,
) -> Result<String, ParseError> {
    expr_parse_ctx
        .model
        .resolve_pipeline(expr_parse_ctx.cpu_id, expr_parse_ctx.dialect_override)
        .map(|resolved| resolved.family_id)
        .map_err(|err| {
            crate::runtime_parse_utils::runtime_bridge_error_to_parse_error(err, end_span)
        })
}

fn family_allows_m68k_operand_shapes(family_id: &str) -> bool {
    family_id.eq_ignore_ascii_case("motorola68000")
}

fn family_allows_m6800_indexed_register_postfix(family_id: &str) -> bool {
    family_id.eq_ignore_ascii_case("motorola6800")
}

fn base_mnemonic_name(name: &str) -> &str {
    name.split('.').next().unwrap_or(name)
}

fn is_m68k_cas2_mnemonic(name: &str) -> bool {
    base_mnemonic_name(name).eq_ignore_ascii_case("CAS2")
}

fn is_m68k_long_divide_pair_mnemonic(name: &str) -> bool {
    matches!(
        base_mnemonic_name(name).to_ascii_uppercase().as_str(),
        "DIVS" | "DIVU" | "DIVSL" | "DIVUL"
    )
}

fn is_m68k_tex_mnemonic(name: &str) -> bool {
    matches!(
        base_mnemonic_name(name).to_ascii_uppercase().as_str(),
        "TEX8" | "TEX16" | "TEX24" | "TEX"
    )
}

fn is_m68k_bitfield_operand(name: &str, operand_index: usize) -> bool {
    match base_mnemonic_name(name).to_ascii_uppercase().as_str() {
        "BFINS" => operand_index == 1,
        "BFTST" | "BFEXTU" | "BFCHG" | "BFEXTS" | "BFCLR" | "BFFFO" | "BFSET" => operand_index == 0,
        _ => false,
    }
}

fn parse_expr_slice(
    expr_parse_ctx: &crate::vm_opasm_parse::VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
) -> Result<Expr, ParseError> {
    crate::vm_opcore::parse_expr_with_authoritative_exvm_contract_and_boundary(
        expr_parse_ctx,
        tokens,
        end_span,
        end_token_text,
        None,
    )
}

fn build_call_expr(name: &str, args: Vec<Expr>) -> Expr {
    let start_span = opcore::expression::expr_span(args.first().expect("call requires args"));
    let end_span = opcore::expression::expr_span(args.last().expect("call requires args"));
    Expr::Call {
        name: name.to_string(),
        args,
        span: Span {
            line: start_span.line,
            col_start: start_span.col_start,
            col_end: end_span.col_end,
        },
    }
}

fn parse_generic_operand_wrapper(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
    parse_inner: &mut ExprSliceParser<'_>,
) -> Option<Expr> {
    let first = tokens.first()?;
    if matches!(first.kind, TokenKind::Hash) {
        let inner = parse_inner_or_error(parse_inner, &tokens[1..], end_span, end_token_text);
        if matches!(inner, Expr::Error(_, _)) {
            return Some(inner);
        }
        let inner_span = opcore::expression::expr_span(&inner);
        return Some(Expr::Immediate(
            Box::new(inner),
            Span {
                line: first.span.line,
                col_start: first.span.col_start,
                col_end: tokens
                    .last()
                    .map(|token| token.span.col_end)
                    .unwrap_or(inner_span.col_end),
            },
        ));
    }

    if is_single_wrapped_operand(tokens, TokenKind::OpenParen, TokenKind::CloseParen) {
        if contains_top_level_comma(&tokens[1..tokens.len() - 1]) {
            let close_span = tokens[tokens.len() - 1].span;
            let elements = parse_wrapped_tuple_elements(tokens, close_span, ")", parse_inner);
            let span = Span {
                line: first.span.line,
                col_start: first.span.col_start,
                col_end: close_span.col_end,
            };
            return Some(Expr::Indirect(Box::new(Expr::Tuple(elements, span)), span));
        }
        let close_span = tokens[tokens.len() - 1].span;
        let inner = parse_inner_or_error(
            parse_inner,
            &tokens[1..tokens.len() - 1],
            close_span,
            Some(")".to_string()),
        );
        return Some(Expr::Indirect(
            Box::new(inner),
            Span {
                line: first.span.line,
                col_start: first.span.col_start,
                col_end: close_span.col_end,
            },
        ));
    }

    if is_single_wrapped_operand(tokens, TokenKind::OpenBracket, TokenKind::CloseBracket) {
        if contains_top_level_comma(&tokens[1..tokens.len() - 1]) {
            let close_span = tokens[tokens.len() - 1].span;
            let elements = parse_wrapped_tuple_elements(tokens, close_span, "]", parse_inner);
            let span = Span {
                line: first.span.line,
                col_start: first.span.col_start,
                col_end: close_span.col_end,
            };
            return Some(Expr::IndirectLong(
                Box::new(Expr::Tuple(elements, span)),
                span,
            ));
        }
        let close_span = tokens[tokens.len() - 1].span;
        let inner = parse_inner_or_error(
            parse_inner,
            &tokens[1..tokens.len() - 1],
            close_span,
            Some("]".to_string()),
        );
        return Some(Expr::IndirectLong(
            Box::new(inner),
            Span {
                line: first.span.line,
                col_start: first.span.col_start,
                col_end: close_span.col_end,
            },
        ));
    }

    None
}

fn parse_wrapped_tuple_elements(
    tokens: &[Token],
    close_span: Span,
    close_token_text: &str,
    parse_inner: &mut ExprSliceParser<'_>,
) -> Vec<Expr> {
    parse_tuple_elements(
        tokens,
        1,
        tokens.len().saturating_sub(1),
        close_span,
        close_token_text,
        parse_inner,
    )
}

fn parse_tuple_elements(
    tokens: &[Token],
    start: usize,
    end: usize,
    close_span: Span,
    close_token_text: &str,
    parse_inner: &mut ExprSliceParser<'_>,
) -> Vec<Expr> {
    split_top_level_comma_ranges(tokens, start, end)
        .into_iter()
        .map(|(start, end)| {
            let (element_end_span, element_end_token_text) = if let Some(comma) = tokens
                .get(end)
                .filter(|token| matches!(token.kind, TokenKind::Comma))
            {
                (comma.span, Some(",".to_string()))
            } else {
                (close_span, Some(close_token_text.to_string()))
            };
            if start == end {
                return Expr::Placeholder(element_end_span);
            }
            if let Some(expr) = parse_generic_operand_wrapper(
                &tokens[start..end],
                element_end_span,
                element_end_token_text.clone(),
                &mut *parse_inner,
            ) {
                return expr;
            }
            parse_inner_or_error(
                parse_inner,
                &tokens[start..end],
                element_end_span,
                element_end_token_text,
            )
        })
        .collect()
}

fn parse_m68k_postincrement_operand<F>(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
    mut parse_inner: F,
) -> Option<Expr>
where
    F: FnMut(&[Token], Span, Option<String>) -> Result<Expr, ParseError>,
{
    let plus = tokens.last()?;
    if !matches!(plus.kind, TokenKind::Operator(OperatorKind::Plus)) {
        return None;
    }
    let Expr::Indirect(inner, indirect_span) = parse_generic_operand_wrapper(
        &tokens[..tokens.len().saturating_sub(1)],
        end_span,
        end_token_text,
        &mut parse_inner,
    )?
    else {
        return None;
    };
    Some(Expr::Unary {
        op: UnaryOp::Plus,
        expr: Box::new(Expr::Indirect(inner, indirect_span)),
        span: Span {
            line: indirect_span.line,
            col_start: indirect_span.col_start,
            col_end: plus.span.col_end,
        },
    })
}

fn parse_m68k_predecrement_operand<F>(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
    mut parse_inner: F,
) -> Option<Expr>
where
    F: FnMut(&[Token], Span, Option<String>) -> Result<Expr, ParseError>,
{
    let minus = tokens.first()?;
    if !matches!(minus.kind, TokenKind::Operator(OperatorKind::Minus)) {
        return None;
    }
    let Expr::Indirect(inner, indirect_span) =
        parse_generic_operand_wrapper(&tokens[1..], end_span, end_token_text, &mut parse_inner)?
    else {
        return None;
    };
    Some(Expr::Unary {
        op: UnaryOp::Minus,
        expr: Box::new(Expr::Indirect(inner, indirect_span)),
        span: minus.span,
    })
}

fn parse_m68k_postfix_tuple_operand<F>(tokens: &[Token], mut parse_inner: F) -> Option<Expr>
where
    F: FnMut(&[Token], Span, Option<String>) -> Result<Expr, ParseError>,
{
    if matches!(
        tokens.first().map(|token| &token.kind),
        Some(TokenKind::Dot)
    ) {
        return None;
    }

    let close = tokens.last()?;
    if !matches!(close.kind, TokenKind::CloseParen) {
        return None;
    }

    let open_index = find_adjacent_top_level_open_paren(tokens)?;
    if open_index == 0 || open_index + 1 >= tokens.len().saturating_sub(1) {
        return None;
    }
    if matches!(tokens[open_index - 1].kind, TokenKind::Colon) {
        return None;
    }
    if !top_level_group_closes_at_end(tokens, open_index) {
        return None;
    }

    let base = parse_inner_or_error(
        &mut parse_inner,
        &tokens[..open_index],
        tokens[open_index].span,
        Some("(".to_string()),
    );
    let mut elements = vec![base];
    elements.extend(parse_tuple_elements(
        tokens,
        open_index + 1,
        tokens.len().saturating_sub(1),
        close.span,
        ")",
        &mut parse_inner,
    ));

    let start_span = opcore::expression::expr_span(&elements[0]);
    let span = Span {
        line: start_span.line,
        col_start: start_span.col_start,
        col_end: close.span.col_end,
    };
    Some(Expr::Indirect(Box::new(Expr::Tuple(elements, span)), span))
}

fn find_adjacent_top_level_open_paren(tokens: &[Token]) -> Option<usize> {
    let mut depth_paren = 0i32;
    let mut depth_bracket = 0i32;
    let mut depth_brace = 0i32;

    for (index, token) in tokens.iter().enumerate() {
        if depth_paren == 0
            && depth_bracket == 0
            && depth_brace == 0
            && matches!(token.kind, TokenKind::OpenParen)
            && index > 0
            && token.span.col_start == tokens[index - 1].span.col_end
        {
            return Some(index);
        }
        update_group_depths_for_token(
            &token.kind,
            &mut depth_paren,
            &mut depth_bracket,
            &mut depth_brace,
        );
    }

    None
}

fn top_level_group_closes_at_end(tokens: &[Token], open_index: usize) -> bool {
    let mut depth = 0i32;
    for (index, token) in tokens.iter().enumerate().skip(open_index) {
        match token.kind {
            TokenKind::OpenParen => depth += 1,
            TokenKind::CloseParen => {
                depth -= 1;
                if depth == 0 {
                    return index == tokens.len().saturating_sub(1);
                }
                if depth < 0 {
                    return false;
                }
            }
            _ => {}
        }
    }

    false
}

fn parse_inner_or_error(
    parse_inner: &mut ExprSliceParser<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
) -> Expr {
    match parse_inner(tokens, end_span, end_token_text) {
        Ok(expr) => expr,
        Err(err) => Expr::Error(err.message, err.span),
    }
}

fn is_single_wrapped_operand(tokens: &[Token], open: TokenKind, close: TokenKind) -> bool {
    if tokens.len() < 2 || tokens.first().map(|token| &token.kind) != Some(&open) {
        return false;
    }
    if tokens.last().map(|token| &token.kind) != Some(&close) {
        return false;
    }

    let mut depth = 0i32;
    for (index, token) in tokens.iter().enumerate() {
        if token.kind == open {
            depth += 1;
        } else if token.kind == close {
            depth -= 1;
            if depth == 0 && index != tokens.len() - 1 {
                return false;
            }
            if depth < 0 {
                return false;
            }
        }
    }

    depth == 0
}

fn contains_top_level_comma(tokens: &[Token]) -> bool {
    let mut depth_paren = 0i32;
    let mut depth_bracket = 0i32;
    let mut depth_brace = 0i32;

    for token in tokens {
        if matches!(token.kind, TokenKind::Comma)
            && depth_paren == 0
            && depth_bracket == 0
            && depth_brace == 0
        {
            return true;
        }
        update_group_depths_for_token(
            &token.kind,
            &mut depth_paren,
            &mut depth_bracket,
            &mut depth_brace,
        );
    }

    false
}

fn parse_m68k_wrapped_operand_or_expr(
    expr_parse_ctx: &crate::vm_opasm_parse::VmExprParseContext<'_>,
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
) -> Result<Expr, ParseError> {
    let mut parse_inner =
        |inner_tokens: &[Token], inner_end_span: Span, inner_end_token_text: Option<String>| {
            parse_expr_slice(
                expr_parse_ctx,
                inner_tokens,
                inner_end_span,
                inner_end_token_text,
            )
        };
    if let Some(expr) =
        parse_generic_operand_wrapper(tokens, end_span, end_token_text.clone(), &mut parse_inner)
    {
        return Ok(expr);
    }

    parse_expr_slice(expr_parse_ctx, tokens, end_span, end_token_text)
}

fn find_top_level_multiply(tokens: &[Token]) -> Option<usize> {
    let mut depth_paren = 0i32;
    let mut depth_bracket = 0i32;
    let mut depth_brace = 0i32;

    for (index, token) in tokens.iter().enumerate() {
        if depth_paren == 0
            && depth_bracket == 0
            && depth_brace == 0
            && matches!(token.kind, TokenKind::Operator(OperatorKind::Multiply))
        {
            return Some(index);
        }
        update_group_depths_for_token(
            &token.kind,
            &mut depth_paren,
            &mut depth_bracket,
            &mut depth_brace,
        );
    }

    None
}

fn parse_m68k_texture_operand(
    tokens: &[Token],
    mnemonic: Option<&str>,
    operand_index: usize,
    expr_parse_ctx: &crate::vm_opasm_parse::VmExprParseContext<'_>,
    end_span: Span,
    end_token_text: Option<String>,
) -> Result<Option<Expr>, ParseError> {
    if operand_index != 0 || !mnemonic.is_some_and(is_m68k_tex_mnemonic) {
        return Ok(None);
    }

    let Some(multiply_index) = find_top_level_multiply(tokens) else {
        return Ok(None);
    };
    if multiply_index == 0 || multiply_index + 1 >= tokens.len() {
        return Ok(None);
    }

    let left = parse_m68k_wrapped_operand_or_expr(
        expr_parse_ctx,
        &tokens[..multiply_index],
        tokens[multiply_index].span,
        Some("*".to_string()),
    )?;
    let right = parse_expr_slice(
        expr_parse_ctx,
        &tokens[multiply_index + 1..],
        end_span,
        end_token_text,
    )?;
    let left_span = opcore::expression::expr_span(&left);
    let right_span = opcore::expression::expr_span(&right);

    Ok(Some(Expr::Binary {
        op: BinaryOp::Multiply,
        left: Box::new(left),
        right: Box::new(right),
        span: Span {
            line: left_span.line,
            col_start: left_span.col_start,
            col_end: right_span.col_end,
        },
    }))
}

fn parse_register_pair_operand(
    tokens: &[Token],
    mnemonic: Option<&str>,
    operand_index: usize,
    expr_parse_ctx: &crate::vm_opasm_parse::VmExprParseContext<'_>,
    end_span: Span,
    end_token_text: Option<String>,
    _boundary_token: Option<&Token>,
) -> Result<Option<Expr>, ParseError> {
    let allow_pair = (mnemonic.is_some_and(is_m68k_cas2_mnemonic) && operand_index <= 2)
        || (mnemonic.is_some_and(is_m68k_long_divide_pair_mnemonic) && operand_index == 1);
    if !allow_pair {
        return Ok(None);
    }

    let mut depth_paren = 0i32;
    let mut depth_bracket = 0i32;
    let mut depth_brace = 0i32;
    let mut colon_index = None;

    for (index, token) in tokens.iter().enumerate() {
        if depth_paren == 0
            && depth_bracket == 0
            && depth_brace == 0
            && matches!(token.kind, TokenKind::Colon)
        {
            colon_index = Some(index);
            break;
        }
        update_group_depths_for_token(
            &token.kind,
            &mut depth_paren,
            &mut depth_bracket,
            &mut depth_brace,
        );
    }

    let Some(colon_index) = colon_index else {
        return Ok(None);
    };
    let left = parse_m68k_wrapped_operand_or_expr(
        expr_parse_ctx,
        &tokens[..colon_index],
        tokens[colon_index].span,
        Some(":".to_string()),
    )?;
    let right = parse_m68k_wrapped_operand_or_expr(
        expr_parse_ctx,
        &tokens[colon_index + 1..],
        end_span,
        end_token_text,
    )?;

    Ok(Some(build_call_expr(".pair", vec![left, right])))
}

fn parse_bitfield_suffix_operand(
    tokens: &[Token],
    mnemonic: Option<&str>,
    operand_index: usize,
    expr_parse_ctx: &crate::vm_opasm_parse::VmExprParseContext<'_>,
    end_span: Span,
    end_token_text: Option<String>,
    _boundary_token: Option<&Token>,
) -> Result<Option<Expr>, ParseError> {
    if !mnemonic.is_some_and(|name| is_m68k_bitfield_operand(name, operand_index)) {
        return Ok(None);
    }

    let Some(last) = tokens.last() else {
        return Ok(None);
    };
    if !matches!(last.kind, TokenKind::CloseBrace) {
        return Ok(None);
    }

    let mut depth_paren = 0i32;
    let mut depth_bracket = 0i32;
    let mut depth_brace = 0i32;
    let mut open_brace_index = None;

    for (index, token) in tokens.iter().enumerate() {
        if depth_paren == 0
            && depth_bracket == 0
            && depth_brace == 0
            && matches!(token.kind, TokenKind::OpenBrace)
        {
            open_brace_index = Some(index);
            break;
        }
        update_group_depths_for_token(
            &token.kind,
            &mut depth_paren,
            &mut depth_bracket,
            &mut depth_brace,
        );
    }

    let Some(open_brace_index) = open_brace_index else {
        return Ok(None);
    };
    if open_brace_index == 0 || open_brace_index + 1 >= tokens.len() {
        return Ok(None);
    }

    let mut inner_depth_paren = 0i32;
    let mut inner_depth_bracket = 0i32;
    let mut inner_depth_brace = 0i32;
    let mut colon_index = None;
    for (offset, token) in tokens[open_brace_index + 1..tokens.len() - 1]
        .iter()
        .enumerate()
    {
        if inner_depth_paren == 0
            && inner_depth_bracket == 0
            && inner_depth_brace == 0
            && matches!(token.kind, TokenKind::Colon)
        {
            colon_index = Some(open_brace_index + 1 + offset);
            break;
        }
        update_group_depths_for_token(
            &token.kind,
            &mut inner_depth_paren,
            &mut inner_depth_bracket,
            &mut inner_depth_brace,
        );
    }

    let Some(colon_index) = colon_index else {
        return Ok(Some(Expr::Error(
            "Expected ':' in bit-field selector".to_string(),
            last.span,
        )));
    };

    let base = parse_m68k_wrapped_operand_or_expr(
        expr_parse_ctx,
        &tokens[..open_brace_index],
        tokens[open_brace_index].span,
        Some("{".to_string()),
    )?;
    let offset = parse_expr_slice(
        expr_parse_ctx,
        &tokens[open_brace_index + 1..colon_index],
        tokens[colon_index].span,
        Some(":".to_string()),
    )?;
    let width = parse_expr_slice(
        expr_parse_ctx,
        &tokens[colon_index + 1..tokens.len() - 1],
        end_span,
        end_token_text,
    )?;

    Ok(Some(build_call_expr(
        ".bitfield",
        vec![base, offset, width],
    )))
}

fn parse_indexed_register_postfix_operand(tokens: &[Token]) -> Option<Expr> {
    if tokens.len() < 2 || tokens.len() > 3 {
        return None;
    }
    let (name, start_span) = match &tokens[0].kind {
        TokenKind::Register(name) | TokenKind::Identifier(name) => (name.clone(), tokens[0].span),
        _ => return None,
    };
    let plus1 = matches!(tokens[1].kind, TokenKind::Operator(OperatorKind::Plus));
    if !plus1 {
        return None;
    }
    let suffix = if tokens.len() == 3 {
        if matches!(tokens[2].kind, TokenKind::Operator(OperatorKind::Plus)) {
            "++"
        } else {
            return None;
        }
    } else {
        "+"
    };
    let end_span = tokens[tokens.len() - 1].span;
    Some(Expr::Register(
        format!("{name}{suffix}"),
        Span {
            line: start_span.line,
            col_start: start_span.col_start,
            col_end: end_span.col_end,
        },
    ))
}

pub(crate) fn update_group_depths_for_token(
    kind: &TokenKind,
    depth_paren: &mut i32,
    depth_bracket: &mut i32,
    depth_brace: &mut i32,
) {
    match kind {
        TokenKind::OpenParen => *depth_paren = depth_paren.saturating_add(1),
        TokenKind::CloseParen => *depth_paren = depth_paren.saturating_sub(1),
        TokenKind::OpenBracket => *depth_bracket = depth_bracket.saturating_add(1),
        TokenKind::CloseBracket => *depth_bracket = depth_bracket.saturating_sub(1),
        TokenKind::OpenBrace => *depth_brace = depth_brace.saturating_add(1),
        TokenKind::CloseBrace => *depth_brace = depth_brace.saturating_sub(1),
        _ => {}
    }
}

pub(crate) fn split_top_level_comma_ranges(
    tokens: &[Token],
    start: usize,
    end: usize,
) -> Vec<(usize, usize)> {
    let mut ranges = Vec::new();
    if start >= end {
        return ranges;
    }

    let mut depth_paren = 0i32;
    let mut depth_bracket = 0i32;
    let mut depth_brace = 0i32;
    let mut current_start = start;

    for (cursor, token) in tokens.iter().enumerate().take(end).skip(start) {
        update_group_depths_for_token(
            &token.kind,
            &mut depth_paren,
            &mut depth_bracket,
            &mut depth_brace,
        );
        if matches!(token.kind, TokenKind::Comma)
            && depth_paren == 0
            && depth_bracket == 0
            && depth_brace == 0
        {
            ranges.push((current_start, cursor));
            current_start = cursor.saturating_add(1);
        }
    }

    ranges.push((current_start, end));
    ranges
}

pub fn load_model_from_registry(
    registry: &registry::registry::ModuleRegistry,
) -> Result<HierarchyExecutionModel, crate::vm_core::RuntimeModelLoadError> {
    crate::vm_core::load_execution_model_from_registry(registry)
}

pub fn load_model_from_chunks(
    chunks: package::HierarchyChunks,
) -> Result<HierarchyExecutionModel, crate::vm_core::RuntimeModelLoadError> {
    crate::vm_core::load_execution_model_from_chunks(chunks)
}

pub fn load_model_from_package_bytes(
    bytes: &[u8],
) -> Result<HierarchyExecutionModel, crate::vm_core::RuntimeModelLoadError> {
    crate::vm_core::load_execution_model_from_package_bytes(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use opcore::tokenizer::NumberLiteral;

    fn span(col_start: usize, col_end: usize) -> Span {
        Span {
            line: 1,
            col_start,
            col_end,
        }
    }

    fn token(kind: TokenKind, col_start: usize, col_end: usize) -> Token {
        Token {
            kind,
            span: span(col_start, col_end),
        }
    }

    fn ident(name: &str, col_start: usize, col_end: usize) -> Token {
        token(TokenKind::Identifier(name.to_string()), col_start, col_end)
    }

    fn number(text: &str, col_start: usize, col_end: usize) -> Token {
        token(
            TokenKind::Number(NumberLiteral {
                text: text.to_string(),
                base: 10,
            }),
            col_start,
            col_end,
        )
    }

    fn parse_test_expr(
        tokens: &[Token],
        end_span: Span,
        end_token_text: Option<String>,
    ) -> Result<Expr, ParseError> {
        match tokens {
            [Token {
                kind: TokenKind::Identifier(name),
                span,
            }] => Ok(Expr::Identifier(name.clone(), *span)),
            [Token {
                kind: TokenKind::Number(num),
                span,
            }] => Ok(Expr::Number(num.text.clone(), *span)),
            [Token {
                kind: TokenKind::Register(name),
                span,
            }] => Ok(Expr::Register(name.clone(), *span)),
            [] => Err(ParseError {
                message: match end_token_text {
                    Some(token) => format!("Expected label or numeric constant, found: {token}"),
                    None => "Unexpected end of expression".to_string(),
                },
                span: end_span,
            }),
            [token, ..] => Err(ParseError {
                message: "Unexpected token in expression".to_string(),
                span: token.span,
            }),
        }
    }

    fn parse_wrapper(tokens: &[Token]) -> Option<Expr> {
        let mut parse_inner = parse_test_expr;
        parse_generic_operand_wrapper(tokens, span(99, 99), None, &mut parse_inner)
    }

    fn parse_postincrement(tokens: &[Token]) -> Option<Expr> {
        parse_m68k_postincrement_operand(tokens, span(99, 99), None, parse_test_expr)
    }

    fn parse_predecrement(tokens: &[Token]) -> Option<Expr> {
        parse_m68k_predecrement_operand(tokens, span(99, 99), None, parse_test_expr)
    }

    fn parse_postfix_tuple(tokens: &[Token]) -> Option<Expr> {
        parse_m68k_postfix_tuple_operand(tokens, parse_test_expr)
    }

    #[test]
    fn vm_opasm_family_gate_allows_m68k_operand_shapes_only_for_motorola68000_family() {
        assert!(family_allows_m68k_operand_shapes("motorola68000"));
        assert!(family_allows_m68k_operand_shapes("MOTOROLA68000"));
        assert!(!family_allows_m68k_operand_shapes("motorola6800"));
        assert!(!family_allows_m68k_operand_shapes("mos6502"));
    }

    #[test]
    fn vm_opasm_family_gate_allows_indexed_register_postfix_only_for_motorola6800_family() {
        assert!(family_allows_m6800_indexed_register_postfix("motorola6800"));
        assert!(family_allows_m6800_indexed_register_postfix("MOTOROLA6800"));
        assert!(!family_allows_m6800_indexed_register_postfix(
            "motorola68000"
        ));
        assert!(!family_allows_m6800_indexed_register_postfix("intel8080"));
    }

    #[test]
    fn vm_opasm_generic_operand_wrapper_parses_immediate() {
        let tokens = vec![token(TokenKind::Hash, 1, 2), number("42", 2, 4)];

        let Some(Expr::Immediate(inner, wrapper_span)) = parse_wrapper(&tokens) else {
            panic!("expected immediate wrapper");
        };

        assert_eq!(wrapper_span, span(1, 4));
        assert!(matches!(*inner, Expr::Number(ref text, _) if text == "42"));
    }

    #[test]
    fn vm_opasm_generic_operand_wrapper_parses_parenthesized_indirect() {
        let tokens = vec![
            token(TokenKind::OpenParen, 1, 2),
            ident("label", 2, 7),
            token(TokenKind::CloseParen, 7, 8),
        ];

        let Some(Expr::Indirect(inner, wrapper_span)) = parse_wrapper(&tokens) else {
            panic!("expected parenthesized indirect wrapper");
        };

        assert_eq!(wrapper_span, span(1, 8));
        assert!(matches!(*inner, Expr::Identifier(ref name, _) if name == "label"));
    }

    #[test]
    fn vm_opasm_generic_operand_wrapper_parses_bracketed_indirect_long() {
        let tokens = vec![
            token(TokenKind::OpenBracket, 1, 2),
            number("4096", 2, 6),
            token(TokenKind::CloseBracket, 6, 7),
        ];

        let Some(Expr::IndirectLong(inner, wrapper_span)) = parse_wrapper(&tokens) else {
            panic!("expected bracketed indirect wrapper");
        };

        assert_eq!(wrapper_span, span(1, 7));
        assert!(matches!(*inner, Expr::Number(ref text, _) if text == "4096"));
    }

    #[test]
    fn vm_opasm_generic_operand_wrapper_parses_parenthesized_tuple_indirect() {
        let tokens = vec![
            token(TokenKind::OpenParen, 1, 2),
            ident("left", 2, 6),
            token(TokenKind::Comma, 6, 7),
            ident("right", 7, 12),
            token(TokenKind::CloseParen, 12, 13),
        ];

        let Some(Expr::Indirect(inner, wrapper_span)) = parse_wrapper(&tokens) else {
            panic!("expected parenthesized tuple indirect wrapper");
        };

        assert_eq!(wrapper_span, span(1, 13));
        let Expr::Tuple(elements, tuple_span) = *inner else {
            panic!("expected tuple inside indirect wrapper");
        };
        assert_eq!(tuple_span, span(1, 13));
        assert_eq!(elements.len(), 2);
        assert!(matches!(elements[0], Expr::Identifier(ref name, _) if name == "left"));
        assert!(matches!(elements[1], Expr::Identifier(ref name, _) if name == "right"));
    }

    #[test]
    fn vm_opasm_generic_operand_wrapper_parses_bracketed_tuple_indirect_long() {
        let tokens = vec![
            token(TokenKind::OpenBracket, 1, 2),
            ident("base", 2, 6),
            token(TokenKind::Comma, 6, 7),
            number("8", 7, 8),
            token(TokenKind::CloseBracket, 8, 9),
        ];

        let Some(Expr::IndirectLong(inner, wrapper_span)) = parse_wrapper(&tokens) else {
            panic!("expected bracketed tuple indirect wrapper");
        };

        assert_eq!(wrapper_span, span(1, 9));
        let Expr::Tuple(elements, tuple_span) = *inner else {
            panic!("expected tuple inside indirect-long wrapper");
        };
        assert_eq!(tuple_span, span(1, 9));
        assert_eq!(elements.len(), 2);
        assert!(matches!(elements[0], Expr::Identifier(ref name, _) if name == "base"));
        assert!(matches!(elements[1], Expr::Number(ref text, _) if text == "8"));
    }

    #[test]
    fn vm_opasm_generic_operand_wrapper_leaves_postfix_forms_for_later_shape_parsing() {
        let tokens = vec![
            token(TokenKind::OpenParen, 1, 2),
            ident("A0", 2, 4),
            token(TokenKind::CloseParen, 4, 5),
            token(TokenKind::Operator(OperatorKind::Plus), 5, 6),
        ];

        assert!(parse_wrapper(&tokens).is_none());
    }

    #[test]
    fn vm_opasm_m68k_operand_shape_parses_postincrement_indirect() {
        let tokens = vec![
            token(TokenKind::OpenParen, 1, 2),
            token(TokenKind::Register("A0".to_string()), 2, 4),
            token(TokenKind::CloseParen, 4, 5),
            token(TokenKind::Operator(OperatorKind::Plus), 5, 6),
        ];

        let Some(Expr::Unary {
            op: UnaryOp::Plus,
            expr,
            span: wrapper_span,
        }) = parse_postincrement(&tokens)
        else {
            panic!("expected m68k postincrement operand");
        };

        assert_eq!(wrapper_span, span(1, 6));
        let Expr::Indirect(inner, indirect_span) = *expr else {
            panic!("expected indirect inside postincrement");
        };
        assert_eq!(indirect_span, span(1, 5));
        assert!(matches!(*inner, Expr::Register(ref name, _) if name == "A0"));
    }

    #[test]
    fn vm_opasm_m68k_operand_shape_parses_predecrement_indirect() {
        let tokens = vec![
            token(TokenKind::Operator(OperatorKind::Minus), 1, 2),
            token(TokenKind::OpenParen, 2, 3),
            token(TokenKind::Register("A7".to_string()), 3, 5),
            token(TokenKind::CloseParen, 5, 6),
        ];

        let Some(Expr::Unary {
            op: UnaryOp::Minus,
            expr,
            span: wrapper_span,
        }) = parse_predecrement(&tokens)
        else {
            panic!("expected m68k predecrement operand");
        };

        assert_eq!(wrapper_span, span(1, 2));
        let Expr::Indirect(inner, indirect_span) = *expr else {
            panic!("expected indirect inside predecrement");
        };
        assert_eq!(indirect_span, span(2, 6));
        assert!(matches!(*inner, Expr::Register(ref name, _) if name == "A7"));
    }

    #[test]
    fn vm_opasm_m68k_operand_shape_parses_postfix_tuple_indirect() {
        let tokens = vec![
            number("4", 1, 2),
            token(TokenKind::OpenParen, 2, 3),
            token(TokenKind::Register("A0".to_string()), 3, 5),
            token(TokenKind::Comma, 5, 6),
            token(TokenKind::Register("D1".to_string()), 6, 8),
            token(TokenKind::CloseParen, 8, 9),
        ];

        let Some(Expr::Indirect(inner, wrapper_span)) = parse_postfix_tuple(&tokens) else {
            panic!("expected m68k postfix tuple indirect");
        };

        assert_eq!(wrapper_span, span(1, 9));
        let Expr::Tuple(elements, tuple_span) = *inner else {
            panic!("expected tuple inside postfix indirect");
        };
        assert_eq!(tuple_span, span(1, 9));
        assert_eq!(elements.len(), 3);
        assert!(matches!(elements[0], Expr::Number(ref text, _) if text == "4"));
        assert!(matches!(elements[1], Expr::Register(ref name, _) if name == "A0"));
        assert!(matches!(elements[2], Expr::Register(ref name, _) if name == "D1"));
    }

    #[test]
    fn vm_opasm_m68k_predecrement_preserves_syntactic_indirect_number() {
        let tokens = vec![
            token(TokenKind::Operator(OperatorKind::Minus), 1, 2),
            token(TokenKind::OpenParen, 2, 3),
            number("1", 3, 4),
            token(TokenKind::CloseParen, 4, 5),
        ];

        let Some(Expr::Unary {
            op: UnaryOp::Minus,
            expr,
            span: wrapper_span,
        }) = parse_predecrement(&tokens)
        else {
            panic!("expected m68k predecrement operand");
        };

        assert_eq!(wrapper_span, span(1, 2));
        let Expr::Indirect(inner, indirect_span) = *expr else {
            panic!("expected indirect inside predecrement");
        };
        assert_eq!(indirect_span, span(2, 5));
        assert!(matches!(*inner, Expr::Number(ref text, _) if text == "1"));
    }
}
