// SPDX-License-Identifier: GPL-3.0-or-later

//! `.opasm` VM surface.
//!
//! This groups assembler-oriented VM functionality, including statement
//! parsing, selector/encoding helpers, and CPU-family VM helpers.

use opcore::parser::{Expr, LineAst, ParseError};
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
    ExportSectionsInclude, LinkerOutputDirective, LinkerOutputFormat, MapFileDirective,
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

pub(crate) fn parse_operand_expr_range(
    tokens: &[Token],
    start: usize,
    end: usize,
    end_span: Span,
    end_token_text: Option<String>,
    expr_parse_ctx: &crate::vm_opasm_parse::VmExprParseContext<'_>,
    operands: &mut Vec<Expr>,
) -> Result<(), ParseError> {
    if start >= end {
        let span = tokens
            .get(start)
            .map(|token| token.span)
            .unwrap_or(end_span);
        operands.push(Expr::Error("Expected expression".to_string(), span));
        return Ok(());
    }
    let boundary_token = tokens.get(end);
    let expr_end_span = boundary_token.map(|token| token.span).unwrap_or(end_span);
    if let Some(expr) = parse_indexed_register_postfix_operand(&tokens[start..end]) {
        operands.push(expr);
        return Ok(());
    }
    match crate::vm_opcore::parse_expr_with_vm_contract_and_boundary(
        expr_parse_ctx,
        &tokens[start..end],
        expr_end_span,
        end_token_text,
        boundary_token,
    ) {
        Ok(expr) => operands.push(expr),
        Err(err) => operands.push(Expr::Error(err.message, err.span)),
    }
    Ok(())
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
