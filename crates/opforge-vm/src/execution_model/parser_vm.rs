use crate::runtime_diagnostics::RuntimeBridgeDiagnostic;
use crate::runtime_error::RuntimeBridgeError;
use crate::runtime_model_types::{
    RuntimeParserContract, RuntimeParserDiagnosticMap, RuntimeParserVmProgram,
};
use crate::runtime_parse_utils::{parse_error_at_end, runtime_bridge_error_to_parse_error};
use opcore::parser::{LineAst, ParseError};
use opcore::tokenizer::{Span, Token};
use package::{
    ParserVmOpcode, PARSER_VM_OPCODE_VERSION_V1, PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
};
use types::processing::ProcessingRequestKind;

use crate::vm_opasm_parse::{
    parse_assignment_envelope_from_tokens, parse_dot_directive_envelope_from_tokens,
    parse_star_org_envelope_from_tokens, ParserVmExecContext,
};

pub(crate) fn parse_line_with_parser_vm(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    parser_contract: &RuntimeParserContract,
    parser_vm_program: &RuntimeParserVmProgram,
    exec_ctx: ParserVmExecContext<'_>,
) -> Result<LineAst, ParseError> {
    if parser_contract.opcode_version != parser_vm_program.opcode_version {
        return Err(parse_error_at_end(
            exec_ctx.source_line,
            exec_ctx.line_num,
            format!(
                "{}: parser contract/program opcode version mismatch ({} != {})",
                parser_contract.diagnostics.invalid_statement,
                parser_contract.opcode_version,
                parser_vm_program.opcode_version
            ),
        ));
    }
    if parser_contract.opcode_version == PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT {
        return super::parser_vm_v2::parse_line_with_parser_vm_v2(
            tokens,
            end_span,
            end_token_text,
            parser_contract,
            parser_vm_program,
            &ProcessingRequestKind::Processor {
                processor: "asm".to_string(),
                kind: "statement".to_string(),
            },
            exec_ctx,
        );
    }
    if parser_contract.opcode_version != PARSER_VM_OPCODE_VERSION_V1 {
        return Err(parse_error_at_end(
            exec_ctx.source_line,
            exec_ctx.line_num,
            format!(
                "{}: unsupported parser contract opcode version {}",
                parser_contract.diagnostics.invalid_statement, parser_contract.opcode_version
            ),
        ));
    }

    let mut pc = 0usize;
    let mut parsed_line: Option<LineAst> = if tokens.is_empty() {
        Some(LineAst::Empty)
    } else {
        None
    };

    while pc < parser_vm_program.program.len() {
        let opcode_byte = parser_vm_program.program[pc];
        pc = pc.saturating_add(1);
        let Some(opcode) = ParserVmOpcode::from_u8(opcode_byte) else {
            return Err(parse_error_at_end(
                exec_ctx.source_line,
                exec_ctx.line_num,
                format!(
                    "{}: invalid parser VM opcode 0x{opcode_byte:02X}",
                    parser_contract.diagnostics.invalid_statement
                ),
            ));
        };
        match opcode {
            ParserVmOpcode::End => {
                return parsed_line.ok_or_else(|| {
                    parse_error_at_end(
                        exec_ctx.source_line,
                        exec_ctx.line_num,
                        format!(
                            "{}: parser VM ended without producing an AST",
                            parser_contract.diagnostics.invalid_statement
                        ),
                    )
                });
            }
            ParserVmOpcode::ParseDotDirectiveEnvelope => {
                if parsed_line.is_some() {
                    continue;
                }
                if let Some(line) = parse_dot_directive_envelope_from_tokens(
                    &tokens,
                    end_span,
                    end_token_text.clone(),
                    &exec_ctx.expr_parse_ctx,
                )? {
                    parsed_line = Some(line);
                }
            }
            ParserVmOpcode::ParseStarOrgEnvelope => {
                if parsed_line.is_some() {
                    continue;
                }
                if let Some(line) = parse_star_org_envelope_from_tokens(
                    &tokens,
                    end_span,
                    end_token_text.clone(),
                    &exec_ctx.expr_parse_ctx,
                )? {
                    parsed_line = Some(line);
                }
            }
            ParserVmOpcode::ParseAssignmentEnvelope => {
                if parsed_line.is_some() {
                    continue;
                }
                if let Some(line) = parse_assignment_envelope_from_tokens(
                    &tokens,
                    end_span,
                    end_token_text.clone(),
                    &exec_ctx.expr_parse_ctx,
                )? {
                    parsed_line = Some(line);
                }
            }
            ParserVmOpcode::EmitDiag => {
                let slot = parser_vm_read_diag_slot(
                    parser_vm_program,
                    &mut pc,
                    end_span,
                    parser_contract,
                    "EmitDiag",
                )?;
                let code = parser_diag_code_for_slot(&parser_contract.diagnostics, slot);
                return Err(runtime_bridge_error_to_parse_error(
                    RuntimeBridgeError::Diagnostic(RuntimeBridgeDiagnostic::new(
                        code,
                        format!("parser VM emitted diagnostic slot {slot}"),
                        Some(end_span),
                    )),
                    end_span,
                ));
            }
            ParserVmOpcode::EmitDiagIfNoAst => {
                let slot = parser_vm_read_diag_slot(
                    parser_vm_program,
                    &mut pc,
                    end_span,
                    parser_contract,
                    "EmitDiagIfNoAst",
                )?;
                if parsed_line.is_some() {
                    continue;
                }
                let code = parser_diag_code_for_slot(&parser_contract.diagnostics, slot);
                return Err(runtime_bridge_error_to_parse_error(
                    RuntimeBridgeError::Diagnostic(RuntimeBridgeDiagnostic::new(
                        code,
                        format!("parser VM emitted diagnostic slot {slot}"),
                        Some(end_span),
                    )),
                    end_span,
                ));
            }
            ParserVmOpcode::Fail => {
                return Err(parse_error_at_end(
                    exec_ctx.source_line,
                    exec_ctx.line_num,
                    format!(
                        "{}: parser VM requested failure",
                        parser_contract.diagnostics.invalid_statement
                    ),
                ));
            }
        }
    }

    Err(parse_error_at_end(
        exec_ctx.source_line,
        exec_ctx.line_num,
        format!(
            "{}: parser VM program terminated without End opcode",
            parser_contract.diagnostics.invalid_statement
        ),
    ))
}

fn parser_diag_code_for_slot(diagnostics: &RuntimeParserDiagnosticMap, slot: u8) -> &str {
    match slot {
        0 => diagnostics.unexpected_token.as_str(),
        1 => diagnostics.expected_expression.as_str(),
        2 => diagnostics.expected_operand.as_str(),
        _ => diagnostics.invalid_statement.as_str(),
    }
}

fn parser_vm_read_diag_slot(
    parser_vm_program: &RuntimeParserVmProgram,
    pc: &mut usize,
    end_span: Span,
    parser_contract: &RuntimeParserContract,
    opcode_name: &str,
) -> Result<u8, ParseError> {
    let Some(slot) = parser_vm_program.program.get(*pc).copied() else {
        return Err(runtime_bridge_error_to_parse_error(
            RuntimeBridgeError::Diagnostic(RuntimeBridgeDiagnostic::new(
                parser_contract.diagnostics.invalid_statement.as_str(),
                format!("parser VM {} missing slot operand", opcode_name),
                Some(end_span),
            )),
            end_span,
        ));
    };
    *pc = pc.saturating_add(1);
    Ok(slot)
}
