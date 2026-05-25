use crate::runtime_model_types::{RuntimeParserContract, RuntimeParserVmProgram};
use crate::runtime_parse_utils::parse_error_at_end;
use crate::vm_opasm_parse::ParserVmExecContext;
use opcore::parser::{LineAst, ParseError};
use opcore::tokenizer::{Span, Token};
use package::PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT;
use types::processing::ProcessingRequestKind;

pub(crate) fn parse_line_with_parser_vm(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    parser_contract: &RuntimeParserContract,
    parser_vm_program: &RuntimeParserVmProgram,
    use_default_statement_fast_path: bool,
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
    if parser_contract.opcode_version != PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT {
        return Err(parse_error_at_end(
            exec_ctx.source_line,
            exec_ctx.line_num,
            format!(
                "{}: unsupported parser contract opcode version {}",
                parser_contract.diagnostics.invalid_statement, parser_contract.opcode_version
            ),
        ));
    }

    if use_default_statement_fast_path {
        return super::parser_vm_v2::parse_line_with_default_statement_parser_vm_v2(
            tokens,
            end_span,
            end_token_text,
            parser_contract,
            &ProcessingRequestKind::Processor {
                processor: "asm".to_string(),
                kind: "statement".to_string(),
            },
            exec_ctx,
        );
    }

    super::parser_vm_v2::parse_line_with_parser_vm_v2(
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
    )
}
