use crate::runtime_diagnostics::RuntimeBridgeDiagnostic;
use crate::runtime_error::RuntimeBridgeError;
use crate::runtime_model_types::{RuntimeParserContract, RuntimeParserVmProgram};
use crate::runtime_parse_utils::{parse_error_at_end, runtime_bridge_error_to_parse_error};
use crate::vm_opasm::{split_top_level_comma_ranges, OperandExprBoundary, OperandExprParseHints};
use crate::vm_opasm_parse::{
    parse_assignment_envelope_from_tokens, parse_dot_directive_envelope_from_tokens,
    parse_star_org_envelope_from_tokens, ParserVmExecContext,
};
use opcore::parser::{Expr, Label, LineAst, ParseError};
use opcore::tokenizer::{OperatorKind, Span, Token, TokenKind};
use package::{
    ParserVmOpcode, ParserVmOpcodeV2, DIAG_PARSER_OPASM_V2_CHECKPOINT_DEPTH_EXCEEDED,
    DIAG_PARSER_OPASM_V2_ENTRY_BOUNDARY_VIOLATION,
    DIAG_PARSER_OPASM_V2_FORBIDDEN_CROSS_CONTRACT_OPCODE,
    DIAG_PARSER_OPASM_V2_MISROUTED_OPCORE_DIRECTIVE, PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
};
use types::line_ast::StatementAst;
use types::processing::ProcessingRequestKind;

const MAX_STEPS_PER_LINE: usize = 2048;
const MAX_VALUE_STACK_DEPTH: usize = 64;
const MAX_CHECKPOINT_DEPTH: usize = 4;

const TOKEN_KIND_IDENTIFIER: u8 = 0x01;
const TOKEN_KIND_REGISTER: u8 = 0x02;
const TOKEN_KIND_DOT: u8 = 0x03;
const TOKEN_KIND_COLON: u8 = 0x04;
const TOKEN_KIND_OPERATOR: u8 = 0x05;
const TOKEN_KIND_QUESTION: u8 = 0x06;
const TOKEN_KIND_COMMA: u8 = 0x07;

const OPERATOR_PLUS: u8 = 0x01;
const OPERATOR_EQ: u8 = 0x02;
const OPERATOR_MULTIPLY: u8 = 0x03;
const DYNAMIC_OPERAND_RANGE: usize = u16::MAX as usize;
const RETIRED_V1_PARSE_INSTRUCTION_ENVELOPE: u8 = 0x07;

#[derive(Clone, Debug)]
enum ParserVmV2Value {
    Bool(bool),
    Text(String),
    Span(Span),
    Label(Label),
    Expr(Expr),
    Boundaries(Vec<(usize, usize)>),
}

#[derive(Clone, Debug, Default)]
struct ParserVmV2AstBuilder {
    label: Option<Label>,
    mnemonic: Option<String>,
    operands: Vec<Expr>,
}

#[derive(Clone, Debug)]
struct ParserVmV2Checkpoint {
    cursor: usize,
    builder: ParserVmV2AstBuilder,
    value_stack_len: usize,
}

pub(crate) fn parse_line_with_parser_vm_v2<'exec>(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    parser_contract: &RuntimeParserContract,
    parser_vm_program: &RuntimeParserVmProgram,
    entry_request: &ProcessingRequestKind,
    exec_ctx: ParserVmExecContext<'exec>,
) -> Result<LineAst, ParseError> {
    enforce_entry_boundary(entry_request, end_span)?;
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
                "{}: unsupported opasm v2 parser contract opcode version {}",
                parser_contract.diagnostics.invalid_statement, parser_contract.opcode_version
            ),
        ));
    }
    reject_misrouted_opcore_directive(tokens.as_slice(), end_span)?;
    if let Some(line) = parse_legacy_deferred_shape(
        tokens.as_slice(),
        end_span,
        end_token_text.clone(),
        &exec_ctx,
    )? {
        return Ok(line);
    }

    let mut state = ParserVmV2State {
        tokens,
        end_span,
        end_token_text,
        parser_contract,
        program: parser_vm_program.program.as_slice(),
        exec_ctx,
        pc: 0,
        cursor: 0,
        steps: 0,
        value_stack: Vec::new(),
        checkpoints: Vec::new(),
        operand_boundaries: Vec::new(),
        builder: ParserVmV2AstBuilder::default(),
        parsed_line: None,
        advance_mnemonic_suffix_plus: false,
    };
    state.run()
}

struct ParserVmV2State<'contract, 'exec> {
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    parser_contract: &'contract RuntimeParserContract,
    program: &'contract [u8],
    exec_ctx: ParserVmExecContext<'exec>,
    pc: usize,
    cursor: usize,
    steps: usize,
    value_stack: Vec<ParserVmV2Value>,
    checkpoints: Vec<ParserVmV2Checkpoint>,
    operand_boundaries: Vec<(usize, usize)>,
    builder: ParserVmV2AstBuilder,
    parsed_line: Option<LineAst>,
    advance_mnemonic_suffix_plus: bool,
}

impl ParserVmV2State<'_, '_> {
    fn run(&mut self) -> Result<LineAst, ParseError> {
        if self.tokens.is_empty() {
            self.parsed_line = Some(LineAst::Empty);
        }
        while self.pc < self.program.len() {
            self.steps = self.steps.saturating_add(1);
            if self.steps > MAX_STEPS_PER_LINE {
                return self.fail_with_code(
                    self.parser_contract.diagnostics.invalid_statement.as_str(),
                    "parser VM v2 step budget exceeded",
                );
            }
            let opcode_byte = self.read_u8("opcode")?;
            let Some(opcode) = ParserVmOpcodeV2::from_u8(opcode_byte) else {
                if ParserVmOpcode::from_u8(opcode_byte).is_some()
                    || opcode_byte == RETIRED_V1_PARSE_INSTRUCTION_ENVELOPE
                {
                    return self.fail_with_code(
                        DIAG_PARSER_OPASM_V2_FORBIDDEN_CROSS_CONTRACT_OPCODE,
                        format!("parser VM v2 rejected cross-contract opcode 0x{opcode_byte:02X}"),
                    );
                }
                return self.fail_with_code(
                    self.parser_contract.diagnostics.invalid_statement.as_str(),
                    format!("invalid parser VM v2 opcode 0x{opcode_byte:02X}"),
                );
            };
            match opcode {
                ParserVmOpcodeV2::End => {
                    return self.parsed_line.clone().ok_or_else(|| {
                        parse_error_at_end(
                            self.exec_ctx.source_line,
                            self.exec_ctx.line_num,
                            format!(
                                "{}: parser VM v2 ended without producing an AST",
                                self.parser_contract.diagnostics.invalid_statement
                            ),
                        )
                    });
                }
                ParserVmOpcodeV2::Jump => {
                    self.pc = self.read_u16("Jump target")? as usize;
                }
                ParserVmOpcodeV2::JumpIfTrue => {
                    let target = self.read_u16("JumpIfTrue target")? as usize;
                    if self.pop_bool("JumpIfTrue")? {
                        self.pc = target;
                    }
                }
                ParserVmOpcodeV2::JumpIfFalse => {
                    let target = self.read_u16("JumpIfFalse target")? as usize;
                    if !self.pop_bool("JumpIfFalse")? {
                        self.pc = target;
                    }
                }
                ParserVmOpcodeV2::Checkpoint => self.checkpoint()?,
                ParserVmOpcodeV2::Rollback => self.rollback()?,
                ParserVmOpcodeV2::Commit => self.commit()?,
                ParserVmOpcodeV2::PeekKind => {
                    let kind = self.read_u8("PeekKind kind")?;
                    self.push_value(ParserVmV2Value::Bool(self.peek_kind(kind)))?;
                }
                ParserVmOpcodeV2::PeekIdentifier => {
                    let expected = self.read_inline_string("PeekIdentifier text")?;
                    self.push_value(ParserVmV2Value::Bool(self.peek_identifier(&expected)))?;
                }
                ParserVmOpcodeV2::PeekOperator => {
                    let expected = self.read_u8("PeekOperator operator")?;
                    self.push_value(ParserVmV2Value::Bool(self.peek_operator(expected)))?;
                }
                ParserVmOpcodeV2::IsEol => {
                    self.push_value(ParserVmV2Value::Bool(self.cursor >= self.tokens.len()))?;
                }
                ParserVmOpcodeV2::Advance => {
                    if self.cursor < self.tokens.len() {
                        let step = if self.advance_mnemonic_suffix_plus {
                            self.advance_mnemonic_suffix_plus = false;
                            2
                        } else {
                            1
                        };
                        self.cursor = self.cursor.saturating_add(step).min(self.tokens.len());
                    }
                }
                ParserVmOpcodeV2::ConsumeKind => {
                    let kind = self.read_u8("ConsumeKind kind")?;
                    if !self.peek_kind(kind) {
                        return self.fail_with_code(
                            self.parser_contract.diagnostics.unexpected_token.as_str(),
                            "parser VM v2 ConsumeKind mismatch",
                        );
                    }
                    self.cursor = self.cursor.saturating_add(1);
                }
                ParserVmOpcodeV2::ConsumeOperator => {
                    let operator = self.read_u8("ConsumeOperator operator")?;
                    if !self.peek_operator(operator) {
                        return self.fail_with_code(
                            self.parser_contract.diagnostics.unexpected_token.as_str(),
                            "parser VM v2 ConsumeOperator mismatch",
                        );
                    }
                    self.cursor = self.cursor.saturating_add(1);
                }
                ParserVmOpcodeV2::LoadIdentifier => {
                    let Some((name, _)) = self.identifier_at_cursor() else {
                        return self.fail_with_code(
                            self.parser_contract.diagnostics.unexpected_token.as_str(),
                            "parser VM v2 expected identifier",
                        );
                    };
                    self.push_value(ParserVmV2Value::Text(name))?;
                }
                ParserVmOpcodeV2::LoadSpan => {
                    let span = self
                        .tokens
                        .get(self.cursor)
                        .map(|token| token.span)
                        .unwrap_or(self.end_span);
                    self.push_value(ParserVmV2Value::Span(span))?;
                }
                ParserVmOpcodeV2::LoadTokenText => {
                    let text = self
                        .tokens
                        .get(self.cursor)
                        .map(token_text)
                        .unwrap_or_default();
                    self.push_value(ParserVmV2Value::Text(text))?;
                }
                ParserVmOpcodeV2::ParseOptionalLeadingLabel => {
                    self.parse_optional_leading_label()?
                }
                ParserVmOpcodeV2::ScanTopLevelCommaBoundaries => {
                    let ranges = split_top_level_comma_ranges(
                        self.tokens.as_slice(),
                        self.cursor,
                        self.tokens.len(),
                    );
                    self.operand_boundaries = ranges.clone();
                    self.push_value(ParserVmV2Value::Boundaries(ranges))?;
                }
                ParserVmOpcodeV2::RequireNoTrailingTokens => {
                    if self.cursor < self.tokens.len() {
                        return self.fail_with_code(
                            self.parser_contract.diagnostics.unexpected_token.as_str(),
                            "parser VM v2 found trailing tokens",
                        );
                    }
                }
                ParserVmOpcodeV2::ParseOperandExprRange => {
                    let start = self.read_u16("ParseOperandExprRange start")? as usize;
                    let end = self.read_u16("ParseOperandExprRange end")? as usize;
                    if start == DYNAMIC_OPERAND_RANGE && end == DYNAMIC_OPERAND_RANGE {
                        self.parse_operand_boundaries_into_builder()?;
                        continue;
                    }
                    self.exec_ctx
                        .expr_parse_ctx
                        .model
                        .ensure_parser_vm_v2_expr_subcall_contract_for_assembler(
                            self.exec_ctx.expr_parse_ctx.cpu_id,
                            self.exec_ctx.expr_parse_ctx.dialect_override,
                        )
                        .map_err(|err| runtime_bridge_error_to_parse_error(err, self.end_span))?;
                    let mut operands = Vec::new();
                    crate::vm_opasm::parse_operand_expr_range(
                        self.tokens.as_slice(),
                        start,
                        end,
                        OperandExprBoundary {
                            end_span: self.end_span,
                            end_token_text: self.end_token_text.clone(),
                        },
                        OperandExprParseHints {
                            mnemonic: self.builder.mnemonic.as_deref(),
                            operand_index: self.builder.operands.len(),
                        },
                        &self.exec_ctx.expr_parse_ctx,
                        &mut operands,
                    )?;
                    let expr = operands.pop().ok_or_else(|| ParseError {
                        message: "parser VM v2 expression sub-call produced no operand".to_string(),
                        span: self.end_span,
                    })?;
                    self.push_value(ParserVmV2Value::Expr(expr))?;
                }
                ParserVmOpcodeV2::BeginStatement => {
                    self.builder = ParserVmV2AstBuilder::default();
                }
                ParserVmOpcodeV2::SetLabel => {
                    let label = self.pop_label("SetLabel")?;
                    self.builder.label = Some(label);
                }
                ParserVmOpcodeV2::SetMnemonic => {
                    let mut mnemonic = self.pop_text("SetMnemonic")?;
                    if self.mnemonic_has_attached_size_plus(&mnemonic) {
                        mnemonic.push('+');
                        self.advance_mnemonic_suffix_plus = true;
                    }
                    self.builder.mnemonic = Some(mnemonic);
                }
                ParserVmOpcodeV2::SetDotMnemonic => {
                    let mnemonic = self.pop_text("SetDotMnemonic")?;
                    self.builder.mnemonic = Some(format!(".{mnemonic}"));
                }
                ParserVmOpcodeV2::PushOperand => {
                    let expr = self.pop_expr("PushOperand")?;
                    self.builder.operands.push(expr);
                }
                ParserVmOpcodeV2::FinishLine => {
                    self.parsed_line = Some(LineAst::Statement(StatementAst {
                        label: self.builder.label.clone(),
                        mnemonic: self.builder.mnemonic.clone(),
                        operands: self.builder.operands.clone(),
                    }));
                }
                ParserVmOpcodeV2::EmitDiag => {
                    let slot = self.read_u8("EmitDiag slot")?;
                    return self.fail_with_code(
                        self.diag_code_for_slot(slot),
                        format!("parser VM v2 emitted diagnostic slot {slot}"),
                    );
                }
                ParserVmOpcodeV2::EmitDiagIfNoResult => {
                    let slot = self.read_u8("EmitDiagIfNoResult slot")?;
                    if self.parsed_line.is_none() {
                        return self.fail_with_code(
                            self.diag_code_for_slot(slot),
                            format!("parser VM v2 emitted diagnostic slot {slot}"),
                        );
                    }
                }
                ParserVmOpcodeV2::Fail => {
                    return self.fail_with_code(
                        self.parser_contract.diagnostics.invalid_statement.as_str(),
                        "parser VM v2 requested failure",
                    );
                }
            }
        }
        self.fail_with_code(
            self.parser_contract.diagnostics.invalid_statement.as_str(),
            "parser VM v2 program terminated without End opcode",
        )
    }

    fn read_u8(&mut self, name: &str) -> Result<u8, ParseError> {
        let Some(value) = self.program.get(self.pc).copied() else {
            return self.fail_with_code(
                self.parser_contract.diagnostics.invalid_statement.as_str(),
                format!("parser VM v2 missing {name}"),
            );
        };
        self.pc = self.pc.saturating_add(1);
        Ok(value)
    }

    fn read_u16(&mut self, name: &str) -> Result<u16, ParseError> {
        let lo = self.read_u8(name)?;
        let hi = self.read_u8(name)?;
        Ok(u16::from_le_bytes([lo, hi]))
    }

    fn read_inline_string(&mut self, name: &str) -> Result<String, ParseError> {
        let len = self.read_u8(name)? as usize;
        let end = self.pc.saturating_add(len);
        let Some(bytes) = self.program.get(self.pc..end) else {
            return self.fail_with_code(
                self.parser_contract.diagnostics.invalid_statement.as_str(),
                format!("parser VM v2 missing {name} bytes"),
            );
        };
        self.pc = end;
        String::from_utf8(bytes.to_vec()).map_err(|_| ParseError {
            message: format!("parser VM v2 invalid UTF-8 in {name}"),
            span: self.end_span,
        })
    }

    fn checkpoint(&mut self) -> Result<(), ParseError> {
        if self.checkpoints.len() >= MAX_CHECKPOINT_DEPTH {
            return self.fail_with_code(
                DIAG_PARSER_OPASM_V2_CHECKPOINT_DEPTH_EXCEEDED,
                "parser VM v2 checkpoint depth exceeded",
            );
        }
        self.checkpoints.push(ParserVmV2Checkpoint {
            cursor: self.cursor,
            builder: self.builder.clone(),
            value_stack_len: self.value_stack.len(),
        });
        Ok(())
    }

    fn rollback(&mut self) -> Result<(), ParseError> {
        let Some(checkpoint) = self.checkpoints.pop() else {
            return self.fail_with_code(
                self.parser_contract.diagnostics.invalid_statement.as_str(),
                "parser VM v2 rollback without checkpoint",
            );
        };
        self.cursor = checkpoint.cursor;
        self.builder = checkpoint.builder;
        self.value_stack.truncate(checkpoint.value_stack_len);
        Ok(())
    }

    fn commit(&mut self) -> Result<(), ParseError> {
        if self.checkpoints.pop().is_none() {
            return self.fail_with_code(
                self.parser_contract.diagnostics.invalid_statement.as_str(),
                "parser VM v2 commit without checkpoint",
            );
        }
        Ok(())
    }

    fn push_value(&mut self, value: ParserVmV2Value) -> Result<(), ParseError> {
        touch_value(&value);
        if self.value_stack.len() >= MAX_VALUE_STACK_DEPTH {
            return self.fail_with_code(
                self.parser_contract.diagnostics.invalid_statement.as_str(),
                "parser VM v2 value stack depth exceeded",
            );
        }
        self.value_stack.push(value);
        Ok(())
    }

    fn pop_value(&mut self, opcode: &str) -> Result<ParserVmV2Value, ParseError> {
        self.value_stack.pop().ok_or_else(|| ParseError {
            message: format!("parser VM v2 {opcode} stack underflow"),
            span: self.end_span,
        })
    }

    fn pop_bool(&mut self, opcode: &str) -> Result<bool, ParseError> {
        match self.pop_value(opcode)? {
            ParserVmV2Value::Bool(value) => Ok(value),
            _ => self.fail_with_code(
                self.parser_contract.diagnostics.invalid_statement.as_str(),
                format!("parser VM v2 {opcode} expected bool"),
            ),
        }
    }

    fn pop_text(&mut self, opcode: &str) -> Result<String, ParseError> {
        match self.pop_value(opcode)? {
            ParserVmV2Value::Text(value) => Ok(value),
            _ => self.fail_with_code(
                self.parser_contract.diagnostics.invalid_statement.as_str(),
                format!("parser VM v2 {opcode} expected text"),
            ),
        }
    }

    fn pop_label(&mut self, opcode: &str) -> Result<Label, ParseError> {
        match self.pop_value(opcode)? {
            ParserVmV2Value::Label(value) => Ok(value),
            _ => self.fail_with_code(
                self.parser_contract.diagnostics.invalid_statement.as_str(),
                format!("parser VM v2 {opcode} expected label"),
            ),
        }
    }

    fn pop_expr(&mut self, opcode: &str) -> Result<Expr, ParseError> {
        match self.pop_value(opcode)? {
            ParserVmV2Value::Expr(value) => Ok(value),
            _ => self.fail_with_code(
                self.parser_contract.diagnostics.invalid_statement.as_str(),
                format!("parser VM v2 {opcode} expected expr"),
            ),
        }
    }

    fn parse_optional_leading_label(&mut self) -> Result<(), ParseError> {
        if self.cursor != 0 {
            return Ok(());
        }
        let Some(first) = self.tokens.first() else {
            return Ok(());
        };
        let label_name = match &first.kind {
            TokenKind::Identifier(name) | TokenKind::Register(name) => Some(name.clone()),
            _ => None,
        };
        let Some(name) = label_name else {
            return Ok(());
        };
        if first.span.col_start != 1 {
            return Ok(());
        }
        let label = Label {
            name,
            span: first.span,
        };
        self.builder.label = Some(label.clone());
        if let Some(colon) = self.tokens.get(1) {
            if matches!(colon.kind, TokenKind::Colon) && colon.span.col_start == first.span.col_end
            {
                self.cursor = 2;
            } else {
                self.cursor = 1;
            }
        } else {
            self.cursor = 1;
        }
        self.push_value(ParserVmV2Value::Label(label))
    }

    fn parse_operand_boundaries_into_builder(&mut self) -> Result<(), ParseError> {
        if self.operand_boundaries.is_empty() {
            return Ok(());
        }
        self.exec_ctx
            .expr_parse_ctx
            .model
            .ensure_parser_vm_v2_expr_subcall_contract_for_assembler(
                self.exec_ctx.expr_parse_ctx.cpu_id,
                self.exec_ctx.expr_parse_ctx.dialect_override,
            )
            .map_err(|err| runtime_bridge_error_to_parse_error(err, self.end_span))?;
        let mnemonic = self.builder.mnemonic.clone();
        for (range_idx, (start, end)) in self.operand_boundaries.clone().into_iter().enumerate() {
            if range_idx == 0 && start == end {
                let span = self
                    .tokens
                    .get(start)
                    .map(|token| token.span)
                    .unwrap_or(self.end_span);
                self.builder
                    .operands
                    .push(Expr::Number("0".to_string(), span));
                continue;
            }
            crate::vm_opasm::parse_operand_expr_range(
                self.tokens.as_slice(),
                start,
                end,
                OperandExprBoundary {
                    end_span: self.end_span,
                    end_token_text: self.end_token_text.clone(),
                },
                OperandExprParseHints {
                    mnemonic: mnemonic.as_deref(),
                    operand_index: range_idx,
                },
                &self.exec_ctx.expr_parse_ctx,
                &mut self.builder.operands,
            )?;
            if matches!(self.builder.operands.last(), Some(Expr::Error(_, _))) {
                break;
            }
        }
        Ok(())
    }

    fn mnemonic_has_attached_size_plus(&self, mnemonic: &str) -> bool {
        if !mnemonic.to_ascii_uppercase().ends_with(".S") {
            return false;
        }
        matches!(
            (self.tokens.get(self.cursor), self.tokens.get(self.cursor.saturating_add(1))),
            (
                Some(Token { span, .. }),
                Some(Token {
                    kind: TokenKind::Operator(OperatorKind::Plus),
                    span: plus_span,
                })
            ) if plus_span.col_start == span.col_end
        )
    }

    fn identifier_at_cursor(&self) -> Option<(String, Span)> {
        match self.tokens.get(self.cursor) {
            Some(Token {
                kind: TokenKind::Identifier(name),
                span,
            }) => Some((name.clone(), *span)),
            _ => None,
        }
    }

    fn peek_identifier(&self, expected: &str) -> bool {
        self.identifier_at_cursor()
            .map(|(name, _)| name.eq_ignore_ascii_case(expected))
            .unwrap_or(false)
    }

    fn peek_kind(&self, expected: u8) -> bool {
        let Some(token) = self.tokens.get(self.cursor) else {
            return false;
        };
        match expected {
            TOKEN_KIND_IDENTIFIER => matches!(token.kind, TokenKind::Identifier(_)),
            TOKEN_KIND_REGISTER => matches!(token.kind, TokenKind::Register(_)),
            TOKEN_KIND_DOT => matches!(token.kind, TokenKind::Dot),
            TOKEN_KIND_COLON => matches!(token.kind, TokenKind::Colon),
            TOKEN_KIND_OPERATOR => matches!(token.kind, TokenKind::Operator(_)),
            TOKEN_KIND_QUESTION => matches!(token.kind, TokenKind::Question),
            TOKEN_KIND_COMMA => matches!(token.kind, TokenKind::Comma),
            _ => false,
        }
    }

    fn peek_operator(&self, expected: u8) -> bool {
        let Some(Token {
            kind: TokenKind::Operator(operator),
            ..
        }) = self.tokens.get(self.cursor)
        else {
            return false;
        };
        matches!(
            (expected, operator),
            (OPERATOR_PLUS, OperatorKind::Plus)
                | (OPERATOR_EQ, OperatorKind::Eq)
                | (OPERATOR_MULTIPLY, OperatorKind::Multiply)
        )
    }

    fn diag_code_for_slot(&self, slot: u8) -> &str {
        match slot {
            0 => self.parser_contract.diagnostics.unexpected_token.as_str(),
            1 => self
                .parser_contract
                .diagnostics
                .expected_expression
                .as_str(),
            2 => self.parser_contract.diagnostics.expected_operand.as_str(),
            _ => self.parser_contract.diagnostics.invalid_statement.as_str(),
        }
    }

    fn fail_with_code<T>(&self, code: &str, message: impl Into<String>) -> Result<T, ParseError> {
        Err(runtime_bridge_error_to_parse_error(
            RuntimeBridgeError::Diagnostic(RuntimeBridgeDiagnostic::new(
                code,
                message.into(),
                Some(self.end_span),
            )),
            self.end_span,
        ))
    }
}

fn enforce_entry_boundary(
    entry_request: &ProcessingRequestKind,
    end_span: Span,
) -> Result<(), ParseError> {
    match entry_request {
        ProcessingRequestKind::Processor { processor, kind }
            if processor == "asm" && kind == "statement" =>
        {
            Ok(())
        }
        _ => Err(runtime_bridge_error_to_parse_error(
            RuntimeBridgeError::Diagnostic(RuntimeBridgeDiagnostic::new(
                DIAG_PARSER_OPASM_V2_ENTRY_BOUNDARY_VIOLATION,
                "opasm v2 parser VM requires Processor { processor: \"asm\", kind: \"statement\" }",
                Some(end_span),
            )),
            end_span,
        )),
    }
}

fn reject_misrouted_opcore_directive(tokens: &[Token], end_span: Span) -> Result<(), ParseError> {
    let directive_idx = match tokens.first() {
        Some(Token {
            kind: TokenKind::Identifier(_) | TokenKind::Register(_),
            span,
        }) if span.col_start == 1 => {
            if matches!(
                tokens.get(1),
                Some(Token {
                    kind: TokenKind::Colon,
                    ..
                })
            ) {
                2
            } else {
                1
            }
        }
        _ => 0,
    };
    let Some(Token {
        kind: TokenKind::Dot,
        ..
    }) = tokens.get(directive_idx)
    else {
        return Ok(());
    };
    let Some(Token {
        kind: TokenKind::Identifier(name),
        ..
    }) = tokens.get(directive_idx.saturating_add(1))
    else {
        return Ok(());
    };
    if !matches!(name.to_ascii_lowercase().as_str(), "include") {
        return Ok(());
    }
    Err(runtime_bridge_error_to_parse_error(
        RuntimeBridgeError::Diagnostic(RuntimeBridgeDiagnostic::new(
            DIAG_PARSER_OPASM_V2_MISROUTED_OPCORE_DIRECTIVE,
            format!("opcore directive .{name} reached opasm v2 parser VM"),
            Some(end_span),
        )),
        end_span,
    ))
}

fn parse_legacy_deferred_shape(
    tokens: &[Token],
    end_span: Span,
    end_token_text: Option<String>,
    exec_ctx: &ParserVmExecContext<'_>,
) -> Result<Option<LineAst>, ParseError> {
    if !is_v2_data_directive_shape(tokens) {
        if let Some(line) = parse_dot_directive_envelope_from_tokens(
            tokens,
            end_span,
            end_token_text.clone(),
            &exec_ctx.expr_parse_ctx,
        )? {
            return Ok(Some(line));
        }
    }
    if let Some(line) = parse_star_org_envelope_from_tokens(
        tokens,
        end_span,
        end_token_text.clone(),
        &exec_ctx.expr_parse_ctx,
    )? {
        return Ok(Some(line));
    }
    parse_assignment_envelope_from_tokens(
        tokens,
        end_span,
        end_token_text,
        &exec_ctx.expr_parse_ctx,
    )
}

fn is_v2_data_directive_shape(tokens: &[Token]) -> bool {
    let directive_idx = match tokens.first() {
        Some(Token {
            kind: TokenKind::Identifier(_) | TokenKind::Register(_),
            span,
        }) if span.col_start == 1 => {
            if matches!(
                tokens.get(1),
                Some(Token {
                    kind: TokenKind::Colon,
                    ..
                })
            ) {
                2
            } else {
                1
            }
        }
        _ => 0,
    };
    matches!(
        tokens.get(directive_idx),
        Some(Token {
            kind: TokenKind::Dot,
            ..
        })
    ) && matches!(
        tokens.get(directive_idx.saturating_add(1)),
        Some(Token { kind: TokenKind::Identifier(name), .. })
            if is_v2_data_directive_name(name)
    )
}

fn is_v2_data_directive_name(name: &str) -> bool {
    matches!(
        name.to_ascii_lowercase().as_str(),
        "byte"
            | "db"
            | "word"
            | "dw"
            | "long"
            | "text"
            | "null"
            | "ptext"
            | "fill"
            | "res"
            | "ds"
            | "align"
    )
}

fn token_text(token: &Token) -> String {
    match &token.kind {
        TokenKind::Identifier(value) | TokenKind::Register(value) => value.clone(),
        TokenKind::Operator(OperatorKind::Plus) => "+".to_string(),
        TokenKind::Operator(OperatorKind::Eq) => "=".to_string(),
        TokenKind::Operator(OperatorKind::Multiply) => "*".to_string(),
        TokenKind::Comma => ",".to_string(),
        TokenKind::Dot => ".".to_string(),
        TokenKind::Colon => ":".to_string(),
        TokenKind::Question => "?".to_string(),
        other => format!("{other:?}"),
    }
}

fn touch_value(value: &ParserVmV2Value) {
    match value {
        ParserVmV2Value::Span(span) => {
            let _ = span.line;
        }
        ParserVmV2Value::Boundaries(ranges) => {
            let _ = ranges.len();
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm_opasm_parse::{
        DynExprProcessingHandler, ExprProcessingHandler, VmExprParseContext,
    };
    use crate::vm_opcore::HierarchyExecutionModel;
    use families::{register_intel8080_family_stack, register_mos6502_family_stack};
    use registry::registry::ModuleRegistry;
    use std::cell::RefCell;
    use std::rc::Rc;
    use types::processing::{OpcoreRequestKind, ProcessingOutcome};

    struct StubExprHandler;

    impl ExprProcessingHandler for StubExprHandler {
        fn process_expr_request(
            &mut self,
            request: ProcessingRequestKind,
            tokens: Vec<Token>,
            end_span: Span,
            _end_token_text: Option<String>,
        ) -> ProcessingOutcome<Expr, ParseError> {
            assert_eq!(
                request,
                ProcessingRequestKind::Opcore(OpcoreRequestKind::Expr)
            );
            let span = tokens.first().map(|token| token.span).unwrap_or(end_span);
            ProcessingOutcome::Done(Expr::Number("42".to_string(), span))
        }
    }

    fn model_for_tests() -> HierarchyExecutionModel {
        let mut registry = ModuleRegistry::new();
        register_mos6502_family_stack(&mut registry);
        register_intel8080_family_stack(&mut registry);
        HierarchyExecutionModel::from_registry(&registry).expect("runtime model should build")
    }

    fn parser_contract_for_tests() -> RuntimeParserContract {
        RuntimeParserContract {
            grammar_id: package::PARSER_GRAMMAR_ID_LINE_V1.to_string(),
            ast_schema_id: package::PARSER_AST_SCHEMA_ID_LINE_V1.to_string(),
            opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
            max_ast_nodes_per_line: 256,
            diagnostics: crate::runtime_model_types::RuntimeParserDiagnosticMap {
                unexpected_token: package::DIAG_PARSER_UNEXPECTED_TOKEN.to_string(),
                expected_expression: package::DIAG_PARSER_EXPECTED_EXPRESSION.to_string(),
                expected_operand: package::DIAG_PARSER_EXPECTED_OPERAND.to_string(),
                invalid_statement: package::DIAG_PARSER_INVALID_STATEMENT.to_string(),
            },
        }
    }

    fn request() -> ProcessingRequestKind {
        ProcessingRequestKind::Processor {
            processor: "asm".to_string(),
            kind: "statement".to_string(),
        }
    }

    fn span(col_start: usize, col_end: usize) -> Span {
        Span {
            line: 1,
            col_start,
            col_end,
        }
    }

    fn ident(name: &str, col_start: usize, col_end: usize) -> Token {
        Token {
            kind: TokenKind::Identifier(name.to_string()),
            span: span(col_start, col_end),
        }
    }

    fn colon(col: usize) -> Token {
        Token {
            kind: TokenKind::Colon,
            span: span(col, col + 1),
        }
    }

    fn comma(col: usize) -> Token {
        Token {
            kind: TokenKind::Comma,
            span: span(col, col + 1),
        }
    }

    fn dot(col: usize) -> Token {
        Token {
            kind: TokenKind::Dot,
            span: span(col, col + 1),
        }
    }

    fn default_statement_program_for_tests() -> RuntimeParserVmProgram {
        RuntimeParserVmProgram {
            opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
            program: vec![
                ParserVmOpcodeV2::BeginStatement as u8,
                ParserVmOpcodeV2::ParseOptionalLeadingLabel as u8,
                ParserVmOpcodeV2::IsEol as u8,
                ParserVmOpcodeV2::JumpIfFalse as u8,
                8,
                0,
                ParserVmOpcodeV2::FinishLine as u8,
                ParserVmOpcodeV2::End as u8,
                ParserVmOpcodeV2::PeekKind as u8,
                TOKEN_KIND_DOT,
                ParserVmOpcodeV2::JumpIfFalse as u8,
                20,
                0,
                ParserVmOpcodeV2::Advance as u8,
                ParserVmOpcodeV2::LoadIdentifier as u8,
                ParserVmOpcodeV2::SetDotMnemonic as u8,
                ParserVmOpcodeV2::Advance as u8,
                ParserVmOpcodeV2::Jump as u8,
                23,
                0,
                ParserVmOpcodeV2::LoadIdentifier as u8,
                ParserVmOpcodeV2::SetMnemonic as u8,
                ParserVmOpcodeV2::Advance as u8,
                ParserVmOpcodeV2::ScanTopLevelCommaBoundaries as u8,
                ParserVmOpcodeV2::ParseOperandExprRange as u8,
                0xFF,
                0xFF,
                0xFF,
                0xFF,
                ParserVmOpcodeV2::FinishLine as u8,
                ParserVmOpcodeV2::End as u8,
            ],
        }
    }

    fn exec_context<'a>(
        model: &'a HierarchyExecutionModel,
        handler: Option<DynExprProcessingHandler<'a>>,
    ) -> ParserVmExecContext<'a> {
        ParserVmExecContext {
            source_line: "label: lda value",
            line_num: 1,
            expr_parse_ctx: VmExprParseContext {
                model,
                cpu_id: "m6502",
                dialect_override: None,
                expr_handler: handler,
            },
        }
    }

    #[test]
    fn parser_vm_v2_rejects_wrong_entry_boundary() {
        let model = model_for_tests();
        let contract = parser_contract_for_tests();
        let program = RuntimeParserVmProgram {
            opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
            program: vec![ParserVmOpcodeV2::End as u8],
        };
        let err = parse_line_with_parser_vm_v2(
            Vec::new(),
            span(1, 1),
            None,
            &contract,
            &program,
            &ProcessingRequestKind::Opcore(OpcoreRequestKind::Statement),
            exec_context(&model, None),
        )
        .expect_err("wrong boundary should fail");
        assert!(err
            .message
            .contains(DIAG_PARSER_OPASM_V2_ENTRY_BOUNDARY_VIOLATION));
    }

    #[test]
    fn parser_vm_v2_rolls_back_cursor_builder_and_stack() {
        let model = model_for_tests();
        let contract = parser_contract_for_tests();
        let program = RuntimeParserVmProgram {
            opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
            program: vec![
                ParserVmOpcodeV2::BeginStatement as u8,
                ParserVmOpcodeV2::ParseOptionalLeadingLabel as u8,
                ParserVmOpcodeV2::SetLabel as u8,
                ParserVmOpcodeV2::Checkpoint as u8,
                ParserVmOpcodeV2::LoadIdentifier as u8,
                ParserVmOpcodeV2::Advance as u8,
                ParserVmOpcodeV2::Rollback as u8,
                ParserVmOpcodeV2::LoadIdentifier as u8,
                ParserVmOpcodeV2::SetMnemonic as u8,
                ParserVmOpcodeV2::Advance as u8,
                ParserVmOpcodeV2::RequireNoTrailingTokens as u8,
                ParserVmOpcodeV2::FinishLine as u8,
                ParserVmOpcodeV2::End as u8,
            ],
        };
        let line = parse_line_with_parser_vm_v2(
            vec![ident("label", 1, 6), colon(6), ident("lda", 8, 11)],
            span(11, 11),
            None,
            &contract,
            &program,
            &request(),
            exec_context(&model, None),
        )
        .expect("v2 parser should produce statement");
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.label.expect("label").name, "label");
                assert_eq!(statement.mnemonic.as_deref(), Some("lda"));
                assert!(statement.operands.is_empty());
            }
            other => panic!("expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parser_vm_v2_dispatches_operand_expr_subcall() {
        let model = model_for_tests();
        let contract = parser_contract_for_tests();
        let handler: DynExprProcessingHandler<'_> =
            Rc::new(RefCell::new(Box::new(StubExprHandler)));
        let program = RuntimeParserVmProgram {
            opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
            program: vec![
                ParserVmOpcodeV2::BeginStatement as u8,
                ParserVmOpcodeV2::LoadIdentifier as u8,
                ParserVmOpcodeV2::SetMnemonic as u8,
                ParserVmOpcodeV2::Advance as u8,
                ParserVmOpcodeV2::ParseOperandExprRange as u8,
                1,
                0,
                2,
                0,
                ParserVmOpcodeV2::PushOperand as u8,
                ParserVmOpcodeV2::FinishLine as u8,
                ParserVmOpcodeV2::End as u8,
            ],
        };
        let line = parse_line_with_parser_vm_v2(
            vec![ident("lda", 1, 4), ident("value", 5, 10)],
            span(10, 10),
            None,
            &contract,
            &program,
            &request(),
            exec_context(&model, Some(handler)),
        )
        .expect("subcall should produce statement");
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.mnemonic.as_deref(), Some("lda"));
                assert_eq!(statement.operands.len(), 1);
                assert!(
                    matches!(statement.operands[0], Expr::Number(ref value, _) if value == "42")
                );
            }
            other => panic!("expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parser_vm_v2_parses_data_directive_dot_mnemonic() {
        let model = model_for_tests();
        let contract = parser_contract_for_tests();
        let handler: DynExprProcessingHandler<'_> =
            Rc::new(RefCell::new(Box::new(StubExprHandler)));
        let program = default_statement_program_for_tests();
        let line = parse_line_with_parser_vm_v2(
            vec![
                ident("data", 1, 5),
                colon(5),
                dot(7),
                ident("byte", 8, 12),
                ident("one", 13, 16),
                comma(16),
                ident("two", 18, 21),
            ],
            span(21, 21),
            None,
            &contract,
            &program,
            &request(),
            exec_context(&model, Some(handler)),
        )
        .expect("data directive should parse through v2");
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.label.expect("label").name, "data");
                assert_eq!(statement.mnemonic.as_deref(), Some(".byte"));
                assert_eq!(statement.operands.len(), 2);
            }
            other => panic!("expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parser_vm_v2_classifies_wi3_data_directive_names() {
        for name in [
            "byte", "db", "word", "dw", "long", "text", "null", "ptext", "fill", "res", "ds",
            "align",
        ] {
            assert!(
                is_v2_data_directive_shape(&[dot(1), ident(name, 2, 2 + name.len())]),
                "expected .{name} to use the v2 data directive path"
            );
        }
        assert!(!is_v2_data_directive_shape(&[
            dot(1),
            ident("section", 2, 9)
        ]));
    }

    #[test]
    fn parser_vm_v2_keeps_non_data_dot_directives_on_legacy_fallback() {
        let model = model_for_tests();
        let contract = parser_contract_for_tests();
        let program = RuntimeParserVmProgram {
            opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
            program: vec![ParserVmOpcodeV2::Fail as u8],
        };
        let line = parse_line_with_parser_vm_v2(
            vec![dot(1), ident("struct", 2, 8)],
            span(8, 8),
            None,
            &contract,
            &program,
            &request(),
            exec_context(&model, None),
        )
        .expect("non-data dot directive should still use legacy fallback");
        match line {
            LineAst::Statement(statement) => {
                assert_eq!(statement.mnemonic.as_deref(), Some(".struct"));
                assert!(statement.operands.is_empty());
            }
            other => panic!("expected statement, got {other:?}"),
        }
    }

    #[test]
    fn parser_vm_v2_rejects_checkpoint_depth_above_four() {
        let model = model_for_tests();
        let contract = parser_contract_for_tests();
        let program = RuntimeParserVmProgram {
            opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
            program: vec![
                ParserVmOpcodeV2::Checkpoint as u8,
                ParserVmOpcodeV2::Checkpoint as u8,
                ParserVmOpcodeV2::Checkpoint as u8,
                ParserVmOpcodeV2::Checkpoint as u8,
                ParserVmOpcodeV2::Checkpoint as u8,
            ],
        };
        let err = parse_line_with_parser_vm_v2(
            vec![ident("lda", 1, 4)],
            span(4, 4),
            None,
            &contract,
            &program,
            &request(),
            exec_context(&model, None),
        )
        .expect_err("fifth checkpoint should fail");
        assert!(err
            .message
            .contains(DIAG_PARSER_OPASM_V2_CHECKPOINT_DEPTH_EXCEEDED));
    }

    #[test]
    fn parser_vm_v2_rejects_forbidden_v1_cross_contract_opcode() {
        let model = model_for_tests();
        let contract = parser_contract_for_tests();
        let program = RuntimeParserVmProgram {
            opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
            program: vec![RETIRED_V1_PARSE_INSTRUCTION_ENVELOPE],
        };
        let err = parse_line_with_parser_vm_v2(
            vec![ident("lda", 1, 4)],
            span(4, 4),
            None,
            &contract,
            &program,
            &request(),
            exec_context(&model, None),
        )
        .expect_err("v1 parser opcode should fail in v2 program");
        assert!(err
            .message
            .contains(DIAG_PARSER_OPASM_V2_FORBIDDEN_CROSS_CONTRACT_OPCODE));
    }

    #[test]
    fn parser_vm_v2_rejects_misrouted_opcore_directive_shape() {
        let model = model_for_tests();
        let contract = parser_contract_for_tests();
        let program = RuntimeParserVmProgram {
            opcode_version: PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
            program: vec![ParserVmOpcodeV2::End as u8],
        };
        let err = parse_line_with_parser_vm_v2(
            vec![dot(1), ident("include", 2, 9)],
            span(9, 9),
            None,
            &contract,
            &program,
            &request(),
            exec_context(&model, None),
        )
        .expect_err("opcore directive should not reach opasm v2");
        assert!(err
            .message
            .contains(DIAG_PARSER_OPASM_V2_MISROUTED_OPCORE_DIRECTIVE));
    }
}
