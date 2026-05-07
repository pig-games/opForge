use opcore::parser::{BinaryOp, Expr, ParseError, UnaryOp};
use opcore::tokenizer::{OperatorKind, Span, Token, TokenKind};
use package::{ExvmOpcodeV2, ExvmOperatorKindV2, ExvmTokenKindV2};

use crate::vm_opcore::ExvmExecutionBudgets;

pub(crate) fn run_exvm_expression_parser_program(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    program: &[u8],
    budgets: ExvmExecutionBudgets,
) -> Result<Expr, ParseError> {
    if tokens.len() > budgets.max_token_count {
        return Err(ParseError {
            message: format!(
                "EXVM token budget exceeded ({}/{})",
                tokens.len(),
                budgets.max_token_count
            ),
            span: end_span,
        });
    }

    let mut runtime = ExvmV2Runtime {
        tokens,
        index: 0,
        end_span,
        end_token_text,
        steps: 0,
        loaded_token_text: None,
        last_peek_result: false,
        operator_spans: Vec::new(),
        budgets,
    };
    let expr = runtime.execute_expression(program)?;
    if runtime.index < runtime.tokens.len() {
        return Err(ParseError {
            message: "Unexpected trailing tokens".to_string(),
            span: runtime.tokens[runtime.index].span,
        });
    }
    Ok(expr)
}

struct ExvmV2Runtime {
    tokens: Vec<Token>,
    index: usize,
    end_span: Span,
    end_token_text: Option<String>,
    steps: usize,
    loaded_token_text: Option<String>,
    last_peek_result: bool,
    operator_spans: Vec<Span>,
    budgets: ExvmExecutionBudgets,
}

impl ExvmV2Runtime {
    fn execute_expression(&mut self, program: &[u8]) -> Result<Expr, ParseError> {
        self.execute_from(program, 0)
    }

    fn execute_from(&mut self, program: &[u8], mut pc: usize) -> Result<Expr, ParseError> {
        let mut output_stack = Vec::new();
        let mut call_stack = Vec::new();

        while pc < program.len() {
            self.consume_step()?;

            let opcode_pc = pc;
            let opcode_byte = program[pc];
            pc += 1;
            let opcode = ExvmOpcodeV2::from_u8(opcode_byte).ok_or_else(|| ParseError {
                message: format!("invalid EXVM opcode 0x{opcode_byte:02X} at pc={opcode_pc}"),
                span: self.current_span(),
            })?;

            match opcode {
                ExvmOpcodeV2::End => {
                    if !call_stack.is_empty() {
                        return Err(ParseError {
                            message: "EXVM program ended inside subroutine".to_string(),
                            span: self.current_span(),
                        });
                    }
                    return self.finish_output_stack(output_stack);
                }
                ExvmOpcodeV2::Jump => {
                    pc = self.read_jump_target(program, &mut pc, opcode_pc)?;
                }
                ExvmOpcodeV2::JumpIfTrue => {
                    let target = self.read_jump_target(program, &mut pc, opcode_pc)?;
                    if self.last_peek_result {
                        pc = target;
                    }
                }
                ExvmOpcodeV2::Call => {
                    let target = self.read_jump_target(program, &mut pc, opcode_pc)?;
                    call_stack.push(pc);
                    pc = target;
                }
                ExvmOpcodeV2::Return => {
                    let target = call_stack.pop().ok_or_else(|| ParseError {
                        message: "EXVM return without call".to_string(),
                        span: self.current_span(),
                    })?;
                    pc = target;
                }
                ExvmOpcodeV2::PeekKind => {
                    let kind_byte = self.read_u8(program, &mut pc, opcode_pc)?;
                    let kind = ExvmTokenKindV2::from_u8(kind_byte).ok_or_else(|| ParseError {
                        message: format!(
                            "invalid EXVM token kind 0x{kind_byte:02X} at pc={opcode_pc}"
                        ),
                        span: self.current_span(),
                    })?;
                    self.last_peek_result = self.peek_matches(kind);
                }
                ExvmOpcodeV2::PeekOperator => {
                    let operator = self.read_operator_kind(program, &mut pc, opcode_pc)?;
                    self.last_peek_result = self.peek_operator_matches(operator);
                }
                ExvmOpcodeV2::Advance => self.advance()?,
                ExvmOpcodeV2::ConsumeOperator => {
                    let operator = self.read_operator_kind(program, &mut pc, opcode_pc)?;
                    self.consume_operator(operator)?;
                }
                ExvmOpcodeV2::LoadTokenText => {
                    let token = self
                        .current_token()
                        .ok_or_else(|| self.expected_leaf_error())?;
                    self.loaded_token_text = Some(token.to_source_text());
                }
                ExvmOpcodeV2::BuildIdentifier => {
                    let token = self
                        .current_token()
                        .ok_or_else(|| self.expected_leaf_error())?;
                    let TokenKind::Identifier(name) = &token.kind else {
                        return Err(self.unexpected_token_error(token.span));
                    };
                    let name = self
                        .loaded_token_text
                        .clone()
                        .unwrap_or_else(|| name.clone());
                    output_stack.push(Expr::Identifier(name, token.span));
                }
                ExvmOpcodeV2::BuildNumber => {
                    let token = self
                        .current_token()
                        .ok_or_else(|| self.expected_leaf_error())?;
                    let TokenKind::Number(number) = &token.kind else {
                        return Err(self.unexpected_token_error(token.span));
                    };
                    let text = self
                        .loaded_token_text
                        .clone()
                        .unwrap_or_else(|| number.text.clone());
                    output_stack.push(Expr::Number(text, token.span));
                }
                ExvmOpcodeV2::BuildCurrentAddress => {
                    let token = self
                        .current_token()
                        .ok_or_else(|| self.expected_leaf_error())?;
                    if token.kind != TokenKind::Dollar {
                        return Err(self.unexpected_token_error(token.span));
                    }
                    output_stack.push(Expr::Dollar(token.span));
                }
                ExvmOpcodeV2::BuildUnary => {
                    let operator = self.read_operator_kind(program, &mut pc, opcode_pc)?;
                    let span = self.operator_spans.pop().ok_or_else(|| ParseError {
                        message: "EXVM operator stack underflow".to_string(),
                        span: self.current_span(),
                    })?;
                    let expr = output_stack.pop().ok_or_else(|| ParseError {
                        message: "EXVM output stack underflow".to_string(),
                        span: self.current_span(),
                    })?;
                    output_stack.push(self.build_unary_expr(operator, expr, span)?);
                }
                ExvmOpcodeV2::BuildBinary => {
                    let operator = self.read_operator_kind(program, &mut pc, opcode_pc)?;
                    let span = self.operator_spans.pop().ok_or_else(|| ParseError {
                        message: "EXVM operator stack underflow".to_string(),
                        span: self.current_span(),
                    })?;
                    let right = output_stack.pop().ok_or_else(|| ParseError {
                        message: "EXVM output stack underflow".to_string(),
                        span: self.current_span(),
                    })?;
                    let left = output_stack.pop().ok_or_else(|| ParseError {
                        message: "EXVM output stack underflow".to_string(),
                        span: self.current_span(),
                    })?;
                    output_stack.push(self.build_binary_expr(operator, left, right, span)?);
                }
                ExvmOpcodeV2::ParseGrouping => {
                    output_stack.push(self.parse_grouping(program)?);
                }
                ExvmOpcodeV2::EmitDiag => return Err(self.expected_leaf_error()),
                ExvmOpcodeV2::Fail => {
                    return Err(ParseError {
                        message: "EXVM program failed".to_string(),
                        span: self.current_span(),
                    });
                }
            }

            if output_stack.len() > self.budgets.max_stack_depth {
                return Err(ParseError {
                    message: format!(
                        "EXVM output stack depth exceeded ({}/{})",
                        output_stack.len(),
                        self.budgets.max_stack_depth
                    ),
                    span: self.current_span(),
                });
            }
        }

        Err(ParseError {
            message: "EXVM program missing End opcode".to_string(),
            span: self.current_span(),
        })
    }

    fn finish_output_stack(&self, mut output_stack: Vec<Expr>) -> Result<Expr, ParseError> {
        match output_stack.pop() {
            Some(expr) if output_stack.is_empty() => Ok(expr),
            Some(_) => Err(ParseError {
                message: "EXVM program ended with multiple expressions".to_string(),
                span: self.current_span(),
            }),
            None => Err(ParseError {
                message: "EXVM program ended without expression".to_string(),
                span: self.current_span(),
            }),
        }
    }

    fn parse_grouping(&mut self, program: &[u8]) -> Result<Expr, ParseError> {
        let token = self
            .current_token()
            .ok_or_else(|| self.expected_leaf_error())?;
        if token.kind != TokenKind::OpenParen {
            return Err(self.unexpected_token_error(token.span));
        }
        self.index += 1;
        self.loaded_token_text = None;

        let inner = self.execute_expression(program)?;
        if self
            .current_token()
            .is_some_and(|current| current.kind == TokenKind::Comma)
        {
            return Err(self.unexpected_token_error(self.current_span()));
        }
        if !self
            .current_token()
            .is_some_and(|current| current.kind == TokenKind::CloseParen)
        {
            return Err(ParseError {
                message: "Missing ')'".to_string(),
                span: self.current_span(),
            });
        }

        self.index += 1;
        self.loaded_token_text = None;
        Ok(inner)
    }

    fn consume_step(&mut self) -> Result<(), ParseError> {
        if self.steps >= self.budgets.max_steps {
            return Err(ParseError {
                message: format!(
                    "EXVM step budget exceeded ({}/{})",
                    self.steps, self.budgets.max_steps
                ),
                span: self.current_span(),
            });
        }
        self.steps += 1;
        Ok(())
    }

    fn read_jump_target(
        &self,
        program: &[u8],
        pc: &mut usize,
        opcode_pc: usize,
    ) -> Result<usize, ParseError> {
        let lo = self.read_u8(program, pc, opcode_pc)?;
        let hi = self.read_u8(program, pc, opcode_pc)?;
        let target = u16::from_le_bytes([lo, hi]) as usize;
        if target >= program.len() {
            return Err(ParseError {
                message: format!("EXVM jump target out of range at pc={opcode_pc}"),
                span: self.current_span(),
            });
        }
        Ok(target)
    }

    fn read_operator_kind(
        &self,
        program: &[u8],
        pc: &mut usize,
        opcode_pc: usize,
    ) -> Result<ExvmOperatorKindV2, ParseError> {
        let operator_byte = self.read_u8(program, pc, opcode_pc)?;
        ExvmOperatorKindV2::from_u8(operator_byte).ok_or_else(|| ParseError {
            message: format!("invalid EXVM operator kind 0x{operator_byte:02X} at pc={opcode_pc}"),
            span: self.current_span(),
        })
    }

    fn read_u8(&self, program: &[u8], pc: &mut usize, opcode_pc: usize) -> Result<u8, ParseError> {
        if *pc >= program.len() {
            return Err(ParseError {
                message: format!("EXVM program truncated at pc={opcode_pc}"),
                span: self.current_span(),
            });
        }
        let value = program[*pc];
        *pc += 1;
        Ok(value)
    }

    fn peek_matches(&self, kind: ExvmTokenKindV2) -> bool {
        match self.current_token().map(|token| &token.kind) {
            Some(TokenKind::Number(_)) => kind == ExvmTokenKindV2::Number,
            Some(TokenKind::Identifier(_)) => kind == ExvmTokenKindV2::Identifier,
            Some(TokenKind::Dollar) => kind == ExvmTokenKindV2::Dollar,
            Some(TokenKind::OpenParen) => kind == ExvmTokenKindV2::OpenParen,
            Some(TokenKind::CloseParen) => kind == ExvmTokenKindV2::CloseParen,
            _ => false,
        }
    }

    fn peek_operator_matches(&self, operator: ExvmOperatorKindV2) -> bool {
        match self.current_token().map(|token| &token.kind) {
            Some(TokenKind::Operator(current)) => *current == Self::operator_kind(operator),
            _ => false,
        }
    }

    fn consume_operator(&mut self, operator: ExvmOperatorKindV2) -> Result<(), ParseError> {
        let token = self
            .current_token()
            .ok_or_else(|| self.expected_leaf_error())?;
        if token.kind != TokenKind::Operator(Self::operator_kind(operator)) {
            return Err(self.unexpected_token_error(token.span));
        }
        self.operator_spans.push(token.span);
        self.index += 1;
        self.loaded_token_text = None;
        Ok(())
    }

    fn build_unary_expr(
        &self,
        operator: ExvmOperatorKindV2,
        expr: Expr,
        span: Span,
    ) -> Result<Expr, ParseError> {
        let op = match operator {
            ExvmOperatorKindV2::Plus => UnaryOp::Plus,
            ExvmOperatorKindV2::Minus => UnaryOp::Minus,
            ExvmOperatorKindV2::BitNot => UnaryOp::BitNot,
            ExvmOperatorKindV2::LogicNot => UnaryOp::LogicNot,
            ExvmOperatorKindV2::Lt => UnaryOp::Low,
            ExvmOperatorKindV2::Gt => UnaryOp::High,
            _ => {
                return Err(ParseError {
                    message: "unsupported EXVM unary operator".to_string(),
                    span,
                })
            }
        };
        Ok(Expr::Unary {
            op,
            expr: Box::new(expr),
            span,
        })
    }

    fn build_binary_expr(
        &self,
        operator: ExvmOperatorKindV2,
        left: Expr,
        right: Expr,
        span: Span,
    ) -> Result<Expr, ParseError> {
        let op = match operator {
            ExvmOperatorKindV2::Plus => BinaryOp::Add,
            ExvmOperatorKindV2::Minus => BinaryOp::Subtract,
            ExvmOperatorKindV2::Multiply => BinaryOp::Multiply,
            ExvmOperatorKindV2::Divide => BinaryOp::Divide,
            ExvmOperatorKindV2::Mod => BinaryOp::Mod,
            ExvmOperatorKindV2::Power => BinaryOp::Power,
            _ => {
                return Err(ParseError {
                    message: "unsupported EXVM binary operator".to_string(),
                    span,
                })
            }
        };
        Ok(Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span,
        })
    }

    fn operator_kind(operator: ExvmOperatorKindV2) -> OperatorKind {
        match operator {
            ExvmOperatorKindV2::Plus => OperatorKind::Plus,
            ExvmOperatorKindV2::Minus => OperatorKind::Minus,
            ExvmOperatorKindV2::Multiply => OperatorKind::Multiply,
            ExvmOperatorKindV2::Divide => OperatorKind::Divide,
            ExvmOperatorKindV2::Mod => OperatorKind::Mod,
            ExvmOperatorKindV2::Power => OperatorKind::Power,
            ExvmOperatorKindV2::BitNot => OperatorKind::BitNot,
            ExvmOperatorKindV2::LogicNot => OperatorKind::LogicNot,
            ExvmOperatorKindV2::Lt => OperatorKind::Lt,
            ExvmOperatorKindV2::Gt => OperatorKind::Gt,
        }
    }

    fn advance(&mut self) -> Result<(), ParseError> {
        if self.current_token().is_none() {
            return Err(self.expected_leaf_error());
        }
        self.index += 1;
        self.loaded_token_text = None;
        Ok(())
    }

    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn current_span(&self) -> Span {
        self.current_token()
            .map(|token| token.span)
            .unwrap_or(self.end_span)
    }

    fn unexpected_token_error(&self, span: Span) -> ParseError {
        ParseError {
            message: "Unexpected token in expression".to_string(),
            span,
        }
    }

    fn expected_leaf_error(&self) -> ParseError {
        match self.current_token() {
            Some(token) => self.unexpected_token_error(token.span),
            None => ParseError {
                message: match self.end_token_text.as_deref() {
                    Some(token) => format!("Expected label or numeric constant, found: {token}"),
                    None => "Unexpected end of expression".to_string(),
                },
                span: self.end_span,
            },
        }
    }
}
