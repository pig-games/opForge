use opcore::parser::{Expr, ParseError};
use opcore::tokenizer::{Span, Token, TokenKind};
use package::{ExvmOpcodeV2, ExvmTokenKindV2};

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
        budgets,
    };
    let expr = runtime.execute(program)?;
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
    budgets: ExvmExecutionBudgets,
}

impl ExvmV2Runtime {
    fn execute(&mut self, program: &[u8]) -> Result<Expr, ParseError> {
        let mut pc = 0usize;
        let mut output_stack = Vec::new();

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
                    return match output_stack.pop() {
                        Some(expr) if output_stack.is_empty() => Ok(expr),
                        Some(_) => Err(ParseError {
                            message: "EXVM program ended with multiple expressions".to_string(),
                            span: self.current_span(),
                        }),
                        None => Err(ParseError {
                            message: "EXVM program ended without expression".to_string(),
                            span: self.current_span(),
                        }),
                    };
                }
                ExvmOpcodeV2::JumpIfTrue => {
                    let target = self.read_jump_target(program, &mut pc, opcode_pc)?;
                    if self.last_peek_result {
                        pc = target;
                    }
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
                ExvmOpcodeV2::Advance => self.advance()?,
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

    fn parse_grouping(&mut self, program: &[u8]) -> Result<Expr, ParseError> {
        let token = self
            .current_token()
            .ok_or_else(|| self.expected_leaf_error())?;
        if token.kind != TokenKind::OpenParen {
            return Err(self.unexpected_token_error(token.span));
        }
        self.index += 1;
        self.loaded_token_text = None;

        let inner = self.execute(program)?;
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
