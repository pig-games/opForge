use opcore::expr_vm::{
    PortableExprDirectLeaf, PortableExprError, PortableExprProgram, PortableExprProgramBuilder,
};
use opcore::expression::expr_span;
use opcore::parser::{BinaryOp, Expr, ParseError, UnaryOp};
use opcore::tokenizer::{OperatorKind, Span, Token, TokenKind};
use package::{ExvmOpcodeV2, ExvmOperatorKindV2, ExvmTokenKindV2};

use crate::vm_opcore::ExvmExecutionBudgets;

pub(crate) trait ExvmRuntimeBackend {
    type Value;
    type FinalOutput;

    fn build_identifier(&mut self, name: String, span: Span) -> Result<Self::Value, ParseError>;
    fn build_number(&mut self, text: String, span: Span) -> Result<Self::Value, ParseError>;
    fn build_current_address(&mut self, span: Span) -> Result<Self::Value, ParseError>;
    fn build_unary(
        &mut self,
        operator: ExvmOperatorKindV2,
        expr: Self::Value,
        span: Span,
    ) -> Result<Self::Value, ParseError>;
    fn build_binary(
        &mut self,
        operator: ExvmOperatorKindV2,
        left: Self::Value,
        right: Self::Value,
        span: Span,
    ) -> Result<Self::Value, ParseError>;
    fn build_ternary(
        &mut self,
        cond: Self::Value,
        then_expr: Self::Value,
        else_expr: Self::Value,
        span: Span,
    ) -> Result<Self::Value, ParseError>;
    fn build_range(
        &mut self,
        start: Self::Value,
        end: Self::Value,
        step: Option<Self::Value>,
        inclusive: bool,
        span: Span,
    ) -> Result<Self::Value, ParseError>;
    fn build_list(
        &mut self,
        elements: Vec<Self::Value>,
        span: Span,
    ) -> Result<Self::Value, ParseError>;
    fn struct_literal_type_name(&self, value: &Self::Value) -> Option<(String, Span)>;
    fn build_struct_literal(
        &mut self,
        type_name: String,
        fields: Vec<(String, Self::Value)>,
        span: Span,
    ) -> Result<Self::Value, ParseError>;
    fn value_span(&self, value: &Self::Value) -> Span;
    fn build_index(
        &mut self,
        base: Self::Value,
        index: Self::Value,
        span: Span,
    ) -> Result<Self::Value, ParseError>;
    fn build_member(
        &mut self,
        base: Self::Value,
        field: String,
        span: Span,
    ) -> Result<Self::Value, ParseError>;
    fn finish_value(&mut self, value: Self::Value) -> Result<Self::FinalOutput, ParseError>;
}

struct AstExprBackend;

impl ExvmRuntimeBackend for AstExprBackend {
    type Value = Expr;
    type FinalOutput = Expr;

    fn build_identifier(&mut self, name: String, span: Span) -> Result<Self::Value, ParseError> {
        Ok(Expr::Identifier(name, span))
    }

    fn build_number(&mut self, text: String, span: Span) -> Result<Self::Value, ParseError> {
        Ok(Expr::Number(text, span))
    }

    fn build_current_address(&mut self, span: Span) -> Result<Self::Value, ParseError> {
        Ok(Expr::Dollar(span))
    }

    fn build_unary(
        &mut self,
        operator: ExvmOperatorKindV2,
        expr: Self::Value,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        let op = exvm_unary_operator(operator, span)?;
        Ok(Expr::Unary {
            op,
            expr: Box::new(expr),
            span,
        })
    }

    fn build_binary(
        &mut self,
        operator: ExvmOperatorKindV2,
        left: Self::Value,
        right: Self::Value,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        let op = exvm_binary_operator(operator, span)?;
        Ok(Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
            span,
        })
    }

    fn build_ternary(
        &mut self,
        cond: Self::Value,
        then_expr: Self::Value,
        else_expr: Self::Value,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Expr::Ternary {
            cond: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
            span,
        })
    }

    fn build_range(
        &mut self,
        start: Self::Value,
        end: Self::Value,
        step: Option<Self::Value>,
        inclusive: bool,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Expr::Range {
            start: Box::new(start),
            end: Box::new(end),
            step: step.map(Box::new),
            inclusive,
            span,
        })
    }

    fn build_list(
        &mut self,
        elements: Vec<Self::Value>,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Expr::List(elements, span))
    }

    fn struct_literal_type_name(&self, value: &Self::Value) -> Option<(String, Span)> {
        match value {
            Expr::Identifier(name, span) | Expr::Register(name, span) => {
                Some((name.clone(), *span))
            }
            _ => None,
        }
    }

    fn build_struct_literal(
        &mut self,
        type_name: String,
        fields: Vec<(String, Self::Value)>,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Expr::StructLiteral {
            type_name,
            fields,
            span,
        })
    }

    fn value_span(&self, value: &Self::Value) -> Span {
        expr_span(value)
    }

    fn build_index(
        &mut self,
        base: Self::Value,
        index: Self::Value,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Expr::Index {
            base: Box::new(base),
            index: Box::new(index),
            span,
        })
    }

    fn build_member(
        &mut self,
        base: Self::Value,
        field: String,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Expr::Member {
            base: Box::new(base),
            field,
            span,
        })
    }

    fn finish_value(&mut self, value: Self::Value) -> Result<Self::FinalOutput, ParseError> {
        Ok(value)
    }
}

#[cfg_attr(not(test), allow(dead_code))]
#[derive(Clone, Debug)]
struct PortableExprRuntimeValue {
    span: Span,
    struct_literal_type_name: Option<String>,
    node: PortableExprRuntimeNode,
}

#[cfg_attr(not(test), allow(dead_code))]
#[derive(Clone, Debug)]
enum PortableExprRuntimeNode {
    Leaf(PortableExprDirectLeaf),
    Unary {
        operator: ExvmOperatorKindV2,
        expr: Box<PortableExprRuntimeValue>,
    },
    Binary {
        operator: ExvmOperatorKindV2,
        left: Box<PortableExprRuntimeValue>,
        right: Box<PortableExprRuntimeValue>,
    },
    Ternary {
        cond: Box<PortableExprRuntimeValue>,
        then_expr: Box<PortableExprRuntimeValue>,
        else_expr: Box<PortableExprRuntimeValue>,
    },
    Range {
        start: Box<PortableExprRuntimeValue>,
        end: Box<PortableExprRuntimeValue>,
        step: Option<Box<PortableExprRuntimeValue>>,
        inclusive: bool,
    },
    List(Vec<PortableExprRuntimeValue>),
    StructLiteral {
        type_name: String,
        fields: Vec<(String, PortableExprRuntimeValue)>,
    },
    Index {
        base: Box<PortableExprRuntimeValue>,
        index: Box<PortableExprRuntimeValue>,
    },
    Member {
        base: Box<PortableExprRuntimeValue>,
        field: String,
    },
}

#[cfg_attr(not(test), allow(dead_code))]
struct PortableExprProgramBackend {
    expr_opcode_version: u16,
}

impl PortableExprProgramBackend {
    fn new(expr_opcode_version: u16, end_span: Span) -> Result<Self, ParseError> {
        PortableExprProgramBuilder::for_scalar(expr_opcode_version)
            .map(|_| Self {
                expr_opcode_version,
            })
            .map_err(|err| portable_expr_error_to_parse_error(err, end_span))
    }

    fn value(
        span: Span,
        struct_literal_type_name: Option<String>,
        node: PortableExprRuntimeNode,
    ) -> PortableExprRuntimeValue {
        PortableExprRuntimeValue {
            span,
            struct_literal_type_name,
            node,
        }
    }

    fn emit_value(
        builder: &mut PortableExprProgramBuilder,
        value: &PortableExprRuntimeValue,
    ) -> Result<(), ParseError> {
        match &value.node {
            PortableExprRuntimeNode::Leaf(leaf) => builder
                .emit_direct_leaf(leaf)
                .map_err(|err| portable_expr_error_to_parse_error(err, value.span)),
            PortableExprRuntimeNode::Unary { operator, expr } => {
                Self::emit_value(builder, expr)?;
                builder
                    .emit_unary(exvm_unary_operator(*operator, value.span)?)
                    .map_err(|err| portable_expr_error_to_parse_error(err, value.span))
            }
            PortableExprRuntimeNode::Binary {
                operator,
                left,
                right,
            } => {
                Self::emit_value(builder, left)?;
                Self::emit_value(builder, right)?;
                builder
                    .emit_binary(exvm_binary_operator(*operator, value.span)?)
                    .map_err(|err| portable_expr_error_to_parse_error(err, value.span))
            }
            PortableExprRuntimeNode::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                Self::emit_value(builder, cond)?;
                Self::emit_value(builder, then_expr)?;
                Self::emit_value(builder, else_expr)?;
                builder
                    .emit_ternary()
                    .map_err(|err| portable_expr_error_to_parse_error(err, value.span))
            }
            PortableExprRuntimeNode::Range {
                start,
                end,
                step,
                inclusive,
            } => {
                Self::emit_value(builder, start)?;
                Self::emit_value(builder, end)?;
                if let Some(step) = step {
                    Self::emit_value(builder, step)?;
                }
                builder
                    .emit_range(step.is_some(), *inclusive)
                    .map_err(|err| portable_expr_error_to_parse_error(err, value.span))
            }
            PortableExprRuntimeNode::List(elements) => {
                for element in elements {
                    Self::emit_value(builder, element)?;
                }
                builder
                    .emit_list(elements.len())
                    .map_err(|err| portable_expr_error_to_parse_error(err, value.span))
            }
            PortableExprRuntimeNode::StructLiteral { type_name, fields } => {
                for (_, field_value) in fields {
                    Self::emit_value(builder, field_value)?;
                }
                let field_names = fields
                    .iter()
                    .map(|(name, _)| name.clone())
                    .collect::<Vec<_>>();
                builder
                    .emit_struct_literal(type_name, &field_names)
                    .map_err(|err| portable_expr_error_to_parse_error(err, value.span))
            }
            PortableExprRuntimeNode::Index { base, index } => {
                Self::emit_value(builder, base)?;
                Self::emit_value(builder, index)?;
                builder
                    .emit_index()
                    .map_err(|err| portable_expr_error_to_parse_error(err, value.span))
            }
            PortableExprRuntimeNode::Member { base, field } => {
                Self::emit_value(builder, base)?;
                builder
                    .emit_member(field)
                    .map_err(|err| portable_expr_error_to_parse_error(err, value.span))
            }
        }
    }
}

impl ExvmRuntimeBackend for PortableExprProgramBackend {
    type Value = PortableExprRuntimeValue;
    type FinalOutput = PortableExprProgram;

    fn build_identifier(&mut self, name: String, span: Span) -> Result<Self::Value, ParseError> {
        Ok(Self::value(
            span,
            Some(name.clone()),
            PortableExprRuntimeNode::Leaf(PortableExprDirectLeaf::SymbolName(name)),
        ))
    }

    fn build_number(&mut self, text: String, span: Span) -> Result<Self::Value, ParseError> {
        Ok(Self::value(
            span,
            None,
            PortableExprRuntimeNode::Leaf(PortableExprDirectLeaf::NumberText(text)),
        ))
    }

    fn build_current_address(&mut self, span: Span) -> Result<Self::Value, ParseError> {
        Ok(Self::value(
            span,
            None,
            PortableExprRuntimeNode::Leaf(PortableExprDirectLeaf::CurrentAddress),
        ))
    }

    fn build_unary(
        &mut self,
        operator: ExvmOperatorKindV2,
        expr: Self::Value,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Self::value(
            span,
            None,
            PortableExprRuntimeNode::Unary {
                operator,
                expr: Box::new(expr),
            },
        ))
    }

    fn build_binary(
        &mut self,
        operator: ExvmOperatorKindV2,
        left: Self::Value,
        right: Self::Value,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Self::value(
            span,
            None,
            PortableExprRuntimeNode::Binary {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            },
        ))
    }

    fn build_ternary(
        &mut self,
        cond: Self::Value,
        then_expr: Self::Value,
        else_expr: Self::Value,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Self::value(
            span,
            None,
            PortableExprRuntimeNode::Ternary {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            },
        ))
    }

    fn build_range(
        &mut self,
        start: Self::Value,
        end: Self::Value,
        step: Option<Self::Value>,
        inclusive: bool,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Self::value(
            span,
            None,
            PortableExprRuntimeNode::Range {
                start: Box::new(start),
                end: Box::new(end),
                step: step.map(Box::new),
                inclusive,
            },
        ))
    }

    fn build_list(
        &mut self,
        elements: Vec<Self::Value>,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Self::value(
            span,
            None,
            PortableExprRuntimeNode::List(elements),
        ))
    }

    fn struct_literal_type_name(&self, value: &Self::Value) -> Option<(String, Span)> {
        value
            .struct_literal_type_name
            .clone()
            .map(|name| (name, value.span))
    }

    fn build_struct_literal(
        &mut self,
        type_name: String,
        fields: Vec<(String, Self::Value)>,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Self::value(
            span,
            None,
            PortableExprRuntimeNode::StructLiteral { type_name, fields },
        ))
    }

    fn value_span(&self, value: &Self::Value) -> Span {
        value.span
    }

    fn build_index(
        &mut self,
        base: Self::Value,
        index: Self::Value,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Self::value(
            span,
            None,
            PortableExprRuntimeNode::Index {
                base: Box::new(base),
                index: Box::new(index),
            },
        ))
    }

    fn build_member(
        &mut self,
        base: Self::Value,
        field: String,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Self::value(
            span,
            None,
            PortableExprRuntimeNode::Member {
                base: Box::new(base),
                field,
            },
        ))
    }

    fn finish_value(&mut self, value: Self::Value) -> Result<Self::FinalOutput, ParseError> {
        let mut builder = PortableExprProgramBuilder::for_scalar(self.expr_opcode_version)
            .map_err(|err| portable_expr_error_to_parse_error(err, value.span))?;
        Self::emit_value(&mut builder, &value)?;
        Ok(builder.finish())
    }
}

pub(crate) fn run_exvm_expression_parser_program(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    program: &[u8],
    budgets: ExvmExecutionBudgets,
) -> Result<Expr, ParseError> {
    run_exvm_expression_parser_program_with_backend(
        tokens,
        end_span,
        end_token_text,
        program,
        budgets,
        AstExprBackend,
    )
}

#[cfg_attr(not(test), allow(dead_code))]
pub(crate) fn run_exvm_expression_parser_program_to_portable_program(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    program: &[u8],
    budgets: ExvmExecutionBudgets,
    expr_opcode_version: u16,
) -> Result<PortableExprProgram, ParseError> {
    run_exvm_expression_parser_program_with_backend(
        tokens,
        end_span,
        end_token_text,
        program,
        budgets,
        PortableExprProgramBackend::new(expr_opcode_version, end_span)?,
    )
}

pub(crate) fn run_exvm_expression_parser_program_with_backend<B: ExvmRuntimeBackend>(
    tokens: Vec<Token>,
    end_span: Span,
    end_token_text: Option<String>,
    program: &[u8],
    budgets: ExvmExecutionBudgets,
    backend: B,
) -> Result<B::FinalOutput, ParseError> {
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
        build_spans: Vec::new(),
        budgets,
        backend,
    };
    let expr = runtime.execute_expression(program)?;
    if runtime.index < runtime.tokens.len() {
        return Err(ParseError {
            message: "Unexpected trailing tokens".to_string(),
            span: runtime.tokens[runtime.index].span,
        });
    }
    runtime.finish_value(expr)
}

struct ExvmV2Runtime<B: ExvmRuntimeBackend> {
    tokens: Vec<Token>,
    index: usize,
    end_span: Span,
    end_token_text: Option<String>,
    steps: usize,
    loaded_token_text: Option<String>,
    last_peek_result: bool,
    build_spans: Vec<Span>,
    budgets: ExvmExecutionBudgets,
    backend: B,
}

impl<B: ExvmRuntimeBackend> ExvmV2Runtime<B> {
    fn execute_expression(&mut self, program: &[u8]) -> Result<B::Value, ParseError> {
        self.execute_from(program, 0)
    }

    fn finish_value(&mut self, value: B::Value) -> Result<B::FinalOutput, ParseError> {
        self.backend.finish_value(value)
    }

    fn execute_from(&mut self, program: &[u8], mut pc: usize) -> Result<B::Value, ParseError> {
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
                ExvmOpcodeV2::ConsumeKind => {
                    let kind = self.read_token_kind(program, &mut pc, opcode_pc)?;
                    self.consume_kind(kind)?;
                }
                ExvmOpcodeV2::LoadTokenText => {
                    let token = self
                        .current_token()
                        .ok_or_else(|| self.expected_leaf_error())?;
                    self.loaded_token_text = Some(token.to_source_text());
                }
                ExvmOpcodeV2::BuildIdentifier => {
                    let (name, span) = match self.current_token() {
                        Some(Token {
                            kind: TokenKind::Identifier(name),
                            span,
                        }) => {
                            let name = self
                                .loaded_token_text
                                .clone()
                                .unwrap_or_else(|| name.clone());
                            (name, *span)
                        }
                        Some(token) => return Err(self.unexpected_token_error(token.span)),
                        None => return Err(self.expected_leaf_error()),
                    };
                    output_stack.push(self.backend.build_identifier(name, span)?);
                }
                ExvmOpcodeV2::BuildNumber => {
                    let (text, span) = match self.current_token() {
                        Some(Token {
                            kind: TokenKind::Number(number),
                            span,
                        }) => {
                            let text = self
                                .loaded_token_text
                                .clone()
                                .unwrap_or_else(|| number.text.clone());
                            (text, *span)
                        }
                        Some(token) => return Err(self.unexpected_token_error(token.span)),
                        None => return Err(self.expected_leaf_error()),
                    };
                    output_stack.push(self.backend.build_number(text, span)?);
                }
                ExvmOpcodeV2::BuildCurrentAddress => {
                    let span = match self.current_token() {
                        Some(Token {
                            kind: TokenKind::Dollar,
                            span,
                        }) => *span,
                        Some(token) => return Err(self.unexpected_token_error(token.span)),
                        None => return Err(self.expected_leaf_error()),
                    };
                    output_stack.push(self.backend.build_current_address(span)?);
                }
                ExvmOpcodeV2::BuildUnary => {
                    let operator = self.read_operator_kind(program, &mut pc, opcode_pc)?;
                    let span = self.pop_build_span()?;
                    let expr = self.pop_output(&mut output_stack)?;
                    output_stack.push(self.backend.build_unary(operator, expr, span)?);
                }
                ExvmOpcodeV2::BuildBinary => {
                    let operator = self.read_operator_kind(program, &mut pc, opcode_pc)?;
                    let span = self.pop_build_span()?;
                    let right = self.pop_output(&mut output_stack)?;
                    let left = self.pop_output(&mut output_stack)?;
                    output_stack.push(self.backend.build_binary(operator, left, right, span)?);
                }
                ExvmOpcodeV2::BuildTernary => {
                    let span = self.pop_build_span()?;
                    let else_expr = self.pop_output(&mut output_stack)?;
                    let then_expr = self.pop_output(&mut output_stack)?;
                    let cond = self.pop_output(&mut output_stack)?;
                    output_stack.push(
                        self.backend
                            .build_ternary(cond, then_expr, else_expr, span)?,
                    );
                }
                ExvmOpcodeV2::BuildRange => {
                    let span = self.pop_build_span()?;
                    let flags = self.read_u8(program, &mut pc, opcode_pc)?;
                    if flags & !0x03 != 0 {
                        return Err(ParseError {
                            message: format!(
                                "invalid EXVM range flags 0x{flags:02X} at pc={opcode_pc}"
                            ),
                            span: self.current_span(),
                        });
                    }
                    let has_step = flags & 0x02 != 0;
                    let inclusive = flags & 0x01 != 0;
                    let step = if has_step {
                        Some(self.pop_output(&mut output_stack)?)
                    } else {
                        None
                    };
                    let end = self.pop_output(&mut output_stack)?;
                    let start = self.pop_output(&mut output_stack)?;
                    output_stack.push(
                        self.backend
                            .build_range(start, end, step, inclusive, span)?,
                    );
                }
                ExvmOpcodeV2::ParseGrouping => {
                    output_stack.push(self.parse_grouping(program)?);
                }
                ExvmOpcodeV2::ParseList => {
                    output_stack.push(self.parse_list(program)?);
                }
                ExvmOpcodeV2::ParseStructLiteralIfPresent => {
                    let expr = self.pop_output(&mut output_stack)?;
                    output_stack.push(self.parse_struct_literal_if_present(program, expr)?);
                }
                ExvmOpcodeV2::ParsePostfixChain => {
                    let expr = self.pop_output(&mut output_stack)?;
                    output_stack.push(self.parse_postfix_chain(program, expr)?);
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

    fn finish_output_stack(&self, mut output_stack: Vec<B::Value>) -> Result<B::Value, ParseError> {
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

    fn parse_grouping(&mut self, program: &[u8]) -> Result<B::Value, ParseError> {
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

    fn parse_list(&mut self, program: &[u8]) -> Result<B::Value, ParseError> {
        let token = self
            .current_token()
            .ok_or_else(|| self.expected_leaf_error())?;
        if token.kind != TokenKind::OpenBrace {
            return Err(self.unexpected_token_error(token.span));
        }
        let open_span = token.span;
        self.index += 1;
        self.loaded_token_text = None;

        let mut elements = Vec::new();
        if !self
            .current_token()
            .is_some_and(|current| current.kind == TokenKind::CloseBrace)
        {
            elements.push(self.execute_expression(program)?);
            while self
                .current_token()
                .is_some_and(|current| current.kind == TokenKind::Comma)
            {
                self.index += 1;
                self.loaded_token_text = None;
                elements.push(self.execute_expression(program)?);
            }
            if !self
                .current_token()
                .is_some_and(|current| current.kind == TokenKind::CloseBrace)
            {
                return Err(ParseError {
                    message: "Missing '}' in list literal".to_string(),
                    span: self.current_span(),
                });
            }
        }

        let close_span = self.current_span();
        self.index += 1;
        self.loaded_token_text = None;
        self.backend.build_list(
            elements,
            Span {
                line: open_span.line,
                col_start: open_span.col_start,
                col_end: close_span.col_end,
            },
        )
    }

    fn parse_struct_literal_if_present(
        &mut self,
        program: &[u8],
        expr: B::Value,
    ) -> Result<B::Value, ParseError> {
        let Some((type_name, type_span)) = self.backend.struct_literal_type_name(&expr) else {
            return Ok(expr);
        };
        if !self
            .current_token()
            .is_some_and(|current| current.kind == TokenKind::OpenBrace)
        {
            return Ok(expr);
        }

        self.index += 1;
        self.loaded_token_text = None;

        let mut fields = Vec::new();
        if !self
            .current_token()
            .is_some_and(|current| current.kind == TokenKind::CloseBrace)
        {
            loop {
                let (field_name, _) =
                    self.consume_identifier_like("Expected field name in struct literal")?;
                if !self.consume_raw_kind(TokenKind::Colon) {
                    return Err(ParseError {
                        message: "Expected ':' after field name in struct literal".to_string(),
                        span: self.current_span(),
                    });
                }
                let field_expr = self.execute_expression(program)?;
                fields.push((field_name, field_expr));

                if self.consume_raw_kind(TokenKind::Comma) {
                    continue;
                }
                if !self.consume_raw_kind(TokenKind::CloseBrace) {
                    return Err(ParseError {
                        message: "Missing '}' in struct literal".to_string(),
                        span: self.current_span(),
                    });
                }
                break;
            }
        } else {
            self.index += 1;
            self.loaded_token_text = None;
        }

        let close_span = self.previous_span();
        self.backend.build_struct_literal(
            type_name,
            fields,
            Span {
                line: type_span.line,
                col_start: type_span.col_start,
                col_end: close_span.col_end,
            },
        )
    }

    fn parse_postfix_chain(
        &mut self,
        program: &[u8],
        mut expr: B::Value,
    ) -> Result<B::Value, ParseError> {
        loop {
            if self.consume_raw_kind(TokenKind::OpenBracket) {
                let index = self.execute_expression(program)?;
                let close_span = self.current_span();
                if !self.consume_raw_kind(TokenKind::CloseBracket) {
                    return Err(ParseError {
                        message: "Missing ']' in index expression".to_string(),
                        span: self.current_span(),
                    });
                }
                let start_span = self.backend.value_span(&expr);
                expr = self.backend.build_index(
                    expr,
                    index,
                    Span {
                        line: start_span.line,
                        col_start: start_span.col_start,
                        col_end: close_span.col_end,
                    },
                )?;
                continue;
            }

            if self.consume_raw_kind(TokenKind::Dot) {
                let (field, field_span) =
                    self.consume_identifier_like("Expected member name after '.'")?;
                let start_span = self.backend.value_span(&expr);
                expr = self.backend.build_member(
                    expr,
                    field,
                    Span {
                        line: start_span.line,
                        col_start: start_span.col_start,
                        col_end: field_span.col_end,
                    },
                )?;
                continue;
            }

            break;
        }
        Ok(expr)
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

    fn read_token_kind(
        &self,
        program: &[u8],
        pc: &mut usize,
        opcode_pc: usize,
    ) -> Result<ExvmTokenKindV2, ParseError> {
        let kind_byte = self.read_u8(program, pc, opcode_pc)?;
        ExvmTokenKindV2::from_u8(kind_byte).ok_or_else(|| ParseError {
            message: format!("invalid EXVM token kind 0x{kind_byte:02X} at pc={opcode_pc}"),
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
        self.current_token()
            .is_some_and(|token| token_matches_kind(&token.kind, kind))
    }

    fn peek_operator_matches(&self, operator: ExvmOperatorKindV2) -> bool {
        match self.current_token().map(|token| &token.kind) {
            Some(TokenKind::Operator(current)) => *current == operator_kind(operator),
            _ => false,
        }
    }

    fn consume_operator(&mut self, operator: ExvmOperatorKindV2) -> Result<(), ParseError> {
        let token = self
            .current_token()
            .ok_or_else(|| self.expected_leaf_error())?;
        if token.kind != TokenKind::Operator(operator_kind(operator)) {
            return Err(self.unexpected_token_error(token.span));
        }
        self.build_spans.push(token.span);
        self.index += 1;
        self.loaded_token_text = None;
        Ok(())
    }

    fn consume_kind(&mut self, kind: ExvmTokenKindV2) -> Result<(), ParseError> {
        let token = self.current_token().ok_or_else(|| match kind {
            ExvmTokenKindV2::Colon => self.missing_colon_error(),
            _ => self.expected_leaf_error(),
        })?;
        if !token_matches_kind(&token.kind, kind) {
            return Err(match kind {
                ExvmTokenKindV2::Colon => self.missing_colon_error(),
                _ => self.unexpected_token_error(token.span),
            });
        }
        if kind == ExvmTokenKindV2::Question {
            self.build_spans.push(token.span);
        }
        self.index += 1;
        self.loaded_token_text = None;
        Ok(())
    }

    fn consume_raw_kind(&mut self, kind: TokenKind) -> bool {
        if self
            .current_token()
            .is_some_and(|current| current.kind == kind)
        {
            self.index += 1;
            self.loaded_token_text = None;
            true
        } else {
            false
        }
    }

    fn consume_identifier_like(
        &mut self,
        message: &'static str,
    ) -> Result<(String, Span), ParseError> {
        match self.current_token() {
            Some(Token {
                kind: TokenKind::Identifier(name),
                span,
            })
            | Some(Token {
                kind: TokenKind::Register(name),
                span,
            }) => {
                let name = name.clone();
                let span = *span;
                self.index += 1;
                self.loaded_token_text = None;
                Ok((name, span))
            }
            Some(token) => Err(ParseError {
                message: message.to_string(),
                span: token.span,
            }),
            None => Err(ParseError {
                message: message.to_string(),
                span: self.end_span,
            }),
        }
    }

    fn pop_build_span(&mut self) -> Result<Span, ParseError> {
        self.build_spans.pop().ok_or_else(|| ParseError {
            message: "EXVM operator stack underflow".to_string(),
            span: self.current_span(),
        })
    }

    fn pop_output(&self, output_stack: &mut Vec<B::Value>) -> Result<B::Value, ParseError> {
        output_stack.pop().ok_or_else(|| ParseError {
            message: "EXVM output stack underflow".to_string(),
            span: self.current_span(),
        })
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

    fn previous_span(&self) -> Span {
        self.tokens
            .get(self.index.saturating_sub(1))
            .map(|token| token.span)
            .unwrap_or(self.end_span)
    }

    fn unexpected_token_error(&self, span: Span) -> ParseError {
        ParseError {
            message: "Unexpected token in expression".to_string(),
            span,
        }
    }

    fn missing_colon_error(&self) -> ParseError {
        ParseError {
            message: "Missing ':' in conditional expression".to_string(),
            span: self.current_span(),
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
        ExvmOperatorKindV2::Shl => OperatorKind::Shl,
        ExvmOperatorKindV2::Shr => OperatorKind::Shr,
        ExvmOperatorKindV2::Eq => OperatorKind::Eq,
        ExvmOperatorKindV2::Ne => OperatorKind::Ne,
        ExvmOperatorKindV2::Ge => OperatorKind::Ge,
        ExvmOperatorKindV2::Le => OperatorKind::Le,
        ExvmOperatorKindV2::BitAnd => OperatorKind::BitAnd,
        ExvmOperatorKindV2::BitOr => OperatorKind::BitOr,
        ExvmOperatorKindV2::BitXor => OperatorKind::BitXor,
        ExvmOperatorKindV2::LogicAnd => OperatorKind::LogicAnd,
        ExvmOperatorKindV2::LogicOr => OperatorKind::LogicOr,
        ExvmOperatorKindV2::LogicXor => OperatorKind::LogicXor,
        ExvmOperatorKindV2::Range => OperatorKind::Range,
        ExvmOperatorKindV2::RangeInclusive => OperatorKind::RangeInclusive,
    }
}

fn exvm_unary_operator(operator: ExvmOperatorKindV2, span: Span) -> Result<UnaryOp, ParseError> {
    match operator {
        ExvmOperatorKindV2::Plus => Ok(UnaryOp::Plus),
        ExvmOperatorKindV2::Minus => Ok(UnaryOp::Minus),
        ExvmOperatorKindV2::BitNot => Ok(UnaryOp::BitNot),
        ExvmOperatorKindV2::LogicNot => Ok(UnaryOp::LogicNot),
        ExvmOperatorKindV2::Lt => Ok(UnaryOp::Low),
        ExvmOperatorKindV2::Gt => Ok(UnaryOp::High),
        _ => Err(ParseError {
            message: "unsupported EXVM unary operator".to_string(),
            span,
        }),
    }
}

fn exvm_binary_operator(operator: ExvmOperatorKindV2, span: Span) -> Result<BinaryOp, ParseError> {
    match operator {
        ExvmOperatorKindV2::Plus => Ok(BinaryOp::Add),
        ExvmOperatorKindV2::Minus => Ok(BinaryOp::Subtract),
        ExvmOperatorKindV2::Multiply => Ok(BinaryOp::Multiply),
        ExvmOperatorKindV2::Divide => Ok(BinaryOp::Divide),
        ExvmOperatorKindV2::Mod => Ok(BinaryOp::Mod),
        ExvmOperatorKindV2::Power => Ok(BinaryOp::Power),
        ExvmOperatorKindV2::Shl => Ok(BinaryOp::Shl),
        ExvmOperatorKindV2::Shr => Ok(BinaryOp::Shr),
        ExvmOperatorKindV2::Eq => Ok(BinaryOp::Eq),
        ExvmOperatorKindV2::Ne => Ok(BinaryOp::Ne),
        ExvmOperatorKindV2::Ge => Ok(BinaryOp::Ge),
        ExvmOperatorKindV2::Gt => Ok(BinaryOp::Gt),
        ExvmOperatorKindV2::Le => Ok(BinaryOp::Le),
        ExvmOperatorKindV2::Lt => Ok(BinaryOp::Lt),
        ExvmOperatorKindV2::BitAnd => Ok(BinaryOp::BitAnd),
        ExvmOperatorKindV2::BitOr => Ok(BinaryOp::BitOr),
        ExvmOperatorKindV2::BitXor => Ok(BinaryOp::BitXor),
        ExvmOperatorKindV2::LogicAnd => Ok(BinaryOp::LogicAnd),
        ExvmOperatorKindV2::LogicOr => Ok(BinaryOp::LogicOr),
        ExvmOperatorKindV2::LogicXor => Ok(BinaryOp::LogicXor),
        _ => Err(ParseError {
            message: "unsupported EXVM binary operator".to_string(),
            span,
        }),
    }
}

fn portable_expr_error_to_parse_error(err: PortableExprError, fallback_span: Span) -> ParseError {
    ParseError {
        message: err.to_string(),
        span: err.span.unwrap_or(fallback_span),
    }
}

fn token_matches_kind(token_kind: &TokenKind, kind: ExvmTokenKindV2) -> bool {
    match token_kind {
        TokenKind::Number(_) => kind == ExvmTokenKindV2::Number,
        TokenKind::Identifier(_) => kind == ExvmTokenKindV2::Identifier,
        TokenKind::Dollar => kind == ExvmTokenKindV2::Dollar,
        TokenKind::OpenParen => kind == ExvmTokenKindV2::OpenParen,
        TokenKind::CloseParen => kind == ExvmTokenKindV2::CloseParen,
        TokenKind::Question => kind == ExvmTokenKindV2::Question,
        TokenKind::Colon => kind == ExvmTokenKindV2::Colon,
        TokenKind::OpenBrace => kind == ExvmTokenKindV2::OpenBrace,
        _ => false,
    }
}
