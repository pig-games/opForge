use opcore::expr_vm::{
    PortableExprDirectLeaf, PortableExprError, PortableExprProgram, PortableExprProgramBuilder,
};
use opcore::expression::expr_span;
use opcore::parser::{BinaryOp, Expr, ParseError, UnaryOp};
use opcore::tokenizer::{OperatorKind, Span, Token, TokenKind};
use package::{ExvmOpcode, ExvmOperatorKind, ExvmTokenKind, EXVM_OPCODE_VERSION};

use crate::vm_opcore::ExvmExecutionBudgets;

pub(crate) trait ExvmRuntimeBackend {
    type Value;
    type FinalOutput;

    fn build_identifier(&mut self, name: String, span: Span) -> Result<Self::Value, ParseError>;
    fn build_number(&mut self, text: String, span: Span) -> Result<Self::Value, ParseError>;
    fn build_string(&mut self, bytes: Vec<u8>, span: Span) -> Result<Self::Value, ParseError>;
    fn build_register(&mut self, name: String, span: Span) -> Result<Self::Value, ParseError>;
    fn build_placeholder(&mut self, span: Span) -> Result<Self::Value, ParseError>;
    fn build_call(
        &mut self,
        name: String,
        args: Vec<Self::Value>,
        span: Span,
    ) -> Result<Self::Value, ParseError>;
    fn build_current_address(&mut self, span: Span) -> Result<Self::Value, ParseError>;
    fn build_unary(
        &mut self,
        operator: ExvmOperatorKind,
        expr: Self::Value,
        span: Span,
    ) -> Result<Self::Value, ParseError>;
    fn build_binary(
        &mut self,
        operator: ExvmOperatorKind,
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

    fn build_string(&mut self, bytes: Vec<u8>, span: Span) -> Result<Self::Value, ParseError> {
        Ok(Expr::String(bytes, span))
    }

    fn build_register(&mut self, name: String, span: Span) -> Result<Self::Value, ParseError> {
        Ok(Expr::Register(name, span))
    }
    fn build_placeholder(&mut self, span: Span) -> Result<Self::Value, ParseError> {
        Ok(Expr::Placeholder(span))
    }
    fn build_call(
        &mut self,
        name: String,
        args: Vec<Self::Value>,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Ok(Expr::Call { name, args, span })
    }

    fn build_current_address(&mut self, span: Span) -> Result<Self::Value, ParseError> {
        Ok(Expr::Dollar(span))
    }

    fn build_unary(
        &mut self,
        operator: ExvmOperatorKind,
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
        operator: ExvmOperatorKind,
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
        operator: ExvmOperatorKind,
        expr: Box<PortableExprRuntimeValue>,
    },
    Binary {
        operator: ExvmOperatorKind,
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

    fn build_string(&mut self, bytes: Vec<u8>, span: Span) -> Result<Self::Value, ParseError> {
        Ok(Self::value(
            span,
            None,
            PortableExprRuntimeNode::Leaf(PortableExprDirectLeaf::StringLiteral(bytes)),
        ))
    }

    fn build_register(&mut self, name: String, span: Span) -> Result<Self::Value, ParseError> {
        self.build_identifier(name, span)
    }
    fn build_placeholder(&mut self, span: Span) -> Result<Self::Value, ParseError> {
        Err(ParseError {
            message: "Placeholder cannot be evaluated as scalar expression".to_owned(),
            span,
        })
    }
    fn build_call(
        &mut self,
        _name: String,
        _args: Vec<Self::Value>,
        span: Span,
    ) -> Result<Self::Value, ParseError> {
        Err(ParseError {
            message: "Call expression cannot be evaluated as scalar expression".to_owned(),
            span,
        })
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
        operator: ExvmOperatorKind,
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
        operator: ExvmOperatorKind,
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

    let mut runtime = ExvmRuntime {
        tokens,
        index: 0,
        end_span,
        end_token_text,
        steps: 0,
        expression_depth: 0,
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

struct ExvmRuntime<B: ExvmRuntimeBackend> {
    tokens: Vec<Token>,
    index: usize,
    end_span: Span,
    end_token_text: Option<String>,
    steps: usize,
    expression_depth: usize,
    loaded_token_text: Option<String>,
    last_peek_result: bool,
    build_spans: Vec<Span>,
    budgets: ExvmExecutionBudgets,
    backend: B,
}

impl<B: ExvmRuntimeBackend> ExvmRuntime<B> {
    fn execute_expression(&mut self, program: &[u8]) -> Result<B::Value, ParseError> {
        const MAX_EXPRESSION_DEPTH: usize = 64;
        if self.expression_depth >= MAX_EXPRESSION_DEPTH {
            return Err(ParseError {
                message: format!(
                    "EXVM expression recursion depth exceeded ({MAX_EXPRESSION_DEPTH})"
                ),
                span: self.current_span(),
            });
        }
        self.expression_depth += 1;
        let result = self.execute_from(program, 0);
        self.expression_depth -= 1;
        result
    }

    fn finish_value(&mut self, value: B::Value) -> Result<B::FinalOutput, ParseError> {
        self.backend.finish_value(value)
    }

    fn execute_from(&mut self, program: &[u8], mut pc: usize) -> Result<B::Value, ParseError> {
        let work =
            types::vm_work::ProgramRun::new("expression_parser", EXVM_OPCODE_VERSION, program);
        let mut output_stack = Vec::new();
        let mut call_stack = Vec::new();

        while pc < program.len() {
            self.consume_step()?;

            let opcode_pc = pc;
            let opcode_byte = program[pc];
            pc += 1;
            let opcode = ExvmOpcode::from_u8(opcode_byte).ok_or_else(|| ParseError {
                message: format!("invalid EXVM opcode 0x{opcode_byte:02X} at pc={opcode_pc}"),
                span: self.current_span(),
            })?;

            work.step(opcode_pc, opcode_byte);
            match opcode {
                ExvmOpcode::End => {
                    if !call_stack.is_empty() {
                        return Err(ParseError {
                            message: "EXVM program ended inside subroutine".to_string(),
                            span: self.current_span(),
                        });
                    }
                    return self.finish_output_stack(output_stack);
                }
                ExvmOpcode::Jump => {
                    pc = self.read_jump_target(program, &mut pc, opcode_pc)?;
                }
                ExvmOpcode::JumpIfTrue => {
                    let target = self.read_jump_target(program, &mut pc, opcode_pc)?;
                    if self.last_peek_result {
                        pc = target;
                    }
                }
                ExvmOpcode::Call => {
                    let target = self.read_jump_target(program, &mut pc, opcode_pc)?;
                    call_stack.push(pc);
                    pc = target;
                }
                ExvmOpcode::Return => {
                    let target = call_stack.pop().ok_or_else(|| ParseError {
                        message: "EXVM return without call".to_string(),
                        span: self.current_span(),
                    })?;
                    pc = target;
                }
                ExvmOpcode::PeekKind => {
                    let kind_byte = self.read_u8(program, &mut pc, opcode_pc)?;
                    let kind = ExvmTokenKind::from_u8(kind_byte).ok_or_else(|| ParseError {
                        message: format!(
                            "invalid EXVM token kind 0x{kind_byte:02X} at pc={opcode_pc}"
                        ),
                        span: self.current_span(),
                    })?;
                    self.last_peek_result = self.peek_matches(kind);
                }
                ExvmOpcode::PeekOperator => {
                    let operator = self.read_operator_kind(program, &mut pc, opcode_pc)?;
                    self.last_peek_result = self.peek_operator_matches(operator);
                }
                ExvmOpcode::Advance => self.advance()?,
                ExvmOpcode::ConsumeOperator => {
                    let operator = self.read_operator_kind(program, &mut pc, opcode_pc)?;
                    self.consume_operator(operator)?;
                }
                ExvmOpcode::ConsumeKind => {
                    let kind = self.read_token_kind(program, &mut pc, opcode_pc)?;
                    self.consume_kind(kind)?;
                }
                ExvmOpcode::LoadTokenText => {
                    let token = self
                        .current_token()
                        .ok_or_else(|| self.expected_leaf_error())?;
                    self.loaded_token_text = Some(token.to_source_text());
                }
                ExvmOpcode::BuildIdentifier => {
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
                ExvmOpcode::BuildNumber => {
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
                ExvmOpcode::BuildString => {
                    let (bytes, span) = match self.current_token() {
                        Some(Token {
                            kind: TokenKind::String(string),
                            span,
                        }) => (string.bytes.clone(), *span),
                        Some(token) => return Err(self.unexpected_token_error(token.span)),
                        None => return Err(self.expected_leaf_error()),
                    };
                    output_stack.push(self.backend.build_string(bytes, span)?);
                }
                ExvmOpcode::BuildRegister => {
                    let (name, span) = match self.current_token() {
                        Some(Token {
                            kind: TokenKind::Register(name),
                            span,
                        }) => (name.clone(), *span),
                        Some(token) => return Err(self.unexpected_token_error(token.span)),
                        None => return Err(self.expected_leaf_error()),
                    };
                    output_stack.push(self.backend.build_register(name, span)?);
                }
                ExvmOpcode::BuildPlaceholder => {
                    let span = match self.current_token() {
                        Some(Token {
                            kind: TokenKind::Question,
                            span,
                        }) => *span,
                        Some(token) => return Err(self.unexpected_token_error(token.span)),
                        None => return Err(self.expected_leaf_error()),
                    };
                    output_stack.push(self.backend.build_placeholder(span)?);
                }
                ExvmOpcode::ParseCall => {
                    output_stack.push(self.parse_call(program)?);
                }
                ExvmOpcode::BuildCurrentAddress => {
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
                ExvmOpcode::BuildUnary => {
                    let operator = self.read_operator_kind(program, &mut pc, opcode_pc)?;
                    let span = self.pop_build_span()?;
                    let expr = self.pop_output(&mut output_stack)?;
                    output_stack.push(self.backend.build_unary(operator, expr, span)?);
                }
                ExvmOpcode::BuildBinary => {
                    let operator = self.read_operator_kind(program, &mut pc, opcode_pc)?;
                    let span = self.pop_build_span()?;
                    let right = self.pop_output(&mut output_stack)?;
                    let left = self.pop_output(&mut output_stack)?;
                    output_stack.push(self.backend.build_binary(operator, left, right, span)?);
                }
                ExvmOpcode::BuildTernary => {
                    let span = self.pop_build_span()?;
                    let else_expr = self.pop_output(&mut output_stack)?;
                    let then_expr = self.pop_output(&mut output_stack)?;
                    let cond = self.pop_output(&mut output_stack)?;
                    output_stack.push(
                        self.backend
                            .build_ternary(cond, then_expr, else_expr, span)?,
                    );
                }
                ExvmOpcode::BuildRange => {
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
                ExvmOpcode::ParseGrouping => {
                    output_stack.push(self.parse_grouping(program)?);
                }
                ExvmOpcode::ParseList => {
                    output_stack.push(self.parse_list(program)?);
                }
                ExvmOpcode::ParseStructLiteralIfPresent => {
                    let expr = self.pop_output(&mut output_stack)?;
                    output_stack.push(self.parse_struct_literal_if_present(program, expr)?);
                }
                ExvmOpcode::ParsePostfixChain => {
                    let expr = self.pop_output(&mut output_stack)?;
                    output_stack.push(self.parse_postfix_chain(program, expr)?);
                }
                ExvmOpcode::EmitDiag => return Err(self.expected_leaf_error()),
                ExvmOpcode::Fail => {
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

    // Shared dot-prefixed function syntax. Names are opaque to EXVM; builtin
    // selection/evaluation belongs to the caller's value semantics.
    fn parse_call(&mut self, program: &[u8]) -> Result<B::Value, ParseError> {
        let dot_span = self.current_span();
        if !self.consume_raw_kind(TokenKind::Dot) {
            return Err(self.unexpected_token_error(dot_span));
        }
        let (name, _) = self.consume_identifier_like("Expected function name after '.'")?;
        if !self.consume_raw_kind(TokenKind::OpenParen) {
            return Err(ParseError {
                message: "Expected '(' after function name".to_owned(),
                span: self.current_span(),
            });
        }
        let mut args = Vec::new();
        if !self.consume_raw_kind(TokenKind::CloseParen) {
            args.push(self.execute_expression(program)?);
            while self.consume_raw_kind(TokenKind::Comma) {
                args.push(self.execute_expression(program)?);
            }
            if !self.consume_raw_kind(TokenKind::CloseParen) {
                return Err(ParseError {
                    message: "Missing ')' in function call".to_owned(),
                    span: self.current_span(),
                });
            }
        }
        let close_span = self.previous_span();
        self.backend.build_call(
            format!(".{name}"),
            args,
            Span {
                line: dot_span.line,
                col_start: dot_span.col_start,
                col_end: close_span.col_end,
            },
        )
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
    ) -> Result<ExvmOperatorKind, ParseError> {
        let operator_byte = self.read_u8(program, pc, opcode_pc)?;
        ExvmOperatorKind::from_u8(operator_byte).ok_or_else(|| ParseError {
            message: format!("invalid EXVM operator kind 0x{operator_byte:02X} at pc={opcode_pc}"),
            span: self.current_span(),
        })
    }

    fn read_token_kind(
        &self,
        program: &[u8],
        pc: &mut usize,
        opcode_pc: usize,
    ) -> Result<ExvmTokenKind, ParseError> {
        let kind_byte = self.read_u8(program, pc, opcode_pc)?;
        ExvmTokenKind::from_u8(kind_byte).ok_or_else(|| ParseError {
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

    fn peek_matches(&self, kind: ExvmTokenKind) -> bool {
        self.current_token()
            .is_some_and(|token| token_matches_kind(&token.kind, kind))
    }

    fn peek_operator_matches(&self, operator: ExvmOperatorKind) -> bool {
        match self.current_token().map(|token| &token.kind) {
            Some(TokenKind::Operator(current)) => *current == operator_kind(operator),
            _ => false,
        }
    }

    fn consume_operator(&mut self, operator: ExvmOperatorKind) -> Result<(), ParseError> {
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

    fn consume_kind(&mut self, kind: ExvmTokenKind) -> Result<(), ParseError> {
        let token = self.current_token().ok_or_else(|| match kind {
            ExvmTokenKind::Colon => self.missing_colon_error(),
            _ => self.expected_leaf_error(),
        })?;
        if !token_matches_kind(&token.kind, kind) {
            return Err(match kind {
                ExvmTokenKind::Colon => self.missing_colon_error(),
                _ => self.unexpected_token_error(token.span),
            });
        }
        if kind == ExvmTokenKind::Question {
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

fn operator_kind(operator: ExvmOperatorKind) -> OperatorKind {
    match operator {
        ExvmOperatorKind::Plus => OperatorKind::Plus,
        ExvmOperatorKind::Minus => OperatorKind::Minus,
        ExvmOperatorKind::Multiply => OperatorKind::Multiply,
        ExvmOperatorKind::Divide => OperatorKind::Divide,
        ExvmOperatorKind::Mod => OperatorKind::Mod,
        ExvmOperatorKind::Power => OperatorKind::Power,
        ExvmOperatorKind::BitNot => OperatorKind::BitNot,
        ExvmOperatorKind::LogicNot => OperatorKind::LogicNot,
        ExvmOperatorKind::Lt => OperatorKind::Lt,
        ExvmOperatorKind::Gt => OperatorKind::Gt,
        ExvmOperatorKind::Shl => OperatorKind::Shl,
        ExvmOperatorKind::Shr => OperatorKind::Shr,
        ExvmOperatorKind::Eq => OperatorKind::Eq,
        ExvmOperatorKind::Ne => OperatorKind::Ne,
        ExvmOperatorKind::Ge => OperatorKind::Ge,
        ExvmOperatorKind::Le => OperatorKind::Le,
        ExvmOperatorKind::BitAnd => OperatorKind::BitAnd,
        ExvmOperatorKind::BitOr => OperatorKind::BitOr,
        ExvmOperatorKind::BitXor => OperatorKind::BitXor,
        ExvmOperatorKind::LogicAnd => OperatorKind::LogicAnd,
        ExvmOperatorKind::LogicOr => OperatorKind::LogicOr,
        ExvmOperatorKind::LogicXor => OperatorKind::LogicXor,
        ExvmOperatorKind::Range => OperatorKind::Range,
        ExvmOperatorKind::RangeInclusive => OperatorKind::RangeInclusive,
    }
}

fn exvm_unary_operator(operator: ExvmOperatorKind, span: Span) -> Result<UnaryOp, ParseError> {
    match operator {
        ExvmOperatorKind::Plus => Ok(UnaryOp::Plus),
        ExvmOperatorKind::Minus => Ok(UnaryOp::Minus),
        ExvmOperatorKind::BitNot => Ok(UnaryOp::BitNot),
        ExvmOperatorKind::LogicNot => Ok(UnaryOp::LogicNot),
        ExvmOperatorKind::Lt => Ok(UnaryOp::Low),
        ExvmOperatorKind::Gt => Ok(UnaryOp::High),
        _ => Err(ParseError {
            message: "unsupported EXVM unary operator".to_string(),
            span,
        }),
    }
}

fn exvm_binary_operator(operator: ExvmOperatorKind, span: Span) -> Result<BinaryOp, ParseError> {
    match operator {
        ExvmOperatorKind::Plus => Ok(BinaryOp::Add),
        ExvmOperatorKind::Minus => Ok(BinaryOp::Subtract),
        ExvmOperatorKind::Multiply => Ok(BinaryOp::Multiply),
        ExvmOperatorKind::Divide => Ok(BinaryOp::Divide),
        ExvmOperatorKind::Mod => Ok(BinaryOp::Mod),
        ExvmOperatorKind::Power => Ok(BinaryOp::Power),
        ExvmOperatorKind::Shl => Ok(BinaryOp::Shl),
        ExvmOperatorKind::Shr => Ok(BinaryOp::Shr),
        ExvmOperatorKind::Eq => Ok(BinaryOp::Eq),
        ExvmOperatorKind::Ne => Ok(BinaryOp::Ne),
        ExvmOperatorKind::Ge => Ok(BinaryOp::Ge),
        ExvmOperatorKind::Gt => Ok(BinaryOp::Gt),
        ExvmOperatorKind::Le => Ok(BinaryOp::Le),
        ExvmOperatorKind::Lt => Ok(BinaryOp::Lt),
        ExvmOperatorKind::BitAnd => Ok(BinaryOp::BitAnd),
        ExvmOperatorKind::BitOr => Ok(BinaryOp::BitOr),
        ExvmOperatorKind::BitXor => Ok(BinaryOp::BitXor),
        ExvmOperatorKind::LogicAnd => Ok(BinaryOp::LogicAnd),
        ExvmOperatorKind::LogicOr => Ok(BinaryOp::LogicOr),
        ExvmOperatorKind::LogicXor => Ok(BinaryOp::LogicXor),
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

fn token_matches_kind(token_kind: &TokenKind, kind: ExvmTokenKind) -> bool {
    match token_kind {
        TokenKind::Number(_) => kind == ExvmTokenKind::Number,
        TokenKind::String(_) => kind == ExvmTokenKind::String,
        TokenKind::Register(_) => kind == ExvmTokenKind::Register,
        TokenKind::Dot => kind == ExvmTokenKind::Dot,
        TokenKind::Identifier(_) => kind == ExvmTokenKind::Identifier,
        TokenKind::Dollar => kind == ExvmTokenKind::Dollar,
        TokenKind::OpenParen => kind == ExvmTokenKind::OpenParen,
        TokenKind::CloseParen => kind == ExvmTokenKind::CloseParen,
        TokenKind::Question => kind == ExvmTokenKind::Question,
        TokenKind::Colon => kind == ExvmTokenKind::Colon,
        TokenKind::OpenBrace => kind == ExvmTokenKind::OpenBrace,
        _ => false,
    }
}

#[cfg(test)]
mod decoded_string_tests {
    use super::*;
    use opcore::tokenizer::StringLiteral;

    fn string(bytes: &[u8]) -> Token {
        Token {
            kind: TokenKind::String(StringLiteral {
                // Deliberately not valid source spelling: EXVM consumes decoded
                // tokenizer bytes and must never reinterpret this field.
                raw: "not source text".to_owned(),
                bytes: bytes.to_vec(),
            }),
            span: Span {
                line: 1,
                col_start: 1,
                col_end: 2,
            },
        }
    }

    #[test]
    fn canonical_exvm_builds_decoded_string_ast_without_text_reparse() {
        for bytes in [b"A".as_slice(), b"AB", b"\0", b"\n", b"", b"ABC"] {
            let tokens = vec![string(bytes)];
            let span = tokens[0].span;
            let ast = run_exvm_expression_parser_program(
                tokens,
                span,
                None,
                crate::vm_opcore::expression_parser_program(),
                ExvmExecutionBudgets::for_tokens(1),
            )
            .unwrap();
            assert_eq!(
                format!("{ast:?}"),
                format!("{:?}", Expr::String(bytes.to_vec(), span))
            );
        }
    }

    #[test]
    fn canonical_exvm_string_program_uses_shared_scalar_leaf_semantics() {
        for bytes in [b"A".as_slice(), b"AB", b"\0", b"\n", b"", b"ABC"] {
            let tokens = vec![string(bytes)];
            let span = tokens[0].span;
            let program = run_exvm_expression_parser_program_to_portable_program(
                tokens,
                span,
                None,
                crate::vm_opcore::expression_parser_program(),
                ExvmExecutionBudgets::for_tokens(1),
                package::EXPR_VM_OPCODE_VERSION_V2,
            )
            .unwrap();
            let mut reference =
                PortableExprProgramBuilder::for_scalar(package::EXPR_VM_OPCODE_VERSION_V2).unwrap();
            reference
                .emit_direct_leaf(&PortableExprDirectLeaf::StringLiteral(bytes.to_vec()))
                .unwrap();
            assert_eq!(program, reference.finish());
        }
    }
}

#[cfg(test)]
mod shared_primary_tests {
    use super::*;
    use opcore::tokenizer::{register_checker_from_fn, Tokenizer};

    fn tokens(source: &str) -> (Vec<Token>, Span) {
        let mut tokenizer = Tokenizer::with_register_checker(
            source,
            1,
            register_checker_from_fn(|name| name.eq_ignore_ascii_case("d0")),
        );
        let mut tokens = Vec::new();
        loop {
            let token = tokenizer.next_token().unwrap();
            if token.kind == TokenKind::End {
                return (tokens, token.span);
            }
            tokens.push(token);
        }
    }

    fn canonical(source: &str) -> Result<Expr, ParseError> {
        let (tokens, end) = tokens(source);
        let budget = ExvmExecutionBudgets::for_tokens(tokens.len());
        run_exvm_expression_parser_program(
            tokens,
            end,
            None,
            crate::vm_opcore::expression_parser_program(),
            budget,
        )
    }

    #[test]
    fn canonical_register_call_and_placeholder_match_existing_ast_semantics() {
        for source in [
            "d0",
            "d0+1",
            "d0.field",
            "d0{field:1}",
            "?",
            "1 ? ? : 3",
            ".len({1,2})",
            ".lo($1234)",
            ".hi($1234)",
            ".min(1,2)",
            ".custom()",
            ".custom(d0,?,.len({1,2}))",
            ".d0(1)",
            ".custom(1)[0]",
            ".custom(1).field",
        ] {
            let (tokens, end) = tokens(source);
            let reference =
                opcore::parser::Parser::parse_expr_from_tokens(tokens, end, None).unwrap();
            assert_eq!(
                format!("{:?}", canonical(source).unwrap()),
                format!("{reference:?}"),
                "{source}"
            );
        }
    }

    #[test]
    fn canonical_calls_reject_malformed_syntax_without_old_parser_fallback() {
        for source in [
            ".",
            ".len",
            ".len(",
            ".len(1",
            ".len(1,)",
            ".len(1,,2)",
            ".1(2)",
        ] {
            assert!(canonical(source).is_err(), "{source}");
        }
        for (source, message) in [
            (".len", "Expected '(' after function name"),
            (".len(1", "Missing ')' in function call"),
        ] {
            assert_eq!(canonical(source).unwrap_err().message, message);
        }
    }

    #[test]
    fn canonical_portable_register_leaf_uses_shared_symbol_lowering() {
        let (tokens, end) = tokens("d0");
        assert!(matches!(tokens[0].kind, TokenKind::Register(_)));
        let program = run_exvm_expression_parser_program_to_portable_program(
            tokens,
            end,
            None,
            crate::vm_opcore::expression_parser_program(),
            ExvmExecutionBudgets::for_tokens(1),
            package::EXPR_VM_OPCODE_VERSION_V2,
        )
        .unwrap();
        let mut reference =
            PortableExprProgramBuilder::for_scalar(package::EXPR_VM_OPCODE_VERSION_V2).unwrap();
        reference
            .emit_direct_leaf(&PortableExprDirectLeaf::SymbolName("d0".to_owned()))
            .unwrap();
        assert_eq!(program, reference.finish());
    }

    #[test]
    fn canonical_portable_calls_and_placeholders_keep_scalar_context_errors() {
        for (source, message) in [
            (
                ".len({1,2})",
                "Call expression cannot be evaluated as scalar expression",
            ),
            ("?", "Placeholder cannot be evaluated as scalar expression"),
        ] {
            let (tokens, end) = tokens(source);
            let budget = ExvmExecutionBudgets::for_tokens(tokens.len());
            let error = run_exvm_expression_parser_program_to_portable_program(
                tokens,
                end,
                None,
                crate::vm_opcore::expression_parser_program(),
                budget,
                package::EXPR_VM_OPCODE_VERSION_V2,
            )
            .unwrap_err();
            assert_eq!(error.message, message);
            assert_eq!(error.span.col_start, 1);
        }
    }
}
