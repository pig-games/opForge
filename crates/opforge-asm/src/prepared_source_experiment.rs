// SPDX-License-Identifier: GPL-3.0-or-later

//! Isolated source-to-prepared-package experiment.
//!
//! The accepted source subset is label-only statements, `.byte`, `.word`, and
//! operandless package instructions. Preparation lowers names and expressions
//! to numeric IDs and flat records; it retains neither source text nor parser
//! ASTs. This is a laboratory path, not a replacement assembler entry point.
//! Only flat global symbols and unsigned byte/word results are accepted. Full
//! assembler scope, truncation-warning and signed-data behavior are not modeled.

use std::collections::HashMap;

use opcore::expr::parse_number;
use opcore::parser::{BinaryOp, Expr, LineAst, Parser, UnaryOp};
use types::hierarchy::ResolvedHierarchy;
use vm::prepared_encoding::{EncodingPreparation, PreparedEncodings, ProgramRef};
use vm::runtime_model_core::RuntimeModelCore;

mod packed;
pub use packed::{PackedLayout, PackedProbeReport, RecordAlignment, TokenWidth};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceExperimentError {
    pub line: u32,
    pub detail: String,
}

impl std::fmt::Display for SourceExperimentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}: {}", self.line, self.detail)
    }
}

impl std::error::Error for SourceExperimentError {}

type Result<T> = std::result::Result<T, SourceExperimentError>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct NameId(u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct SymbolId(u32);

#[derive(Clone, Copy, Debug)]
enum Operation {
    Byte,
    Word,
    Instruction(ProgramRef),
}

#[derive(Clone, Copy, Debug)]
struct ExprRange {
    start: u32,
    end: u32,
}

#[derive(Clone, Copy, Debug)]
enum ExprOp {
    Constant(i64),
    Symbol(SymbolId),
    Positive,
    Negative,
    Add,
    Subtract,
}

#[derive(Clone, Copy, Debug)]
struct PreparedLine {
    line: u32,
    label: Option<SymbolId>,
    statement: Option<NameId>,
    operands: ExprRange,
}

/// Prepared statements and a frozen, string-free package encoding directory.
pub struct PreparedSourceExperiment {
    lines: Box<[PreparedLine]>,
    operations: Box<[Operation]>,
    operands: Box<[ExprRange]>,
    expressions: Box<[ExprOp]>,
    symbol_count: usize,
    max_expression_ops: usize,
    encodings: PreparedEncodings,
    max_address: u32,
    little_endian: bool,
}

impl PreparedSourceExperiment {
    /// Parse and lower a complete source string. Any operandless statement is
    /// accepted if the package has its canonical `implied` encoding entry.
    pub fn prepare(
        source: &str,
        model: &RuntimeModelCore,
        resolved: &ResolvedHierarchy,
    ) -> Result<Self> {
        let properties = model
            .cpu_execution_properties(&resolved.cpu_id)
            .map_err(|error| source_error(1, format!("CPU properties: {error}")))?
            .ok_or_else(|| source_error(1, "package CPU data properties are unavailable"))?;
        let mut names = HashMap::<String, NameId>::new();
        let mut symbol_ids = HashMap::<String, SymbolId>::new();
        let mut definitions = HashMap::<SymbolId, u32>::new();
        let mut first_reference = HashMap::<SymbolId, u32>::new();
        let mut operations = Vec::<Option<Operation>>::new();
        let mut lines = Vec::new();
        let mut operands = Vec::new();
        let mut expressions = Vec::new();
        let mut bindings = EncodingPreparation::new(model);

        for (index, source_line) in source.lines().enumerate() {
            let line_num = checked_index(index + 1, u32::MAX)?;
            vm::prepared_event!("prepared.source.line_parse", 1);
            let ast = match Parser::from_line(source_line, line_num)
                .and_then(|mut parser| parser.parse_compat_mixed_line())
            {
                Ok(ast) => ast,
                Err(error) => return Err(source_error(line_num, error.message)),
            };
            match ast {
                LineAst::Empty => {}
                LineAst::Statement(statement) => {
                    let label = statement
                        .label
                        .as_ref()
                        .map(|label| intern_symbol(&mut symbol_ids, &label.name, line_num))
                        .transpose()?;
                    if let Some(symbol) = label {
                        if definitions.insert(symbol, line_num).is_some() {
                            return Err(source_error(
                                line_num,
                                format!("duplicate symbol id {}", symbol.0),
                            ));
                        }
                    }
                    let Some(name) = statement.mnemonic.as_deref() else {
                        lines.push(PreparedLine {
                            line: line_num,
                            label,
                            statement: None,
                            operands: empty_range(operands.len())?,
                        });
                        continue;
                    };

                    vm::prepared_event!("prepared.source.name_lookup", 1);
                    let name_id = intern_name(&mut names, &mut operations, name)?;
                    let operation = match name.to_ascii_uppercase().as_str() {
                        ".BYTE" => Operation::Byte,
                        ".WORD" => Operation::Word,
                        _ if name.starts_with('.') => {
                            return Err(source_error(
                                line_num,
                                format!("unsupported directive id {}", name_id.0),
                            ));
                        }
                        _ => {
                            if !statement.operands.is_empty() {
                                return Err(source_error(
                                    line_num,
                                    format!("instruction id {} has operands", name_id.0),
                                ));
                            }
                            let reference = if let Some(Operation::Instruction(reference)) =
                                operations[name_id.0 as usize]
                            {
                                reference
                            } else {
                                let reference = bindings
                                    .bind(resolved, name, "implied")
                                    .map_err(|error| {
                                        source_error(line_num, format!("package bind: {error}"))
                                    })?
                                    .ok_or_else(|| {
                                        source_error(
                                            line_num,
                                            format!(
                                                "no implied package entry for name id {}",
                                                name_id.0
                                            ),
                                        )
                                    })?;
                                reference
                            };
                            Operation::Instruction(reference)
                        }
                    };
                    operations[name_id.0 as usize] = Some(operation);
                    if matches!(operation, Operation::Byte | Operation::Word)
                        && statement.operands.is_empty()
                    {
                        return Err(source_error(
                            line_num,
                            "data directive requires an expression",
                        ));
                    }
                    let start = operands.len();
                    for expr in statement.operands {
                        let expr_start = expressions.len();
                        lower_expr(
                            expr,
                            &mut symbol_ids,
                            &mut first_reference,
                            &mut expressions,
                            line_num,
                        )?;
                        operands.push(ExprRange {
                            start: checked_index(expr_start, line_num)?,
                            end: checked_index(expressions.len(), line_num)?,
                        });
                    }
                    lines.push(PreparedLine {
                        line: line_num,
                        label,
                        statement: Some(name_id),
                        operands: ExprRange {
                            start: checked_index(start, line_num)?,
                            end: checked_index(operands.len(), line_num)?,
                        },
                    });
                }
                _ => {
                    return Err(source_error(
                        line_num,
                        "unsupported source construct (only statements and empty lines are accepted)",
                    ));
                }
            }
        }

        if let Some((symbol, line)) = first_reference
            .iter()
            .filter(|(symbol, _)| !definitions.contains_key(*symbol))
            .min_by_key(|(symbol, line)| (**line, symbol.0))
        {
            return Err(source_error(
                *line,
                format!("undefined symbol id {}", symbol.0),
            ));
        }
        let operations = operations
            .into_iter()
            .enumerate()
            .map(|(index, operation)| {
                operation.ok_or_else(|| source_error(1, format!("unbound statement id {index}")))
            })
            .collect::<Result<Vec<_>>>()?;
        let max_expression_ops = operands
            .iter()
            .map(|range| (range.end - range.start) as usize)
            .max()
            .unwrap_or(0);

        Ok(Self {
            lines: lines.into_boxed_slice(),
            operations: operations.into_boxed_slice(),
            operands: operands.into_boxed_slice(),
            expressions: expressions.into_boxed_slice(),
            symbol_count: symbol_ids.len(),
            max_expression_ops,
            encodings: bindings.finish(),
            max_address: properties.max_program_address,
            little_endian: properties.data_little_endian,
        })
    }

    /// Start a fresh layout and emission at `origin`. Forward references are
    /// resolved from this run's labels, so repeated origins recompute values.
    pub fn run(&self, origin: u32) -> Result<Vec<(u32, u8)>> {
        if origin > self.max_address {
            return Err(source_error(1, "origin exceeds package address limit"));
        }
        let mut pc = origin;
        let mut symbols = vec![None; self.symbol_count];
        let mut expression_stack = Vec::with_capacity(self.max_expression_ops);
        for line in self.lines.iter() {
            vm::prepared_event!("prepared.source.record_visit", 1);
            if let Some(symbol) = line.label {
                symbols[symbol.0 as usize] = Some(pc);
            }
            let Some(statement) = line.statement else {
                continue;
            };
            match self.operations[statement.0 as usize] {
                Operation::Byte => {
                    pc = advance(
                        pc,
                        self.operand_ranges(line).len(),
                        self.max_address,
                        line.line,
                    )?;
                }
                Operation::Word => {
                    let count = self
                        .operand_ranges(line)
                        .len()
                        .checked_mul(2)
                        .ok_or_else(|| source_error(line.line, "layout overflow"))?;
                    pc = advance(pc, count, self.max_address, line.line)?;
                }
                Operation::Instruction(reference) => {
                    let bytes = self.encode(reference, line.line)?;
                    pc = advance(pc, bytes.len(), self.max_address, line.line)?;
                }
            }
        }

        pc = origin;
        let mut output = Vec::new();
        for line in self.lines.iter() {
            vm::prepared_event!("prepared.source.record_visit", 1);
            if let Some(statement) = line.statement {
                match self.operations[statement.0 as usize] {
                    Operation::Byte => {
                        for range in self.operand_ranges(line) {
                            let value =
                                self.eval(*range, &symbols, line.line, &mut expression_stack)?;
                            if !(0..=u8::MAX as i64).contains(&value) {
                                return Err(source_error(line.line, "byte value out of range"));
                            }
                            output.push((pc, value as u8));
                            pc = advance(pc, 1, self.max_address, line.line)?;
                        }
                    }
                    Operation::Word => {
                        for range in self.operand_ranges(line) {
                            let value =
                                self.eval(*range, &symbols, line.line, &mut expression_stack)?;
                            if !(0..=u16::MAX as i64).contains(&value) {
                                return Err(source_error(line.line, "word value out of range"));
                            }
                            let [low, high] = (value as u16).to_le_bytes();
                            let bytes = if self.little_endian {
                                [low, high]
                            } else {
                                [high, low]
                            };
                            for byte in bytes {
                                output.push((pc, byte));
                                pc = advance(pc, 1, self.max_address, line.line)?;
                            }
                        }
                    }
                    Operation::Instruction(reference) => {
                        for byte in self.encode(reference, line.line)? {
                            output.push((pc, byte));
                            pc = advance(pc, 1, self.max_address, line.line)?;
                        }
                    }
                }
            }
        }
        Ok(output)
    }

    pub fn program_count(&self) -> usize {
        self.encodings.program_count()
    }

    /// Approximate owned payload size; excludes allocator overhead and temporary
    /// maps/strings used by preparation.
    pub fn retained_bytes(&self) -> usize {
        std::mem::size_of::<Self>()
            + self.retained_source_bytes()
            + self.retained_expression_bytes()
            + self
                .retained_encoding_bytes()
                .saturating_sub(std::mem::size_of::<PreparedEncodings>())
    }

    pub fn retained_source_bytes(&self) -> usize {
        std::mem::size_of_val(&*self.lines)
            + std::mem::size_of_val(&*self.operations)
            + std::mem::size_of_val(&*self.operands)
    }

    pub fn retained_expression_bytes(&self) -> usize {
        std::mem::size_of_val(&*self.expressions)
    }

    pub fn retained_encoding_bytes(&self) -> usize {
        self.encodings.retained_bytes()
    }

    /// Replay vectors' payload bound; excludes allocator metadata and output.
    pub fn replay_workspace_bytes(&self) -> usize {
        self.symbol_count * std::mem::size_of::<Option<u32>>()
            + self.max_expression_ops * std::mem::size_of::<i64>()
    }

    /// Pack this prepared program into explicit length-prefixed records, verify
    /// one full decode against S1, then time bounded checksum-only decodes.
    pub fn packed_probe(
        &self,
        layout: PackedLayout,
        decode_iterations: usize,
    ) -> Result<PackedProbeReport> {
        packed::probe(self, layout, decode_iterations).map_err(|detail| source_error(1, detail))
    }

    fn operand_ranges(&self, line: &PreparedLine) -> &[ExprRange] {
        &self.operands[line.operands.start as usize..line.operands.end as usize]
    }

    fn eval(
        &self,
        range: ExprRange,
        symbols: &[Option<u32>],
        line: u32,
        stack: &mut Vec<i64>,
    ) -> Result<i64> {
        stack.clear();
        for operation in &self.expressions[range.start as usize..range.end as usize] {
            vm::prepared_event!("prepared.source.expression_op", 1);
            match operation {
                ExprOp::Constant(value) => stack.push(*value),
                ExprOp::Symbol(id) => {
                    vm::prepared_event!("prepared.source.symbol_id_load", 1);
                    let value = symbols
                        .get(id.0 as usize)
                        .and_then(|value| *value)
                        .ok_or_else(|| {
                            source_error(line, format!("undefined symbol id {}", id.0))
                        })?;
                    stack.push(i64::from(value));
                }
                ExprOp::Positive => {
                    if stack.is_empty() {
                        return Err(source_error(line, "invalid expression stack"));
                    }
                }
                ExprOp::Negative => {
                    let value = stack
                        .pop()
                        .ok_or_else(|| source_error(line, "invalid expression stack"))?;
                    stack.push(
                        value
                            .checked_neg()
                            .ok_or_else(|| source_error(line, "expression overflow"))?,
                    );
                }
                ExprOp::Add | ExprOp::Subtract => {
                    let right = stack
                        .pop()
                        .ok_or_else(|| source_error(line, "invalid expression stack"))?;
                    let left = stack
                        .pop()
                        .ok_or_else(|| source_error(line, "invalid expression stack"))?;
                    let value = if matches!(operation, ExprOp::Add) {
                        left.checked_add(right)
                    } else {
                        left.checked_sub(right)
                    }
                    .ok_or_else(|| source_error(line, "expression overflow"))?;
                    stack.push(value);
                }
            }
        }
        if stack.len() != 1 {
            return Err(source_error(line, "invalid expression stack"));
        }
        Ok(stack[0])
    }

    fn encode(&self, reference: ProgramRef, line: u32) -> Result<Vec<u8>> {
        vm::prepared_event!("prepared.source.instruction_replay", 1);
        self.encodings
            .encode(reference, &[])
            .map_err(|error| source_error(line, format!("prepared instruction: {error}")))
    }
}

fn intern_name(
    names: &mut HashMap<String, NameId>,
    operations: &mut Vec<Option<Operation>>,
    name: &str,
) -> Result<NameId> {
    let key = name.to_ascii_uppercase();
    if let Some(id) = names.get(&key) {
        return Ok(*id);
    }
    let id = NameId(checked_index(names.len(), 1)?);
    names.insert(key, id);
    operations.push(None);
    Ok(id)
}

fn intern_symbol(
    symbols: &mut HashMap<String, SymbolId>,
    name: &str,
    line: u32,
) -> Result<SymbolId> {
    if !name
        .as_bytes()
        .first()
        .is_some_and(|byte| byte.is_ascii_alphabetic() || *byte == b'_')
        || !name
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
    {
        return Err(source_error(
            line,
            "only flat ASCII identifier symbols are supported",
        ));
    }
    vm::prepared_event!("prepared.source.symbol_name_lookup", 1);
    let key = name.to_ascii_uppercase();
    if let Some(id) = symbols.get(&key) {
        return Ok(*id);
    }
    let id = SymbolId(checked_index(symbols.len(), line)?);
    symbols.insert(key, id);
    Ok(id)
}

fn lower_expr(
    expr: Expr,
    symbols: &mut HashMap<String, SymbolId>,
    first_reference: &mut HashMap<SymbolId, u32>,
    output: &mut Vec<ExprOp>,
    line: u32,
) -> Result<()> {
    match expr {
        Expr::Number(number, _) => {
            output
                .push(ExprOp::Constant(parse_number(&number).ok_or_else(
                    || source_error(line, "invalid numeric literal"),
                )?))
        }
        Expr::Identifier(name, _) => {
            let symbol = intern_symbol(symbols, &name, line)?;
            first_reference.entry(symbol).or_insert(line);
            output.push(ExprOp::Symbol(symbol));
        }
        Expr::Unary {
            op: UnaryOp::Plus,
            expr,
            ..
        } => {
            lower_expr(*expr, symbols, first_reference, output, line)?;
            output.push(ExprOp::Positive);
        }
        Expr::Unary {
            op: UnaryOp::Minus,
            expr,
            ..
        } => {
            lower_expr(*expr, symbols, first_reference, output, line)?;
            output.push(ExprOp::Negative);
        }
        Expr::Binary {
            op: BinaryOp::Add,
            left,
            right,
            ..
        } => {
            lower_expr(*left, symbols, first_reference, output, line)?;
            lower_expr(*right, symbols, first_reference, output, line)?;
            output.push(ExprOp::Add);
        }
        Expr::Binary {
            op: BinaryOp::Subtract,
            left,
            right,
            ..
        } => {
            lower_expr(*left, symbols, first_reference, output, line)?;
            lower_expr(*right, symbols, first_reference, output, line)?;
            output.push(ExprOp::Subtract);
        }
        _ => return Err(source_error(line, "unsupported expression form")),
    }
    Ok(())
}

fn advance(pc: u32, bytes: usize, max_address: u32, line: u32) -> Result<u32> {
    let next = pc
        .checked_add(u32::try_from(bytes).map_err(|_| source_error(line, "layout overflow"))?)
        .ok_or_else(|| source_error(line, "layout overflow"))?;
    if bytes > 0 && next.saturating_sub(1) > max_address {
        return Err(source_error(line, "output exceeds package address limit"));
    }
    Ok(next)
}

fn checked_index(index: usize, line: u32) -> Result<u32> {
    u32::try_from(index).map_err(|_| source_error(line, "compact source exceeds 32-bit indices"))
}

fn empty_range(index: usize) -> Result<ExprRange> {
    let index = checked_index(index, 1)?;
    Ok(ExprRange {
        start: index,
        end: index,
    })
}

fn source_error(line: u32, detail: impl Into<String>) -> SourceExperimentError {
    SourceExperimentError {
        line,
        detail: detail.into(),
    }
}
