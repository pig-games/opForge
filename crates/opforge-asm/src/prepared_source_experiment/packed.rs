// SPDX-License-Identifier: GPL-3.0-or-later

//! Compact raw-record layout and decoder microbenchmark for the S1 experiment.

use std::time::{Duration, Instant};

use super::{ExprOp, Operation, PreparedSourceExperiment};

const MAGIC: &[u8; 4] = b"PSP1";
const MAX_ITERATIONS: usize = 10_000;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// Width of instruction identity only; expression tags are fixed byte codes.
pub enum TokenWidth {
    Byte,
    Word,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RecordAlignment {
    BytePacked,
    WordAligned,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PackedLayout {
    pub token_width: TokenWidth,
    pub alignment: RecordAlignment,
}

impl PackedLayout {
    pub const BYTE_PACKED: Self = Self {
        token_width: TokenWidth::Byte,
        alignment: RecordAlignment::BytePacked,
    };
    pub const NATIVE_WORD_ALIGNED: Self = Self {
        token_width: TokenWidth::Word,
        alignment: RecordAlignment::WordAligned,
    };
}

#[derive(Clone, Debug)]
pub struct PackedProbeReport {
    pub layout: PackedLayout,
    pub packed_bytes: usize,
    pub line_records: u32,
    /// Number of inline operand-expression records.
    pub expression_records: u32,
    pub checksum: u64,
    pub pack_time: Duration,
    pub validation_time: Duration,
    pub decode_time: Duration,
    pub decode_iterations: usize,
}

struct Block(Box<[u8]>);

pub(super) fn probe(
    source: &PreparedSourceExperiment,
    layout: PackedLayout,
    iterations: usize,
) -> Result<PackedProbeReport, String> {
    if iterations == 0 || iterations > MAX_ITERATIONS {
        return Err(format!(
            "decode iterations must be between 1 and {MAX_ITERATIONS}"
        ));
    }
    let started = Instant::now();
    let block = pack(source, layout)?;
    let pack_time = started.elapsed();
    let started = Instant::now();
    let (checksum, lines, expressions) = decode(&block.0, layout, Some(source))?;
    let validation_time = started.elapsed();
    let started = Instant::now();
    for _ in 0..iterations {
        let result = decode(&block.0, layout, None)?;
        if result != (checksum, lines, expressions) {
            return Err("packed decoder checksum changed".into());
        }
        std::hint::black_box(result.0);
    }
    Ok(PackedProbeReport {
        layout,
        packed_bytes: block.0.len(),
        line_records: lines,
        expression_records: expressions,
        checksum,
        pack_time,
        validation_time,
        decode_time: started.elapsed(),
        decode_iterations: iterations,
    })
}

// Format: 12-byte header; each line is u16 byte length, u16 source line,
// flags (label/statement), then optional label u16. Statements have kind;
// instructions additionally carry selected-width NameId; all statements have
// u16 operand count and inline u16-length postfix expressions. Package program
// references/encoding directory are external and excluded from this byte count.
fn pack(source: &PreparedSourceExperiment, layout: PackedLayout) -> Result<Block, String> {
    let line_count = u16::try_from(source.lines.len()).map_err(|_| "line count exceeds u16")?;
    let symbol_count =
        u16::try_from(source.symbol_count).map_err(|_| "symbol ID space exceeds u16")?;
    let mut out = Vec::new();
    out.extend_from_slice(MAGIC);
    out.extend_from_slice(&line_count.to_be_bytes());
    out.extend_from_slice(&symbol_count.to_be_bytes());
    out.push(width_code(layout.token_width));
    out.push(align_code(layout.alignment));
    out.extend_from_slice(&[0, 0]);
    for line in source.lines.iter() {
        align(&mut out, layout);
        let length_at = out.len();
        out.extend_from_slice(&[0, 0]);
        put_u16(&mut out, line.line, layout)?;
        let flags = u8::from(line.label.is_some()) | (u8::from(line.statement.is_some()) << 1);
        out.push(flags);
        if let Some(id) = line.label {
            put_u16(&mut out, id.0, layout)?;
        }
        if let Some(name) = line.statement {
            match source.operations[name.0 as usize] {
                Operation::Byte => out.push(1),
                Operation::Word => out.push(2),
                Operation::Instruction(_) => {
                    out.push(3);
                    put_token(&mut out, name.0, layout)?;
                }
            }
            let ranges = source.operand_ranges(line);
            put_u16(
                &mut out,
                u32::try_from(ranges.len()).map_err(|_| "operand count exceeds u32")?,
                layout,
            )?;
            for range in ranges {
                pack_expression(&mut out, source, *range, layout)?;
            }
        }
        let length =
            u16::try_from(out.len() - length_at - 2).map_err(|_| "line record exceeds u16")?;
        out[length_at..length_at + 2].copy_from_slice(&length.to_be_bytes());
    }
    Ok(Block(out.into_boxed_slice()))
}

fn pack_expression(
    out: &mut Vec<u8>,
    source: &PreparedSourceExperiment,
    range: super::ExprRange,
    layout: PackedLayout,
) -> Result<(), String> {
    align(out, layout);
    let length_at = out.len();
    out.extend_from_slice(&[0, 0]);
    for op in &source.expressions[range.start as usize..range.end as usize] {
        let (tag, payload) = match op {
            ExprOp::Constant(value) => (1, Some(*value)),
            ExprOp::Symbol(id) => {
                out.push(2);
                put_u16(out, id.0, layout)?;
                continue;
            }
            ExprOp::Positive => (3, None),
            ExprOp::Negative => (4, None),
            ExprOp::Add => (5, None),
            ExprOp::Subtract => (6, None),
        };
        out.push(tag);
        if let Some(value) = payload {
            let width = if i8::try_from(value).is_ok() {
                1
            } else if i16::try_from(value).is_ok() {
                2
            } else if i32::try_from(value).is_ok() {
                4
            } else {
                8
            };
            out.push(width);
            align(out, layout);
            match width {
                1 => out.push(value as i8 as u8),
                2 => out.extend_from_slice(&(value as i16).to_be_bytes()),
                4 => out.extend_from_slice(&(value as i32).to_be_bytes()),
                _ => out.extend_from_slice(&value.to_be_bytes()),
            }
        }
    }
    let length =
        u16::try_from(out.len() - length_at - 2).map_err(|_| "expression record exceeds u16")?;
    out[length_at..length_at + 2].copy_from_slice(&length.to_be_bytes());
    Ok(())
}

fn decode(
    bytes: &[u8],
    layout: PackedLayout,
    expected: Option<&PreparedSourceExperiment>,
) -> Result<(u64, u32, u32), String> {
    let mut c = Cursor::new(bytes, layout);
    if c.take(4)? != MAGIC {
        return Err("invalid packed source magic".into());
    }
    let lines = c.u16()? as usize;
    let symbols = c.u16()? as usize;
    if c.byte()? != width_code(layout.token_width) || c.byte()? != align_code(layout.alignment) {
        return Err("packed layout header mismatch".into());
    }
    if c.take(2)? != [0, 0] {
        return Err("invalid packed header reserved field".into());
    }
    if let Some(s) = expected {
        if lines != s.lines.len() || symbols != s.symbol_count {
            return Err("packed header differs from S1".into());
        }
    }
    let mut hash = 0xcbf29ce484222325u64;
    let mut expression_records = 0u32;
    for i in 0..lines {
        let mut r = c.record()?;
        let line = r.u16()?;
        let flags = r.byte()?;
        if flags & !3 != 0 {
            return Err("unknown packed line flags".into());
        }
        let label = if flags & 1 != 0 { Some(r.u16()?) } else { None };
        let statement = flags & 2 != 0;
        hash_num(&mut hash, u64::from(line));
        hash_num(&mut hash, u64::from(flags));
        hash_num(&mut hash, u64::from(label.unwrap_or(u16::MAX)));
        if let Some(s) = expected {
            let exp = s.lines[i];
            if line as u32 != exp.line
                || label.map(u32::from) != exp.label.map(|x| x.0)
                || statement != exp.statement.is_some()
            {
                return Err(format!("line record {i} differs from S1"));
            }
        }
        if statement {
            let kind = r.byte()?;
            let (kind_expected, name_expected) = if let Some(s) = expected {
                let name = s.lines[i].statement.expect("statement flag checked");
                (
                    match s.operations[name.0 as usize] {
                        Operation::Byte => 1,
                        Operation::Word => 2,
                        Operation::Instruction(_) => 3,
                    },
                    Some(name.0),
                )
            } else {
                (0, None)
            };
            let name = if kind == 3 { Some(r.token()?) } else { None };
            if !matches!(kind, 1..=3) {
                return Err("unknown packed statement kind".into());
            }
            if expected.is_some()
                && (kind != kind_expected || (kind == 3 && name.map(u32::from) != name_expected))
            {
                return Err(format!("statement in line record {i} differs from S1"));
            }
            hash_num(&mut hash, u64::from(kind));
            hash_num(&mut hash, u64::from(name.unwrap_or(u16::MAX)));
            let count = r.u16()? as usize;
            let ranges = expected.map(|s| s.operand_ranges(&s.lines[i]));
            if ranges.is_some_and(|x| x.len() != count) {
                return Err(format!("operand count in line record {i} differs from S1"));
            }
            for j in 0..count {
                let expr = r.record()?;
                decode_expression(expr, ranges.map(|x| &x[j]), expected, &mut hash)?;
                expression_records += 1;
            }
        } else if flags & 1 == 0 {
            return Err("empty source records are not packed".into());
        }
        r.finish()?;
    }
    c.finish()?;
    Ok((hash, lines as u32, expression_records))
}

fn decode_expression(
    mut c: Cursor<'_>,
    range: Option<&super::ExprRange>,
    source: Option<&PreparedSourceExperiment>,
    hash: &mut u64,
) -> Result<(), String> {
    let expected_ops = range
        .map(|r| &source.expect("range has source").expressions[r.start as usize..r.end as usize]);
    let mut index = 0;
    while !c.done() {
        let tag = c.byte()?;
        let actual = match tag {
            1 => ExprOp::Constant(c.constant()?),
            2 => ExprOp::Symbol(super::SymbolId(c.u16()? as u32)),
            3 => ExprOp::Positive,
            4 => ExprOp::Negative,
            5 => ExprOp::Add,
            6 => ExprOp::Subtract,
            _ => return Err(format!("unknown expression token {tag}")),
        };
        if let Some(ops) = expected_ops {
            if ops.get(index).is_none_or(|v| !same_op(*v, actual)) {
                return Err(format!("expression token {index} differs from S1"));
            }
        }
        hash_num(hash, u64::from(tag));
        match actual {
            ExprOp::Constant(v) => hash_num(hash, v as u64),
            ExprOp::Symbol(s) => hash_num(hash, u64::from(s.0)),
            _ => hash_num(hash, 0),
        }
        index += 1;
    }
    if expected_ops.is_some_and(|ops| ops.len() != index) {
        return Err("expression token count differs from S1".into());
    }
    Ok(())
}

fn same_op(a: ExprOp, b: ExprOp) -> bool {
    match (a, b) {
        (ExprOp::Constant(x), ExprOp::Constant(y)) => x == y,
        (ExprOp::Symbol(x), ExprOp::Symbol(y)) => x.0 == y.0,
        (ExprOp::Positive, ExprOp::Positive)
        | (ExprOp::Negative, ExprOp::Negative)
        | (ExprOp::Add, ExprOp::Add)
        | (ExprOp::Subtract, ExprOp::Subtract) => true,
        _ => false,
    }
}
fn hash_num(h: &mut u64, n: u64) {
    for b in n.to_be_bytes() {
        *h ^= u64::from(b);
        *h = h.wrapping_mul(0x100000001b3);
    }
}
fn align(out: &mut Vec<u8>, layout: PackedLayout) {
    if layout.alignment == RecordAlignment::WordAligned && !out.len().is_multiple_of(2) {
        out.push(0);
    }
}
fn align_cursor(c: &mut Cursor<'_>) -> Result<(), String> {
    if c.layout.alignment == RecordAlignment::WordAligned
        && !c.pos.is_multiple_of(2)
        && c.byte()? != 0
    {
        return Err("nonzero alignment padding".into());
    }
    Ok(())
}
fn put_u16(out: &mut Vec<u8>, n: u32, layout: PackedLayout) -> Result<(), String> {
    let n = u16::try_from(n).map_err(|_| "field exceeds u16")?;
    align(out, layout);
    out.extend_from_slice(&n.to_be_bytes());
    Ok(())
}
fn put_token(out: &mut Vec<u8>, n: u32, layout: PackedLayout) -> Result<(), String> {
    match layout.token_width {
        TokenWidth::Byte => out.push(u8::try_from(n).map_err(|_| "token ID exceeds byte width")?),
        TokenWidth::Word => put_u16(out, n, layout)?,
    }
    Ok(())
}

fn width_code(w: TokenWidth) -> u8 {
    match w {
        TokenWidth::Byte => 1,
        TokenWidth::Word => 2,
    }
}
fn align_code(a: RecordAlignment) -> u8 {
    match a {
        RecordAlignment::BytePacked => 0,
        RecordAlignment::WordAligned => 1,
    }
}

struct Cursor<'a> {
    bytes: &'a [u8],
    pos: usize,
    layout: PackedLayout,
}
impl<'a> Cursor<'a> {
    fn new(bytes: &'a [u8], layout: PackedLayout) -> Self {
        Self {
            bytes,
            pos: 0,
            layout,
        }
    }
    fn take(&mut self, n: usize) -> Result<&'a [u8], String> {
        let end = self.pos.checked_add(n).ok_or("cursor overflow")?;
        let b = self
            .bytes
            .get(self.pos..end)
            .ok_or("truncated packed block")?;
        self.pos = end;
        Ok(b)
    }
    fn byte(&mut self) -> Result<u8, String> {
        Ok(self.take(1)?[0])
    }
    fn u16(&mut self) -> Result<u16, String> {
        align_cursor(self)?;
        Ok(u16::from_be_bytes(
            self.take(2)?.try_into().expect("length"),
        ))
    }
    fn token(&mut self) -> Result<u16, String> {
        match self.layout.token_width {
            TokenWidth::Byte => Ok(u16::from(self.byte()?)),
            TokenWidth::Word => self.u16(),
        }
    }
    fn constant(&mut self) -> Result<i64, String> {
        let width = self.byte()? as usize;
        if !matches!(width, 1 | 2 | 4 | 8) {
            return Err("invalid constant width".into());
        }
        align_cursor(self)?;
        let bytes = self.take(width)?;
        let mut full = [if bytes[0] & 0x80 != 0 { 0xff } else { 0 }; 8];
        full[8 - width..].copy_from_slice(bytes);
        Ok(i64::from_be_bytes(full))
    }
    fn record(&mut self) -> Result<Cursor<'a>, String> {
        align_cursor(self)?;
        let length = usize::from(self.u16()?);
        let data = self.take(length)?;
        Ok(Cursor::new(data, self.layout))
    }
    fn done(&self) -> bool {
        self.pos == self.bytes.len()
    }
    fn finish(&self) -> Result<(), String> {
        if self.done() {
            Ok(())
        } else {
            Err("trailing bytes in packed record".into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use families::register_mos6502_family_stack;
    use registry::registry::ModuleRegistry;
    use vm::runtime_model_core::RuntimeModelCore;

    #[test]
    fn compact_records_decode_same_source_in_all_layouts() {
        let mut registry = ModuleRegistry::new();
        register_mos6502_family_stack(&mut registry);
        let model = RuntimeModelCore::from_registry(&registry).expect("model");
        let resolved = model.resolve_pipeline("m6502", None).unwrap();
        let source = String::from("start:\n NOP\n.byte end-start\n.word end+1\nend:\n");
        let mut prepared = PreparedSourceExperiment::prepare(&source, &model, &resolved).unwrap();
        let layouts = [
            PackedLayout::BYTE_PACKED,
            PackedLayout::NATIVE_WORD_ALIGNED,
            PackedLayout {
                token_width: TokenWidth::Byte,
                alignment: RecordAlignment::WordAligned,
            },
            PackedLayout {
                token_width: TokenWidth::Word,
                alignment: RecordAlignment::BytePacked,
            },
        ];
        let reports = layouts.map(|layout| prepared.packed_probe(layout, 8).unwrap());
        assert!(reports.iter().all(|r| r.checksum == reports[0].checksum));
        assert!(reports
            .iter()
            .all(|r| r.line_records == 5 && r.expression_records == 2));
        assert!(reports.iter().all(|r| r.packed_bytes > 12));
        for layout in layouts {
            let block = pack(&prepared, layout).unwrap();
            for end in 0..block.0.len() {
                assert!(
                    decode(&block.0[..end], layout, None).is_err(),
                    "truncation {end}"
                );
            }
            for value in [
                i64::MIN,
                i64::MAX,
                -32769,
                -32768,
                -129,
                -128,
                127,
                128,
                32767,
                32768,
            ] {
                for op in prepared.expressions.iter_mut() {
                    if let ExprOp::Constant(stored) = op {
                        *stored = value;
                    }
                }
                prepared.packed_probe(layout, 1).unwrap();
            }
        }
        assert!(put_token(&mut Vec::new(), 256, PackedLayout::BYTE_PACKED).is_err());
        assert!(put_token(&mut Vec::new(), 65536, PackedLayout::NATIVE_WORD_ALIGNED).is_err());
        assert!(prepared.packed_probe(PackedLayout::BYTE_PACKED, 0).is_err());
        assert!(prepared
            .packed_probe(PackedLayout::BYTE_PACKED, MAX_ITERATIONS + 1)
            .is_err());
    }
}
