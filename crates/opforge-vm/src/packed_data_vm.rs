// SPDX-License-Identifier: GPL-3.0-or-later
//! VM-owned built-in data operand grammar over immutable compact source.
use crate::macro_descriptor_vm::DescriptorError;
use package::package::{PARSER_VM_MACRO_VERSION, PARSER_VM_PACKED_DATA_ENTRY};

pub const RESULT_KIND: u16 = 18;
pub const UNIT_OFFSET: usize = 4;
pub const UNIT_BYTES: usize = 8;
pub const VALUES_OFFSET: usize = 12;
pub const VALUES_BYTES: usize = 16;
pub const FIXED_WIDTH: usize = 20;
pub const PREFIX_BYTES: usize = 24;
fn error(status: u32, offset: usize, message: &'static str) -> DescriptorError {
    DescriptorError {
        status,
        offset: offset as u32,
        message,
    }
}

/// Parse spans only; evaluation and data emission belong to callers. No partial
/// result is published when token validation, capacity, or the budget fails.
pub fn execute(
    entry: u16,
    version: u16,
    program: &[u8],
    source: &[u8],
    capacity: usize,
    mut steps: usize,
) -> Result<Vec<[u8; 32]>, DescriptorError> {
    if entry != PARSER_VM_PACKED_DATA_ENTRY || version != PARSER_VM_MACRO_VERSION {
        return Err(error(4, 0, "Invalid packed data entry or version"));
    }
    if program.len() != 16
        || program[0] != 0x93
        || program[1] & !1 != 0
        || program[2] != 0x94
        || program[5] != 0x95
        || program[14..] != [0x83, 0]
        || u16::from_be_bytes([program[12], program[13]]) == 0
    {
        return Err(error(6, 0, "Invalid packed data program"));
    }
    let mut tick = |offset| {
        steps = steps
            .checked_sub(1)
            .ok_or(error(12, offset, "Step budget exceeded"))?;
        Ok::<_, DescriptorError>(())
    };
    if source.len() < 4
        || source.len() > 256
        || usize::from(source[0]) + 1 != source.len()
        || source[1] & !63 != 0
    {
        return Err(error(4, 0, "Invalid packed record extent or flags"));
    }
    tick(0)?;
    if source[1] & 24 != 0 || source.len() == 4 {
        return Ok(Vec::new());
    }
    let mut head = 4;
    let mut prefix = 0;
    if program[1] & 1 != 0 && source.len() >= 9 && source[4] <= 1 && source[8] == 5 {
        head = 9;
        prefix = 5;
    }
    tick(head)?;
    if source.len() - head < 5
        || source[head] != 7
        || source[head + 1] > 1
        || source[head + 4] != 0
        || source[head + 2..head + 4] != program[3..5]
    {
        return Ok(Vec::new());
    }
    if prefix != 0 && source[7] != 0 {
        return Err(error(5, 7, "Qualified data label"));
    }
    let start = head + 5;
    let mut p = start;
    let mut depth = 0usize;
    let mut comma = None;
    while p < source.len() {
        tick(p)?;
        let kind = source[p];
        let len = match kind {
            0 | 1 => 4,
            2 => 5,
            3 if comma.is_some() => {
                let len = *source
                    .get(p + 1)
                    .ok_or(error(5, p, "Truncated data string"))?;
                2 + usize::from(len)
            }
            129 => {
                let len = usize::from(*source.get(p + 1).ok_or(error(
                    5,
                    p,
                    "Truncated compiled data expression",
                ))?);
                if len == 0 {
                    return Err(error(5, p, "Empty compiled data expression"));
                }
                2 + len
            }
            4 if depth == 0 => {
                if comma.is_none() {
                    if p == start {
                        return Err(error(5, p, "Empty data unit"));
                    }
                    comma = Some(p);
                }
                1
            }
            14 => {
                depth += 1;
                if depth > 16 {
                    return Err(error(5, p, "Data delimiter depth exceeded"));
                }
                1
            }
            15 => {
                depth = depth
                    .checked_sub(1)
                    .ok_or(error(5, p, "Unmatched data delimiter"))?;
                1
            }
            6 | 7 | 9 | 16..=39 => 1,
            _ => return Err(error(5, p, "Invalid data token")),
        };
        if p + len > source.len() {
            return Err(error(5, p, "Truncated data token"));
        }
        p += len;
    }
    if depth != 0 {
        return Err(error(5, p, "Unclosed data delimiter"));
    }
    let comma = comma.ok_or(error(5, p, "Data unit requires comma"))?;
    let values = comma + 1;
    if values == source.len() {
        return Err(error(5, values, "Empty data values"));
    }
    let mut width = 0u32;
    if comma - start == 4 && source[start] <= 1 && source[start + 3] == 0 {
        let id = &source[start + 1..start + 3];
        if id == &program[6..8] {
            width = 1;
        } else if id == &program[8..10] {
            width = u32::from(u16::from_be_bytes([program[12], program[13]]));
        } else if id == &program[10..12] {
            width = 4;
        }
    }
    tick(p)?;
    if capacity < 32 {
        return Err(error(7, p, "Data result capacity exceeded"));
    }
    tick(p)?;
    let mut row = [0; 32];
    row[..2].copy_from_slice(&RESULT_KIND.to_be_bytes());
    for (at, value) in [
        (UNIT_OFFSET, start as u32),
        (UNIT_BYTES, (comma - start) as u32),
        (VALUES_OFFSET, values as u32),
        (VALUES_BYTES, (source.len() - values) as u32),
        (FIXED_WIDTH, width),
        (PREFIX_BYTES, prefix),
    ] {
        row[at..at + 4].copy_from_slice(&value.to_be_bytes());
    }
    Ok(vec![row])
}

#[cfg(test)]
mod tests {
    use super::*;
    use package::package::packed_data_program;
    fn record(label: bool, operand: &[u8]) -> Vec<u8> {
        let mut out = vec![0, 1, 0, 1];
        if label {
            out.extend([0, 0, 90, 0, 5]);
        }
        out.extend([7, 0, 0x12, 0x34, 0]);
        out.extend(operand);
        out[0] = (out.len() - 1) as u8;
        out
    }
    fn run(
        source: &[u8],
        word: u16,
        capacity: usize,
        budget: usize,
    ) -> Result<Vec<[u8; 32]>, DescriptorError> {
        execute(
            6,
            2,
            &packed_data_program(0x1234, 1, 2, 3, word),
            source,
            capacity,
            budget,
        )
    }
    fn field(row: &[u8; 32], at: usize) -> u32 {
        u32::from_be_bytes(row[at..at + 4].try_into().unwrap())
    }
    #[test]
    fn fixed_and_expression_units() {
        for word in [1, 2] {
            for label in [false, true] {
                for (id, width) in [(1, 1), (2, word), (3, 4), (4, 0)] {
                    let rows = run(
                        &record(label, &[0, 0, id, 0, 4, 2, 0, 0, 0, 9]),
                        word,
                        32,
                        20,
                    )
                    .unwrap();
                    assert_eq!(field(&rows[0], FIXED_WIDTH), u32::from(width));
                    assert_eq!(field(&rows[0], UNIT_BYTES), 4);
                    assert_eq!(field(&rows[0], VALUES_BYTES), 5);
                    assert_eq!(field(&rows[0], PREFIX_BYTES), if label { 5 } else { 0 });
                }
            }
        }
        let rows = run(
            &record(
                false,
                &[14, 0, 0, 2, 0, 16, 2, 0, 0, 0, 1, 15, 4, 2, 0, 0, 0, 9],
            ),
            2,
            32,
            20,
        )
        .unwrap();
        assert_eq!(field(&rows[0], FIXED_WIDTH), 0);
        assert_eq!(field(&rows[0], UNIT_BYTES), 12);
    }
    #[test]
    fn compiled_expression_spans_and_arbitrary_word_width() {
        let source = record(false, &[129, 2, 0x04, 0, 4, 129, 3, 0x04, 4, 0]);
        let rows = run(&source, 3, 32, 10).unwrap();
        assert_eq!(field(&rows[0], UNIT_BYTES), 4);
        assert_eq!(field(&rows[0], VALUES_BYTES), 5);
        assert_eq!(field(&rows[0], FIXED_WIDTH), 0);
        for bad in [
            &[129, 0, 4, 2, 0, 0, 0, 1][..],
            &[129, 20, 4, 2, 0, 0, 0, 1][..],
        ] {
            assert_eq!(run(&record(false, bad), 2, 32, 10).unwrap_err().status, 5);
        }
        let named = record(false, &[0, 0, 2, 0, 4, 129, 1, 0]);
        assert_eq!(field(&run(&named, 3, 32, 10).unwrap()[0], FIXED_WIDTH), 3);
    }
    #[test]
    fn malformed_and_atomic_limits() {
        for operand in [
            &[][..],
            &[4, 2, 0, 0, 0, 1],
            &[2, 0, 0, 0, 1],
            &[2, 0, 0, 0, 1, 4],
            &[14, 2, 0, 0, 0, 1, 4, 2, 0, 0, 0, 1],
            &[2, 0, 0, 0, 1, 4, 10],
            &[2, 0, 0, 0, 1, 4, 2, 0],
        ] {
            assert_eq!(
                run(&record(false, operand), 2, 32, 30).unwrap_err().status,
                5
            );
        }
        let source = record(false, &[0, 0, 2, 0, 4, 2, 0, 0, 0, 1]);
        assert_eq!(run(&source, 2, 31, 7).unwrap_err().status, 7);
        assert_eq!(run(&source, 2, 32, 6).unwrap_err().status, 12);
        assert_eq!(run(&source, 2, 32, 7).unwrap().len(), 1);
        let mut oversized = source.clone();
        oversized.resize(257, 0);
        assert_eq!(run(&oversized, 2, 32, 300).unwrap_err().status, 4);
        let mut program = packed_data_program(0x1234, 1, 2, 3, 2);
        program[13] = 0;
        assert_eq!(
            execute(6, 2, &program, &source, 32, 20).unwrap_err().status,
            6
        );
    }
    #[test]
    fn record_boundary_and_balanced_delimiter_limit() {
        let mut operand = vec![0, 0, 2, 0, 4, 3, 240];
        operand.extend([b'x'; 240]);
        let source = record(false, &operand);
        assert_eq!(source.len(), 256);
        assert_eq!(
            field(&run(&source, 2, 32, 20).unwrap()[0], VALUES_BYTES),
            242
        );
        for depth in [16, 17] {
            let mut operand = vec![14; depth];
            operand.extend([2, 0, 0, 0, 2]);
            operand.extend(vec![15; depth]);
            operand.extend([4, 2, 0, 0, 0, 1]);
            let result = run(&record(false, &operand), 2, 32, 100);
            if depth == 16 {
                assert!(result.is_ok());
            } else {
                assert_eq!(result.unwrap_err().status, 5);
            }
        }
        let source = record(false, &[0, 0, 2, 1, 4, 2, 0, 0, 0, 1]);
        assert_eq!(field(&run(&source, 2, 32, 20).unwrap()[0], FIXED_WIDTH), 0);
    }
    #[test]
    fn nonmatches_and_label_policy() {
        let mut source = record(true, &[0, 0, 2, 0, 4, 2, 0, 0, 0, 1]);
        source[7] = 1;
        assert_eq!(run(&source, 2, 32, 20).unwrap_err().status, 5);
        source[12] = 0x35;
        assert!(run(&source, 2, 0, 2).unwrap().is_empty());
        source[1] = 8;
        assert!(run(&source, 2, 0, 1).unwrap().is_empty());
    }
}
