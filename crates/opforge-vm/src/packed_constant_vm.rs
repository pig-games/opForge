// SPDX-License-Identifier: GPL-3.0-or-later
//! Shared immutable scalar declaration envelope over packed source records.
use crate::macro_descriptor_vm::DescriptorError;
use package::package::{PARSER_VM_MACRO_VERSION, PARSER_VM_PACKED_CONSTANT_ENTRY};
pub const RESULT_KIND: u16 = 20;
pub const VALUE_OFFSET: usize = 4;
pub const VALUE_BYTES: usize = 8;
fn error(status: u32, offset: usize, message: &'static str) -> DescriptorError {
    DescriptorError {
        status,
        offset: offset as u32,
        message,
    }
}
/// Return declaration operand spans atomically; scalar syntax is compiled later.
/// An unmatched record remains untouched, including inactive/template records.
pub fn execute(
    entry: u16,
    version: u16,
    program: &[u8],
    source: &[u8],
    capacity: usize,
    steps: usize,
) -> Result<Vec<[u8; 32]>, DescriptorError> {
    if entry != PARSER_VM_PACKED_CONSTANT_ENTRY || version != PARSER_VM_MACRO_VERSION {
        return Err(error(4, 0, "Invalid packed constant entry or version"));
    }
    if program.len() != 5 || program[0] != 0x98 || program[3..] != [0x83, 0] {
        return Err(error(6, 0, "Invalid packed constant program"));
    }
    if source.len() < 4
        || source.len() > 256
        || usize::from(source[0]) + 1 != source.len()
        || source[1] & !63 != 0
    {
        return Err(error(4, 0, "Invalid packed record extent or flags"));
    }
    if steps == 0 {
        return Err(error(12, 0, "Step budget exceeded"));
    }
    if source[1] & (8 | 16) != 0 || source.len() < 13 || source[4] > 1 {
        return Ok(vec![]);
    }
    let mut head = 8;
    if source[head] == 5 {
        head += 1;
    }
    if source.len() - head < 5
        || source[head] != 7
        || source[head + 1] > 1
        || source[head + 2..head + 4] != program[1..3]
        || source[head + 4] != 0
    {
        return Ok(vec![]);
    }
    if steps < 2 {
        return Err(error(12, head, "Step budget exceeded"));
    }
    if source[7] != 0 {
        return Err(error(5, 7, "Qualified constant declaration"));
    }
    head += 5;
    if capacity < 32 {
        return Err(error(7, head, "Constant result capacity exceeded"));
    }
    if steps < 3 {
        return Err(error(12, head, "Step budget exceeded"));
    }
    let mut row = [0; 32];
    row[..2].copy_from_slice(&RESULT_KIND.to_be_bytes());
    row[VALUE_OFFSET..VALUE_OFFSET + 4].copy_from_slice(&(head as u32).to_be_bytes());
    row[VALUE_BYTES..VALUE_BYTES + 4]
        .copy_from_slice(&((source.len() - head) as u32).to_be_bytes());
    Ok(vec![row])
}
#[cfg(test)]
mod tests {
    use super::*;
    use package::package::packed_constant_program;
    fn run(
        source: &[u8],
        capacity: usize,
        budget: usize,
    ) -> Result<Vec<[u8; 32]>, DescriptorError> {
        execute(9, 2, &packed_constant_program(42), source, capacity, budget)
    }
    fn record(colon: bool, value: &[u8]) -> Vec<u8> {
        let mut r = vec![0, 1, 0, 7, 0, 0, 90, 0];
        if colon {
            r.push(5);
        }
        r.extend([7, 0, 0, 42, 0]);
        r.extend(value);
        r[0] = (r.len() - 1) as u8;
        r
    }
    #[test]
    fn declaration_spans_preserve_scalar_and_compound_inputs() {
        for colon in [false, true] {
            for value in [&[2, 0, 0, 0, 3][..], &[12, 2, 0, 0, 0, 3, 13][..], &[][..]] {
                let r = record(colon, value);
                let rows = run(&r, 32, 3).unwrap();
                assert_eq!(
                    u32::from_be_bytes(rows[0][4..8].try_into().unwrap()) as usize,
                    13 + usize::from(colon)
                );
                assert_eq!(
                    u32::from_be_bytes(rows[0][8..12].try_into().unwrap()) as usize,
                    value.len()
                );
            }
        }
    }
    #[test]
    fn rejects_invalid_envelopes_and_preserves_unmatched_records() {
        let mut r = record(false, &[2, 0, 0, 0, 3]);
        assert_eq!(run(&r, 31, 3).unwrap_err().status, 7);
        assert_eq!(run(&r, 32, 2).unwrap_err().status, 12);
        r[7] = 1;
        assert_eq!(run(&r, 32, 3).unwrap_err().status, 5);
        r[7] = 0;
        r[11] = 43;
        assert!(run(&r, 0, 1).unwrap().is_empty());
        r[11] = 42;
        r[1] = 8;
        assert!(run(&r, 0, 1).unwrap().is_empty());
        r[1] = 1;
        r[0] -= 1;
        assert_eq!(run(&r, 32, 3).unwrap_err().status, 4);
    }
}
