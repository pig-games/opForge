// SPDX-License-Identifier: GPL-3.0-or-later
//! Package-selected inline metadata over immutable packed records, without source parsing.
use crate::macro_descriptor_vm::DescriptorError;
use package::package::{PARSER_VM_MACRO_VERSION, PARSER_VM_PACKED_METADATA_ENTRY};

pub const RESULT_KIND: u16 = 19;
pub const ROLE: usize = 2;
pub const KEY: usize = 4;
pub const VALUE_OFFSET: usize = 8;
pub const VALUE_BYTES: usize = 12;
pub const VALUE_KIND: usize = 16;

fn error(status: u32, offset: usize, message: &'static str) -> DescriptorError {
    DescriptorError {
        status,
        offset: offset as u32,
        message,
    }
}

/// Result spans refer to the complete record. Errors publish no partial results.
/// This checkpoint accepts decoded strings only; raw identifier/number spellings
/// and block/target-specific metadata deliberately remain unsupported.
pub fn execute(
    entry: u16,
    version: u16,
    program: &[u8],
    source: &[u8],
    capacity: usize,
    mut steps: usize,
) -> Result<Vec<[u8; 32]>, DescriptorError> {
    if entry != PARSER_VM_PACKED_METADATA_ENTRY || version != PARSER_VM_MACRO_VERSION {
        return Err(error(4, 0, "Invalid packed metadata entry or version"));
    }
    if program.len() != 34
        || program[..2] != [0x96, 6]
        || program[32..] != [0x83, 0]
        || program[2..32]
            .chunks_exact(5)
            .any(|row| !(1..=2).contains(&row[2]) || !(1..=5).contains(&row[3]) || row[4] > 2)
    {
        return Err(error(6, 0, "Invalid packed metadata program"));
    }
    if source.len() < 4
        || source.len() > 256
        || usize::from(source[0]) + 1 != source.len()
        || source[1] & !63 != 0
    {
        return Err(error(4, 0, "Invalid packed record extent or flags"));
    }
    let mut tick = |offset| {
        steps = steps
            .checked_sub(1)
            .ok_or(error(12, offset, "Step budget exceeded"))?;
        Ok::<_, DescriptorError>(())
    };
    tick(0)?;
    if source[1] & 24 != 0 || source.len() == 4 {
        return Ok(Vec::new());
    }
    tick(4)?;
    if source.len() < 9 || source[4] != 7 || source[5] > 1 {
        return Ok(Vec::new());
    }
    let Some(selected) = program[2..32]
        .chunks_exact(5)
        .find(|row| row[..2] == source[6..8])
    else {
        return Ok(Vec::new());
    };
    if source[8] != 0 || source[1] & 6 != 0 {
        return Err(error(5, 8, "Unsupported metadata qualification or block"));
    }
    tick(9)?;
    let (offset, bytes, kind) = if source.len() == 9 && selected[4] == 1 {
        (9, 0, 0)
    } else {
        if source.len() < 11 || source[9] != 3 {
            return Err(error(5, 9, "Metadata requires a decoded string"));
        }
        let bytes = usize::from(source[10]);
        if 11 + bytes != source.len() || selected[4] == 2 && bytes != 2 {
            return Err(error(5, 11, "Invalid metadata operand extent"));
        }
        (11, bytes, 3)
    };
    tick(offset)?;
    if capacity < 32 {
        return Err(error(7, offset, "Metadata result capacity exceeded"));
    }
    tick(offset)?;
    let mut row = [0; 32];
    row[..2].copy_from_slice(&RESULT_KIND.to_be_bytes());
    row[ROLE..ROLE + 2].copy_from_slice(&u16::from(selected[2]).to_be_bytes());
    for (field, value) in [
        (KEY, u32::from(selected[3])),
        (VALUE_OFFSET, offset as u32),
        (VALUE_BYTES, bytes as u32),
        (VALUE_KIND, kind),
    ] {
        row[field..field + 4].copy_from_slice(&value.to_be_bytes());
    }
    Ok(vec![row])
}

#[cfg(test)]
mod tests {
    use super::*;
    use package::package::packed_metadata_program;
    fn record(head: u16, operand: &[u8]) -> Vec<u8> {
        let mut source = vec![0, 1, 0, 7, 7, 0];
        source.extend(head.to_be_bytes());
        source.push(0);
        source.extend(operand);
        source[0] = (source.len() - 1) as u8;
        source
    }
    fn run(source: &[u8], capacity: usize, steps: usize) -> Result<Vec<[u8; 32]>, DescriptorError> {
        execute(
            7,
            2,
            &packed_metadata_program([10, 11, 12, 13, 14, 15]),
            source,
            capacity,
            steps,
        )
    }
    #[test]
    fn required_optional_and_descriptive_spans() {
        for head in [10, 11, 12, 14, 15] {
            let row = run(&record(head, &[3, 3, b'a', b'/', b'b']), 32, 5).unwrap()[0];
            assert_eq!(u32::from_be_bytes(row[8..12].try_into().unwrap()), 11);
            assert_eq!(u32::from_be_bytes(row[12..16].try_into().unwrap()), 3);
            assert_eq!(row[3], if head >= 14 { 2 } else { 1 });
            assert_eq!(&row[20..], &[0; 12]);
            assert_eq!(run(&record(head, &[3, 0]), 32, 5).unwrap().len(), 1);
        }
        for head in [11, 12] {
            assert_eq!(run(&record(head, &[]), 32, 5).unwrap()[0][19], 0);
        }
        assert_eq!(
            run(&record(13, &[3, 2, b'a', b'a']), 32, 5).unwrap()[0][7],
            4
        );
    }
    #[test]
    fn malformed_operands_and_atomic_failure() {
        for source in [
            record(10, &[]),
            record(10, &[2, 0, 0, 0, 1]),
            record(10, &[3, 2, b'a']),
            record(10, &[3, 1, b'a', 4]),
            record(13, &[3, 0]),
        ] {
            assert_eq!(run(&source, 32, 20).unwrap_err().status, 5);
        }
        let source = record(10, &[3, 0]);
        assert_eq!(run(&source, 31, 5).unwrap_err().status, 7);
        assert_eq!(run(&source, 32, 4).unwrap_err().status, 12);
        let mut source = source;
        source[1] |= 2;
        assert_eq!(run(&source, 32, 5).unwrap_err().status, 5);
    }
    #[test]
    fn inactive_and_unrelated_are_quiet() {
        let mut source = record(10, &[2]);
        for flags in [8, 16] {
            source[1] = flags;
            assert!(run(&source, 0, 1).unwrap().is_empty());
        }
        assert!(run(&record(90, &[2]), 0, 2).unwrap().is_empty());
        assert!(run(&[3, 0, 0, 1], 0, 1).unwrap().is_empty());
        let mut program = packed_metadata_program([10, 11, 12, 13, 14, 15]);
        program[6] = 3;
        assert_eq!(
            execute(7, 2, &program, &source, 32, 5).unwrap_err().status,
            6
        );
    }
}
