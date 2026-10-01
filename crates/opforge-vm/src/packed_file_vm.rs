// SPDX-License-Identifier: GPL-3.0-or-later
//! Package-selected file operands over immutable prepared-source input.
use crate::macro_descriptor_vm::DescriptorError;
use package::package::{PARSER_VM_MACRO_VERSION, PARSER_VM_PACKED_FILE_ENTRY};

pub const RESULT_KIND: u16 = 17;
pub const PATH_OFFSET: usize = 4;
pub const PATH_BYTES: usize = 8;
pub const PREFIX_BYTES: usize = 12;

fn error(status: u32, offset: usize, message: &'static str) -> DescriptorError {
    DescriptorError {
        status,
        offset: offset as u32,
        message,
    }
}

/// Results contain only big-endian record-relative spans, never host pointers.
/// Publication is atomic: errors return no partially filled result storage.
pub fn execute(
    entry: u16,
    version: u16,
    program: &[u8],
    source: &[u8],
    capacity: usize,
    mut steps: usize,
) -> Result<Vec<[u8; 32]>, DescriptorError> {
    if entry != PARSER_VM_PACKED_FILE_ENTRY || version != PARSER_VM_MACRO_VERSION {
        return Err(error(4, 0, "Invalid packed file entry or version"));
    }
    if program.len() != 8
        || program[0] != 0x90
        || program[1] & !1 != 0
        || program[2] != 0x91
        || program[5..] != [0x92, 0x83, 0]
    {
        return Err(error(6, 0, "Invalid packed file program"));
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
    tick(0)?; // envelope
    if source[1] & (8 | 16) != 0 || source.len() == 4 {
        return Ok(Vec::new());
    }
    let mut head = 4;
    let mut prefix = 0;
    if program[1] & 1 != 0 && source.len() >= 9 && source[head] <= 1 && source[head + 4] == 5 {
        head += 5;
        prefix = 5;
    }
    tick(head)?; // selected directive match
    if source.len() - head < 5
        || source[head] != 7
        || source[head + 1] > 1
        || source[head + 4] != 0
        || source[head + 2..head + 4] != program[3..5]
    {
        return Ok(Vec::new());
    }
    if prefix != 0 && source[7] != 0 {
        return Err(error(5, 7, "Qualified file label"));
    }
    head += 5;
    tick(head)?; // decoded operand
    if source.len() - head < 2 || source[head] != 3 {
        return Err(error(5, head, "File directive requires one decoded string"));
    }
    let bytes = usize::from(source[head + 1]);
    head += 2;
    if bytes == 0 || head + bytes != source.len() {
        return Err(error(5, head, "Invalid file operand extent"));
    }
    tick(head)?; // publish
    if capacity < 32 {
        return Err(error(7, head, "File result capacity exceeded"));
    }
    tick(head)?; // end
    let mut row = [0; 32];
    row[..2].copy_from_slice(&RESULT_KIND.to_be_bytes());
    row[PATH_OFFSET..PATH_OFFSET + 4].copy_from_slice(&(head as u32).to_be_bytes());
    row[PATH_BYTES..PATH_BYTES + 4].copy_from_slice(&(bytes as u32).to_be_bytes());
    row[PREFIX_BYTES..PREFIX_BYTES + 4].copy_from_slice(&(prefix as u32).to_be_bytes());
    Ok(vec![row])
}

#[cfg(test)]
mod tests {
    use super::*;
    use package::package::packed_file_program;
    fn record(label: bool, operand: &[u8]) -> Vec<u8> {
        let mut bytes = vec![0, 1, 0, 7];
        if label {
            bytes.extend([0, 0, 90, 0, 5]);
        }
        bytes.extend([7, 0, 0x12, 0x34, 0]);
        bytes.extend_from_slice(operand);
        bytes[0] = (bytes.len() - 1) as u8;
        bytes
    }
    fn run(
        source: &[u8],
        capacity: usize,
        budget: usize,
    ) -> Result<Vec<[u8; 32]>, DescriptorError> {
        execute(5, 2, &packed_file_program(0x1234), source, capacity, budget)
    }
    #[test]
    fn decoded_file_spans_and_labels() {
        for label in [false, true] {
            let source = record(label, &[3, 3, b'a', b'/', b'b']);
            let rows = run(&source, 32, 5).unwrap();
            let row = &rows[0];
            assert_eq!(
                u32::from_be_bytes(row[4..8].try_into().unwrap()),
                if label { 16 } else { 11 }
            );
            assert_eq!(u32::from_be_bytes(row[8..12].try_into().unwrap()), 3);
            assert_eq!(
                u32::from_be_bytes(row[12..16].try_into().unwrap()),
                if label { 5 } else { 0 }
            );
            assert_eq!(&row[16..], &[0; 16]);
        }
    }
    #[test]
    fn recognized_bad_operands_fail() {
        for operand in [
            &[][..],
            &[2, 0, 0, 0, 1],
            &[3, 0],
            &[3, 2, b'a'],
            &[3, 1, b'a', 4],
        ] {
            assert_eq!(run(&record(false, operand), 32, 10).unwrap_err().status, 5);
        }
    }
    #[test]
    fn nonmatches_and_inactive_lines_are_quiet() {
        let mut source = record(false, &[3, 1, b'a']);
        source[7] = 0x35;
        assert!(run(&source, 0, 2).unwrap().is_empty());
        source[7] = 0x34;
        for flag in [8, 16] {
            source[1] = flag;
            assert!(run(&source, 0, 1).unwrap().is_empty());
        }
        assert!(run(&[3, 0, 0, 1], 0, 1).unwrap().is_empty());
    }
    #[test]
    fn label_policy_and_record_contract() {
        let mut source = record(true, &[3, 1, b'a']);
        let mut program = packed_file_program(0x1234);
        program[1] = 0;
        assert!(execute(5, 2, &program, &source, 32, 2).unwrap().is_empty());
        source[7] = 1;
        assert_eq!(run(&source, 32, 5).unwrap_err().status, 5);
        // A qualified unrelated label is still a nonmatch.
        source[12] = 0x35;
        assert!(run(&source, 32, 2).unwrap().is_empty());
        source[1] = 64;
        assert_eq!(run(&source, 32, 5).unwrap_err().status, 4);
    }
    #[test]
    fn publication_and_program_contract_fail_closed() {
        let source = record(false, &[3, 1, b'a']);
        assert_eq!(run(&source, 31, 5).unwrap_err().status, 7);
        assert_eq!(run(&source, 32, 4).unwrap_err().status, 12);
        let mut program = packed_file_program(0x1234);
        program[1] = 2;
        assert_eq!(
            execute(5, 2, &program, &source, 32, 5).unwrap_err().status,
            6
        );
        assert_eq!(
            execute(3, 2, &program, &source, 32, 5).unwrap_err().status,
            4
        );
    }
}
