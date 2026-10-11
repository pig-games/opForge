// SPDX-License-Identifier: GPL-3.0-or-later
//! Package-selected file operands over immutable prepared-source input.
use crate::macro_descriptor_vm::DescriptorError;
use package::package::{
    FILE_ACTION_BINARY, FILE_ACTION_SOURCE, FILE_OP_END, FILE_OP_ENVELOPE, FILE_OP_OPERAND,
    FILE_OP_PUBLISH, FILE_OP_SELECT, FILE_PROGRAM_COUNT, FILE_PROGRAM_FIXED_BYTES,
    FILE_PROGRAM_FLAGS, FILE_PROGRAM_ROWS, FILE_PROGRAM_ROW_BYTES, FILE_PROGRAM_SELECT,
    FILE_ROW_ACTION, PARSER_VM_MACRO_VERSION, PARSER_VM_PACKED_FILE_ENTRY,
};

pub const RESULT_KIND: u16 = 17;
pub const PATH_OFFSET: usize = 4;
pub const PATH_BYTES: usize = 8;
pub const PREFIX_BYTES: usize = 12;
pub const ACTION: usize = 16;

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
    let count = program.get(FILE_PROGRAM_COUNT).copied().unwrap_or(0) as usize;
    if count == 0
        || program.len() != FILE_PROGRAM_FIXED_BYTES + count * FILE_PROGRAM_ROW_BYTES
        || program[0] != FILE_OP_ENVELOPE
        || program[FILE_PROGRAM_FLAGS] & !1 != 0
        || program[FILE_PROGRAM_SELECT] != FILE_OP_SELECT
        || program[FILE_PROGRAM_ROWS + count * FILE_PROGRAM_ROW_BYTES..]
            != [FILE_OP_OPERAND, FILE_OP_PUBLISH, FILE_OP_END]
    {
        return Err(error(6, 0, "Invalid packed file program"));
    }
    let rows = &program[FILE_PROGRAM_ROWS..FILE_PROGRAM_ROWS + count * FILE_PROGRAM_ROW_BYTES];
    for (index, row) in rows.chunks_exact(FILE_PROGRAM_ROW_BYTES).enumerate() {
        if !matches!(
            row[FILE_ROW_ACTION],
            FILE_ACTION_BINARY | FILE_ACTION_SOURCE
        ) || rows[..index * FILE_PROGRAM_ROW_BYTES]
            .chunks_exact(FILE_PROGRAM_ROW_BYTES)
            .any(|prior| prior[..2] == row[..2])
        {
            return Err(error(6, 0, "Invalid packed file action table"));
        }
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
    if program[FILE_PROGRAM_FLAGS] & 1 != 0
        && source.len() >= 9
        && source[head] <= 1
        && source[head + 4] == 5
    {
        head += 5;
        prefix = 5;
    }
    tick(head)?; // selected directive match
    if source.len() - head < 5 || source[head] != 7 || source[head + 1] > 1 || source[head + 4] != 0
    {
        return Ok(Vec::new());
    }
    let Some(selected) = rows
        .chunks_exact(FILE_PROGRAM_ROW_BYTES)
        .find(|row| row[..2] == source[head + 2..head + 4])
    else {
        return Ok(Vec::new());
    };
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
    row[ACTION..ACTION + 4].copy_from_slice(&u32::from(selected[FILE_ROW_ACTION]).to_be_bytes());
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
        execute(
            5,
            2,
            &packed_file_program(&[(0x1234, FILE_ACTION_BINARY), (0x1235, FILE_ACTION_SOURCE)]),
            source,
            capacity,
            budget,
        )
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
            assert_eq!(u32::from_be_bytes(row[16..20].try_into().unwrap()), 1);
            assert_eq!(&row[20..], &[0; 12]);
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
        source[7] = 0x36;
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
        let mut program =
            packed_file_program(&[(0x1234, FILE_ACTION_BINARY), (0x1235, FILE_ACTION_SOURCE)]);
        program[1] = 0;
        assert!(execute(5, 2, &program, &source, 32, 2).unwrap().is_empty());
        source[7] = 1;
        assert_eq!(run(&source, 32, 5).unwrap_err().status, 5);
        // A qualified unrelated label is still a nonmatch.
        source[12] = 0x36;
        assert!(run(&source, 32, 2).unwrap().is_empty());
        source[1] = 64;
        assert_eq!(run(&source, 32, 5).unwrap_err().status, 4);
    }
    #[test]
    fn publication_and_program_contract_fail_closed() {
        let source = record(false, &[3, 1, b'a']);
        assert_eq!(run(&source, 31, 5).unwrap_err().status, 7);
        assert_eq!(run(&source, 32, 4).unwrap_err().status, 12);
        let mut program =
            packed_file_program(&[(0x1234, FILE_ACTION_BINARY), (0x1235, FILE_ACTION_SOURCE)]);
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

    #[test]
    fn action_table_and_exact_budget_contract() {
        for (id, action) in [(0x34, FILE_ACTION_BINARY), (0x35, FILE_ACTION_SOURCE)] {
            let mut source = record(false, &[3, 1, b'a']);
            source[7] = id;
            let row = run(&source, 32, 5).unwrap()[0];
            assert_eq!(
                u32::from_be_bytes(row[ACTION..ACTION + 4].try_into().unwrap()),
                u32::from(action)
            );
            for budget in 0..5 {
                assert_eq!(run(&source, 32, budget).unwrap_err().status, 12);
            }
        }
        let source = record(false, &[3, 1, b'a']);
        for rows in [
            vec![(0x1234, 0)],
            vec![(0x1234, 3)],
            vec![(0x1234, 1), (0x1234, 2)],
        ] {
            assert_eq!(
                execute(5, 2, &packed_file_program(&rows), &source, 32, 5)
                    .unwrap_err()
                    .status,
                6
            );
        }
        let mut program = packed_file_program(&[(0x1234, 1)]);
        program[FILE_PROGRAM_COUNT] = 2;
        assert_eq!(
            execute(5, 2, &program, &source, 32, 5).unwrap_err().status,
            6
        );
    }
}
