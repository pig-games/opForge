// SPDX-License-Identifier: GPL-3.0-or-later
//! Package-selected inline metadata over immutable packed records, without source parsing.
use crate::macro_descriptor_vm::DescriptorError;
use package::package::{
    METADATA_HEAD_COUNT, METADATA_KEY_NAME, METADATA_KEY_VERSION, METADATA_OPERAND_BIN,
    METADATA_OPERAND_FILL, METADATA_OPERAND_OPTIONAL, METADATA_OPERAND_REQUIRED, METADATA_OP_END,
    METADATA_OP_PUBLISH, METADATA_OP_SELECT, METADATA_PROGRAM_BYTES, METADATA_PROGRAM_PUBLISH,
    METADATA_PROGRAM_ROWS, METADATA_ROLE_DESCRIPTIVE, METADATA_ROLE_OUTPUT, METADATA_ROW_BYTES,
    METADATA_ROW_HEAD, METADATA_ROW_KEY, METADATA_ROW_OPERAND, METADATA_ROW_ROLE,
    PARSER_VM_MACRO_VERSION, PARSER_VM_PACKED_METADATA_ENTRY,
};

pub const RESULT_KIND: u16 = 19;
pub const RESULT_BYTES: usize = 32;
const RECORD_BYTES: usize = 4;
const MAX_RECORD_BYTES: usize = 256;
const RECORD_LENGTH: usize = 0;
const RECORD_FLAGS: usize = 1;
const IMMUTABLE_FLAGS: u8 = 63;
const INACTIVE_FLAGS: u8 = 24;
const BLOCK_FLAGS: u8 = 6;
const HEAD: usize = RECORD_BYTES;
const HEAD_PREFIX: usize = HEAD + 1;
const HEAD_ID: usize = HEAD + 2;
const HEAD_QUALIFIER: usize = HEAD + 4;
const OPERAND: usize = HEAD + 5;
const STRING_LENGTH: usize = OPERAND + 1;
const STRING_VALUE: usize = OPERAND + 2;
const DOT_KIND: u8 = 7;
const STRING_KIND: u8 = 3;
pub const ROLE: usize = 2;
pub const KEY: usize = 4;
pub const VALUE_OFFSET: usize = 8;
pub const VALUE_BYTES: usize = 12;
pub const VALUE_KIND: usize = 16;
/// A nonzero start-spelling extent signals an inclusive binary range.
pub const RANGE_TEXT_OFFSET: usize = 20;
pub const RANGE_TEXT_BYTES: usize = 22;
pub const RANGE_START: usize = 24;
pub const RANGE_END: usize = 28;
pub const FILL_VALUE: usize = RANGE_START;

fn error(status: u32, offset: usize, message: &'static str) -> DescriptorError {
    DescriptorError {
        status,
        offset: offset as u32,
        message,
    }
}

/// Result spans refer to the complete record. Errors publish no partial results.
/// Operands are decoded literals. Binary range/fill interpretation belongs to the
/// package-selected operand operation; block/target metadata remains unsupported.
/// Budget units cover envelope, head lookup, operand, publication, and end:
/// matched records need five, inactive one, unrelated two. Literal scans are
/// bounded by the validated 256-byte record and do not consume per-byte units.
pub fn execute(
    entry: u16,
    version: u16,
    program: &[u8],
    source: &[u8],
    capacity: usize,
    mut steps: usize,
) -> Result<Vec<[u8; RESULT_BYTES]>, DescriptorError> {
    if entry != PARSER_VM_PACKED_METADATA_ENTRY || version != PARSER_VM_MACRO_VERSION {
        return Err(error(4, 0, "Invalid packed metadata entry or version"));
    }
    if program.len() != METADATA_PROGRAM_BYTES
        || program[..METADATA_PROGRAM_ROWS] != [METADATA_OP_SELECT, METADATA_HEAD_COUNT as u8]
        || program[METADATA_PROGRAM_PUBLISH..] != [METADATA_OP_PUBLISH, METADATA_OP_END]
        || program[METADATA_PROGRAM_ROWS..METADATA_PROGRAM_PUBLISH]
            .chunks_exact(METADATA_ROW_BYTES)
            .any(|row| {
                !(METADATA_ROLE_OUTPUT..=METADATA_ROLE_DESCRIPTIVE)
                    .contains(&row[METADATA_ROW_ROLE])
                    || !(METADATA_KEY_NAME..=METADATA_KEY_VERSION).contains(&row[METADATA_ROW_KEY])
                    || !matches!(
                        row[METADATA_ROW_OPERAND],
                        METADATA_OPERAND_REQUIRED
                            | METADATA_OPERAND_OPTIONAL
                            | METADATA_OPERAND_BIN
                            | METADATA_OPERAND_FILL
                    )
            })
    {
        return Err(error(6, 0, "Invalid packed metadata program"));
    }
    if source.len() < RECORD_BYTES
        || source.len() > MAX_RECORD_BYTES
        || usize::from(source[RECORD_LENGTH]) + 1 != source.len()
        || source[RECORD_FLAGS] & !IMMUTABLE_FLAGS != 0
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
    if source[RECORD_FLAGS] & INACTIVE_FLAGS != 0 || source.len() == RECORD_BYTES {
        return Ok(Vec::new());
    }
    tick(HEAD)?;
    if source.len() < OPERAND || source[HEAD] != DOT_KIND || source[HEAD_PREFIX] > 1 {
        return Ok(Vec::new());
    }
    let Some(selected) = program[METADATA_PROGRAM_ROWS..METADATA_PROGRAM_PUBLISH]
        .chunks_exact(METADATA_ROW_BYTES)
        .find(|row| {
            row[METADATA_ROW_HEAD..METADATA_ROW_HEAD + 2] == source[HEAD_ID..HEAD_QUALIFIER]
        })
    else {
        return Ok(Vec::new());
    };
    if source[HEAD_QUALIFIER] != 0 || source[RECORD_FLAGS] & BLOCK_FLAGS != 0 {
        return Err(error(
            5,
            HEAD_QUALIFIER,
            "Unsupported metadata qualification or block",
        ));
    }
    tick(OPERAND)?;
    let (offset, bytes, kind) = if source.len() == OPERAND
        && matches!(
            selected[METADATA_ROW_OPERAND],
            METADATA_OPERAND_OPTIONAL | METADATA_OPERAND_BIN
        ) {
        (OPERAND, 0, 0)
    } else {
        if source.len() < STRING_VALUE || source[OPERAND] != STRING_KIND {
            return Err(error(5, OPERAND, "Metadata requires a decoded string"));
        }
        let bytes = usize::from(source[STRING_LENGTH]);
        if STRING_VALUE + bytes != source.len() {
            return Err(error(5, STRING_VALUE, "Invalid metadata operand extent"));
        }
        (STRING_VALUE, bytes, u32::from(STRING_KIND))
    };
    let mut row = [0; RESULT_BYTES];
    let (offset, bytes) = match selected[METADATA_ROW_OPERAND] {
        METADATA_OPERAND_BIN => decode_bin(source, offset, bytes, &mut row)?,
        METADATA_OPERAND_FILL => {
            let value = hex(&source[offset..offset + bytes], 2, 2).ok_or(error(
                5,
                offset,
                "Invalid metadata fill byte",
            ))?;
            row[FILL_VALUE..FILL_VALUE + 4].copy_from_slice(&value.to_be_bytes());
            (offset, bytes)
        }
        _ => (offset, bytes),
    };
    tick(offset)?;
    if capacity < RESULT_BYTES {
        return Err(error(7, offset, "Metadata result capacity exceeded"));
    }
    tick(offset)?;
    row[..2].copy_from_slice(&RESULT_KIND.to_be_bytes());
    row[ROLE..ROLE + 2].copy_from_slice(&u16::from(selected[METADATA_ROW_ROLE]).to_be_bytes());
    for (field, value) in [
        (KEY, u32::from(selected[METADATA_ROW_KEY])),
        (VALUE_OFFSET, offset as u32),
        (VALUE_BYTES, bytes as u32),
        (VALUE_KIND, kind),
    ] {
        row[field..field + 4].copy_from_slice(&value.to_be_bytes());
    }
    Ok(vec![row])
}

fn hex(bytes: &[u8], min: usize, max: usize) -> Option<u32> {
    if !(min..=max).contains(&bytes.len()) {
        return None;
    }
    bytes.iter().try_fold(0u32, |value, byte| {
        let digit = match byte {
            b'0'..=b'9' => byte - b'0',
            b'a'..=b'f' => byte - b'a' + 10,
            b'A'..=b'F' => byte - b'A' + 10,
            _ => return None,
        };
        Some((value << 4) | u32::from(digit))
    })
}

fn decode_bin(
    source: &[u8],
    offset: usize,
    bytes: usize,
    row: &mut [u8; RESULT_BYTES],
) -> Result<(usize, usize), DescriptorError> {
    let value = &source[offset..offset + bytes];
    let mut separators = value
        .iter()
        .enumerate()
        .rev()
        .filter_map(|(index, byte)| (*byte == b':').then_some(index));
    let Some(last) = separators.next() else {
        return Ok((offset, bytes));
    };
    let previous = separators.next();
    let start_offset = previous.map_or(0, |index| index + 1);
    let start_bytes = last - start_offset;
    let start = hex(&value[start_offset..last], 4, 8).ok_or(error(
        5,
        offset,
        "Invalid metadata binary range",
    ))?;
    let end =
        hex(&value[last + 1..], 4, 8).ok_or(error(5, offset, "Invalid metadata binary range"))?;
    if start > end {
        return Err(error(5, offset, "Invalid metadata binary range"));
    }
    row[RANGE_TEXT_OFFSET..RANGE_TEXT_OFFSET + 2]
        .copy_from_slice(&((offset + start_offset) as u16).to_be_bytes());
    row[RANGE_TEXT_BYTES..RANGE_TEXT_BYTES + 2]
        .copy_from_slice(&(start_bytes as u16).to_be_bytes());
    row[RANGE_START..RANGE_START + 4].copy_from_slice(&start.to_be_bytes());
    row[RANGE_END..RANGE_END + 4].copy_from_slice(&end.to_be_bytes());
    Ok((offset, previous.unwrap_or(0)))
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
        source[RECORD_LENGTH] = (source.len() - 1) as u8;
        source
    }
    fn run(
        source: &[u8],
        capacity: usize,
        steps: usize,
    ) -> Result<Vec<[u8; RESULT_BYTES]>, DescriptorError> {
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
        for head in [10, 11, 14, 15] {
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
    fn numeric_bin_and_fill_match_shared_output_model() {
        use crate::output_model::parse_bin_output_arg;
        for value in [
            "",
            "image",
            "0000:000F",
            "ABcd:ffff",
            "image:0000:000f",
            ":0000:000f",
            "Work:image:0000:000f",
            "00000000:ffffffff",
            "000:ffff",
            "0000:fff",
            "ffff:0000",
            "name:bad:ffff",
            "a:b:c",
        ] {
            let mut operand = vec![3, value.len() as u8];
            operand.extend_from_slice(value.as_bytes());
            let actual = run(&record(12, &operand), 32, 5);
            match parse_bin_output_arg(value) {
                Err(_) => assert_eq!(actual.unwrap_err().status, 5, "{value}"),
                Ok(expected) => {
                    let source = record(12, &operand);
                    let row = actual.unwrap()[0];
                    let name_offset = u32::from_be_bytes(row[8..12].try_into().unwrap()) as usize;
                    let name_bytes = u32::from_be_bytes(row[12..16].try_into().unwrap()) as usize;
                    assert_eq!(
                        &source[name_offset..name_offset + name_bytes],
                        expected.name.as_deref().unwrap_or("").as_bytes(),
                        "{value}"
                    );
                    let text_offset = u16::from_be_bytes(row[20..22].try_into().unwrap()) as usize;
                    let text_bytes = u16::from_be_bytes(row[22..24].try_into().unwrap()) as usize;
                    if let Some(range) = expected.range {
                        assert_eq!(
                            &source[text_offset..text_offset + text_bytes],
                            range.start_str.as_bytes()
                        );
                        assert_eq!(
                            u32::from_be_bytes(row[24..28].try_into().unwrap()),
                            range.start
                        );
                        assert_eq!(
                            u32::from_be_bytes(row[28..32].try_into().unwrap()),
                            range.end
                        );
                    } else {
                        assert_eq!(&row[20..], &[0; 12]);
                    }
                }
            }
        }
        for value in ["00", "aa", "FF", "Af", "gg", "a", "aaa", "", "0x"] {
            let mut operand = vec![3, value.len() as u8];
            operand.extend_from_slice(value.as_bytes());
            let actual = run(&record(13, &operand), 32, 5);
            match u8::from_str_radix(value, 16)
                .ok()
                .filter(|_| value.len() == 2)
            {
                Some(expected) => assert_eq!(
                    u32::from_be_bytes(actual.unwrap()[0][24..28].try_into().unwrap()),
                    u32::from(expected)
                ),
                None => assert_eq!(actual.unwrap_err().status, 5),
            }
        }
        let source = record(
            12,
            &[3, 9, b'0', b'0', b'0', b'0', b':', b'f', b'f', b'f', b'f'],
        );
        assert_eq!(run(&source, 31, 5).unwrap_err().status, 7);
        assert_eq!(run(&source, 32, 4).unwrap_err().status, 12);
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
    fn selector_layout_preserves_wire_bytes_and_rejects_malformed_operations() {
        let expected = [
            0x96, 6, 0, 10, 1, 1, 0, 0, 11, 1, 2, 1, 0, 12, 1, 3, 3, 0, 13, 1, 4, 4, 0, 14, 2, 1,
            0, 0, 15, 2, 5, 0, 0x83, 0,
        ];
        let program = packed_metadata_program([10, 11, 12, 13, 14, 15]);
        assert_eq!(program, expected);
        let source = record(10, &[STRING_KIND, 1, b'x']);
        for (field, invalid) in [
            (0, 0),
            (1, 5),
            (METADATA_PROGRAM_ROWS + METADATA_ROW_ROLE, 0),
            (METADATA_PROGRAM_ROWS + METADATA_ROW_ROLE, 3),
            (METADATA_PROGRAM_ROWS + METADATA_ROW_KEY, 0),
            (METADATA_PROGRAM_ROWS + METADATA_ROW_KEY, 6),
            (METADATA_PROGRAM_ROWS + METADATA_ROW_OPERAND, 2),
            (METADATA_PROGRAM_ROWS + METADATA_ROW_OPERAND, 255),
            (METADATA_PROGRAM_PUBLISH, 0),
            (METADATA_PROGRAM_BYTES - 1, 1),
        ] {
            let mut invalid_program = program.clone();
            invalid_program[field] = invalid;
            assert_eq!(
                execute(
                    PARSER_VM_PACKED_METADATA_ENTRY,
                    PARSER_VM_MACRO_VERSION,
                    &invalid_program,
                    &source,
                    RESULT_BYTES,
                    5
                )
                .unwrap_err()
                .status,
                6
            );
        }
        for extent in 0..program.len() {
            assert_eq!(
                execute(
                    PARSER_VM_PACKED_METADATA_ENTRY,
                    PARSER_VM_MACRO_VERSION,
                    &program[..extent],
                    &source,
                    RESULT_BYTES,
                    5
                )
                .unwrap_err()
                .status,
                6
            );
        }
    }

    #[test]
    fn bounded_literals_and_budget_capacity_boundaries_publish_atomically() {
        let mut operand = vec![STRING_KIND, (MAX_RECORD_BYTES - STRING_VALUE) as u8];
        operand.resize(MAX_RECORD_BYTES - OPERAND, b'x');
        let source = record(10, &operand);
        assert_eq!(source.len(), MAX_RECORD_BYTES);
        for budget in 0..5 {
            assert_eq!(run(&source, RESULT_BYTES, budget).unwrap_err().status, 12);
        }
        for capacity in [0, 1, RESULT_BYTES - 1] {
            assert_eq!(run(&source, capacity, 5).unwrap_err().status, 7);
        }
        let expected = run(&source, RESULT_BYTES, 5).unwrap();
        assert_eq!(expected.len(), 1);
        for extent in 0..source.len() {
            assert_eq!(
                run(&source[..extent], RESULT_BYTES, 5).unwrap_err().status,
                4
            );
        }
        let mut short = source[..source.len() - 1].to_vec();
        short[RECORD_LENGTH] = (short.len() - 1) as u8;
        assert_eq!(run(&short, RESULT_BYTES, 5).unwrap_err().status, 5);
        let mut oversized = source.clone();
        oversized.push(b'x');
        assert_eq!(run(&oversized, RESULT_BYTES, 5).unwrap_err().status, 4);
        assert_eq!(run(&source, RESULT_BYTES, 5).unwrap(), expected);
        let mut inactive = source;
        inactive[RECORD_FLAGS] = INACTIVE_FLAGS;
        assert!(run(&inactive, 0, 1).unwrap().is_empty());
        assert_eq!(run(&inactive, 0, 0).unwrap_err().status, 12);
        let unrelated = record(90, &[]);
        assert_eq!(run(&unrelated, 0, 1).unwrap_err().status, 12);
        assert!(run(&unrelated, 0, 2).unwrap().is_empty());
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
        program[6] = 2;
        assert_eq!(
            execute(7, 2, &program, &source, 32, 5).unwrap_err().status,
            6
        );
    }
}
