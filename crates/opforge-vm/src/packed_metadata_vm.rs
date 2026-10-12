// SPDX-License-Identifier: GPL-3.0-or-later
//! Package-selected metadata contexts and operands over immutable packed records.
use crate::macro_descriptor_vm::DescriptorError;
#[cfg(test)]
use package::package::{
    metadata_program, MetadataRow, METADATA_HEAD_COUNT, METADATA_ROLE_META_OPEN,
};
use package::package::{
    METADATA_CONTEXT_NONE, METADATA_CONTEXT_TARGET, METADATA_KEY_NAME, METADATA_KEY_VERSION,
    METADATA_OPERAND_BIN, METADATA_OPERAND_FILL, METADATA_OPERAND_NONE, METADATA_OPERAND_OPTIONAL,
    METADATA_OPERAND_REQUIRED, METADATA_OP_END, METADATA_OP_PUBLISH, METADATA_OP_SELECT,
    METADATA_PROGRAM_COUNT, METADATA_PROGRAM_ROWS, METADATA_PROGRAM_SELECTED_TARGET,
    METADATA_PROGRAM_TARGET_COUNT, METADATA_ROLE_DESCRIPTIVE, METADATA_ROLE_OUTPUT,
    METADATA_ROLE_OUTPUT_OPEN, METADATA_ROLE_TARGET_CLOSE, METADATA_ROLE_TARGET_OPEN,
    METADATA_ROW_BYTES, METADATA_ROW_CONTEXT, METADATA_ROW_KEY, METADATA_ROW_OPERAND,
    METADATA_ROW_ROLE, METADATA_ROW_TARGET, PARSER_VM_MACRO_VERSION,
    PARSER_VM_PACKED_METADATA_ENTRY,
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
pub const TARGET: usize = 4;
pub const KEY: usize = 6;
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

/// Validate sorted row integrity once when acquiring a package program.
pub fn validate_program(program: &[u8]) -> Result<(), DescriptorError> {
    if program.len() < METADATA_PROGRAM_ROWS + 2
        || program[0] != METADATA_OP_SELECT
        || program[1] != 0
    {
        return Err(error(6, 0, "Invalid packed metadata program"));
    }
    let count = usize::from(u16::from_be_bytes(
        program[METADATA_PROGRAM_COUNT..METADATA_PROGRAM_COUNT + 2]
            .try_into()
            .unwrap(),
    ));
    let publish = METADATA_PROGRAM_ROWS + count * METADATA_ROW_BYTES;
    let target_count = u16::from_be_bytes(
        program[METADATA_PROGRAM_TARGET_COUNT..METADATA_PROGRAM_TARGET_COUNT + 2]
            .try_into()
            .unwrap(),
    );
    let selected = u16::from_be_bytes(
        program[METADATA_PROGRAM_SELECTED_TARGET..METADATA_PROGRAM_SELECTED_TARGET + 2]
            .try_into()
            .unwrap(),
    );
    if count == 0
        || program.len() != publish + 2
        || program[publish..] != [METADATA_OP_PUBLISH, METADATA_OP_END]
        || selected > target_count
    {
        return Err(error(6, 0, "Invalid packed metadata program extent"));
    }
    let rows = &program[METADATA_PROGRAM_ROWS..publish];
    let mut previous = None;
    let mut contexts = 0;
    for row in rows.chunks_exact(METADATA_ROW_BYTES) {
        let head = u16::from_be_bytes(row[..2].try_into().unwrap());
        if previous.is_some_and(|previous| previous > head) {
            return Err(error(6, 0, "Unordered metadata heads"));
        }
        if previous != Some(head) {
            contexts = 0;
        }
        if contexts & row[METADATA_ROW_CONTEXT] != 0 {
            return Err(error(6, 0, "Overlapping metadata contexts"));
        }
        contexts |= row[METADATA_ROW_CONTEXT];
        previous = Some(head);
        validate_row(row, target_count)?;
    }
    Ok(())
}

fn validate_row(row: &[u8], target_count: u16) -> Result<(), DescriptorError> {
    let role = row[METADATA_ROW_ROLE];
    let key = row[METADATA_ROW_KEY];
    let operand = row[METADATA_ROW_OPERAND];
    let target = u16::from_be_bytes(row[METADATA_ROW_TARGET..].try_into().unwrap());
    let value = matches!(role, METADATA_ROLE_OUTPUT | METADATA_ROLE_DESCRIPTIVE);
    if row[METADATA_ROW_CONTEXT] == 0
        || row[METADATA_ROW_CONTEXT] & !15 != 0
        || !(METADATA_ROLE_OUTPUT..=METADATA_ROLE_TARGET_CLOSE).contains(&role)
        || target > target_count
        || value
            && (!(METADATA_KEY_NAME..=METADATA_KEY_VERSION).contains(&key)
                || !matches!(
                    operand,
                    METADATA_OPERAND_REQUIRED
                        | METADATA_OPERAND_OPTIONAL
                        | METADATA_OPERAND_BIN
                        | METADATA_OPERAND_FILL
                ))
        || !value && (key != 0 || operand != METADATA_OPERAND_NONE)
        || matches!(role, METADATA_ROLE_TARGET_OPEN | METADATA_ROLE_TARGET_CLOSE) && target == 0
    {
        return Err(error(6, 0, "Invalid packed metadata row"));
    }
    Ok(())
}

/// Result spans refer to the complete record. Errors publish no partial results.
/// Five budget units cover envelope, binary head lookup, operand, publication,
/// and end. Literal scans fit the validated <=256-byte record. Package acquisition
/// validates sorted rows once; execution checks the exact BE16 extent/trailer and
/// selected row. Lookup needs <=16 comparisons plus <=4 disjoint context rows.
pub fn execute(
    entry: u16,
    version: u16,
    program: &[u8],
    source: &[u8],
    capacity: usize,
    steps: usize,
) -> Result<Vec<[u8; RESULT_BYTES]>, DescriptorError> {
    execute_with_context(entry, version, program, source, capacity, steps, 0)
}

/// Low word is metadata context; high word is its active canonical target ID.
/// This is one current grammar; the default entry supplies outside context.
pub fn execute_with_context(
    entry: u16,
    version: u16,
    program: &[u8],
    source: &[u8],
    capacity: usize,
    mut steps: usize,
    context: u32,
) -> Result<Vec<[u8; RESULT_BYTES]>, DescriptorError> {
    if entry != PARSER_VM_PACKED_METADATA_ENTRY || version != PARSER_VM_MACRO_VERSION {
        return Err(error(4, 0, "Invalid packed metadata entry or version"));
    }
    let read_word = |at: usize| u16::from_be_bytes(program[at..at + 2].try_into().unwrap());
    if program.len() < METADATA_PROGRAM_ROWS + 2
        || program[0] != METADATA_OP_SELECT
        || program[1] != 0
    {
        return Err(error(6, 0, "Invalid packed metadata program"));
    }
    let count = usize::from(read_word(METADATA_PROGRAM_COUNT));
    let publish = METADATA_PROGRAM_ROWS + count * METADATA_ROW_BYTES;
    let target_count = read_word(METADATA_PROGRAM_TARGET_COUNT);
    if count == 0
        || program.len() != publish + 2
        || program[publish..] != [METADATA_OP_PUBLISH, METADATA_OP_END]
        || read_word(METADATA_PROGRAM_SELECTED_TARGET) > target_count
    {
        return Err(error(6, 0, "Invalid packed metadata program extent"));
    }
    let rows = &program[METADATA_PROGRAM_ROWS..publish];
    let state = context as u16;
    let active_target = (context >> 16) as u16;
    if state > METADATA_CONTEXT_TARGET
        || active_target > target_count
        || (state == METADATA_CONTEXT_TARGET) != (active_target != 0)
    {
        return Err(error(4, 0, "Invalid metadata context"));
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
    let invalid = || {
        error(
            5,
            HEAD,
            "Only metadata directives are allowed in metadata blocks",
        )
    };
    if source.len() < OPERAND || source[HEAD] != DOT_KIND || source[HEAD_PREFIX] > 1 {
        return if state == METADATA_CONTEXT_NONE {
            Ok(Vec::new())
        } else {
            Err(invalid())
        };
    }
    let head = u16::from_be_bytes(source[HEAD_ID..HEAD_QUALIFIER].try_into().unwrap());
    let row_head = |index: usize| {
        u16::from_be_bytes(
            rows[index * METADATA_ROW_BYTES..index * METADATA_ROW_BYTES + 2]
                .try_into()
                .unwrap(),
        )
    };
    let mut low = 0;
    let mut high = count;
    while low < high {
        let mid = low + (high - low) / 2;
        if row_head(mid) < head {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    let start = low;
    let mut selected = None;
    let mut fallback = false;
    while low < count && low < start + 4 && row_head(low) == head {
        let row = &rows[low * METADATA_ROW_BYTES..(low + 1) * METADATA_ROW_BYTES];
        validate_row(row, target_count)?;
        fallback |= row[METADATA_ROW_ROLE] == METADATA_ROLE_OUTPUT_OPEN;
        if row[METADATA_ROW_CONTEXT] & (1 << state) != 0 {
            selected = Some(row);
            break;
        }
        low += 1;
    }
    let Some(selected) = selected else {
        return if state == METADATA_CONTEXT_NONE
            && (start == count || row_head(start) != head || fallback)
        {
            Ok(Vec::new())
        } else {
            Err(invalid())
        };
    };
    if source[HEAD_QUALIFIER] != 0 || source[RECORD_FLAGS] & BLOCK_FLAGS != 0 {
        return Err(error(
            5,
            HEAD_QUALIFIER,
            "Unsupported metadata qualification or label",
        ));
    }
    let role = selected[METADATA_ROW_ROLE];
    let mut target = u16::from_be_bytes(selected[METADATA_ROW_TARGET..].try_into().unwrap());
    if role == METADATA_ROLE_TARGET_CLOSE && target != active_target {
        return Err(error(5, HEAD, "Mismatched metadata target close"));
    }
    if role == METADATA_ROLE_OUTPUT && target == 0 && state == METADATA_CONTEXT_TARGET {
        target = active_target;
    }
    tick(OPERAND)?;
    let policy = selected[METADATA_ROW_OPERAND];
    let (offset, bytes, kind) = if source.len() == OPERAND
        && matches!(
            policy,
            METADATA_OPERAND_OPTIONAL | METADATA_OPERAND_BIN | METADATA_OPERAND_NONE
        ) {
        (OPERAND, 0, 0)
    } else {
        if policy == METADATA_OPERAND_NONE
            || source.len() < STRING_VALUE
            || source[OPERAND] != STRING_KIND
        {
            return Err(error(5, OPERAND, "Invalid metadata operand"));
        }
        let bytes = usize::from(source[STRING_LENGTH]);
        if STRING_VALUE + bytes != source.len() {
            return Err(error(5, STRING_VALUE, "Invalid metadata operand extent"));
        }
        (STRING_VALUE, bytes, u32::from(STRING_KIND))
    };
    let mut row = [0; RESULT_BYTES];
    let (offset, bytes) = match policy {
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
    row[ROLE..ROLE + 2].copy_from_slice(&u16::from(role).to_be_bytes());
    row[TARGET..TARGET + 2].copy_from_slice(&target.to_be_bytes());
    row[KEY..KEY + 2].copy_from_slice(&u16::from(selected[METADATA_ROW_KEY]).to_be_bytes());
    for (field, value) in [
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
        let program = packed_metadata_program([10, 11, 12, 13, 14, 15]);
        assert_eq!(
            &program[..METADATA_PROGRAM_ROWS],
            &[0x96, 0, 0, 6, 0, 0, 0, 0]
        );
        assert_eq!(
            program.len(),
            METADATA_PROGRAM_ROWS + METADATA_HEAD_COUNT * METADATA_ROW_BYTES + 2
        );
        let publish = program.len() - 2;
        assert_eq!(&program[publish..], &[0x83, 0]);
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
            (publish, 0),
            (program.len() - 1, 1),
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
        program[METADATA_PROGRAM_ROWS + METADATA_ROW_OPERAND] = 2;
        assert_eq!(validate_program(&program).unwrap_err().status, 6);
    }
    #[test]
    fn contexts_targets_alias_closers_and_integrity() {
        let row = |head, contexts, role, key, operand, target| MetadataRow {
            head,
            contexts,
            role,
            key,
            operand,
            target,
        };
        let program = metadata_program(
            &[
                row(10, 1, METADATA_ROLE_META_OPEN, 0, METADATA_OPERAND_NONE, 0),
                row(
                    11,
                    2,
                    METADATA_ROLE_OUTPUT_OPEN,
                    0,
                    METADATA_OPERAND_NONE,
                    0,
                ),
                row(
                    12,
                    4,
                    METADATA_ROLE_TARGET_OPEN,
                    0,
                    METADATA_OPERAND_NONE,
                    2,
                ),
                // Canonical, identifier alias and numeric-suffix close share target.
                row(
                    13,
                    8,
                    METADATA_ROLE_TARGET_CLOSE,
                    0,
                    METADATA_OPERAND_NONE,
                    2,
                ),
                row(
                    14,
                    8,
                    METADATA_ROLE_TARGET_CLOSE,
                    0,
                    METADATA_OPERAND_NONE,
                    2,
                ),
                row(
                    15,
                    8,
                    METADATA_ROLE_TARGET_CLOSE,
                    0,
                    METADATA_OPERAND_NONE,
                    2,
                ),
                row(
                    16,
                    2,
                    METADATA_ROLE_DESCRIPTIVE,
                    METADATA_KEY_NAME,
                    METADATA_OPERAND_REQUIRED,
                    0,
                ),
                row(
                    16,
                    12,
                    METADATA_ROLE_OUTPUT,
                    METADATA_KEY_NAME,
                    METADATA_OPERAND_REQUIRED,
                    0,
                ),
                row(
                    17,
                    8,
                    METADATA_ROLE_TARGET_CLOSE,
                    0,
                    METADATA_OPERAND_NONE,
                    1,
                ),
            ],
            2,
            2,
        )
        .unwrap();
        validate_program(&program).unwrap();
        let run = |head, operand: &[u8], context| {
            execute_with_context(7, 2, &program, &record(head, operand), 32, 5, context)
        };
        assert_eq!(run(10, &[], 0).unwrap()[0][3], METADATA_ROLE_META_OPEN);
        assert!(run(11, &[], 0).unwrap().is_empty());
        assert!(run(11, &[], 1).is_ok());
        assert!(run(12, &[], 2).is_ok());
        for closer in [13, 14, 15] {
            assert!(run(closer, &[], (2 << 16) | 3).is_ok());
        }
        assert_eq!(run(17, &[], (2 << 16) | 3).unwrap_err().status, 5);
        let value = [3, 1, b'x'];
        assert_eq!(run(16, &value, 1).unwrap()[0][3], METADATA_ROLE_DESCRIPTIVE);
        let event = run(16, &value, (2 << 16) | 3).unwrap()[0];
        assert_eq!(event[3], METADATA_ROLE_OUTPUT);
        assert_eq!(
            u16::from_be_bytes(event[TARGET..TARGET + 2].try_into().unwrap()),
            2
        );
        assert_eq!(run(90, &[], 2).unwrap_err().status, 5);
        assert_eq!(run(10, &[], 1).unwrap_err().status, 5);
        assert_eq!(run(12, &[], 3).unwrap_err().status, 4);
        assert_eq!(run(12, &[], (3 << 16) | 3).unwrap_err().status, 4);
        let mut malformed = program.clone();
        malformed[METADATA_PROGRAM_ROWS + METADATA_ROW_OPERAND] = METADATA_OPERAND_REQUIRED;
        assert_eq!(validate_program(&malformed).unwrap_err().status, 6);
        for length in [0, 7, program.len() - 1] {
            assert_eq!(validate_program(&program[..length]).unwrap_err().status, 6);
        }
        let mut overlap = program.clone();
        let index = 6 * METADATA_ROW_BYTES + METADATA_PROGRAM_ROWS;
        overlap[index + METADATA_ROW_CONTEXT] = 12;
        assert_eq!(validate_program(&overlap).unwrap_err().status, 6);
        assert_eq!(
            execute_with_context(7, 2, &program, &record(10, &[]), 31, 5, 0)
                .unwrap_err()
                .status,
            7
        );
        assert_eq!(
            execute_with_context(7, 2, &program, &record(10, &[]), 32, 4, 0)
                .unwrap_err()
                .status,
            12
        );
    }
}
