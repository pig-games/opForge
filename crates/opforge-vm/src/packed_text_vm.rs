// SPDX-License-Identifier: GPL-3.0-or-later
//! Text directive envelopes over immutable compact source. No evaluation,
//! encoding, name lookup, or source spelling belongs to this grammar service.
use crate::macro_descriptor_vm::DescriptorError;
use crate::packed_source_layout::*;
use package::package::{
    packed_text_program, PACKED_TEXT_DIRECTIVE_ACTION, PACKED_TEXT_DIRECTIVE_BYTES,
    PACKED_TEXT_DIRECTIVE_ID, PACKED_TEXT_HEAD_COUNT, PACKED_TEXT_PROGRAM_BYTES,
    PACKED_TEXT_PROGRAM_HEADER, PARSER_VM_MACRO_VERSION, PARSER_VM_PACKED_TEXT_ENTRY,
};

pub const HEADER_KIND: u16 = 21;
pub const OPERAND_KIND: u16 = 22;
pub const ACTION: usize = 2;
pub const PREFIX_BYTES: usize = 4;
pub const OPERAND_COUNT: usize = 8;
pub const OPERAND_SLOT: usize = 2;
pub const OPERAND_TYPE: usize = 4;
pub const OPERAND_OFFSET: usize = 8;
pub const OPERAND_BYTES: usize = 12;
pub const NAME: u16 = 1;
pub const STRING: u16 = 2;
pub const EXPRESSION: u16 = 3;
pub const SELECT: u16 = 1;
pub const BEGIN: u16 = 2;
pub const END: u16 = 3;
pub const RANGE: u16 = 4;
pub const TABLE: u16 = 5;
pub const ESCAPE: u16 = 6;
pub const TEXT: u16 = 7;
pub const NULL: u16 = 8;
pub const PTEXT: u16 = 9;
pub const MAX_DELIMITER_DEPTH: usize = 16;
fn error(status: u32, offset: usize, message: &'static str) -> DescriptorError {
    DescriptorError {
        status,
        offset: offset as u32,
        message,
    }
}
fn put(row: &mut [u8; 32], at: usize, value: usize) {
    row[at..at + 4].copy_from_slice(&(value as u32).to_be_bytes());
}
fn descriptor(program: &[u8]) -> Result<[u16; PACKED_TEXT_HEAD_COUNT], DescriptorError> {
    if program.len() != PACKED_TEXT_PROGRAM_BYTES {
        return Err(error(6, 0, "Invalid packed text program"));
    }
    let heads: [u16; PACKED_TEXT_HEAD_COUNT] = std::array::from_fn(|i| {
        let at =
            PACKED_TEXT_PROGRAM_HEADER + i * PACKED_TEXT_DIRECTIVE_BYTES + PACKED_TEXT_DIRECTIVE_ID;
        u16::from_be_bytes([program[at], program[at + 1]])
    });
    if program != packed_text_program(heads)
        || heads.contains(&0)
        || (0..heads.len()).any(|i| heads[..i].contains(&heads[i]))
    {
        return Err(error(6, 0, "Invalid packed text program"));
    }
    Ok(heads)
}

/// Tokenizer-boundary contextual role for the next raw token. The prefix is
/// already emitted compact source; its length byte is not yet finalized.
/// No caller needs to recognize text directive or comma syntax. A zero result
/// leaves ordinary lexical binding intact; 1/2 select global encoding names.
pub fn name_role(program: &[u8], prefix: &[u8]) -> Result<u16, DescriptorError> {
    let heads = descriptor(program)?;
    if prefix.len() < RECORD_BYTES + DOT_BYTES || prefix.len() > MAX_RECORD_BYTES {
        return Ok(0);
    }
    let head = RECORD_BYTES + label_prefix(prefix, true);
    let Some(id) = directive_id(prefix, head) else {
        return Ok(0);
    };
    let Some(index) = heads.iter().position(|value| *value == id) else {
        return Ok(0);
    };
    let action = u16::from(
        program[PACKED_TEXT_PROGRAM_HEADER
            + index * PACKED_TEXT_DIRECTIVE_BYTES
            + PACKED_TEXT_DIRECTIVE_ACTION],
    );
    if ![SELECT, BEGIN].contains(&action) {
        return Ok(0);
    }
    let tail = &prefix[head + DOT_BYTES..];
    if tail.is_empty() {
        return Ok(1);
    }
    if action == BEGIN
        && tail.len() == NAME_BYTES + 1
        && canonical_name(&tail[..NAME_BYTES])
        && tail[NAME_BYTES] == TOKEN_COMMA
    {
        return Ok(2);
    }
    Ok(0)
}
fn label_prefix(source: &[u8], allow_unmarked: bool) -> usize {
    if source.len() < RECORD_BYTES + LABEL_BYTES
        || source[RECORD_BYTES + NAME_KIND] > TOKEN_REGISTER
    {
        return 0;
    }
    if source[RECORD_BYTES + LABEL_COLON] == TOKEN_COLON {
        LABEL_BYTES
    } else if allow_unmarked
        && source.len() >= RECORD_BYTES + NAME_BYTES + DOT_BYTES
        && source[RECORD_FLAGS] & FLAG_INDENT == 0
    {
        NAME_BYTES
    } else {
        0
    }
}
fn directive_id(source: &[u8], head: usize) -> Option<u16> {
    let dot = source.get(head..head + DOT_BYTES)?;
    if dot[DOT_KIND] != TOKEN_DOT || !canonical_name(&dot[DOT_NAME..]) {
        return None;
    }
    Some(u16::from_be_bytes([
        dot[DOT_NAME + NAME_ID],
        dot[DOT_NAME + NAME_ID + 1],
    ]))
}

/// Publish a header and typed operand spans atomically. String spans include
/// their token header; NAME spans contain canonical unqualified numeric IDs.
/// Expressions remain opaque to this service after bounded envelope validation.
pub fn execute(
    entry: u16,
    version: u16,
    program: &[u8],
    source: &[u8],
    capacity: usize,
    mut steps: usize,
) -> Result<Vec<[u8; 32]>, DescriptorError> {
    if entry != PARSER_VM_PACKED_TEXT_ENTRY || version != PARSER_VM_MACRO_VERSION {
        return Err(error(4, 0, "Invalid packed text entry or version"));
    }
    let heads = descriptor(program)?;
    if source.len() < RECORD_BYTES
        || source.len() > MAX_RECORD_BYTES
        || usize::from(source[RECORD_LENGTH]) + 1 != source.len()
        || source[RECORD_FLAGS] & !FLAG_ALLOWED != 0
    {
        return Err(error(4, 0, "Invalid packed record extent or flags"));
    }
    let mut tick = |at| {
        steps = steps
            .checked_sub(1)
            .ok_or(error(12, at, "Step budget exceeded"))?;
        Ok::<_, DescriptorError>(())
    };
    tick(0)?;
    if source[RECORD_FLAGS] & FLAG_INACTIVE != 0 || source.len() == RECORD_BYTES {
        return Ok(vec![]);
    }
    let prefix = label_prefix(source, false);
    let head = RECORD_BYTES + prefix;
    tick(head)?;
    let Some(id) = directive_id(source, head) else {
        return Ok(vec![]);
    };
    let Some(index) = heads.iter().position(|value| *value == id) else {
        return Ok(vec![]);
    };
    if prefix != 0 && source[RECORD_BYTES + NAME_QUALIFIER] != 0 {
        return Err(error(
            5,
            RECORD_BYTES + NAME_QUALIFIER,
            "Qualified text label",
        ));
    }
    let action = u16::from(
        program[PACKED_TEXT_PROGRAM_HEADER
            + index * PACKED_TEXT_DIRECTIVE_BYTES
            + PACKED_TEXT_DIRECTIVE_ACTION],
    );
    let mut spans = Vec::new();
    let mut stack = Vec::new();
    let mut start = head + DOT_BYTES;
    let mut p = start;
    while p < source.len() {
        tick(p)?;
        let kind = source[p];
        let bytes = match kind {
            TOKEN_IDENTIFIER | TOKEN_REGISTER => NAME_BYTES,
            TOKEN_NUMBER => NUMBER_BYTES,
            TOKEN_WIDE_NUMBER => WIDE_NUMBER_BYTES,
            TOKEN_STRING | TOKEN_COMPILED_EXPRESSION | TOKEN_TYPED_EXPRESSION => {
                let n = usize::from(*source.get(p + STRING_LENGTH).ok_or(error(
                    5,
                    p,
                    "Truncated text token",
                ))?);
                if kind != TOKEN_STRING && n == 0 {
                    return Err(error(5, p, "Empty compiled expression"));
                }
                STRING_HEADER_BYTES + n
            }
            TOKEN_COMMA if stack.is_empty() => {
                if start == p {
                    return Err(error(5, p, "Empty text operand"));
                }
                spans.push((start, p - start));
                start = p + 1;
                1
            }
            TOKEN_OPEN_BRACKET | TOKEN_OPEN_BRACE | TOKEN_OPEN_PAREN => {
                if stack.len() == MAX_DELIMITER_DEPTH {
                    return Err(error(5, p, "Text delimiter depth exceeded"));
                }
                stack.push(kind + 1);
                1
            }
            TOKEN_CLOSE_BRACKET | TOKEN_CLOSE_BRACE | TOKEN_CLOSE_PAREN => {
                if stack.pop() != Some(kind) {
                    return Err(error(5, p, "Mismatched text delimiter"));
                }
                1
            }
            TOKEN_COMMA
            | TOKEN_EQUAL
            | TOKEN_DOT
            | TOKEN_OPERATOR
            | TOKEN_OPERATOR_FIRST..=TOKEN_OPERATOR_LAST => 1,
            _ => return Err(error(5, p, "Invalid text token")),
        };
        if p + bytes > source.len() {
            return Err(error(5, p, "Truncated text token"));
        }
        p += bytes;
    }
    if !stack.is_empty() {
        return Err(error(5, p, "Unclosed text delimiter"));
    }
    if start < p {
        spans.push((start, p - start));
    } else if start != head + DOT_BYTES {
        return Err(error(5, p, "Empty text operand"));
    }
    let count = spans.len();
    let valid_count = match action {
        SELECT | NULL | PTEXT => count == 1,
        BEGIN => (1..=2).contains(&count),
        END => count == 0,
        RANGE => count == 3,
        TABLE | ESCAPE => count >= 2,
        TEXT => count >= 1,
        _ => false,
    };
    if !valid_count {
        return Err(error(5, p, "Invalid text operand count"));
    }
    let mut rows = Vec::with_capacity(count + 1);
    let mut header = [0; 32];
    header[..2].copy_from_slice(&HEADER_KIND.to_be_bytes());
    header[ACTION..ACTION + 2].copy_from_slice(&action.to_be_bytes());
    put(&mut header, PREFIX_BYTES, prefix);
    put(&mut header, OPERAND_COUNT, count);
    rows.push(header);
    for (slot, (offset, bytes)) in spans.into_iter().enumerate() {
        tick(offset)?;
        let literal = source[offset] == TOKEN_STRING
            && bytes == usize::from(source[offset + STRING_LENGTH]) + STRING_HEADER_BYTES;
        let ty = match action {
            SELECT | BEGIN => {
                if !canonical_name(&source[offset..offset + bytes]) {
                    return Err(error(5, offset, "Text name requires canonical numeric ID"));
                }
                NAME
            }
            TEXT | NULL | PTEXT => {
                if !literal {
                    return Err(error(5, offset, "Text operand requires literal string"));
                }
                STRING
            }
            TABLE | ESCAPE if slot == 0 => {
                if !literal {
                    return Err(error(5, offset, "Text definition requires literal string"));
                }
                STRING
            }
            RANGE if slot < 2 && literal => {
                if bytes != STRING_HEADER_BYTES + 1 {
                    return Err(error(5, offset, "Text range character requires one byte"));
                }
                STRING
            }
            ESCAPE if literal => STRING,
            _ => EXPRESSION,
        };
        let mut row = [0; 32];
        row[..2].copy_from_slice(&OPERAND_KIND.to_be_bytes());
        row[OPERAND_SLOT..OPERAND_SLOT + 2].copy_from_slice(&(slot as u16).to_be_bytes());
        row[OPERAND_TYPE..OPERAND_TYPE + 2].copy_from_slice(&ty.to_be_bytes());
        put(&mut row, OPERAND_OFFSET, offset);
        put(&mut row, OPERAND_BYTES, bytes);
        rows.push(row);
    }
    tick(p)?;
    if capacity < rows.len() * 32 {
        return Err(error(7, p, "Text result capacity exceeded"));
    }
    tick(p)?;
    Ok(rows)
}

#[cfg(test)]
mod tests {
    use super::*;
    const HEADS: [u16; 10] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    fn record(head: u16, operands: &[u8]) -> Vec<u8> {
        let mut source = vec![0, 1, 0, 1, 7, 0];
        source.extend(head.to_be_bytes());
        source.push(0);
        source.extend(operands);
        source[0] = (source.len() - 1) as u8;
        source
    }
    fn run(
        source: &[u8],
        capacity: usize,
        budget: usize,
    ) -> Result<Vec<[u8; 32]>, DescriptorError> {
        execute(
            PARSER_VM_PACKED_TEXT_ENTRY,
            PARSER_VM_MACRO_VERSION,
            &packed_text_program(HEADS),
            source,
            capacity,
            budget,
        )
    }
    fn field(row: &[u8; 32], at: usize) -> u32 {
        u32::from_be_bytes(row[at..at + 4].try_into().unwrap())
    }
    #[test]
    fn tokenizer_name_roles_share_descriptor_policy() {
        let program = packed_text_program(HEADS);
        assert_eq!(name_role(&program, &record(1, &[])).unwrap(), 1);
        assert_eq!(name_role(&program, &record(3, &[])).unwrap(), 1);
        assert_eq!(
            name_role(&program, &record(3, &[0, 0, 20, 0, 4])).unwrap(),
            2
        );
        assert_eq!(
            name_role(&program, &record(1, &[0, 0, 20, 0, 4])).unwrap(),
            0
        );
        assert_eq!(
            name_role(&program, &record(3, &[0, 0, 20, 1, 4])).unwrap(),
            0
        );
        assert_eq!(name_role(&program, &record(8, &[])).unwrap(), 0);
        let ordinary = record(3, &[]);
        for colon in [false, true] {
            for flags in [
                0,
                FLAG_PLAN,
                FLAG_MUTABLE_SNAPSHOT,
                FLAG_PLAN | FLAG_MUTABLE_SNAPSHOT,
            ] {
                let mut prefix = vec![0, flags, 0, 1, TOKEN_IDENTIFIER, 0, 40, 0];
                if colon {
                    prefix.push(TOKEN_COLON);
                }
                prefix.extend(&ordinary[RECORD_BYTES..]);
                assert_eq!(name_role(&program, &prefix).unwrap(), 1);
                prefix[RECORD_FLAGS] |= FLAG_INDENT;
                assert_eq!(
                    name_role(&program, &prefix).unwrap(),
                    if colon { 1 } else { 0 }
                );
            }
        }
    }
    #[test]
    fn typed_spans_preserve_decoded_string_payloads() {
        // Delimiter-looking bytes within a string are data, never grammar.
        let source = record(7, &[3, 3, 4, 10, 15, 4, 3, 0, 4, 2, 0, 0, 0, 9]);
        let rows = run(&source, 128, 100).unwrap();
        assert_eq!(field(&rows[0], OPERAND_COUNT), 3);
        assert_eq!(rows[1][OPERAND_TYPE + 1], STRING as u8);
        assert_eq!(rows[2][OPERAND_TYPE + 1], STRING as u8);
        assert_eq!(rows[3][OPERAND_TYPE + 1], EXPRESSION as u8);
        assert_eq!(field(&rows[2], OPERAND_BYTES), 2); // Empty replacement is legal.
        assert_eq!(field(&rows[3], OPERAND_OFFSET), 18);
    }
    #[test]
    fn every_directive_count_and_literal_policy() {
        let name = [0, 0, 20, 0];
        let string = [3, 1, b'A'];
        let number = [2, 0, 0, 0, 1];
        for head in [1, 2, 3] {
            assert!(run(&record(head, &name), 96, 100).is_ok());
        }
        assert!(run(&record(4, &[]), 32, 100).is_ok());
        for head in [8, 9, 10] {
            assert!(run(&record(head, &string), 64, 100).is_ok());
            assert_eq!(run(&record(head, &number), 64, 100).unwrap_err().status, 5);
            assert_eq!(run(&record(head, &[]), 64, 100).unwrap_err().status, 5);
        }
        let mut range = string.to_vec();
        range.push(4);
        range.extend(string);
        range.push(4);
        range.extend(number);
        assert!(run(&record(5, &range), 128, 100).is_ok());
        let mut table = string.to_vec();
        table.push(4);
        table.extend(number);
        assert!(run(&record(6, &table), 96, 100).is_ok());
        assert_eq!(run(&record(5, &table), 96, 100).unwrap_err().status, 5);
        assert_eq!(run(&record(1, &string), 64, 100).unwrap_err().status, 5);
        assert_eq!(
            run(&record(1, &[0, 0, 20, 1]), 64, 100).unwrap_err().status,
            5
        );
    }
    #[test]
    fn current_record_flags_preserve_activity_policy() {
        let ordinary = record(8, &[TOKEN_STRING, 0]);
        let expected = run(&ordinary, 64, 100).unwrap();
        for flag in [
            FLAG_MUTABLE_SNAPSHOT,
            FLAG_PLAN,
            FLAG_BLOCK_OPEN,
            FLAG_BLOCK_CLOSE,
        ] {
            let mut source = ordinary.clone();
            source[RECORD_FLAGS] |= flag;
            assert_eq!(run(&source, 64, 100).unwrap(), expected);
        }
        for flag in [FLAG_OMIT, FLAG_LAYOUT] {
            let mut source = record(8, &[255]);
            source[RECORD_FLAGS] |= flag;
            assert!(run(&source, 0, 1).unwrap().is_empty());
        }
        let mut source = ordinary;
        source[RECORD_FLAGS] |= !FLAG_ALLOWED;
        assert_eq!(run(&source, 64, 100).unwrap_err().status, 4);
    }
    #[test]
    fn envelopes_capacity_and_budget_are_atomic() {
        let source = record(8, &[3, 0]);
        assert_eq!(run(&source, 63, 100).unwrap_err().status, 7);
        assert_eq!(run(&source, 64, 0).unwrap_err().status, 12);
        for truncated in [
            &[3][..],
            &[3, 2, b'A'][..],
            &[129, 0][..],
            &[14, 11][..],
            &[14][..],
            &[4][..],
            &[3, 0, 4][..],
        ] {
            assert_eq!(run(&record(8, truncated), 512, 100).unwrap_err().status, 5);
        }
        let mut labeled = vec![0, 1, 0, 1, 0, 0, 40, 0, 5];
        labeled.extend(&source[4..]);
        labeled[0] = (labeled.len() - 1) as u8;
        let rows = run(&labeled, 64, 100).unwrap();
        assert_eq!(field(&rows[0], PREFIX_BYTES), 5);
        assert_eq!(field(&rows[1], OPERAND_OFFSET), 14);
        labeled[7] = 1;
        assert_eq!(run(&labeled, 64, 100).unwrap_err().status, 5);
        let mut inactive = source.clone();
        inactive[1] |= 8;
        assert!(run(&inactive, 0, 1).unwrap().is_empty());
        assert!(run(&record(99, &[255]), 0, 2).unwrap().is_empty());
        let mut program = packed_text_program(HEADS);
        program[4] = 9;
        assert_eq!(
            execute(10, PARSER_VM_MACRO_VERSION, &program, &source, 64, 100)
                .unwrap_err()
                .status,
            6
        );
    }
}
