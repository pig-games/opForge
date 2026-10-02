// SPDX-License-Identifier: GPL-3.0-or-later
//! Shared initial-target selection before any target package has been acquired.
//! Input is unbound TKVM rows and decoded lexemes, never source text. Discovery
//! stops at the first statement outside the unconditional module/CPU preamble.
use crate::macro_descriptor_vm::DescriptorError;
use package::package::{PARSER_VM_MACRO_VERSION, PARSER_VM_TARGET_BOOTSTRAP_ENTRY};

pub const RESULT_KIND: u16 = 20;
pub const SELECT: u16 = 1;
pub const STOP: u16 = 2;

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub kind: u16,
    pub offset: u32,
    pub bytes: u32,
}

fn error(status: u32, offset: usize, message: &'static str) -> DescriptorError {
    DescriptorError {
        status,
        offset: offset as u32,
        message,
    }
}

/// Return at most one 32-byte event. Offsets are relative to `lexemes`;
/// no output is published if validation, budget or result capacity fails.
pub fn execute(
    entry: u16,
    version: u16,
    program: &[u8],
    tokens: &[Token],
    lexemes: &[u8],
    capacity: usize,
    mut steps: usize,
) -> Result<Vec<[u8; 32]>, DescriptorError> {
    if entry != PARSER_VM_TARGET_BOOTSTRAP_ENTRY || version != PARSER_VM_MACRO_VERSION {
        return Err(error(4, 0, "Invalid bootstrap entry or version"));
    }
    if program.len() < 4 || program[0] != 0x97 || program[1] == 0 {
        return Err(error(6, 0, "Invalid bootstrap program"));
    }
    let mut cursor = 2;
    let mut rows = Vec::new();
    for _ in 0..program[1] {
        let row =
            program
                .get(cursor..cursor + 2)
                .ok_or(error(6, cursor, "Truncated bootstrap row"))?;
        let role = row[0];
        let len = usize::from(row[1]);
        cursor += 2;
        let name = program.get(cursor..cursor + len).ok_or(error(
            6,
            cursor,
            "Truncated bootstrap name",
        ))?;
        if !(1..=2).contains(&role) || len == 0 || !name.iter().all(u8::is_ascii_alphabetic) {
            return Err(error(6, cursor, "Invalid bootstrap policy"));
        }
        rows.push((role, name));
        cursor += len;
    }
    if program.get(cursor..) != Some(&[0x83, 0][..]) {
        return Err(error(6, cursor, "Invalid bootstrap terminator"));
    }
    let mut tick = || {
        steps = steps
            .checked_sub(1)
            .ok_or(error(12, 0, "Bootstrap step budget exceeded"))?;
        Ok::<_, DescriptorError>(())
    };
    for token in tokens {
        let start = token.offset as usize;
        let len = token.bytes as usize;
        if token.kind > 40 || start > lexemes.len() || len > lexemes.len() - start {
            return Err(error(4, start, "Invalid bootstrap token span"));
        }
    }
    tick()?;
    if tokens.is_empty() {
        return Ok(Vec::new());
    }
    let mut action = STOP;
    let mut selected = None;
    if tokens.len() >= 2 && tokens[0].kind == 7 && tokens[1].kind == 0 {
        let head = tokens[1];
        let name = &lexemes[head.offset as usize..head.offset as usize + head.bytes as usize];
        for (role, spelling) in rows {
            tick()?;
            if !name.eq_ignore_ascii_case(spelling) {
                continue;
            }
            if tokens.len() != 3 {
                return Err(error(5, 2, "Bootstrap declaration requires one name"));
            }
            let operand = tokens[2];
            if !matches!(operand.kind, 0 | 2 | 3) || operand.bytes == 0 || operand.bytes > 255 {
                return Err(error(5, 2, "Invalid bootstrap declaration name"));
            }
            let bytes =
                &lexemes[operand.offset as usize..operand.offset as usize + operand.bytes as usize];
            if bytes.contains(&0) {
                return Err(error(5, 2, "Embedded NUL in bootstrap name"));
            }
            if role == 1 {
                if operand.kind != 0 {
                    return Err(error(5, 2, "Module name must be an identifier"));
                }
                return Ok(Vec::new());
            }
            action = SELECT;
            selected = Some(operand);
            break;
        }
    }
    tick()?;
    if capacity < 32 {
        return Err(error(7, 0, "Bootstrap result capacity exceeded"));
    }
    let mut row = [0; 32];
    row[..2].copy_from_slice(&RESULT_KIND.to_be_bytes());
    row[2..4].copy_from_slice(&action.to_be_bytes());
    if let Some(token) = selected {
        row[4..8].copy_from_slice(&token.offset.to_be_bytes());
        row[8..12].copy_from_slice(&token.bytes.to_be_bytes());
    }
    Ok(vec![row])
}

#[cfg(test)]
mod tests {
    use super::*;
    use package::package::target_bootstrap_program;
    fn run(tokens: &[Token], bytes: &[u8]) -> Result<Vec<[u8; 32]>, DescriptorError> {
        execute(8, 2, &target_bootstrap_program(), tokens, bytes, 32, 32)
    }
    fn name(kind: u16, offset: u32, bytes: u32) -> Token {
        Token {
            kind,
            offset,
            bytes,
        }
    }
    #[test]
    fn preserves_numeric_alias_and_decoded_quoted_name() {
        for kind in [0, 2, 3] {
            let rows = run(
                &[name(7, 0, 1), name(0, 1, 3), name(kind, 4, 4)],
                b".CpU6502",
            )
            .unwrap();
            assert_eq!(&rows[0][..12], &[0, 20, 0, 1, 0, 0, 0, 4, 0, 0, 0, 4]);
        }
    }
    #[test]
    fn empty_and_module_preamble_continue_but_other_statements_stop() {
        assert!(run(&[], b"").unwrap().is_empty());
        assert!(run(
            &[name(7, 0, 1), name(0, 1, 6), name(0, 7, 7)],
            b".moduleapp.foo"
        )
        .unwrap()
        .is_empty());
        for head in [b"if".as_slice(), b"include", b"macro", b"org"] {
            let mut bytes = vec![b'.'];
            bytes.extend(head);
            let rows = run(&[name(7, 0, 1), name(0, 1, head.len() as u32)], &bytes).unwrap();
            assert_eq!(&rows[0][..4], &[0, 20, 0, 2]);
        }
    }
    #[test]
    fn policy_operands_control_recognition() {
        let mut program = target_bootstrap_program();
        let cpu = program.windows(3).position(|v| v == b"cpu").unwrap();
        program[cpu..cpu + 3].copy_from_slice(b"isa");
        let tokens = [name(7, 0, 1), name(0, 1, 3), name(2, 4, 4)];
        assert_eq!(
            &execute(8, 2, &program, &tokens, b".isa6502", 32, 32).unwrap()[0][..4],
            &[0, 20, 0, 1]
        );
        assert_eq!(
            &execute(8, 2, &program, &tokens, b".cpu6502", 32, 32).unwrap()[0][..4],
            &[0, 20, 0, 2]
        );
    }
    #[test]
    fn rejects_bad_program_spans_names_and_budgets_atomically() {
        assert_eq!(
            run(&[name(7, 0, 1), name(0, 1, 3)], b".cpu")
                .unwrap_err()
                .status,
            5
        );
        assert_eq!(run(&[name(0, u32::MAX, 1)], b"").unwrap_err().status, 4);
        let tokens = [name(7, 0, 1), name(0, 1, 3), name(3, 4, 1)];
        assert_eq!(run(&tokens, b".cpu\0").unwrap_err().status, 5);
        assert_eq!(
            execute(8, 2, &target_bootstrap_program(), &[], b"", 32, 0)
                .unwrap_err()
                .status,
            12
        );
        assert_eq!(
            execute(8, 2, &target_bootstrap_program(), &tokens, b".cpuX", 0, 32)
                .unwrap_err()
                .status,
            7
        );
        let mut program = target_bootstrap_program();
        program.pop();
        assert_eq!(
            execute(8, 2, &program, &[], b"", 32, 32)
                .unwrap_err()
                .status,
            6
        );
    }
}
