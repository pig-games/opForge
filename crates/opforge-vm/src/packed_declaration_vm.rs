// SPDX-License-Identifier: GPL-3.0-or-later
//! Shared scalar declaration envelope over packed source records.
use crate::macro_descriptor_vm::DescriptorError;
use package::package::{PARSER_VM_MACRO_VERSION, PARSER_VM_PACKED_DECLARATION_ENTRY};
pub const RESULT_KIND: u16 = 20;
pub const ROLE_OFFSET: usize = 2;
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
    if entry != PARSER_VM_PACKED_DECLARATION_ENTRY || version != PARSER_VM_MACRO_VERSION {
        return Err(error(4, 0, "Invalid packed declaration entry or version"));
    }
    if program.len() != 23
        || program[..2] != [0x98, 3]
        || program[11..] != [2, 2, 2, 5, 34, 3, 3, 5, 9, 34, 0x83, 0]
        || program[2..11]
            .chunks_exact(3)
            .any(|row| !(1..=2).contains(&row[2]))
        || (0..3).any(|index| {
            (0..index).any(|other| {
                program[2 + index * 3..4 + index * 3] == program[2 + other * 3..4 + other * 3]
            })
        })
    {
        return Err(error(6, 0, "Invalid packed declaration program"));
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
    if source[1] & (8 | 16) != 0 || source.len() < 10 || source[4] > 1 {
        return Ok(vec![]);
    }
    let (role, match_head, head) = if source.get(8..11) == Some(&program[18..21]) {
        (program[16], 8, 11)
    } else if source.get(8..10) == Some(&program[14..16]) {
        (program[12], 8, 10)
    } else {
        let mut head = 8;
        if source.get(head) == Some(&5) {
            head += 1;
        }
        if source.len() - head < 5
            || source[head] != 7
            || source[head + 1] > 1
            || source[head + 4] != 0
        {
            return Ok(vec![]);
        }
        let Some(role) = program[2..11]
            .chunks_exact(3)
            .find_map(|row| (source[head + 2..head + 4] == row[..2]).then_some(row[2]))
        else {
            return Ok(vec![]);
        };
        (role, head, head + 5)
    };
    if steps < 2 {
        return Err(error(12, match_head, "Step budget exceeded"));
    }
    if source[7] != 0 {
        return Err(error(5, 7, "Qualified scalar declaration"));
    }
    if capacity < 32 {
        return Err(error(7, head, "Declaration result capacity exceeded"));
    }
    if steps < 3 {
        return Err(error(12, head, "Step budget exceeded"));
    }
    let mut row = [0; 32];
    row[ROLE_OFFSET..ROLE_OFFSET + 2].copy_from_slice(&u16::from(role).to_be_bytes());
    row[..2].copy_from_slice(&RESULT_KIND.to_be_bytes());
    row[VALUE_OFFSET..VALUE_OFFSET + 4].copy_from_slice(&(head as u32).to_be_bytes());
    row[VALUE_BYTES..VALUE_BYTES + 4]
        .copy_from_slice(&((source.len() - head) as u32).to_be_bytes());
    Ok(vec![row])
}
#[cfg(test)]
mod tests {
    use super::*;
    use package::package::packed_declaration_program;
    fn run(
        source: &[u8],
        capacity: usize,
        budget: usize,
    ) -> Result<Vec<[u8; 32]>, DescriptorError> {
        execute(
            9,
            2,
            &packed_declaration_program([42, 43, 44]),
            source,
            capacity,
            budget,
        )
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
    fn assignment(value: &[u8]) -> Vec<u8> {
        let mut source = vec![0, 1, 0, 7, 0, 0, 90, 0, 5, 34];
        source.extend(value);
        source[0] = (source.len() - 1) as u8;
        source
    }
    #[test]
    fn colon_equal_returns_mutable_scalar_spans() {
        for value in [&[2, 0, 0, 0, 3][..], &[][..]] {
            let rows = run(&assignment(value), 32, 3).unwrap();
            assert_eq!(u16::from_be_bytes(rows[0][2..4].try_into().unwrap()), 2);
            assert_eq!(u32::from_be_bytes(rows[0][4..8].try_into().unwrap()), 10);
            assert_eq!(
                u32::from_be_bytes(rows[0][8..12].try_into().unwrap()) as usize,
                value.len()
            );
        }
    }
    fn question_kind() -> u8 {
        // Derive the wire kind through TKVM decoding, rather than mistaking an
        // EXVM or operator enum value for the packed punctuation identity.
        let kind = (0..=40)
            .find(|kind| {
                crate::tokenizer_runtime_utils::vm_build_token(*kind, b"?", 1, 0, 1, 0).is_ok_and(
                    |token| {
                        matches!(
                            token.kind,
                            crate::portable_contract::PortableTokenKind::Question
                        )
                    },
                )
            })
            .unwrap();
        let native = include_str!("../../../native/motorola68000/amigaos/tkvm/tkvm_runtime.asm");
        let declared = native
            .lines()
            .find(|line| line.starts_with("TK_KIND_QUESTION "))
            .unwrap()
            .split('=')
            .nth(1)
            .unwrap()
            .trim()
            .parse::<u8>()
            .unwrap();
        assert_eq!(
            kind, declared,
            "Rust and native TKVM question kind must agree"
        );
        kind
    }
    #[test]
    fn conditional_assignment_preserves_operand_spans_and_resources() {
        for value in [&[2, 0, 0, 0, 3][..], &[][..]] {
            let mut source = assignment(value);
            source.insert(9, question_kind());
            source[0] += 1;
            let rows = run(&source, 32, 3).unwrap();
            assert_eq!(u16::from_be_bytes(rows[0][2..4].try_into().unwrap()), 3);
            assert_eq!(u32::from_be_bytes(rows[0][4..8].try_into().unwrap()), 11);
            assert_eq!(
                u32::from_be_bytes(rows[0][8..12].try_into().unwrap()) as usize,
                value.len()
            );
            assert_eq!(run(&source, 31, 3).unwrap_err().status, 7);
            assert_eq!(run(&source, 32, 2).unwrap_err().status, 12);
        }
    }
    #[test]
    fn declarations_accept_arbitrary_source_line_numbers() {
        for mut source in [assignment(&[]), record(false, &[]), record(true, &[])] {
            for line in [1u16, 256, 65535] {
                source[2..4].copy_from_slice(&line.to_be_bytes());
                assert_eq!(run(&source, 32, 3).unwrap().len(), 1);
            }
        }
    }
    #[test]
    fn colon_equal_near_misses_remain_unmatched() {
        for operator in [&[34][..], &[5][..], &[5, 35][..], &[34, 5][..]] {
            let mut source = assignment(&[]);
            source.truncate(8);
            source.extend(operator);
            source[0] = (source.len() - 1) as u8;
            assert!(run(&source, 0, 1).unwrap().is_empty());
        }
    }
    #[test]
    fn colon_equal_qualification_and_resource_errors_are_atomic() {
        let mut source = assignment(&[2, 0, 0, 0, 3]);
        for budget in 0..3 {
            assert_eq!(run(&source, 32, budget).unwrap_err().status, 12);
        }
        assert_eq!(run(&source, 31, 3).unwrap_err().status, 7);
        source[7] = 1;
        assert_eq!(run(&source, 32, 3).unwrap_err().status, 5);
        source[7] = 0;
        for flags in [8, 16] {
            source[1] = flags;
            assert!(run(&source, 0, 1).unwrap().is_empty());
        }
    }
    #[test]
    fn roles_and_program_rejections_are_shared_and_atomic() {
        for (id, role) in [(42, 1u16), (43, 2), (44, 2)] {
            let mut source = record(false, &[2, 0, 0, 0, 1]);
            source[11] = id;
            let rows = run(&source, 32, 3).unwrap();
            assert_eq!(u16::from_be_bytes(rows[0][2..4].try_into().unwrap()), role);
        }
        let source = record(false, &[2, 0, 0, 0, 1]);
        let valid = packed_declaration_program([42, 43, 44]);
        for program in [
            valid[..16].to_vec(),
            valid[..11].iter().copied().chain([0x83, 0]).collect(),
            {
                let mut p = valid.clone();
                p[15] = 35;
                p
            },
            packed_declaration_program([42, 42, 44]),
            {
                let mut p = valid.clone();
                p[4] = 3;
                p
            },
        ] {
            assert_eq!(
                execute(9, 2, &program, &source, 32, 3).unwrap_err().status,
                6
            );
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
        r[11] = 45;
        assert!(run(&r, 0, 1).unwrap().is_empty());
        r[11] = 42;
        r[1] = 8;
        assert!(run(&r, 0, 1).unwrap().is_empty());
        r[1] = 1;
        r[0] -= 1;
        assert_eq!(run(&r, 32, 3).unwrap_err().status, 4);
    }
}
