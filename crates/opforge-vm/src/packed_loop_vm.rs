// SPDX-License-Identifier: GPL-3.0-or-later
//! Package-owned loop header grammar over immutable packed writer records.
use package::package::MacroDescriptorOpcode;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoopHeader {
    Matched {
        variable: Option<u16>,
        operand_offset: usize,
    },
    While {
        operand_offset: usize,
    },
    ScopedFor {
        variable: Option<u16>,
        operand_offset: usize,
    },
    ScopedWhile {
        operand_offset: usize,
    },
    Endfor,
    Endwhile,
    Ordinary,
    Malformed,
}
fn valid_program(program: &[u8]) -> bool {
    program.len() == 17
        && program[0] == MacroDescriptorOpcode::LoopHeader as u8
        && program[15..]
            == [
                MacroDescriptorOpcode::Publish as u8,
                MacroDescriptorOpcode::End as u8,
            ]
}

/// Bind a grammar name while the writer has emitted `.for <bare variable>`.
/// Its length byte is not final; the slice supplies the current packed extent.
pub fn context_keyword(program: &[u8], prefix: &[u8], candidate: u16) -> bool {
    loop_prefix(program, prefix)
        && prefix.len() == 13
        && prefix[9] <= 1
        && prefix[12] == 0
        && candidate.to_be_bytes() == program[3..5]
}

fn loop_prefix(program: &[u8], prefix: &[u8]) -> bool {
    valid_program(program)
        && (9..=256).contains(&prefix.len())
        && prefix[1] & 0xd8 == 0
        && prefix[4] == 7
        && prefix[5] <= 1
        && prefix[6..8] == program[1..3]
        && prefix[8] == 0
}

/// Shared directives and assignment operands may shadow package register names;
/// instruction register roles retain their package identity.
pub fn value_context(prefix: &[u8]) -> bool {
    prefix.len() >= 9
        && (prefix[4] == 7
            || (prefix[4] <= 1 && (prefix[8] == 34 || (prefix[8] == 7 && prefix.len() >= 13))))
}

/// Classify the envelope only. EXVM owns the complete expression grammar.
pub fn execute(program: &[u8], source: &[u8]) -> LoopHeader {
    use LoopHeader::*;
    if !valid_program(program)
        || source.len() < 4
        || source.len() > 256
        || usize::from(source[0]) + 1 != source.len()
        || source[1] & !63 != 0
    {
        return Malformed;
    }
    if source[1] & 24 != 0 || source.len() < 9 || source[4] != 7 || source[5] > 1 {
        return Ordinary;
    }
    let head = &source[6..8];
    let role = if head == &program[1..3] {
        0
    } else if head == &program[5..7] {
        1
    } else if head == &program[7..9] {
        2
    } else if head == &program[9..11] {
        3
    } else if head == &program[11..13] {
        4
    } else if head == &program[13..15] {
        5
    } else {
        return Ordinary;
    };
    if source[8] != 0 {
        return Malformed;
    }
    if role == 1 || role == 3 {
        return if source.len() != 9 {
            Malformed
        } else if role == 1 {
            Endfor
        } else {
            Endwhile
        };
    }
    if source.len() == 9 {
        return Malformed;
    }
    if role == 2 || role == 5 {
        return if role == 2 {
            While { operand_offset: 9 }
        } else {
            ScopedWhile { operand_offset: 9 }
        };
    }
    // Only two consecutive bare name tokens establish the iterable form.
    // Other expressions, including a single name, go to counted EXVM.
    if source.len() >= 17 && source[9] <= 1 && source[13] <= 1 && source[14..16] == program[3..5] {
        if source[12] != 0 || source[16] != 0 || source.len() == 17 {
            return Malformed;
        }
        let variable = Some(u16::from_be_bytes([source[10], source[11]]));
        return if role == 4 {
            ScopedFor {
                variable,
                operand_offset: 17,
            }
        } else {
            Matched {
                variable,
                operand_offset: 17,
            }
        };
    }
    if role == 4 {
        ScopedFor {
            variable: None,
            operand_offset: 9,
        }
    } else {
        Matched {
            variable: None,
            operand_offset: 9,
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use package::package::packed_loop_program;
    fn record(operand: &[u8]) -> Vec<u8> {
        let mut record = vec![0, 0, 0, 1, 7, 0, 0, 42, 0];
        record.extend_from_slice(operand);
        record[0] = (record.len() - 1) as u8;
        record
    }
    #[test]
    fn contextual_binding_does_not_reserve_other_statement_names() {
        let plan = packed_loop_program(42, 43, 44, 45, 46, 47, 48);
        let mut prefix = record(&[0, 0, 90, 0]);
        prefix[0] = 0; // The writer has not finalized the length.
        assert!(context_keyword(&plan, &prefix, 43));
        assert!(!context_keyword(&plan, &prefix, 44));
        for length in 0..prefix.len() {
            assert!(!context_keyword(&plan, &prefix[..length], 43));
        }
        for (offset, value) in [(1, 8), (4, 0), (7, 41), (8, 1), (9, 2), (12, 1)] {
            let mut other = prefix.clone();
            other[offset] = value;
            assert!(!context_keyword(&plan, &other, 43));
        }
        for length in 0..plan.len() {
            assert!(!context_keyword(&plan[..length], &prefix, 43));
        }
    }
    #[test]
    fn binding_context_preserves_instruction_register_roles() {
        let prefix = record(&[]);
        assert!(value_context(&prefix));
        for length in 0..9 {
            assert!(!value_context(&prefix[..length]));
        }
        let mut instruction = prefix.clone();
        instruction[4] = 0;
        assert!(!value_context(&instruction));
        instruction[8] = 34;
        assert!(value_context(&instruction));
        instruction[8] = 7;
        assert!(!value_context(&instruction));
        instruction.extend([0, 0, 90, 0]);
        assert!(value_context(&instruction));
    }
    #[test]
    fn numeric_iterable_identity_and_expression_boundary() {
        let plan = packed_loop_program(42, 43, 44, 45, 46, 47, 48);
        for variable in [0u16, 255, 256, 65535] {
            let [hi, lo] = variable.to_be_bytes();
            for expression in [&[2, 0, 0, 0, 3][..], &[12, 13][..], &[255][..]] {
                let mut operand = vec![0, hi, lo, 0, 0, 0, 43, 0];
                operand.extend_from_slice(expression);
                assert_eq!(
                    execute(&plan, &record(&operand)),
                    LoopHeader::Matched {
                        variable: Some(variable),
                        operand_offset: 17
                    }
                );
            }
        }
        assert_eq!(
            execute(&plan, &record(&[0, 0, 90, 0])),
            LoopHeader::Matched {
                variable: None,
                operand_offset: 9
            }
        );
        for operand in [
            &[][..],
            &[0, 0, 90, 0, 0, 0, 43, 0][..],
            &[0, 0, 90, 1, 0, 0, 43, 0, 2][..],
            &[0, 0, 90, 0, 0, 0, 43, 1, 2][..],
        ] {
            assert_eq!(execute(&plan, &record(operand)), LoopHeader::Malformed);
        }
    }
    #[test]
    fn while_and_matching_closing_roles_are_numeric() {
        let plan = packed_loop_program(42, 43, 44, 45, 46, 47, 48);
        let mut header = record(&[2, 0, 0, 0, 1]);
        header[7] = 45;
        assert_eq!(
            execute(&plan, &header),
            LoopHeader::While { operand_offset: 9 }
        );
        for (head, role) in [(44, LoopHeader::Endfor), (46, LoopHeader::Endwhile)] {
            let mut closing = record(&[]);
            closing[7] = head;
            assert_eq!(execute(&plan, &closing), role);
            closing.push(2);
            closing[0] += 1;
            assert_eq!(execute(&plan, &closing), LoopHeader::Malformed);
        }
        header = record(&[]);
        header[7] = 45;
        assert_eq!(execute(&plan, &header), LoopHeader::Malformed);
    }
    #[test]
    fn scoped_roles_retain_numeric_operand_boundaries() {
        let plan = packed_loop_program(42, 43, 44, 45, 46, 47, 48);
        let mut header = record(&[2, 0, 0, 0, 2]);
        header[7] = 47;
        assert_eq!(
            execute(&plan, &header),
            LoopHeader::ScopedFor {
                variable: None,
                operand_offset: 9
            }
        );
        header[7] = 48;
        assert_eq!(
            execute(&plan, &header),
            LoopHeader::ScopedWhile { operand_offset: 9 }
        );
        let mut iterable = record(&[0, 0, 90, 0, 0, 0, 43, 0, 2]);
        iterable[7] = 47;
        assert_eq!(
            execute(&plan, &iterable),
            LoopHeader::ScopedFor {
                variable: Some(90),
                operand_offset: 17
            }
        );
    }
    #[test]
    fn program_and_record_bounds_are_atomic() {
        let plan = packed_loop_program(42, 43, 44, 45, 46, 47, 48);
        let record = record(&[2]);
        for length in 0..plan.len() {
            assert_eq!(execute(&plan[..length], &record), LoopHeader::Malformed);
        }
        for length in 0..record.len() {
            assert_eq!(execute(&plan, &record[..length]), LoopHeader::Malformed);
        }
        for index in [0, 15, 16] {
            let mut corrupt = plan.clone();
            corrupt[index] ^= 1;
            assert_eq!(execute(&corrupt, &record), LoopHeader::Malformed);
        }
        let mut ordinary = record.clone();
        ordinary[7] = 49;
        assert_eq!(execute(&plan, &ordinary), LoopHeader::Ordinary);
        for flags in [8, 16] {
            ordinary = record.clone();
            ordinary[1] = flags;
            assert_eq!(execute(&plan, &ordinary), LoopHeader::Ordinary);
        }
    }
}
