// SPDX-License-Identifier: GPL-3.0-or-later
//! Package-owned loop header grammar over immutable packed writer records.
use package::package::MacroDescriptorOpcode;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoopHeader {
    Matched {
        variable: Option<u16>,
        operand_offset: usize,
    },
    Ordinary,
    Malformed,
}
fn valid_program(program: &[u8]) -> bool {
    program.len() == 7
        && program[0] == MacroDescriptorOpcode::LoopHeader as u8
        && program[5..]
            == [
                MacroDescriptorOpcode::Publish as u8,
                MacroDescriptorOpcode::End as u8,
            ]
}

/// Bind a grammar name while the writer has emitted `.for <bare variable>`.
/// Its length byte is not final; the slice supplies the current packed extent.
pub fn context_keyword(program: &[u8], prefix: &[u8], candidate: u16) -> bool {
    valid_program(program)
        && prefix.len() == 13
        && prefix[1] & 0xd8 == 0
        && prefix[4] == 7
        && prefix[5] <= 1
        && prefix[6..8] == program[1..3]
        && prefix[8] == 0
        && prefix[9] <= 1
        && prefix[12] == 0
        && candidate.to_be_bytes() == program[3..5]
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
    if source[1] & 24 != 0
        || source.len() < 9
        || source[4] != 7
        || source[5] > 1
        || source[6..8] != program[1..3]
    {
        return Ordinary;
    }
    if source[8] != 0 || source.len() == 9 {
        return Malformed;
    }
    // Only two consecutive bare name tokens establish the iterable form.
    // Other expressions, including a single name, go to counted EXVM.
    if source.len() >= 17 && source[9] <= 1 && source[13] <= 1 && source[14..16] == program[3..5] {
        if source[12] != 0 || source[16] != 0 || source.len() == 17 {
            return Malformed;
        }
        return Matched {
            variable: Some(u16::from_be_bytes([source[10], source[11]])),
            operand_offset: 17,
        };
    }
    Matched {
        variable: None,
        operand_offset: 9,
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
        let plan = packed_loop_program(42, 43);
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
    fn numeric_iterable_identity_and_expression_boundary() {
        let plan = packed_loop_program(42, 43);
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
    fn program_and_record_bounds_are_atomic() {
        let plan = packed_loop_program(42, 43);
        let record = record(&[2]);
        for length in 0..plan.len() {
            assert_eq!(execute(&plan[..length], &record), LoopHeader::Malformed);
        }
        for length in 0..record.len() {
            assert_eq!(execute(&plan, &record[..length]), LoopHeader::Malformed);
        }
        for index in [0, 5, 6] {
            let mut corrupt = plan.clone();
            corrupt[index] ^= 1;
            assert_eq!(execute(&corrupt, &record), LoopHeader::Malformed);
        }
        let mut ordinary = record.clone();
        ordinary[7] = 44;
        assert_eq!(execute(&plan, &ordinary), LoopHeader::Ordinary);
        for flags in [8, 16] {
            ordinary = record.clone();
            ordinary[1] = flags;
            assert_eq!(execute(&plan, &ordinary), LoopHeader::Ordinary);
        }
    }
}
