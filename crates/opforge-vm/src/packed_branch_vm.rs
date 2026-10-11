// SPDX-License-Identifier: GPL-3.0-or-later
//! Shared package-owned conditional heads; expressions remain EXVM-owned.
use package::package::PACKED_BRANCH_OPCODE;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Branch {
    Ordinary,
    If,
    Elseif,
    Else,
    Endif,
    Malformed,
}

const RECORD_HEADER_BYTES: usize = 4;
const NAME_TOKEN_BYTES: usize = 4;
const TOKEN_COLON: u8 = 5;
const TOKEN_DOT: u8 = 7;
const HEAD_OPERAND_OFFSET: usize = RECORD_HEADER_BYTES + 1 + NAME_TOKEN_BYTES;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Classification {
    pub branch: Branch,
    /// Offset from the original record start, including any leading label.
    /// Zero for ordinary or malformed records.
    pub operand_offset: usize,
    /// Bytes occupied by the optional numeric label and colon token.
    /// Zero for ordinary or malformed records.
    pub label_prefix_bytes: usize,
}

impl Classification {
    fn unselected(branch: Branch) -> Self {
        Self {
            branch,
            operand_offset: 0,
            label_prefix_bytes: 0,
        }
    }
}

pub fn execute(program: &[u8], record: &[u8]) -> Branch {
    classify(program, record).branch
}

/// Classify the bounded record envelope; EXVM owns expression syntax.
pub fn classify(program: &[u8], record: &[u8]) -> Classification {
    if program.len() != 20
        || program[..2] != [PACKED_BRANCH_OPCODE, 4]
        || program[18..] != [0x83, 0]
        || program[2..18]
            .chunks_exact(4)
            .enumerate()
            .any(|(index, row)| row[2] != index as u8 + 1 || row[3] != u8::from(index < 2))
        || !(4..=256).contains(&record.len())
        || usize::from(record[0]) + 1 != record.len()
    {
        return Classification::unselected(Branch::Malformed);
    }
    // All writer flag bits are structural/private roles, not branch grammar.
    if record[1] & 24 != 0 || record.len() < HEAD_OPERAND_OFFSET {
        return Classification::unselected(Branch::Ordinary);
    }
    let label_prefix_bytes = if record[RECORD_HEADER_BYTES] <= 1
        && record[RECORD_HEADER_BYTES + NAME_TOKEN_BYTES - 1] == 0
    {
        NAME_TOKEN_BYTES
            + usize::from(record[RECORD_HEADER_BYTES + NAME_TOKEN_BYTES] == TOKEN_COLON)
    } else {
        0
    };
    let head = &record[label_prefix_bytes..];
    if head.len() < HEAD_OPERAND_OFFSET || head[4] != TOKEN_DOT || head[5] > 1 {
        return Classification::unselected(Branch::Ordinary);
    }
    let Some(row) = program[2..18]
        .chunks_exact(4)
        .find(|row| row[..2] == head[6..8])
    else {
        return Classification::unselected(Branch::Ordinary);
    };
    if head[8] != 0 || (head.len() > HEAD_OPERAND_OFFSET) != (row[3] != 0) {
        return Classification::unselected(Branch::Malformed);
    }
    let branch = match row[2] {
        1 => Branch::If,
        2 => Branch::Elseif,
        3 => Branch::Else,
        4 => Branch::Endif,
        _ => Branch::Malformed,
    };
    Classification {
        branch,
        operand_offset: HEAD_OPERAND_OFFSET + label_prefix_bytes,
        label_prefix_bytes,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use package::package::packed_branch_program;
    fn record(head: u16, expression: bool) -> Vec<u8> {
        let [high, low] = head.to_be_bytes();
        let mut bytes = vec![8, 0, 0, 1, 7, 0, high, low, 0];
        if expression {
            bytes.push(2);
            bytes[0] += 1;
        }
        bytes
    }
    #[test]
    fn roles_and_operand_policies_are_numeric() {
        let program = packed_branch_program([256, 257, 258, 259]);
        for (head, role, expression) in [
            (256, Branch::If, true),
            (257, Branch::Elseif, true),
            (258, Branch::Else, false),
            (259, Branch::Endif, false),
        ] {
            assert_eq!(execute(&program, &record(head, expression)), role);
            assert_eq!(
                execute(&program, &record(head, !expression)),
                Branch::Malformed
            );
        }
        assert_eq!(execute(&program, &record(260, true)), Branch::Ordinary);
        let mut private = record(256, true);
        private[1] = 128;
        assert_eq!(execute(&program, &private), Branch::If);
        for flag in [8, 16] {
            private[1] = flag;
            assert_eq!(execute(&program, &private), Branch::Ordinary);
        }
        private = record(256, true);
        private[8] = 1;
        assert_eq!(execute(&program, &private), Branch::Malformed);
    }
    fn labelled_record(head: u16, expression: bool, colon: bool) -> Vec<u8> {
        let mut bytes = record(head, expression);
        let mut label = vec![0, 0x12, 0x34, 0];
        if colon {
            label.push(TOKEN_COLON);
        }
        bytes.splice(RECORD_HEADER_BYTES..RECORD_HEADER_BYTES, label);
        bytes[0] = (bytes.len() - 1) as u8;
        bytes
    }

    #[test]
    fn leading_numeric_labels_preserve_roles_and_original_operand_offsets() {
        let program = packed_branch_program([256, 257, 258, 259]);
        for (head, branch, expression) in [
            (256, Branch::If, true),
            (257, Branch::Elseif, true),
            (258, Branch::Else, false),
            (259, Branch::Endif, false),
        ] {
            for colon in [false, true] {
                let prefix = NAME_TOKEN_BYTES + usize::from(colon);
                let bytes = labelled_record(head, expression, colon);
                let mut register_label = bytes.clone();
                register_label[RECORD_HEADER_BYTES] = 1;
                assert_eq!(execute(&program, &register_label), branch);
                for flag in [8, 16] {
                    let mut omitted = bytes.clone();
                    omitted[1] = flag;
                    assert_eq!(
                        classify(&program, &omitted),
                        Classification::unselected(Branch::Ordinary)
                    );
                }
                assert_eq!(
                    classify(&program, &bytes),
                    Classification {
                        branch,
                        operand_offset: HEAD_OPERAND_OFFSET + prefix,
                        label_prefix_bytes: prefix,
                    }
                );
                assert_eq!(
                    execute(&program, &labelled_record(head, !expression, colon)),
                    Branch::Malformed
                );
                let mut qualified = bytes.clone();
                qualified[8 + prefix] = 1;
                assert_eq!(execute(&program, &qualified), Branch::Malformed);
            }
            assert_eq!(
                classify(&program, &record(head, expression)),
                Classification {
                    branch,
                    operand_offset: HEAD_OPERAND_OFFSET,
                    label_prefix_bytes: 0,
                }
            );
        }
    }

    #[test]
    fn label_normalization_is_bounded_and_rejects_noncanonical_prefixes() {
        let program = packed_branch_program([256, 257, 258, 259]);
        for colon in [false, true] {
            let bytes = labelled_record(256, true, colon);
            for end in 0..bytes.len() {
                assert_eq!(execute(&program, &bytes[..end]), Branch::Malformed);
            }
            // A finalized shorter record cannot borrow its head/operand from the next record.
            for end in
                RECORD_HEADER_BYTES..HEAD_OPERAND_OFFSET + NAME_TOKEN_BYTES + usize::from(colon)
            {
                let mut short = bytes[..end].to_vec();
                short[0] = (end - 1) as u8;
                assert_eq!(execute(&program, &short), Branch::Ordinary);
            }
            for (offset, value) in [
                (4, 2),
                (7, 1),
                (8 + NAME_TOKEN_BYTES + usize::from(colon), 1),
            ] {
                let mut bad = bytes.clone();
                bad[offset] = value;
                let expected = if offset > 7 {
                    Branch::Malformed
                } else {
                    Branch::Ordinary
                };
                assert_eq!(execute(&program, &bad), expected);
            }
            let mut maximum = bytes.clone();
            maximum.resize(256, 2);
            maximum[0] = 255;
            assert_eq!(execute(&program, &maximum), Branch::If);
            maximum.push(2);
            assert_eq!(execute(&program, &maximum), Branch::Malformed);
            let mut wrong_separator = bytes.clone();
            wrong_separator[8] = if colon { 6 } else { TOKEN_COLON };
            assert_eq!(execute(&program, &wrong_separator), Branch::Ordinary);
        }
    }

    #[test]
    fn program_and_record_fail_atomically() {
        let program = packed_branch_program([1, 2, 3, 4]);
        let bytes = record(1, true);
        for length in 0..program.len() {
            assert_eq!(execute(&program[..length], &bytes), Branch::Malformed);
        }
        for length in 0..bytes.len() {
            assert_eq!(execute(&program, &bytes[..length]), Branch::Malformed);
        }
        for offset in [0, 1, 4, 5, 8, 9, 12, 13, 16, 17, 18, 19] {
            let mut bad = program.clone();
            bad[offset] ^= 1;
            assert_eq!(execute(&bad, &bytes), Branch::Malformed);
        }
    }
}
