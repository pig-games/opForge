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

pub fn execute(program: &[u8], record: &[u8]) -> Branch {
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
        return Branch::Malformed;
    }
    // All writer flag bits are structural/private roles, not branch grammar.
    if record[1] & 24 != 0 || record.len() < 9 || record[4] != 7 || record[5] > 1 {
        return Branch::Ordinary;
    }
    let Some(row) = program[2..18]
        .chunks_exact(4)
        .find(|row| row[..2] == record[6..8])
    else {
        return Branch::Ordinary;
    };
    if record[8] != 0 || (record.len() > 9) != (row[3] != 0) {
        return Branch::Malformed;
    }
    match row[2] {
        1 => Branch::If,
        2 => Branch::Elseif,
        3 => Branch::Else,
        4 => Branch::Endif,
        _ => Branch::Malformed,
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
