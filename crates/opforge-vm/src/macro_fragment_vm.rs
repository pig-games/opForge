// SPDX-License-Identifier: GPL-3.0-or-later
//! Atomic recipes over original raw generated-call bytes. Binding and copying
//! belong to the consumer; substituted argument bytes never enter this scanner.
use crate::macro_descriptor_vm::{Descriptor, DescriptorError};
use package::package::{PARSER_VM_MACRO_FRAGMENT_ENTRY, PARSER_VM_MACRO_VERSION};

pub const LITERAL: u16 = 13;
pub const POSITIONAL: u16 = 14;
pub const NAMED: u16 = 15;
pub const SUPPLIED_LIST: u16 = 16;

fn error(status: u32, offset: usize, message: &'static str) -> DescriptorError {
    DescriptorError {
        status,
        offset: offset as u32,
        message,
    }
}
fn name_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

/// Execute entry 4, contract 2. Capacity is in records; budget charges each
/// program byte, each inspected source byte and each staged record. No result
/// escapes on failure. All source spans are byte offsets, including UTF-8 text.
pub fn execute(
    entry: u16,
    version: u16,
    program: &[u8],
    source: &str,
    capacity: usize,
    steps: usize,
) -> Result<Vec<Descriptor>, DescriptorError> {
    if entry != PARSER_VM_MACRO_FRAGMENT_ENTRY
        || version != PARSER_VM_MACRO_VERSION
        || source.len() > 1024
    {
        return Err(error(4, 0, "Invalid fragment request"));
    }
    if program.len() != 10 || program[0] != 0x86 || program[1] != 1 || program[8..] != [0x83, 0] {
        return Err(error(6, 0, "Invalid fragment program"));
    }
    let [positional, dot, first, last, open, close] = program[2..8].try_into().unwrap();
    let markers = [positional, dot, open, close];
    if markers.iter().any(|b: &u8| !b.is_ascii_punctuation())
        || markers
            .iter()
            .enumerate()
            .any(|(i, b)| markers[..i].contains(b))
        || first < b'1'
        || last > b'9'
        || first > last
    {
        return Err(error(6, 0, "Invalid fragment grammar"));
    }
    let mut budget =
        steps
            .checked_sub(program.len())
            .ok_or(error(12, 0, "Step budget exceeded"))?;
    let bytes = source.as_bytes();
    let mut records: Vec<Descriptor> = Vec::new();
    let mut emit = |kind,
                    start: usize,
                    end: usize,
                    aux: [u32; 3],
                    budget: &mut usize|
     -> Result<(), DescriptorError> {
        *budget = budget
            .checked_sub(1)
            .ok_or(error(12, start, "Step budget exceeded"))?;
        if records.len() >= capacity.min(64) {
            return Err(error(7, start, "Fragment capacity exceeded"));
        }
        records.push(Descriptor {
            kind,
            flags: 0,
            token_start: 0,
            token_end: 0,
            source_start: start as u32,
            source_end: end as u32,
            aux,
        });
        Ok(())
    };
    let mut i = 0;
    let mut literal = 0;
    while i < bytes.len() {
        budget = budget
            .checked_sub(1)
            .ok_or(error(12, i, "Step budget exceeded"))?;
        let mut end = i + 1;
        let mut kind = LITERAL;
        let mut aux = [0; 3];
        if (bytes[i] == positional || bytes[i] == dot) && i + 1 < bytes.len() {
            let next = bytes[i + 1];
            if (first..=last).contains(&next) {
                kind = POSITIONAL;
                end = i + 2;
                aux[0] = (next - first) as u32;
            } else if bytes[i] == dot {
                if next == positional {
                    kind = SUPPLIED_LIST;
                    end = i + 2;
                } else {
                    let start = i + if next == open { 2 } else { 1 };
                    let mut j = start;
                    while j < bytes.len() && name_byte(bytes[j]) {
                        budget =
                            budget
                                .checked_sub(1)
                                .ok_or(error(12, j, "Step budget exceeded"))?;
                        j += 1;
                    }
                    if j > start && (next != open || bytes.get(j) == Some(&close)) {
                        kind = NAMED;
                        end = j + usize::from(next == open);
                        aux[0] = start as u32;
                        aux[1] = j as u32;
                    }
                }
            }
        }
        if kind != LITERAL {
            if literal < i {
                emit(LITERAL, literal, i, [0; 3], &mut budget)?;
            }
            emit(kind, i, end, aux, &mut budget)?;
            literal = end;
        }
        i = end;
    }
    if literal < i {
        emit(LITERAL, literal, i, [0; 3], &mut budget)?;
    }
    Ok(records)
}

#[cfg(test)]
mod tests {
    use super::*;
    use package::package::macro_fragment_program;
    fn run(s: &str) -> Vec<Descriptor> {
        execute(4, 2, &macro_fragment_program(), s, 64, 65536).unwrap()
    }
    #[test]
    fn raw_recipes() {
        let s = "é'@1'\\.2.@.name.{foo_2}@10";
        let r = run(s);
        assert_eq!(
            r.iter().map(|x| x.kind).collect::<Vec<_>>(),
            [13, 14, 13, 14, 16, 15, 15, 14, 13]
        );
        assert_eq!(r[1].aux[0], 0);
        assert_eq!(r[3].aux[0], 1);
        assert_eq!(&s[r[5].aux[0] as usize..r[5].aux[1] as usize], "name");
        assert_eq!(&s[r[6].aux[0] as usize..r[6].aux[1] as usize], "foo_2");
        assert_eq!(r.first().unwrap().source_start, 0);
        assert_eq!(r.last().unwrap().source_end, s.len() as u32);
        assert!(r.windows(2).all(|w| w[0].source_end == w[1].source_start));
        assert!(r.iter().all(|r| r.token_start == 0 && r.token_end == 0));
    }
    #[test]
    fn literal_and_empty() {
        assert!(run("").is_empty());
        assert_eq!(run(".{bad .{} @0 é").len(), 1);
        assert_eq!(
            run("@1.2.@").iter().map(|r| r.kind).collect::<Vec<_>>(),
            [14, 14, 16]
        );
    }
    #[test]
    fn selected_grammar_and_budget() {
        let p = vec![0x86, 1, b'%', b'!', b'3', b'5', b'[', b']', 0x83, 0];
        let r = execute(4, 2, &p, "@1%3!%![name]", 64, 65536).unwrap();
        assert_eq!(
            r.iter().map(|r| r.kind).collect::<Vec<_>>(),
            [13, 14, 16, 15]
        );
        assert_eq!(r[1].aux[0], 0);
        for (s, cost) in [
            ("", 10),
            ("x", 12),
            ("@1", 12),
            (".name", 16),
            (".{name}", 16),
        ] {
            assert!(execute(4, 2, &macro_fragment_program(), s, 64, cost).is_ok());
            assert_eq!(
                execute(4, 2, &macro_fragment_program(), s, 64, cost - 1)
                    .unwrap_err()
                    .status,
                12
            );
        }
    }
    #[test]
    fn failure_offsets() {
        let p = macro_fragment_program();
        for (source, capacity, budget, status, offset) in [
            ("abc@1", 0, 65536, 7, 0),
            ("abc@1", 1, 65536, 7, 3),
            ("@1abc", 1, 65536, 7, 2),
            ("abc", 64, 12, 12, 2),
            (".name", 64, 12, 12, 2),
            ("abc@1", 64, 14, 12, 0),
            ("@1abc", 64, 15, 12, 2),
        ] {
            let e = execute(4, 2, &p, source, capacity, budget).unwrap_err();
            assert_eq!((e.status, e.offset), (status, offset), "{source}");
        }
        let mut bad = p.clone();
        bad[8] = 0;
        assert_eq!(execute(4, 2, &bad, "abc", 64, 65536).unwrap_err().offset, 0);
    }
    #[test]
    fn complete_line_spelling_bound() {
        let p = macro_fragment_program();
        let records = execute(4, 2, &p, &"x".repeat(1024), 64, 65536).unwrap();
        assert_eq!(records.len(), 1);
        assert_eq!(records[0].source_end, 1024);
    }
    #[test]
    fn rejected_requests_and_programs() {
        let p = macro_fragment_program();
        for n in 0..p.len() {
            assert_eq!(execute(4, 2, &p[..n], "", 64, 65536).unwrap_err().status, 6);
        }
        for n in [0, 1, 8, 9] {
            let mut bad = p.clone();
            bad[n] ^= 1;
            assert_eq!(execute(4, 2, &bad, "", 64, 65536).unwrap_err().status, 6);
        }
        for (n, v) in [
            (2, b'a'),
            (3, b'@'),
            (4, b'0'),
            (5, b':'),
            (6, b'.'),
            (7, 0),
        ] {
            let mut bad = p.clone();
            bad[n] = v;
            assert_eq!(execute(4, 2, &bad, "", 64, 65536).unwrap_err().status, 6);
        }
        let mut trailing = p.clone();
        trailing.push(0);
        assert_eq!(
            execute(4, 2, &trailing, "", 64, 65536).unwrap_err().status,
            6
        );
        assert_eq!(execute(3, 2, &p, "", 64, 65536).unwrap_err().status, 4);
        assert_eq!(execute(4, 1, &p, "", 64, 65536).unwrap_err().status, 4);
        assert_eq!(
            execute(4, 2, &p, &"x".repeat(1025), 64, 65536)
                .unwrap_err()
                .status,
            4
        );
        assert_eq!(execute(4, 2, &p, "x", 0, 65536).unwrap_err().status, 7);
        assert_eq!(
            execute(4, 2, &p, &"@1".repeat(65), 100, 65536)
                .unwrap_err()
                .status,
            7
        );
        assert_eq!(execute(4, 2, &p, "x", 64, 0).unwrap_err().status, 12);
    }
}
