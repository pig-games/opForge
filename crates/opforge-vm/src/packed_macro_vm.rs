// SPDX-License-Identifier: GPL-3.0-or-later
//! Package-selected macro boundaries over compact binary records.
use crate::macro_descriptor_vm::{Descriptor, DescriptorError, NONE};
use package::package::{PARSER_VM_MACRO_VERSION, PARSER_VM_PACKED_MACRO_ENTRY};
fn error(status: u32, offset: usize, message: &'static str) -> DescriptorError {
    DescriptorError {
        status,
        offset: offset as u32,
        message,
    }
}
#[derive(Clone, Copy)]
struct Token {
    kind: u8,
    start: usize,
    end: usize,
}
/// Token ranges are byte offsets, matching source ranges for arguments.
/// Source offsets always refer to the supplied complete packed record.
pub fn execute(
    entry: u16,
    version: u16,
    program: &[u8],
    source: &[u8],
    capacity: usize,
    mut steps: usize,
) -> Result<Vec<Descriptor>, DescriptorError> {
    if entry != PARSER_VM_PACKED_MACRO_ENTRY || version != PARSER_VM_MACRO_VERSION {
        return Err(error(4, 0, "Invalid packed macro entry or version"));
    }
    let mut tick = |n: usize| {
        steps = steps
            .checked_sub(n)
            .ok_or(error(12, 0, "Step budget exceeded"))?;
        Ok::<_, DescriptorError>(())
    };
    if source.len() < 4 || source.len() > 256 || usize::from(source[0]) + 1 != source.len() {
        return Err(error(4, 0, "Invalid packed record extent"));
    }
    let mut end = source.len();
    if source[1] & 32 != 0 {
        if end < 10 || source[end - 6] != 42 || source[end - 1] != 6 {
            return Err(error(5, end, "Invalid packed plan trailer"));
        }
        end -= 6;
    }
    let mut tokens = Vec::new();
    let mut p = 4;
    while p < end {
        tick(1)?;
        let kind = source[p];
        let len = match kind {
            0 | 1 => 4,
            2 => 5,
            3 | 41 => {
                if p + 2 > end {
                    return Err(error(5, p, "Truncated packed payload"));
                }
                2 + usize::from(source[p + 1])
            }
            4..=40 => 1,
            _ => return Err(error(5, p, "Unknown packed token")),
        };
        if p + len > end {
            return Err(error(5, p, "Truncated packed token"));
        }
        tokens.push(Token {
            kind,
            start: p,
            end: p + len,
        });
        p += len;
    }
    let mut pc = 0;
    let mut envelope = None;
    let mut ranges = None;
    let mut published = false;
    let mut records = Vec::new();
    while pc < program.len() {
        tick(1)?;
        let op = program[pc];
        pc += 1;
        if published && op != 0 {
            return Err(error(6, pc, "Operation after publication"));
        }
        match op {
            0x84 => {
                let flags = *program
                    .get(pc)
                    .ok_or(error(6, pc, "Truncated packed program"))?;
                pc += 1;
                if flags & !7 != 0 || envelope.is_some() {
                    return Err(error(6, pc, "Invalid packed envelope policy"));
                }
                let mut i = 0;
                let mut label = NONE;
                if flags & 1 != 0 && tokens.get(i).is_some_and(|t| t.kind <= 1) {
                    label = 4;
                    i += 1;
                    if tokens.get(i).is_some_and(|t| t.kind == 5) {
                        i += 1;
                    }
                }
                if !tokens.get(i).is_some_and(|t| t.kind == 7)
                    || !tokens.get(i + 1).is_some_and(|t| t.kind <= 1)
                {
                    return Err(error(
                        4,
                        tokens.get(i).map_or(end, |t| t.start),
                        "Expected packed dot call",
                    ));
                }
                let head = i + 1;
                i += 2;
                let mut z = tokens.len();
                if flags & 2 != 0 && tokens.get(i).is_some_and(|t| t.kind == 14) {
                    let mut depth = 1;
                    let mut close = None;
                    for (j, t) in tokens.iter().enumerate().skip(i + 1) {
                        tick(1)?;
                        match t.kind {
                            14 => depth += 1,
                            15 => {
                                depth -= 1;
                                if depth == 0 {
                                    close = Some(j);
                                    break;
                                }
                            }
                            _ => {}
                        }
                    }
                    let c = close.ok_or(error(
                        4,
                        tokens[i].start,
                        "Unterminated packed argument list",
                    ))?;
                    if c + 1 != tokens.len() {
                        return Err(error(
                            4,
                            tokens[c].end,
                            "Unexpected tokens after packed list",
                        ));
                    }
                    i += 1;
                    z = c;
                } else if flags & 4 != 0 && tokens.get(i).is_some_and(|t| t.kind == 4) {
                    i += 1;
                    if i == z {
                        return Err(error(4, end, "Empty packed argument list"));
                    }
                }
                let start = tokens.get(i).map_or(end, |t| t.start);
                let stop = tokens.get(z).map_or(end, |t| t.start);
                records.push(Descriptor {
                    kind: 8,
                    flags: 1,
                    token_start: tokens[head].start as u32,
                    token_end: tokens[head].end as u32,
                    source_start: start as u32,
                    source_end: stop as u32,
                    aux: [1, 0, label],
                });
                envelope = Some((i, z));
            }
            0x85 => {
                if program.get(pc..pc + 2) != Some(&[2, 4][..]) || ranges.is_some() {
                    return Err(error(6, pc, "Invalid packed split policy"));
                }
                pc += 2;
                let (a, z) = envelope.ok_or(error(6, pc, "Packed envelope required"))?;
                let mut stack = Vec::new();
                let mut start = a;
                for i in a..=z {
                    tick(1)?;
                    if i == z && !stack.is_empty() {
                        return Err(error(
                            4,
                            tokens.get(i).map_or(end, |t| t.start),
                            "Unclosed packed delimiter",
                        ));
                    }
                    if i == z || tokens[i].kind == 4 && stack.is_empty() {
                        if start == i {
                            if a == z {
                                break;
                            }
                            return Err(error(
                                4,
                                tokens.get(i).map_or(end, |t| t.start),
                                "Empty packed argument",
                            ));
                        }
                        records.push(Descriptor {
                            kind: 9,
                            flags: 0,
                            token_start: tokens[start].start as u32,
                            token_end: tokens[i - 1].end as u32,
                            source_start: tokens[start].start as u32,
                            source_end: tokens[i - 1].end as u32,
                            aux: [NONE; 3],
                        });
                        start = i + 1;
                    } else {
                        match tokens[i].kind {
                            14 | 10 | 12 => {
                                if stack.len() == 16 {
                                    return Err(error(
                                        4,
                                        tokens[i].start,
                                        "Packed delimiter depth exceeded",
                                    ));
                                }
                                stack.push(tokens[i].kind);
                            }
                            15 | 11 | 13 => {
                                let expected = match tokens[i].kind {
                                    15 => 14,
                                    11 => 10,
                                    _ => 12,
                                };
                                if stack.pop() != Some(expected) {
                                    return Err(error(
                                        4,
                                        tokens[i].start,
                                        "Mismatched packed delimiter",
                                    ));
                                }
                            }
                            _ => {}
                        }
                    }
                }
                records[0].aux[1] = (records.len() - 1) as u32;
                ranges = Some(());
            }
            0x83 => {
                if ranges.is_none() || published {
                    return Err(error(6, pc, "Invalid packed publish order"));
                }
                if records.len() > capacity.min(64) {
                    return Err(error(7, 0, "Descriptor capacity exceeded"));
                }
                published = true;
            }
            0 => {
                if !published || pc != program.len() {
                    return Err(error(6, pc, "Invalid packed termination"));
                }
                return Ok(records);
            }
            _ => return Err(error(6, pc - 1, "Invalid packed opcode")),
        }
    }
    Err(error(6, pc, "Missing packed end"))
}

#[cfg(test)]
mod tests {
    use super::*;
    fn run(payload: &[u8]) -> Result<Vec<Descriptor>, DescriptorError> {
        let mut line = vec![0, 0, 0, 1];
        line.extend(payload);
        line[0] = (line.len() - 1) as u8;
        execute(
            3,
            2,
            &package::package::packed_macro_call_program(),
            &line,
            64,
            1000,
        )
    }
    #[test]
    fn nested_and_matched_boundaries() {
        // .m 1, [1,2], "[,]" -- string payload is opaque.
        let r = run(&[
            7, 1, 0, 1, 0, 2, 0, 0, 0, 1, 4, 10, 2, 0, 0, 0, 1, 4, 2, 0, 0, 0, 2, 11, 4, 3, 3,
            b'[', b',', b']',
        ])
        .unwrap();
        assert_eq!(r[0].aux, [1, 3, NONE]);
        assert_eq!((r[1].source_start, r[1].source_end), (9, 14));
        assert_eq!(r[2].token_end - r[2].token_start, 13);
    }
    #[test]
    fn labels_outer_parentheses_and_atomic_failures() {
        let r = run(&[1, 0, 2, 0, 5, 7, 1, 0, 1, 0, 14, 2, 0, 0, 0, 3, 15]).unwrap();
        assert_eq!(r[0].aux, [1, 1, 4]);
        assert_eq!((r[1].source_start, r[1].source_end), (15, 20));
        for p in [
            &[7, 1, 0, 1, 0, 4][..],
            &[7, 1, 0, 1, 0, 14][..],
            &[7, 1, 0, 1, 0, 2][..],
            &[7, 1, 0, 1, 0, 4, 4][..],
        ] {
            assert!(run(p).is_err());
        }
    }
    #[test]
    fn matched_delimiters_reject_mismatch_unclosed_and_depth_overflow() {
        let head = [7, 1, 0, 1, 0];
        for args in [
            vec![15],
            vec![10, 15],
            vec![10, 2, 0, 0, 0, 1],
            [vec![10; 17], vec![2, 0, 0, 0, 1], vec![11; 17]].concat(),
        ] {
            let mut p = head.to_vec();
            p.extend(args);
            assert_eq!(run(&p).unwrap_err().status, 4);
        }
        let mut p = head.to_vec();
        p.extend([vec![10; 16], vec![2, 0, 0, 0, 1], vec![11; 16]].concat());
        assert!(run(&p).is_ok());
    }
    #[test]
    fn plan_trailer_and_capacity_are_validated() {
        let mut l = vec![14, 32, 0, 1, 7, 1, 0, 1, 0, 42, 0, 0, 0, 9, 6];
        let p = package::package::packed_macro_call_program();
        assert_eq!(execute(3, 2, &p, &l, 64, 100).unwrap()[0].source_end, 9);
        assert_eq!(execute(3, 2, &p, &l, 0, 100).unwrap_err().status, 7);
        l[14] = 5;
        assert_eq!(execute(3, 2, &p, &l, 64, 100).unwrap_err().status, 5);
    }
}
