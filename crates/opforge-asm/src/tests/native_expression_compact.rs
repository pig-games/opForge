//! Focused proof cases for the experimental compact checked-i32 ExprVM entry.
//!
//! The batch selector 0x8000 is owned by this test protocol.  It is not a
//! canonical ExprVM bytecode version; the native wrapper dispatches it to the
//! compact evaluator and leaves the ordinary v1/v2 selector contract intact.
use super::*;

const COMPACT_SELECTOR: u16 = 0x8000;

struct CompactCase {
    name: &'static str,
    selector: u16,
    code: Vec<u8>,
    expected: Result<i64, u32>,
    pc: u32,
    symbol: u32,
    symbol_refs: u32,
}

fn lit8(code: &mut Vec<u8>, value: i8) {
    code.extend([0x13, value as u8]);
}
fn lit16(code: &mut Vec<u8>, value: i16) {
    code.push(0x14);
    code.extend(value.to_le_bytes());
}
fn lit32(code: &mut Vec<u8>, value: i32) {
    code.push(0x15);
    code.extend(value.to_le_bytes());
}
fn op(code: &mut Vec<u8>, opcode: u8) {
    code.push(opcode);
}

fn compact_cases() -> Vec<CompactCase> {
    let mut cases = Vec::new();
    for (name, value) in [("i8-min", i8::MIN), ("i8-max", i8::MAX)] {
        let mut code = Vec::new();
        lit8(&mut code, value);
        code.push(0);
        cases.push(CompactCase {
            name,
            selector: COMPACT_SELECTOR,
            code,
            expected: Ok(i64::from(value)),
            pc: 0,
            symbol: 0,
            symbol_refs: 0,
        });
    }
    for (name, value) in [("i16-min", i16::MIN), ("i16-max", i16::MAX)] {
        let mut code = Vec::new();
        lit16(&mut code, value);
        code.push(0);
        cases.push(CompactCase {
            name,
            selector: COMPACT_SELECTOR,
            code,
            expected: Ok(i64::from(value)),
            pc: 0,
            symbol: 0,
            symbol_refs: 0,
        });
    }
    for value in [i32::MIN, i32::MAX, -1, 0] {
        let mut code = Vec::new();
        lit32(&mut code, value);
        code.push(0);
        cases.push(CompactCase {
            name: "i32-boundary",
            selector: COMPACT_SELECTOR,
            code,
            expected: Ok(i64::from(value)),
            pc: 0,
            symbol: 0,
            symbol_refs: 0,
        });
    }

    let mut code = Vec::new();
    lit8(&mut code, 7);
    op(&mut code, 0x30);
    code.push(0);
    cases.push(CompactCase {
        name: "negate",
        selector: COMPACT_SELECTOR,
        code,
        expected: 7i32.checked_neg().map(i64::from).ok_or(1),
        pc: 0,
        symbol: 0,
        symbol_refs: 0,
    });

    for (name, left, right, opcode) in [
        ("add", 120i32, 8i32, 0x31),
        ("subtract", -120i32, 8i32, 0x32),
        ("multiply", -12i32, 3i32, 0x33),
    ] {
        let mut code = Vec::new();
        lit8(&mut code, left as i8);
        lit16(&mut code, right as i16);
        op(&mut code, opcode);
        code.push(0);
        let expected = match opcode {
            0x31 => left.checked_add(right).unwrap(),
            0x32 => left.checked_sub(right).unwrap(),
            0x33 => left.checked_mul(right).unwrap(),
            _ => unreachable!(),
        };
        cases.push(CompactCase {
            name,
            selector: COMPACT_SELECTOR,
            code,
            expected: Ok(i64::from(expected)),
            pc: 0,
            symbol: 0,
            symbol_refs: 0,
        });
    }

    let mut code = Vec::new();
    code.push(0x11);
    lit8(&mut code, 1);
    op(&mut code, 0x31);
    code.extend([0x12, 0, 0]);
    op(&mut code, 0x33);
    code.push(0);
    let pc_symbol_expected = i64::from((0x1234i32.checked_add(1).unwrap()).checked_mul(2).unwrap());
    cases.push(CompactCase {
        name: "pc-symbol-fused",
        selector: COMPACT_SELECTOR,
        code,
        expected: Ok(pc_symbol_expected),
        pc: 0x1234,
        symbol: 2,
        symbol_refs: 1,
    });

    // Identical table bits have signed meaning in the checked-i32 entry and
    // unsigned meaning in the canonical entry. Alternate entries to detect
    // leaked evaluator mode, including both signed boundaries.
    for value in [i32::MIN, -7, -1, i32::MAX] {
        for selector in [COMPACT_SELECTOR, 2, COMPACT_SELECTOR] {
            cases.push(CompactCase {
                name: "symbol-signedness-by-entry",
                selector,
                code: vec![0x12, 0, 0, 0],
                expected: Ok(if selector == COMPACT_SELECTOR {
                    i64::from(value)
                } else {
                    i64::from(value as u32)
                }),
                pc: 0,
                symbol: value as u32,
                symbol_refs: 1,
            });
        }
    }

    // Generic operator pairs must work on dynamic symbols, not just on folded
    // literals. Alternate canonical calls so compact signedness never leaks.
    for (name, value, right, operator) in [
        ("bit-and", -1i32, 0x5ai32, 21u8),
        ("bit-or", i32::MIN, 0x55, 22),
        ("bit-xor", i32::MAX, -1, 23),
        ("shift-left", 3, 5, 13),
        ("shift-left-overflow", i32::MAX, 1, 13),
        ("shift-right", i32::MAX, 7, 14),
        ("shift-right-negative", -2, 1, 14),
        ("shift-count-wrap", i32::MIN, 32, 14),
        ("shift-count-negative", i32::MAX, -1, 14),
    ] {
        for selector in [COMPACT_SELECTOR, 2, COMPACT_SELECTOR] {
            let compact = selector == COMPACT_SELECTOR;
            let mut code = vec![0x12, 0, 0];
            if compact {
                lit32(&mut code, right);
            } else {
                code.push(0x10);
                code.extend(i64::from(right).to_le_bytes());
            }
            code.extend([0x21, operator, 0]);
            let left = if compact {
                i64::from(value)
            } else {
                i64::from(value as u32)
            };
            let right = i64::from(right);
            let result = match operator {
                13 => left.wrapping_shl((right & 31) as u32),
                14 => (left as u64).wrapping_shr((right & 31) as u32) as i64,
                21 => left & right,
                22 => left | right,
                23 => left ^ right,
                _ => unreachable!(),
            };
            cases.push(CompactCase {
                name,
                selector,
                code,
                expected: if compact {
                    i32::try_from(result).map(i64::from).map_err(|_| 1)
                } else {
                    Ok(result)
                },
                pc: 0,
                symbol: value as u32,
                symbol_refs: 1,
            });
        }
    }
    for value in [i32::MIN, i32::MAX, -1] {
        for selector in [COMPACT_SELECTOR, 2, COMPACT_SELECTOR] {
            let left = if selector == COMPACT_SELECTOR {
                i64::from(value)
            } else {
                i64::from(value as u32)
            };
            cases.push(CompactCase {
                name: "bit-not-symbol",
                selector,
                code: vec![0x12, 0, 0, 0x20, 2, 0],
                expected: Ok(!left),
                pc: 0,
                symbol: value as u32,
                symbol_refs: 1,
            });
        }
    }

    for (name, code, expected) in vec![
        ("missing-end", vec![0x13, 1], Err(51)),
        ("unknown", vec![0xff], Err(52)),
        ("truncated-unary-pair", vec![0x20], Err(1)),
        ("truncated-binary-pair", vec![0x21], Err(1)),
        ("unknown-unary-pair", vec![0x13, 1, 0x20, 255, 0], Err(1)),
        (
            "unknown-binary-pair",
            vec![0x13, 1, 0x13, 2, 0x21, 255, 0],
            Err(1),
        ),
        ("literal-underflow", vec![0x13], Err(53)),
        ("literal-i16-underflow", vec![0x14, 1], Err(53)),
        ("literal-overflow", vec![0x15, 1, 2, 3], Err(53)),
        ("empty-end", vec![0], Err(56)),
        ("bad-end", vec![0x13, 1, 0, 0x13, 2], Err(56)),
        ("old-v2-rejected", vec![0x10, 1, 0, 0], Err(52)),
        (
            "negate-min",
            {
                let mut c = Vec::new();
                lit32(&mut c, i32::MIN);
                op(&mut c, 0x30);
                c.push(0);
                c
            },
            i32::MIN.checked_neg().ok_or(1),
        ),
        (
            "add-overflow",
            {
                let mut c = Vec::new();
                lit32(&mut c, i32::MAX);
                lit8(&mut c, 1);
                op(&mut c, 0x31);
                c.push(0);
                c
            },
            i32::MAX.checked_add(1).ok_or(1),
        ),
        (
            "subtract-overflow",
            {
                let mut c = Vec::new();
                lit32(&mut c, i32::MIN);
                lit8(&mut c, 1);
                op(&mut c, 0x32);
                c.push(0);
                c
            },
            i32::MIN.checked_sub(1).ok_or(1),
        ),
        (
            "multiply-overflow",
            {
                let mut c = Vec::new();
                lit32(&mut c, i32::MAX);
                lit8(&mut c, 2);
                op(&mut c, 0x33);
                c.push(0);
                c
            },
            i32::MAX.checked_mul(2).ok_or(1),
        ),
        ("stack-underflow", vec![0x31, 0], Err(1)),
        ("truncated-symbol", vec![0x12, 0], Err(1)),
        ("bad-symbol-index", vec![0x12, 1, 0, 0], Err(1)),
        (
            "stack-overflow",
            vec![
                0x13, 1, 0x13, 2, 0x13, 3, 0x13, 4, 0x13, 5, 0x13, 6, 0x13, 7, 0x13, 8, 0x13, 9, 0,
            ],
            Err(54),
        ),
    ] {
        cases.push(CompactCase {
            name,
            selector: COMPACT_SELECTOR,
            code,
            expected: expected.map(i64::from),
            pc: 0,
            symbol: 0,
            symbol_refs: 0,
        });
    }
    let mut wide = Vec::new();
    wide.push(0x10);
    wide.extend((0x1_0000_0001i64).to_le_bytes());
    wide.push(0x10);
    wide.extend(2i64.to_le_bytes());
    wide.extend([0x21, 6, 0]);
    cases.push(CompactCase {
        name: "canonical-v2-after-compact",
        selector: 2,
        code: wide,
        expected: Ok(0x1_0000_0001i64 + 2),
        pc: 0,
        symbol: 0,
        symbol_refs: 0,
    });
    cases
}

fn compact_batch(cases: &[CompactCase]) -> (Vec<u8>, Vec<u8>) {
    let mut input = (cases.len() as u32).to_be_bytes().to_vec();
    let mut output = Vec::new();
    for case in cases {
        assert!(case.code.len() <= 256, "{}", case.name);
        input.extend(case.selector.to_be_bytes());
        input.extend((case.code.len() as u16).to_be_bytes());
        input.extend(case.pc.to_be_bytes());
        input.extend(case.symbol.to_be_bytes());
        input.extend(&case.code);
        if case.code.len() % 2 != 0 {
            input.push(0);
        }
        let words = match case.expected {
            Ok(value) => [
                0,
                0,
                (value as u64 >> 32) as u32,
                value as u32,
                case.symbol_refs,
                0,
            ],
            // The runtime preserves observed symbol-reference flags on failure;
            // only the failed result is unavailable.
            Err(status) => [status, 1, 0, 0, case.symbol_refs, 0],
        };
        for word in words {
            output.extend(word.to_be_bytes());
        }
    }
    (input, output)
}

#[test]
fn native_expression_compact_live_rust_oracle() {
    let cases = compact_cases();
    let (input, output) = compact_batch(&cases);
    assert!(cases.len() >= 20);
    assert!(input.len() < 4096);
    assert_eq!(output.len(), cases.len() * 24);
    assert!(cases
        .iter()
        .any(|case| matches!(case.expected, Ok(value) if value == i64::from(i32::MIN))));
    assert!(cases.iter().any(|case| matches!(case.expected, Err(1))));
}

#[test]
fn native_expression_compact_fs_uae() {
    let cases = compact_cases();
    let (input, expected) = compact_batch(&cases);
    match crate::fs_uae_smoke::run_exprvm_i64_harness_from_env(&workspace_root(), &input, &expected)
        .expect("compact checked-i32 native proof")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
            eprintln!("PASS: {} compact/native entry cases", cases.len());
        }
    }
}
