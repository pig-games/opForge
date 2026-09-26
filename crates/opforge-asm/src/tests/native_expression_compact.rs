//! Focused proof cases for the compact full-i64 ExprVM entry.
//!
//! Selector 0x8000 belongs to this native test protocol. It is not a canonical
//! ExprVM bytecode version; the native wrapper dispatches it to evalCompact64.
use super::*;

const COMPACT_SELECTOR: u16 = 0x8000;

struct CompactCase {
    name: &'static str,
    selector: u16,
    code: Vec<u8>,
    expected: Result<i64, u32>,
    pc: u32,
    symbol: i64,
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
fn lit_u32(code: &mut Vec<u8>, value: u32) {
    code.push(0x16);
    code.extend(value.to_le_bytes());
}
fn lit64(code: &mut Vec<u8>, value: i64) {
    code.push(0x17);
    code.extend(value.to_le_bytes());
}
fn op(code: &mut Vec<u8>, opcode: u8) {
    code.push(opcode);
}

fn case(
    name: &'static str,
    selector: u16,
    code: Vec<u8>,
    expected: Result<i64, u32>,
    symbol: i64,
    symbol_refs: u32,
) -> CompactCase {
    CompactCase {
        name,
        selector,
        code,
        expected,
        pc: 0,
        symbol,
        symbol_refs,
    }
}

fn compact_cases() -> Vec<CompactCase> {
    let mut cases = Vec::new();
    for (name, value) in [("i8-min", i8::MIN), ("i8-max", i8::MAX)] {
        let mut code = Vec::new();
        lit8(&mut code, value);
        code.push(0);
        cases.push(case(
            name,
            COMPACT_SELECTOR,
            code,
            Ok(i64::from(value)),
            0,
            0,
        ));
    }
    for (name, value) in [("i16-min", i16::MIN), ("i16-max", i16::MAX)] {
        let mut code = Vec::new();
        lit16(&mut code, value);
        code.push(0);
        cases.push(case(
            name,
            COMPACT_SELECTOR,
            code,
            Ok(i64::from(value)),
            0,
            0,
        ));
    }
    for value in [i32::MIN, i32::MAX, -1, 0] {
        let mut code = Vec::new();
        lit32(&mut code, value);
        code.push(0);
        cases.push(case(
            "i32-boundary",
            COMPACT_SELECTOR,
            code,
            Ok(i64::from(value)),
            0,
            0,
        ));
    }
    for value in [0, 1, 0x7fff_ffff, 0x8000_0000, u32::MAX] {
        let mut code = Vec::new();
        lit_u32(&mut code, value);
        code.push(0);
        cases.push(case(
            "u32-positive",
            COMPACT_SELECTOR,
            code,
            Ok(i64::from(value)),
            0,
            0,
        ));
    }
    for value in [
        i64::MIN,
        i64::MAX,
        -0x1_0000_0001i64,
        0x1_0000_0001i64,
        -1,
        0,
    ] {
        let mut code = Vec::new();
        lit64(&mut code, value);
        code.push(0);
        cases.push(case(
            "i64-boundary",
            COMPACT_SELECTOR,
            code,
            Ok(value),
            0,
            0,
        ));
    }

    // Signed and unsigned literal meaning, signed division/modulo and wider
    // intermediates are all observable through the scalar result.
    for (name, left, right, operator, expected) in [
        ("u32-div-positive", 0xffff_fffeu64, 2u64, 11, 0x7fff_ffff),
        ("u32-mod-positive", 0xffff_fffe, 3, 12, 2),
        ("i64-div-negative", (-17i64) as u64, 5, 11, (-3i64) as u64),
        ("i64-mod-negative", (-17i64) as u64, 5, 12, (-2i64) as u64),
        (
            "wide-add",
            0x7fff_ffff_ffff_ffff,
            1,
            6,
            0x8000_0000_0000_0000,
        ),
        (
            "wide-multiply",
            0x1_0000_0001,
            0x1_0000_0001,
            10,
            0x2_0000_0001,
        ),
    ] {
        let mut code = Vec::new();
        if name.starts_with("u32") {
            lit_u32(&mut code, left as u32);
        } else {
            lit64(&mut code, left as i64);
        }
        if name.starts_with("u32") {
            lit8(&mut code, right as i8);
        } else {
            lit64(&mut code, right as i64);
        }
        code.extend([0x21, operator, 0]);
        cases.push(case(
            name,
            COMPACT_SELECTOR,
            code,
            Ok(expected as i64),
            0,
            0,
        ));
    }

    let mut code = Vec::new();
    lit8(&mut code, 7);
    op(&mut code, 0x30);
    code.push(0);
    cases.push(case("negate", COMPACT_SELECTOR, code, Ok(-7), 0, 0));
    for (name, left, right, opcode) in [
        ("add", 120i64, 8i64, 0x31),
        ("subtract", -120, 8, 0x32),
        ("multiply", -12, 3, 0x33),
        ("add-wrap", i64::MAX, 1, 0x31),
        ("subtract-wrap", i64::MIN, 1, 0x32),
        ("multiply-wrap", i64::MAX, 2, 0x33),
    ] {
        let mut code = Vec::new();
        lit64(&mut code, left);
        lit64(&mut code, right);
        op(&mut code, opcode);
        code.push(0);
        let expected = match opcode {
            0x31 => left.wrapping_add(right),
            0x32 => left.wrapping_sub(right),
            _ => left.wrapping_mul(right),
        };
        cases.push(case(name, COMPACT_SELECTOR, code, Ok(expected), 0, 0));
    }

    let mut fused = vec![0x11];
    lit8(&mut fused, 1);
    fused.push(0x31);
    fused.extend([0x12, 0, 0]);
    fused.push(0x33);
    fused.push(0);
    cases.push(CompactCase {
        name: "pc-symbol-fused",
        selector: COMPACT_SELECTOR,
        code: fused,
        expected: Ok((0x1234i64 + 1).wrapping_mul(2)),
        pc: 0x1234,
        symbol: 2,
        symbol_refs: 1,
    });

    // Full-width compact symbols, while canonical symbols stay zero-extended
    // from the low word. Alternating entrypoints detects evaluator-mode leaks.
    for value in [
        i64::MIN,
        -0x1_0000_0001,
        -7,
        -1,
        0,
        i64::from(u32::MAX),
        0x1_0000_0001,
        i64::MAX,
    ] {
        for selector in [COMPACT_SELECTOR, 2, COMPACT_SELECTOR] {
            let code = vec![0x12, 0, 0, 0];
            let expected = if selector == COMPACT_SELECTOR {
                value
            } else {
                i64::from(value as u32)
            };
            cases.push(case(
                "symbol-width-by-entry",
                selector,
                code,
                Ok(expected),
                value,
                1,
            ));
        }
    }

    // Dynamic symbol arithmetic, signed comparisons, and unsigned-looking
    // $fffffffe semantics through U32 (which must remain positive 4294967294).
    for (name, symbol, rhs, operator, expected) in [
        (
            "symbol-wide-add",
            0x1_0000_0001i64,
            2i64,
            6,
            0x1_0000_0003i64,
        ),
        ("symbol-div", -17, 5, 11, -3),
        ("symbol-mod", -17, 5, 12, -2),
        ("symbol-gt", 0x1_0000_0001i64, 0xffff_fffei64, 18, 1i64),
        ("symbol-lt", -0x1_0000_0001i64, -1i64, 20, 1i64),
        ("u32-div-positive", 0xffff_fffei64, 2i64, 11, 0x7fff_ffffi64),
    ] {
        let mut code = vec![0x12, 0, 0];
        if name == "u32-div-positive" {
            lit_u32(&mut code, rhs as u32);
        } else {
            lit64(&mut code, rhs);
        }
        code.extend([0x21, operator, 0]);
        cases.push(case(name, COMPACT_SELECTOR, code, Ok(expected), symbol, 1));
    }
    for (name, value, right, operator) in [
        ("bit-and", -1i64, 0x5a, 21),
        ("bit-or", i64::MIN, 0x55, 22),
        ("bit-xor", i64::MAX, -1, 23),
        ("shift-left", 3, 5, 13),
        ("shift-left-overflow", i64::MAX, 1, 13),
        ("shift-right", i64::MAX, 7, 14),
        ("shift-right-negative", -2, 1, 14),
        ("shift-count-wrap", i64::MIN, 64, 14),
        ("shift-count-negative", i64::MAX, -1, 14),
    ] {
        for selector in [COMPACT_SELECTOR, 2, COMPACT_SELECTOR] {
            let mut code = vec![0x12, 0, 0];
            if selector == COMPACT_SELECTOR {
                lit64(&mut code, right);
            } else {
                code.push(0x10);
                code.extend(right.to_le_bytes());
            }
            code.extend([0x21, operator, 0]);
            let left = if selector == COMPACT_SELECTOR {
                value
            } else {
                i64::from(value as u32)
            };
            let result = match operator {
                13 => left.wrapping_shl((right as u64 & 31) as u32),
                14 => ((left as u64).wrapping_shr((right as u64 & 31) as u32)) as i64,
                21 => left & right,
                22 => left | right,
                23 => left ^ right,
                _ => unreachable!(),
            };
            cases.push(case(name, selector, code, Ok(result), value, 1));
        }
    }
    for value in [i64::MIN, i64::MAX, -1] {
        for selector in [COMPACT_SELECTOR, 2, COMPACT_SELECTOR] {
            let left = if selector == COMPACT_SELECTOR {
                value
            } else {
                i64::from(value as u32)
            };
            cases.push(case(
                "bit-not-symbol",
                selector,
                vec![0x12, 0, 0, 0x20, 2, 0],
                Ok(!left),
                value,
                1,
            ));
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
        ("literal-i32-underflow", vec![0x15, 1, 2, 3], Err(53)),
        ("literal-u32-underflow", vec![0x16, 1, 2, 3], Err(53)),
        (
            "literal-i64-underflow",
            vec![0x17, 1, 2, 3, 4, 5, 6],
            Err(53),
        ),
        ("empty-end", vec![0], Err(56)),
        ("bad-end", vec![0x13, 1, 0, 0x13, 2], Err(56)),
        ("old-v2-rejected", vec![0x10, 1, 0, 0], Err(52)),
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
        cases.push(case(name, COMPACT_SELECTOR, code, expected, 0, 0));
    }

    let mut wide = Vec::new();
    wide.push(0x10);
    wide.extend(0x1_0000_0001i64.to_le_bytes());
    wide.push(0x10);
    wide.extend(2i64.to_le_bytes());
    wide.extend([0x21, 6, 0]);
    cases.push(case(
        "canonical-v2-after-compact",
        2,
        wide,
        Ok(0x1_0000_0003),
        0,
        0,
    ));
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
        input.extend((case.symbol as u32).to_be_bytes());
        if case.selector == COMPACT_SELECTOR {
            input.extend(((case.symbol as u64 >> 32) as u32).to_be_bytes());
        }
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
    assert!(cases.len() >= 40);
    assert!(input.len() < 8192);
    assert_eq!(output.len(), cases.len() * 24);
    assert!(cases
        .iter()
        .any(|case| matches!(case.expected, Ok(i64::MAX))));
    assert!(cases.iter().any(|case| matches!(case.expected, Err(1))));
}

#[test]
fn native_expression_compact_fs_uae() {
    let cases = compact_cases();
    let (input, expected) = compact_batch(&cases);
    match crate::fs_uae_smoke::run_exprvm_i64_harness_from_env(&workspace_root(), &input, &expected)
        .expect("compact full-i64 native proof")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
            eprintln!("PASS: {} compact/native entry cases", cases.len());
        }
    }
}
