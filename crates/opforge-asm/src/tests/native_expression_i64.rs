//! Signed scalar authority at the native evaluator boundary.
//! Native operator IDs are the existing bridge ABI, not Rust portable opcodes.
use super::*;
use opcore::expr::{apply_binary, apply_unary};
use opcore::parser::UnaryOp;

struct ScalarCase {
    name: String,
    code: Vec<u8>,
    expected: Result<i64, u32>,
    symbol_refs: u32,
}

fn literal(code: &mut Vec<u8>, value: i64) {
    code.push(0x10);
    code.extend(value.to_le_bytes());
}

fn scalar_cases() -> Vec<ScalarCase> {
    let mut cases = Vec::new();
    let binary_ops = [
        (BinaryOp::Multiply, 10),
        (BinaryOp::Divide, 11),
        (BinaryOp::Mod, 12),
        (BinaryOp::Power, 3),
        (BinaryOp::Shl, 13),
        (BinaryOp::Shr, 14),
        (BinaryOp::Add, 6),
        (BinaryOp::Subtract, 7),
        (BinaryOp::Eq, 15),
        (BinaryOp::Ne, 16),
        (BinaryOp::Ge, 17),
        (BinaryOp::Gt, 18),
        (BinaryOp::Le, 19),
        (BinaryOp::Lt, 20),
        (BinaryOp::BitAnd, 21),
        (BinaryOp::BitOr, 22),
        (BinaryOp::BitXor, 23),
        (BinaryOp::LogicAnd, 24),
        (BinaryOp::LogicOr, 8),
        (BinaryOp::LogicXor, 25),
    ];
    let pairs = [
        (0x1_0000_0001, 3),
        (-0x1_0000_0001, 7),
        (i64::MAX, 2),
        (i64::MIN, 1),
        (0x7fff_ffff_ffff, 0xffff_ffff),
        (0x8000_0000, -3),
        (0, 0),
        (-7, -3),
    ];
    for (op, id) in binary_ops {
        for (left, right) in pairs {
            let mut code = Vec::new();
            literal(&mut code, left);
            literal(&mut code, right);
            code.extend([0x21, id, 0]);
            cases.push(ScalarCase {
                name: format!("{left} {op:?} {right}"),
                code,
                expected: apply_binary(op, left, right, Span::default()).map_err(|_| 1),
                symbol_refs: 0,
            });
        }
    }
    for (op, id) in [
        (UnaryOp::Plus, 0),
        (UnaryOp::Minus, 1),
        (UnaryOp::BitNot, 2),
        (UnaryOp::LogicNot, 3),
        (UnaryOp::High, 4),
        (UnaryOp::Low, 5),
    ] {
        for value in [0, 0x1_0000_0000, -0x1_0000_0001, i64::MAX] {
            let mut code = Vec::new();
            literal(&mut code, value);
            code.extend([0x20, id, 0]);
            cases.push(ScalarCase {
                name: format!("{op:?} {value}"),
                code,
                expected: apply_unary(op, value, Span::default()).map_err(|_| 1),
                symbol_refs: 0,
            });
        }
    }
    for (op, id, left, right) in [
        (BinaryOp::Shl, 13, 0x1_8000_0001, 0),
        (BinaryOp::Shl, 13, 0x1_8000_0001, 31),
        (BinaryOp::Shl, 13, 0x1_8000_0001, 32),
        (BinaryOp::Shr, 14, -0x1_8000_0001, 0),
        (BinaryOp::Shr, 14, -0x1_8000_0001, 31),
        (BinaryOp::Shr, 14, -0x1_8000_0001, 32),
        (BinaryOp::Power, 3, 2, 63),
        (BinaryOp::Power, 3, 2, 0x1_0000_0000),
        (BinaryOp::Multiply, 10, 0xffff_ffff, 0xffff_ffff),
        (BinaryOp::Divide, 11, i64::MIN, i64::MIN),
        (BinaryOp::Mod, 12, i64::MIN, 3),
    ] {
        let mut code = Vec::new();
        literal(&mut code, left);
        literal(&mut code, right);
        code.extend([0x21, id, 0]);
        cases.push(ScalarCase {
            name: format!("boundary {left} {op:?} {right}"),
            code,
            expected: apply_binary(op, left, right, Span::default()).map_err(|_| 1),
            symbol_refs: 0,
        });
    }
    // Both Rust and native reject unrepresentable signed quotient/remainder.
    for id in [11, 12] {
        let mut code = Vec::new();
        literal(&mut code, i64::MIN);
        literal(&mut code, -1);
        code.extend([0x21, id, 0]);
        cases.push(ScalarCase {
            name: format!("overflow guard {id}"),
            code,
            expected: Err(1),
            symbol_refs: 0,
        });
    }
    // Both Rust and native fail closed for unrepresentable MIN negation.
    let mut code = Vec::new();
    literal(&mut code, i64::MIN);
    code.extend([0x20, 1, 0]);
    cases.push(ScalarCase {
        name: "minimum unary guard".into(),
        code,
        expected: Err(1),
        symbol_refs: 0,
    });
    // The condition has only its high word set. Keep both result words.
    for condition in [0, 0x1_0000_0000] {
        let mut code = Vec::new();
        for value in [condition, 0x2_0000_0001, -0x3_0000_0001] {
            literal(&mut code, value);
        }
        code.extend([0x21, 9, 0]);
        let expr = Expr::Ternary {
            cond: Box::new(Expr::Number(condition.to_string(), Span::default())),
            then_expr: Box::new(Expr::Number("8589934593".into(), Span::default())),
            else_expr: Box::new(Expr::Number("-12884901889".into(), Span::default())),
            span: Span::default(),
        };
        let context = opcore::expr::SimpleEvalContext::new(|_| None);
        cases.push(ScalarCase {
            name: format!("ternary {condition}"),
            code,
            expected: opcore::expr::eval_expr(&expr, &context).map_err(|_| 1),
            symbol_refs: 0,
        });
    }
    // Exercise symbol lookup after arithmetic has overwritten D1 scratch.
    let mut code = Vec::new();
    literal(&mut code, 0x1_0000_0001);
    literal(&mut code, 2);
    code.extend([0x21, 6]);
    code.extend([0x12, 0, 0]);
    code.extend([0x21, 6, 0]);
    cases.push(ScalarCase {
        name: "arithmetic then unsigned symbol".into(),
        code,
        expected: Ok(0x2_0000_0002),
        symbol_refs: 1,
    });
    cases.push(ScalarCase {
        name: "unsigned current address".into(),
        code: vec![0x11, 0],
        expected: Ok(0xffff_ffff),
        symbol_refs: 0,
    });
    // Native program-boundary contracts. These status values are native ABI
    // evidence, not a claim of equivalent Rust diagnostic numbering.
    for (name, code, status) in [
        ("empty", vec![], 51),
        ("unknown opcode", vec![0xff], 52),
        ("truncated literal", vec![0x10, 0], 53),
        ("empty end", vec![0], 56),
    ] {
        cases.push(ScalarCase {
            name: name.to_string(),
            code,
            expected: Err(status),
            symbol_refs: 0,
        });
    }
    let mut code = Vec::new();
    for value in 0..9 {
        literal(&mut code, value);
    }
    code.push(0);
    cases.push(ScalarCase {
        name: "stack overflow".into(),
        code,
        expected: Err(54),
        symbol_refs: 0,
    });
    cases
}

fn scalar_batch(cases: &[ScalarCase]) -> (Vec<u8>, Vec<u8>) {
    assert!(cases.len() <= 512);
    let mut input = (cases.len() as u32).to_be_bytes().to_vec();
    let mut output = Vec::new();
    for case in cases {
        assert!(case.code.len() <= 256, "{}", case.name);
        // Canonical representation selector; compact records use 0x8000.
        input.extend(1u16.to_be_bytes());
        input.extend((case.code.len() as u16).to_be_bytes());
        input.extend(u32::MAX.to_be_bytes());
        input.extend(u32::MAX.to_be_bytes());
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
fn native_expression_i64_live_rust_oracle() {
    // Level A: live Rust arithmetic authority plus bounded harness serialization.
    let cases = scalar_cases();
    let (input, output) = scalar_batch(&cases);
    assert!(cases.len() > 175);
    assert_eq!(output.len(), cases.len() * 24);
    assert!(input.len() < 140 * 1024);
    assert!(cases
        .iter()
        .any(|case| matches!(case.expected, Ok(value) if value > i64::from(u32::MAX))));
    assert!(cases
        .iter()
        .any(|case| matches!(case.expected, Ok(value) if value < i64::from(i32::MIN))));
}

#[test]
fn native_expression_i64_rust_division_overflow_domain() {
    // Level A: overflow is an explicit error, never a panic or successful value.
    for op in [BinaryOp::Divide, BinaryOp::Mod] {
        assert!(
            apply_binary(op, i64::MIN, -1, Span::default()).is_err(),
            "Rust overflow authority changed for {op:?}"
        );
    }
}

#[test]
fn native_expression_i64_rust_unary_minimum_domain() {
    assert!(apply_unary(UnaryOp::Minus, i64::MIN, Span::default()).is_err());
}

#[test]
fn native_expression_i64_fs_uae() {
    // Level D: actual dynamically generated cases, in-memory Rust oracle, fresh
    // guest protocol, explicit zero harness exit, exact output and cleanup.
    let cases = scalar_cases();
    let (input, expected) = scalar_batch(&cases);
    match crate::fs_uae_smoke::run_exprvm_i64_harness_from_env(&workspace_root(), &input, &expected)
        .expect("signed scalar native proof")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success && run.protocol_completed && run.exit_code == Some(0),
                "scalar harness failed: {}\n{}",
                run.stdout,
                run.stderr
            );
            eprintln!(
                "PASS: {} scalar cases, current native ExprVM contract; live Rust values plus explicit native failure contracts",
                cases.len()
            );
        }
    }
}

fn wide_bridge_source() -> String {
    let expressions = [
        "4294967296 != 0",
        "$1_0000_0000 != 0",
        "0x1_0000_0000 / 4294967296",
        "0X100000001 - 4294967296",
        "100000002h - 4294967296",
        "0o40000000003 - 4294967296",
        "40000000004q - 4294967296",
        "4294967301d - 4294967296",
        "%100000000000000000000000000000110 - 4294967296",
        "0b100000000000000000000000000000111 - 4294967296",
        "100000000000000000000000000001000b - 4294967296",
        "0B8H - 175",
        "(-4294967297 < -4294967296) + 9",
        "(2147483648 > 0) + 10",
        "(9223372036854775807 > 4294967296) + 11",
        "((-9223372036854775807 - 1) < 0) + 12",
        "((4294967297 * 4294967297) / 4294967296) + 12",
        "((4294967296 | 15) & 255)",
        "(4294967296 >> 28)",
        "(4294967296 ? 17 : 0)",
        "(0 ? 0 : 4294967314) - 4294967296",
        "(2 ** 32) / 4294967296 + 18",
        "(4294967296 && 1) + 19",
        "(4294967296 == 0) + 21",
        "(0_x100000000 != 0) + 21",
        "(100000000h_ != 0) + 22",
        "2a_h_ - 18",
        "(wide_symbol > 0) + 24",
        "((wide_symbol + 1) / 4294967296) + 25",
    ];
    let mut source = String::from("wide_symbol .const $ffffffff\n");
    source.extend(
        expressions
            .iter()
            .map(|expr| format!("        .byte ({expr})\n")),
    );
    source
}

fn wide_bridge_oracle(source: &str) -> Vec<u8> {
    let mut lines = vec![".cpu 65c02"];
    lines.extend(source.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("assemble live Rust wide bridge authority");
    assert!(
        diagnostics.is_empty(),
        "wide bridge Rust diagnostics: {diagnostics:?}"
    );
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_expression_i64_bridge_rust_oracle() {
    // Level A: fixture values exercise high authority rather than comparing two
    // equally truncated constants. Final emitted bytes remain representable.
    assert_eq!(
        wide_bridge_oracle(&wide_bridge_source()),
        [
            1, 1, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
            23, 24, 25, 26
        ]
    );
}

fn rust_scalar_text_value(text: &str) -> i64 {
    rust_scalar_text_result(text).expect("evaluate live Rust scalar text")
}

fn rust_scalar_text_result(text: &str) -> Result<i64, opcore::expr::EvalError> {
    let mut tokenizer = opcore::tokenizer::Tokenizer::new(text, 1);
    let mut tokens = Vec::new();
    let end = loop {
        let token = tokenizer
            .next_token()
            .map_err(|error| opcore::expr::EvalError::new(format!("{error:?}")))?;
        if matches!(token.kind, opcore::tokenizer::TokenKind::End) {
            break token.span;
        }
        tokens.push(token);
    };
    let expr = opcore::parser::Parser::parse_expr_from_tokens(tokens, end, None)
        .map_err(|error| opcore::expr::EvalError::new(format!("{error:?}")))?;
    opcore::expr::eval_expr(&expr, &opcore::expr::SimpleEvalContext::new(|_| None))
}

fn scalar_service_batch() -> (Vec<u8>, Vec<u8>) {
    let mut cases = Vec::new();
    for len in [16u16, 19, 20, 27, 28, 32, 36] {
        for text in ["0", "2147483648", "-2147483649", "4294967296"] {
            cases.push((len, text));
        }
    }
    for text in [
        "2147483647",
        "-2147483648",
        "9223372036854775807",
        "(-9223372036854775807 - 1)",
    ] {
        cases.push((28, text));
    }
    assert_eq!(cases.len(), 32);
    let expected_bytes = cases.len() * 64;
    let mut input = (cases.len() as u32).to_be_bytes().to_vec();
    let mut output = Vec::new();
    for (len, text) in cases {
        let value = rust_scalar_text_value(text);
        for word in [1, len, text.len() as u16, 0] {
            input.extend(word.to_be_bytes());
        }
        input.extend(0u32.to_be_bytes());
        input.extend(text.as_bytes());
        if text.len() % 2 != 0 {
            input.push(0);
        }
        let formatted = format!("VALUE {value}");
        assert!(formatted.len() <= 32);
        for word in [
            0u32,
            0,
            formatted.len() as u32,
            if len >= 20 { value as u32 } else { 0x1122_3344 },
            if len >= 28 {
                (value as u64 >> 32) as u32
            } else {
                0x5566_7788
            },
            if len >= 28 { 64 } else { 0x99aa_bbcc },
        ] {
            output.extend(word.to_be_bytes());
        }
        output.extend(formatted.as_bytes());
        output.resize(output.len() + 32 - formatted.len(), 0);
        output.extend(0xddee_ff00u32.to_be_bytes());
        output.extend(0xa1b2_c3d4u32.to_be_bytes());
    }
    assert_eq!(output.len(), expected_bytes);
    (input, output)
}

#[test]
fn native_expression_i64_service_rust_oracle() {
    // Level A: actual Rust text parsing, evaluation and signed formatting.
    assert_eq!(
        rust_scalar_text_value("(-9223372036854775807 - 1)"),
        i64::MIN
    );
    let (input, output) = scalar_service_batch();
    assert_eq!(&input[..4], &32u32.to_be_bytes());
    assert_eq!(output.len(), 2048);
}
