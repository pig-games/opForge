//! Canonical EXVM numeric compiler trees, including decoded string leaves.
use super::*;
use crate::fs_uae_smoke::FsUaeSmokeOutcome;
use opcore::parser::{BinaryOp, Expr, UnaryOp};
use opcore::tokenizer::{Span, TokenKind};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use vm::portable_contract::{PortableOperatorKind, PortableToken, PortableTokenKind};
use vm::runtime_portable_types::{PortableTokenizeRequest, PortableTokenizerByteStream};

const NODE_BYTES: usize = 24;
struct Case {
    tokens: Vec<u8>,
    program: Vec<u8>,
    arena: u16,
    steps: u32,
    lower_mode: u16,
    expected: Vec<u8>,
}

fn tk_constants() -> BTreeMap<String, u8> {
    let source = include_str!("../../../../native/motorola68000/amigaos/tkvm/tkvm_runtime.asm");
    source
        .lines()
        .filter_map(|line| {
            let (name, value) = line.split_once('=')?;
            let name = name.trim();
            name.starts_with("TK_KIND_").then(|| {
                (
                    name.to_owned(),
                    value.split(';').next().unwrap().trim().parse().unwrap(),
                )
            })
        })
        .collect()
}

fn packed_kind(kind: &PortableTokenKind, constants: &BTreeMap<String, u8>) -> u8 {
    use PortableTokenKind::*;
    let name = match kind {
        Identifier(_) | Register(_) => "IDENTIFIER",
        Number { .. } => "NUMBER",
        String { .. } => "STRING",
        Comma => "COMMA",
        Colon => "COLON",
        Dollar => "DOLLAR",
        Dot => "DOT",
        Hash => "HASH",
        Question => "QUESTION",
        At => "AT",
        OpenBracket => "OPEN_BRACKET",
        CloseBracket => "CLOSE_BRACKET",
        OpenBrace => "OPEN_BRACE",
        CloseBrace => "CLOSE_BRACE",
        OpenParen => "OPEN_PAREN",
        CloseParen => "CLOSE_PAREN",
        Operator(op) => match op {
            PortableOperatorKind::Range => "OP_RANGE",
            PortableOperatorKind::RangeInclusive => "OP_RANGE_INCLUSIVE",
            PortableOperatorKind::Plus => "OP_PLUS",
            PortableOperatorKind::Minus => "OP_MINUS",
            PortableOperatorKind::Multiply => "OP_MULTIPLY",
            PortableOperatorKind::Power => "OP_POWER",
            PortableOperatorKind::Divide => "OP_DIVIDE",
            PortableOperatorKind::Mod => "OP_MOD",
            PortableOperatorKind::Shl => "OP_SHL",
            PortableOperatorKind::Shr => "OP_SHR",
            PortableOperatorKind::BitNot => "OP_BIT_NOT",
            PortableOperatorKind::LogicNot => "OP_LOGIC_NOT",
            PortableOperatorKind::BitAnd => "OP_BIT_AND",
            PortableOperatorKind::BitOr => "OP_BIT_OR",
            PortableOperatorKind::BitXor => "OP_BIT_XOR",
            PortableOperatorKind::LogicAnd => "OP_LOGIC_AND",
            PortableOperatorKind::LogicOr => "OP_LOGIC_OR",
            PortableOperatorKind::LogicXor => "OP_LOGIC_XOR",
            PortableOperatorKind::Eq => "OP_EQ",
            PortableOperatorKind::Ne => "OP_NE",
            PortableOperatorKind::Ge => "OP_GE",
            PortableOperatorKind::Gt => "OP_GT",
            PortableOperatorKind::Le => "OP_LE",
            PortableOperatorKind::Lt => "OP_LT",
        },
    };
    constants[&format!("TK_KIND_{name}")]
}

fn tokenize(source: &str) -> Vec<PortableToken> {
    let model = load_opasm_model_from_package_bytes(&tkpkg_smoke_package_bytes());
    let program = model
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap();
    let policy = model.resolve_token_policy("m68020", None).unwrap();
    model
        .tokenize_with_vm_core(
            &PortableTokenizeRequest {
                family_id: "motorola68000",
                cpu_id: "m68020",
                dialect_id: "motorola68k",
                source_line: source,
                source_stream: PortableTokenizerByteStream::from_source_line(source),
                line_num: 1,
                token_policy: policy,
            },
            &program,
        )
        .unwrap()
}

// The numeric representation adapter binds names to ID7 and retains decoded strings.
// AST spans are numeric-token byte offsets, permitting exact tree comparisons.
fn numeric(source: &str) -> (Vec<u8>, Vec<opcore::tokenizer::Token>) {
    let constants = tk_constants();
    let mut packed = Vec::new();
    let mut core = Vec::new();
    for portable in tokenize(source) {
        let mut token = portable.to_core_token();
        let start = packed.len();
        packed.push(packed_kind(&portable.kind, &constants));
        match &portable.kind {
            PortableTokenKind::Identifier(_) | PortableTokenKind::Register(_) => {
                packed.extend([0, 7, 0]);
                token.kind = TokenKind::Identifier("bound".to_owned());
            }
            PortableTokenKind::Number { text, .. } => {
                let value = opcore::expression::parse_number_text(text, token.span).unwrap();
                packed.extend(value.to_be_bytes());
            }
            PortableTokenKind::String { bytes, .. } => {
                packed.push(u8::try_from(bytes.len()).unwrap());
                packed.extend(bytes);
            }
            _ => {}
        }
        token.span = Span {
            line: 1,
            col_start: start,
            col_end: packed.len(),
        };
        core.push(token);
    }
    (packed, core)
}

fn unary(op: UnaryOp) -> u8 {
    use package::ExvmOperatorKind as O;
    (match op {
        UnaryOp::Plus => O::Plus,
        UnaryOp::Minus => O::Minus,
        UnaryOp::BitNot => O::BitNot,
        UnaryOp::LogicNot => O::LogicNot,
        UnaryOp::Low => O::Lt,
        UnaryOp::High => O::Gt,
    }) as u8
}
fn binary(op: BinaryOp) -> u8 {
    use package::ExvmOperatorKind as O;
    (match op {
        BinaryOp::Add => O::Plus,
        BinaryOp::Subtract => O::Minus,
        BinaryOp::Multiply => O::Multiply,
        BinaryOp::Divide => O::Divide,
        BinaryOp::Mod => O::Mod,
        BinaryOp::Power => O::Power,
        BinaryOp::Shl => O::Shl,
        BinaryOp::Shr => O::Shr,
        BinaryOp::Eq => O::Eq,
        BinaryOp::Ne => O::Ne,
        BinaryOp::Ge => O::Ge,
        BinaryOp::Gt => O::Gt,
        BinaryOp::Le => O::Le,
        BinaryOp::Lt => O::Lt,
        BinaryOp::BitAnd => O::BitAnd,
        BinaryOp::BitOr => O::BitOr,
        BinaryOp::BitXor => O::BitXor,
        BinaryOp::LogicAnd => O::LogicAnd,
        BinaryOp::LogicOr => O::LogicOr,
        BinaryOp::LogicXor => O::LogicXor,
    }) as u8
}
fn tree(expr: &Expr, nodes: &mut Vec<u8>) -> u32 {
    let (kind, op, count, first, second, span) = match expr {
        Expr::Number(text, span) => (
            1,
            0,
            0,
            opcore::expression::parse_number_text(text, *span).unwrap(),
            0,
            *span,
        ),
        Expr::String(bytes, span) => {
            assert!((1..=2).contains(&bytes.len()));
            let scalar = bytes
                .iter()
                .fold(0u32, |value, byte| (value << 8) | u32::from(*byte));
            (1, 0, 0, scalar, 0, *span)
        }
        Expr::Identifier(_, span) => (2, 0, 0, 7, 0, *span),
        Expr::Dollar(span) => (3, 0, 0, 0, 0, *span),
        Expr::Unary { op, expr, span } => (4, unary(*op), 1, tree(expr, nodes), 0, *span),
        Expr::Binary {
            op,
            left,
            right,
            span,
        } => {
            let first = tree(left, nodes);
            let second = tree(right, nodes);
            (5, binary(*op), 2, first, second, *span)
        }
        _ => panic!("unsupported scalar oracle {expr:?}"),
    };
    let offset = nodes.len() as u32;
    nodes.extend([kind, op]);
    nodes.extend((count as u16).to_be_bytes());
    for word in [first, second, 0, u32::MAX] {
        nodes.extend(word.to_be_bytes());
    }
    nodes.extend((span.col_start as u16).to_be_bytes());
    nodes.extend((span.col_end as u16).to_be_bytes());
    assert_eq!(nodes.len() % NODE_BYTES, 0);
    offset
}
// Opcode/unary values are shared. Native binary IDs retain the existing evaluator
// representation and are read from its named constants, rather than Rust IDs.
fn native_constant(name: &str) -> u8 {
    include_str!("../../../../native/motorola68000/amigaos/exprvm/exprvm_runtime.asm")
        .lines()
        .find_map(|line| {
            let (key, value) = line.split_once('=')?;
            (key.trim() == name).then(|| value.trim().parse().unwrap())
        })
        .unwrap()
}
fn postfix(expr: &Expr, bytes: &mut Vec<u8>) {
    use opcore::expr_vm::{ExprVmOpcodeV2 as O, ExprVmUnary as U};
    match expr {
        Expr::Number(text, span) => {
            bytes.push(O::PushLiteral as u8);
            bytes.extend(
                i64::from(opcore::expression::parse_number_text(text, *span).unwrap())
                    .to_le_bytes(),
            );
        }
        Expr::String(value, _) => {
            assert!((1..=2).contains(&value.len()));
            let scalar = value
                .iter()
                .fold(0i64, |scalar, byte| (scalar << 8) | i64::from(*byte));
            bytes.push(O::PushLiteral as u8);
            bytes.extend(scalar.to_le_bytes());
        }
        Expr::Identifier(_, _) => {
            bytes.push(O::PushSymbol as u8);
            bytes.extend(7u16.to_le_bytes());
        }
        Expr::Dollar(_) => bytes.push(O::PushCurrentAddress as u8),
        Expr::Unary { op, expr, .. } => {
            postfix(expr, bytes);
            bytes.push(O::ApplyUnary as u8);
            bytes.push(match op {
                UnaryOp::Plus => U::Plus,
                UnaryOp::Minus => U::Minus,
                UnaryOp::BitNot => U::BitNot,
                UnaryOp::LogicNot => U::LogicNot,
                UnaryOp::Low => U::Low,
                UnaryOp::High => U::High,
            } as u8);
        }
        Expr::Binary {
            op, left, right, ..
        } => {
            postfix(left, bytes);
            postfix(right, bytes);
            bytes.push(O::ApplyBinary as u8);
            let suffix = match op {
                BinaryOp::Add => "ADD",
                BinaryOp::Subtract => "SUBTRACT",
                BinaryOp::Multiply => "MULTIPLY",
                BinaryOp::Divide => "DIVIDE",
                BinaryOp::Mod => "MOD",
                BinaryOp::Power => "POWER",
                BinaryOp::Shl => "SHIFT_LEFT",
                BinaryOp::Shr => "SHIFT_RIGHT",
                BinaryOp::Eq => "EQ",
                BinaryOp::Ne => "NE",
                BinaryOp::Ge => "GE",
                BinaryOp::Gt => "GT",
                BinaryOp::Le => "LE",
                BinaryOp::Lt => "LT",
                BinaryOp::BitAnd => "BIT_AND",
                BinaryOp::BitOr => "BIT_OR",
                BinaryOp::BitXor => "BIT_XOR",
                BinaryOp::LogicAnd => "LOGIC_AND",
                BinaryOp::LogicOr => "LOGIC_OR",
                BinaryOp::LogicXor => "LOGIC_XOR",
            };
            bytes.push(native_constant(&format!("EXPRVM_BINARY_{suffix}")));
        }
        _ => panic!("unsupported postfix oracle {expr:?}"),
    }
}
fn lower_result(expected: &mut Vec<u8>, status: u32, payload: &[u8]) {
    expected.extend(status.to_be_bytes());
    expected.extend((payload.len() as u32).to_be_bytes());
    expected.extend(payload);
    if !expected.len().is_multiple_of(2) {
        expected.push(0);
    }
}
fn success(source: &str) -> Case {
    let (tokens, core) = numeric(source);
    let end = Span {
        line: 1,
        col_start: tokens.len(),
        col_end: tokens.len(),
    };
    let ast = vm::vm_opcore::parse_expression_tokens(core, end, None).unwrap();
    let mut nodes = Vec::new();
    let root = tree(&ast, &mut nodes);
    let mut expected = Vec::new();
    for word in [0, tokens.len() as u32, root, nodes.len() as u32] {
        expected.extend(word.to_be_bytes());
    }
    expected.extend(nodes);
    let mut payload = Vec::new();
    postfix(&ast, &mut payload);
    payload.push(opcore::expr_vm::ExprVmOpcodeV2::End as u8);
    lower_result(&mut expected, 0, &payload);
    Case {
        tokens,
        program: vm::vm_opcore::expression_parser_program().to_vec(),
        arena: 3072,
        steps: 8192,
        lower_mode: 0,
        expected,
    }
}
fn failure(
    tokens: Vec<u8>,
    program: Vec<u8>,
    arena: u16,
    steps: u32,
    status: u32,
    consumed: u32,
) -> Case {
    let mut expected = Vec::new();
    for word in [status, consumed, u32::MAX, 0] {
        expected.extend(word.to_be_bytes());
    }
    lower_result(&mut expected, 1, &[]);
    Case {
        tokens,
        program,
        arena,
        steps,
        lower_mode: 0,
        expected,
    }
}
fn batch() -> (Vec<u8>, Vec<u8>) {
    let mut cases: Vec<_> = [
        "1+2*3",
        "2**3**2",
        "(1+2)*3",
        "value+(3*2)",
        "(3*2)+value",
        "($-$)+(3*2)",
        "value-value+(3*2)",
        "value+$",
        "-~+1",
        "<value",
        ">value",
        "<value+1",
        ">value+256",
        "!0",
        "1<2",
        "1>=2",
        "1==2",
        "1!=2",
        "1&&2||3",
        "1^^2",
        "1<<2|3&4^5",
        "4294967295",
        "'A'",
        "'AB'",
        "'\\n'",
    ]
    .into_iter()
    .map(success)
    .collect();
    // Both native bound-name wire forms are IDs; neither reclassifies a CPU register.
    let mut alternate_name = success("value+1");
    alternate_name.tokens[0] = 1;
    cases.push(alternate_name);
    cases.push(success(&["1"; 20].join("+")));
    cases.push(success(&["value"; 8].join("**")));
    let mut stack_overflow = success(&["value"; 10].join("**"));
    let used = u32::from_be_bytes(stack_overflow.expected[12..16].try_into().unwrap()) as usize;
    stack_overflow.expected.truncate(16 + used);
    // Scratch output is uncommitted on failure; the harness publishes no bytes.
    lower_result(&mut stack_overflow.expected, 3, &[]);
    cases.push(stack_overflow);
    cases.push(success(&format!("{}1{}", "(".repeat(16), ")".repeat(16))));
    for mode in 1..=4 {
        let mut case = success("1+2");
        let used = u32::from_be_bytes(case.expected[12..16].try_into().unwrap()) as usize;
        case.expected.truncate(16 + used);
        lower_result(&mut case.expected, if mode == 4 { 2 } else { 1 }, &[]);
        case.lower_mode = mode;
        cases.push(case);
    }
    let canonical = vm::vm_opcore::expression_parser_program().to_vec();
    // Missing, odd, short and wrapping workspace must fail before use.
    for mode in 5..=8 {
        let mut case = failure(numeric("1").0, canonical.clone(), 768, 8192, 3, 0);
        case.lower_mode = mode;
        cases.push(case);
    }
    for (source, cursor) in [("1+", 6), ("(1", 6), ("'ABC'", 0), ("''", 0)] {
        cases.push(failure(
            numeric(source).0,
            canonical.clone(),
            768,
            8192,
            1,
            cursor,
        ));
    }
    cases.push(failure(
        numeric("1..2").0,
        canonical.clone(),
        768,
        8192,
        6,
        11,
    ));
    cases.push(failure(
        numeric("1?2:3").0,
        canonical.clone(),
        768,
        8192,
        6,
        17,
    ));
    cases.push(failure(
        numeric(&format!("{}1{}", "(".repeat(17), ")".repeat(17))).0,
        canonical.clone(),
        768,
        8192,
        4,
        16,
    ));
    let mut prefix = success("1");
    prefix.tokens = numeric("1,2").0;
    cases.push(prefix);
    let literal = numeric("1").0;
    cases.push(failure(literal.clone(), vec![4], 768, 8192, 2, 0));
    cases.push(failure(literal.clone(), vec![1, 0, 0], 768, 3, 5, 0));
    cases.push(failure(literal.clone(), vec![], 768, 8192, 2, 0));
    // Unassigned opcodes must stay invalid on either side of each table range.
    for opcode in [0x05, 0x44, 0x71, 0x73, 0xff] {
        cases.push(failure(literal.clone(), vec![opcode], 768, 8192, 2, 0));
    }
    cases.push(failure(
        literal.clone(),
        vec![3, 0xff, 0xff],
        768,
        8192,
        2,
        0,
    ));
    cases.push(failure(literal.clone(), vec![3, 0], 768, 8192, 2, 0));
    cases.push(failure(literal.clone(), canonical.clone(), 768, 0, 5, 0));
    cases.push(failure(literal, canonical.clone(), 0, 8192, 3, 0));
    cases.push(failure(numeric("1+2").0, canonical.clone(), 24, 8192, 3, 6));
    cases.push(failure(
        vec![tk_constants()["TK_KIND_NUMBER"], 0],
        canonical.clone(),
        768,
        8192,
        1,
        0,
    ));
    cases.push(failure(
        vec![tk_constants()["TK_KIND_IDENTIFIER"], 0, 7, 1],
        canonical.clone(),
        768,
        8192,
        1,
        0,
    ));
    cases.push(failure(
        numeric("{1}").0,
        canonical.clone(),
        768,
        8192,
        6,
        0,
    ));
    cases.push(failure(numeric("1[0]").0, canonical, 768, 8192, 6, 5));
    let mut input = (cases.len() as u32).to_be_bytes().to_vec();
    let mut expected = Vec::new();
    for case in cases {
        input.extend((case.tokens.len() as u16).to_be_bytes());
        input.extend((case.program.len() as u16).to_be_bytes());
        input.extend(case.arena.to_be_bytes());
        input.extend(case.lower_mode.to_be_bytes());
        input.extend(case.steps.to_be_bytes());
        input.extend(case.tokens);
        input.extend(case.program);
        if !input.len().is_multiple_of(2) {
            input.push(0);
        }
        expected.extend(case.expected);
    }
    assert!(input.len() <= 65536);
    assert!(expected.len() <= 16384);
    (input, expected)
}

struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}
fn assemble(root: &Path, input: &[u8]) -> (Scratch, Vec<u8>, String) {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let path = std::env::temp_dir().join(format!(
        "opforge-exvm-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&path).unwrap();
    let scratch = Scratch(path);
    std::fs::write(scratch.0.join("binary-exvm-cases.bin"), input).unwrap();
    let source =
        std::fs::read_to_string(root.join(
            "native/motorola68000/amigaos/test-harnesses/experimental/binary_exvm_harness.asm",
        ))
        .unwrap();
    let entry = scratch.0.join("entry.asm");
    std::fs::write(&entry, &source).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_owned(),
        entry.to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        root.join("native/motorola68000")
            .to_string_lossy()
            .into_owned(),
        "-I".into(),
        root.join("native/motorola68000/amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(scratch.0.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => {
            panic!(
                "native EXVM component: {error}; diagnostics: {:?}",
                error.diagnostics()
            )
        }
        cli_core::CliRunError::Workflow { error, .. } => panic!("native EXVM component: {error}"),
        _ => panic!("native EXVM component warnings"),
    });
    let image = std::fs::read(scratch.0.join("binary-exvm.hunk")).unwrap();
    (scratch, image, source)
}

#[test]
fn native_expression_compiler_live_canonical_tree_oracle() {
    let (input, expected) = batch();
    assert!(input.len() > 1024 && expected.len() > 1024);
    let core: Vec<_> = tokenize("'AB'")
        .iter()
        .map(PortableToken::to_core_token)
        .collect();
    assert!(
        matches!(vm::vm_opcore::parse_expression_tokens(core, Span::default(), None).unwrap(), Expr::String(bytes, _) if bytes == b"AB")
    );
}
#[test]
fn native_expression_compiler_host_assembles() {
    let (input, _) = batch();
    assert!(!assemble(&workspace_root(), &input).1.is_empty());
}
#[test]
#[ignore = "requires configured FS-UAE; one fresh scalar compiler tree batch"]
fn native_expression_compiler_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let root = workspace_root();
    let (input, expected) = batch();
    let (_scratch, image, source) = assemble(&root, &input);
    let case = OpforgeNativeCliParityCase {
        name: "canonical-exvm-numeric-compiler",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/binary-exvm-nodes.bin",
            rust_oracle: &expected,
        },
    };
    let result = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh compiler batch completion");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
}
