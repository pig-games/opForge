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
const TYPED_LOWER: u16 = 9;
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
                let value = opcore::expr::parse_number(text).unwrap() as u64;
                if let Ok(small) = u32::try_from(value) {
                    packed.extend(small.to_be_bytes());
                } else {
                    // Raw transport tag, independent of the canonical grammar category.
                    *packed.last_mut().unwrap() = 64;
                    packed.extend(value.to_be_bytes());
                }
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
fn tree(expr: &Expr, nodes: &mut Vec<u8>, core: &[opcore::tokenizer::Token]) -> u32 {
    let mut third = 0;
    let (kind, op, count, first, second, span) = match expr {
        Expr::Number(text, span) => {
            let value = opcore::expr::parse_number(text).unwrap() as u64;
            (1, 0, 0, value as u32, (value >> 32) as u32, *span)
        }
        Expr::String(bytes, span) => {
            assert!((1..=2).contains(&bytes.len()));
            let scalar = bytes
                .iter()
                .fold(0u32, |value, byte| (value << 8) | u32::from(*byte));
            (1, 0, 0, scalar, 0, *span)
        }
        Expr::Identifier(_, span) => (2, 0, 0, 7, 0, *span),
        Expr::Dollar(span) => (3, 0, 0, 0, 0, *span),
        Expr::Unary { op, expr, span } => (4, unary(*op), 1, tree(expr, nodes, core), 0, *span),
        Expr::Binary {
            op,
            left,
            right,
            span,
        } => {
            let first = tree(left, nodes, core);
            let second = tree(right, nodes, core);
            (5, binary(*op), 2, first, second, *span)
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
            span,
        } => {
            let first = tree(cond, nodes, core);
            let second = tree(then_expr, nodes, core);
            third = tree(else_expr, nodes, core);
            (11, 0, 3, first, second, *span)
        }
        Expr::Range {
            start,
            end,
            step,
            inclusive,
            span,
        } => {
            let first = tree(start, nodes, core);
            let second = tree(end, nodes, core);
            third = step
                .as_ref()
                .map_or(u32::MAX, |step| tree(step, nodes, core));
            (
                6,
                u8::from(*inclusive) | (u8::from(step.is_some()) << 1),
                if step.is_some() { 3 } else { 2 },
                first,
                second,
                *span,
            )
        }
        Expr::List(items, span) => (7, 0, items.len(), children(items, nodes, core), 0, *span),
        Expr::Index { base, index, span } => {
            let first = tree(base, nodes, core);
            let second = tree(index, nodes, core);
            (8, 0, 2, first, second, *span)
        }
        Expr::Call { args, span, .. } => (9, 0, args.len(), 7, children(args, nodes, core), *span),
        Expr::Member { base, span, .. } => (10, 0, 1, tree(base, nodes, core), 7, *span),
        Expr::StructLiteral { fields, span, .. } => {
            // EXVM builds the type identifier before entering the envelope and
            // reuses that allocation for the final struct root.
            let root = append_node(nodes, 12, 0, fields.len(), 7, u32::MAX, 0, *span);
            let mut head = u32::MAX;
            let mut previous = None;
            for (_, value) in fields {
                let subtree_start = nodes.len();
                let child = tree(value, nodes, core);
                let value_start = nodes[subtree_start..]
                    .chunks_exact(NODE_BYTES)
                    .map(|node| u16::from_be_bytes(node[20..22].try_into().unwrap()) as usize)
                    .min()
                    .unwrap();
                // Rust's AST omits field-name spans. Locate the key immediately
                // before the shared AST value subtree, including grouped values.
                let colon = core
                    .iter()
                    .rposition(|token| {
                        token.span.col_start < value_start && matches!(token.kind, TokenKind::Colon)
                    })
                    .unwrap();
                let field_span = Span {
                    line: span.line,
                    col_start: core[colon - 1].span.col_start,
                    col_end: u16::from_be_bytes(
                        nodes[child as usize + 22..child as usize + 24]
                            .try_into()
                            .unwrap(),
                    ) as usize,
                };
                let field = append_node(nodes, 13, 0, 1, 7, child, 0, field_span);
                if let Some(previous) = previous {
                    nodes[previous + 16..previous + 20].copy_from_slice(&field.to_be_bytes());
                } else {
                    head = field;
                }
                previous = Some(field as usize);
            }
            nodes[root as usize + 8..root as usize + 12].copy_from_slice(&head.to_be_bytes());
            return root;
        }
        _ => panic!("unsupported tree oracle {expr:?}"),
    };
    append_node(nodes, kind, op, count, first, second, third, span)
}
#[allow(clippy::too_many_arguments)]
fn append_node(
    nodes: &mut Vec<u8>,
    kind: u8,
    op: u8,
    count: usize,
    first: u32,
    second: u32,
    third: u32,
    span: Span,
) -> u32 {
    let offset = nodes.len() as u32;
    nodes.extend([kind, op]);
    nodes.extend((count as u16).to_be_bytes());
    for word in [first, second, third, u32::MAX] {
        nodes.extend(word.to_be_bytes());
    }
    nodes.extend((span.col_start as u16).to_be_bytes());
    nodes.extend((span.col_end as u16).to_be_bytes());
    assert_eq!(nodes.len() % NODE_BYTES, 0);
    offset
}
// Only child roots are siblings; nested children retain their own links.
fn children(items: &[Expr], nodes: &mut Vec<u8>, core: &[opcore::tokenizer::Token]) -> u32 {
    let mut head = u32::MAX;
    let mut previous = None;
    for item in items {
        let offset = tree(item, nodes, core);
        if let Some(previous) = previous {
            nodes[previous + 16..previous + 20].copy_from_slice(&offset.to_be_bytes());
        } else {
            head = offset;
        }
        previous = Some(offset as usize);
    }
    head
}
fn scalar_tree(expr: &Expr) -> bool {
    match expr {
        Expr::Number(..) | Expr::String(..) | Expr::Identifier(..) | Expr::Dollar(..) => true,
        Expr::Unary { expr, .. } => scalar_tree(expr),
        Expr::Binary { left, right, .. } => scalar_tree(left) && scalar_tree(right),
        _ => false,
    }
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
    use opcore::expr_vm::{ExprVmOpcode as O, ExprVmUnary as U};
    match expr {
        Expr::Number(text, _) => {
            bytes.push(O::PushLiteral as u8);
            bytes.extend(opcore::expr::parse_number(text).unwrap().to_le_bytes());
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
    let ast = vm::vm_opcore::parse_expression_tokens(core.clone(), end, None).unwrap();
    let mut nodes = Vec::new();
    let root = tree(&ast, &mut nodes, &core);
    let mut expected = Vec::new();
    for word in [0, tokens.len() as u32, root, nodes.len() as u32] {
        expected.extend(word.to_be_bytes());
    }
    expected.extend(nodes);
    let mut payload = Vec::new();
    if scalar_tree(&ast) {
        postfix(&ast, &mut payload);
        payload.push(opcore::expr_vm::ExprVmOpcode::End as u8);
        lower_result(&mut expected, 0, &payload);
    } else {
        // Ternary and compound trees use canonical typed lowering.
        lower_result(&mut expected, 1, &[]);
    }
    Case {
        tokens,
        program: vm::vm_opcore::expression_parser_program().to_vec(),
        arena: 3072,
        steps: 8192,
        lower_mode: 0,
        expected,
    }
}
fn typed_success(source: &str) -> Case {
    let mut case = success(source);
    let used = u32::from_be_bytes(case.expected[12..16].try_into().unwrap()) as usize;
    case.expected.truncate(16 + used);
    let (_, core) = numeric(source);
    let ast = vm::vm_opcore::parse_expression_tokens(core, Span::default(), None).unwrap();
    let program =
        opcore::expr_vm::compile_core_expr_to_portable_program_shape_preserving(&ast).unwrap();
    // Every lexical name is bound to numeric ID7 by this representation adapter.
    let code = &program.code;
    use opcore::expr_vm::ExprVmOpcode as O;
    let mut payload = Vec::new();
    let mut ip = 0;
    while ip < code.len() {
        let op = code[ip];
        payload.push(op);
        ip += 1;
        let opcode = O::from_u8(op).unwrap();
        let width = match opcode {
            O::PushLiteral => 8,
            O::PushSymbol | O::BuildList | O::GetMember => 2,
            O::BuildStructLiteral => {
                let count = u16::from_le_bytes(code[ip + 2..ip + 4].try_into().unwrap());
                4 + usize::from(count) * 2
            }
            O::ApplyUnary | O::ApplyBinary | O::BuildRange => 1,
            O::End
            | O::PushCurrentAddress
            | O::SelectTernary
            | O::IndexValue
            | O::RequireScalar => 0,
            _ => panic!("unsupported typed oracle opcode {opcode:?}"),
        };
        if matches!(opcode, O::PushSymbol | O::GetMember) {
            payload.extend(7u16.to_le_bytes());
        } else if opcode == O::BuildStructLiteral {
            payload.extend(7u16.to_le_bytes());
            payload.extend(&code[ip + 2..ip + 4]);
            for _ in 0..(width - 4) / 2 {
                payload.extend(7u16.to_le_bytes());
            }
        } else {
            payload.extend(&code[ip..ip + width]);
        }
        ip += width;
    }
    lower_result(&mut case.expected, 0, &payload);
    case.lower_mode = TYPED_LOWER;
    case
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
        "4294967296",
        "$123456789abcdef",
        "9223372036854775807-4294967296",
        "-9223372036854775807-1",
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
    // Every truncated wide payload rejects before the grammar consumes it.
    let wide = numeric("4294967296").0;
    for length in 1..wide.len() {
        cases.push(failure(
            wide[..length].to_vec(),
            canonical.clone(),
            768,
            8192,
            1,
            0,
        ));
    }
    let mut overflowing = vec![64];
    overflowing.extend((i64::MAX as u64 + 1).to_be_bytes());
    cases.push(failure(overflowing, canonical.clone(), 768, 8192, 1, 0));
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
    cases.extend(
        [
            "{}",
            "{1}",
            "{1,2,3}",
            "{-1,(2+3),+4}",
            "{{1},{2,3}}",
            "1..2",
            "1..=3",
            "4..0:-2",
            "0..=6:2",
            "(0..=6:2)[1]",
            "value[0]",
            "{1,2}[1]",
            "{{1,2}}[0][1]",
            ".len({1,2})",
            ".bound()",
            ".bound(1,2,3)",
            ".bound(.bound({}),(-1))",
        ]
        .into_iter()
        .map(success),
    );
    cases.push(success(&format!("{}1{}", "{".repeat(16), "}".repeat(16))));
    // Child roots leave OUTPUTS immediately: wide envelopes must not consume
    // one output-stack slot per item, even beyond its 64-slot capacity.
    let wide = vec!["1"; 65].join(",");
    for source in [format!("{{{wide}}}"), format!(".bound({wide})")] {
        let mut case = success(&source);
        case.steps = 65536;
        cases.push(case);
    }
    // Compound parents need their own node after all children have succeeded.
    for (source, arena) in [("{1}", 24), ("1[0]", 48), ("1..2", 48), (".bound(1)", 24)] {
        let tokens = numeric(source).0;
        let consumed = tokens.len() as u32;
        cases.push(failure(tokens, canonical.clone(), arena, 8192, 3, consumed));
    }
    let mixed = |depth| {
        (0..depth).fold("value[0]".to_owned(), |source, level| match level % 3 {
            0 => format!("{{{source}}}"),
            1 => format!(".bound({source})"),
            _ => format!("({source})"),
        })
    };
    // Fifteen enclosing envelopes plus the innermost index reach depth sixteen.
    cases.push(success(&mixed(15)));
    let (tokens, core) = numeric(&mixed(16));
    let consumed = core
        .iter()
        .find(|token| matches!(token.kind, TokenKind::OpenBracket))
        .unwrap()
        .span
        .col_start as u32;
    cases.push(failure(tokens, canonical.clone(), 3072, 8192, 4, consumed));
    // Delimiter errors retain the exact bounded prefix consumed before failure.
    for (source, cursor) in [
        ("{1", 6),
        ("{1,}", 7),
        ("{1 2}", 6),
        ("1[", 6),
        ("1[0", 11),
        ("1[0,1]", 11),
        (".bound", 5),
        (".bound(1", 11),
        (".bound(1,)", 12),
        (".bound(1 2)", 11),
    ] {
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
        numeric(&format!("{}1{}", "{".repeat(17), "}".repeat(17))).0,
        canonical.clone(),
        768,
        8192,
        4,
        17,
    ));
    for expression in [
        "1?2:3",
        "0?1:0?2:3",
        "1?0?4:5:6",
        "1||0?2+3:4*5",
        "value?{1,2}:{3}",
        "(value?{1}:{2})[0]",
    ] {
        cases.push(success(expression));
    }
    for expression in [
        "1?2:3",
        "0?1:0?2:3",
        "value?{1,2}:{3}",
        "(value?{1}:{2})[0]",
        "value?(1..=3):(9..3:-2)",
    ] {
        cases.push(typed_success(expression));
    }
    // Ternary owns a fourth node after its three leaves.
    cases.push(failure(
        numeric("1?2:3").0,
        canonical.clone(),
        72,
        8192,
        3,
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
    cases.push(failure(literal.clone(), vec![0x43, 4], 768, 8192, 2, 0));
    // A valid literal followed by ADVANCE exhausts the program without END.
    // The next fetch must reject at the exact byte boundary, after consuming it.
    cases.push(failure(
        literal.clone(),
        vec![
            package::ExvmOpcode::BuildNumber as u8,
            package::ExvmOpcode::Advance as u8,
        ],
        768,
        8192,
        2,
        literal.len() as u32,
    ));
    // Targets are offsets within the program, never its one-past-end boundary.
    cases.push(failure(literal.clone(), vec![1, 3, 0], 768, 8192, 2, 0));
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
    // Opaque function IDs are preserved by the compiler, independent of names.
    let mut opaque_call = success(".bound()");
    opaque_call.tokens[2..4].copy_from_slice(&123u16.to_be_bytes());
    opaque_call.expected[20..24].copy_from_slice(&123u32.to_be_bytes());
    cases.push(opaque_call);
    pack_batch(cases)
}
fn pack_batch(cases: Vec<Case>) -> (Vec<u8>, Vec<u8>) {
    assert!(cases.len() <= 128);
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
    assert!(input.len() <= 98304, "compiler input bytes={}", input.len());
    assert!(
        expected.len() <= 16384,
        "compiler output bytes={}",
        expected.len()
    );
    (input, expected)
}

// Separate bounded batch: the existing scalar/compound oracle fills its harness.
fn struct_batch() -> (Vec<u8>, Vec<u8>) {
    let mut cases: Vec<_> = [
        "Point{}",
        "Point{x:1}",
        "Point{x:1,y:2}",
        "Point{Y:2,X:1}",
        "Point{x:(1+2)*3}",
        "Point{x:1?2:3,y:4..=8:2}",
        "Point{x:{1,2},y:Point{x:3}}.y.x",
        "Point{x:{Point{x:4}}}.x[0].x",
        "{Point{x:1},Point{x:2}}[1].x",
        "value?Point{x:1}:Point{x:2}",
        "Point{x:Point{y:5}.y}",
        // Grammar permits duplicate keys; typed construction owns validation.
        "Point{x:1,x:2}",
    ]
    .into_iter()
    .map(typed_success)
    .collect();
    cases.push(success("Point{x:1}")); // scalar lowering rejects a struct tree
                                       // The canonical action only attaches braces to identifiers/registers.
    let mut numeric_prefix = success("1");
    numeric_prefix.tokens = numeric("1{x:2}").0;
    cases.push(numeric_prefix);
    let mut alternate_names = typed_success("Point{x:1}");
    alternate_names.tokens[0] = 1;
    alternate_names.tokens[5] = 1;
    cases.push(alternate_names);
    let mut accepted_key = typed_success("Point{x:1}");
    accepted_key.tokens[8] = 0xfe; // accepted AST field metadata retains its role
    cases.push(accepted_key);
    // Preserve opaque nominal/member IDs independently, across both AST and wire.
    let mut opaque = typed_success("Point{x:1,y:2}");
    for (start, identity) in [(1, 123u16), (6, 11), (17, 12)] {
        opaque.tokens[start..start + 2].copy_from_slice(&identity.to_be_bytes());
    }
    let used = u32::from_be_bytes(opaque.expected[12..16].try_into().unwrap()) as usize;
    assert_eq!(used, 5 * NODE_BYTES);
    for (node, identity) in [(0, 123u32), (48, 11), (96, 12)] {
        opaque.expected[16 + node + 4..16 + node + 8].copy_from_slice(&identity.to_be_bytes());
    }
    let payload = &mut opaque.expected[16 + used + 8..];
    // Two canonical literal instructions precede the variable struct operand.
    assert_eq!(
        payload[18],
        opcore::expr_vm::ExprVmOpcode::BuildStructLiteral as u8
    );
    for (start, identity) in [(19, 123u16), (23, 11), (25, 12)] {
        payload[start..start + 2].copy_from_slice(&identity.to_le_bytes());
    }
    cases.push(opaque);
    let canonical = vm::vm_opcore::expression_parser_program().to_vec();
    for (source, cursor) in [
        ("Point{", 5),
        ("Point{x}", 9),
        ("Point{x:}", 10),
        ("Point{x:1", 15),
        ("Point{x:1,}", 16),
        ("Point{x:1 y:2}", 15),
        ("Point{1:2}", 5),
        ("Point{x:1,,y:2}", 16),
    ] {
        cases.push(failure(
            numeric(source).0,
            canonical.clone(),
            3072,
            8192,
            1,
            cursor,
        ));
    }
    // The tokenizer folds dotted names into one identifier. Construct the
    // distinct DOT/member envelope explicitly to test the field-key boundary.
    let mut dotted_key = numeric("Point{x:1}").0;
    dotted_key.splice(9..9, [tk_constants()["TK_KIND_DOT"], 0, 0, 7, 0]);
    cases.push(failure(dotted_key, canonical.clone(), 3072, 8192, 1, 9));
    // Reusing the identifier root permits an empty struct in exactly one node.
    let mut empty = typed_success("Point{}");
    empty.arena = NODE_BYTES as u16;
    cases.push(empty);
    for (source, arena, cursor) in [
        ("Point{x:1}", 24, 10),
        ("Point{x:1}", 48, 15),
        ("Point{x:Point{}}", 48, 16),
    ] {
        cases.push(failure(
            numeric(source).0,
            canonical.clone(),
            arena,
            8192,
            3,
            cursor,
        ));
    }
    // Packed name payloads must be bounded both at the type and field cursor.
    let tokens = numeric("Point{x:1}").0;
    for length in 1..4 {
        cases.push(failure(
            tokens[..length].to_vec(),
            canonical.clone(),
            3072,
            8192,
            1,
            0,
        ));
        cases.push(failure(
            tokens[..5 + length].to_vec(),
            canonical.clone(),
            3072,
            8192,
            1,
            5,
        ));
    }
    let mut qualified = tokens.clone();
    qualified[8] = 1;
    cases.push(failure(qualified, canonical.clone(), 3072, 8192, 1, 5));
    let nested = (0..16).fold("1".to_owned(), |value, _| format!("Point{{x:{value}}}"));
    cases.push(typed_success(&nested));
    let too_deep = format!("Point{{x:{nested}}}");
    cases.push(failure(
        numeric(&too_deep).0,
        canonical.clone(),
        3072,
        65536,
        4,
        164,
    ));
    // The action cannot construct a struct without the grammar's identifier.
    cases.push(failure(
        numeric("{x:1}").0,
        vec![package::ExvmOpcode::ParseStructLiteralIfPresent as u8],
        3072,
        8192,
        1,
        0,
    ));
    pack_batch(cases)
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
#[ignore = "requires configured FS-UAE; one fresh canonical compiler tree batch"]
fn native_expression_compiler_fs_uae() {
    run_native_batch(batch(), "canonical-exvm-numeric-compiler");
}
fn run_native_batch((input, expected): (Vec<u8>, Vec<u8>), name: &'static str) {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let root = workspace_root();
    let diagnostic = std::env::var_os("OPFORGE_NATIVE_MISMATCH_DIR").map(|directory| {
        PathBuf::from(directory)
            .join(name)
            .join("Work/binary-exvm-nodes.native")
    });
    if let Some(path) = &diagnostic {
        // Never confuse a previous diagnostic with this fresh invocation.
        match std::fs::remove_file(path) {
            Ok(()) => {}
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => panic!("clear previous compiler diagnostic: {error}"),
        }
    }
    let (_scratch, image, source) = assemble(&root, &input);
    let case = OpforgeNativeCliParityCase {
        name,
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
    let result =
        run_prebuilt_compact_cli_case_from_env(&root, &case, &image).unwrap_or_else(|error| {
            if let Some(path) = &diagnostic {
                if let Ok(actual) = std::fs::read(path) {
                    let count = u32::from_be_bytes(input[..4].try_into().unwrap()) as usize;
                    eprintln!("{}", batch_differences(&actual, &expected, count));
                }
            }
            panic!("fresh compiler batch completion: {error}")
        });
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
    eprintln!(
        "EXVM_TREE cases={} oracle_bytes={} image_bytes={} seconds={:?} exact=true",
        u32::from_be_bytes(input[..4].try_into().unwrap()),
        expected.len(),
        image.len(),
        runs[0].start_to_done_host_seconds
    );
}

// Decode each bounded result independently: one changed arena or payload length
// must not shift the comparison for every following case.
fn batch_differences(actual: &[u8], expected: &[u8], count: usize) -> String {
    fn record(bytes: &[u8], start: usize) -> Option<(&[u8], [u32; 6])> {
        let mut words = [0; 6];
        let header = bytes.get(start..start.checked_add(16)?)?;
        for (word, bytes) in words[..4].iter_mut().zip(header.chunks_exact(4)) {
            *word = u32::from_be_bytes(bytes.try_into().ok()?);
        }
        let lower = start.checked_add(16)?.checked_add(words[3] as usize)?;
        let header = bytes.get(lower..lower.checked_add(8)?)?;
        for (word, bytes) in words[4..].iter_mut().zip(header.chunks_exact(4)) {
            *word = u32::from_be_bytes(bytes.try_into().ok()?);
        }
        let end = lower.checked_add(8)?.checked_add(words[5] as usize)?;
        let end = end.checked_add(end % 2)?;
        Some((bytes.get(start..end)?, words))
    }
    let mut native_start = 0;
    let mut rust_start = 0;
    let mut details = Vec::new();
    for index in 0..count {
        let (Some((native, native_words)), Some((rust, rust_words))) =
            (record(actual, native_start), record(expected, rust_start))
        else {
            details.push(format!(
                "case {index}: invalid result bounds native@{native_start} rust@{rust_start}"
            ));
            break;
        };
        if native != rust {
            let first = native.iter().zip(rust).position(|(a, b)| a != b);
            details.push(format!(
                "case {index}: native@{native_start} bytes={} header={native_words:?}; rust@{rust_start} bytes={} header={rust_words:?}; first relative mismatch={first:?}",
                native.len(), rust.len()
            ));
        }
        native_start += native.len();
        rust_start += rust.len();
    }
    details.push(format!(
        "decoded native={native_start}/{} rust={rust_start}/{}",
        actual.len(),
        expected.len()
    ));
    details.join("\n")
}

#[test]
fn native_expression_compiler_struct_live_canonical_tree_oracle() {
    let (input, expected) = struct_batch();
    assert!(input.len() > 1024 && expected.len() > 1024);
}
#[test]
#[ignore = "requires configured FS-UAE; one fresh canonical struct compiler batch"]
fn native_expression_compiler_struct_fs_uae() {
    run_native_batch(struct_batch(), "canonical-exvm-struct-compiler");
}
