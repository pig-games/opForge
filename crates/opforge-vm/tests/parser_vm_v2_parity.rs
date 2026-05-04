use std::cell::{Cell, RefCell};
use std::rc::Rc;
use std::time::Instant;

use families::{
    register_intel8080_family_stack, register_mos6502_family_stack,
    register_motorola68000_family_stack, register_motorola6800_family_stack,
};
use opcore::parser::{Expr, LineAst, ParseError};
use opcore::tokenizer::{Span, Token};
use package::{ParserVmOpcode, PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT};
use registry::registry::ModuleRegistry;
use registry::syntax::{
    parser_from_line_with_registers, register_checker_from_fn, register_checker_none,
    RegisterChecker,
};
use types::processing::{OpcoreRequestKind, ProcessingOutcome, ProcessingRequestKind};
use vm::vm_opasm::{
    parse_statement_line_with_model, parse_statement_line_with_model_and_expr_handler,
    DynExprProcessingHandler, ExprProcessingHandler, HierarchyExecutionModel,
};

#[derive(Debug, PartialEq, Eq)]
enum NormalizedParse {
    Ast(String),
    Error { message: String, span: Span },
}

fn registry_for_parity() -> ModuleRegistry {
    let mut registry = ModuleRegistry::new();
    register_intel8080_family_stack(&mut registry);
    register_mos6502_family_stack(&mut registry);
    register_motorola6800_family_stack(&mut registry);
    register_motorola68000_family_stack(&mut registry);
    registry
}

fn model_for_parity() -> HierarchyExecutionModel {
    HierarchyExecutionModel::from_registry(&registry_for_parity()).expect("execution model build")
}

fn normalize(result: Result<LineAst, ParseError>) -> NormalizedParse {
    match result {
        Ok(ast) => NormalizedParse::Ast(format!("{ast:?}")),
        Err(err) => NormalizedParse::Error {
            message: err.message,
            span: err.span,
        },
    }
}

fn parse_v2(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    line: &str,
    line_num: u32,
    register_checker: &RegisterChecker,
) -> Result<LineAst, ParseError> {
    parse_statement_line_with_model(
        model,
        cpu_id,
        dialect_override,
        line,
        line_num,
        register_checker,
    )
    .map(|(ast, _, _)| ast)
}

fn parse_host(
    line: &str,
    line_num: u32,
    register_checker: RegisterChecker,
) -> Result<LineAst, ParseError> {
    parser_from_line_with_registers(line, line_num, register_checker)
        .and_then(|mut parser| parser.parse_compat_mixed_line())
}

fn assert_public_v2_matches_host(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    dialect_override: Option<&str>,
    register_checker: RegisterChecker,
    lines: &[&str],
) {
    for (idx, line) in lines.iter().enumerate() {
        let line_num = (idx + 1) as u32;
        let v2 = normalize(parse_v2(
            model,
            cpu_id,
            dialect_override,
            line,
            line_num,
            &register_checker,
        ));
        let host = normalize(parse_host(line, line_num, register_checker.clone()));
        assert_eq!(v2, host, "public PRVM v2 parity mismatch for {line:?}");
    }
}

#[test]
fn parser_vm_v2_parity_statement_data_assignment_and_block_corpus() {
    let model = model_for_parity();
    let m6502_registers = register_checker_from_fn(families::mos6502::is_register);
    let m68k_registers = register_checker_from_fn(families::m68k::is_register);

    let opasm_lines = [
        "    NOP",
        "    LDA #$42",
        "label: LDA ($10),Y",
        "    TESTOP one,two,three,four",
        "label: .byte $01, $02",
        "    .db $03",
        "    .word $1234",
        "    .dw $5678",
        "    .long $01020304",
        "    .text \"AZ\"",
        "    .null \"A\"",
        "    .ptext \"A\"",
        "    .fill 3,$ff",
        "    .res 4",
        "    .ds 5",
        "    .align 2",
        "* = $1000",
        "    .org $2000",
        "name = 1",
        "name := 2",
        "name :?= 3",
        "name += 4",
        "name -= 5",
        "name *= 6",
        "name /= 7",
        "name %= 8",
        "name **= 2",
        "name |= 1",
        "name ^= 1",
        "    .region app",
        "    .endregion",
        "    .section code",
        "    .endsection",
        "    .encode \"petscii\"",
        "    .endencode",
        "    .meta",
        "    .endmeta",
        "    .output \"demo.bin\"",
        "    .endoutput",
    ];
    assert_public_v2_matches_host(&model, "m6502", None, m6502_registers, &opasm_lines);

    let m68k_lines = [
        "    MOVE.B D0,D1",
        "    MOVE.W #1,(A0)",
        "    MOVE.L (A0)+,-(A1)",
        "    MOVE.L 4(A0,D1.W),D0",
        "    MOVE.L label(PC),D0",
        "    MOVEM.L D0-D7/A0-A6,-(SP)",
    ];
    assert_public_v2_matches_host(&model, "m68020", None, m68k_registers, &m68k_lines);
}

#[test]
fn parser_vm_v2_parity_m68k_authority_covers_special_addressing_shapes() {
    let model = model_for_parity();
    let register_checker = register_checker_from_fn(families::m68k::is_register);
    let lines = ["    BFTST D0{1:3}", "    CAS2.W D0:D1,(A0):(A1),D2:D3"];

    for (idx, line) in lines.iter().enumerate() {
        let ast = parse_v2(
            &model,
            "m68020",
            None,
            line,
            (idx + 1) as u32,
            &register_checker,
        )
        .expect("m68k special addressing shape should parse through v2");
        assert!(
            !format!("{ast:?}").contains("Error("),
            "m68k special addressing shape produced Expr::Error: {ast:?}"
        );
    }
}

#[test]
fn parser_vm_v2_parity_m68k_pair_operands_match_host_before_semantics() {
    let model = model_for_parity();
    let register_checker = register_checker_from_fn(families::m68k::is_register);
    let lines = ["    CAS2.W 1:D1,(A0):(A1),D2:D3", "    DIVS.L D0,1:D1"];

    assert_public_v2_matches_host(&model, "m68020", None, register_checker, &lines);
}

#[test]
fn parser_vm_v2_parity_m68k_wrapped_operands_match_host_before_semantics() {
    let model = model_for_parity();
    let register_checker = register_checker_from_fn(families::m68k::is_register);
    let lines = ["    MOVE.L (4,A0)+,D0", "    MOVE.L -(4,A0),D0"];

    assert_public_v2_matches_host(&model, "m68020", None, register_checker, &lines);
}

#[test]
fn parser_vm_v2_parity_preserves_expression_error_shapes() {
    let model = model_for_parity();
    let register_checker = register_checker_from_fn(families::mos6502::is_register);
    let lines = ["label = 1 +", "    LDA #(", "    .if 1 +"];

    assert_public_v2_matches_host(&model, "m6502", None, register_checker, &lines);
}

#[test]
fn parser_vm_v2_parity_reports_malformed_and_trailing_tokens() {
    let model = model_for_parity();
    let register_checker = register_checker_none();
    let lines = [
        "    .endfor 1",
        "    .endsection extra",
        "    .endmeta extra",
    ];

    assert_public_v2_matches_host(&model, "m6502", None, register_checker, &lines);
}

struct CountingExprHandler {
    calls: Rc<Cell<usize>>,
    requests: Rc<RefCell<Vec<ProcessingRequestKind>>>,
}

type CountingHandlerParts<'a> = (
    DynExprProcessingHandler<'a>,
    Rc<Cell<usize>>,
    Rc<RefCell<Vec<ProcessingRequestKind>>>,
);

impl ExprProcessingHandler for CountingExprHandler {
    fn process_expr_request(
        &mut self,
        request: ProcessingRequestKind,
        tokens: Vec<Token>,
        end_span: Span,
        _end_token_text: Option<String>,
    ) -> ProcessingOutcome<Expr, ParseError> {
        self.calls.set(self.calls.get().saturating_add(1));
        self.requests.borrow_mut().push(request);
        let span = tokens.first().map(|token| token.span).unwrap_or(end_span);
        ProcessingOutcome::Done(Expr::Number("42".to_string(), span))
    }
}

fn counting_handler<'a>() -> CountingHandlerParts<'a> {
    let calls = Rc::new(Cell::new(0));
    let requests = Rc::new(RefCell::new(Vec::new()));
    let handler = CountingExprHandler {
        calls: Rc::clone(&calls),
        requests: Rc::clone(&requests),
    };
    (Rc::new(RefCell::new(Box::new(handler))), calls, requests)
}

#[test]
fn parser_vm_v2_parity_routes_expr_work_only_through_typed_subcalls() {
    let model = model_for_parity();
    let register_checker = register_checker_from_fn(families::mos6502::is_register);
    let (handler, calls, requests) = counting_handler();

    let (ast, _, _) = parse_statement_line_with_model_and_expr_handler(
        &model,
        "m6502",
        None,
        "    LDA #target + 1",
        1,
        &register_checker,
        Some(handler),
    )
    .expect("expression-bearing line should parse through handler");

    assert!(calls.get() > 0, "expression handler was not called");
    assert!(
        requests.borrow().iter().all(|request| matches!(
            request,
            ProcessingRequestKind::Opcore(OpcoreRequestKind::Expr)
        )),
        "unexpected expression request path: {:?}",
        requests.borrow()
    );
    assert!(
        format!("{ast:?}").contains("Number(\"42\""),
        "handler result was not embedded in AST: {ast:?}"
    );

    let (handler, calls, requests) = counting_handler();
    parse_statement_line_with_model_and_expr_handler(
        &model,
        "m6502",
        None,
        "    NOP",
        2,
        &register_checker,
        Some(handler),
    )
    .expect("expression-free line should parse");
    assert_eq!(calls.get(), 0, "expression-free line made sub-calls");
    assert!(requests.borrow().is_empty());
}

#[test]
fn parser_vm_v2_parity_rejects_checkpoint_depth_boundary_through_public_runtime() {
    let registry = registry_for_parity();
    let mut chunks = vm::builder::build_hierarchy_chunks_from_registry(&registry)
        .expect("hierarchy chunks build");
    for program in &mut chunks.parser_vm_programs {
        if matches!(
            program.owner,
            vm::hierarchy::ScopedOwner::Family(ref family_id)
                if family_id.eq_ignore_ascii_case("mos6502")
        ) {
            program.opcode_version = PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT;
            program.program = vec![
                ParserVmOpcode::Checkpoint as u8,
                ParserVmOpcode::Checkpoint as u8,
                ParserVmOpcode::Checkpoint as u8,
                ParserVmOpcode::Checkpoint as u8,
                ParserVmOpcode::Checkpoint as u8,
            ];
        }
    }
    let model = HierarchyExecutionModel::from_chunks(chunks).expect("execution model build");
    let register_checker = register_checker_from_fn(families::mos6502::is_register);

    let err = parse_v2(&model, "m6502", None, "    NOP", 1, &register_checker)
        .expect_err("fifth checkpoint should fail through public runtime path");
    assert!(
        err.message.contains("checkpoint depth exceeded"),
        "unexpected checkpoint boundary error: {err:?}"
    );
}

#[test]
fn parser_vm_v2_parity_records_non_gated_throughput_sample() {
    // WI-6 local sample, macOS debug test build, 2026-04-28: 768 public PRVM v2
    // parses completed in ~25.3 ms; 768 host-parser proxy parses completed in
    // ~1.23 ms. This is recorded only as a native-port baseline and does not
    // assert a speed ratio. The retired v1 helper path no longer exists after
    // WI-5, so the host parser is the available pre-v2 behavior proxy.
    let model = model_for_parity();
    let register_checker = register_checker_from_fn(families::mos6502::is_register);
    let corpus = [
        "    NOP",
        "    LDA #$42",
        "label: .byte $01, $02",
        "name += 4",
        "    .section code",
        "    .endsection",
    ];
    let iterations = 128;

    let v2_start = Instant::now();
    for _ in 0..iterations {
        for (idx, line) in corpus.iter().enumerate() {
            parse_v2(
                &model,
                "m6502",
                None,
                line,
                (idx + 1) as u32,
                &register_checker,
            )
            .expect("v2 corpus line should parse");
        }
    }
    let v2_elapsed = v2_start.elapsed();

    let host_start = Instant::now();
    for _ in 0..iterations {
        for (idx, line) in corpus.iter().enumerate() {
            parse_host(line, (idx + 1) as u32, register_checker.clone())
                .expect("host corpus line should parse");
        }
    }
    let host_elapsed = host_start.elapsed();

    eprintln!(
        "parser_vm_v2_parity throughput sample: {} v2 parses in {:?}; {} host parses in {:?}",
        iterations * corpus.len(),
        v2_elapsed,
        iterations * corpus.len(),
        host_elapsed
    );
}
