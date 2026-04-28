use std::collections::HashMap;

use families::{register_mos6502_family_stack, register_motorola68000_family_stack};
use opcore::parser::{Expr, Label, LineAst, ParseError};
use opcore::tokenizer::Span;
use registry::registry::ModuleRegistry;
use registry::syntax::{register_checker_from_fn, RegisterChecker};
use types::line_ast::StatementAst;
use vm::native_prvm::{
    NativePrvmExprSlotState, NativePrvmHostExpressionBridge, NATIVE_PRVM_EXPR_RESULT_SLOT_SIZE,
};
use vm::vm_opasm::{parse_statement_line_with_model, HierarchyExecutionModel};

const RESULT_RECORD_SIZE: usize = 32;
const DIAGNOSTIC_RECORD_SIZE: usize = 32;
const EXPR_REQUEST_RECORD_SIZE: usize = 32;
const EXPR_RESULT_SLOT_SIZE: usize = 32;

const RESULT_BEGIN_STATEMENT: u16 = 1;
const RESULT_LABEL_TEXT: u16 = 2;
const RESULT_MNEMONIC_TEXT: u16 = 3;
const RESULT_OPERAND_EXPR_SLOT: u16 = 4;
const RESULT_FINISH_LINE: u16 = 5;

const STATUS_NEWLINE_UNSUPPORTED: i32 = 2;
const STATUS_ENTRY_BOUNDARY: i32 = 3;

#[derive(Debug, PartialEq, Eq)]
struct NativeDiagnostic {
    code: u16,
    span: Span,
    token_index: Option<u32>,
    message: Option<String>,
}

#[derive(Debug, PartialEq, Eq)]
struct NativeExprRequest {
    operand_index: u32,
    expr_slot_index: u32,
    start_token: u32,
    end_token: u32,
    boundary_span: Span,
}

#[derive(Debug, PartialEq, Eq)]
enum NativeExprSlotState {
    Empty,
    ReadyExpression,
    ReadyExpressionError,
}

#[derive(Debug, PartialEq, Eq)]
struct NativeExprResultSlot {
    state: NativeExprSlotState,
    expr_slot_index: u32,
    span: Span,
    host_expr_handle: u32,
}

#[derive(Debug, PartialEq, Eq)]
struct NativeStatusReturn {
    status: i32,
    first: u32,
    second: u32,
    third: u32,
}

fn registry_for_native_abi() -> ModuleRegistry {
    let mut registry = ModuleRegistry::new();
    register_mos6502_family_stack(&mut registry);
    register_motorola68000_family_stack(&mut registry);
    registry
}

fn model_for_native_abi() -> HierarchyExecutionModel {
    HierarchyExecutionModel::from_registry(&registry_for_native_abi()).expect("execution model")
}

fn parse_v2_statement(
    model: &HierarchyExecutionModel,
    line: &str,
    register_checker: &RegisterChecker,
) -> Result<LineAst, ParseError> {
    parse_statement_line_with_model(model, "m6502", None, line, 1, register_checker)
        .map(|(line_ast, _, _)| line_ast)
}

fn append_u16(bytes: &mut Vec<u8>, value: u16) {
    bytes.extend_from_slice(&value.to_be_bytes());
}

fn append_u32(bytes: &mut Vec<u8>, value: u32) {
    bytes.extend_from_slice(&value.to_be_bytes());
}

fn read_u16(bytes: &[u8], offset: usize) -> Result<u16, String> {
    let field = bytes
        .get(offset..offset + 2)
        .ok_or_else(|| format!("missing u16 at offset {offset}"))?;
    Ok(u16::from_be_bytes(
        field.try_into().expect("u16 slice width"),
    ))
}

fn read_u32(bytes: &[u8], offset: usize) -> Result<u32, String> {
    let field = bytes
        .get(offset..offset + 4)
        .ok_or_else(|| format!("missing u32 at offset {offset}"))?;
    Ok(u32::from_be_bytes(
        field.try_into().expect("u32 slice width"),
    ))
}

fn span_from_record(bytes: &[u8]) -> Result<Span, String> {
    Ok(Span {
        line: read_u32(bytes, 4)?,
        col_start: read_u32(bytes, 8)? as usize,
        col_end: read_u32(bytes, 12)? as usize,
    })
}

fn append_lexeme(lexemes: &mut Vec<u8>, text: &str) -> (u32, u32) {
    let offset = lexemes.len() as u32;
    lexemes.extend_from_slice(text.as_bytes());
    (offset, text.len() as u32)
}

fn lexeme_text(lexemes: &[u8], offset: u32, len: u32) -> Result<String, String> {
    let start = offset as usize;
    let end = start.saturating_add(len as usize);
    let field = lexemes
        .get(start..end)
        .ok_or_else(|| format!("lexeme range {start}..{end} out of bounds"))?;
    String::from_utf8(field.to_vec()).map_err(|err| err.to_string())
}

fn append_record(bytes: &mut Vec<u8>, kind: u16, span: Span, args: [u32; 4]) {
    append_u16(bytes, kind);
    append_u16(bytes, 0);
    append_u32(bytes, span.line);
    append_u32(bytes, span.col_start as u32);
    append_u32(bytes, span.col_end as u32);
    for value in args {
        append_u32(bytes, value);
    }
}

fn decode_statement_result(
    bytes: &[u8],
    lexemes: &[u8],
    expression_slots: &HashMap<u32, Expr>,
) -> Result<LineAst, String> {
    if !bytes.len().is_multiple_of(RESULT_RECORD_SIZE) {
        return Err("result buffer is not aligned to result record size".to_string());
    }

    let mut saw_begin = false;
    let mut saw_finish = false;
    let mut label: Option<Label> = None;
    let mut mnemonic: Option<String> = None;
    let mut operands = Vec::new();

    for record in bytes.chunks_exact(RESULT_RECORD_SIZE) {
        let kind = read_u16(record, 0)?;
        let span = span_from_record(record)?;
        match kind {
            RESULT_BEGIN_STATEMENT => {
                if saw_begin {
                    return Err("duplicate BEGIN_STATEMENT".to_string());
                }
                saw_begin = true;
            }
            RESULT_LABEL_TEXT => {
                let text = lexeme_text(lexemes, read_u32(record, 16)?, read_u32(record, 20)?)?;
                label = Some(Label { name: text, span });
            }
            RESULT_MNEMONIC_TEXT => {
                mnemonic = Some(lexeme_text(
                    lexemes,
                    read_u32(record, 16)?,
                    read_u32(record, 20)?,
                )?);
            }
            RESULT_OPERAND_EXPR_SLOT => {
                let operand_index = read_u32(record, 16)? as usize;
                let expr_slot_index = read_u32(record, 20)?;
                let start_token = read_u32(record, 24)?;
                let end_token = read_u32(record, 28)?;
                if operand_index != operands.len() {
                    return Err(format!(
                        "operand index {operand_index} did not match operand order {}",
                        operands.len()
                    ));
                }
                if start_token > end_token {
                    return Err("operand token range is not half-open".to_string());
                }
                let expr = expression_slots
                    .get(&expr_slot_index)
                    .ok_or_else(|| format!("missing expression slot {expr_slot_index}"))?;
                operands.push(expr.clone());
            }
            RESULT_FINISH_LINE => {
                saw_finish = true;
            }
            other => return Err(format!("unsupported result record kind {other}")),
        }
    }

    if !saw_begin || !saw_finish {
        return Err("result did not contain begin and finish records".to_string());
    }

    Ok(LineAst::Statement(StatementAst {
        label,
        mnemonic,
        operands,
    }))
}

fn append_diagnostic_record(
    bytes: &mut Vec<u8>,
    code: u16,
    span: Span,
    token_index: Option<u32>,
    message: Option<(u32, u32)>,
) {
    append_u16(bytes, code);
    append_u16(bytes, 0);
    append_u32(bytes, span.line);
    append_u32(bytes, span.col_start as u32);
    append_u32(bytes, span.col_end as u32);
    append_u32(bytes, token_index.unwrap_or(u32::MAX));
    let (message_offset, message_len) = message.unwrap_or((u32::MAX, 0));
    append_u32(bytes, message_offset);
    append_u32(bytes, message_len);
    append_u32(bytes, 0);
}

fn decode_diagnostic_record(bytes: &[u8], lexemes: &[u8]) -> Result<NativeDiagnostic, String> {
    if bytes.len() != DIAGNOSTIC_RECORD_SIZE {
        return Err("diagnostic record has wrong size".to_string());
    }
    let message_offset = read_u32(bytes, 20)?;
    let message_len = read_u32(bytes, 24)?;
    let message = if message_offset == u32::MAX {
        None
    } else {
        Some(lexeme_text(lexemes, message_offset, message_len)?)
    };
    let token_index = match read_u32(bytes, 16)? {
        u32::MAX => None,
        value => Some(value),
    };
    Ok(NativeDiagnostic {
        code: read_u16(bytes, 0)?,
        span: span_from_record(bytes)?,
        token_index,
        message,
    })
}

fn append_expr_request_record(
    bytes: &mut Vec<u8>,
    operand_index: u32,
    expr_slot_index: u32,
    start_token: u32,
    end_token: u32,
    boundary_span: Span,
) {
    append_u16(bytes, 1);
    append_u16(bytes, 0);
    append_u32(bytes, operand_index);
    append_u32(bytes, expr_slot_index);
    append_u32(bytes, start_token);
    append_u32(bytes, end_token);
    append_u32(bytes, boundary_span.line);
    append_u32(bytes, boundary_span.col_start as u32);
    append_u32(bytes, boundary_span.col_end as u32);
}

fn decode_expr_request_record(bytes: &[u8]) -> Result<NativeExprRequest, String> {
    if bytes.len() != EXPR_REQUEST_RECORD_SIZE {
        return Err("expression request record has wrong size".to_string());
    }
    if read_u16(bytes, 0)? != 1 || read_u16(bytes, 2)? != 0 {
        return Err("unsupported expression request header".to_string());
    }
    Ok(NativeExprRequest {
        operand_index: read_u32(bytes, 4)?,
        expr_slot_index: read_u32(bytes, 8)?,
        start_token: read_u32(bytes, 12)?,
        end_token: read_u32(bytes, 16)?,
        boundary_span: Span {
            line: read_u32(bytes, 20)?,
            col_start: read_u32(bytes, 24)? as usize,
            col_end: read_u32(bytes, 28)? as usize,
        },
    })
}

fn append_expr_result_slot(
    bytes: &mut Vec<u8>,
    state: u16,
    expr_slot_index: u32,
    span: Span,
    host_expr_handle: u32,
) {
    append_u16(bytes, state);
    append_u16(bytes, 0);
    append_u32(bytes, expr_slot_index);
    append_u32(bytes, span.line);
    append_u32(bytes, span.col_start as u32);
    append_u32(bytes, span.col_end as u32);
    append_u32(bytes, host_expr_handle);
    append_u32(bytes, u32::MAX);
    append_u32(bytes, 0);
}

fn decode_expr_result_slot(bytes: &[u8]) -> Result<NativeExprResultSlot, String> {
    if bytes.len() != EXPR_RESULT_SLOT_SIZE {
        return Err("expression result slot has wrong size".to_string());
    }
    if read_u16(bytes, 2)? != 0 || read_u32(bytes, 24)? != u32::MAX || read_u32(bytes, 28)? != 0 {
        return Err("expression result slot reserved fields are not v0.1 conforming".to_string());
    }
    let state = match read_u16(bytes, 0)? {
        0 => NativeExprSlotState::Empty,
        1 => NativeExprSlotState::ReadyExpression,
        2 => NativeExprSlotState::ReadyExpressionError,
        other => return Err(format!("unsupported expression slot state {other}")),
    };
    Ok(NativeExprResultSlot {
        state,
        expr_slot_index: read_u32(bytes, 4)?,
        span: Span {
            line: read_u32(bytes, 8)?,
            col_start: read_u32(bytes, 12)? as usize,
            col_end: read_u32(bytes, 16)? as usize,
        },
        host_expr_handle: read_u32(bytes, 20)?,
    })
}

fn decode_status_return(status: i32, first: u32, second: u32, third: u32) -> NativeStatusReturn {
    NativeStatusReturn {
        status,
        first,
        second,
        third,
    }
}

#[test]
fn native_prvm_abi_decodes_success_result_to_rust_v2_statement_shape() {
    let model = model_for_native_abi();
    let register_checker = register_checker_from_fn(families::mos6502::is_register);
    let rust_ast = parse_v2_statement(&model, "start: LDA #42", &register_checker)
        .expect("Rust PRVM v2 should parse authority line");
    let LineAst::Statement(statement) = &rust_ast else {
        panic!("expected statement AST: {rust_ast:?}");
    };
    let label = statement.label.as_ref().expect("label should exist");
    let mnemonic = statement.mnemonic.as_ref().expect("mnemonic should exist");
    let operand = statement.operands.first().expect("operand should exist");

    let mut lexemes = Vec::new();
    let label_ref = append_lexeme(&mut lexemes, &label.name);
    let mnemonic_ref = append_lexeme(&mut lexemes, mnemonic);
    let mut expression_slots = HashMap::new();
    expression_slots.insert(0, operand.clone());

    let mut records = Vec::new();
    append_record(
        &mut records,
        RESULT_BEGIN_STATEMENT,
        label.span,
        [0, 0, 0, 0],
    );
    append_record(
        &mut records,
        RESULT_LABEL_TEXT,
        label.span,
        [label_ref.0, label_ref.1, 0, 0],
    );
    append_record(
        &mut records,
        RESULT_MNEMONIC_TEXT,
        Span {
            line: 1,
            col_start: 8,
            col_end: 11,
        },
        [mnemonic_ref.0, mnemonic_ref.1, 0, 0],
    );
    append_record(
        &mut records,
        RESULT_OPERAND_EXPR_SLOT,
        Span {
            line: 1,
            col_start: 12,
            col_end: 15,
        },
        [0, 0, 2, 4],
    );
    append_record(
        &mut records,
        RESULT_FINISH_LINE,
        Span {
            line: 1,
            col_start: 15,
            col_end: 15,
        },
        [0, 0, 0, 0],
    );

    let decoded = decode_statement_result(&records, &lexemes, &expression_slots)
        .expect("native result should decode");
    assert_eq!(format!("{decoded:?}"), format!("{rust_ast:?}"));
}

#[test]
fn native_prvm_abi_decodes_no_expression_statement_result_to_rust_v2_shape() {
    let model = model_for_native_abi();
    let register_checker = register_checker_from_fn(families::mos6502::is_register);
    let rust_ast = parse_v2_statement(&model, " NOP", &register_checker)
        .expect("Rust PRVM v2 should parse no-expression statement");
    let LineAst::Statement(statement) = &rust_ast else {
        panic!("expected statement AST: {rust_ast:?}");
    };
    assert!(statement.label.is_none());
    assert!(statement.operands.is_empty());
    let mnemonic = statement.mnemonic.as_ref().expect("mnemonic should exist");

    let mut lexemes = Vec::new();
    let mnemonic_ref = append_lexeme(&mut lexemes, mnemonic);
    let expression_slots = HashMap::new();

    let mut records = Vec::new();
    append_record(
        &mut records,
        RESULT_BEGIN_STATEMENT,
        Span {
            line: 1,
            col_start: 0,
            col_end: 0,
        },
        [0, 0, 0, 0],
    );
    append_record(
        &mut records,
        RESULT_MNEMONIC_TEXT,
        Span {
            line: 1,
            col_start: 2,
            col_end: 5,
        },
        [mnemonic_ref.0, mnemonic_ref.1, 0, 0],
    );
    append_record(
        &mut records,
        RESULT_FINISH_LINE,
        Span {
            line: 1,
            col_start: 0,
            col_end: 0,
        },
        [0, 0, 0, 0],
    );

    let decoded = decode_statement_result(&records, &lexemes, &expression_slots)
        .expect("native no-expression result should decode");
    assert_eq!(format!("{decoded:?}"), format!("{rust_ast:?}"));
}

#[test]
fn native_prvm_abi_decodes_diagnostic_records_with_span_and_message() {
    let mut lexemes = Vec::new();
    let message_ref = append_lexeme(&mut lexemes, "unexpected trailing token");
    let span = Span {
        line: 7,
        col_start: 9,
        col_end: 10,
    };
    let mut record = Vec::new();
    append_diagnostic_record(&mut record, 3, span, Some(2), Some(message_ref));

    let decoded = decode_diagnostic_record(&record, &lexemes).expect("diagnostic should decode");
    assert_eq!(
        decoded,
        NativeDiagnostic {
            code: 3,
            span,
            token_index: Some(2),
            message: Some("unexpected trailing token".to_string()),
        }
    );
}

#[test]
fn native_prvm_abi_decodes_expression_request_with_bounded_range_and_resume_slot() {
    let mut record = Vec::new();
    append_expr_request_record(
        &mut record,
        1,
        4,
        3,
        8,
        Span {
            line: 2,
            col_start: 12,
            col_end: 18,
        },
    );

    let decoded = decode_expr_request_record(&record).expect("expression request should decode");
    let token_count = 9;
    let expr_slot_count = 5;
    assert!(decoded.start_token < decoded.end_token);
    assert!(decoded.end_token <= token_count);
    assert!(decoded.expr_slot_index < expr_slot_count);
    assert_eq!(decoded.operand_index, 1);
    assert_eq!(decoded.boundary_span.col_start, 12);
}

#[test]
fn native_prvm_abi_decodes_expression_result_slots_for_resume() {
    let span = Span {
        line: 4,
        col_start: 6,
        col_end: 12,
    };
    let mut ready_record = Vec::new();
    append_expr_result_slot(&mut ready_record, 1, 2, span, 42);
    let ready =
        decode_expr_result_slot(&ready_record).expect("ready expression slot should decode");
    assert_eq!(ready.state, NativeExprSlotState::ReadyExpression);
    assert_eq!(ready.expr_slot_index, 2);
    assert_eq!(ready.host_expr_handle, 42);

    let mut error_record = Vec::new();
    append_expr_result_slot(&mut error_record, 2, 3, span, 99);
    let error = decode_expr_result_slot(&error_record).expect("ready error slot should decode");
    assert_eq!(error.state, NativeExprSlotState::ReadyExpressionError);
    assert_eq!(error.host_expr_handle, 99);

    let mut malformed = error_record.clone();
    malformed[24..28].copy_from_slice(&0_u32.to_be_bytes());
    let err = decode_expr_result_slot(&malformed)
        .expect_err("reserved expression-result fields should be enforced");
    assert!(err.contains("reserved fields"));
}

#[test]
fn native_prvm_abi_host_bridge_fills_expression_slot_from_rust_parser() {
    let model = model_for_native_abi();
    let register_checker = register_checker_from_fn(families::mos6502::is_register);
    let rust_ast = parse_v2_statement(&model, " LDA #42", &register_checker)
        .expect("Rust PRVM v2 should parse expression-bearing line");

    let mut request_record = Vec::new();
    append_expr_request_record(
        &mut request_record,
        0,
        0,
        1,
        3,
        Span {
            line: 1,
            col_start: 6,
            col_end: 9,
        },
    );
    let mut result_slot = vec![0; NATIVE_PRVM_EXPR_RESULT_SLOT_SIZE];
    let mut bridge = NativePrvmHostExpressionBridge::from_source_line(
        &model,
        "m6502",
        None,
        " LDA #42",
        1,
        &register_checker,
        Some("LDA"),
    )
    .expect("bridge should tokenize native source line");

    let bridged = bridge
        .handle_expression_request_record(&request_record, &mut result_slot)
        .expect("host bridge should parse requested operand expression");
    assert_eq!(bridged.slot_state, NativePrvmExprSlotState::ReadyExpression);
    assert_eq!(bridged.host_expr_handle, 0);
    assert!(matches!(
        bridged.expr,
        Expr::Immediate(ref inner, _) if matches!(inner.as_ref(), Expr::Number(value, _) if value == "42")
    ));

    let decoded_slot = decode_expr_result_slot(&result_slot).expect("slot should decode");
    assert_eq!(decoded_slot.state, NativeExprSlotState::ReadyExpression);
    assert_eq!(decoded_slot.expr_slot_index, 0);
    assert_eq!(decoded_slot.host_expr_handle, 0);
    assert!(matches!(
        bridge.expression_for_handle(decoded_slot.host_expr_handle),
        Some(Expr::Immediate(inner, _)) if matches!(inner.as_ref(), Expr::Number(value, _) if value == "42")
    ));

    let mut lexemes = Vec::new();
    let mnemonic_ref = append_lexeme(&mut lexemes, "LDA");
    let mut records = Vec::new();
    append_record(
        &mut records,
        RESULT_BEGIN_STATEMENT,
        Span {
            line: 1,
            col_start: 0,
            col_end: 0,
        },
        [0, 0, 0, 0],
    );
    append_record(
        &mut records,
        RESULT_MNEMONIC_TEXT,
        Span {
            line: 1,
            col_start: 2,
            col_end: 5,
        },
        [mnemonic_ref.0, mnemonic_ref.1, 0, 0],
    );
    append_record(
        &mut records,
        RESULT_OPERAND_EXPR_SLOT,
        Span {
            line: 1,
            col_start: 6,
            col_end: 9,
        },
        [0, 0, 1, 3],
    );
    append_record(
        &mut records,
        RESULT_FINISH_LINE,
        Span {
            line: 1,
            col_start: 9,
            col_end: 9,
        },
        [0, 0, 0, 0],
    );

    let decoded = decode_statement_result(&records, &lexemes, bridge.expression_slots())
        .expect("native result should decode through host-filled expression slot");
    assert_eq!(format!("{decoded:?}"), format!("{rust_ast:?}"));
}

#[test]
fn native_prvm_abi_host_bridge_preserves_expr_error_slots() {
    let model = model_for_native_abi();
    let register_checker = register_checker_from_fn(families::mos6502::is_register);
    let mut request_record = Vec::new();
    append_expr_request_record(
        &mut request_record,
        0,
        2,
        1,
        1,
        Span {
            line: 1,
            col_start: 6,
            col_end: 6,
        },
    );
    let mut result_slot = vec![0; NATIVE_PRVM_EXPR_RESULT_SLOT_SIZE];
    let mut bridge = NativePrvmHostExpressionBridge::from_source_line(
        &model,
        "m6502",
        None,
        " LDA",
        1,
        &register_checker,
        Some("LDA"),
    )
    .expect("bridge should tokenize native source line");

    let bridged = bridge
        .handle_expression_request_record(&request_record, &mut result_slot)
        .expect("host bridge should preserve empty expression as Expr::Error");
    assert_eq!(
        bridged.slot_state,
        NativePrvmExprSlotState::ReadyExpressionError
    );
    assert!(matches!(
        bridge.expression_for_native_slot(2),
        Some(Expr::Error(message, span))
            if message == "Expected expression" && span.col_start == 6 && span.col_end == 6
    ));

    let decoded_slot = decode_expr_result_slot(&result_slot).expect("slot should decode");
    assert_eq!(
        decoded_slot.state,
        NativeExprSlotState::ReadyExpressionError
    );
    assert_eq!(decoded_slot.expr_slot_index, 2);
    assert_eq!(decoded_slot.host_expr_handle, 0);
}

#[test]
fn native_prvm_abi_maps_newline_and_entry_boundary_statuses() {
    let newline = decode_status_return(STATUS_NEWLINE_UNSUPPORTED, 0, 6, 0);
    assert_eq!(newline.status, STATUS_NEWLINE_UNSUPPORTED);
    assert_eq!(newline.first, 0);
    assert_eq!(newline.second, 6);
    assert_eq!(newline.third, 0);

    let entry_boundary = decode_status_return(STATUS_ENTRY_BOUNDARY, 0, 0, 0);
    assert_eq!(entry_boundary.status, STATUS_ENTRY_BOUNDARY);
    assert_eq!(entry_boundary.first, 0);
    assert_eq!(entry_boundary.second, 0);
    assert_eq!(entry_boundary.third, 0);
}
