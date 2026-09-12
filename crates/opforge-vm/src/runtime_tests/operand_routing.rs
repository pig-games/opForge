use super::*;
use types::target_callbacks::{self as audit, Mode};

fn model() -> HierarchyExecutionModel {
    let mut registry = parity_registry();
    register_motorola68000_family_stack(&mut registry);
    HierarchyExecutionModel::from_registry(&registry).unwrap()
}

#[test]
fn generic_directives_never_dispatch_to_family_parsers_across_targets() {
    let model = model();
    let corpus = [
        ".cpu next_cpu",
        ".unknown_shared_name (base+2)",
        ".org (base+2)*3",
        ".byte 1,2",
        ".word (base+2)",
        ".long .len({1,2})",
        ".byte {1,2}",
        ".text \"hello\"",
        ".if (flag+2)>3",
        ".struct item",
        ".byte 1+",
        ".word (1+2",
        ".byte 1+,2",
    ];
    for line in corpus {
        let _audit = audit::install(Mode::Refuse);
        let expected = model
            .parse_portable_line_for_assembler("m6502", None, line, 7)
            .map_err(|err| (err.message, err.span));
        for cpu in ["z80", "m68000", "m6809"] {
            let actual = model
                .parse_portable_line_for_assembler(cpu, None, line, 7)
                .map_err(|err| (err.message, err.span));
            assert_eq!(actual, expected, "{cpu}: {line}");
            audit::check().unwrap();
        }
        assert!(audit::snapshot().unwrap()["attempts"]
            .as_array()
            .unwrap()
            .is_empty());
    }
}

#[test]
fn core_grouping_is_not_instruction_indirection_and_errors_keep_boundaries() {
    let model = model();
    let _audit = audit::install(Mode::Refuse);
    let line = model
        .parse_portable_line_for_assembler("m68000", None, ".word (1+2)", 3)
        .unwrap();
    let LineAst::Statement(statement) = line.to_core_line_ast() else {
        panic!("statement")
    };
    assert!(matches!(
        &statement.operands[0],
        Expr::Binary {
            op: BinaryOp::Add,
            ..
        }
    ));
    let line = model
        .parse_portable_line_for_assembler("m68000", None, " move.l (a0),d0", 3)
        .unwrap();
    let LineAst::Statement(statement) = line.to_core_line_ast() else {
        panic!("statement")
    };
    assert!(matches!(&statement.operands[0], Expr::Indirect(_, _)));
    let line = model
        .parse_portable_line_for_assembler("m68000", None, ".byte 1+,2", 3)
        .unwrap();
    let LineAst::Statement(statement) = line.to_core_line_ast() else {
        panic!("statement")
    };
    let Expr::Error(message, span) = &statement.operands[0] else {
        panic!("expression error")
    };
    assert_eq!(message, "Unexpected token in expression");
    assert_eq!(
        *span,
        Span {
            line: 3,
            col_start: 9,
            col_end: 10
        }
    );
    audit::check().unwrap();
}

#[test]
fn shared_instruction_atoms_preserve_ast_spans_and_budget_checks() {
    let mut model = model();
    let _audit = audit::install(Mode::Refuse);
    for (cpu, line) in [
        ("m6502", " lda value"),
        ("z80", " mvi a,7"),
        ("m68000", " moveq #7,d0"),
        ("m6809", " lda value"),
    ] {
        let parsed = model
            .parse_portable_line_for_assembler(cpu, None, line, 5)
            .unwrap();
        let LineAst::Statement(statement) = parsed.to_core_line_ast() else {
            panic!("statement")
        };
        assert!(!statement.operands.is_empty());
        assert!(statement
            .operands
            .iter()
            .all(|expr| !matches!(expr, Expr::Error(_, _))));
        for operand in &statement.operands {
            let span = opcore::expression::expr_span(operand);
            assert_eq!(span.line, 5);
            assert!(span.col_start > 1 && span.col_end <= line.len() + 1);
        }
        audit::check().unwrap();
    }
    let mut limits = model.runtime_budget_limits();
    limits.max_parser_tokens_per_line = 0;
    model.set_runtime_budget_limits_for_tests(limits);
    let failure = match model.parse_portable_line_for_assembler("m68000", None, ".word 7", 1) {
        Err(err) => err.message,
        Ok(ast) => {
            let LineAst::Statement(statement) = ast.to_core_line_ast() else {
                panic!("statement")
            };
            let Expr::Error(message, _) = &statement.operands[0] else {
                panic!("budget error")
            };
            message.clone()
        }
    };
    assert!(
        failure.contains("parser token budget exceeded"),
        "{failure}"
    );
}
