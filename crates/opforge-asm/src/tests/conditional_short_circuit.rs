//! Inactive conditional expressions do not resolve symbols or raise evaluation errors.
use super::*;

#[test]
fn conditional_short_circuit_skips_inactive_expressions() {
    for expr in ["missing", "1/0"] {
        let mut symbols = SymbolTable::new();
        let registry = default_registry();
        let mut asm = make_asm_line(&mut symbols, &registry);
        let lines = [
            ".if 0".to_string(),
            format!(".if {expr}"),
            ".else".to_string(),
            ".endif".to_string(),
            format!(".match {expr}"),
            format!(".case {expr}"),
            ".endmatch".to_string(),
            ".else".to_string(),
            ".byte 7".to_string(),
        ];
        for line in &lines {
            let status = process_line(&mut asm, &format!("    {line}"), 0, 2);
            assert_ne!(status, LineStatus::Error, "{line}: {}", asm.error_message());
        }
        assert_eq!(asm.bytes(), &[7]);
        asm.clear_conditionals();
        for line in [
            ".if 1".to_string(),
            format!(".elseif {expr}"),
            ".elseif 0".to_string(),
            ".else".to_string(),
            ".endif".to_string(),
            ".match 2".to_string(),
            ".case 2".to_string(),
            format!(".case {expr}"),
            ".default".to_string(),
            ".endmatch".to_string(),
        ] {
            assert_eq!(
                process_line(&mut asm, &format!("    {line}"), 0, 2),
                LineStatus::Ok,
                "{line}: {}",
                asm.error_message()
            );
        }
        assert!(asm.cond_is_empty());
    }
}

#[test]
fn conditional_short_circuit_preserves_active_expression_errors() {
    for expr in ["missing", "1/0"] {
        for (prefix, directive) in [
            (None, ".if"),
            (None, ".match"),
            (Some(".if 0"), ".elseif"),
            (Some(".match 2"), ".case"),
        ] {
            let mut symbols = SymbolTable::new();
            let registry = default_registry();
            let mut asm = make_asm_line(&mut symbols, &registry);
            if let Some(prefix) = prefix {
                assert_eq!(
                    process_line(&mut asm, &format!("    {prefix}"), 0, 2),
                    LineStatus::Ok
                );
            }
            assert_eq!(
                process_line(&mut asm, &format!("    {directive} {expr}"), 0, 2),
                LineStatus::Error
            );
            assert_ne!(asm.error().unwrap().kind(), AsmErrorKind::Conditional);
        }
    }
}

#[test]
fn conditional_short_circuit_preserves_structural_errors() {
    for (prefix, invalid, expected) in [
        (
            vec![".if 1", ".else"],
            ".elseif 1/0",
            ".else or .elseif cannot follow .else",
        ),
        (
            vec![".if 1", ".else"],
            ".else",
            ".else or .elseif cannot follow .else",
        ),
        (
            vec![".if 0", ".elseif 0", ".else"],
            ".elseif missing",
            ".else or .elseif cannot follow .else",
        ),
        (
            vec![".match 1", ".case 1", ".default"],
            ".case 1/0",
            ".case cannot follow .default",
        ),
        (
            vec![".match 1"],
            ".elseif 1/0",
            ".else or .elseif found without matching .if",
        ),
    ] {
        let mut symbols = SymbolTable::new();
        let registry = default_registry();
        let mut asm = make_asm_line(&mut symbols, &registry);
        for line in prefix {
            assert_eq!(
                process_line(&mut asm, &format!("    {line}"), 0, 2),
                LineStatus::Ok
            );
        }
        assert_eq!(
            process_line(&mut asm, &format!("    {invalid}"), 0, 2),
            LineStatus::Error
        );
        assert_eq!(asm.error().unwrap().kind(), AsmErrorKind::Conditional);
        assert!(
            asm.error_message().contains(expected),
            "{}",
            asm.error_message()
        );
    }
}
