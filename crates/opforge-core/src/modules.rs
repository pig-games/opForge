// SPDX-License-Identifier: GPL-3.0-or-later

//! Parser-backed helpers for module and `.use` syntax.

use crate::parser::{Expr, LineAst, Parser};
use types::processing::{ProcessingOutcome, ProcessingReturn};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseDirectiveSpec {
    pub module_id: String,
    pub alias: Option<String>,
    pub items: Vec<String>,
    pub item_aliases: Vec<Option<String>>,
    pub section_maps: Vec<(String, String)>,
}

fn parse_line_ast(line: &str, line_num: u32) -> Option<LineAst> {
    match process_module_item_request(line, line_num) {
        ProcessingOutcome::Done(ast) => Some(ast),
        ProcessingOutcome::Return(_) | ProcessingOutcome::Error(_) => None,
    }
}

pub fn process_module_item_request(
    line: &str,
    line_num: u32,
) -> ProcessingOutcome<LineAst, crate::parser::ParseError> {
    match Parser::process_opcore_line_request(line, line_num) {
        ProcessingOutcome::Done(ast @ LineAst::Use(..)) => ProcessingOutcome::Done(ast),
        ProcessingOutcome::Done(ref ast @ LineAst::Statement(ref statement)) => {
            let Some(mnemonic) = statement.mnemonic.as_deref() else {
                return ProcessingOutcome::Return(ProcessingReturn::Unknown);
            };
            if mnemonic.eq_ignore_ascii_case(".module")
                || mnemonic.eq_ignore_ascii_case(".endmodule")
            {
                ProcessingOutcome::Done(ast.clone())
            } else {
                ProcessingOutcome::Return(ProcessingReturn::Unknown)
            }
        }
        ProcessingOutcome::Done(_) => ProcessingOutcome::Return(ProcessingReturn::Unknown),
        ProcessingOutcome::Return(ret) => ProcessingOutcome::Return(ret),
        ProcessingOutcome::Error(err) => ProcessingOutcome::Error(err),
    }
}

#[must_use]
pub fn expr_to_ident(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Identifier(name, _) | Expr::Register(name, _) => Some(name.clone()),
        _ => None,
    }
}

#[must_use]
pub fn scan_module_ids(lines: &[String]) -> Vec<String> {
    let mut modules = Vec::new();
    for (idx, line) in lines.iter().enumerate() {
        let Some(LineAst::Statement(statement)) = parse_line_ast(line, idx as u32 + 1) else {
            continue;
        };
        let mnemonic = statement.mnemonic;
        let operands = statement.operands;
        let Some(mnemonic) = mnemonic else { continue };
        if !mnemonic.eq_ignore_ascii_case(".module") {
            continue;
        }
        if let Some(expr) = operands.first() {
            if let Some(name) = expr_to_ident(expr) {
                modules.push(name);
            }
        }
    }
    modules
}

#[must_use]
pub fn collect_use_directives(lines: &[String]) -> Vec<String> {
    let mut uses = Vec::new();
    for (idx, line) in lines.iter().enumerate() {
        let Some(ast) = parse_line_ast(line, idx as u32 + 1) else {
            continue;
        };
        if let LineAst::Use(use_ast) = ast {
            let module_id = use_ast.module_id;
            uses.push(module_id);
        }
    }
    uses
}

#[must_use]
pub fn collect_use_directives_with_items(lines: &[String]) -> Vec<UseDirectiveSpec> {
    let mut uses = Vec::new();
    for (idx, line) in lines.iter().enumerate() {
        let Some(ast) = parse_line_ast(line, idx as u32 + 1) else {
            continue;
        };
        if let LineAst::Use(use_ast) = ast {
            let item_names: Vec<String> =
                use_ast.items.iter().map(|item| item.name.clone()).collect();
            let item_aliases: Vec<Option<String>> = use_ast
                .items
                .iter()
                .map(|item| item.alias.clone())
                .collect();
            let section_maps: Vec<(String, String)> = use_ast
                .section_maps
                .iter()
                .map(|section_map| (section_map.logical.clone(), section_map.concrete.clone()))
                .collect();
            uses.push(UseDirectiveSpec {
                module_id: use_ast.module_id,
                alias: use_ast.alias,
                items: item_names,
                item_aliases,
                section_maps,
            });
        }
    }
    uses
}

#[must_use]
pub fn extract_module_block(lines: &[String], module_id: &str) -> Option<Vec<String>> {
    let mut captured = Vec::new();
    let mut capture = false;
    let mut depth = 0usize;
    for (idx, line) in lines.iter().enumerate() {
        let Some(LineAst::Statement(statement)) = parse_line_ast(line, idx as u32 + 1) else {
            if capture {
                captured.push(line.clone());
            }
            continue;
        };
        let mnemonic = statement.mnemonic;
        let operands = statement.operands;
        let Some(mnemonic) = mnemonic else {
            if capture {
                captured.push(line.clone());
            }
            continue;
        };
        if mnemonic.eq_ignore_ascii_case(".module") {
            if !capture {
                if let Some(expr) = operands.first() {
                    if let Some(name) = expr_to_ident(expr) {
                        if name.eq_ignore_ascii_case(module_id) {
                            capture = true;
                            depth = 1;
                            captured.push(line.clone());
                            continue;
                        }
                    }
                }
            } else {
                depth += 1;
                captured.push(line.clone());
                continue;
            }
        }
        if mnemonic.eq_ignore_ascii_case(".endmodule") && capture {
            captured.push(line.clone());
            depth = depth.saturating_sub(1);
            if depth == 0 {
                break;
            }
            continue;
        }
        if capture {
            captured.push(line.clone());
        }
    }
    if capture {
        Some(captured)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::{
        collect_use_directives, collect_use_directives_with_items, extract_module_block,
        scan_module_ids, UseDirectiveSpec,
    };

    #[test]
    fn scan_module_ids_detects_declared_modules() {
        let lines = vec![
            ".module mforth.base".to_string(),
            "    ; body".to_string(),
            ".endmodule".to_string(),
        ];
        assert_eq!(scan_module_ids(&lines), vec!["mforth.base".to_string()]);
    }

    #[test]
    fn collect_use_directives_detects_dependencies() {
        let lines = vec![
            ".use mforth.base".to_string(),
            ".use mforth.kernel (*)".to_string(),
        ];
        assert_eq!(
            collect_use_directives(&lines),
            vec!["mforth.base".to_string(), "mforth.kernel".to_string()]
        );
    }

    #[test]
    fn collect_use_directives_with_items_keeps_alias_and_items() {
        let lines = vec![".use mforth.kernel (foo, bar) as kern".to_string()];
        assert_eq!(
            collect_use_directives_with_items(&lines),
            vec![UseDirectiveSpec {
                module_id: "mforth.kernel".to_string(),
                alias: Some("kern".to_string()),
                items: vec!["foo".to_string(), "bar".to_string()],
                item_aliases: vec![None, None],
                section_maps: vec![],
            }]
        );
    }

    #[test]
    fn collect_use_directives_with_items_keeps_item_aliases() {
        let lines = vec![".use mforth.kernel (foo as f, bar)".to_string()];
        assert_eq!(
            collect_use_directives_with_items(&lines),
            vec![UseDirectiveSpec {
                module_id: "mforth.kernel".to_string(),
                alias: None,
                items: vec!["foo".to_string(), "bar".to_string()],
                item_aliases: vec![Some("f".to_string()), None],
                section_maps: vec![],
            }]
        );
    }

    #[test]
    fn collect_use_directives_with_items_keeps_qualified_section_maps() {
        let lines = vec![".use opasm.amigaos.engine (sessionPass) as engine map { code -> app_code, tables -> app_data }".to_string()];
        assert_eq!(
            collect_use_directives_with_items(&lines),
            vec![UseDirectiveSpec {
                module_id: "opasm.amigaos.engine".to_string(),
                alias: Some("engine".to_string()),
                items: vec!["sessionPass".to_string()],
                item_aliases: vec![None],
                section_maps: vec![
                    ("code".to_string(), "app_code".to_string()),
                    ("tables".to_string(), "app_data".to_string()),
                ],
            }]
        );
    }

    #[test]
    fn extract_module_block_returns_nested_module_lines() {
        let lines = vec![
            ".module outer".to_string(),
            "value = 1".to_string(),
            ".module inner".to_string(),
            ".endmodule".to_string(),
            ".endmodule".to_string(),
        ];

        assert_eq!(extract_module_block(&lines, "outer"), Some(lines));
    }
}
