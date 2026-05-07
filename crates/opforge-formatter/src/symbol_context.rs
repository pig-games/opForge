// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use std::collections::HashMap;
use std::path::PathBuf;

use opcore::modules::scan_module_ids;
use opcore::parser::{LineAst, Parser};

use super::{FormatterConfig, LabelCaseStyle, SurfaceLineKind, SurfaceParsedDocument};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelRole {
    Generic,
    Routine,
    Data,
    Constant,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocumentFormatContext {
    pub label_roles: Vec<Option<LabelRole>>,
    pub reference_renames: HashMap<String, String>,
    pub selective_import_renames: Vec<HashMap<String, String>>,
}

pub fn effective_label_case(config: &FormatterConfig, role: LabelRole) -> LabelCaseStyle {
    match role {
        LabelRole::Routine if config.routine_label_case != LabelCaseStyle::Keep => {
            config.routine_label_case
        }
        LabelRole::Data if config.data_label_case != LabelCaseStyle::Keep => config.data_label_case,
        LabelRole::Constant if config.constant_label_case != LabelCaseStyle::Keep => {
            config.constant_label_case
        }
        _ => config.label_case,
    }
}

pub fn build_inline_context(
    source: &str,
    parsed: &SurfaceParsedDocument,
    config: &FormatterConfig,
) -> DocumentFormatContext {
    let path = PathBuf::from("<memory>");
    build_project_context(
        &[(path.clone(), source.to_string(), parsed.clone())],
        config,
    )
    .remove(&path)
    .unwrap_or_else(|| DocumentFormatContext {
        label_roles: classify_label_roles(parsed),
        reference_renames: HashMap::new(),
        selective_import_renames: vec![HashMap::new(); parsed.lines.len()],
    })
}

pub fn build_project_context(
    entries: &[(PathBuf, String, SurfaceParsedDocument)],
    config: &FormatterConfig,
) -> HashMap<PathBuf, DocumentFormatContext> {
    let mut interim_docs = Vec::with_capacity(entries.len());
    let mut module_symbol_maps: HashMap<String, HashMap<String, String>> = HashMap::new();

    for (path, source, parsed) in entries {
        let source_lines: Vec<String> = source.lines().map(|line| line.to_string()).collect();
        let module_id = scan_module_ids(&source_lines).into_iter().next();
        let label_roles = classify_label_roles(parsed);
        let local_definitions = collect_local_definition_styles(parsed, &label_roles, config);
        if let Some(module_id) = &module_id {
            module_symbol_maps.insert(module_id.clone(), local_definitions.clone());
        }
        interim_docs.push(InterimDocument {
            path: path.clone(),
            source_lines,
            label_roles,
            local_definitions,
            line_count: parsed.lines.len(),
        });
    }

    let mut contexts = HashMap::with_capacity(interim_docs.len());
    for doc in interim_docs {
        let mut reference_renames = doc.local_definitions.clone();
        let mut selective_import_renames = vec![HashMap::new(); doc.line_count];

        for (idx, line) in doc.source_lines.iter().enumerate() {
            let Some(LineAst::Use(use_ast)) = parse_line_ast(line, idx as u32 + 1) else {
                continue;
            };
            let Some(module_exports) = module_symbol_maps.get(&use_ast.module_id) else {
                continue;
            };
            for item in use_ast.items {
                if item.name == "*" {
                    continue;
                }
                let Some(styled_name) = module_exports.get(&item.name) else {
                    continue;
                };
                if styled_name == &item.name {
                    continue;
                }
                selective_import_renames[idx].insert(item.name.clone(), styled_name.clone());
                if item.alias.is_none() {
                    reference_renames
                        .entry(item.name.clone())
                        .or_insert_with(|| styled_name.clone());
                }
            }
        }

        contexts.insert(
            doc.path,
            DocumentFormatContext {
                label_roles: doc.label_roles,
                reference_renames,
                selective_import_renames,
            },
        );
    }

    contexts
}

fn collect_local_definition_styles(
    parsed: &SurfaceParsedDocument,
    label_roles: &[Option<LabelRole>],
    config: &FormatterConfig,
) -> HashMap<String, String> {
    let mut styles = HashMap::new();
    for (idx, line) in parsed.lines.iter().enumerate() {
        let Some(label) = line.label.as_deref() else {
            continue;
        };
        if label == "*" {
            continue;
        }
        let role = label_roles
            .get(idx)
            .and_then(|role| *role)
            .unwrap_or(LabelRole::Generic);
        styles.insert(
            label.to_string(),
            effective_label_case(config, role).apply(label),
        );
    }
    styles
}

fn parse_line_ast(line: &str, line_num: u32) -> Option<LineAst> {
    let mut parser = Parser::from_line(line, line_num).ok()?;
    parser.parse_compat_mixed_line().ok()
}

fn classify_label_roles(parsed: &SurfaceParsedDocument) -> Vec<Option<LabelRole>> {
    let mut roles = Vec::with_capacity(parsed.lines.len());
    for (idx, line) in parsed.lines.iter().enumerate() {
        if line.label.is_none() {
            roles.push(None);
            continue;
        }
        roles.push(Some(classify_line_role(parsed, idx, line)));
    }
    roles
}

fn classify_line_role(
    parsed: &SurfaceParsedDocument,
    index: usize,
    line: &super::SurfaceParsedLine,
) -> LabelRole {
    match line.kind {
        SurfaceLineKind::Instruction => LabelRole::Routine,
        SurfaceLineKind::Assignment => LabelRole::Constant,
        SurfaceLineKind::Directive => classify_directive_role(line.head.as_deref()),
        SurfaceLineKind::LabelOnly => {
            classify_following_role(parsed, index + 1).unwrap_or(LabelRole::Generic)
        }
        _ => LabelRole::Generic,
    }
}

fn classify_following_role(parsed: &SurfaceParsedDocument, start: usize) -> Option<LabelRole> {
    for line in parsed.lines.iter().skip(start) {
        match line.kind {
            SurfaceLineKind::Empty | SurfaceLineKind::CommentOnly | SurfaceLineKind::LabelOnly => {
                continue;
            }
            SurfaceLineKind::Instruction => return Some(LabelRole::Routine),
            SurfaceLineKind::Assignment => return Some(LabelRole::Constant),
            SurfaceLineKind::Directive => {
                return Some(classify_directive_role(line.head.as_deref()))
            }
            _ => return Some(LabelRole::Generic),
        }
    }
    None
}

fn classify_directive_role(head: Option<&str>) -> LabelRole {
    let Some(head) = head else {
        return LabelRole::Generic;
    };
    match head.to_ascii_lowercase().as_str() {
        ".const" | ".set" => LabelRole::Constant,
        ".var" | ".byte" | ".db" | ".word" | ".dw" | ".long" | ".text" | ".null" | ".ptext"
        | ".ds" | ".emit" | ".res" | ".fill" => LabelRole::Data,
        _ => LabelRole::Generic,
    }
}

struct InterimDocument {
    path: PathBuf,
    source_lines: Vec<String>,
    label_roles: Vec<Option<LabelRole>>,
    local_definitions: HashMap<String, String>,
    line_count: usize,
}
