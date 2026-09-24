// SPDX-License-Identifier: GPL-3.0-or-later

use std::collections::{HashMap, HashSet};
use std::io;
use std::path::{Path, PathBuf};

use asm::error::{AsmError, AsmErrorKind, AsmRunError, Diagnostic, Severity};
use asm::phase_profile::{self, PhaseBucket};
use asm::preprocess::{AsmMacroExports, AsmMacroProcessor};
use opcore::expr::{eval_expr as eval_core_expr, EvalContext};
use opcore::macro_processor::CompileTimeVisibility;
use opcore::modules::{expr_to_ident, extract_module_block, UseDirectiveSpec};
use opcore::parser::{Expr, LineAst, Parser};
use opcore::tokenizer::{ConditionalKind, Span};
use types::path_display::stable_path_string;
use types::processing::ProcessingOutcome;
use types::source_map::{SourceMap, SourceOrigin};
use types::symbol::SymbolVisibility;

use crate::{FsSourceProvider, SourceProvider};

const DEFAULT_MODULE_EXTENSIONS: &[&str] = &["asm", "inc"];

#[derive(Debug, Clone)]
struct ModuleFileInfo {
    path: PathBuf,
    source_root: PathBuf,
    has_explicit_modules: bool,
}

#[derive(Debug, Default)]
struct ModuleIndex {
    modules: HashMap<String, Vec<ModuleFileInfo>>,
}

#[derive(Debug, Clone)]
struct ModuleSource {
    path: PathBuf,
    lines: Vec<String>,
    first_line: u32,
}

struct ModuleLoadContext<'a> {
    index: &'a ModuleIndex,
    loaded: &'a mut HashSet<String>,
    entry_modules: &'a HashMap<String, ModuleSource>,
    order: &'a mut Vec<(String, ModuleSource)>,
    stack: &'a mut Vec<String>,
    defines: &'a [String],
    include_roots: &'a [PathBuf],
    dependency_files: &'a mut HashSet<PathBuf>,
    pp_macro_depth: usize,
    source_provider: &'a dyn SourceProvider,
}

#[derive(Debug, Clone)]
struct ModuleUseRef {
    module_id: String,
    span: Span,
}

fn canonical_module_id(module_id: &str) -> String {
    module_id.to_ascii_lowercase()
}

fn module_search_root(root_path: &Path) -> PathBuf {
    match root_path.parent() {
        Some(parent) if !parent.as_os_str().is_empty() => parent.to_path_buf(),
        _ => PathBuf::from("."),
    }
}

fn module_id_from_path(path: &Path) -> Result<String, AsmRunError> {
    crate::root_module_id_from_lines(path, &[])
}

fn expand_with_processor(
    mp: &mut AsmMacroProcessor,
    lines: &[String],
) -> Result<Vec<String>, AsmRunError> {
    let _expand_scope = phase_profile::scope(PhaseBucket::PrepareMacroSegmentStatementExpand);
    match mp.expand(lines) {
        Ok(lines) => Ok(lines),
        Err(err) => {
            let err_msg = AsmError::new(AsmErrorKind::Preprocess, err.message(), None);
            let mut diagnostics = Vec::new();
            if let Some(line) = err.line() {
                diagnostics.push(
                    Diagnostic::new(line, Severity::Error, err_msg.clone())
                        .with_column(err.column()),
                );
            }
            Err(AsmRunError::new_with_traces(
                err_msg,
                diagnostics,
                lines.to_vec(),
                mp.take_runtime_processing_traces(),
            ))
        }
    }
}

fn is_wildcard_selective(items: &[String]) -> bool {
    items.len() == 1 && items[0] == "*"
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ActiveConditionalKind {
    If,
    Switch,
}

#[derive(Debug, Clone)]
struct ActiveConditionalFrame {
    kind: ActiveConditionalKind,
    parent_active: bool,
    branch_taken: bool,
    current_active: bool,
    switch_value: Option<i64>,
}

#[derive(Debug, Default)]
struct StaticConditionalEvalContext;

impl EvalContext for StaticConditionalEvalContext {
    fn lookup_symbol(&self, _name: &str) -> Option<i64> {
        None
    }

    fn current_address(&self) -> Option<i64> {
        None
    }
}

fn current_branch_is_active(stack: &[ActiveConditionalFrame]) -> bool {
    stack
        .last()
        .map(|frame| frame.current_active)
        .unwrap_or(true)
}

fn eval_static_condition(expr: &Expr) -> Option<bool> {
    eval_core_expr(expr, &StaticConditionalEvalContext)
        .ok()
        .map(|value| value != 0)
}

fn apply_conditional_ast(
    stack: &mut Vec<ActiveConditionalFrame>,
    kind: ConditionalKind,
    exprs: &[Expr],
) {
    let parent_active = current_branch_is_active(stack);
    match kind {
        ConditionalKind::If => {
            let branch_active = parent_active
                && exprs
                    .first()
                    .and_then(eval_static_condition)
                    .unwrap_or(false);
            stack.push(ActiveConditionalFrame {
                kind: ActiveConditionalKind::If,
                parent_active,
                branch_taken: branch_active,
                current_active: branch_active,
                switch_value: None,
            });
        }
        ConditionalKind::ElseIf => {
            let Some(frame) = stack.last_mut() else {
                return;
            };
            if frame.kind != ActiveConditionalKind::If {
                return;
            }
            let branch_active = frame.parent_active
                && !frame.branch_taken
                && exprs
                    .first()
                    .and_then(eval_static_condition)
                    .unwrap_or(false);
            frame.current_active = branch_active;
            frame.branch_taken |= branch_active;
        }
        ConditionalKind::Else => {
            let Some(frame) = stack.last_mut() else {
                return;
            };
            if frame.kind != ActiveConditionalKind::If {
                return;
            }
            let branch_active = frame.parent_active && !frame.branch_taken;
            frame.current_active = branch_active;
            frame.branch_taken |= branch_active;
        }
        ConditionalKind::EndIf => {
            if matches!(
                stack.last().map(|frame| frame.kind),
                Some(ActiveConditionalKind::If)
            ) {
                stack.pop();
            }
        }
        ConditionalKind::Switch => {
            let switch_value = if parent_active {
                exprs
                    .first()
                    .and_then(|expr| eval_core_expr(expr, &StaticConditionalEvalContext).ok())
            } else {
                None
            };
            stack.push(ActiveConditionalFrame {
                kind: ActiveConditionalKind::Switch,
                parent_active,
                branch_taken: false,
                current_active: false,
                switch_value,
            });
        }
        ConditionalKind::Case => {
            let Some(frame) = stack.last_mut() else {
                return;
            };
            if frame.kind != ActiveConditionalKind::Switch {
                return;
            }
            let branch_active = frame.parent_active
                && !frame.branch_taken
                && frame.switch_value.is_some()
                && exprs.iter().any(|expr| {
                    eval_core_expr(expr, &StaticConditionalEvalContext)
                        .ok()
                        .zip(frame.switch_value)
                        .is_some_and(|(value, switch)| value == switch)
                });
            frame.current_active = branch_active;
            frame.branch_taken |= branch_active;
        }
        ConditionalKind::Default => {
            let Some(frame) = stack.last_mut() else {
                return;
            };
            if frame.kind != ActiveConditionalKind::Switch {
                return;
            }
            let branch_active = frame.parent_active && !frame.branch_taken;
            frame.current_active = branch_active;
            frame.branch_taken |= branch_active;
        }
        ConditionalKind::EndSwitch => {
            if matches!(
                stack.last().map(|frame| frame.kind),
                Some(ActiveConditionalKind::Switch)
            ) {
                stack.pop();
            }
        }
    }
}

fn scan_active_module_items(lines: &[String]) -> Vec<LineAst> {
    let _parse_scope = phase_profile::scope(PhaseBucket::PrepareParseLineAst);
    let mut out = Vec::new();
    let mut stack = Vec::new();
    for (idx, line) in lines.iter().enumerate() {
        let ProcessingOutcome::Done(ast) =
            Parser::process_opcore_line_request(line, idx as u32 + 1)
        else {
            continue;
        };
        if let LineAst::Conditional(cond) = &ast {
            apply_conditional_ast(&mut stack, cond.kind, &cond.exprs);
            continue;
        }
        if current_branch_is_active(&stack) {
            out.push(ast);
        }
    }
    out
}

pub(crate) fn scan_module_ids_from_processing(lines: &[String]) -> Vec<String> {
    scan_module_starts_from_processing(lines)
        .into_iter()
        .map(|(id, _)| id)
        .collect()
}

fn scan_module_starts_from_processing(lines: &[String]) -> Vec<(String, usize)> {
    let mut modules = Vec::new();
    let mut stack = Vec::new();
    for (index, line) in lines.iter().enumerate() {
        let ProcessingOutcome::Done(ast) =
            Parser::process_opcore_line_request(line, index as u32 + 1)
        else {
            continue;
        };
        if let LineAst::Conditional(cond) = &ast {
            apply_conditional_ast(&mut stack, cond.kind, &cond.exprs);
            continue;
        }
        if !current_branch_is_active(&stack) {
            continue;
        }
        let LineAst::Statement(statement) = ast else {
            continue;
        };
        let Some(mnemonic) = statement.mnemonic.as_deref() else {
            continue;
        };
        if !mnemonic.eq_ignore_ascii_case(".module") {
            continue;
        }
        if let Some(expr) = statement.operands.first() {
            if let Some(name) = expr_to_ident(expr) {
                modules.push((name, index));
            }
        }
    }
    modules
}

fn collect_use_directives_from_processing(lines: &[String]) -> Vec<ModuleUseRef> {
    let mut uses = Vec::new();
    for ast in scan_active_module_items(lines) {
        let LineAst::Use(use_ast) = ast else {
            continue;
        };
        uses.push(ModuleUseRef {
            module_id: use_ast.module_id,
            span: use_ast.span,
        });
    }
    uses
}

fn collect_use_directives_with_items_from_processing(lines: &[String]) -> Vec<UseDirectiveSpec> {
    let mut uses = Vec::new();
    for ast in scan_active_module_items(lines) {
        let LineAst::Use(use_ast) = ast else {
            continue;
        };
        let item_aliases = use_ast
            .items
            .iter()
            .map(|item| item.alias.clone())
            .collect();
        let section_maps = use_ast
            .section_maps
            .iter()
            .map(|section_map| (section_map.logical.clone(), section_map.concrete.clone()))
            .collect();
        uses.push(UseDirectiveSpec {
            module_id: use_ast.module_id,
            alias: use_ast.alias,
            items: use_ast.items.into_iter().map(|item| item.name).collect(),
            item_aliases,
            section_maps,
        });
    }
    uses
}

fn collect_source_files(
    root: &Path,
    extensions: &[&str],
    source_provider: &dyn SourceProvider,
) -> io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let entries = source_provider.read_dir(&dir)?;

        for path in entries {
            if source_provider.is_dir(&path)? {
                stack.push(path);
                continue;
            }
            let ext = path.extension().and_then(|s| s.to_str()).unwrap_or("");
            if extensions
                .iter()
                .any(|candidate| candidate.eq_ignore_ascii_case(ext))
            {
                files.push(path);
            }
        }
    }
    Ok(files)
}

fn build_module_index(
    roots: &[PathBuf],
    entry_path: &Path,
    entry_lines: &[String],
    source_provider: &dyn SourceProvider,
) -> Result<ModuleIndex, AsmRunError> {
    let mut index = ModuleIndex::default();
    for root in roots {
        let files = collect_source_files(root, DEFAULT_MODULE_EXTENSIONS, source_provider)
            .map_err(|err| {
                AsmRunError::new(
                    AsmError::new(AsmErrorKind::Io, "Error reading module roots", None),
                    vec![],
                    vec![err.to_string()],
                )
            })?;

        for path in files {
            let lines = if path == entry_path {
                entry_lines.to_vec()
            } else {
                let contents = source_provider.read_string(&path).map_err(|err| {
                    AsmRunError::new(
                        AsmError::new(AsmErrorKind::Io, "Error reading module source", None),
                        vec![],
                        vec![err.to_string()],
                    )
                })?;
                contents.lines().map(str::to_owned).collect()
            };
            let explicit_modules = scan_module_ids_from_processing(&lines);
            if explicit_modules.is_empty() {
                let implicit_id = module_id_from_path(&path)?;
                let canonical = canonical_module_id(&implicit_id);
                index
                    .modules
                    .entry(canonical)
                    .or_default()
                    .push(ModuleFileInfo {
                        path,
                        source_root: root.clone(),
                        has_explicit_modules: false,
                    });
                continue;
            }
            for module_id in explicit_modules {
                let canonical = canonical_module_id(&module_id);
                index
                    .modules
                    .entry(canonical)
                    .or_default()
                    .push(ModuleFileInfo {
                        path: path.clone(),
                        source_root: root.clone(),
                        has_explicit_modules: true,
                    });
            }
        }
    }

    for infos in index.modules.values_mut() {
        infos.sort_by(|left, right| left.path.cmp(&right.path));
        infos.dedup_by(|left, right| left.path == right.path);
    }

    Ok(index)
}

fn load_module_recursive(
    import: &ModuleUseRef,
    ctx: &mut ModuleLoadContext<'_>,
    importing_path: &Path,
    importing_lines: &[String],
) -> Result<(), AsmRunError> {
    let _module_use_scope = phase_profile::scope(PhaseBucket::PrepareModuleUseImport);
    let module_id = import.module_id.as_str();
    let canonical = canonical_module_id(module_id);
    if let Some(pos) = ctx
        .stack
        .iter()
        .position(|name| canonical_module_id(name) == canonical)
    {
        let mut cycle: Vec<String> = ctx.stack[pos..].to_vec();
        cycle.push(module_id.to_string());
        let message = format!("cyclic module import: {}", cycle.join(" -> "));
        return Err(module_import_error(
            &message,
            Some(module_id),
            import,
            importing_path,
            importing_lines,
        ));
    }
    if ctx.loaded.contains(&canonical) {
        return Ok(());
    }
    let source = if let Some(source) = ctx.entry_modules.get(&canonical) {
        source.clone()
    } else {
        let infos = ctx.index.modules.get(&canonical).ok_or_else(|| {
            let mut message = format!("unknown module: {module_id}");
            if !ctx.stack.is_empty() {
                let chain = ctx.stack.join(" -> ");
                message.push_str(&format!(" (import stack: {chain})"));
            }
            module_import_error(
                &message,
                Some(module_id),
                import,
                importing_path,
                importing_lines,
            )
        })?;
        if infos.len() > 1 {
            let mut message = format!("Ambiguous module: {module_id}");
            if !ctx.stack.is_empty() {
                let chain = ctx.stack.join(" -> ");
                message.push_str(&format!(" (import stack: {chain})"));
            }
            let candidates = infos
                .iter()
                .map(|info| {
                    format!(
                        "{} [root: {}]",
                        info.path.to_string_lossy(),
                        info.source_root.to_string_lossy()
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            message.push_str(&format!("; candidates: {candidates}"));
            return Err(AsmRunError::new(
                AsmError::new(AsmErrorKind::Directive, &message, None),
                vec![],
                vec![],
            ));
        }
        let info = &infos[0];

        let (source_lines, dependency_files) =
            crate::expand_source_file_with_dependencies_with_provider(
                &info.path,
                ctx.defines,
                ctx.include_roots,
                ctx.pp_macro_depth,
                ctx.source_provider,
            )?;
        for path in dependency_files {
            ctx.dependency_files.insert(path);
        }
        let module_lines = if info.has_explicit_modules {
            let matching_declarations = scan_module_ids_from_processing(&source_lines)
                .into_iter()
                .filter(|name| name.eq_ignore_ascii_case(module_id))
                .count();
            if matching_declarations > 1 {
                return Err(module_import_error(
                    &format!("Ambiguous module declaration: {module_id}"),
                    Some(module_id),
                    import,
                    importing_path,
                    importing_lines,
                ));
            }
            extract_module_block(&source_lines, module_id).ok_or_else(|| {
                AsmRunError::new(
                    AsmError::new(
                        AsmErrorKind::Directive,
                        "Module not found in source",
                        Some(module_id),
                    ),
                    vec![],
                    vec![],
                )
            })?
        } else {
            source_lines.clone()
        };

        let first_line = source_lines
            .windows(module_lines.len().max(1))
            .position(|window| window == module_lines.as_slice())
            .map_or(1, |offset| offset as u32 + 1);
        ModuleSource {
            path: info.path.clone(),
            lines: module_lines,
            first_line,
        }
    };
    ctx.stack.push(module_id.to_string());
    for mut dep in collect_use_directives_from_processing(&source.lines) {
        dep.span.line += source.first_line - 1;
        load_module_recursive(&dep, ctx, &source.path, &source.lines)?;
    }

    ctx.loaded.insert(canonical);
    ctx.order.push((module_id.to_string(), source));
    ctx.stack.pop();
    Ok(())
}

fn module_import_error(
    message: &str,
    param: Option<&str>,
    import: &ModuleUseRef,
    importing_path: &Path,
    importing_lines: &[String],
) -> AsmRunError {
    let error = AsmError::new(AsmErrorKind::Directive, message, param);
    let diagnostic = Diagnostic::new(import.span.line, Severity::Error, error.clone())
        .with_column(Some(import.span.col_start))
        .with_col_end(Some(import.span.col_end))
        .with_file(Some(stable_path_string(importing_path)));
    AsmRunError::new(error, vec![diagnostic], importing_lines.to_vec())
}

#[derive(Debug)]
pub struct ModuleGraphResult {
    pub lines: Vec<String>,
    pub source_map: SourceMap,
    pub dependency_files: Vec<PathBuf>,
    pub module_macro_names: HashMap<String, HashMap<String, SymbolVisibility>>,
}

pub fn module_search_root_for_path(root_path: &Path) -> PathBuf {
    module_search_root(root_path)
}

pub fn load_module_graph(
    root_path: &Path,
    root_lines: Vec<String>,
    defines: &[String],
    include_roots: &[PathBuf],
    module_roots: &[PathBuf],
    pp_macro_depth: usize,
) -> Result<ModuleGraphResult, AsmRunError> {
    let source_provider = FsSourceProvider;
    load_module_graph_with_provider(
        root_path,
        root_lines,
        defines,
        include_roots,
        module_roots,
        pp_macro_depth,
        &source_provider,
    )
}

pub fn load_module_graph_with_provider(
    root_path: &Path,
    root_lines: Vec<String>,
    defines: &[String],
    include_roots: &[PathBuf],
    module_roots: &[PathBuf],
    pp_macro_depth: usize,
    source_provider: &dyn SourceProvider,
) -> Result<ModuleGraphResult, AsmRunError> {
    let root_dir = module_search_root(root_path);
    let mut search_roots = Vec::with_capacity(module_roots.len() + 1);
    search_roots.push(root_dir);
    for root in module_roots {
        if !search_roots.iter().any(|existing| existing == root) {
            search_roots.push(root.clone());
        }
    }
    let index = build_module_index(&search_roots, root_path, &root_lines, source_provider)?;

    // Entry modules are graph nodes, not already-loaded dependencies. Keep the
    // caller's prepared lines: rereading the file loses preprocessing results.
    let explicit_modules = scan_module_starts_from_processing(&root_lines);
    let mut entry_modules = HashMap::new();
    let mut entry_order = Vec::new();
    let mut prefix = Vec::new();
    let mut suffix = Vec::new();
    let mut suffix_line = 1;
    if explicit_modules.is_empty() {
        let id = module_id_from_path(root_path)?;
        entry_order.push(id.clone());
        entry_modules.insert(
            canonical_module_id(&id),
            ModuleSource {
                path: root_path.to_path_buf(),
                lines: root_lines.clone(),
                first_line: 1,
            },
        );
    } else {
        let mut cursor = 0;
        for (id, start) in explicit_modules {
            let lines = extract_module_block(&root_lines[start..], &id).ok_or_else(|| {
                AsmRunError::new(
                    AsmError::new(AsmErrorKind::Directive, "Entry module not found", Some(&id)),
                    vec![],
                    root_lines.clone(),
                )
            })?;
            let end = start + lines.len();
            if cursor == 0 {
                prefix.extend_from_slice(&root_lines[..start]);
            }
            let block_start = if cursor == 0 { start } else { cursor };
            let canonical = canonical_module_id(&id);
            if entry_modules.contains_key(&canonical) {
                return Err(AsmRunError::new(
                    AsmError::new(AsmErrorKind::Directive, "Duplicate entry module", Some(&id)),
                    vec![],
                    root_lines,
                ));
            }
            entry_order.push(id);
            entry_modules.insert(
                canonical,
                ModuleSource {
                    path: root_path.to_path_buf(),
                    lines: root_lines[block_start..end].to_vec(),
                    first_line: block_start as u32 + 1,
                },
            );
            cursor = end;
        }
        suffix.extend_from_slice(&root_lines[cursor..]);
        suffix_line = cursor as u32 + 1;
    }

    let mut loaded = HashSet::new();
    let mut order = Vec::new();
    let mut stack = Vec::new();
    let mut dependency_files = HashSet::new();
    let mut ctx = ModuleLoadContext {
        index: &index,
        loaded: &mut loaded,
        entry_modules: &entry_modules,
        order: &mut order,
        stack: &mut stack,
        defines,
        include_roots,
        dependency_files: &mut dependency_files,
        pp_macro_depth,
        source_provider,
    };
    for id in entry_order {
        let import = ModuleUseRef {
            module_id: id,
            span: Span {
                line: 1,
                col_start: 1,
                col_end: 1,
            },
        };
        load_module_recursive(&import, &mut ctx, root_path, &root_lines)?;
    }

    let mut module_exports: HashMap<String, AsmMacroExports> = HashMap::new();
    let mut expanded_deps: Vec<(String, ModuleSource)> = Vec::new();

    for (module_id, source) in &order {
        let module_lines = &source.lines;
        let canonical = canonical_module_id(module_id);
        let use_directives: Vec<UseDirectiveSpec> =
            collect_use_directives_with_items_from_processing(module_lines);

        let mut mp = AsmMacroProcessor::new(pp_macro_depth);
        for import in &use_directives {
            let dep_canonical = canonical_module_id(&import.module_id);
            if let Some(dep_exports) = module_exports.get(&dep_canonical) {
                if is_wildcard_selective(&import.items) {
                    mp.inject_all(dep_exports);
                } else if !import.items.is_empty() {
                    mp.inject_from(dep_exports, &import.items);
                } else {
                    mp.inject_qualified(dep_exports, &import.module_id);
                    if let Some(alias) = import.alias.as_deref() {
                        mp.inject_qualified(dep_exports, alias);
                    }
                }
            }
        }

        let expanded = expand_with_processor(&mut mp, module_lines)?;
        module_exports.insert(canonical, mp.take_native_exports());
        expanded_deps.push((
            module_id.clone(),
            ModuleSource {
                lines: expanded,
                ..source.clone()
            },
        ));
    }

    let mut combined = Vec::new();
    let mut origins = Vec::new();
    let root_file = stable_path_string(root_path);
    for (idx, line) in prefix.into_iter().enumerate() {
        combined.push(line);
        origins.push(SourceOrigin::new(Some(root_file.clone()), idx as u32 + 1));
    }
    for (module_id, source) in expanded_deps {
        let file_name = stable_path_string(&source.path);
        // Give file-derived modules ordinary scope in the combined stream;
        // actual source lines keep their original physical origins.
        let implicit = scan_module_ids_from_processing(&source.lines).is_empty();
        if implicit {
            combined.push(format!(".module {module_id}"));
            origins.push(SourceOrigin::new(
                Some(file_name.clone()),
                source.first_line,
            ));
        }
        let line_count = source.lines.len() as u32;
        for (idx, line) in source.lines.into_iter().enumerate() {
            combined.push(line);
            origins.push(SourceOrigin::new(
                Some(file_name.clone()),
                source.first_line + idx as u32,
            ));
        }
        if implicit {
            combined.push(".endmodule".to_owned());
            origins.push(SourceOrigin::new(
                Some(file_name),
                source.first_line + line_count.saturating_sub(1),
            ));
        }
    }
    for (idx, line) in suffix.into_iter().enumerate() {
        combined.push(line);
        origins.push(SourceOrigin::new(
            Some(root_file.clone()),
            suffix_line + idx as u32,
        ));
    }

    let module_macro_names: HashMap<String, HashMap<String, SymbolVisibility>> = module_exports
        .into_iter()
        .map(|(id, exports)| {
            let visibility_index = exports
                .visibility_index()
                .into_iter()
                .map(|(name, visibility)| {
                    let visibility = match visibility {
                        CompileTimeVisibility::Public => SymbolVisibility::Public,
                        CompileTimeVisibility::Private => SymbolVisibility::Private,
                    };
                    (name, visibility)
                })
                .collect();
            (id, visibility_index)
        })
        .collect();

    Ok(ModuleGraphResult {
        lines: combined,
        source_map: SourceMap::new(origins),
        dependency_files: {
            let mut files: Vec<PathBuf> = dependency_files.into_iter().collect();
            files.sort();
            files
        },
        module_macro_names,
    })
}
