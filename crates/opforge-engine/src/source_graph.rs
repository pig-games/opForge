// SPDX-License-Identifier: GPL-3.0-or-later

use std::collections::{HashMap, HashSet};
use std::io;
use std::path::{Path, PathBuf};

use asm::error::{AsmError, AsmErrorKind, AsmRunError, Diagnostic, Severity};
use asm::preprocess::{AsmMacroExports, AsmMacroProcessor};
use opcore::macro_processor::CompileTimeVisibility;
use opcore::modules::{expr_to_ident, extract_module_block, UseDirectiveSpec};
use opcore::parser::LineAst;
use opcore::services::process_module_item as process_stable_module_item;
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

struct ModuleLoadContext<'a> {
    index: &'a ModuleIndex,
    loaded: &'a mut HashSet<String>,
    preloaded: &'a HashSet<String>,
    order: &'a mut Vec<(String, PathBuf, Vec<String>)>,
    stack: &'a mut Vec<String>,
    defines: &'a [String],
    include_roots: &'a [PathBuf],
    dependency_files: &'a mut HashSet<PathBuf>,
    pp_macro_depth: usize,
    source_provider: &'a dyn SourceProvider,
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

pub(crate) fn scan_module_ids_from_processing(lines: &[String]) -> Vec<String> {
    let mut modules = Vec::new();
    for (idx, line) in lines.iter().enumerate() {
        let ProcessingOutcome::Done(LineAst::Statement(statement)) =
            process_stable_module_item(line, idx as u32 + 1)
        else {
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
                modules.push(name);
            }
        }
    }
    modules
}

fn collect_use_directives_from_processing(lines: &[String]) -> Vec<String> {
    let mut uses = Vec::new();
    for (idx, line) in lines.iter().enumerate() {
        let ProcessingOutcome::Done(LineAst::Use(use_ast)) =
            process_stable_module_item(line, idx as u32 + 1)
        else {
            continue;
        };
        uses.push(use_ast.module_id);
    }
    uses
}

fn collect_use_directives_with_items_from_processing(lines: &[String]) -> Vec<UseDirectiveSpec> {
    let mut uses = Vec::new();
    for (idx, line) in lines.iter().enumerate() {
        let ProcessingOutcome::Done(LineAst::Use(use_ast)) =
            process_stable_module_item(line, idx as u32 + 1)
        else {
            continue;
        };
        uses.push(UseDirectiveSpec {
            module_id: use_ast.module_id,
            alias: use_ast.alias,
            items: use_ast.items.into_iter().map(|item| item.name).collect(),
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
            let contents = source_provider.read_string(&path).map_err(|err| {
                AsmRunError::new(
                    AsmError::new(AsmErrorKind::Io, "Error reading module source", None),
                    vec![],
                    vec![err.to_string()],
                )
            })?;
            let lines: Vec<String> = contents.lines().map(|s| s.to_string()).collect();
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
    module_id: &str,
    ctx: &mut ModuleLoadContext<'_>,
) -> Result<(), AsmRunError> {
    let canonical = canonical_module_id(module_id);
    if ctx.loaded.contains(&canonical) || ctx.preloaded.contains(&canonical) {
        return Ok(());
    }
    let infos = ctx.index.modules.get(&canonical).ok_or_else(|| {
        let mut message = format!("Missing module: {module_id}");
        if !ctx.stack.is_empty() {
            let chain = ctx.stack.join(" -> ");
            message.push_str(&format!(" (import stack: {chain})"));
        }
        AsmRunError::new(
            AsmError::new(AsmErrorKind::Directive, &message, None),
            vec![],
            vec![],
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

    ctx.stack.push(module_id.to_string());
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
        source_lines
    };

    for dep in collect_use_directives_from_processing(&module_lines) {
        load_module_recursive(&dep, ctx)?;
    }

    ctx.loaded.insert(canonical);
    ctx.order
        .push((module_id.to_string(), info.path.clone(), module_lines));
    ctx.stack.pop();
    Ok(())
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
    let index = build_module_index(&search_roots, source_provider)?;

    let mut preloaded = HashSet::new();
    let mut explicit_modules = scan_module_ids_from_processing(&root_lines);
    if explicit_modules.is_empty() {
        explicit_modules.push(module_id_from_path(root_path)?);
    }
    for module_id in explicit_modules {
        preloaded.insert(canonical_module_id(&module_id));
    }

    let mut loaded = HashSet::new();
    let mut order: Vec<(String, PathBuf, Vec<String>)> = Vec::new();
    let mut stack = Vec::new();
    let mut dependency_files = HashSet::new();
    let mut ctx = ModuleLoadContext {
        index: &index,
        loaded: &mut loaded,
        preloaded: &preloaded,
        order: &mut order,
        stack: &mut stack,
        defines,
        include_roots,
        dependency_files: &mut dependency_files,
        pp_macro_depth,
        source_provider,
    };
    for dep in collect_use_directives_from_processing(&root_lines) {
        load_module_recursive(&dep, &mut ctx)?;
    }

    let mut module_exports: HashMap<String, AsmMacroExports> = HashMap::new();
    let mut expanded_deps: Vec<(PathBuf, Vec<String>)> = Vec::new();

    for (module_id, module_path, module_lines) in &order {
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
        expanded_deps.push((module_path.clone(), expanded));
    }

    let root_uses = collect_use_directives_with_items_from_processing(&root_lines);
    let mut mp = AsmMacroProcessor::new(pp_macro_depth);
    for import in &root_uses {
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

    let expanded_root = expand_with_processor(&mut mp, &root_lines)?;

    let mut combined = Vec::new();
    let mut origins = Vec::new();
    for (module_path, dep_lines) in expanded_deps {
        let file_name = module_path.to_string_lossy().to_string();
        for (idx, line) in dep_lines.iter().enumerate() {
            combined.push(line.clone());
            origins.push(SourceOrigin::new(Some(file_name.clone()), idx as u32 + 1));
        }
    }
    let root_file = root_path.to_string_lossy().to_string();
    for (idx, line) in expanded_root.iter().enumerate() {
        combined.push(line.clone());
        origins.push(SourceOrigin::new(Some(root_file.clone()), idx as u32 + 1));
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
