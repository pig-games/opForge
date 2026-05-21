// SPDX-License-Identifier: GPL-3.0-or-later

//! Symbol model and symbol-table types shared across libopforge crates.

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::io::{self, Write};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolVisibility {
    Public,
    Private,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Label,
    Variable,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub val: u32,
    pub rw: bool,
    pub updated: bool,
    pub visibility: SymbolVisibility,
    pub module_id: Option<String>,
}

impl Symbol {
    #[must_use]
    pub fn kind(&self) -> SymbolKind {
        if self.rw {
            SymbolKind::Variable
        } else {
            SymbolKind::Label
        }
    }
}

/// Transitional alias for existing internal naming.
pub type SymbolTableEntry = Symbol;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceSpan {
    pub line: u32,
    pub col_start: usize,
    pub col_end: usize,
}

#[derive(Debug, Clone)]
pub struct ImportItem {
    pub name: String,
    pub alias: Option<String>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct ImportParam {
    pub name: String,
    pub value_repr: String,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct ImportSectionMap {
    pub logical: String,
    pub concrete: String,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalSectionKind {
    Code,
    Data,
    Bss,
}

#[derive(Debug, Clone)]
pub struct LogicalSectionContract {
    pub name: String,
    pub kind: LogicalSectionKind,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct ModuleImport {
    pub module_id: String,
    pub alias: Option<String>,
    pub qualifier: Option<String>,
    pub items: Vec<ImportItem>,
    pub selected_roots: Vec<ImportItem>,
    pub params: Vec<ImportParam>,
    pub section_maps: Vec<ImportSectionMap>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub name: String,
    pub imports: Vec<ModuleImport>,
    pub logical_sections: Vec<LogicalSectionContract>,
    pub symbol_references: HashMap<String, HashSet<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReachableUnit {
    pub importing_module: String,
    pub module_id: String,
    pub symbol_name: String,
    pub full_name: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportIssueKind {
    Directive,
    Symbol,
}

#[derive(Debug, Clone)]
pub struct ImportIssue {
    pub line: u32,
    pub column: Option<usize>,
    pub kind: ImportIssueKind,
    pub message: String,
    pub param: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportResult {
    Ok,
    AliasCollision,
    SelectiveCollision,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportedSymbolResolution {
    Resolved {
        module_id: String,
        symbol_name: String,
        full_name: String,
    },
    Unresolved,
    Ambiguous,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[must_use]
pub enum SymbolTableResult {
    Ok,
    Duplicate,
    NotFound,
    TableFull,
}

pub const MAX_ENTRIES: usize = 66000;

fn normalized_ascii_upper_lookup_key(name: &str) -> Cow<'_, str> {
    if name.bytes().any(|byte| byte.is_ascii_lowercase()) {
        Cow::Owned(name.to_ascii_uppercase())
    } else {
        Cow::Borrowed(name)
    }
}

#[derive(Debug, Default)]
pub struct SymbolTable {
    entries: Vec<SymbolTableEntry>,
    index: HashMap<String, usize>,
    module_info: Vec<ModuleInfo>,
    module_index: HashMap<String, usize>,
}

impl SymbolTable {
    #[must_use]
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            index: HashMap::new(),
            module_info: Vec::new(),
            module_index: HashMap::new(),
        }
    }

    pub fn register_module(&mut self, name: &str) -> SymbolTableResult {
        let key = normalized_ascii_upper_lookup_key(name);
        if self.module_index.contains_key(key.as_ref()) {
            return SymbolTableResult::Duplicate;
        }
        let key = key.into_owned();
        let idx = self.module_info.len();
        self.module_info.push(ModuleInfo {
            name: name.to_string(),
            imports: Vec::new(),
            logical_sections: Vec::new(),
            symbol_references: HashMap::new(),
        });
        self.module_index.insert(key, idx);
        SymbolTableResult::Ok
    }

    pub fn add_logical_section(
        &mut self,
        module: &str,
        name: String,
        kind: LogicalSectionKind,
        span: SourceSpan,
    ) {
        if self.module_info_mut(module).is_none() {
            let _ = self.register_module(module);
        }
        let info = self.module_info_mut(module).expect("module info");
        if let Some(existing) = info
            .logical_sections
            .iter_mut()
            .find(|section| section.name.eq_ignore_ascii_case(&name))
        {
            existing.kind = kind;
            existing.span = span;
            return;
        }
        info.logical_sections
            .push(LogicalSectionContract { name, kind, span });
    }

    #[must_use]
    pub fn has_module(&self, name: &str) -> bool {
        let key = normalized_ascii_upper_lookup_key(name);
        self.module_index.contains_key(key.as_ref())
    }

    pub fn add_import(&mut self, module: &str, import: ModuleImport) -> ImportResult {
        if self.module_info_mut(module).is_none() {
            let _ = self.register_module(module);
        }
        let info = self.module_info_mut(module).expect("module info");

        if let Some(qualifier) = &import.qualifier {
            if info
                .imports
                .iter()
                .filter_map(|existing| existing.qualifier.as_ref())
                .any(|existing| existing.eq_ignore_ascii_case(qualifier))
            {
                return ImportResult::AliasCollision;
            }
        }

        for item in &import.items {
            if item.name == "*" && item.alias.is_none() {
                continue;
            }
            let local = item.alias.as_deref().unwrap_or(&item.name);
            if info
                .imports
                .iter()
                .flat_map(|existing| existing.items.iter())
                .any(|existing| {
                    if existing.name == "*" && existing.alias.is_none() {
                        return false;
                    }
                    let existing_local = existing.alias.as_deref().unwrap_or(&existing.name);
                    existing_local.eq_ignore_ascii_case(local)
                })
            {
                return ImportResult::SelectiveCollision;
            }
        }

        info.imports.push(import);
        ImportResult::Ok
    }

    #[must_use]
    pub fn module_imports(&self, name: &str) -> Option<&[ModuleImport]> {
        self.module_info(name).map(|info| info.imports.as_slice())
    }

    #[must_use]
    pub fn modules(&self) -> &[ModuleInfo] {
        &self.module_info
    }

    pub fn record_symbol_reference(&mut self, source_full_name: &str, target_full_name: &str) {
        if source_full_name.eq_ignore_ascii_case(target_full_name) {
            return;
        }
        let module_name = self
            .entry(source_full_name)
            .and_then(|entry| entry.module_id.clone())
            .or_else(|| {
                self.module_symbol_for_full_name(source_full_name)
                    .map(|(module, _)| module)
            });
        let Some(module_name) = module_name else {
            return;
        };
        let Some(info) = self.module_info_mut(&module_name) else {
            return;
        };
        info.symbol_references
            .entry(source_full_name.to_string())
            .or_default()
            .insert(target_full_name.to_string());
    }

    #[must_use]
    pub fn reachable_units_from_selected_roots(&self) -> Vec<ReachableUnit> {
        let mut reachable = HashMap::new();
        let mut seen = HashSet::new();
        let mut queue = Vec::new();
        for module in &self.module_info {
            for import in &module.imports {
                for root in &import.selected_roots {
                    if root.name == "*" && root.alias.is_none() {
                        continue;
                    }
                    let full_name = format!("{}.{}", import.module_id, root.name);
                    queue.push(ReachableUnit {
                        importing_module: module.name.clone(),
                        module_id: import.module_id.clone(),
                        symbol_name: root.name.clone(),
                        full_name,
                    });
                }
            }
        }
        let imported_modules: HashSet<_> = self
            .module_info
            .iter()
            .flat_map(|module| module.imports.iter())
            .map(|import| normalized_ascii_upper_lookup_key(&import.module_id).into_owned())
            .collect();
        for module in &self.module_info {
            if imported_modules.contains(normalized_ascii_upper_lookup_key(&module.name).as_ref()) {
                continue;
            }
            for reference in module
                .symbol_references
                .values()
                .flat_map(|references| references.iter())
            {
                if let Some((module_id, symbol_name)) = self.module_symbol_for_full_name(reference)
                {
                    queue.push(ReachableUnit {
                        importing_module: module.name.clone(),
                        module_id,
                        symbol_name,
                        full_name: reference.clone(),
                    });
                }
            }
        }
        queue.sort_by(|left, right| {
            normalized_ascii_upper_lookup_key(&left.full_name)
                .cmp(&normalized_ascii_upper_lookup_key(&right.full_name))
        });
        while let Some(unit) = queue.pop() {
            let key = normalized_ascii_upper_lookup_key(&unit.full_name).into_owned();
            if !seen.insert(key.clone()) {
                continue;
            }
            if let Some(info) = self.module_info(&unit.module_id) {
                if let Some(references) = info.symbol_references.get(&unit.full_name) {
                    let mut references: Vec<_> = references.iter().cloned().collect();
                    references
                        .sort_by_key(|name| normalized_ascii_upper_lookup_key(name).into_owned());
                    for reference in references.into_iter().rev() {
                        if let Some((module_id, symbol_name)) =
                            self.module_symbol_for_full_name(&reference)
                        {
                            let importing_module =
                                if module_id.eq_ignore_ascii_case(&unit.module_id) {
                                    unit.importing_module.clone()
                                } else {
                                    unit.module_id.clone()
                                };
                            queue.push(ReachableUnit {
                                importing_module,
                                module_id,
                                symbol_name,
                                full_name: reference,
                            });
                        }
                    }
                }
            }
            reachable.insert(key, unit);
        }
        let mut reachable: Vec<_> = reachable.into_values().collect();
        reachable.sort_by(|left, right| {
            normalized_ascii_upper_lookup_key(&left.full_name)
                .cmp(&normalized_ascii_upper_lookup_key(&right.full_name))
        });
        reachable
    }

    fn module_symbol_for_full_name(&self, full_name: &str) -> Option<(String, String)> {
        self.module_info
            .iter()
            .filter_map(|module| {
                full_name
                    .strip_prefix(&format!("{}.", module.name))
                    .map(|symbol| (module.name.clone(), symbol.to_string()))
            })
            .max_by_key(|(module, _)| module.len())
    }

    fn module_info(&self, name: &str) -> Option<&ModuleInfo> {
        let key = normalized_ascii_upper_lookup_key(name);
        self.module_index
            .get(key.as_ref())
            .map(|&idx| &self.module_info[idx])
    }

    fn module_info_mut(&mut self, name: &str) -> Option<&mut ModuleInfo> {
        let key = normalized_ascii_upper_lookup_key(name);
        self.module_index
            .get(key.as_ref())
            .copied()
            .map(|idx| &mut self.module_info[idx])
    }

    #[must_use]
    pub fn resolve_import_alias(&self, module: &str, alias: &str) -> Option<&str> {
        self.module_info(module).and_then(|info| {
            info.imports.iter().find_map(|import| {
                import.qualifier.as_ref().and_then(|candidate| {
                    if candidate.eq_ignore_ascii_case(alias) {
                        Some(import.module_id.as_str())
                    } else {
                        None
                    }
                })
            })
        })
    }

    #[must_use]
    pub fn resolve_selective_import(&self, module: &str, name: &str) -> Option<(&str, &str)> {
        self.module_info(module).and_then(|info| {
            info.imports.iter().find_map(|import| {
                import.items.iter().find_map(|item| {
                    if item.name == "*" && item.alias.is_none() {
                        let full_name = format!("{}.{}", import.module_id, name);
                        if let Some(entry) = self.entry(&full_name) {
                            if entry.visibility == SymbolVisibility::Public {
                                let target_name =
                                    entry.name.rsplit('.').next().unwrap_or(&entry.name);
                                return Some((import.module_id.as_str(), target_name));
                            }
                        }
                        return None;
                    }
                    let local = item.alias.as_deref().unwrap_or(&item.name);
                    if local.eq_ignore_ascii_case(name) {
                        Some((import.module_id.as_str(), item.name.as_str()))
                    } else {
                        None
                    }
                })
            })
        })
    }

    #[must_use]
    pub fn resolve_imported_symbol(&self, module: &str, name: &str) -> ImportedSymbolResolution {
        let Some(info) = self.module_info(module) else {
            return ImportedSymbolResolution::Unresolved;
        };

        if !name.contains('.') {
            if let Some((target_module, target_name)) = self.resolve_selective_import(module, name)
            {
                return self.resolved_import_name(target_module, target_name);
            }
            return ImportedSymbolResolution::Unresolved;
        }

        if let Some((prefix, rest)) = name.split_once('.') {
            if let Some(import) = info.imports.iter().find(|import| {
                import
                    .qualifier
                    .as_ref()
                    .is_some_and(|qualifier| qualifier.eq_ignore_ascii_case(prefix))
            }) {
                return self.resolved_import_name(import.module_id.as_str(), rest);
            }
        }

        let mut matches = info.imports.iter().filter_map(|import| {
            let module_id = import.module_id.as_str();
            let separator_index = module_id.len();
            if name.len() <= separator_index || !name.is_char_boundary(separator_index) {
                return None;
            }
            let (candidate_module, rest_with_dot) = name.split_at(separator_index);
            if !candidate_module.eq_ignore_ascii_case(module_id) || !rest_with_dot.starts_with('.')
            {
                return None;
            }
            let target_name = &rest_with_dot[1..];
            if target_name.is_empty() {
                return None;
            }
            Some(self.resolved_import_name(module_id, target_name))
        });

        let Some(first) = matches.next() else {
            return ImportedSymbolResolution::Unresolved;
        };
        if matches.next().is_some() {
            return ImportedSymbolResolution::Ambiguous;
        }
        first
    }

    fn resolved_import_name(&self, module_id: &str, symbol_name: &str) -> ImportedSymbolResolution {
        let full_name = format!("{module_id}.{symbol_name}");
        let full_name = self
            .entry(&full_name)
            .map(|entry| entry.name.clone())
            .unwrap_or(full_name);
        ImportedSymbolResolution::Resolved {
            module_id: module_id.to_string(),
            symbol_name: symbol_name.to_string(),
            full_name,
        }
    }

    #[must_use]
    pub fn validate_imports(
        &self,
        known_compile_time_symbols: &HashMap<String, HashMap<String, SymbolVisibility>>,
    ) -> Vec<ImportIssue> {
        let mut issues = Vec::new();

        for info in &self.module_info {
            for import in &info.imports {
                if !self.has_module(&import.module_id) {
                    issues.push(ImportIssue {
                        line: import.span.line,
                        column: Some(import.span.col_start),
                        kind: ImportIssueKind::Directive,
                        message: "Missing module".to_string(),
                        param: Some(import.module_id.clone()),
                    });
                    continue;
                }
                let dep_info = self.module_info(&import.module_id);
                let mut checked_maps = HashSet::new();
                for map in &import.section_maps {
                    let map_key = normalized_ascii_upper_lookup_key(&map.logical).into_owned();
                    if !checked_maps.insert(map_key) {
                        issues.push(ImportIssue {
                            line: map.span.line,
                            column: Some(map.span.col_start),
                            kind: ImportIssueKind::Directive,
                            message: "Duplicate import section map entry".to_string(),
                            param: Some(map.logical.clone()),
                        });
                        continue;
                    }
                    let Some(dep_info) = dep_info else {
                        continue;
                    };
                    if !dep_info
                        .logical_sections
                        .iter()
                        .any(|section| section.name.eq_ignore_ascii_case(&map.logical))
                    {
                        issues.push(ImportIssue {
                            line: map.span.line,
                            column: Some(map.span.col_start),
                            kind: ImportIssueKind::Directive,
                            message: format!(
                                "Unknown logical section '{}' in module '{}'",
                                map.logical, import.module_id
                            ),
                            param: Some(map.logical.clone()),
                        });
                    }
                }
                let mut checked_import_items = HashSet::new();
                for item in import.items.iter().chain(import.selected_roots.iter()) {
                    if item.name == "*" && item.alias.is_none() {
                        continue;
                    }
                    if !checked_import_items
                        .insert(normalized_ascii_upper_lookup_key(&item.name).into_owned())
                    {
                        continue;
                    }
                    let full_name = format!("{}.{}", import.module_id, item.name);
                    match self.entry(&full_name) {
                        Some(entry) => {
                            if entry.visibility == SymbolVisibility::Private {
                                issues.push(ImportIssue {
                                    line: item.span.line,
                                    column: Some(item.span.col_start),
                                    kind: ImportIssueKind::Symbol,
                                    message: "Symbol is private".to_string(),
                                    param: Some(item.name.clone()),
                                });
                            }
                        }
                        None => {
                            let dep_canonical = import.module_id.to_ascii_lowercase();
                            if let Some(symbols) = known_compile_time_symbols.get(&dep_canonical) {
                                if let Some(visibility) = symbols
                                    .get(normalized_ascii_upper_lookup_key(&item.name).as_ref())
                                {
                                    if *visibility == SymbolVisibility::Private {
                                        issues.push(ImportIssue {
                                            line: item.span.line,
                                            column: Some(item.span.col_start),
                                            kind: ImportIssueKind::Symbol,
                                            message: "Symbol is private".to_string(),
                                            param: Some(item.name.clone()),
                                        });
                                    }
                                    continue;
                                }
                            }
                            issues.push(ImportIssue {
                                line: item.span.line,
                                column: Some(item.span.col_start),
                                kind: ImportIssueKind::Directive,
                                message: "Missing imported symbol".to_string(),
                                param: Some(item.name.clone()),
                            });
                        }
                    }
                }
            }
        }

        let mut visiting = Vec::new();
        let mut visited = HashSet::new();
        for module in &self.module_info {
            self.detect_import_cycles(
                module.name.as_str(),
                &mut visiting,
                &mut visited,
                &mut issues,
            );
        }

        issues
    }

    fn detect_import_cycles(
        &self,
        module: &str,
        visiting: &mut Vec<String>,
        visited: &mut HashSet<String>,
        issues: &mut Vec<ImportIssue>,
    ) {
        let module_upper = normalized_ascii_upper_lookup_key(module);
        if visited.contains(module_upper.as_ref()) {
            return;
        }
        if let Some(pos) = visiting
            .iter()
            .position(|name| name.eq_ignore_ascii_case(module))
        {
            let cycle = &visiting[pos..];
            for window in cycle.windows(2) {
                let from = &window[0];
                let to = &window[1];
                self.push_cycle_issue(from, to, issues);
            }
            if let (Some(first), Some(last)) = (cycle.first(), cycle.last()) {
                self.push_cycle_issue(last, first, issues);
            }
            return;
        }

        visiting.push(module.to_string());
        if let Some(info) = self.module_info(module) {
            for import in &info.imports {
                self.detect_import_cycles(&import.module_id, visiting, visited, issues);
            }
        }
        visiting.pop();
        visited.insert(module_upper.into_owned());
    }

    fn push_cycle_issue(&self, from: &str, to: &str, issues: &mut Vec<ImportIssue>) {
        if let Some(info) = self.module_info(from) {
            for import in &info.imports {
                if import.module_id.eq_ignore_ascii_case(to) {
                    issues.push(ImportIssue {
                        line: import.span.line,
                        column: Some(import.span.col_start),
                        kind: ImportIssueKind::Directive,
                        message: "Import cycle detected".to_string(),
                        param: Some(import.module_id.clone()),
                    });
                }
            }
        }
    }

    pub fn add(
        &mut self,
        name: &str,
        val: u32,
        rw: bool,
        visibility: SymbolVisibility,
        module_id: Option<&str>,
    ) -> SymbolTableResult {
        if self.entries.len() >= MAX_ENTRIES {
            return SymbolTableResult::TableFull;
        }

        let key = normalized_ascii_upper_lookup_key(name);
        if let Some(&idx) = self.index.get(key.as_ref()) {
            let entry = &mut self.entries[idx];
            if entry.rw {
                entry.val = val;
                return SymbolTableResult::Ok;
            }
            return SymbolTableResult::Duplicate;
        }
        let key = key.into_owned();

        let idx = self.entries.len();
        self.entries.push(SymbolTableEntry {
            name: name.to_string(),
            val,
            rw,
            updated: false,
            visibility,
            module_id: module_id.map(str::to_string),
        });
        self.index.insert(key, idx);
        SymbolTableResult::Ok
    }

    pub fn update(&mut self, name: &str, val: u32) -> SymbolTableResult {
        if let Some(entry) = self.entry_mut(name) {
            if entry.rw || !entry.updated {
                entry.val = val;
                entry.updated = true;
                return SymbolTableResult::Ok;
            }
            return SymbolTableResult::Duplicate;
        }
        SymbolTableResult::NotFound
    }

    #[must_use]
    pub fn lookup(&self, name: &str) -> Option<u32> {
        self.entry(name).map(|e| e.val)
    }

    #[must_use]
    pub fn entry(&self, name: &str) -> Option<&SymbolTableEntry> {
        let key = normalized_ascii_upper_lookup_key(name);
        self.index.get(key.as_ref()).map(|&idx| &self.entries[idx])
    }

    pub fn entry_mut(&mut self, name: &str) -> Option<&mut SymbolTableEntry> {
        let key = normalized_ascii_upper_lookup_key(name);
        self.index
            .get(key.as_ref())
            .copied()
            .map(|idx| &mut self.entries[idx])
    }

    #[must_use]
    pub fn entries(&self) -> &[SymbolTableEntry] {
        &self.entries
    }

    pub fn dump<W: Write>(&self, mut out: W) -> io::Result<()> {
        for entry in &self.entries {
            writeln!(out, "{:<16}: {:04x} ({})", entry.name, entry.val, entry.val)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::{
        ImportItem, ImportResult, ImportedSymbolResolution, ModuleImport, SourceSpan, SymbolTable,
        SymbolTableResult, SymbolVisibility,
    };

    #[test]
    fn add_and_lookup_are_case_insensitive() {
        let mut table = SymbolTable::new();
        assert_eq!(
            table.add("Foo", 0x10, false, SymbolVisibility::Private, None),
            SymbolTableResult::Ok
        );
        assert_eq!(table.lookup("foo"), Some(0x10));
        assert_eq!(table.lookup("FOO"), Some(0x10));
    }

    #[test]
    fn add_import_detects_alias_collision() {
        let mut table = SymbolTable::new();
        assert_eq!(table.register_module("alpha"), SymbolTableResult::Ok);
        let span = SourceSpan {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        let import = ModuleImport {
            module_id: "beta".to_string(),
            alias: Some("M".to_string()),
            qualifier: Some("M".to_string()),
            items: Vec::new(),
            selected_roots: Vec::new(),
            params: Vec::new(),
            section_maps: Vec::new(),
            span,
        };
        assert_eq!(table.add_import("alpha", import), ImportResult::Ok);

        let import = ModuleImport {
            module_id: "gamma".to_string(),
            alias: Some("m".to_string()),
            qualifier: Some("m".to_string()),
            items: Vec::new(),
            selected_roots: Vec::new(),
            params: Vec::new(),
            section_maps: Vec::new(),
            span,
        };
        assert_eq!(
            table.add_import("alpha", import),
            ImportResult::AliasCollision
        );
    }

    #[test]
    fn add_import_detects_implicit_qualifier_collision() {
        let mut table = SymbolTable::new();
        assert_eq!(table.register_module("alpha"), SymbolTableResult::Ok);
        let span = SourceSpan {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        let import = ModuleImport {
            module_id: "opasm.amigaos.engine".to_string(),
            alias: None,
            qualifier: Some("engine".to_string()),
            items: Vec::new(),
            selected_roots: Vec::new(),
            params: Vec::new(),
            section_maps: Vec::new(),
            span,
        };
        assert_eq!(table.add_import("alpha", import), ImportResult::Ok);

        let import = ModuleImport {
            module_id: "example.engine".to_string(),
            alias: None,
            qualifier: Some("ENGINE".to_string()),
            items: Vec::new(),
            selected_roots: Vec::new(),
            params: Vec::new(),
            section_maps: Vec::new(),
            span,
        };
        assert_eq!(
            table.add_import("alpha", import),
            ImportResult::AliasCollision
        );
    }

    #[test]
    fn add_import_detects_explicit_implicit_qualifier_collision() {
        let mut table = SymbolTable::new();
        assert_eq!(table.register_module("alpha"), SymbolTableResult::Ok);
        let span = SourceSpan {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        let import = ModuleImport {
            module_id: "opasm.amigaos.engine".to_string(),
            alias: None,
            qualifier: Some("engine".to_string()),
            items: Vec::new(),
            selected_roots: Vec::new(),
            params: Vec::new(),
            section_maps: Vec::new(),
            span,
        };
        assert_eq!(table.add_import("alpha", import), ImportResult::Ok);

        let import = ModuleImport {
            module_id: "example.runtime".to_string(),
            alias: Some("Engine".to_string()),
            qualifier: Some("Engine".to_string()),
            items: Vec::new(),
            selected_roots: Vec::new(),
            params: Vec::new(),
            section_maps: Vec::new(),
            span,
        };
        assert_eq!(
            table.add_import("alpha", import),
            ImportResult::AliasCollision
        );
    }

    #[test]
    fn direct_selective_import_does_not_claim_implicit_qualifier() {
        let mut table = SymbolTable::new();
        assert_eq!(table.register_module("alpha"), SymbolTableResult::Ok);
        let span = SourceSpan {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        let import = ModuleImport {
            module_id: "opasm.amigaos.engine".to_string(),
            alias: None,
            qualifier: None,
            items: vec![ImportItem {
                name: "sessionPass".to_string(),
                alias: None,
                span,
            }],
            selected_roots: vec![ImportItem {
                name: "sessionPass".to_string(),
                alias: None,
                span,
            }],
            params: Vec::new(),
            section_maps: Vec::new(),
            span,
        };
        assert_eq!(table.add_import("alpha", import), ImportResult::Ok);

        let import = ModuleImport {
            module_id: "example.engine".to_string(),
            alias: None,
            qualifier: Some("engine".to_string()),
            items: Vec::new(),
            selected_roots: Vec::new(),
            params: Vec::new(),
            section_maps: Vec::new(),
            span,
        };
        assert_eq!(table.add_import("alpha", import), ImportResult::Ok);
    }

    #[test]
    fn validate_imports_deduplicates_direct_items_that_are_selected_roots() {
        let mut table = SymbolTable::new();
        assert_eq!(table.register_module("alpha"), SymbolTableResult::Ok);
        assert_eq!(
            table.register_module("opasm.amigaos.engine"),
            SymbolTableResult::Ok
        );
        let span = SourceSpan {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        let item = ImportItem {
            name: "sessionPass".to_string(),
            alias: None,
            span,
        };
        let import = ModuleImport {
            module_id: "opasm.amigaos.engine".to_string(),
            alias: None,
            qualifier: None,
            items: vec![item.clone()],
            selected_roots: vec![item],
            params: Vec::new(),
            section_maps: Vec::new(),
            span,
        };
        assert_eq!(table.add_import("alpha", import), ImportResult::Ok);

        let issues = table.validate_imports(&HashMap::new());

        assert_eq!(issues.len(), 1);
        assert_eq!(issues[0].message, "Missing imported symbol");
        assert_eq!(issues[0].param.as_deref(), Some("sessionPass"));
    }

    #[test]
    fn reachable_units_start_from_selected_roots() {
        let mut table = SymbolTable::new();
        assert_eq!(table.register_module("alpha"), SymbolTableResult::Ok);
        let span = SourceSpan {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        let import = ModuleImport {
            module_id: "dep".to_string(),
            alias: Some("d".to_string()),
            qualifier: Some("d".to_string()),
            items: Vec::new(),
            selected_roots: vec![ImportItem {
                name: "entry".to_string(),
                alias: None,
                span,
            }],
            params: Vec::new(),
            section_maps: Vec::new(),
            span,
        };
        assert_eq!(table.add_import("alpha", import), ImportResult::Ok);

        let reachable = table.reachable_units_from_selected_roots();

        assert_eq!(reachable.len(), 1);
        assert_eq!(reachable[0].importing_module, "alpha");
        assert_eq!(reachable[0].module_id, "dep");
        assert_eq!(reachable[0].symbol_name, "entry");
        assert_eq!(reachable[0].full_name, "dep.entry");
    }

    #[test]
    fn reachable_units_include_recorded_dependencies_and_cycles_once() {
        let mut table = SymbolTable::new();
        assert_eq!(table.register_module("alpha"), SymbolTableResult::Ok);
        assert_eq!(table.register_module("dep"), SymbolTableResult::Ok);
        assert_eq!(table.register_module("util"), SymbolTableResult::Ok);
        let span = SourceSpan {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        assert_eq!(
            table.add_import(
                "alpha",
                ModuleImport {
                    module_id: "dep".to_string(),
                    alias: Some("d".to_string()),
                    qualifier: Some("d".to_string()),
                    items: Vec::new(),
                    selected_roots: vec![ImportItem {
                        name: "entry".to_string(),
                        alias: None,
                        span,
                    }],
                    params: Vec::new(),
                    section_maps: Vec::new(),
                    span,
                },
            ),
            ImportResult::Ok
        );
        table.record_symbol_reference("dep.entry", "util.helper");
        table.record_symbol_reference("util.helper", "dep.entry");

        let reachable = table.reachable_units_from_selected_roots();
        let names: Vec<_> = reachable
            .iter()
            .map(|unit| unit.full_name.as_str())
            .collect();

        assert_eq!(names, vec!["dep.entry", "util.helper"]);
    }

    #[test]
    fn reachable_units_start_from_root_module_references() {
        let mut table = SymbolTable::new();
        assert_eq!(table.register_module("main"), SymbolTableResult::Ok);
        assert_eq!(table.register_module("dep"), SymbolTableResult::Ok);
        let span = SourceSpan {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        assert_eq!(
            table.add_import(
                "main",
                ModuleImport {
                    module_id: "dep".to_string(),
                    alias: Some("d".to_string()),
                    qualifier: Some("d".to_string()),
                    items: Vec::new(),
                    selected_roots: Vec::new(),
                    params: Vec::new(),
                    section_maps: Vec::new(),
                    span,
                },
            ),
            ImportResult::Ok
        );
        table.record_symbol_reference("main.entry", "dep.entry");

        let reachable = table.reachable_units_from_selected_roots();

        assert_eq!(reachable.len(), 1);
        assert_eq!(reachable[0].full_name, "dep.entry");
    }

    #[test]
    fn reachable_units_exclude_unreferenced_public_exports() {
        let mut table = SymbolTable::new();
        assert_eq!(table.register_module("main"), SymbolTableResult::Ok);
        assert_eq!(table.register_module("dep"), SymbolTableResult::Ok);
        let span = SourceSpan {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        assert_eq!(
            table.add_import(
                "main",
                ModuleImport {
                    module_id: "dep".to_string(),
                    alias: Some("d".to_string()),
                    qualifier: Some("d".to_string()),
                    items: Vec::new(),
                    selected_roots: vec![ImportItem {
                        name: "entry".to_string(),
                        alias: None,
                        span,
                    }],
                    params: Vec::new(),
                    section_maps: Vec::new(),
                    span,
                },
            ),
            ImportResult::Ok
        );

        let reachable = table.reachable_units_from_selected_roots();
        let names: Vec<_> = reachable
            .iter()
            .map(|unit| unit.full_name.as_str())
            .collect();

        assert_eq!(names, vec!["dep.entry"]);
    }

    #[test]
    fn resolve_imported_symbol_handles_alias_implicit_full_path_and_selective() {
        let mut table = SymbolTable::new();
        assert_eq!(table.register_module("alpha"), SymbolTableResult::Ok);
        assert_eq!(
            table.register_module("opasm.amigaos.engine"),
            SymbolTableResult::Ok
        );
        assert_eq!(table.register_module("math"), SymbolTableResult::Ok);
        let span = SourceSpan {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        assert_eq!(
            table.add(
                "opasm.amigaos.engine.sessionPass",
                1,
                false,
                SymbolVisibility::Public,
                Some("opasm.amigaos.engine")
            ),
            SymbolTableResult::Ok
        );
        assert_eq!(
            table.add("math.sum", 2, false, SymbolVisibility::Public, Some("math")),
            SymbolTableResult::Ok
        );
        assert_eq!(
            table.add_import(
                "alpha",
                ModuleImport {
                    module_id: "opasm.amigaos.engine".to_string(),
                    alias: None,
                    qualifier: Some("engine".to_string()),
                    items: Vec::new(),
                    selected_roots: Vec::new(),
                    params: Vec::new(),
                    section_maps: Vec::new(),
                    span,
                }
            ),
            ImportResult::Ok
        );
        assert_eq!(
            table.add_import(
                "alpha",
                ModuleImport {
                    module_id: "math".to_string(),
                    alias: Some("m".to_string()),
                    qualifier: Some("m".to_string()),
                    items: vec![ImportItem {
                        name: "sum".to_string(),
                        alias: Some("total".to_string()),
                        span,
                    }],
                    selected_roots: vec![ImportItem {
                        name: "sum".to_string(),
                        alias: Some("total".to_string()),
                        span,
                    }],
                    params: Vec::new(),
                    section_maps: Vec::new(),
                    span,
                }
            ),
            ImportResult::Ok
        );

        assert_eq!(
            table.resolve_imported_symbol("alpha", "engine.sessionPass"),
            ImportedSymbolResolution::Resolved {
                module_id: "opasm.amigaos.engine".to_string(),
                symbol_name: "sessionPass".to_string(),
                full_name: "opasm.amigaos.engine.sessionPass".to_string(),
            }
        );
        assert_eq!(
            table.resolve_imported_symbol("alpha", "opasm.amigaos.engine.sessionPass"),
            ImportedSymbolResolution::Resolved {
                module_id: "opasm.amigaos.engine".to_string(),
                symbol_name: "sessionPass".to_string(),
                full_name: "opasm.amigaos.engine.sessionPass".to_string(),
            }
        );
        assert_eq!(
            table.resolve_imported_symbol("alpha", "m.sum"),
            ImportedSymbolResolution::Resolved {
                module_id: "math".to_string(),
                symbol_name: "sum".to_string(),
                full_name: "math.sum".to_string(),
            }
        );
        assert_eq!(
            table.resolve_imported_symbol("alpha", "total"),
            ImportedSymbolResolution::Resolved {
                module_id: "math".to_string(),
                symbol_name: "sum".to_string(),
                full_name: "math.sum".to_string(),
            }
        );
    }

    #[test]
    fn resolve_imported_symbol_reports_ambiguous_full_module_path() {
        let mut table = SymbolTable::new();
        assert_eq!(table.register_module("alpha"), SymbolTableResult::Ok);
        assert_eq!(table.register_module("pkg.core"), SymbolTableResult::Ok);
        assert_eq!(
            table.register_module("pkg.core.util"),
            SymbolTableResult::Ok
        );
        let span = SourceSpan {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        assert_eq!(
            table.add_import(
                "alpha",
                ModuleImport {
                    module_id: "pkg.core".to_string(),
                    alias: Some("core".to_string()),
                    qualifier: Some("core".to_string()),
                    items: Vec::new(),
                    selected_roots: Vec::new(),
                    params: Vec::new(),
                    section_maps: Vec::new(),
                    span,
                }
            ),
            ImportResult::Ok
        );
        assert_eq!(
            table.add_import(
                "alpha",
                ModuleImport {
                    module_id: "pkg.core.util".to_string(),
                    alias: Some("util".to_string()),
                    qualifier: Some("util".to_string()),
                    items: Vec::new(),
                    selected_roots: Vec::new(),
                    params: Vec::new(),
                    section_maps: Vec::new(),
                    span,
                }
            ),
            ImportResult::Ok
        );

        assert_eq!(
            table.resolve_imported_symbol("alpha", "pkg.core.util.entry"),
            ImportedSymbolResolution::Ambiguous
        );
        assert_eq!(
            table.resolve_imported_symbol("alpha", "pkg.missing.entry"),
            ImportedSymbolResolution::Unresolved
        );
    }

    #[test]
    fn module_imports_lookup_is_case_insensitive() {
        let mut table = SymbolTable::new();
        assert_eq!(table.register_module("Core.Utils"), SymbolTableResult::Ok);
        let span = SourceSpan {
            line: 1,
            col_start: 1,
            col_end: 1,
        };
        let import = ModuleImport {
            module_id: "math".to_string(),
            alias: Some("M".to_string()),
            qualifier: Some("M".to_string()),
            items: vec![ImportItem {
                name: "add".to_string(),
                alias: None,
                span,
            }],
            selected_roots: vec![ImportItem {
                name: "add".to_string(),
                alias: None,
                span,
            }],
            params: Vec::new(),
            section_maps: Vec::new(),
            span,
        };
        assert_eq!(table.add_import("core.utils", import), ImportResult::Ok);

        let imports = table
            .module_imports("CORE.UTILS")
            .expect("module imports should exist");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].module_id, "math");
        assert_eq!(imports[0].alias.as_deref(), Some("M"));
        assert_eq!(imports[0].qualifier.as_deref(), Some("M"));
    }
}
