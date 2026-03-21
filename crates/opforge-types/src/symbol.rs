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
pub struct ModuleImport {
    pub module_id: String,
    pub alias: Option<String>,
    pub items: Vec<ImportItem>,
    pub params: Vec<ImportParam>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub name: String,
    pub imports: Vec<ModuleImport>,
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
        });
        self.module_index.insert(key, idx);
        SymbolTableResult::Ok
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

        if let Some(alias) = &import.alias {
            if info
                .imports
                .iter()
                .filter_map(|existing| existing.alias.as_ref())
                .any(|existing| existing.eq_ignore_ascii_case(alias))
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
                import.alias.as_ref().and_then(|candidate| {
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
                for item in &import.items {
                    if item.name == "*" && item.alias.is_none() {
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
    use super::{
        ImportItem, ImportResult, ModuleImport, SourceSpan, SymbolTable, SymbolTableResult,
        SymbolVisibility,
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
            items: Vec::new(),
            params: Vec::new(),
            span,
        };
        assert_eq!(table.add_import("alpha", import), ImportResult::Ok);

        let import = ModuleImport {
            module_id: "gamma".to_string(),
            alias: Some("m".to_string()),
            items: Vec::new(),
            params: Vec::new(),
            span,
        };
        assert_eq!(
            table.add_import("alpha", import),
            ImportResult::AliasCollision
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
            items: vec![ImportItem {
                name: "add".to_string(),
                alias: None,
                span,
            }],
            params: Vec::new(),
            span,
        };
        assert_eq!(table.add_import("core.utils", import), ImportResult::Ok);

        let imports = table
            .module_imports("CORE.UTILS")
            .expect("module imports should exist");
        assert_eq!(imports.len(), 1);
        assert_eq!(imports[0].module_id, "math");
        assert_eq!(imports[0].alias.as_deref(), Some("M"));
    }
}
