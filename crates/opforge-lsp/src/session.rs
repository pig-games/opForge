// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use serde_json::{json, Map, Value};

use crate::lsp::code_actions::quick_fix_actions;
use crate::lsp::completion::{completion_items, CompletionRequestContext};
use crate::lsp::config::LspConfig;
use crate::lsp::definition::definition_locations;
use crate::lsp::diagnostics::{dedup_diagnostics, diagnostics_for_uri};
use crate::lsp::document_state::{DocumentState, UseImportDecl};
use crate::lsp::document_symbols::document_symbols;
use crate::lsp::hover::{hover_response, HoverRequestContext};
use crate::lsp::member_context::{member_completion_context, member_lookup_context};
use crate::lsp::validation_runner::{run_validation, ValidationDiagnostic};
use crate::lsp::workspace_index::{IndexedSymbol, WorkspaceIndex};
use libopforge::io::{MemorySourceProvider, SourceProvider};
use libopforge::opcore::{
    parse_incbin_target_from_source_line, parse_include_target_from_source_line,
};
use libopforge::registry::{
    default_asm_registry, default_cpu, resolve_cpu_for_line, AsmRegistry, AsmRegistryContext,
    CpuType,
};

static OVERLAY_DIR_SEQUENCE: AtomicUsize = AtomicUsize::new(1);

struct ValidationWorkerGuard {
    counter: Arc<AtomicUsize>,
}

impl ValidationWorkerGuard {
    fn new(counter: Arc<AtomicUsize>) -> Self {
        Self { counter }
    }
}

impl Drop for ValidationWorkerGuard {
    fn drop(&mut self) {
        self.counter.fetch_sub(1, Ordering::Relaxed);
    }
}

#[derive(Debug, Clone)]
pub enum OutboundMessage {
    Response {
        id: Value,
        result: Value,
    },
    Error {
        id: Value,
        code: i64,
        message: String,
    },
    Notification {
        method: String,
        params: Value,
    },
}

pub struct LspSession {
    config: LspConfig,
    context: AsmRegistryContext,
    documents: HashMap<String, DocumentState>,
    workspace_index: WorkspaceIndex,
    last_validation_at: HashMap<String, Instant>,
    validation_tx: Sender<ValidationTaskResult>,
    validation_rx: Receiver<ValidationTaskResult>,
    latest_validation_generation: HashMap<String, u64>,
    next_validation_generation: u64,
    pending_validation_uris: HashSet<String>,
    diagnostic_contributions_by_root: HashMap<String, HashMap<String, Vec<ValidationDiagnostic>>>,
    validation_dependencies_by_root: HashMap<String, HashSet<String>>,
    active_validations: Arc<AtomicUsize>,
    workspace_index_rebuilds: u64,
    shutdown_requested: bool,
    exit_requested: bool,
}

impl Default for LspSession {
    fn default() -> Self {
        Self::new()
    }
}

impl LspSession {
    pub fn new() -> Self {
        Self::with_registry(default_asm_registry())
    }

    pub fn with_registry(registry: AsmRegistry) -> Self {
        let (validation_tx, validation_rx) = mpsc::channel();
        Self {
            config: LspConfig::default(),
            context: AsmRegistryContext::new(registry),
            documents: HashMap::new(),
            workspace_index: WorkspaceIndex::default(),
            last_validation_at: HashMap::new(),
            validation_tx,
            validation_rx,
            latest_validation_generation: HashMap::new(),
            next_validation_generation: 1,
            pending_validation_uris: HashSet::new(),
            diagnostic_contributions_by_root: HashMap::new(),
            validation_dependencies_by_root: HashMap::new(),
            active_validations: Arc::new(AtomicUsize::new(0)),
            workspace_index_rebuilds: 0,
            shutdown_requested: false,
            exit_requested: false,
        }
    }

    pub fn should_exit(&self) -> bool {
        self.exit_requested
    }

    pub fn poll_async_notifications(&mut self) -> Vec<OutboundMessage> {
        self.drain_validation_results()
    }

    pub fn handle_message(&mut self, message: &Value) -> Vec<OutboundMessage> {
        let mut out = self.drain_validation_results();
        let method = message.get("method").and_then(Value::as_str);
        let id = message.get("id").cloned();

        let Some(method) = method else {
            return out;
        };
        let params = message.get("params").cloned().unwrap_or(Value::Null);

        if let Some(id) = id {
            match self.handle_request(method, &params) {
                Ok(result) => out.push(OutboundMessage::Response { id, result }),
                Err((code, msg)) => out.push(OutboundMessage::Error {
                    id,
                    code,
                    message: msg,
                }),
            }
            return out;
        }

        out.extend(self.handle_notification(method, &params));
        out
    }

    fn handle_request(&mut self, method: &str, params: &Value) -> Result<Value, (i64, String)> {
        if self.shutdown_requested && method != "shutdown" {
            return Err((-32600, "server has shut down".to_string()));
        }

        match method {
            "initialize" => Ok(self.handle_initialize(params)),
            "shutdown" => {
                self.shutdown_requested = true;
                Ok(Value::Null)
            }
            "textDocument/completion" => Ok(self.handle_completion(params)),
            "textDocument/hover" => Ok(self.handle_hover(params)),
            "textDocument/definition" => Ok(self.handle_definition(params)),
            "textDocument/references" => Ok(self.handle_references(params)),
            "textDocument/prepareRename" => Ok(self.handle_prepare_rename(params)),
            "textDocument/rename" => self.handle_rename(params),
            "textDocument/documentSymbol" => Ok(self.handle_document_symbol(params)),
            "textDocument/codeAction" => Ok(Value::Array(quick_fix_actions(params))),
            "workspace/symbol" => Ok(self.handle_workspace_symbol(params)),
            "opforge/internalWorkspaceIndexStats" => {
                Ok(self.handle_internal_workspace_index_stats())
            }
            _ => Err((-32601, format!("method not found: {method}"))),
        }
    }

    fn handle_notification(&mut self, method: &str, params: &Value) -> Vec<OutboundMessage> {
        if self.shutdown_requested && method != "exit" {
            return Vec::new();
        }

        match method {
            "initialized" => Vec::new(),
            "exit" => {
                self.exit_requested = true;
                Vec::new()
            }
            "workspace/didChangeConfiguration" => self.handle_config_change(params),
            "textDocument/didOpen" => self.handle_did_open(params),
            "textDocument/didChange" => self.handle_did_change(params),
            "textDocument/didSave" => self.handle_did_save(params),
            "textDocument/didClose" => self.handle_did_close(params),
            _ => Vec::new(),
        }
    }

    fn handle_initialize(&mut self, params: &Value) -> Value {
        self.config
            .update_from_workspace_settings(params.get("initializationOptions"));
        merge_initialize_roots(&mut self.config, params);
        self.rebuild_workspace_index();

        json!({
            "capabilities": {
                "textDocumentSync": {
                    "openClose": true,
                    "change": 1,
                    "save": { "includeText": true }
                },
                "completionProvider": { "resolveProvider": false, "triggerCharacters": ["."] },
                "hoverProvider": true,
                "definitionProvider": true,
                "referencesProvider": true,
                "renameProvider": { "prepareProvider": true },
                "documentSymbolProvider": true,
                "codeActionProvider": { "codeActionKinds": ["quickfix"] },
                "workspaceSymbolProvider": true
            },
            "serverInfo": {
                "name": "lsp",
                "version": env!("CARGO_PKG_VERSION")
            }
        })
    }

    fn handle_config_change(&mut self, params: &Value) -> Vec<OutboundMessage> {
        let previous = self.config.clone();
        self.config
            .update_from_workspace_settings(params.get("settings"));
        self.rebuild_workspace_index();
        if validation_refresh_required(&previous, &self.config) {
            return self.refresh_validation_for_open_documents();
        }
        Vec::new()
    }

    fn upsert_open_document_state(&mut self, uri: &str, version: i64, text: String) {
        let path = uri_to_path(uri);
        let mut state = DocumentState::new(uri.to_string(), path, version, text);
        state.refresh_derived_state(self.context.registry());
        self.documents.insert(uri.to_string(), state);
        self.refresh_workspace_index_for_document(uri);
    }

    fn handle_did_open(&mut self, params: &Value) -> Vec<OutboundMessage> {
        let Some(doc) = params.get("textDocument") else {
            return Vec::new();
        };
        let Some(uri) = doc.get("uri").and_then(Value::as_str) else {
            return Vec::new();
        };
        let text = doc
            .get("text")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_string();
        let version = doc.get("version").and_then(Value::as_i64).unwrap_or(0);
        self.upsert_open_document_state(uri, version, text);
        self.maybe_validate_and_publish(uri, true)
    }

    fn handle_did_change(&mut self, params: &Value) -> Vec<OutboundMessage> {
        let Some(doc) = params.get("textDocument") else {
            return Vec::new();
        };
        let Some(uri) = doc.get("uri").and_then(Value::as_str) else {
            return Vec::new();
        };
        let version = doc.get("version").and_then(Value::as_i64).unwrap_or(0);
        let text = params
            .get("contentChanges")
            .and_then(Value::as_array)
            .and_then(|changes| changes.last())
            .and_then(|entry| entry.get("text"))
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_string();
        if text.is_empty() && !self.documents.contains_key(uri) {
            return Vec::new();
        }
        self.upsert_open_document_state(uri, version, text);
        let stale_targets = self.invalidate_dependent_validation_contributions(uri);
        let mut out = if stale_targets.is_empty() {
            Vec::new()
        } else {
            self.publish_merged_diagnostics_for_targets(stale_targets)
        };
        out.extend(self.maybe_validate_and_publish(uri, false));
        out
    }

    fn handle_did_save(&mut self, params: &Value) -> Vec<OutboundMessage> {
        let Some(uri) = params
            .get("textDocument")
            .and_then(|value| value.get("uri"))
            .and_then(Value::as_str)
        else {
            return Vec::new();
        };
        if let Some(text) = params.get("text").and_then(Value::as_str) {
            if let Some(state) = self.documents.get_mut(uri) {
                state.text = text.to_string();
                state.refresh_derived_state(self.context.registry());
            }
        }
        if self.documents.contains_key(uri) {
            self.refresh_workspace_index_for_document(uri);
        } else {
            self.refresh_rooted_workspace_document(uri);
        }
        self.maybe_validate_and_publish(uri, true)
    }

    fn handle_did_close(&mut self, params: &Value) -> Vec<OutboundMessage> {
        let Some(uri) = params
            .get("textDocument")
            .and_then(|value| value.get("uri"))
            .and_then(Value::as_str)
        else {
            return Vec::new();
        };
        self.invalidate_validation_generation(uri);
        self.pending_validation_uris.remove(uri);
        self.documents.remove(uri);
        self.refresh_rooted_workspace_document(uri);
        let mut targets: HashSet<String> = self
            .diagnostic_contributions_by_root
            .remove(uri)
            .unwrap_or_default()
            .into_keys()
            .collect();
        targets.extend(self.invalidate_dependent_validation_contributions(uri));
        self.validation_dependencies_by_root.remove(uri);
        targets.insert(uri.to_string());
        self.publish_merged_diagnostics_for_targets(targets)
    }

    fn handle_completion(&self, params: &Value) -> Value {
        let Some(uri) = params
            .get("textDocument")
            .and_then(|value| value.get("uri"))
            .and_then(Value::as_str)
        else {
            return Value::Array(Vec::new());
        };
        let line = params
            .get("position")
            .and_then(|value| value.get("line"))
            .and_then(Value::as_u64)
            .unwrap_or(0) as u32;
        let character = params
            .get("position")
            .and_then(|value| value.get("character"))
            .and_then(Value::as_u64)
            .unwrap_or(0) as usize;
        let doc = self.documents.get(uri);
        let cpu = self.resolve_cpu_for_request(doc, line + 1);
        let line_text = doc.and_then(|state| state.lines.get(line as usize));
        let prefix = line_text
            .map(|line_value| token_prefix_at(line_value, character))
            .unwrap_or_default();
        let member_ctx =
            line_text.and_then(|line_value| member_completion_context(line_value, character));
        Value::Array(completion_items(
            self.context.snapshot(),
            &self.workspace_index,
            doc,
            cpu,
            CompletionRequestContext {
                config: &self.config,
                current_uri: uri,
                cursor_line: line + 1,
                prefix: prefix.as_str(),
                member_ctx: member_ctx.as_ref(),
            },
        ))
    }

    fn handle_hover(&self, params: &Value) -> Value {
        let Some(uri) = params
            .get("textDocument")
            .and_then(|value| value.get("uri"))
            .and_then(Value::as_str)
        else {
            return Value::Null;
        };
        let line = params
            .get("position")
            .and_then(|value| value.get("line"))
            .and_then(Value::as_u64)
            .unwrap_or(0) as u32;
        let character = params
            .get("position")
            .and_then(|value| value.get("character"))
            .and_then(Value::as_u64)
            .unwrap_or(0) as usize;
        let doc = self.documents.get(uri);
        let cpu = self.resolve_cpu_for_request(doc, line + 1);
        let line_text = doc.and_then(|state| state.lines.get(line as usize));
        let word = line_text
            .map(|line_value| token_word_at(line_value, character))
            .unwrap_or_default();
        let member_ctx =
            line_text.and_then(|line_value| member_lookup_context(line_value, character));
        hover_response(
            self.context.snapshot(),
            &self.workspace_index,
            doc,
            cpu,
            HoverRequestContext {
                config: &self.config,
                current_uri: uri,
                request_line: line + 1,
                word: word.as_str(),
                member_ctx: member_ctx.as_ref(),
            },
        )
        .unwrap_or(Value::Null)
    }

    fn handle_definition(&self, params: &Value) -> Value {
        let Some(uri) = params
            .get("textDocument")
            .and_then(|value| value.get("uri"))
            .and_then(Value::as_str)
        else {
            return Value::Array(Vec::new());
        };
        let line = params
            .get("position")
            .and_then(|value| value.get("line"))
            .and_then(Value::as_u64)
            .unwrap_or(0) as usize;
        let character = params
            .get("position")
            .and_then(|value| value.get("character"))
            .and_then(Value::as_u64)
            .unwrap_or(0) as usize;
        let doc = self.documents.get(uri);
        let line_text = doc.and_then(|state| state.lines.get(line));
        let word = line_text
            .map(|line_value| token_word_at(line_value, character))
            .unwrap_or_default();
        let member_ctx =
            line_text.and_then(|line_value| member_lookup_context(line_value, character));
        Value::Array(definition_locations(
            &self.config,
            &self.workspace_index,
            doc,
            uri,
            (line + 1) as u32,
            word.as_str(),
            member_ctx.as_ref(),
        ))
    }

    fn handle_document_symbol(&self, params: &Value) -> Value {
        let Some(uri) = params
            .get("textDocument")
            .and_then(|value| value.get("uri"))
            .and_then(Value::as_str)
        else {
            return Value::Array(Vec::new());
        };
        let Some(doc) = self.documents.get(uri) else {
            return Value::Array(Vec::new());
        };
        Value::Array(document_symbols(doc))
    }

    fn handle_internal_workspace_index_stats(&self) -> Value {
        json!({
            "rootedRebuilds": self.workspace_index_rebuilds,
            "indexedUris": self.workspace_index.document_uris(),
        })
    }

    fn handle_references(&self, params: &Value) -> Value {
        let Some(uri) = params
            .get("textDocument")
            .and_then(|value| value.get("uri"))
            .and_then(Value::as_str)
        else {
            return Value::Array(Vec::new());
        };
        let line = params
            .get("position")
            .and_then(|value| value.get("line"))
            .and_then(Value::as_u64)
            .unwrap_or(0) as usize;
        let character = params
            .get("position")
            .and_then(|value| value.get("character"))
            .and_then(Value::as_u64)
            .unwrap_or(0) as usize;
        let include_declaration = params
            .get("context")
            .and_then(|value| value.get("includeDeclaration"))
            .and_then(Value::as_bool)
            .unwrap_or(true);
        let doc = self.documents.get(uri);
        let line_text = doc.and_then(|state| state.lines.get(line));
        let word = line_text
            .map(|line_value| token_word_at(line_value, character))
            .unwrap_or_default();
        let member_ctx =
            line_text.and_then(|line_value| member_lookup_context(line_value, character));
        if word.is_empty() {
            return Value::Array(Vec::new());
        }

        let defs = definition_locations(
            &self.config,
            &self.workspace_index,
            doc,
            uri,
            (line + 1) as u32,
            word.as_str(),
            member_ctx.as_ref(),
        );
        let Some(target) = defs
            .first()
            .and_then(|value| self.symbol_from_location_value(value))
        else {
            return Value::Array(Vec::new());
        };

        Value::Array(self.reference_locations_for_target(&target, include_declaration))
    }

    fn handle_workspace_symbol(&self, params: &Value) -> Value {
        let query = params
            .get("query")
            .and_then(Value::as_str)
            .unwrap_or_default();
        if query.is_empty() {
            return Value::Array(Vec::new());
        }
        let symbols = self.workspace_index.search_symbols(query, 100);
        Value::Array(
            symbols
                .into_iter()
                .map(|symbol| {
                    json!({
                        "name": symbol.name,
                        "kind": workspace_symbol_kind_to_lsp(&symbol.kind),
                        "location": {
                            "uri": symbol.uri,
                            "range": {
                                "start": {"line": symbol.line.saturating_sub(1), "character": symbol.col_start.saturating_sub(1)},
                                "end": {"line": symbol.line.saturating_sub(1), "character": symbol.col_end.saturating_sub(1).max(symbol.col_start.saturating_sub(1) + 1)},
                            }
                        }
                    })
                })
                .collect(),
        )
    }

    fn handle_prepare_rename(&self, params: &Value) -> Value {
        let Some(uri) = params
            .get("textDocument")
            .and_then(|value| value.get("uri"))
            .and_then(Value::as_str)
        else {
            return Value::Null;
        };
        let line = params
            .get("position")
            .and_then(|value| value.get("line"))
            .and_then(Value::as_u64)
            .unwrap_or(0) as usize;
        let character = params
            .get("position")
            .and_then(|value| value.get("character"))
            .and_then(Value::as_u64)
            .unwrap_or(0) as usize;

        let Some((target, current_word, line_text)) =
            self.resolve_rename_target(uri, line, character)
        else {
            return Value::Null;
        };
        let Some((start_col, end_col, _)) = token_span_at(line_text, character) else {
            return Value::Null;
        };
        let (rename_start, rename_end, placeholder) =
            rename_span_for_word(&current_word, start_col, end_col, &target.name);
        json!({
            "range": {
                "start": {"line": line as u32, "character": rename_start.saturating_sub(1)},
                "end": {"line": line as u32, "character": rename_end.saturating_sub(1)},
            },
            "placeholder": placeholder,
        })
    }

    fn handle_rename(&self, params: &Value) -> Result<Value, (i64, String)> {
        let Some(uri) = params
            .get("textDocument")
            .and_then(|value| value.get("uri"))
            .and_then(Value::as_str)
        else {
            return Ok(Value::Null);
        };
        let Some(new_name) = params.get("newName").and_then(Value::as_str) else {
            return Err((-32602, "missing rename newName".to_string()));
        };
        if !is_valid_rename_identifier(new_name) {
            return Err((-32602, "invalid rename identifier".to_string()));
        }

        let line = params
            .get("position")
            .and_then(|value| value.get("line"))
            .and_then(Value::as_u64)
            .unwrap_or(0) as usize;
        let character = params
            .get("position")
            .and_then(|value| value.get("character"))
            .and_then(Value::as_u64)
            .unwrap_or(0) as usize;
        let Some((target, _, _)) = self.resolve_rename_target(uri, line, character) else {
            return Ok(Value::Null);
        };

        let edits_by_uri = self.rename_text_edits_for_target(&target, new_name);
        if edits_by_uri.is_empty() {
            return Ok(Value::Null);
        }

        let mut changes = Map::new();
        for (edit_uri, edits) in edits_by_uri {
            changes.insert(edit_uri, Value::Array(edits));
        }
        Ok(json!({ "changes": Value::Object(changes) }))
    }

    fn resolve_rename_target(
        &self,
        uri: &str,
        line: usize,
        character: usize,
    ) -> Option<(IndexedSymbol, String, &str)> {
        let doc = self.documents.get(uri)?;
        let line_text = doc.lines.get(line)?;
        let word = token_word_at(line_text, character);
        if word.is_empty() {
            return None;
        }
        let member_ctx = member_lookup_context(line_text, character);
        let defs = definition_locations(
            &self.config,
            &self.workspace_index,
            Some(doc),
            uri,
            (line + 1) as u32,
            word.as_str(),
            member_ctx.as_ref(),
        );
        let target = defs
            .first()
            .and_then(|value| self.symbol_from_location_value(value))?;
        Some((target, word, line_text.as_str()))
    }

    fn resolve_cpu_for_request(&self, doc: Option<&DocumentState>, line: u32) -> CpuType {
        let workspace_default = self
            .config
            .default_cpu
            .as_deref()
            .and_then(|name| self.context.registry().resolve_cpu_name(name));
        if let Some(doc) = doc {
            resolve_cpu_for_line(line, &doc.cpu_transitions, workspace_default)
        } else {
            workspace_default.unwrap_or(default_cpu())
        }
    }

    fn symbol_from_location_value(&self, location: &Value) -> Option<IndexedSymbol> {
        let uri = location.get("uri").and_then(Value::as_str)?;
        let start = location.get("range")?.get("start")?;
        let line0 = start.get("line").and_then(Value::as_u64)? as u32;
        let char0 = start.get("character").and_then(Value::as_u64)? as u32;
        let line = line0.saturating_add(1);
        let col = char0.saturating_add(1);
        self.workspace_index
            .all_symbols_for_uri(uri)
            .into_iter()
            .find(|symbol| {
                symbol.line == line
                    && col >= symbol.col_start
                    && col <= symbol.col_end.max(symbol.col_start)
            })
    }

    fn reference_locations_for_target(
        &self,
        target: &IndexedSymbol,
        include_declaration: bool,
    ) -> Vec<Value> {
        let mut out = Vec::new();
        let mut seen = HashSet::new();
        for uri in self.workspace_index.document_uris() {
            let spellings = self.reference_spellings_for_document(&uri, target);
            if spellings.is_empty() {
                continue;
            }
            let spelling_set: HashSet<String> = spellings
                .into_iter()
                .map(|s| s.to_ascii_lowercase())
                .collect();
            let lines = self.lines_for_uri(&uri);
            for (line_idx, line_text) in lines.iter().enumerate() {
                for (start_col, end_col, token) in symbol_token_spans(line_text) {
                    if !spelling_set.contains(&token.to_ascii_lowercase()) {
                        continue;
                    }
                    if !include_declaration
                        && uri == target.uri
                        && (line_idx as u32 + 1) == target.line
                        && start_col == target.col_start
                    {
                        continue;
                    }
                    let key = (uri.clone(), line_idx as u32, start_col);
                    if !seen.insert(key) {
                        continue;
                    }
                    out.push(json!({
                        "uri": uri,
                        "range": {
                            "start": {"line": line_idx as u32, "character": start_col.saturating_sub(1)},
                            "end": {"line": line_idx as u32, "character": end_col.saturating_sub(1)},
                        }
                    }));
                }
            }
        }
        out.sort_by(|a, b| {
            let a_uri = a.get("uri").and_then(Value::as_str).unwrap_or_default();
            let b_uri = b.get("uri").and_then(Value::as_str).unwrap_or_default();
            let a_line = a
                .get("range")
                .and_then(|range| range.get("start"))
                .and_then(|start| start.get("line"))
                .and_then(Value::as_u64)
                .unwrap_or(0);
            let b_line = b
                .get("range")
                .and_then(|range| range.get("start"))
                .and_then(|start| start.get("line"))
                .and_then(Value::as_u64)
                .unwrap_or(0);
            let a_char = a
                .get("range")
                .and_then(|range| range.get("start"))
                .and_then(|start| start.get("character"))
                .and_then(Value::as_u64)
                .unwrap_or(0);
            let b_char = b
                .get("range")
                .and_then(|range| range.get("start"))
                .and_then(|start| start.get("character"))
                .and_then(Value::as_u64)
                .unwrap_or(0);
            a_uri
                .cmp(b_uri)
                .then(a_line.cmp(&b_line))
                .then(a_char.cmp(&b_char))
        });
        out
    }

    fn rename_text_edits_for_target(
        &self,
        target: &IndexedSymbol,
        new_name: &str,
    ) -> Vec<(String, Vec<Value>)> {
        let mut out: Vec<(String, Vec<Value>)> = Vec::new();
        for uri in self.workspace_index.document_uris() {
            let rules = self.rename_rules_for_document(&uri, target, new_name);
            if rules.is_empty() {
                continue;
            }
            let lines = self.lines_for_uri(&uri);
            let mut edits = Vec::new();
            for (line_idx, line_text) in lines.iter().enumerate() {
                for (start_col, end_col, token) in symbol_token_spans(line_text) {
                    let lower = token.to_ascii_lowercase();
                    let Some(replacement) = rules.get(&lower) else {
                        continue;
                    };
                    if token == *replacement {
                        continue;
                    }
                    edits.push(json!({
                        "range": {
                            "start": {"line": line_idx as u32, "character": start_col.saturating_sub(1)},
                            "end": {"line": line_idx as u32, "character": end_col.saturating_sub(1)},
                        },
                        "newText": replacement,
                    }));
                }
            }
            if !edits.is_empty() {
                out.push((uri, edits));
            }
        }
        out.sort_by(|(left_uri, _), (right_uri, _)| left_uri.cmp(right_uri));
        out
    }

    fn rename_rules_for_document(
        &self,
        uri: &str,
        target: &IndexedSymbol,
        new_name: &str,
    ) -> HashMap<String, String> {
        let mut rules = HashMap::new();
        if uri == target.uri
            || self.document_matches_owner_module(uri, target.owner_module.as_deref())
        {
            rules.insert(target.name.to_ascii_lowercase(), new_name.to_string());
        }

        if let Some(module_id) = target.owner_module.as_deref() {
            let module_qualified = format!("{module_id}.{}", target.name);
            let module_replacement = format!("{module_id}.{new_name}");
            rules.insert(
                module_qualified.to_ascii_lowercase(),
                module_replacement.to_string(),
            );

            for import in self.imports_for_uri(uri) {
                if !import.module_id.eq_ignore_ascii_case(module_id) {
                    continue;
                }
                if !qualified_import_includes_symbol(&import, &target.name) {
                    continue;
                }
                if let Some(qualifier) = &import.qualifier {
                    let old = format!("{qualifier}.{}", target.name);
                    let new = format!("{qualifier}.{new_name}");
                    rules.insert(old.to_ascii_lowercase(), new);
                }
            }
        }

        rules
    }

    fn reference_spellings_for_document(&self, uri: &str, target: &IndexedSymbol) -> Vec<String> {
        let mut out = HashSet::new();
        let same_doc = uri == target.uri;
        if same_doc || self.document_matches_owner_module(uri, target.owner_module.as_deref()) {
            out.insert(target.name.clone());
        }

        if let Some(module_id) = target.owner_module.as_deref() {
            out.insert(format!("{module_id}.{}", target.name));
            for import in self.imports_for_uri(uri) {
                if !import.module_id.eq_ignore_ascii_case(module_id) {
                    continue;
                }
                if qualified_import_includes_symbol(&import, &target.name) {
                    if let Some(qualifier) = &import.qualifier {
                        out.insert(format!("{qualifier}.{}", target.name));
                    }
                }
                if import.wildcard {
                    out.insert(target.name.clone());
                }
                for item in &import.items {
                    if item.source_name.eq_ignore_ascii_case(&target.name) {
                        out.insert(item.local_name.clone());
                    }
                }
            }
        } else if same_doc {
            out.insert(target.name.clone());
        }

        let mut values: Vec<String> = out.into_iter().collect();
        values.sort_by_key(|item| item.to_ascii_lowercase());
        values
    }

    fn imports_for_uri(&self, uri: &str) -> Vec<UseImportDecl> {
        if let Some(doc) = self.documents.get(uri) {
            return doc.imports.clone();
        }
        self.workspace_index.imports_for_uri(uri)
    }

    fn document_matches_owner_module(&self, uri: &str, owner_module: Option<&str>) -> bool {
        let Some(owner_module) = owner_module else {
            return false;
        };
        self.workspace_index
            .all_symbols_for_uri(uri)
            .into_iter()
            .any(|symbol| {
                matches!(symbol.kind, crate::lsp::document_state::SymbolKind::Module)
                    && symbol.name.eq_ignore_ascii_case(owner_module)
            })
    }

    fn lines_for_uri(&self, uri: &str) -> Vec<String> {
        if let Some(doc) = self.documents.get(uri) {
            return doc.lines.clone();
        }
        let Some(path) = uri_to_path(uri) else {
            return Vec::new();
        };
        let Ok(text) = fs::read_to_string(path) else {
            return Vec::new();
        };
        let mut lines: Vec<String> = text.split('\n').map(ToString::to_string).collect();
        if text.ends_with('\n') {
            lines.push(String::new());
        }
        lines
    }

    fn maybe_validate_and_publish(&mut self, uri: &str, force: bool) -> Vec<OutboundMessage> {
        if !force && self.config.validation.debounce_ms > 0 {
            let now = Instant::now();
            if let Some(previous) = self.last_validation_at.get(uri) {
                let elapsed = now.saturating_duration_since(*previous);
                if elapsed < Duration::from_millis(self.config.validation.debounce_ms) {
                    return self.drain_validation_results();
                }
            }
        }
        if force && !self.config.validation.on_save {
            return self.drain_validation_results();
        }
        // Record the timestamp so subsequent non-forced events are debounced.
        self.last_validation_at
            .insert(uri.to_string(), Instant::now());
        if !self.schedule_validation(uri) {
            self.pending_validation_uris.insert(uri.to_string());
        }
        self.drain_validation_results()
    }

    fn schedule_validation(&mut self, uri: &str) -> bool {
        /// Maximum number of concurrent validation threads.
        const MAX_CONCURRENT_VALIDATIONS: usize = 2;

        let Some(doc) = self.documents.get(uri).cloned() else {
            return false;
        };
        if doc.path.is_none() {
            return false;
        }
        // Skip spawning if we already have the maximum number of active
        // validation threads running.  The next change/save event will
        // re-schedule.
        if self.active_validations.load(Ordering::Relaxed) >= MAX_CONCURRENT_VALIDATIONS {
            return false;
        }
        self.pending_validation_uris.remove(uri);
        let generation = self.issue_validation_generation(uri);
        let config = self.config.clone();
        let documents = self.documents.clone();
        let workspace_index = self.workspace_index.clone();
        let tx = self.validation_tx.clone();
        let root_uri = uri.to_string();
        let counter = Arc::clone(&self.active_validations);
        counter.fetch_add(1, Ordering::Relaxed);
        thread::spawn(move || {
            let _guard = ValidationWorkerGuard::new(counter);
            let result = run_validation_task(
                config,
                doc,
                documents,
                workspace_index,
                generation,
                root_uri,
            );
            let _ = tx.send(result);
        });
        true
    }

    fn drain_validation_results(&mut self) -> Vec<OutboundMessage> {
        let mut out = Vec::new();
        loop {
            match self.validation_rx.try_recv() {
                Ok(result) => out.extend(self.apply_validation_result(result)),
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => break,
            }
        }
        self.schedule_pending_validations();
        out
    }

    fn apply_validation_result(&mut self, result: ValidationTaskResult) -> Vec<OutboundMessage> {
        let latest_generation = self
            .latest_validation_generation
            .get(&result.root_uri)
            .copied()
            .unwrap_or_default();
        if result.generation != latest_generation {
            return Vec::new();
        }

        if let Some(current) = self.documents.get(&result.root_uri) {
            if current.version != result.version {
                return Vec::new();
            }
        } else {
            return Vec::new();
        }

        self.update_validation_dependencies(&result.root_uri, result.dependencies);
        self.publish_validation_diagnostics(&result.root_uri, result.diagnostics)
    }

    fn publish_validation_diagnostics(
        &mut self,
        root_uri: &str,
        diagnostics: Vec<ValidationDiagnostic>,
    ) -> Vec<OutboundMessage> {
        let diagnostics = dedup_diagnostics(diagnostics);

        let grouped = group_diagnostics_by_uri(root_uri, &diagnostics);
        let previous_uris = self
            .diagnostic_contributions_by_root
            .get(root_uri)
            .map(|contributions| contributions.keys().cloned().collect::<HashSet<_>>())
            .unwrap_or_default();
        let new_uris: HashSet<String> = grouped.keys().cloned().collect();
        let affected_targets: HashSet<String> = previous_uris.union(&new_uris).cloned().collect();
        self.diagnostic_contributions_by_root
            .insert(root_uri.to_string(), grouped);
        self.publish_merged_diagnostics_for_targets(affected_targets)
    }

    fn update_validation_dependencies(&mut self, root_uri: &str, dependencies: HashSet<String>) {
        self.validation_dependencies_by_root
            .insert(root_uri.to_string(), dependencies);
    }

    fn invalidate_dependent_validation_contributions(
        &mut self,
        changed_uri: &str,
    ) -> HashSet<String> {
        let dependent_roots: Vec<String> = self
            .validation_dependencies_by_root
            .iter()
            .filter_map(|(root_uri, dependencies)| {
                if root_uri != changed_uri && dependencies.contains(changed_uri) {
                    Some(root_uri.clone())
                } else {
                    None
                }
            })
            .collect();

        let mut affected_targets = HashSet::new();
        for root_uri in dependent_roots {
            self.invalidate_validation_generation(&root_uri);
            self.last_validation_at.remove(&root_uri);
            if self.documents.contains_key(&root_uri) {
                self.pending_validation_uris.insert(root_uri.clone());
            }
            self.validation_dependencies_by_root.remove(&root_uri);
            if let Some(contributions) = self.diagnostic_contributions_by_root.remove(&root_uri) {
                affected_targets.extend(contributions.into_keys());
            }
        }
        if !affected_targets.is_empty() {
            affected_targets.insert(changed_uri.to_string());
        }
        affected_targets
    }

    fn issue_validation_generation(&mut self, uri: &str) -> u64 {
        let generation = self.next_validation_generation;
        self.next_validation_generation = self.next_validation_generation.saturating_add(1);
        self.latest_validation_generation
            .insert(uri.to_string(), generation);
        generation
    }

    fn invalidate_validation_generation(&mut self, uri: &str) {
        let generation = self.next_validation_generation;
        self.next_validation_generation = self.next_validation_generation.saturating_add(1);
        self.latest_validation_generation
            .insert(uri.to_string(), generation);
    }

    fn refresh_validation_for_open_documents(&mut self) -> Vec<OutboundMessage> {
        let mut uris: Vec<String> = self.documents.keys().cloned().collect();
        uris.sort();
        for uri in uris {
            self.last_validation_at.remove(&uri);
            self.invalidate_validation_generation(&uri);
            self.pending_validation_uris.insert(uri);
        }
        self.drain_validation_results()
    }

    fn rebuild_workspace_index(&mut self) {
        self.context.rebuild_snapshot();
        self.workspace_index
            .rebuild(self.context.registry(), &self.config, &self.documents);
        self.workspace_index_rebuilds = self.workspace_index_rebuilds.saturating_add(1);
    }

    fn refresh_workspace_index_for_document(&mut self, uri: &str) {
        let Some(doc) = self.documents.get(uri) else {
            return;
        };
        self.workspace_index.index_document(doc);
    }

    fn refresh_rooted_workspace_document(&mut self, uri: &str) {
        let _ = self.workspace_index.refresh_rooted_document(
            self.context.registry(),
            &self.config,
            uri,
        );
    }

    fn schedule_pending_validations(&mut self) {
        if self.pending_validation_uris.is_empty() {
            return;
        }
        let mut uris: Vec<String> = self.pending_validation_uris.iter().cloned().collect();
        uris.sort();
        for uri in uris {
            if !self.pending_validation_uris.contains(&uri) {
                continue;
            }
            if !self.schedule_validation(&uri) {
                break;
            }
        }
    }

    fn publish_merged_diagnostics_for_targets(
        &self,
        targets: HashSet<String>,
    ) -> Vec<OutboundMessage> {
        let mut sorted_targets: Vec<String> = targets.into_iter().collect();
        sorted_targets.sort();
        sorted_targets
            .into_iter()
            .map(|target_uri| {
                let diagnostics = self.merged_diagnostics_for_target(&target_uri);
                OutboundMessage::Notification {
                    method: "textDocument/publishDiagnostics".to_string(),
                    params: json!({
                        "uri": target_uri,
                        "diagnostics": diagnostics_for_uri(&target_uri, &diagnostics),
                    }),
                }
            })
            .collect()
    }

    fn merged_diagnostics_for_target(&self, target_uri: &str) -> Vec<ValidationDiagnostic> {
        let mut merged = Vec::new();
        for contributions in self.diagnostic_contributions_by_root.values() {
            if let Some(diagnostics) = contributions.get(target_uri) {
                merged.extend(diagnostics.iter().cloned());
            }
        }
        dedup_diagnostics(merged)
    }
}

fn workspace_symbol_kind_to_lsp(kind: &crate::lsp::document_state::SymbolKind) -> u32 {
    match kind {
        crate::lsp::document_state::SymbolKind::Module => 2,
        crate::lsp::document_state::SymbolKind::Namespace => 3,
        crate::lsp::document_state::SymbolKind::Macro => 12,
        crate::lsp::document_state::SymbolKind::Label
        | crate::lsp::document_state::SymbolKind::Assignment => 13,
        crate::lsp::document_state::SymbolKind::Section => 5,
        crate::lsp::document_state::SymbolKind::Statement => 6,
        crate::lsp::document_state::SymbolKind::UseImport => 9,
    }
}

fn group_diagnostics_by_uri(
    active_uri: &str,
    diagnostics: &[ValidationDiagnostic],
) -> HashMap<String, Vec<ValidationDiagnostic>> {
    let mut grouped: HashMap<String, Vec<ValidationDiagnostic>> = HashMap::new();
    for diagnostic in diagnostics {
        let target_uri = diagnostic
            .file
            .as_ref()
            .map(|path| path_to_file_uri(Path::new(path)))
            .unwrap_or_else(|| active_uri.to_string());
        grouped
            .entry(target_uri)
            .or_default()
            .push(diagnostic.clone());
    }
    if grouped.is_empty() {
        grouped.insert(active_uri.to_string(), Vec::new());
    }
    grouped
}

fn merge_initialize_roots(config: &mut LspConfig, params: &Value) {
    let mut merged = config.roots.clone();
    for root in initialize_roots_from_params(params) {
        if !merged.iter().any(|existing| existing == &root) {
            merged.push(root);
        }
    }
    merged.sort();
    merged.dedup();
    config.roots = merged;
}

fn initialize_roots_from_params(params: &Value) -> Vec<String> {
    let mut roots = Vec::new();
    if let Some(uri) = params.get("rootUri").and_then(Value::as_str) {
        if let Some(path) = uri_to_path(uri) {
            roots.push(path.to_string_lossy().to_string());
        }
    }
    if let Some(folders) = params.get("workspaceFolders").and_then(Value::as_array) {
        for folder in folders {
            if let Some(uri) = folder.get("uri").and_then(Value::as_str) {
                if let Some(path) = uri_to_path(uri) {
                    roots.push(path.to_string_lossy().to_string());
                }
            }
        }
    }
    roots.sort();
    roots.dedup();
    roots
}

#[derive(Debug)]
struct OverlayWorkspace {
    temp_root: PathBuf,
    working_dir: PathBuf,
    root_file: PathBuf,
    original_root: PathBuf,
    source_files: Vec<PathBuf>,
}

#[derive(Debug)]
struct ValidationTaskResult {
    root_uri: String,
    version: i64,
    generation: u64,
    dependencies: HashSet<String>,
    diagnostics: Vec<ValidationDiagnostic>,
}

fn run_validation_task(
    config: LspConfig,
    doc: DocumentState,
    open_docs: HashMap<String, DocumentState>,
    workspace_index: WorkspaceIndex,
    generation: u64,
    root_uri: String,
) -> ValidationTaskResult {
    let overlay = match create_overlay_workspace(&config, &doc, &open_docs, &workspace_index) {
        Ok(overlay) => overlay,
        Err(message) => {
            return ValidationTaskResult {
                root_uri,
                version: doc.version,
                generation,
                dependencies: HashSet::new(),
                diagnostics: vec![overlay_failure_diagnostic(&doc, message)],
            };
        }
    };
    let module_search_roots =
        crate::lsp::workspace_index::module_search_roots_for_request(&config, &root_uri);
    let result = run_validation(
        &config,
        &overlay.root_file,
        &overlay.working_dir,
        &overlay.original_root,
        &module_search_roots,
    );

    let diagnostics = remap_overlay_diagnostics(
        result.diagnostics,
        &overlay.working_dir,
        &overlay.original_root,
    );
    let _ = fs::remove_dir_all(&overlay.temp_root);
    let dependencies = overlay
        .source_files
        .iter()
        .map(|path| path_to_file_uri(path))
        .collect();

    ValidationTaskResult {
        root_uri,
        version: doc.version,
        generation,
        dependencies,
        diagnostics,
    }
}

fn create_overlay_workspace(
    config: &LspConfig,
    active_doc: &DocumentState,
    open_docs: &HashMap<String, DocumentState>,
    workspace_index: &WorkspaceIndex,
) -> Result<OverlayWorkspace, String> {
    let Some(original_file) = active_doc.path.as_ref() else {
        return Err("active document has no filesystem path".to_string());
    };
    let original_root = overlay_root_for_active_file(config, original_file);
    let time_part = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|err| format!("system clock error: {err}"))?
        .as_nanos();
    let seq_part = OVERLAY_DIR_SEQUENCE.fetch_add(1, Ordering::Relaxed);
    let temp_root = std::env::temp_dir().join(format!("lsp-overlay-{time_part}-{seq_part}"));
    let working_dir = temp_root.join("workspace");
    fs::create_dir_all(&working_dir)
        .map_err(|err| format!("create overlay workspace {}: {err}", working_dir.display()))?;

    let source_files = collect_overlay_source_files(
        config,
        active_doc,
        open_docs,
        workspace_index,
        &original_root,
    )?;

    for source_path in &source_files {
        stage_overlay_file(&original_root, &working_dir, source_path, open_docs)?;
    }

    let root_file = overlay_target_path(&working_dir, &original_root, original_file)?;

    Ok(OverlayWorkspace {
        temp_root,
        working_dir,
        root_file,
        original_root,
        source_files,
    })
}

fn overlay_failure_diagnostic(doc: &DocumentState, message: String) -> ValidationDiagnostic {
    ValidationDiagnostic {
        code: "LSPVALIDATOR".to_string(),
        severity: "error".to_string(),
        message: format!("Validation did not complete: {message}"),
        file: doc
            .path
            .as_ref()
            .map(|path| path.to_string_lossy().to_string()),
        line: 1,
        col_start: Some(1),
        col_end: Some(1),
        fixits: Vec::new(),
    }
}

fn validation_refresh_required(previous: &LspConfig, current: &LspConfig) -> bool {
    previous != current
}

fn remap_overlay_diagnostics(
    mut diagnostics: Vec<ValidationDiagnostic>,
    overlay_root: &Path,
    original_root: &Path,
) -> Vec<ValidationDiagnostic> {
    for diag in &mut diagnostics {
        if let Some(file) = &diag.file {
            let candidate = PathBuf::from(file);
            if let Ok(relative) = candidate.strip_prefix(overlay_root) {
                let remapped = original_root.join(relative);
                diag.file = Some(remapped.to_string_lossy().to_string());
            }
        }
    }
    diagnostics
}

fn overlay_root_for_active_file(config: &LspConfig, original_file: &Path) -> PathBuf {
    preferred_workspace_root_for_path(config, original_file).unwrap_or_else(|| {
        let parent = original_file.parent().unwrap_or(Path::new("."));
        parent.parent().unwrap_or(parent).to_path_buf()
    })
}

fn collect_overlay_source_files(
    config: &LspConfig,
    active_doc: &DocumentState,
    open_docs: &HashMap<String, DocumentState>,
    workspace_index: &WorkspaceIndex,
    original_root: &Path,
) -> Result<Vec<PathBuf>, String> {
    let mut staged = HashSet::new();
    let mut queued = Vec::new();

    let Some(active_path) = active_doc.path.as_ref() else {
        return Err("active document has no filesystem path".to_string());
    };
    enqueue_overlay_path(active_path, original_root, &mut staged, &mut queued);
    for doc in open_docs.values() {
        let Some(path) = doc.path.as_ref() else {
            continue;
        };
        enqueue_overlay_path(path, original_root, &mut staged, &mut queued);
    }

    let registry = default_asm_registry();
    let source_provider = overlay_source_provider(active_doc, open_docs);
    let include_paths = resolved_overlay_search_paths(&config.include_paths, original_root);
    let mut cursor = 0usize;
    while cursor < queued.len() {
        let current_path = queued[cursor].clone();
        cursor += 1;
        let imports = overlay_imports_for_path(&registry, &current_path, open_docs)?;
        let current_uri = path_to_file_uri(&current_path);
        for import in imports {
            for candidate in workspace_index.module_document_paths(&import.module_id) {
                enqueue_overlay_path(&candidate, original_root, &mut staged, &mut queued);
            }
            for candidate in crate::lsp::workspace_index::resolve_module_target(
                &import.module_id,
                config,
                &current_uri,
            ) {
                enqueue_overlay_path(&candidate, original_root, &mut staged, &mut queued);
            }
        }
        for candidate in overlay_dependency_files_for_path(
            &current_path,
            &include_paths,
            Arc::clone(&source_provider),
        ) {
            enqueue_overlay_path(&candidate, original_root, &mut staged, &mut queued);
        }
    }

    let mut files: Vec<PathBuf> = staged.into_iter().collect();
    files.sort();
    Ok(files)
}

fn enqueue_overlay_path(
    path: &Path,
    original_root: &Path,
    staged: &mut HashSet<PathBuf>,
    queued: &mut Vec<PathBuf>,
) {
    let Ok(relative) = overlay_relative_path(original_root, path) else {
        return;
    };
    let candidate = original_root.join(relative);
    if staged.insert(candidate.clone()) {
        queued.push(candidate);
    }
}

fn overlay_source_provider(
    active_doc: &DocumentState,
    open_docs: &HashMap<String, DocumentState>,
) -> Arc<dyn SourceProvider> {
    let mut source_provider = MemorySourceProvider::new();
    if let Some(path) = active_doc.path.as_ref() {
        source_provider.insert_file(path.clone(), active_doc.text.clone());
    }
    for doc in open_docs.values() {
        let Some(path) = doc.path.as_ref() else {
            continue;
        };
        source_provider.insert_file(path.clone(), doc.text.clone());
    }
    Arc::from(source_provider.with_fs_fallback())
}

fn resolved_overlay_search_paths(paths: &[String], original_root: &Path) -> Vec<PathBuf> {
    paths
        .iter()
        .map(PathBuf::from)
        .map(|path| {
            if path.is_absolute() {
                path
            } else {
                original_root.join(path)
            }
        })
        .collect()
}

fn overlay_dependency_files_for_path(
    path: &Path,
    include_paths: &[PathBuf],
    source_provider: Arc<dyn SourceProvider>,
) -> Vec<PathBuf> {
    let mut dependencies = HashSet::new();
    collect_overlay_include_dependencies(
        path,
        include_paths,
        source_provider.as_ref(),
        &mut dependencies,
    );
    let mut dependency_files: Vec<PathBuf> = dependencies.into_iter().collect();
    dependency_files.sort();
    dependency_files
}

fn collect_overlay_include_dependencies(
    path: &Path,
    include_paths: &[PathBuf],
    source_provider: &dyn SourceProvider,
    dependencies: &mut HashSet<PathBuf>,
) {
    let Ok(text) = source_provider.read_string(path) else {
        return;
    };

    for line in text.lines() {
        if let Some(include_target) = parse_include_target_from_source_line(line) {
            let Some(resolved) = resolve_overlay_include_target(
                path,
                &include_target,
                include_paths,
                source_provider,
            ) else {
                continue;
            };
            if dependencies.insert(resolved.clone()) {
                collect_overlay_include_dependencies(
                    &resolved,
                    include_paths,
                    source_provider,
                    dependencies,
                );
            }
        }

        if let Some(incbin_target) = parse_incbin_target_from_source_line(line) {
            let Some(resolved) = resolve_overlay_include_target(
                path,
                &incbin_target,
                include_paths,
                source_provider,
            ) else {
                continue;
            };
            dependencies.insert(resolved);
        }
    }
}

fn resolve_overlay_include_target(
    base_path: &Path,
    include_target: &str,
    include_paths: &[PathBuf],
    source_provider: &dyn SourceProvider,
) -> Option<PathBuf> {
    let include_path = PathBuf::from(include_target);
    if include_path.is_absolute() {
        return source_provider
            .is_file(&include_path)
            .ok()
            .filter(|is_file| *is_file)
            .map(|_| include_path);
    }

    let mut candidates = Vec::new();
    if let Some(parent) = base_path.parent() {
        candidates.push(parent.join(&include_path));
    }
    for include_root in include_paths {
        candidates.push(include_root.join(&include_path));
    }

    for candidate in candidates {
        if source_provider.is_file(&candidate).ok().unwrap_or(false) {
            return source_provider
                .canonicalize(&candidate)
                .ok()
                .or(Some(candidate));
        }
    }

    None
}

fn overlay_imports_for_path(
    registry: &AsmRegistry,
    path: &Path,
    open_docs: &HashMap<String, DocumentState>,
) -> Result<Vec<UseImportDecl>, String> {
    if let Some(doc) = open_docs
        .values()
        .find(|doc| doc.path.as_ref().is_some_and(|doc_path| doc_path == path))
    {
        return Ok(doc.imports.clone());
    }
    let text = fs::read_to_string(path)
        .map_err(|err| format!("read dependency {}: {err}", path.display()))?;
    let mut doc = DocumentState::new(path_to_file_uri(path), Some(path.to_path_buf()), 0, text);
    doc.refresh_derived_state(registry);
    Ok(doc.imports)
}

fn stage_overlay_file(
    original_root: &Path,
    working_dir: &Path,
    source_path: &Path,
    open_docs: &HashMap<String, DocumentState>,
) -> Result<(), String> {
    ensure_overlay_path_is_not_symlinked(original_root, source_path)?;
    let target = overlay_target_path(working_dir, original_root, source_path)?;
    if let Some(parent) = target.parent() {
        fs::create_dir_all(parent)
            .map_err(|err| format!("create overlay parent {}: {err}", parent.display()))?;
    }
    if let Some(doc) = open_docs.values().find(|doc| {
        doc.path
            .as_ref()
            .is_some_and(|doc_path| doc_path == source_path)
    }) {
        fs::write(&target, doc.text.as_bytes())
            .map_err(|err| format!("write overlay file {}: {err}", target.display()))?;
        return Ok(());
    }
    let bytes = fs::read(source_path)
        .map_err(|err| format!("read overlay source {}: {err}", source_path.display()))?;
    fs::write(&target, bytes)
        .map_err(|err| format!("write overlay file {}: {err}", target.display()))?;
    Ok(())
}

fn overlay_target_path(
    working_dir: &Path,
    original_root: &Path,
    source_path: &Path,
) -> Result<PathBuf, String> {
    let relative = overlay_relative_path(original_root, source_path)?;
    Ok(working_dir.join(relative))
}

fn overlay_relative_path(original_root: &Path, source_path: &Path) -> Result<PathBuf, String> {
    if let Ok(relative) = source_path.strip_prefix(original_root) {
        return Ok(relative.to_path_buf());
    }

    let canonical_root = fs::canonicalize(original_root).map_err(|err| {
        format!(
            "canonicalize overlay root {}: {err}",
            original_root.display()
        )
    })?;
    let canonical_source = fs::canonicalize(source_path).map_err(|err| {
        format!(
            "canonicalize overlay source {}: {err}",
            source_path.display()
        )
    })?;
    canonical_source
        .strip_prefix(&canonical_root)
        .map(Path::to_path_buf)
        .map_err(|_| {
            format!(
                "path {} escapes overlay root {}",
                source_path.display(),
                original_root.display()
            )
        })
}

fn ensure_overlay_path_is_not_symlinked(
    original_root: &Path,
    source_path: &Path,
) -> Result<(), String> {
    let mut current = original_root.to_path_buf();
    let root_meta = fs::symlink_metadata(&current)
        .map_err(|err| format!("inspect overlay root {}: {err}", current.display()))?;
    if root_meta.file_type().is_symlink() {
        return Err(format!(
            "refusing to stage symlinked overlay root {}",
            current.display()
        ));
    }

    let relative = overlay_relative_path(original_root, source_path)?;

    for component in relative.components() {
        current.push(component.as_os_str());
        match fs::symlink_metadata(&current) {
            Ok(metadata) if metadata.file_type().is_symlink() => {
                return Err(format!(
                    "refusing to stage symlinked path component {}",
                    current.display()
                ));
            }
            Ok(_) => {}
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => continue,
            Err(err) => {
                return Err(format!(
                    "inspect overlay source component {}: {err}",
                    current.display()
                ));
            }
        }
    }

    Ok(())
}

pub(crate) fn configured_workspace_roots(config: &LspConfig) -> Vec<PathBuf> {
    let mut roots = Vec::new();
    for root in &config.roots {
        let path = PathBuf::from(root);
        let normalized = if path.is_file() {
            path.parent().map(Path::to_path_buf).unwrap_or(path)
        } else {
            path
        };
        if !roots.iter().any(|existing| existing == &normalized) {
            roots.push(normalized);
        }
    }
    roots
}

pub(crate) fn preferred_workspace_root_for_path(
    config: &LspConfig,
    path: &Path,
) -> Option<PathBuf> {
    let mut matches: Vec<PathBuf> = configured_workspace_roots(config)
        .into_iter()
        .filter(|root| path.starts_with(root))
        .collect();
    matches.sort_by_key(|root| root.components().count());
    matches.pop()
}

pub fn uri_to_path(uri: &str) -> Option<PathBuf> {
    if !uri.starts_with("file://") {
        return None;
    }
    let raw = uri.trim_start_matches("file://");
    let path = if let Some(rest) = raw.strip_prefix("localhost/") {
        format!("/{rest}")
    } else if raw.starts_with('/') || looks_like_windows_drive(raw) {
        raw.to_string()
    } else {
        format!("//{raw}")
    };
    let mut decoded = percent_decode(&path);
    if decoded.starts_with('/') && looks_like_windows_drive(&decoded[1..]) {
        decoded.remove(0);
    }
    if decoded.is_empty() {
        None
    } else {
        Some(PathBuf::from(decoded))
    }
}

pub fn path_to_file_uri(path: &Path) -> String {
    let raw = path.to_string_lossy().replace('\\', "/");
    let encoded = percent_encode(raw.as_ref());
    if encoded.starts_with("//") {
        format!("file:{encoded}")
    } else if looks_like_windows_drive(encoded.as_str()) {
        format!("file:///{encoded}")
    } else {
        format!("file://{encoded}")
    }
}

fn looks_like_windows_drive(path: &str) -> bool {
    let bytes = path.as_bytes();
    bytes.len() >= 3
        && bytes[0].is_ascii_alphabetic()
        && bytes[1] == b':'
        && matches!(bytes[2], b'/' | b'\\')
}

fn percent_decode(input: &str) -> String {
    let bytes = input.as_bytes();
    let mut decoded_bytes = Vec::with_capacity(bytes.len());
    let mut i = 0usize;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            let hi = bytes[i + 1] as char;
            let lo = bytes[i + 2] as char;
            if hi.is_ascii_hexdigit() && lo.is_ascii_hexdigit() {
                let hex = format!("{hi}{lo}");
                if let Ok(value) = u8::from_str_radix(&hex, 16) {
                    decoded_bytes.push(value);
                    i += 3;
                    continue;
                }
            }
        }
        decoded_bytes.push(bytes[i]);
        i += 1;
    }
    String::from_utf8(decoded_bytes).unwrap_or_else(|_| input.to_string())
}

fn percent_encode(input: &str) -> String {
    let mut out = String::new();
    for b in input.bytes() {
        let c = b as char;
        if c.is_ascii_alphanumeric() || matches!(c, '/' | '-' | '_' | '.' | '~' | ':') {
            out.push(c);
        } else {
            out.push('%');
            out.push_str(&format!("{:02X}", b));
        }
    }
    out
}

fn token_prefix_at(line: &str, char_index: usize) -> String {
    let mut idx = char_index.min(line.len());
    let bytes = line.as_bytes();
    while idx > 0 && is_symbol_char(bytes[idx - 1] as char) {
        idx -= 1;
    }
    line.get(idx..char_index.min(line.len()))
        .unwrap_or_default()
        .to_string()
}

fn token_word_at(line: &str, char_index: usize) -> String {
    let bytes = line.as_bytes();
    if bytes.is_empty() {
        return String::new();
    }
    let mut start = char_index.min(bytes.len());
    if start == bytes.len() && start > 0 {
        start -= 1;
    }
    while start > 0 && is_symbol_char(bytes[start - 1] as char) {
        start -= 1;
    }
    let mut end = char_index.min(bytes.len());
    while end < bytes.len() && is_symbol_char(bytes[end] as char) {
        end += 1;
    }
    line.get(start..end).unwrap_or_default().to_string()
}

fn is_symbol_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || matches!(ch, '_' | '.' | '$')
}

fn is_valid_rename_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first.is_ascii_alphabetic() || matches!(first, '_' | '.' | '$')) {
        return false;
    }
    chars.all(is_symbol_char)
}

fn qualified_import_includes_symbol(
    import: &crate::lsp::document_state::UseImportDecl,
    symbol_name: &str,
) -> bool {
    import.selected_roots.is_empty()
        || import
            .selected_roots
            .iter()
            .any(|root| root.source_name.eq_ignore_ascii_case(symbol_name))
}

fn symbol_token_spans(line: &str) -> Vec<(u32, u32, String)> {
    let bytes = line.as_bytes();
    let mut out = Vec::new();
    let mut idx = 0usize;
    while idx < bytes.len() {
        if !is_symbol_char(bytes[idx] as char) {
            idx += 1;
            continue;
        }
        let start = idx;
        while idx < bytes.len() && is_symbol_char(bytes[idx] as char) {
            idx += 1;
        }
        let end = idx;
        let token = line.get(start..end).unwrap_or_default().to_string();
        if !token.is_empty() {
            out.push((start as u32 + 1, end as u32 + 1, token));
        }
    }
    out
}

fn token_span_at(line: &str, char_index: usize) -> Option<(u32, u32, String)> {
    let char1 = char_index as u32 + 1;
    symbol_token_spans(line)
        .into_iter()
        .find(|(start, end, _)| char1 >= *start && char1 < *end)
}

fn rename_span_for_word(
    current_word: &str,
    token_start: u32,
    token_end: u32,
    target_name: &str,
) -> (u32, u32, String) {
    if let Some(dot) = current_word.rfind('.') {
        let leaf = &current_word[dot + 1..];
        if leaf.eq_ignore_ascii_case(target_name) {
            let leaf_start = token_start.saturating_add(dot as u32 + 1);
            return (leaf_start, token_end, target_name.to_string());
        }
    }
    (token_start, token_end, target_name.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unique_temp_dir(prefix: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_nanos();
        let dir = std::env::temp_dir().join(format!("{prefix}-{nanos}-{}", std::process::id()));
        fs::create_dir_all(&dir).expect("create temp dir");
        dir
    }

    #[test]
    fn file_uri_roundtrip_smoke() {
        let path = PathBuf::from("/tmp/opforge test.asm");
        let uri = path_to_file_uri(&path);
        let parsed = uri_to_path(&uri).expect("uri should parse");
        assert_eq!(parsed, path);
    }

    #[test]
    fn windows_drive_file_uri_roundtrip_smoke() {
        let uri = "file:///C:/Users/test/opforge.asm";
        let parsed = uri_to_path(uri).expect("uri should parse");
        assert_eq!(parsed, PathBuf::from("C:/Users/test/opforge.asm"));
        assert_eq!(
            path_to_file_uri(Path::new("C:/Users/test/opforge.asm")),
            uri
        );
    }

    #[test]
    fn unc_file_uri_roundtrip_smoke() {
        let uri = "file://server/share/opforge.asm";
        let parsed = uri_to_path(uri).expect("uri should parse");
        assert_eq!(parsed, PathBuf::from("//server/share/opforge.asm"));
        assert_eq!(
            path_to_file_uri(Path::new("//server/share/opforge.asm")),
            uri
        );
    }

    #[test]
    fn completion_tracks_nearest_prior_cpu_directive() {
        let mut session = LspSession::new();
        let uri = "file:///tmp/lsp_completion.asm";

        let _ = session.handle_message(&json!({
            "jsonrpc":"2.0",
            "id": 1,
            "method":"initialize",
            "params": {}
        }));
        let _ = session.handle_message(&json!({
            "jsonrpc":"2.0",
            "method":"textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": uri,
                    "version": 1,
                    "text": ".cpu 6502\n    brk\n.cpu z80\n    djnz label\n"
                }
            }
        }));

        let z80_response = session.handle_message(&json!({
            "jsonrpc":"2.0",
            "id": 2,
            "method":"textDocument/completion",
            "params":{
                "textDocument":{"uri": uri},
                "position":{"line":3,"character":6}
            }
        }));
        let mut has_djnz = false;
        for msg in z80_response {
            if let OutboundMessage::Response { result, .. } = msg {
                if let Some(items) = result.as_array() {
                    has_djnz = items.iter().any(|item| {
                        item.get("label")
                            .and_then(Value::as_str)
                            .is_some_and(|label| label.eq_ignore_ascii_case("djnz"))
                    });
                }
            }
        }
        assert!(has_djnz, "z80 completion should include djnz");
    }

    #[test]
    fn shutdown_does_not_request_exit_until_exit_notification_arrives() {
        let mut session = LspSession::new();

        let shutdown = session.handle_message(&json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "shutdown",
            "params": null
        }));

        assert!(matches!(
            shutdown.as_slice(),
            [OutboundMessage::Response {
                result: Value::Null,
                ..
            }]
        ));
        assert!(
            !session.should_exit(),
            "shutdown alone should not terminate the server"
        );

        let post_shutdown = session.handle_message(&json!({
            "jsonrpc": "2.0",
            "id": 2,
            "method": "textDocument/completion",
            "params": {
                "textDocument": { "uri": "file:///tmp/example.asm" },
                "position": { "line": 0, "character": 0 }
            }
        }));

        assert!(matches!(
            post_shutdown.as_slice(),
            [OutboundMessage::Error { code: -32600, .. }]
        ));

        let _ = session.handle_message(&json!({
            "jsonrpc": "2.0",
            "method": "exit",
            "params": null
        }));
        assert!(
            session.should_exit(),
            "exit notification should terminate the server"
        );
    }

    #[test]
    fn overlay_dependency_staging_accepts_single_quoted_include_targets() {
        let temp_dir = unique_temp_dir("lsp-overlay-single-quote");
        let main = temp_dir.join("main.asm");
        let include = temp_dir.join("shared.inc");

        fs::write(&main, ".include 'shared.inc'\n.byte VALUE\n").expect("write main source");
        fs::write(&include, "VALUE .const 1\n").expect("write include source");

        let dependencies = overlay_dependency_files_for_path(
            &main,
            &[],
            Arc::from(MemorySourceProvider::new().with_fs_fallback()),
        );

        assert!(dependencies.iter().any(|path| path.ends_with("shared.inc")));
        let _ = fs::remove_dir_all(temp_dir);
    }

    #[test]
    fn overlay_dependency_staging_preserves_semicolons_inside_quoted_include_paths() {
        let temp_dir = unique_temp_dir("lsp-overlay-semicolon");
        let main = temp_dir.join("main.asm");
        let include = temp_dir.join("dir;name.inc");

        fs::write(
            &main,
            ".include \"dir;name.inc\" ; trailing comment\n.byte VALUE\n",
        )
        .expect("write main source");
        fs::write(&include, "VALUE .const 1\n").expect("write include source");

        let dependencies = overlay_dependency_files_for_path(
            &main,
            &[],
            Arc::from(MemorySourceProvider::new().with_fs_fallback()),
        );

        assert!(dependencies
            .iter()
            .any(|path| path.ends_with("dir;name.inc")));
        let _ = fs::remove_dir_all(temp_dir);
    }

    #[test]
    fn overlay_dependency_staging_includes_recursive_incbin_payloads() {
        let temp_dir = unique_temp_dir("lsp-overlay-incbin");
        let main = temp_dir.join("main.asm");
        let include = temp_dir.join("shared.inc");
        let assets = temp_dir.join("assets");
        let payload = assets.join("payload.bin");

        fs::create_dir_all(&assets).expect("create assets dir");
        fs::write(&main, ".include \"shared.inc\"\n.byte 0\n").expect("write main source");
        fs::write(&include, ".incbin \"assets/payload.bin\"\n").expect("write include source");
        fs::write(&payload, [0xde, 0xad, 0xbe, 0xef]).expect("write binary payload");

        let dependencies = overlay_dependency_files_for_path(
            &main,
            &[],
            Arc::from(MemorySourceProvider::new().with_fs_fallback()),
        );

        assert!(dependencies.iter().any(|path| path.ends_with("shared.inc")));
        assert!(dependencies
            .iter()
            .any(|path| path.ends_with(Path::new("assets/payload.bin"))));
        let _ = fs::remove_dir_all(temp_dir);
    }

    #[test]
    fn validation_overlay_uses_workspace_root_module_fallback() {
        let temp_dir = unique_temp_dir("lsp-validation-module-root");
        let app_dir = temp_dir.join("app");
        let main = app_dir.join("main.asm");
        let dep = temp_dir.join("dep.asm");

        fs::create_dir_all(&app_dir).expect("create app dir");
        fs::write(
            &main,
            ".module main\n.use dep (VALUE)\nstart:\n    .byte VALUE\n.endmodule\n",
        )
        .expect("write main source");
        fs::write(
            &dep,
            ".module dep\n.pub\nVALUE .const 5\n.priv\n.endmodule\n",
        )
        .expect("write dep source");

        let registry = default_asm_registry();
        let main_uri = path_to_file_uri(&main);
        let mut doc = DocumentState::new(
            main_uri.clone(),
            Some(main.clone()),
            1,
            fs::read_to_string(&main).expect("read main source"),
        );
        doc.refresh_derived_state(&registry);

        let config = LspConfig {
            roots: vec![temp_dir.to_string_lossy().to_string()],
            ..LspConfig::default()
        };

        let result = run_validation_task(
            config,
            doc,
            HashMap::new(),
            WorkspaceIndex::default(),
            1,
            main_uri,
        );

        assert!(
            result.diagnostics.iter().all(|diagnostic| !diagnostic
                .message
                .to_ascii_lowercase()
                .contains("missing module")),
            "validation should use workspace-root fallback module paths: {:?}",
            result.diagnostics
        );
        let _ = fs::remove_dir_all(temp_dir);
    }

    #[test]
    fn dependency_change_clears_stale_cross_root_diagnostics() {
        let temp_dir = unique_temp_dir("lsp-stale-diagnostic-contribution");
        let main = temp_dir.join("main.asm");
        let dep = temp_dir.join("dep.asm");
        fs::write(&main, ".include \"dep.asm\"\n").expect("write main source");
        fs::write(&dep, ".byte MISSING\n").expect("write dependency source");

        let main_uri = path_to_file_uri(&main);
        let dep_uri = path_to_file_uri(&dep);
        let mut session = LspSession::new();
        session.documents.insert(
            main_uri.clone(),
            DocumentState::new(
                main_uri.clone(),
                Some(main.clone()),
                1,
                fs::read_to_string(&main).expect("read main source"),
            ),
        );
        session.documents.insert(
            dep_uri.clone(),
            DocumentState::new(
                dep_uri.clone(),
                Some(dep.clone()),
                1,
                fs::read_to_string(&dep).expect("read dependency source"),
            ),
        );
        session.active_validations.store(2, Ordering::Relaxed);
        session
            .last_validation_at
            .insert(dep_uri.clone(), Instant::now());
        session.validation_dependencies_by_root.insert(
            main_uri.clone(),
            HashSet::from([main_uri.clone(), dep_uri.clone()]),
        );
        session.diagnostic_contributions_by_root.insert(
            main_uri.clone(),
            HashMap::from([(
                dep_uri.clone(),
                vec![ValidationDiagnostic {
                    code: "EASM".to_string(),
                    severity: "error".to_string(),
                    message: "stale dependency diagnostic".to_string(),
                    file: Some(dep.to_string_lossy().to_string()),
                    line: 1,
                    col_start: Some(7),
                    col_end: Some(14),
                    fixits: Vec::new(),
                }],
            )]),
        );

        let out = session.handle_message(&json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didChange",
            "params": {
                "textDocument": {"uri": dep_uri.clone(), "version": 2},
                "contentChanges": [{"text": ".byte 1\n"}]
            }
        }));

        assert!(!session
            .diagnostic_contributions_by_root
            .contains_key(&main_uri));
        assert!(!session
            .validation_dependencies_by_root
            .contains_key(&main_uri));
        assert!(session.pending_validation_uris.contains(&main_uri));
        assert!(out.iter().any(|message| matches!(
            message,
            OutboundMessage::Notification { method, params }
                if method == "textDocument/publishDiagnostics"
                    && params.get("uri").and_then(Value::as_str) == Some(dep_uri.as_str())
                    && params
                        .get("diagnostics")
                        .and_then(Value::as_array)
                        .is_some_and(|diagnostics| diagnostics.is_empty())
        )));
        let _ = fs::remove_dir_all(temp_dir);
    }
}
