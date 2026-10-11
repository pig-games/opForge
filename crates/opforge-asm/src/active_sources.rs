// SPDX-License-Identifier: GPL-3.0-or-later
//! Text acquisition reached exclusively from active statement traversal.
use crate::error::{AsmError, AsmErrorKind, Diagnostic, Severity};
use crate::{line::AsmLine, preprocess::AsmMacroProcessor};
use opcore::preprocess::{PreprocessFileLoader, Preprocessor};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use types::{binary_resource::BinaryResourceReader, source_map::SourceOrigin};

pub(crate) struct RuntimeSource {
    pub(crate) lines: Vec<String>,
    pub(crate) first_line: u32,
}

pub(crate) enum SourceAcquisitionError {
    Message(String),
    Diagnostic(Box<Diagnostic>),
}

impl From<String> for SourceAcquisitionError {
    fn from(message: String) -> Self {
        Self::Message(message)
    }
}

impl From<&str> for SourceAcquisitionError {
    fn from(message: &str) -> Self {
        Self::Message(message.to_string())
    }
}

#[derive(Default)]
pub(crate) struct ActiveSources {
    processors: HashMap<String, AsmMacroProcessor>,
    seeds: HashMap<String, opcore::macro_processor::MacroProcessor>,
    current_module: String,
    started: std::collections::HashSet<String>,
    pub(crate) include_stack: Vec<PathBuf>,
    pub(crate) expansion_depth: usize,
    pub(crate) activity_failed: bool,
    pub(crate) captured_definitions: std::collections::HashSet<u32>,
}

impl ActiveSources {
    pub(crate) fn set_seeds(
        &mut self,
        seeds: HashMap<String, opcore::macro_processor::MacroProcessor>,
    ) {
        self.seeds = seeds;
    }
    pub(crate) fn enabled(&self) -> bool {
        self.seeds.contains_key(&self.current_module)
            || self.processors.contains_key(&self.current_module)
    }
    pub(crate) fn configured(&self) -> bool {
        !self.seeds.is_empty() || !self.processors.is_empty()
    }
}

#[derive(Debug)]
struct AcquiredFile {
    reader: Arc<dyn BinaryResourceReader>,
    path: PathBuf,
    text: String,
}
impl PreprocessFileLoader for AcquiredFile {
    fn read_to_string(&self, path: &Path) -> std::io::Result<String> {
        if path == self.path {
            Ok(self.text.clone())
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::PermissionDenied,
                "Nested source acquisition must use active traversal",
            ))
        }
    }
    fn read_bytes(&self, path: &Path) -> std::io::Result<Vec<u8>> {
        self.reader.read_bytes(path)
    }
    fn is_file(&self, path: &Path) -> bool {
        self.reader.is_file(path)
    }
    fn canonicalize(&self, path: &Path) -> std::io::Result<PathBuf> {
        self.reader.canonicalize(path)
    }
}

impl AsmLine<'_> {
    pub(crate) fn acquire_include(
        &mut self,
        line: u32,
        filename: &str,
    ) -> Result<(PathBuf, Arc<RuntimeSource>), SourceAcquisitionError> {
        let resources = self
            .source_resources()
            .ok_or("INCLUDE source resource context unavailable")?;
        let origin = resources
            .borrow()
            .origin(line)
            .ok_or("INCLUDE source origin unavailable")?;
        let context = origin
            .binary_context
            .as_ref()
            .ok_or("INCLUDE source resource context unavailable")?;
        let reader = resources.borrow().include_reader()?;
        let (max_depth, defines) = resources.borrow().source_config();
        let target = Path::new(filename);
        if filename.is_empty() || filename.contains('\0') {
            return Err("INCLUDE requires a nonempty filename without NUL".into());
        }
        let candidates: Vec<_> = if target.is_absolute() {
            vec![target.to_path_buf()]
        } else {
            std::iter::once(context.base_dir.join(target))
                .chain(context.allowed_roots.iter().map(|root| root.join(target)))
                .collect()
        };
        let path = candidates
            .iter()
            .find_map(|candidate| {
                if !reader.is_file(candidate) {
                    return None;
                }
                let canonical = reader.canonicalize(candidate).ok()?;
                context
                    .allowed_roots
                    .iter()
                    .any(|root| {
                        reader
                            .canonicalize(root)
                            .is_ok_and(|root| canonical.starts_with(root))
                    })
                    .then_some(canonical)
            })
            .ok_or_else(|| {
                format!(
                    "INCLUDE file not found: {filename} (searched: {})",
                    candidates
                        .iter()
                        .map(|p| p.display().to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            })?;
        let root = origin
            .file
            .as_deref()
            .and_then(|file| reader.canonicalize(Path::new(file)).ok());
        if self.active_sources.include_stack.contains(&path) || root.as_ref() == Some(&path) {
            let mut chain: Vec<_> = self
                .active_sources
                .include_stack
                .iter()
                .map(|p| p.display().to_string())
                .collect();
            if chain.is_empty() {
                if let Some(root) = root {
                    chain.push(root.display().to_string());
                }
            }
            chain.push(path.display().to_string());
            return Err(format!("INCLUDE cycle detected: {}", chain.join(" -> ")).into());
        }
        let parent_depth = self.active_sources.include_stack.len().max(1);
        if parent_depth >= max_depth {
            return Err(format!("INCLUDE nesting exceeded maximum depth ({max_depth})").into());
        }
        let bytes = resources.borrow_mut().include_bytes(&path)?;
        let text = String::from_utf8(bytes.as_ref().clone())
            .map_err(|_| format!("INCLUDE source is not UTF-8: {}", path.display()))?;
        let mut pp = Preprocessor::with_max_depth(max_depth);
        pp.defer_all_includes();
        for root in &context.allowed_roots {
            pp.add_include_root(root.clone());
        }
        for define in &defines {
            let (name, value) = define.split_once('=').unwrap_or((define, "1"));
            pp.define(name, value);
        }
        let loader = AcquiredFile {
            reader,
            path: path.clone(),
            text,
        };
        pp.process_file_with_loader(&path.to_string_lossy(), &loader)
            .map_err(|err| {
                let mut diagnostic = Diagnostic::new(
                    err.line().unwrap_or(1),
                    Severity::Error,
                    AsmError::new(AsmErrorKind::Preprocess, err.message(), None),
                )
                .with_column(err.column())
                .with_source(err.source().map(str::to_string));
                diagnostic.file = Some(err.file().unwrap_or(&path.to_string_lossy()).to_string());
                SourceAcquisitionError::Diagnostic(Box::new(diagnostic))
            })?;
        self.begin_source_execution();
        let lines = pp.lines().to_vec();
        let origins = pp.origins().to_vec();
        let key = (
            line,
            format!("include:{}\n{}", path.display(), lines.join("\n")),
        );
        let source = resources.borrow_mut().intern_source(key, lines, origins);
        Ok((path, source))
    }

    fn expand_acquired_lines(
        &mut self,
        lines: &[String],
        origins: &[SourceOrigin],
    ) -> Result<(Vec<String>, Vec<SourceOrigin>), String> {
        let module = self.active_sources.current_module.clone();
        let max_depth = self
            .source_resources()
            .map(|resources| resources.borrow().source_config().0)
            .unwrap_or(64);
        let processor = self
            .active_sources
            .processors
            .entry(module)
            .or_insert_with(|| AsmMacroProcessor::for_source_execution(max_depth));
        processor
            .expand_with_origins(lines, origins)
            .map_err(|err| err.message().to_string())
    }

    pub(crate) fn begin_source_execution(&mut self) {
        if !self
            .active_sources
            .started
            .insert(self.active_sources.current_module.clone())
        {
            return;
        }
        let module = self.active_sources.current_module.clone();
        let max_depth = self
            .source_resources()
            .map(|resources| resources.borrow().source_config().0)
            .unwrap_or(64);
        let processor = self
            .active_sources
            .processors
            .entry(module.clone())
            .or_insert_with(|| AsmMacroProcessor::for_source_execution(max_depth));
        if let Some(seed) = self.active_sources.seeds.get(&module) {
            processor.seed_templates_from(seed);
        }
    }

    pub(crate) fn expand_active_invocation(
        &mut self,
        line: u32,
        source: &str,
    ) -> Result<Option<Arc<RuntimeSource>>, String> {
        if self.active_sources.seeds.is_empty() && self.active_sources.processors.is_empty() {
            return Ok(None);
        }
        let Some(resources) = self.source_resources() else {
            return Ok(None);
        };
        let origin = resources
            .borrow()
            .origin(line)
            .unwrap_or_else(|| SourceOrigin::new(None, line));
        let (code, _) = opcore::text_utils::split_comment(source);
        let mut words = code.split_whitespace();
        if words
            .next()
            .is_some_and(|word| word.eq_ignore_ascii_case(".module"))
        {
            self.active_sources.current_module = words.next().unwrap_or("").to_ascii_lowercase();
        }
        let module = &self.active_sources.current_module;
        if !self.active_sources.seeds.contains_key(module)
            && !self.active_sources.processors.contains_key(module)
        {
            return Ok(None);
        }
        let (lines, origins) = self.expand_acquired_lines(&[source.to_string()], &[origin])?;
        if lines.len() == 1 && lines[0] == source {
            return Ok(None);
        }
        let max_depth = resources.borrow().source_config().0;
        if self.active_sources.expansion_depth >= max_depth {
            return Err(format!(
                "Source expansion exceeded maximum depth ({max_depth})"
            ));
        }
        let key = (line, format!("expansion:{}", lines.join("\n")));
        let expanded = resources.borrow_mut().intern_source(key, lines, origins);
        Ok(Some(expanded))
    }

    pub(crate) fn capture_active_definition(
        &mut self,
        lines: &[String],
        first_line: u32,
    ) -> Result<(), String> {
        let resources = self
            .source_resources()
            .ok_or("Source resource context unavailable")?;
        let origins: Vec<_> = (0..lines.len())
            .map(|index| {
                resources
                    .borrow()
                    .origin(first_line + index as u32)
                    .unwrap_or_else(|| SourceOrigin::new(None, first_line + index as u32))
            })
            .collect();
        self.expand_acquired_lines(lines, &origins)?;
        Ok(())
    }
}
