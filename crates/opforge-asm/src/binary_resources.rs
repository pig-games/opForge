// SPDX-License-Identifier: GPL-3.0-or-later
//! Session-owned immutable asset cache, reached only from active shared directives.
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};
use types::source_map::SourceMap;
pub(crate) struct BinaryResources {
    source_map: SourceMap,
    original_line_count: usize,
    cache: HashMap<PathBuf, Arc<Vec<u8>>>,
    resolved: HashMap<(u32, String), Arc<Vec<u8>>>,
    sources: HashMap<
        (u32, String, Vec<types::source_map::SourceOrigin>),
        Arc<crate::active_sources::RuntimeSource>,
    >,
}
impl BinaryResources {
    pub(crate) fn new(source_map: SourceMap) -> Self {
        let original_line_count = source_map.origins().len();
        Self {
            source_map,
            original_line_count,
            cache: HashMap::new(),
            resolved: HashMap::new(),
            sources: HashMap::new(),
        }
    }
    pub(crate) fn dependencies(&self) -> Vec<PathBuf> {
        self.cache.keys().cloned().collect()
    }
    pub(crate) fn acquired_source_map(&self) -> Option<SourceMap> {
        (self.source_map.origins().len() > self.original_line_count)
            .then(|| self.source_map.clone())
    }
    pub(crate) fn is_acquired_line(&self, line: u32) -> bool {
        line as usize > self.original_line_count
    }

    pub(crate) fn origin(&self, line: u32) -> Option<types::source_map::SourceOrigin> {
        self.source_map.origin_for_line(line).cloned()
    }

    pub(crate) fn intern_source(
        &mut self,
        key: (u32, String),
        lines: Vec<String>,
        origins: Vec<types::source_map::SourceOrigin>,
    ) -> Arc<crate::active_sources::RuntimeSource> {
        // Equal emitted text can originate in different template overloads.
        // Keep definition-site authority and diagnostics in the cache identity.
        let key = (key.0, key.1, origins.clone());
        if let Some(source) = self.sources.get(&key) {
            return source.clone();
        }
        let first_line = self.source_map.append_origins(&origins);
        let source = Arc::new(crate::active_sources::RuntimeSource { lines, first_line });
        self.sources.insert(key, source.clone());
        source
    }

    pub(crate) fn include_reader(
        &self,
    ) -> Result<Arc<dyn types::binary_resource::BinaryResourceReader>, String> {
        self.source_map
            .binary_reader
            .clone()
            .ok_or_else(|| "INCLUDE source provider has no owned reader".into())
    }

    pub(crate) fn source_config(&self) -> (usize, Vec<String>) {
        (
            self.source_map.source_max_depth,
            self.source_map.preprocess_defines.clone(),
        )
    }

    pub(crate) fn include_bytes(&mut self, path: &Path) -> Result<Arc<Vec<u8>>, String> {
        if let Some(bytes) = self.cache.get(path) {
            return Ok(bytes.clone());
        }
        let bytes = Arc::new(
            self.include_reader()?
                .read_bytes(path)
                .map_err(|err| format!("Error opening source file {}: {err}", path.display()))?,
        );
        self.cache.insert(path.to_path_buf(), bytes.clone());
        Ok(bytes)
    }
    pub(crate) fn load(&mut self, line: u32, filename: &str) -> Result<Arc<Vec<u8>>, String> {
        if filename.is_empty() || filename.contains('\0') {
            return Err("INCBIN requires a nonempty filename without NUL".into());
        }
        let request = (line, filename.to_string());
        if let Some(bytes) = self.resolved.get(&request) {
            return Ok(bytes.clone());
        }
        let context = self
            .source_map
            .origin_for_line(line)
            .and_then(|origin| origin.binary_context.as_ref())
            .ok_or("INCBIN source resource context unavailable")?;
        let reader = self
            .source_map
            .binary_reader
            .as_ref()
            .ok_or("INCBIN source provider has no owned binary reader")?;
        let path = Path::new(filename);
        let candidates = if path.is_absolute() {
            vec![path.to_path_buf()]
        } else {
            std::iter::once(context.base_dir.join(path))
                .chain(context.allowed_roots.iter().map(|root| root.join(path)))
                .collect()
        };
        for candidate in &candidates {
            if !reader.is_file(candidate) {
                continue;
            }
            let Ok(canonical) = reader.canonicalize(candidate) else {
                continue;
            };
            if !context.allowed_roots.iter().any(|root| {
                reader
                    .canonicalize(root)
                    .is_ok_and(|root| canonical.starts_with(root))
            }) {
                continue;
            }
            if let Some(bytes) = self.cache.get(&canonical) {
                self.resolved.insert(request, bytes.clone());
                return Ok(bytes.clone());
            }
            let bytes = Arc::new(reader.read_bytes(&canonical).map_err(|error| {
                format!("Error opening binary file {}: {error}", canonical.display())
            })?);
            self.cache.insert(canonical, bytes.clone());
            self.resolved.insert(request, bytes.clone());
            return Ok(bytes);
        }
        Err(format!(
            "INCBIN file not found: {filename} (searched: {})",
            candidates
                .iter()
                .map(|path| path.display().to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use types::{
        binary_resource::{BinaryResourceContext, BinaryResourceReader},
        source_map::SourceOrigin,
    };
    #[test]
    fn acquired_source_identity_retains_definition_origins() {
        let mut resources = BinaryResources::new(SourceMap::default());
        let key = (1, "expansion:.byte 7".into());
        let origin = SourceOrigin::new(Some("first.i".into()), 4);
        let first =
            resources.intern_source(key.clone(), vec![".byte 7".into()], vec![origin.clone()]);
        let replay = resources.intern_source(key.clone(), vec![".byte 7".into()], vec![origin]);
        assert!(Arc::ptr_eq(&first, &replay));
        let second = resources.intern_source(
            key,
            vec![".byte 7".into()],
            vec![SourceOrigin::new(Some("second.i".into()), 8)],
        );
        assert_ne!(first.first_line, second.first_line);
        assert_eq!(
            resources.origin(second.first_line).unwrap().file.as_deref(),
            Some("second.i")
        );
    }
    #[derive(Debug)]
    struct Reader {
        reads: Arc<AtomicUsize>,
        stats: Arc<AtomicUsize>,
    }
    impl BinaryResourceReader for Reader {
        fn read_bytes(&self, _: &Path) -> std::io::Result<Vec<u8>> {
            self.reads.fetch_add(1, Ordering::SeqCst);
            Ok(vec![0, 128, 255])
        }
        fn is_file(&self, path: &Path) -> bool {
            self.stats.fetch_add(1, Ordering::SeqCst);
            path == Path::new("/project/data.bin")
        }
        fn canonicalize(&self, path: &Path) -> std::io::Result<PathBuf> {
            self.stats.fetch_add(1, Ordering::SeqCst);
            Ok(path.to_path_buf())
        }
    }
    #[test]
    fn binary_resource_cache_reads_once_and_reports_only_loaded_assets() {
        let reads = Arc::new(AtomicUsize::new(0));
        let stats = Arc::new(AtomicUsize::new(0));
        let mut origin = SourceOrigin::new(Some("/project/main.asm".into()), 1);
        origin.binary_context = Some(BinaryResourceContext {
            base_dir: "/project".into(),
            allowed_roots: vec!["/project".into()],
        });
        let mut map = SourceMap::new(vec![origin.clone(), origin]);
        map.binary_reader = Some(Arc::new(Reader {
            reads: reads.clone(),
            stats: stats.clone(),
        }));
        let mut resources = BinaryResources::new(map);
        assert_eq!(reads.load(Ordering::SeqCst), 0);
        assert!(resources.dependencies().is_empty());
        let first = resources.load(1, "data.bin").unwrap();
        let initial_stats = stats.load(Ordering::SeqCst);
        assert!(Arc::ptr_eq(&first, &resources.load(1, "data.bin").unwrap()));
        assert_eq!(stats.load(Ordering::SeqCst), initial_stats);
        assert!(Arc::ptr_eq(&first, &resources.load(2, "data.bin").unwrap()));
        assert_eq!(reads.load(Ordering::SeqCst), 1);
        assert_eq!(
            resources.dependencies(),
            [PathBuf::from("/project/data.bin")]
        );
        assert!(resources.load(2, "missing.bin").is_err());
        assert_eq!(reads.load(Ordering::SeqCst), 1);
    }
}
