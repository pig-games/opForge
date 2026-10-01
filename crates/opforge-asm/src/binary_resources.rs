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
    cache: HashMap<PathBuf, Arc<Vec<u8>>>,
    resolved: HashMap<(u32, String), Arc<Vec<u8>>>,
}
impl BinaryResources {
    pub(crate) fn new(source_map: SourceMap) -> Self {
        Self {
            source_map,
            cache: HashMap::new(),
            resolved: HashMap::new(),
        }
    }
    pub(crate) fn dependencies(&self) -> Vec<PathBuf> {
        self.cache.keys().cloned().collect()
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
