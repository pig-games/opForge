// SPDX-License-Identifier: GPL-3.0-or-later

use std::fs;
use std::io::{self, Write};
use std::path::Component;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use std::collections::{HashMap, HashSet};

pub trait SourceProvider: Send + Sync {
    fn read_string(&self, path: &Path) -> io::Result<String>;
    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>>;
    fn is_dir(&self, path: &Path) -> io::Result<bool>;
    fn is_file(&self, path: &Path) -> io::Result<bool>;
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf>;
}

pub trait OutputSink: Send + Sync {
    fn create_dir_all(&self, path: &Path) -> io::Result<()>;
    fn create_file(&self, path: &Path) -> io::Result<Box<dyn Write>>;
    fn write_text(&self, path: &Path, content: &str) -> io::Result<()>;
    fn write_bytes(&self, path: &Path, bytes: &[u8]) -> io::Result<()>;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct FsSourceProvider;

impl SourceProvider for FsSourceProvider {
    fn read_string(&self, path: &Path) -> io::Result<String> {
        fs::read_to_string(path)
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let mut entries = Vec::new();
        for entry in fs::read_dir(path)? {
            entries.push(entry?.path());
        }
        entries.sort();
        Ok(entries)
    }

    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        Ok(fs::metadata(path)?.is_dir())
    }

    fn is_file(&self, path: &Path) -> io::Result<bool> {
        Ok(fs::metadata(path)?.is_file())
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        fs::canonicalize(path)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct FsOutputSink;

impl OutputSink for FsOutputSink {
    fn create_dir_all(&self, path: &Path) -> io::Result<()> {
        fs::create_dir_all(path)
    }

    fn create_file(&self, path: &Path) -> io::Result<Box<dyn Write>> {
        Ok(Box::new(fs::File::create(path)?))
    }

    fn write_text(&self, path: &Path, content: &str) -> io::Result<()> {
        fs::write(path, content)
    }

    fn write_bytes(&self, path: &Path, bytes: &[u8]) -> io::Result<()> {
        fs::write(path, bytes)
    }
}

fn normalize_path(path: &Path) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Prefix(prefix) => normalized.push(prefix.as_os_str()),
            Component::RootDir => normalized.push(Path::new("/")),
            Component::CurDir => {}
            Component::ParentDir => {
                if path.is_absolute() {
                    if matches!(
                        normalized.components().next_back(),
                        Some(Component::Normal(_))
                    ) {
                        normalized.pop();
                    }
                } else {
                    match normalized.components().next_back() {
                        Some(Component::Normal(_)) => {
                            normalized.pop();
                        }
                        Some(Component::ParentDir) | None => normalized.push(".."),
                        Some(Component::RootDir) | Some(Component::Prefix(_)) => {}
                        Some(Component::CurDir) => {}
                    }
                }
            }
            Component::Normal(part) => normalized.push(part),
        }
    }
    if normalized.as_os_str().is_empty() && path.is_absolute() {
        PathBuf::from("/")
    } else {
        normalized
    }
}

#[derive(Debug, Default, Clone)]
pub struct MemorySourceProvider {
    files: HashMap<PathBuf, String>,
}

#[derive(Debug, Clone)]
struct MemoryFsOverlaySourceProvider {
    memory: MemorySourceProvider,
    fallback: FsSourceProvider,
}

impl MemorySourceProvider {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_file(mut self, path: impl Into<PathBuf>, content: impl Into<String>) -> Self {
        self.insert_file(path, content);
        self
    }

    pub fn insert_file(&mut self, path: impl Into<PathBuf>, content: impl Into<String>) {
        let path = normalize_path(&path.into());
        self.files.insert(path, content.into());
    }

    pub fn with_fs_fallback(self) -> Box<dyn SourceProvider> {
        Box::new(MemoryFsOverlaySourceProvider {
            memory: self,
            fallback: FsSourceProvider,
        })
    }
}

impl MemoryFsOverlaySourceProvider {
    fn merged_dir_entries(
        &self,
        path: &Path,
        memory_entries: io::Result<Vec<PathBuf>>,
        fallback_entries: io::Result<Vec<PathBuf>>,
    ) -> io::Result<Vec<PathBuf>> {
        match (memory_entries, fallback_entries) {
            (Ok(mut memory_entries), Ok(fallback_entries)) => {
                for entry in fallback_entries {
                    if !memory_entries.contains(&entry) {
                        memory_entries.push(entry);
                    }
                }
                memory_entries.sort();
                Ok(memory_entries)
            }
            (Ok(memory_entries), Err(err)) if err.kind() == io::ErrorKind::NotFound => {
                Ok(memory_entries)
            }
            (Err(err), Ok(fallback_entries)) if err.kind() == io::ErrorKind::NotFound => {
                Ok(fallback_entries)
            }
            (Err(memory_err), Err(fallback_err))
                if memory_err.kind() == io::ErrorKind::NotFound
                    && fallback_err.kind() == io::ErrorKind::NotFound =>
            {
                Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    format!("missing directory {}", path.display()),
                ))
            }
            (Err(err), _) => Err(err),
            (_, Err(err)) => Err(err),
        }
    }

    fn memory_contains_path(&self, path: &Path) -> io::Result<bool> {
        Ok(self.memory.is_file(path)? || self.memory.is_dir(path)?)
    }
}

impl SourceProvider for MemoryFsOverlaySourceProvider {
    fn read_string(&self, path: &Path) -> io::Result<String> {
        match self.memory.read_string(path) {
            Ok(contents) => Ok(contents),
            Err(err) if err.kind() == io::ErrorKind::NotFound => self.fallback.read_string(path),
            Err(err) => Err(err),
        }
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        self.merged_dir_entries(
            path,
            self.memory.read_dir(path),
            self.fallback.read_dir(path),
        )
    }

    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        if self.memory.is_dir(path)? {
            return Ok(true);
        }
        match self.fallback.is_dir(path) {
            Ok(is_dir) => Ok(is_dir),
            Err(err) if err.kind() == io::ErrorKind::NotFound => Ok(false),
            Err(err) => Err(err),
        }
    }

    fn is_file(&self, path: &Path) -> io::Result<bool> {
        if self.memory.is_file(path)? {
            return Ok(true);
        }
        match self.fallback.is_file(path) {
            Ok(is_file) => Ok(is_file),
            Err(err) if err.kind() == io::ErrorKind::NotFound => Ok(false),
            Err(err) => Err(err),
        }
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        if self.memory_contains_path(path)? {
            self.memory.canonicalize(path)
        } else {
            self.fallback.canonicalize(path)
        }
    }
}

impl SourceProvider for MemorySourceProvider {
    fn read_string(&self, path: &Path) -> io::Result<String> {
        self.files
            .get(&normalize_path(path))
            .cloned()
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "missing file"))
    }

    fn read_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let root = normalize_path(path);
        let mut entries = HashSet::new();
        for file in self.files.keys() {
            if let Ok(relative) = file.strip_prefix(&root) {
                if relative.as_os_str().is_empty() {
                    continue;
                }
                if let Some(first) = relative.components().next() {
                    match first {
                        Component::Normal(name) => {
                            entries.insert(root.join(name));
                        }
                        Component::CurDir => {}
                        _ => {}
                    }
                }
            }
        }
        let mut entries: Vec<PathBuf> = entries.into_iter().collect();
        entries.sort();
        Ok(entries)
    }

    fn is_dir(&self, path: &Path) -> io::Result<bool> {
        let root = normalize_path(path);
        Ok(self.files.keys().any(|file| {
            file != &root
                && file
                    .strip_prefix(&root)
                    .map(|relative| !relative.as_os_str().is_empty())
                    .unwrap_or(false)
        }))
    }

    fn is_file(&self, path: &Path) -> io::Result<bool> {
        Ok(self.files.contains_key(&normalize_path(path)))
    }

    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf> {
        Ok(normalize_path(path))
    }
}

#[derive(Debug, Default, Clone)]
pub struct MemoryOutputSink {
    files: Arc<Mutex<HashMap<PathBuf, Vec<u8>>>>,
    dirs: Arc<Mutex<HashSet<PathBuf>>>,
}

impl MemoryOutputSink {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn directories(&self) -> Vec<PathBuf> {
        let mut dirs: Vec<PathBuf> = self
            .dirs
            .lock()
            .expect("dirs lock")
            .iter()
            .cloned()
            .collect();
        dirs.sort();
        dirs
    }

    pub fn files(&self) -> Vec<(PathBuf, Vec<u8>)> {
        let mut files: Vec<(PathBuf, Vec<u8>)> = self
            .files
            .lock()
            .expect("files lock")
            .iter()
            .map(|(path, bytes)| (path.clone(), bytes.clone()))
            .collect();
        files.sort_by(|left, right| left.0.cmp(&right.0));
        files
    }

    pub fn text(
        &self,
        path: impl Into<PathBuf>,
    ) -> Result<Option<String>, std::string::FromUtf8Error> {
        self.files
            .lock()
            .expect("files lock")
            .get(&path.into())
            .map(|bytes| String::from_utf8(bytes.clone()))
            .transpose()
    }

    pub fn bytes(&self, path: impl Into<PathBuf>) -> Option<Vec<u8>> {
        self.files
            .lock()
            .expect("files lock")
            .get(&path.into())
            .cloned()
    }
}

#[derive(Debug)]
struct MemoryOutputWriter {
    path: PathBuf,
    files: Arc<Mutex<HashMap<PathBuf, Vec<u8>>>>,
    buffer: Vec<u8>,
}

impl Write for MemoryOutputWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.buffer.extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl Drop for MemoryOutputWriter {
    fn drop(&mut self) {
        self.files
            .lock()
            .expect("files lock")
            .insert(self.path.clone(), self.buffer.clone());
    }
}

impl OutputSink for MemoryOutputSink {
    fn create_dir_all(&self, path: &Path) -> io::Result<()> {
        self.dirs
            .lock()
            .expect("dirs lock")
            .insert(PathBuf::from(path));
        Ok(())
    }

    fn create_file(&self, path: &Path) -> io::Result<Box<dyn Write>> {
        Ok(Box::new(MemoryOutputWriter {
            path: PathBuf::from(path),
            files: Arc::clone(&self.files),
            buffer: Vec::new(),
        }))
    }

    fn write_text(&self, path: &Path, content: &str) -> io::Result<()> {
        self.files
            .lock()
            .expect("files lock")
            .insert(PathBuf::from(path), content.as_bytes().to_vec());
        Ok(())
    }

    fn write_bytes(&self, path: &Path, bytes: &[u8]) -> io::Result<()> {
        self.files
            .lock()
            .expect("files lock")
            .insert(PathBuf::from(path), bytes.to_vec());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{
        normalize_path, MemoryOutputSink, MemorySourceProvider, OutputSink, SourceProvider,
    };
    use std::fs;
    use std::io::ErrorKind;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    fn make_temp_dir(name: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock before epoch")
            .as_nanos();
        let path = std::env::temp_dir().join(format!(
            "libopforge-engine-io-{name}-{}-{nanos}",
            std::process::id()
        ));
        fs::create_dir_all(&path).expect("create temp dir");
        path
    }

    #[test]
    fn normalize_path_preserves_leading_parent_segments_for_relative_paths() {
        assert_eq!(
            normalize_path(Path::new("../inc.asm")),
            PathBuf::from("../inc.asm")
        );
        assert_eq!(
            normalize_path(Path::new("a/../../inc.asm")),
            PathBuf::from("../inc.asm")
        );
        assert_eq!(
            normalize_path(Path::new("../../dir/./file.asm")),
            PathBuf::from("../../dir/file.asm")
        );
    }

    #[test]
    fn memory_source_provider_normalizes_inserted_and_lookup_paths_consistently() {
        let mut provider = MemorySourceProvider::new();
        provider.insert_file("./virtual/../main.asm", ".module main\n.endmodule\n");

        let content = provider
            .read_string(Path::new("main.asm"))
            .expect("normalized lookup should succeed");
        assert!(content.contains(".module main"));
        assert!(provider.is_file(Path::new("./main.asm")).expect("is_file"));
        assert_eq!(
            provider
                .canonicalize(Path::new("./virtual/../main.asm"))
                .expect("canonicalize"),
            PathBuf::from("main.asm")
        );
    }

    #[test]
    fn memory_source_provider_does_not_alias_leading_parent_paths_to_local_files() {
        let provider =
            MemorySourceProvider::new().with_file("../inc.asm", "FROM_PARENT .const 1\n");

        let err = provider
            .read_string(Path::new("inc.asm"))
            .expect_err("local file should not alias to parent path");
        assert_eq!(err.kind(), ErrorKind::NotFound);

        let content = provider
            .read_string(Path::new("../inc.asm"))
            .expect("parent-relative path should still resolve");
        assert!(content.contains("FROM_PARENT"));
    }

    #[test]
    fn memory_source_provider_with_fs_fallback_reads_missing_files_from_filesystem() {
        let temp_dir = make_temp_dir("fs-fallback");
        let include_path = temp_dir.join("inc.asm");
        fs::write(&include_path, "FROM_FS .const 1\n").expect("write include file");

        let provider = MemorySourceProvider::new()
            .with_file("/virtual/main.asm", ".module main\n.endmodule\n")
            .with_fs_fallback();

        let content = provider
            .read_string(&include_path)
            .expect("filesystem fallback should resolve missing file");
        assert!(content.contains("FROM_FS"));
    }

    #[test]
    fn memory_output_sink_text_returns_utf8_error_for_binary_artifacts() {
        let sink = MemoryOutputSink::new();
        sink.write_bytes(Path::new("/virtual/out.bin"), &[0xff, 0x00, 0x41])
            .expect("write binary output");

        let err = sink
            .text("/virtual/out.bin")
            .expect_err("binary output should not decode as utf8");
        assert_eq!(err.utf8_error().valid_up_to(), 0);

        let bytes = sink
            .bytes("/virtual/out.bin")
            .expect("raw bytes should remain available");
        assert_eq!(bytes, vec![0xff, 0x00, 0x41]);

        assert_eq!(sink.text("/virtual/missing.bin"), Ok(None));
    }
}
