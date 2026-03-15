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
                normalized.pop();
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

impl MemorySourceProvider {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_file(mut self, path: impl Into<PathBuf>, content: impl Into<String>) -> Self {
        self.insert_file(path, content);
        self
    }

    pub fn insert_file(&mut self, path: impl Into<PathBuf>, content: impl Into<String>) {
        self.files.insert(path.into(), content.into());
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

    pub fn text(&self, path: impl Into<PathBuf>) -> Option<String> {
        self.files
            .lock()
            .expect("files lock")
            .get(&path.into())
            .map(|bytes| String::from_utf8(bytes.clone()).expect("utf8 output"))
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
