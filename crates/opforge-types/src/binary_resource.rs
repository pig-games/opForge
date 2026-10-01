// SPDX-License-Identifier: GPL-3.0-or-later
//! Owned source-provider capability for active binary-file directives.
use std::{
    fmt::Debug,
    io,
    path::{Path, PathBuf},
};
pub trait BinaryResourceReader: Debug + Send + Sync {
    fn read_bytes(&self, path: &Path) -> io::Result<Vec<u8>>;
    fn is_file(&self, path: &Path) -> bool;
    fn canonicalize(&self, path: &Path) -> io::Result<PathBuf>;
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryResourceContext {
    pub base_dir: PathBuf,
    pub allowed_roots: Vec<PathBuf>,
}
