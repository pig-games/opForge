use std::path::{Path, PathBuf};

pub mod vasm;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum OracleAvailability {
    Disabled(String),
    Missing(String),
    Ready,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct OracleAssembleRequest<'a> {
    pub cpu: &'a str,
    pub cpu_profile: Option<&'a str>,
    pub source_path: &'a Path,
    pub output_dir: &'a Path,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct OracleAssembleSuccess {
    pub output_path: PathBuf,
    pub bytes: Vec<u8>,
    pub stdout_path: Option<PathBuf>,
    pub stderr_path: Option<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct OracleAssembleFailure {
    pub diagnostics_path: PathBuf,
    pub stdout_path: Option<PathBuf>,
    pub stderr_path: Option<PathBuf>,
    pub diagnostics_text: String,
    pub summary: String,
}

pub(crate) trait ExternalOracleAdapter {
    fn oracle_id(&self) -> &'static str;
    fn oracle_profile(&self) -> &'static str;
    fn supports_family(&self, family: &str) -> bool;
    fn supports_cpu(&self, cpu: &str) -> bool;
    fn supports_profile(&self, cpu: &str, profile: Option<&str>) -> bool;
    fn availability(&self) -> OracleAvailability;
    fn assemble_flat_binary(
        &self,
        request: OracleAssembleRequest<'_>,
    ) -> Result<OracleAssembleSuccess, OracleAssembleFailure>;
}
