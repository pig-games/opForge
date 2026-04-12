// SPDX-License-Identifier: GPL-3.0-or-later

//! Shared `.opasm` runtime-model bootstrap helpers used by `asm` and `engine`.

use crate::vm_opasm::{self, HierarchyExecutionModel};
use std::fs;
use std::path::{Path, PathBuf};

pub const VM_RUNTIME_PACKAGE_ARTIFACT_RELATIVE_PATH: &str = "target/vm/opforge-vm-runtime.opasm";

pub fn runtime_package_artifact_path_for_dir(base_dir: &Path) -> PathBuf {
    base_dir.join(VM_RUNTIME_PACKAGE_ARTIFACT_RELATIVE_PATH)
}

pub fn runtime_package_artifact_path_from_cwd() -> Option<PathBuf> {
    std::env::current_dir()
        .ok()
        .map(|base_dir| runtime_package_artifact_path_for_dir(base_dir.as_path()))
}

pub fn load_execution_model_from_path(path: &Path) -> Option<HierarchyExecutionModel> {
    let bytes = fs::read(path).ok()?;
    load_execution_model_from_package_bytes(bytes.as_slice())
}

pub fn load_execution_model_from_package_bytes(
    package_bytes: &[u8],
) -> Option<HierarchyExecutionModel> {
    vm_opasm::load_model_from_package_bytes(package_bytes).ok()
}

pub fn persist_runtime_package_artifact(path: &Path, package_bytes: &[u8]) {
    if let Some(parent) = path.parent() {
        let _ = fs::create_dir_all(parent);
    }
    let _ = fs::write(path, package_bytes);
}

pub fn bootstrap_execution_model(
    artifact_path: Option<&Path>,
    fallback_package_bytes: Option<&[u8]>,
    persist_fallback_artifact: bool,
) -> Option<HierarchyExecutionModel> {
    if let Some(path) = artifact_path {
        if let Some(model) = load_execution_model_from_path(path) {
            return Some(model);
        }
        if let Some(package_bytes) = fallback_package_bytes {
            let model = load_execution_model_from_package_bytes(package_bytes)?;
            if persist_fallback_artifact {
                persist_runtime_package_artifact(path, package_bytes);
            }
            return Some(model);
        }
        return None;
    }

    load_execution_model_from_package_bytes(fallback_package_bytes?)
}

pub fn bootstrap_execution_model_for_request(
    explicit_package_path: Option<&Path>,
    cwd_artifact_path: Option<&Path>,
    fallback_package_bytes: Option<&[u8]>,
    persist_fallback_artifact: bool,
) -> Option<HierarchyExecutionModel> {
    if let Some(path) = explicit_package_path {
        return bootstrap_execution_model(Some(path), None, false);
    }

    if let Some(path) = cwd_artifact_path {
        return bootstrap_execution_model(
            Some(path),
            fallback_package_bytes,
            persist_fallback_artifact,
        );
    }

    bootstrap_execution_model(None, fallback_package_bytes, false)
}
