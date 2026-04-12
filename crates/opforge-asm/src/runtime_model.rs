// SPDX-License-Identifier: GPL-3.0-or-later

//! Runtime-model bootstrap helpers for assembler-side VM integration.

use std::path::Path;
#[cfg(feature = "vm-runtime-opasm-artifact")]
use std::path::PathBuf;

use registry::cpu::CpuType;
use registry::registry::ModuleRegistry;
#[cfg(not(feature = "vm-runtime-opasm-unbundled"))]
use vm::builder::build_hierarchy_package_from_registry;
use vm::runtime_bootstrap;
use vm::vm_opasm::HierarchyExecutionModel;

pub fn build_execution_model(
    registry: &ModuleRegistry,
    cpu: CpuType,
) -> Option<HierarchyExecutionModel> {
    build_execution_model_for_request(registry, cpu, None)
}

pub fn build_execution_model_for_request(
    registry: &ModuleRegistry,
    cpu: CpuType,
    opasm_package_path: Option<&Path>,
) -> Option<HierarchyExecutionModel> {
    #[cfg(feature = "vm-runtime-opasm-artifact")]
    let cwd_artifact_path = runtime_package_artifact_path();

    #[cfg(feature = "vm-runtime-opasm-artifact")]
    let cwd_artifact_path = cwd_artifact_path.as_deref();

    #[cfg(not(feature = "vm-runtime-opasm-artifact"))]
    let cwd_artifact_path = None;

    build_execution_model_for_request_with_artifact_path(
        registry,
        cpu,
        opasm_package_path,
        cwd_artifact_path,
    )
}

pub(crate) fn build_execution_model_for_request_with_artifact_path(
    registry: &ModuleRegistry,
    cpu: CpuType,
    opasm_package_path: Option<&Path>,
    cwd_artifact_path: Option<&Path>,
) -> Option<HierarchyExecutionModel> {
    #[cfg(any(feature = "vm-runtime-only", feature = "vm-runtime-opasm-unbundled"))]
    let _ = cpu;

    #[cfg(feature = "vm-runtime-opasm-unbundled")]
    let _ = registry;

    #[cfg(all(
        not(feature = "vm-runtime-opasm-unbundled"),
        not(feature = "vm-runtime-only")
    ))]
    let fallback_package_bytes = if registry.resolve_pipeline(cpu, None).is_ok() {
        build_hierarchy_package_from_registry(registry).ok()
    } else {
        None
    };

    #[cfg(all(
        not(feature = "vm-runtime-opasm-unbundled"),
        feature = "vm-runtime-only"
    ))]
    let fallback_package_bytes = build_hierarchy_package_from_registry(registry).ok();

    #[cfg(feature = "vm-runtime-opasm-unbundled")]
    let fallback_package_bytes: Option<Vec<u8>> = None;

    runtime_bootstrap::bootstrap_execution_model_for_request(
        opasm_package_path,
        cwd_artifact_path,
        fallback_package_bytes.as_deref(),
        fallback_package_bytes.is_some(),
    )
}

#[cfg(feature = "vm-runtime-opasm-artifact")]
pub fn runtime_package_artifact_path_for_dir(base_dir: &Path) -> PathBuf {
    runtime_bootstrap::runtime_package_artifact_path_for_dir(base_dir)
}

#[cfg(feature = "vm-runtime-opasm-artifact")]
fn runtime_package_artifact_path() -> Option<PathBuf> {
    runtime_bootstrap::runtime_package_artifact_path_from_cwd()
}

pub fn load_execution_model_from_path(path: &Path) -> Option<HierarchyExecutionModel> {
    runtime_bootstrap::load_execution_model_from_path(path)
}
