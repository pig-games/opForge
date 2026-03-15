// SPDX-License-Identifier: GPL-3.0-or-later

//! Runtime-model bootstrap helpers for assembler-side VM integration.

use std::fs;
use std::path::Path;
#[cfg(feature = "vm-runtime-opasm-artifact")]
use std::path::PathBuf;

use registry::cpu::CpuType;
use registry::registry::ModuleRegistry;
#[cfg(not(feature = "vm-runtime-opasm-unbundled"))]
use vm::builder::build_hierarchy_package_from_registry;
use vm::vm_opasm::HierarchyExecutionModel;

#[cfg(feature = "vm-runtime-opasm-artifact")]
pub const VM_RUNTIME_PACKAGE_ARTIFACT_RELATIVE_PATH: &str = "target/vm/opforge-vm-runtime.opasm";

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
    #[cfg(feature = "vm-runtime-only")]
    let _ = cpu;

    #[cfg(not(feature = "vm-runtime-only"))]
    let has_host_pipeline = registry.resolve_pipeline(cpu, None).is_ok();

    if let Some(path) = opasm_package_path {
        if let Some(model) = load_execution_model_from_path(path) {
            return Some(model);
        }
        return None;
    }

    #[cfg(feature = "vm-runtime-opasm-artifact")]
    {
        if let Some(path) = runtime_package_artifact_path() {
            if let Some(model) = load_execution_model_from_path(path.as_path()) {
                return Some(model);
            }
            #[cfg(not(feature = "vm-runtime-opasm-unbundled"))]
            #[cfg(not(feature = "vm-runtime-only"))]
            if has_host_pipeline {
                if let Ok(package_bytes) = build_hierarchy_package_from_registry(registry) {
                    if let Ok(model) =
                        vm::vm_opasm::load_model_from_package_bytes(package_bytes.as_slice())
                    {
                        persist_runtime_package_artifact(path.as_path(), &package_bytes);
                        return Some(model);
                    }
                }
            }
            return None;
        }
    }

    #[cfg(not(feature = "vm-runtime-opasm-unbundled"))]
    {
        #[cfg(not(feature = "vm-runtime-only"))]
        {
            if !has_host_pipeline {
                return None;
            }

            let package_bytes = build_hierarchy_package_from_registry(registry).ok()?;
            vm::vm_opasm::load_model_from_package_bytes(package_bytes.as_slice()).ok()
        }

        #[cfg(feature = "vm-runtime-only")]
        {
            let package_bytes = build_hierarchy_package_from_registry(registry).ok()?;
            vm::vm_opasm::load_model_from_package_bytes(package_bytes.as_slice()).ok()
        }
    }

    #[cfg(all(feature = "vm-runtime-opasm-unbundled", feature = "vm-runtime-only"))]
    {
        let _ = registry;
        None
    }

    #[cfg(all(
        feature = "vm-runtime-opasm-unbundled",
        not(feature = "vm-runtime-only")
    ))]
    {
        let _ = (registry, cpu);
        None
    }
}

#[cfg(feature = "vm-runtime-opasm-artifact")]
pub fn runtime_package_artifact_path_for_dir(base_dir: &Path) -> PathBuf {
    base_dir.join(VM_RUNTIME_PACKAGE_ARTIFACT_RELATIVE_PATH)
}

#[cfg(feature = "vm-runtime-opasm-artifact")]
fn runtime_package_artifact_path() -> Option<PathBuf> {
    std::env::current_dir()
        .ok()
        .map(|base_dir| runtime_package_artifact_path_for_dir(base_dir.as_path()))
}

pub fn load_execution_model_from_path(path: &Path) -> Option<HierarchyExecutionModel> {
    let bytes = fs::read(path).ok()?;
    vm::vm_opasm::load_model_from_package_bytes(bytes.as_slice()).ok()
}

#[cfg(feature = "vm-runtime-opasm-artifact")]
#[cfg(not(feature = "vm-runtime-only"))]
fn persist_runtime_package_artifact(path: &Path, package_bytes: &[u8]) {
    if let Some(parent) = path.parent() {
        let _ = fs::create_dir_all(parent);
    }
    let _ = fs::write(path, package_bytes);
}
