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
    #[cfg(feature = "vm-runtime-only")]
    let _ = cpu;

    #[cfg(not(feature = "vm-runtime-only"))]
    let has_host_pipeline = registry.resolve_pipeline(cpu, None).is_ok();

    if let Some(path) = opasm_package_path {
        return runtime_bootstrap::bootstrap_execution_model(Some(path), None, false);
    }

    #[cfg(feature = "vm-runtime-opasm-artifact")]
    {
        if let Some(path) = runtime_package_artifact_path() {
            #[cfg(all(
                not(feature = "vm-runtime-opasm-unbundled"),
                not(feature = "vm-runtime-only")
            ))]
            let fallback_package_bytes = if has_host_pipeline {
                build_hierarchy_package_from_registry(registry).ok()
            } else {
                None
            };

            #[cfg(not(all(
                not(feature = "vm-runtime-opasm-unbundled"),
                not(feature = "vm-runtime-only")
            )))]
            let fallback_package_bytes: Option<Vec<u8>> = None;

            return runtime_bootstrap::bootstrap_execution_model(
                Some(path.as_path()),
                fallback_package_bytes.as_deref(),
                fallback_package_bytes.is_some(),
            );
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
            runtime_bootstrap::bootstrap_execution_model(
                None,
                Some(package_bytes.as_slice()),
                false,
            )
        }

        #[cfg(feature = "vm-runtime-only")]
        {
            let package_bytes = build_hierarchy_package_from_registry(registry).ok()?;
            runtime_bootstrap::bootstrap_execution_model(
                None,
                Some(package_bytes.as_slice()),
                false,
            )
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
    runtime_bootstrap::runtime_package_artifact_path_for_dir(base_dir)
}

#[cfg(feature = "vm-runtime-opasm-artifact")]
fn runtime_package_artifact_path() -> Option<PathBuf> {
    runtime_bootstrap::runtime_package_artifact_path_from_cwd()
}

pub fn load_execution_model_from_path(path: &Path) -> Option<HierarchyExecutionModel> {
    runtime_bootstrap::load_execution_model_from_path(path)
}
