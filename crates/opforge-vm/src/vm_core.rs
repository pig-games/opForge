// SPDX-License-Identifier: GPL-3.0-or-later

//! Shared VM substrate used by both `.opcore` and `.opasm` processor domains.
//!
//! This is the first explicit partition surface for VM code that is not owned
//! by either processor-specific VM head.

use package::HierarchyChunks;
use registry::registry::ModuleRegistry;

pub use crate::bytecode;
pub use crate::execution_model::HierarchyExecutionModel;
pub use crate::hierarchy;
pub use crate::portable_contract;
pub use crate::rewrite;
pub use crate::rollout;
pub use crate::runtime_bridge;
pub use crate::runtime_contract_types;
pub use crate::runtime_diagnostics;
pub use crate::runtime_error;
pub use crate::runtime_model_core;
pub use crate::runtime_model_core::{RuntimeModelCore, RuntimeModelLoadError};
pub use crate::runtime_model_types;
pub use crate::runtime_parse_utils;
pub use crate::runtime_portable_types;

/// Shared VM-core model loader from assembler registry state.
pub fn load_execution_model_from_registry(
    registry: &ModuleRegistry,
) -> Result<HierarchyExecutionModel, RuntimeModelLoadError> {
    Ok(HierarchyExecutionModel::from_runtime_model_core(
        RuntimeModelCore::from_registry(registry)?,
    ))
}

/// Shared VM-core model loader from serialized `.opasm`/hierarchy package
/// bytes.
pub fn load_execution_model_from_package_bytes(
    bytes: &[u8],
) -> Result<HierarchyExecutionModel, RuntimeModelLoadError> {
    Ok(HierarchyExecutionModel::from_runtime_model_core(
        RuntimeModelCore::from_package_bytes(bytes)?,
    ))
}

/// Shared VM-core model loader from decoded hierarchy chunks.
pub fn load_execution_model_from_chunks(
    chunks: HierarchyChunks,
) -> Result<HierarchyExecutionModel, RuntimeModelLoadError> {
    Ok(HierarchyExecutionModel::from_runtime_model_core(
        RuntimeModelCore::from_chunks(chunks)?,
    ))
}
