// SPDX-License-Identifier: GPL-3.0-or-later

//! Pure hierarchy-aware target selection bridge used by runtime models.

use types::hierarchy::{
    HierarchyError, HierarchyPackage, ResolvedHierarchy, ResolvedHierarchyContext,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HierarchyRuntimeBridgeError {
    ActiveCpuNotSet,
    Hierarchy(HierarchyError),
}

impl std::fmt::Display for HierarchyRuntimeBridgeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ActiveCpuNotSet => write!(f, "active cpu is not set"),
            Self::Hierarchy(err) => write!(f, "hierarchy resolution error: {}", err),
        }
    }
}

impl std::error::Error for HierarchyRuntimeBridgeError {}

impl From<HierarchyError> for HierarchyRuntimeBridgeError {
    fn from(value: HierarchyError) -> Self {
        Self::Hierarchy(value)
    }
}

/// Small bridge state that mirrors host-side active target selection APIs.
#[derive(Debug)]
pub struct HierarchyRuntimeBridge {
    package: HierarchyPackage,
    active_cpu: Option<String>,
    dialect_override: Option<String>,
}

impl HierarchyRuntimeBridge {
    pub fn new(package: HierarchyPackage) -> Self {
        Self {
            package,
            active_cpu: None,
            dialect_override: None,
        }
    }

    pub fn package(&self) -> &HierarchyPackage {
        &self.package
    }

    pub fn active_cpu(&self) -> Option<&str> {
        self.active_cpu.as_deref()
    }

    pub fn dialect_override(&self) -> Option<&str> {
        self.dialect_override.as_deref()
    }

    pub fn set_active_cpu(&mut self, cpu_id: &str) -> Result<(), HierarchyRuntimeBridgeError> {
        self.package
            .resolve_pipeline(cpu_id, self.dialect_override.as_deref())?;
        self.active_cpu = Some(cpu_id.to_string());
        Ok(())
    }

    pub fn set_dialect_override(
        &mut self,
        dialect_override: Option<&str>,
    ) -> Result<(), HierarchyRuntimeBridgeError> {
        if let Some(cpu_id) = self.active_cpu.as_deref() {
            self.package.resolve_pipeline(cpu_id, dialect_override)?;
        }
        self.dialect_override = dialect_override.map(ToString::to_string);
        Ok(())
    }

    pub fn resolve_pipeline(
        &self,
        cpu_id: &str,
        dialect_override: Option<&str>,
    ) -> Result<ResolvedHierarchy, HierarchyRuntimeBridgeError> {
        self.package
            .resolve_pipeline(cpu_id, dialect_override)
            .map_err(Into::into)
    }

    pub fn resolve_active_pipeline(
        &self,
    ) -> Result<ResolvedHierarchy, HierarchyRuntimeBridgeError> {
        let cpu_id = self
            .active_cpu
            .as_deref()
            .ok_or(HierarchyRuntimeBridgeError::ActiveCpuNotSet)?;
        self.resolve_pipeline(cpu_id, self.dialect_override.as_deref())
    }

    pub fn resolve_active_pipeline_context(
        &self,
    ) -> Result<ResolvedHierarchyContext<'_>, HierarchyRuntimeBridgeError> {
        let cpu_id = self
            .active_cpu
            .as_deref()
            .ok_or(HierarchyRuntimeBridgeError::ActiveCpuNotSet)?;
        self.package
            .resolve_pipeline_context(cpu_id, self.dialect_override.as_deref())
            .map_err(Into::into)
    }
}
