// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Shared runtime bridge error surface.

use package::OpcpuCodecError;
use types::hierarchy::HierarchyError;

use crate::builder::HierarchyBuildError;
use crate::bytecode::VmError;
use crate::operand_record_vm::OperandRecordVmError;
use crate::runtime_bridge::HierarchyRuntimeBridgeError;
use crate::runtime_diagnostics::RuntimeBridgeDiagnostic;
use crate::runtime_model_core::RuntimeModelLoadError;
use crate::value_vm::ValueVmError;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RuntimeBridgeError {
    ActiveCpuNotSet,
    Build(HierarchyBuildError),
    Package(OpcpuCodecError),
    Hierarchy(HierarchyError),
    Resolve(String),
    Diagnostic(RuntimeBridgeDiagnostic),
    Vm(VmError),
    ValueVm(ValueVmError),
    OperandRecordVm(OperandRecordVmError),
}

impl std::fmt::Display for RuntimeBridgeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ActiveCpuNotSet => write!(f, "active cpu is not set"),
            Self::Build(err) => write!(f, "runtime model build error: {}", err),
            Self::Package(err) => write!(f, "runtime package error: {}", err),
            Self::Hierarchy(err) => write!(f, "hierarchy resolution error: {}", err),
            Self::Resolve(err) => write!(f, "{}", err),
            Self::Diagnostic(diag) => write!(f, "{}", diag.render()),
            Self::Vm(err) => write!(f, "VM encode error: {}", err),
            Self::ValueVm(err) => write!(f, "value VM error: {}", err),
            Self::OperandRecordVm(err) => write!(f, "operand-record VM error: {}", err),
        }
    }
}

impl std::error::Error for RuntimeBridgeError {}

impl From<HierarchyError> for RuntimeBridgeError {
    fn from(value: HierarchyError) -> Self {
        Self::Hierarchy(value)
    }
}

impl From<HierarchyBuildError> for RuntimeBridgeError {
    fn from(value: HierarchyBuildError) -> Self {
        Self::Build(value)
    }
}

impl From<RuntimeModelLoadError> for RuntimeBridgeError {
    fn from(value: RuntimeModelLoadError) -> Self {
        match value {
            RuntimeModelLoadError::Build(err) => Self::Build(err),
            RuntimeModelLoadError::Package(err) => Self::Package(err),
            RuntimeModelLoadError::Hierarchy(err) => Self::Hierarchy(err),
        }
    }
}

impl From<HierarchyRuntimeBridgeError> for RuntimeBridgeError {
    fn from(value: HierarchyRuntimeBridgeError) -> Self {
        match value {
            HierarchyRuntimeBridgeError::ActiveCpuNotSet => Self::ActiveCpuNotSet,
            HierarchyRuntimeBridgeError::Hierarchy(err) => Self::Hierarchy(err),
        }
    }
}

impl From<OpcpuCodecError> for RuntimeBridgeError {
    fn from(value: OpcpuCodecError) -> Self {
        Self::Package(value)
    }
}

impl From<VmError> for RuntimeBridgeError {
    fn from(value: VmError) -> Self {
        Self::Vm(value)
    }
}

impl From<ValueVmError> for RuntimeBridgeError {
    fn from(value: ValueVmError) -> Self {
        Self::ValueVm(value)
    }
}

impl From<OperandRecordVmError> for RuntimeBridgeError {
    fn from(value: OperandRecordVmError) -> Self {
        Self::OperandRecordVm(value)
    }
}
