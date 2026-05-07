// SPDX-License-Identifier: GPL-3.0-or-later

//! VM runtime and bridge interfaces for libopforge.
//!
//! The long-term target is a single `vm` crate with explicit internal
//! partitioning:
//!
//! - [`vm_core`]: shared VM substrate used by both processor domains
//! - [`vm_opcore`]: `.opcore` VM helpers
//! - [`vm_opasm`]: `.opasm` VM helpers
//!
//! Existing top-level modules remain available during migration, but new code
//! should prefer these partition surfaces when choosing ownership.

pub mod builder;
pub mod bytecode;
pub mod execution_model;
pub mod expr_vm_compat;
pub(crate) mod exvm_v2_runtime;
pub mod hierarchy;
pub mod intel8080_vm;
pub mod listing;
pub mod native6502;
pub mod native6502_abi;
pub mod native_prvm;
pub mod output_artifacts;
pub(crate) mod output_components;
pub(crate) mod output_hunk;
pub mod output_model;
pub mod portable_contract;
pub mod rewrite;
pub mod rollout;
pub mod runtime_bootstrap;
pub mod runtime_bridge;
pub mod runtime_contract_types;
pub mod runtime_diagnostics;
pub mod runtime_error;
pub mod runtime_expr_parser;
pub mod runtime_model_core;
pub mod runtime_model_types;
pub mod runtime_parse_utils;
pub mod runtime_portable_types;
#[cfg(test)]
mod runtime_tests;
pub mod selector_encoding_utils;
pub mod tokenizer_runtime_utils;
pub mod vm_core;
pub mod vm_opasm;
pub(crate) mod vm_opasm_parse;
pub mod vm_opcore;
