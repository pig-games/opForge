// SPDX-License-Identifier: GPL-3.0-or-later

//! WDC 65816 CPU implementation.

pub mod instructions;

pub mod module;

pub mod selector;

pub mod state;

mod handler;

pub use handler::M65816CpuHandler;
pub use selector::{
    encode_runtime_operand_plan, input_shape_requires_runtime_family_support, VmSelectorAdapter,
};
