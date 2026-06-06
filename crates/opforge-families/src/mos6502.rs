// SPDX-License-Identifier: GPL-3.0-or-later

//! MOS 6502 family implementation.

mod cpu_handler;

mod handler;

mod selector;

pub mod module;

pub mod operand;

pub(crate) mod operand_resolution;

pub mod table;

pub use cpu_handler::M6502CpuHandler;
pub use handler::MOS6502FamilyHandler;
pub use operand::{AddressMode, FamilyOperand, Operand, OperandForce};
pub use selector::{selector_input_from_exprs, VmSelectorInput};
pub use table::{has_mnemonic, lookup_instruction, FAMILY_INSTRUCTION_TABLE};

pub fn is_register(name: &str) -> bool {
    matches!(name, "A" | "X" | "Y")
}
