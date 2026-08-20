// SPDX-License-Identifier: GPL-3.0-or-later

//! Motorola 6800 family implementation.

pub mod module;

mod handler;

pub mod operand;

pub mod package_programs;

mod table;

pub use handler::M6800FamilyHandler;
pub use operand::{AddressMode, FamilyOperand, IndexedAutoMode, Operand};
pub use table::{
    has_mnemonic, lookup_instruction, lookup_prefixed_instruction, FAMILY_INSTRUCTION_TABLE,
    PAGE2_PREFIX, PAGE3_PREFIX, PREFIXED_FAMILY_INSTRUCTION_TABLE,
};

pub fn is_register(name: &str) -> bool {
    matches!(
        name.to_ascii_uppercase().as_str(),
        "A" | "B" | "CC" | "DP" | "D" | "X" | "Y" | "U" | "S" | "PC" | "E" | "F" | "W" | "V" | "MD"
    )
}
