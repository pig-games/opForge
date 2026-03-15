// SPDX-License-Identifier: GPL-3.0-or-later

//! Intel 8080 family implementation.

pub mod dialect;

pub mod extensions;

pub mod handler;

pub mod module;

mod operand;

pub mod table;

pub use crate::i8085::I8085CpuHandler;
pub use dialect::{
    find_mapping, is_z80_only_mnemonic, map_register, map_zilog_to_canonical, DialectEntry,
    OperandTransform,
};
pub use extensions::{I8085_EXTENSION_TABLE, Z80_EXTENSION_TABLE};
pub use handler::Intel8080FamilyHandler;
pub use operand::{expr_span, FamilyOperand, Operand};
pub use table::{lookup_instruction, ArgType, InstructionEntry, Prefix, FAMILY_INSTRUCTION_TABLE};

pub fn is_register(name: &str) -> bool {
    matches!(
        name.to_ascii_uppercase().as_str(),
        "A" | "B"
            | "C"
            | "D"
            | "E"
            | "H"
            | "L"
            | "I"
            | "R"
            | "IXH"
            | "IXL"
            | "IYH"
            | "IYL"
            | "BC"
            | "DE"
            | "HL"
            | "SP"
            | "AF"
            | "AF'"
            | "PSW"
            | "IX"
            | "IY"
            | "M"
    )
}

pub fn is_condition(name: &str) -> bool {
    matches!(
        name.to_ascii_uppercase().as_str(),
        "NZ" | "Z" | "NC" | "C" | "PO" | "PE" | "P" | "M"
    )
}

pub fn is_index_register(name: &str) -> bool {
    matches!(name.to_ascii_uppercase().as_str(), "IX" | "IY")
}
