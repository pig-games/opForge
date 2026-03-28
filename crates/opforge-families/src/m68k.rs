// SPDX-License-Identifier: GPL-3.0-or-later

//! Motorola 68000 family implementation.

pub mod module;

mod handler;

pub mod operand;

mod table;

pub use handler::M68KFamilyHandler;
pub use operand::{FamilyOperand, Operand};
pub use table::{
    has_m68010_mnemonic, has_m68020_mnemonic, has_mnemonic, parse_m68010_mnemonic,
    parse_m68020_mnemonic, parse_mnemonic, M68010MnemonicKind, M68020MnemonicKind, MnemonicKind,
    OperationSize,
};

pub fn is_register(name: &str) -> bool {
    matches!(
        name.to_ascii_uppercase().as_str(),
        "D0" | "D1"
            | "D2"
            | "D3"
            | "D4"
            | "D5"
            | "D6"
            | "D7"
            | "A0"
            | "A1"
            | "A2"
            | "A3"
            | "A4"
            | "A5"
            | "A6"
            | "A7"
            | "SP"
            | "PC"
            | "SR"
            | "CCR"
            | "USP"
            | "SSP"
    )
}
