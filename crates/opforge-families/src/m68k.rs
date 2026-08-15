// SPDX-License-Identifier: GPL-3.0-or-later

//! Motorola 68000 family implementation.

pub mod module;
pub mod package_programs;

mod compatibility;
mod fpu_capability;
mod handler;
mod operand_surface;

pub mod operand;
pub mod state;

mod table;

pub(crate) use compatibility::validate_68080_register_compatibility;
pub(crate) use fpu_capability::{deferred_fpu_message_for_cpu, validate_fpu_target_for_cpu};
pub(crate) use handler::EffectiveAddressKind;
pub use handler::M68KFamilyHandler;
pub use operand::{FamilyOperand, Operand};
pub use operand_surface::parse_runtime_operand_surface_expr;
pub use table::{
    has_fpu_mnemonic, has_m68010_mnemonic, has_m68020_mnemonic, has_m68080_mnemonic, has_mnemonic,
    parse_fpu_mnemonic, parse_m68010_mnemonic, parse_m68020_mnemonic, parse_m68080_mnemonic,
    parse_mnemonic, FpuFormat, FpuMnemonicKind, M68010MnemonicKind, M68020MnemonicKind,
    M68080MnemonicKind, MnemonicKind, OperationSize,
};

fn parse_numeric_suffix(name: &str, prefix: char) -> Option<u8> {
    let suffix = name.strip_prefix(prefix)?;
    suffix.parse::<u8>().ok()
}

pub fn is_68080_data_bank_register(name: &str) -> bool {
    let upper = name.to_ascii_uppercase();
    parse_numeric_suffix(&upper, 'E').is_some_and(|reg| reg <= 23)
}

pub fn is_68080_address_bank_register(name: &str) -> bool {
    let upper = name.to_ascii_uppercase();
    parse_numeric_suffix(&upper, 'B').is_some_and(|reg| reg <= 7)
}

pub fn is_data_register(name: &str) -> bool {
    let upper = name.to_ascii_uppercase();
    parse_numeric_suffix(&upper, 'D').is_some_and(|reg| reg <= 7)
        || is_68080_data_bank_register(&upper)
}

pub fn is_address_register(name: &str) -> bool {
    let upper = name.to_ascii_uppercase();
    upper == "SP"
        || parse_numeric_suffix(&upper, 'A').is_some_and(|reg| reg <= 7)
        || is_68080_address_bank_register(&upper)
}

pub fn is_register(name: &str) -> bool {
    matches!(
        name.to_ascii_uppercase().as_str(),
        "PC" | "SR" | "CCR" | "USP" | "SSP"
    ) || is_data_register(name)
        || is_address_register(name)
}
