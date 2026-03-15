// SPDX-License-Identifier: GPL-3.0-or-later

//! Zilog Z80 CPU implementation.

pub mod dialect;

pub mod extensions;

mod handler;

pub mod module;

pub use dialect::{is_z80_only_mnemonic, map_register};
pub use extensions::{lookup_extension, Z80_EXTENSION_TABLE};
pub use handler::Z80CpuHandler;

pub fn is_register(ident: &str) -> bool {
    matches!(
        ident,
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
            | "AF"
            | "AF'"
            | "BC"
            | "DE"
            | "HL"
            | "SP"
            | "IX"
            | "IY"
            | "M"
            | "NZ"
            | "Z"
            | "NC"
            | "PO"
            | "PE"
            | "P"
    )
}
