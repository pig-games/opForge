// SPDX-License-Identifier: GPL-3.0-or-later

//! Intel 8085 CPU implementation.

pub mod extensions;

mod handler;

pub mod module;

pub use extensions::{lookup_extension, I8085_EXTENSION_TABLE};
pub use handler::I8085CpuHandler;

pub fn is_register(ident: &str) -> bool {
    matches!(
        ident,
        "A" | "B" | "C" | "D" | "E" | "H" | "L" | "M" | "SP" | "PSW"
    )
}
