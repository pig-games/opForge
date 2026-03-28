// SPDX-License-Identifier: GPL-3.0-or-later

//! Motorola 68000 CPU implementation.

pub mod module;

mod handler;

pub use handler::M68000CpuHandler;
