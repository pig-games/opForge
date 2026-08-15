// SPDX-License-Identifier: GPL-3.0-or-later

//! Motorola 68080 CPU implementation.

pub mod module;
pub mod package_programs;

mod handler;

pub use handler::M68080CpuHandler;
