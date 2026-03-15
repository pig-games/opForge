// SPDX-License-Identifier: GPL-3.0-or-later

//! WDC 65816 CPU implementation.

pub mod instructions;

pub mod module;

pub mod state;

mod handler;

pub use handler::M65816CpuHandler;
