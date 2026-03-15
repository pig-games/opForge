// SPDX-License-Identifier: GPL-3.0-or-later

//! 45GS02 CPU implementation.

pub mod instructions;

pub mod module;

mod handler;

pub use handler::M45GS02CpuHandler;
