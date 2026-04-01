// SPDX-License-Identifier: GPL-3.0-or-later

//! Motorola 68010 CPU implementation.

pub mod module;

mod handler;

pub use handler::M68010CpuHandler;
