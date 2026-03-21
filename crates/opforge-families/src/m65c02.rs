// SPDX-License-Identifier: GPL-3.0-or-later

//! WDC 65C02 CPU implementation.

pub mod instructions;

pub mod module;

mod handler;

pub use handler::M65C02CpuHandler;
