// SPDX-License-Identifier: GPL-3.0-or-later

//! Assembler processing for libopforge.

#[cfg(test)]
mod external_oracle;
#[cfg(test)]
mod fs_uae_smoke;
#[cfg(test)]
mod normalization;
#[cfg(test)]
mod oracle;
#[cfg(test)]
mod tests;

pub mod engine;
pub mod error;
pub mod expression;
pub mod line;
pub mod listing;
pub mod opasm;
pub mod operand;
pub mod output;
pub mod phase_profile;
pub mod preprocess;
mod repetition_driver;
pub mod runtime_config;
pub mod runtime_model;
pub mod state;

pub use line::repetition;
