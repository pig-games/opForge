// SPDX-License-Identifier: GPL-3.0-or-later

//! Language-core processing for libopforge.

pub mod conditional;
pub mod error;
pub mod expr;
pub mod expr_vm;
pub mod expression;
pub mod imports;
pub mod macro_processor;
pub mod modules;
pub mod parser;
pub mod preprocess;
pub mod scope;
pub mod services;
pub mod struct_table;
pub mod text_utils;
pub mod tokenizer;

pub use error::{CoreError, CoreErrorKind, LineParseError, ModuleItemError};

#[cfg(test)]
mod parser_reporter;
#[cfg(test)]
mod report;
