// SPDX-License-Identifier: GPL-3.0-or-later

//! CLI parsing and validation support for opForge hosts.

mod cli;
mod run;

pub use cli::*;
pub use run::{
    has_werror_violations, run_with_cli_with_context, run_with_validated_cli_with_context,
    CliRunError, CliRunReport,
};
