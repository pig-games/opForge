// SPDX-License-Identifier: GPL-3.0-or-later

//! Temporary bridge for expression-VM constants during package extraction.

pub const EXPR_VM_OPCODE_VERSION_V1: u16 = 0x0001;
pub const DIAG_EXPR_INVALID_OPCODE: &str = "ope001";
pub const DIAG_EXPR_STACK_UNDERFLOW: &str = "ope002";
pub const DIAG_EXPR_STACK_DEPTH_EXCEEDED: &str = "ope003";
pub const DIAG_EXPR_UNKNOWN_SYMBOL: &str = "ope004";
pub const DIAG_EXPR_EVAL_FAILURE: &str = "ope005";
pub const DIAG_EXPR_UNSUPPORTED_FEATURE: &str = "ope006";
pub const DIAG_EXPR_BUDGET_EXCEEDED: &str = "ope007";
pub const DIAG_EXPR_INVALID_PROGRAM: &str = "ope008";
