// SPDX-License-Identifier: GPL-3.0-or-later

//! Assembler-specific CPU, family, and dialect registry APIs for libopforge.

pub mod cpu;
pub mod family;
pub mod registry;
pub mod symbol_stability;
pub mod syntax;

pub use cpu::{CpuFamily, CpuType, EncodeError, OperandParseError};
pub use family::{
    expr_has_symbol_references, expr_has_unstable_symbols, AssemblerContext, CpuHandler,
    EncodeOutcome, EncodeResult, FamilyEncodeResult, FamilyHandler, FamilyParseError,
};
pub use registry::{
    AsmRegistry, CpuHandlerDyn, CpuModule, CpuValidator, DialectModule, FamilyHandlerDyn,
    FamilyModule, FamilyOperandSet, ModuleRegistry, OperandSet, RegistryError, ResolvedPipeline,
    VmEncodeCandidate,
};
pub use syntax::{
    parse_pack_directive_from_tokens, parse_place_directive_from_tokens_with,
    parse_statement_definition_from_line, parse_statement_signature_from_tokens,
    parser_from_line_with_registers, register_checker_from_fn, register_checker_none,
    RegisterChecker,
};
