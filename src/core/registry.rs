// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Module registry for CPU families, CPUs, and syntax dialects.
//!
//! This registry provides a standardized interface for selecting the family,
//! CPU, and dialect modules used during assembly. The interfaces intentionally
//! do not expose instruction tables; they expose only parsing, resolution, and
//! encoding functions.

use std::collections::HashMap;

use crate::core::cpu::{CpuFamily, CpuType};
use crate::core::family::{AssemblerContext, EncodeResult, FamilyHandler, FamilyParseError};
use crate::core::parser::Expr;

use crate::families::intel8080 as intel8080;
use crate::families::mos6502 as mos6502;
use crate::i8085::I8085CpuHandler;
use crate::m65c02::M65C02CpuHandler;
use crate::z80::Z80CpuHandler;

#[derive(Clone, Debug)]
pub enum FamilyOperandAny {
    Intel8080(intel8080::FamilyOperand),
    MOS6502(mos6502::FamilyOperand),
}

impl FamilyOperandAny {
    pub fn family_id(&self) -> CpuFamily {
        match self {
            FamilyOperandAny::Intel8080(_) => CpuFamily::Intel8080,
            FamilyOperandAny::MOS6502(_) => CpuFamily::MOS6502,
        }
    }
}

#[derive(Clone, Debug)]
pub enum OperandAny {
    Intel8080(intel8080::Operand),
    MOS6502(mos6502::Operand),
}

impl OperandAny {
    pub fn family_id(&self) -> CpuFamily {
        match self {
            OperandAny::Intel8080(_) => CpuFamily::Intel8080,
            OperandAny::MOS6502(_) => CpuFamily::MOS6502,
        }
    }
}

pub trait FamilyHandlerDyn: Send + Sync {
    fn family_id(&self) -> CpuFamily;
    fn parse_operands(
        &self,
        mnemonic: &str,
        exprs: &[Expr],
    ) -> Result<Vec<FamilyOperandAny>, FamilyParseError>;
    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[OperandAny],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>>;
    fn is_register(&self, name: &str) -> bool;
    fn is_condition(&self, name: &str) -> bool;
}

pub trait CpuHandlerDyn: Send + Sync {
    fn cpu_id(&self) -> CpuType;
    fn family_id(&self) -> CpuFamily;
    fn resolve_operands(
        &self,
        mnemonic: &str,
        family_operands: &[FamilyOperandAny],
        ctx: &dyn AssemblerContext,
    ) -> Result<Vec<OperandAny>, String>;
    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[OperandAny],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>>;
    fn supports_mnemonic(&self, mnemonic: &str) -> bool;
}

pub trait DialectModule: Send + Sync {
    fn dialect_id(&self) -> &'static str;
    fn family_id(&self) -> CpuFamily;
    fn map_mnemonic(
        &self,
        mnemonic: &str,
        operands: &[FamilyOperandAny],
    ) -> Option<(String, Vec<FamilyOperandAny>)>;
}

pub trait CpuValidator: Send + Sync {
    fn validate_instruction(
        &self,
        _mnemonic: &str,
        _operands: &[OperandAny],
        _ctx: &dyn AssemblerContext,
    ) -> Result<(), String> {
        Ok(())
    }
}

pub trait FamilyModule: Send + Sync {
    fn family_id(&self) -> CpuFamily;
    fn canonical_dialect(&self) -> &'static str;
    fn dialects(&self) -> Vec<Box<dyn DialectModule>>;
    fn handler(&self) -> Box<dyn FamilyHandlerDyn>;
}

pub trait CpuModule: Send + Sync {
    fn cpu_id(&self) -> CpuType;
    fn family_id(&self) -> CpuFamily;
    fn default_dialect(&self) -> &'static str;
    fn handler(&self) -> Box<dyn CpuHandlerDyn>;
    fn validator(&self) -> Option<Box<dyn CpuValidator>> {
        None
    }
}

#[derive(Debug, Clone)]
pub enum RegistryError {
    MissingFamily(CpuFamily),
    MissingCpu(CpuType),
    MissingDialect { family: CpuFamily, dialect: String },
}

pub struct ResolvedPipeline<'a> {
    pub family: Box<dyn FamilyHandlerDyn>,
    pub cpu: Box<dyn CpuHandlerDyn>,
    pub dialect: &'a dyn DialectModule,
    pub validator: Option<Box<dyn CpuValidator>>,
}

pub struct ModuleRegistry {
    families: HashMap<CpuFamily, Box<dyn FamilyModule>>,
    cpus: HashMap<CpuType, Box<dyn CpuModule>>,
    dialects: HashMap<(CpuFamily, String), Box<dyn DialectModule>>,
}

impl Default for ModuleRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleRegistry {
    pub fn new() -> Self {
        Self {
            families: HashMap::new(),
            cpus: HashMap::new(),
            dialects: HashMap::new(),
        }
    }

    pub fn with_defaults() -> Self {
        let mut registry = Self::new();
        registry.register_family(Box::new(
            crate::families::intel8080::registry::Intel8080FamilyModule,
        ));
        registry.register_family(Box::new(
            crate::families::mos6502::registry::MOS6502FamilyModule,
        ));

        registry.register_cpu(Box::new(crate::i8085::registry::I8085CpuModule));
        registry.register_cpu(Box::new(crate::z80::registry::Z80CpuModule));
        registry.register_cpu(Box::new(
            crate::families::mos6502::registry::M6502CpuModule,
        ));
        registry.register_cpu(Box::new(crate::m65c02::registry::M65C02CpuModule));
        registry
    }

    pub fn register_family(&mut self, module: Box<dyn FamilyModule>) {
        let family_id = module.family_id();
        for dialect in module.dialects() {
            let key = (family_id, normalize_dialect(dialect.dialect_id()));
            self.dialects.insert(key, dialect);
        }
        self.families.insert(family_id, module);
    }

    pub fn register_cpu(&mut self, module: Box<dyn CpuModule>) {
        let cpu_id = module.cpu_id();
        self.cpus.insert(cpu_id, module);
    }

    pub fn resolve_pipeline(
        &self,
        cpu: CpuType,
        dialect_override: Option<&str>,
    ) -> Result<ResolvedPipeline<'_>, RegistryError> {
        let cpu_module = self
            .cpus
            .get(&cpu)
            .ok_or(RegistryError::MissingCpu(cpu))?;
        let family_id = cpu_module.family_id();
        let family_module = self
            .families
            .get(&family_id)
            .ok_or(RegistryError::MissingFamily(family_id))?;

        let selected = if let Some(override_id) = dialect_override {
            self.lookup_dialect(family_id, override_id)
                .ok_or_else(|| RegistryError::MissingDialect {
                    family: family_id,
                    dialect: override_id.to_string(),
                })?
        } else if let Some(dialect) =
            self.lookup_dialect(family_id, cpu_module.default_dialect())
        {
            dialect
        } else {
            self.lookup_dialect(family_id, family_module.canonical_dialect())
                .ok_or_else(|| RegistryError::MissingDialect {
                    family: family_id,
                    dialect: family_module.canonical_dialect().to_string(),
                })?
        };

        Ok(ResolvedPipeline {
            family: family_module.handler(),
            cpu: cpu_module.handler(),
            dialect: selected,
            validator: cpu_module.validator(),
        })
    }

    fn lookup_dialect(
        &self,
        family: CpuFamily,
        dialect: &str,
    ) -> Option<&dyn DialectModule> {
        let key = (family, normalize_dialect(dialect));
        self.dialects.get(&key).map(|dialect| dialect.as_ref())
    }
}

fn normalize_dialect(dialect: &str) -> String {
    dialect.to_ascii_lowercase()
}

fn expect_intel_family_operands(
    operands: &[FamilyOperandAny],
) -> Result<Vec<intel8080::FamilyOperand>, String> {
    operands
        .iter()
        .map(|operand| match operand {
            FamilyOperandAny::Intel8080(inner) => Ok(inner.clone()),
            _ => Err("expected Intel 8080 family operands".to_string()),
        })
        .collect()
}

fn expect_mos6502_family_operands(
    operands: &[FamilyOperandAny],
) -> Result<Vec<mos6502::FamilyOperand>, String> {
    operands
        .iter()
        .map(|operand| match operand {
            FamilyOperandAny::MOS6502(inner) => Ok(inner.clone()),
            _ => Err("expected MOS 6502 family operands".to_string()),
        })
        .collect()
}

fn expect_intel_operands(operands: &[OperandAny]) -> Result<Vec<intel8080::Operand>, String> {
    operands
        .iter()
        .map(|operand| match operand {
            OperandAny::Intel8080(inner) => Ok(inner.clone()),
            _ => Err("expected Intel 8080 operands".to_string()),
        })
        .collect()
}

fn expect_mos6502_operands(operands: &[OperandAny]) -> Result<Vec<mos6502::Operand>, String> {
    operands
        .iter()
        .map(|operand| match operand {
            OperandAny::MOS6502(inner) => Ok(inner.clone()),
            _ => Err("expected MOS 6502 operands".to_string()),
        })
        .collect()
}

impl FamilyHandlerDyn for intel8080::Intel8080FamilyHandler {
    fn family_id(&self) -> CpuFamily {
        CpuFamily::Intel8080
    }

    fn parse_operands(
        &self,
        mnemonic: &str,
        exprs: &[Expr],
    ) -> Result<Vec<FamilyOperandAny>, FamilyParseError> {
        <Self as FamilyHandler>::parse_operands(self, mnemonic, exprs)
            .map(|ops| ops.into_iter().map(FamilyOperandAny::Intel8080).collect())
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[OperandAny],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let converted = match expect_intel_operands(operands) {
            Ok(ops) => ops,
            Err(msg) => return EncodeResult::error(msg),
        };
        <Self as FamilyHandler>::encode_instruction(self, mnemonic, &converted, ctx)
    }

    fn is_register(&self, name: &str) -> bool {
        <Self as FamilyHandler>::is_register(self, name)
    }

    fn is_condition(&self, name: &str) -> bool {
        <Self as FamilyHandler>::is_condition(self, name)
    }
}

impl FamilyHandlerDyn for mos6502::MOS6502FamilyHandler {
    fn family_id(&self) -> CpuFamily {
        CpuFamily::MOS6502
    }

    fn parse_operands(
        &self,
        mnemonic: &str,
        exprs: &[Expr],
    ) -> Result<Vec<FamilyOperandAny>, FamilyParseError> {
        <Self as FamilyHandler>::parse_operands(self, mnemonic, exprs)
            .map(|ops| ops.into_iter().map(FamilyOperandAny::MOS6502).collect())
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[OperandAny],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let converted = match expect_mos6502_operands(operands) {
            Ok(ops) => ops,
            Err(msg) => return EncodeResult::error(msg),
        };
        <Self as FamilyHandler>::encode_instruction(self, mnemonic, &converted, ctx)
    }

    fn is_register(&self, name: &str) -> bool {
        <Self as FamilyHandler>::is_register(self, name)
    }

    fn is_condition(&self, name: &str) -> bool {
        <Self as FamilyHandler>::is_condition(self, name)
    }
}

impl CpuHandlerDyn for I8085CpuHandler {
    fn cpu_id(&self) -> CpuType {
        CpuType::I8085
    }

    fn family_id(&self) -> CpuFamily {
        CpuFamily::Intel8080
    }

    fn resolve_operands(
        &self,
        mnemonic: &str,
        family_operands: &[FamilyOperandAny],
        ctx: &dyn AssemblerContext,
    ) -> Result<Vec<OperandAny>, String> {
        let converted = expect_intel_family_operands(family_operands)?;
        <Self as crate::core::family::CpuHandler>::resolve_operands(self, mnemonic, &converted, ctx)
            .map(|ops| ops.into_iter().map(OperandAny::Intel8080).collect())
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[OperandAny],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let converted = match expect_intel_operands(operands) {
            Ok(ops) => ops,
            Err(msg) => return EncodeResult::error(msg),
        };
        <Self as crate::core::family::CpuHandler>::encode_instruction(self, mnemonic, &converted, ctx)
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        <Self as crate::core::family::CpuHandler>::supports_mnemonic(self, mnemonic)
    }
}

impl CpuHandlerDyn for Z80CpuHandler {
    fn cpu_id(&self) -> CpuType {
        CpuType::Z80
    }

    fn family_id(&self) -> CpuFamily {
        CpuFamily::Intel8080
    }

    fn resolve_operands(
        &self,
        mnemonic: &str,
        family_operands: &[FamilyOperandAny],
        ctx: &dyn AssemblerContext,
    ) -> Result<Vec<OperandAny>, String> {
        let converted = expect_intel_family_operands(family_operands)?;
        <Self as crate::core::family::CpuHandler>::resolve_operands(self, mnemonic, &converted, ctx)
            .map(|ops| ops.into_iter().map(OperandAny::Intel8080).collect())
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[OperandAny],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let converted = match expect_intel_operands(operands) {
            Ok(ops) => ops,
            Err(msg) => return EncodeResult::error(msg),
        };
        <Self as crate::core::family::CpuHandler>::encode_instruction(self, mnemonic, &converted, ctx)
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        <Self as crate::core::family::CpuHandler>::supports_mnemonic(self, mnemonic)
    }
}

impl CpuHandlerDyn for mos6502::M6502CpuHandler {
    fn cpu_id(&self) -> CpuType {
        CpuType::M6502
    }

    fn family_id(&self) -> CpuFamily {
        CpuFamily::MOS6502
    }

    fn resolve_operands(
        &self,
        mnemonic: &str,
        family_operands: &[FamilyOperandAny],
        ctx: &dyn AssemblerContext,
    ) -> Result<Vec<OperandAny>, String> {
        let converted = expect_mos6502_family_operands(family_operands)?;
        <Self as crate::core::family::CpuHandler>::resolve_operands(self, mnemonic, &converted, ctx)
            .map(|ops| ops.into_iter().map(OperandAny::MOS6502).collect())
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[OperandAny],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let converted = match expect_mos6502_operands(operands) {
            Ok(ops) => ops,
            Err(msg) => return EncodeResult::error(msg),
        };
        <Self as crate::core::family::CpuHandler>::encode_instruction(self, mnemonic, &converted, ctx)
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        <Self as crate::core::family::CpuHandler>::supports_mnemonic(self, mnemonic)
    }
}

impl CpuHandlerDyn for M65C02CpuHandler {
    fn cpu_id(&self) -> CpuType {
        CpuType::M65C02
    }

    fn family_id(&self) -> CpuFamily {
        CpuFamily::MOS6502
    }

    fn resolve_operands(
        &self,
        mnemonic: &str,
        family_operands: &[FamilyOperandAny],
        ctx: &dyn AssemblerContext,
    ) -> Result<Vec<OperandAny>, String> {
        let converted = expect_mos6502_family_operands(family_operands)?;
        <Self as crate::core::family::CpuHandler>::resolve_operands(self, mnemonic, &converted, ctx)
            .map(|ops| ops.into_iter().map(OperandAny::MOS6502).collect())
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[OperandAny],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let converted = match expect_mos6502_operands(operands) {
            Ok(ops) => ops,
            Err(msg) => return EncodeResult::error(msg),
        };
        <Self as crate::core::family::CpuHandler>::encode_instruction(self, mnemonic, &converted, ctx)
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        <Self as crate::core::family::CpuHandler>::supports_mnemonic(self, mnemonic)
    }
}
