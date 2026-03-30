// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68030 CPU handler implementation.

use crate::families::m68k::{
    parse_fpu_mnemonic, parse_m68020_mnemonic, FpuMnemonicKind, M68020MnemonicKind, OperationSize,
};
use crate::families::m68k::{FamilyOperand, M68KFamilyHandler, Operand};
use crate::m68020::M68020CpuHandler;
use registry::family::{AssemblerContext, CpuHandler, EncodeResult};

#[derive(Debug)]
pub struct M68030CpuHandler {
    base: M68020CpuHandler,
}

impl Default for M68030CpuHandler {
    fn default() -> Self {
        Self::new()
    }
}

impl M68030CpuHandler {
    const LEGAL_FPU_TARGETS: [u32; 2] = [1, 2];

    pub fn new() -> Self {
        Self {
            base: M68020CpuHandler::new(),
        }
    }

    fn fpu_target_name(state_value: u32) -> &'static str {
        match state_value {
            1 => "68881",
            2 => "68882",
            3 => "68040",
            _ => "none",
        }
    }

    fn handle_fpu_mnemonic(
        &self,
        display_name: &str,
        ctx: &dyn AssemblerContext,
    ) -> Result<&'static str, EncodeResult<Vec<u8>>> {
        let target = ctx
            .cpu_state_flag(crate::families::m68k::state::FPU_TARGET_KEY)
            .unwrap_or(0);

        if target == 0 {
            return Err(EncodeResult::error(format!(
                "{display_name} requires an active .fpu target on m68030; legal .fpu targets for m68030 FPU instructions: 68881, 68882"
            )));
        }

        if !Self::LEGAL_FPU_TARGETS.contains(&target) {
            return Err(EncodeResult::error(format!(
                "{display_name} is not available with .fpu {} on m68030; legal .fpu targets for m68030 FPU instructions: 68881, 68882",
                Self::fpu_target_name(target),
            )));
        }

        Ok(Self::fpu_target_name(target))
    }

    fn encode_pflush(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("PFLUSH does not support size suffixes");
        }

        let [fc_operand, mask_operand] = operands else {
            return EncodeResult::error("PFLUSH currently expects exactly two operands: #fc,#mask");
        };

        let Operand::Immediate { expr: fc_expr, .. } = fc_operand else {
            return EncodeResult::error_with_span(
                "PFLUSH FC operand must be an immediate value on m68030",
                fc_operand.span(),
            );
        };
        let Operand::Immediate {
            expr: mask_expr, ..
        } = mask_operand
        else {
            return EncodeResult::error_with_span(
                "PFLUSH mask operand must be an immediate value on m68030",
                mask_operand.span(),
            );
        };

        let fc_value = match M68KFamilyHandler::eval_expr(fc_expr, ctx) {
            Ok(value) => value,
            Err(err) => return EncodeResult::error_with_span(err, fc_operand.span()),
        };
        if !(0..=7).contains(&fc_value) {
            return EncodeResult::error_with_span(
                "PFLUSH FC immediate out of range (0-7)",
                fc_operand.span(),
            );
        }

        let mask_value = match M68KFamilyHandler::eval_expr(mask_expr, ctx) {
            Ok(value) => value,
            Err(err) => return EncodeResult::error_with_span(err, mask_operand.span()),
        };
        if !(0..=7).contains(&mask_value) {
            return EncodeResult::error_with_span(
                "PFLUSH mask immediate out of range (0-7)",
                mask_operand.span(),
            );
        }

        let fc_bits = 0b10_000_u16 | fc_value as u16;
        let second_word = 0x3000_u16 | ((mask_value as u16) << 5) | fc_bits;

        let mut bytes = Vec::new();
        M68KFamilyHandler::emit_word(&mut bytes, 0xF000);
        M68KFamilyHandler::emit_word(&mut bytes, second_word);
        EncodeResult::ok(bytes)
    }
}

impl CpuHandler for M68030CpuHandler {
    type Family = M68KFamilyHandler;

    fn family(&self) -> &Self::Family {
        self.base.family()
    }

    fn resolve_operands(
        &self,
        mnemonic: &str,
        family_operands: &[FamilyOperand],
        ctx: &dyn AssemblerContext,
    ) -> Result<Vec<Operand>, String> {
        self.base.resolve_operands(mnemonic, family_operands, ctx)
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if let Some(parsed) = parse_fpu_mnemonic(mnemonic) {
            if parsed.has_unknown_size_suffix {
                return EncodeResult::error(format!(
                    "unsupported size suffix for {}",
                    parsed.display_name
                ));
            }

            if let Err(err) = self.handle_fpu_mnemonic(&parsed.display_name, ctx) {
                return err;
            }

            return match parsed.kind {
                FpuMnemonicKind::Fmove
                | FpuMnemonicKind::Fmovem
                | FpuMnemonicKind::Fadd
                | FpuMnemonicKind::Fsub
                | FpuMnemonicKind::Fmul
                | FpuMnemonicKind::Fdiv
                | FpuMnemonicKind::Fsqrt
                | FpuMnemonicKind::Fabs
                | FpuMnemonicKind::Fneg
                | FpuMnemonicKind::Fcmp
                | FpuMnemonicKind::Ftst
                | FpuMnemonicKind::Fint
                | FpuMnemonicKind::Fintrz
                | FpuMnemonicKind::Fsin
                | FpuMnemonicKind::Fcos
                | FpuMnemonicKind::Fsincos
                | FpuMnemonicKind::Ftan
                | FpuMnemonicKind::Fasin
                | FpuMnemonicKind::Facos
                | FpuMnemonicKind::Fatan
                | FpuMnemonicKind::Fsinh
                | FpuMnemonicKind::Fcosh
                | FpuMnemonicKind::Ftanh
                | FpuMnemonicKind::Fatanh
                | FpuMnemonicKind::Fbranch
                | FpuMnemonicKind::Fdbcc
                | FpuMnemonicKind::Fscc
                | FpuMnemonicKind::Ftrapcc
                | FpuMnemonicKind::Fsave
                | FpuMnemonicKind::Frestore => {
                    self.base.encode_instruction(mnemonic, operands, ctx)
                }
            };
        }

        if let Some(parsed) = parse_m68020_mnemonic(mnemonic) {
            if matches!(parsed.kind, M68020MnemonicKind::Pflush) {
                if parsed.has_unknown_size_suffix {
                    return EncodeResult::error(format!(
                        "unsupported size suffix for {}",
                        parsed.display_name
                    ));
                }
                return self.encode_pflush(parsed.size, operands, ctx);
            }
        }

        self.base.encode_instruction(mnemonic, operands, ctx)
    }

    fn supports_mnemonic(&self, mnemonic: &str) -> bool {
        self.base.supports_mnemonic(mnemonic)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use opcore::parser::Expr;
    use std::collections::HashMap;
    use types::symbol::SymbolTable;

    #[derive(Default)]
    struct TestContext {
        state_flags: HashMap<String, u32>,
        symbols: SymbolTable,
    }

    impl TestContext {
        fn with_cpu_state_flag(mut self, key: &str, value: u32) -> Self {
            self.state_flags.insert(key.to_string(), value);
            self
        }
    }

    impl AssemblerContext for TestContext {
        fn eval_expr(&self, _expr: &Expr) -> Result<i64, String> {
            Err("unexpected expression evaluation in test".to_string())
        }

        fn symbols(&self) -> &SymbolTable {
            &self.symbols
        }

        fn has_symbol(&self, _name: &str) -> bool {
            false
        }

        fn symbol_is_finalized(&self, _name: &str) -> Option<bool> {
            None
        }

        fn current_address(&self) -> u32 {
            0
        }

        fn pass(&self) -> u8 {
            2
        }

        fn scalar_value_symbol(&self, _name: &str) -> Option<i64> {
            None
        }

        fn cpu_state_flag(&self, key: &str) -> Option<u32> {
            self.state_flags.get(key).copied()
        }
    }

    #[test]
    fn fpu_mnemonics_report_incompatible_target_on_m68030() {
        let handler = M68030CpuHandler::new();
        let ctx = TestContext::default()
            .with_cpu_state_flag(crate::families::m68k::state::FPU_TARGET_KEY, 3);

        match handler.encode_instruction("FADD", &[], &ctx) {
            EncodeResult::Error(message, None) => {
                assert!(message.contains("FADD is not available with .fpu 68040 on m68030"));
                assert!(message.contains("68881, 68882"));
            }
            other => panic!("expected incompatible-target diagnostic, got {other:?}"),
        }
    }
}
