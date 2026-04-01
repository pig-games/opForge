// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68000 family runtime state helpers.

use std::collections::HashMap;

use opcore::parser::Expr;
use registry::cpu::CpuType;
use registry::family::AssemblerContext;

pub const FPU_TARGET_KEY: &str = "m68k.fpu_target";
pub const RUNTIME_DIRECTIVE_IDS: &[&str] = &["FPU"];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FpuTarget {
    None,
    Mc68881,
    Mc68882,
    Mc68040,
}

impl FpuTarget {
    fn state_value(self) -> u32 {
        match self {
            Self::None => 0,
            Self::Mc68881 => 1,
            Self::Mc68882 => 2,
            Self::Mc68040 => 3,
        }
    }

    fn display_name(self) -> &'static str {
        match self {
            Self::None => "none",
            Self::Mc68881 => "68881",
            Self::Mc68882 => "68882",
            Self::Mc68040 => "68040",
        }
    }

    fn parse(expr: &Expr) -> Option<Self> {
        let raw = match expr {
            Expr::Identifier(name, _) | Expr::Register(name, _) | Expr::Number(name, _) => {
                name.as_str()
            }
            Expr::String(bytes, _) => std::str::from_utf8(bytes).ok()?,
            _ => return None,
        };

        match raw.to_ascii_lowercase().as_str() {
            "none" => Some(Self::None),
            "68881" => Some(Self::Mc68881),
            "68882" => Some(Self::Mc68882),
            "68040" => Some(Self::Mc68040),
            _ => None,
        }
    }
}

pub fn initial_runtime_state() -> HashMap<String, u32> {
    let mut state = HashMap::new();
    state.insert(FPU_TARGET_KEY.to_string(), FpuTarget::None.state_value());
    state
}

pub fn apply_runtime_directive(
    directive: &str,
    operands: &[Expr],
    cpu: CpuType,
    _ctx: &dyn AssemblerContext,
    state: &mut HashMap<String, u32>,
) -> Result<bool, String> {
    if !directive.eq_ignore_ascii_case("FPU") {
        return Ok(false);
    }

    if operands.len() != 1 {
        return Err(".fpu requires exactly one target: none, 68881, 68882, 68040".to_string());
    }

    let target = FpuTarget::parse(&operands[0])
        .ok_or_else(|| ".fpu requires one of: none, 68881, 68882, 68040".to_string())?;

    if !cpu_supports_target(cpu, target) {
        let legal_targets = legal_targets_for_cpu(cpu)
            .iter()
            .map(|target| target.display_name())
            .collect::<Vec<_>>()
            .join(", ");
        return Err(format!(
            "FPU target {} is not supported on {}; legal .fpu targets for {}: {}",
            target.display_name(),
            cpu.as_str(),
            cpu.as_str(),
            legal_targets
        ));
    }

    state.insert(FPU_TARGET_KEY.to_string(), target.state_value());
    Ok(true)
}

fn cpu_supports_target(cpu: CpuType, target: FpuTarget) -> bool {
    legal_targets_for_cpu(cpu).contains(&target)
}

fn legal_targets_for_cpu(cpu: CpuType) -> &'static [FpuTarget] {
    match cpu.as_str() {
        "m68020" | "m68030" => &[FpuTarget::None, FpuTarget::Mc68881, FpuTarget::Mc68882],
        "m68040" => &[FpuTarget::None, FpuTarget::Mc68040],
        "m68000" | "m68010" => &[FpuTarget::None],
        _ => &[FpuTarget::None],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use opcore::parser::Expr;
    use opcore::tokenizer::Span;

    struct DummyContext;

    impl registry::family::AssemblerContext for DummyContext {
        fn eval_expr(&self, _expr: &Expr) -> Result<i64, String> {
            Err("not used".to_string())
        }

        fn symbols(&self) -> &types::symbol::SymbolTable {
            panic!("not used")
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
            1
        }
    }

    fn ident(name: &str) -> Expr {
        Expr::Identifier(
            name.to_string(),
            Span {
                line: 1,
                col_start: 1,
                col_end: name.len(),
            },
        )
    }

    #[test]
    fn initial_runtime_state_defaults_to_no_fpu() {
        let state = initial_runtime_state();
        assert_eq!(state.get(FPU_TARGET_KEY), Some(&0));
    }

    #[test]
    fn apply_runtime_directive_rejects_illegal_pairings() {
        let ctx = DummyContext;
        let mut state = initial_runtime_state();
        let err = apply_runtime_directive(
            "FPU",
            &[ident("68881")],
            CpuType::new("m68040"),
            &ctx,
            &mut state,
        )
        .expect_err("pairing should be rejected");
        assert!(err.contains("68881"));
        assert!(err.contains("m68040"));
    }
}
