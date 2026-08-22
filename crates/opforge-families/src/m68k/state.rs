// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68000 family runtime state helpers.

use std::collections::HashMap;

use opcore::parser::Expr;
use registry::cpu::CpuType;
use registry::family::AssemblerContext;

pub const FPU_TARGET_KEY: &str = "m68k.fpu_target";
pub const APOLLO_MODE_KEY: &str = "m68k.apollo_mode";
pub const CPU_IS_68080_KEY: &str = "m68k.cpu_is_68080";
pub const CPU_LEVEL_KEY: &str = "m68k.cpu_level";
pub const RUNTIME_DIRECTIVE_IDS: &[&str] = &["FPU", "APOLLO"];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FpuTarget {
    None,
    Mc68881,
    Mc68882,
    Mc68040,
    Mc68080,
}

impl FpuTarget {
    fn state_value(self) -> u32 {
        match self {
            Self::None => 0,
            Self::Mc68881 => 1,
            Self::Mc68882 => 2,
            Self::Mc68040 => 3,
            Self::Mc68080 => 4,
        }
    }

    fn display_name(self) -> &'static str {
        match self {
            Self::None => "none",
            Self::Mc68881 => "68881",
            Self::Mc68882 => "68882",
            Self::Mc68040 => "68040",
            Self::Mc68080 => "68080",
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
            "68080" => Some(Self::Mc68080),
            _ => None,
        }
    }
}

pub fn initial_runtime_state(cpu: CpuType) -> HashMap<String, u32> {
    let mut state = HashMap::new();
    let default_fpu_target = if cpu.as_str() == "m68080" {
        FpuTarget::Mc68080
    } else {
        FpuTarget::None
    };
    state.insert(FPU_TARGET_KEY.to_string(), default_fpu_target.state_value());
    state.insert(APOLLO_MODE_KEY.to_string(), 0);
    state.insert(
        CPU_IS_68080_KEY.to_string(),
        u32::from(cpu.as_str() == "m68080"),
    );
    let cpu_level = match cpu.as_str() {
        "m68010" => 1,
        "m68020" => 2,
        "m68030" => 3,
        "m68040" => 4,
        "m68080" => 5,
        _ => 0,
    };
    state.insert(CPU_LEVEL_KEY.to_string(), cpu_level);
    state
}

pub fn apply_runtime_directive(
    directive: &str,
    operands: &[Expr],
    cpu: CpuType,
    _ctx: &dyn AssemblerContext,
    state: &mut HashMap<String, u32>,
) -> Result<bool, String> {
    if directive.eq_ignore_ascii_case("FPU") {
        if operands.len() != 1 {
            return Err(
                ".fpu requires exactly one target: none, 68881, 68882, 68040, 68080".to_string(),
            );
        }

        let target = FpuTarget::parse(&operands[0])
            .ok_or_else(|| ".fpu requires one of: none, 68881, 68882, 68040, 68080".to_string())?;

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
        return Ok(true);
    }

    if directive.eq_ignore_ascii_case("APOLLO") {
        if cpu.as_str() != "m68080" {
            return Err(format!(
                ".apollo is only supported on m68080 (active cpu: {})",
                cpu.as_str()
            ));
        }

        if operands.len() != 1 {
            return Err(".apollo requires exactly one state: on, off, 1, 0".to_string());
        }

        let raw = match &operands[0] {
            Expr::Identifier(name, _) | Expr::Register(name, _) | Expr::Number(name, _) => {
                name.to_ascii_lowercase()
            }
            Expr::String(bytes, _) => std::str::from_utf8(bytes)
                .map(|value| value.to_ascii_lowercase())
                .map_err(|_| ".apollo requires one of: on, off, 1, 0".to_string())?,
            _ => return Err(".apollo requires one of: on, off, 1, 0".to_string()),
        };

        let enabled = match raw.as_str() {
            "on" | "1" => 1,
            "off" | "0" => 0,
            _ => return Err(".apollo requires one of: on, off, 1, 0".to_string()),
        };

        state.insert(APOLLO_MODE_KEY.to_string(), enabled);
        return Ok(true);
    }

    Ok(false)
}

fn cpu_supports_target(cpu: CpuType, target: FpuTarget) -> bool {
    legal_targets_for_cpu(cpu).contains(&target)
}

fn legal_targets_for_cpu(cpu: CpuType) -> &'static [FpuTarget] {
    match cpu.as_str() {
        "m68020" | "m68030" => &[FpuTarget::None, FpuTarget::Mc68881, FpuTarget::Mc68882],
        "m68040" => &[FpuTarget::None, FpuTarget::Mc68040],
        "m68080" => &[FpuTarget::None, FpuTarget::Mc68080],
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
        let state = initial_runtime_state(CpuType::new("m68040"));
        assert_eq!(state.get(FPU_TARGET_KEY), Some(&0));
        assert_eq!(state.get(APOLLO_MODE_KEY), Some(&0));
        assert_eq!(state.get(CPU_IS_68080_KEY), Some(&0));
    }

    #[test]
    fn initial_runtime_state_defaults_to_integrated_fpu_and_apollo_off_for_m68080() {
        let state = initial_runtime_state(CpuType::new("m68080"));
        assert_eq!(state.get(FPU_TARGET_KEY), Some(&4));
        assert_eq!(state.get(APOLLO_MODE_KEY), Some(&0));
        assert_eq!(state.get(CPU_IS_68080_KEY), Some(&1));
    }

    #[test]
    fn apply_runtime_directive_rejects_illegal_pairings() {
        let ctx = DummyContext;
        let mut state = initial_runtime_state(CpuType::new("m68040"));
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

    #[test]
    fn apply_runtime_directive_accepts_68080_target_on_m68080() {
        let ctx = DummyContext;
        let mut state = initial_runtime_state(CpuType::new("m68080"));
        assert!(apply_runtime_directive(
            "FPU",
            &[ident("68080")],
            CpuType::new("m68080"),
            &ctx,
            &mut state,
        )
        .expect(".fpu 68080 should be accepted on m68080"));
        assert_eq!(state.get(FPU_TARGET_KEY), Some(&4));
    }

    #[test]
    fn apollo_directive_rejects_non_68080_cpu() {
        let ctx = DummyContext;
        let mut state = initial_runtime_state(CpuType::new("m68040"));
        let err = apply_runtime_directive(
            "APOLLO",
            &[ident("on")],
            CpuType::new("m68040"),
            &ctx,
            &mut state,
        )
        .expect_err("non-68080 CPU must be rejected");
        assert!(err.contains("m68080"));
        assert!(err.contains("m68040"));
    }

    #[test]
    fn apollo_directive_accepts_on_and_off_for_68080() {
        let ctx = DummyContext;
        let mut state = initial_runtime_state(CpuType::new("m68080"));

        assert!(apply_runtime_directive(
            "APOLLO",
            &[ident("on")],
            CpuType::new("m68080"),
            &ctx,
            &mut state,
        )
        .expect(".apollo on should be accepted"));
        assert_eq!(state.get(APOLLO_MODE_KEY), Some(&1));

        assert!(apply_runtime_directive(
            "APOLLO",
            &[ident("off")],
            CpuType::new("m68080"),
            &ctx,
            &mut state,
        )
        .expect(".apollo off should be accepted"));
        assert_eq!(state.get(APOLLO_MODE_KEY), Some(&0));
    }
}
