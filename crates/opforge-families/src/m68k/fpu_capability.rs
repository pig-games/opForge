// SPDX-License-Identifier: GPL-3.0-or-later

//! Shared wrapper-level Motorola 68k FPU capability validation.

use super::state;
use registry::family::{AssemblerContext, EncodeResult};

fn fpu_target_name(state_value: u32) -> &'static str {
    match state_value {
        1 => "68881",
        2 => "68882",
        3 => "68040",
        4 => "68080",
        _ => "none",
    }
}

pub(crate) fn validate_fpu_target_for_cpu(
    display_name: &str,
    ctx: &dyn AssemblerContext,
    cpu_name: &'static str,
    legal_targets: &[u32],
    legal_target_names: &'static str,
) -> Result<&'static str, EncodeResult<Vec<u8>>> {
    let target = ctx.cpu_state_flag(state::FPU_TARGET_KEY).unwrap_or(0);

    if target == 0 {
        return Err(EncodeResult::error(format!(
            "{display_name} requires an active .fpu target on {cpu_name}; legal .fpu targets for {cpu_name} FPU instructions: {legal_target_names}"
        )));
    }

    let target_name = fpu_target_name(target);
    if !legal_targets.contains(&target) {
        return Err(EncodeResult::error(format!(
            "{display_name} is not available with .fpu {target_name} on {cpu_name}; legal .fpu targets for {cpu_name} FPU instructions: {legal_target_names}"
        )));
    }

    Ok(target_name)
}

pub(crate) fn deferred_fpu_message_for_cpu(
    display_name: &str,
    target_name: &str,
    cpu_name: &'static str,
) -> EncodeResult<Vec<u8>> {
    EncodeResult::error(format!(
        "{display_name} is recognized for .fpu {target_name} on {cpu_name}, but FPU encoding is not yet implemented"
    ))
}

#[cfg(test)]
mod tests {
    use super::{deferred_fpu_message_for_cpu, validate_fpu_target_for_cpu};
    use crate::families::m68k::state;
    use crate::m68020::M68020CpuHandler;
    use crate::m68030::M68030CpuHandler;
    use crate::m68040::M68040CpuHandler;
    use crate::m68080::M68080CpuHandler;
    use opcore::parser::Expr;
    use registry::family::{AssemblerContext, CpuHandler, EncodeResult};
    use std::collections::HashMap;
    use types::symbol::SymbolTable;

    #[derive(Clone, Copy, Debug)]
    enum WrapperCpu {
        M68020,
        M68030,
        M68040,
        M68080,
    }

    impl WrapperCpu {
        fn name(self) -> &'static str {
            match self {
                Self::M68020 => "m68020",
                Self::M68030 => "m68030",
                Self::M68040 => "m68040",
                Self::M68080 => "m68080",
            }
        }
    }

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

    fn encode_wrapper_fpu_mnemonic(
        cpu: WrapperCpu,
        mnemonic: &str,
        target: u32,
    ) -> EncodeResult<Vec<u8>> {
        let ctx = TestContext::default().with_cpu_state_flag(state::FPU_TARGET_KEY, target);
        match cpu {
            WrapperCpu::M68020 => M68020CpuHandler::new().encode_instruction(mnemonic, &[], &ctx),
            WrapperCpu::M68030 => M68030CpuHandler::new().encode_instruction(mnemonic, &[], &ctx),
            WrapperCpu::M68040 => M68040CpuHandler::new().encode_instruction(mnemonic, &[], &ctx),
            WrapperCpu::M68080 => {
                M68080CpuHandler::new().encode_m68k_instruction(mnemonic, &[], &ctx)
            }
        }
    }

    #[test]
    fn fpu_mnemonics_accept_legal_targets_across_m68k_wrappers() {
        let cases = [
            (WrapperCpu::M68020, "FNOP", 1),
            (WrapperCpu::M68020, "FNOP", 2),
            (WrapperCpu::M68030, "FNOP", 1),
            (WrapperCpu::M68030, "FNOP", 2),
            (WrapperCpu::M68040, "FNOP", 3),
            (WrapperCpu::M68080, "FNOP", 4),
        ];

        for (cpu, mnemonic, target) in cases {
            match encode_wrapper_fpu_mnemonic(cpu, mnemonic, target) {
                EncodeResult::Ok(_) => {}
                other => panic!(
                    "expected {mnemonic} to accept .fpu {target} on {}, got {other:?}",
                    cpu.name()
                ),
            }
        }
    }

    #[test]
    fn fpu_mnemonics_report_target_matrix_diagnostics_across_m68k_wrappers() {
        let cases = [
            (
                WrapperCpu::M68020,
                "FMOVE",
                0,
                "FMOVE requires an active .fpu target on m68020; legal .fpu targets for m68020 FPU instructions: 68881, 68882",
            ),
            (
                WrapperCpu::M68020,
                "FMOVE",
                3,
                "FMOVE is not available with .fpu 68040 on m68020; legal .fpu targets for m68020 FPU instructions: 68881, 68882",
            ),
            (
                WrapperCpu::M68030,
                "FADD",
                0,
                "FADD requires an active .fpu target on m68030; legal .fpu targets for m68030 FPU instructions: 68881, 68882",
            ),
            (
                WrapperCpu::M68030,
                "FADD",
                3,
                "FADD is not available with .fpu 68040 on m68030; legal .fpu targets for m68030 FPU instructions: 68881, 68882",
            ),
            (
                WrapperCpu::M68040,
                "FSIN",
                0,
                "FSIN requires an active .fpu target on m68040; legal .fpu targets for m68040 FPU instructions: 68040",
            ),
            (
                WrapperCpu::M68040,
                "FSIN",
                1,
                "FSIN is not available with .fpu 68881 on m68040; legal .fpu targets for m68040 FPU instructions: 68040",
            ),
            (
                WrapperCpu::M68080,
                "FADD",
                0,
                "FADD requires an active .fpu target on m68080; legal .fpu targets for m68080 FPU instructions: 68080",
            ),
            (
                WrapperCpu::M68080,
                "FADD",
                1,
                "FADD is not available with .fpu 68881 on m68080; legal .fpu targets for m68080 FPU instructions: 68080",
            ),
        ];

        for (cpu, mnemonic, target, expected_message) in cases {
            match encode_wrapper_fpu_mnemonic(cpu, mnemonic, target) {
                EncodeResult::Error(message, None) => assert_eq!(message, expected_message),
                other => panic!(
                    "expected wrapper-level FPU target diagnostic for {mnemonic} on {}, got {other:?}",
                    cpu.name()
                ),
            }
        }
    }

    #[test]
    fn fpu_mnemonics_use_shared_deferred_diagnostics() {
        let cases = [
            (
                "FMOVE",
                "68881",
                "m68020",
                "FMOVE is recognized for .fpu 68881 on m68020, but FPU encoding is not yet implemented",
            ),
            (
                "FADD",
                "68882",
                "m68030",
                "FADD is recognized for .fpu 68882 on m68030, but FPU encoding is not yet implemented",
            ),
            (
                "FSIN",
                "68040",
                "m68040",
                "FSIN is recognized for .fpu 68040 on m68040, but FPU encoding is not yet implemented",
            ),
            (
                "FADD",
                "68080",
                "m68080",
                "FADD is recognized for .fpu 68080 on m68080, but FPU encoding is not yet implemented",
            ),
        ];

        for (display_name, target_name, cpu_name, expected_message) in cases {
            match deferred_fpu_message_for_cpu(display_name, target_name, cpu_name) {
                EncodeResult::Error(message, None) => assert_eq!(message, expected_message),
                other => panic!(
                    "expected shared deferred FPU diagnostic for {display_name} on {cpu_name}, got {other:?}"
                ),
            }
        }
    }

    #[test]
    fn fpu_mnemonics_shared_helper_resolves_68080_target_name() {
        let ctx = TestContext::default().with_cpu_state_flag(state::FPU_TARGET_KEY, 4);
        let actual_name = validate_fpu_target_for_cpu("FADD", &ctx, "m68080", &[4], "68080")
            .unwrap_or_else(|err| match err {
                EncodeResult::Error(message, _) => {
                    panic!("expected target-name lookup success, got {message}")
                }
                other => panic!("expected target-name lookup success, got {other:?}"),
            });

        assert_eq!(actual_name, "68080");
    }
}
