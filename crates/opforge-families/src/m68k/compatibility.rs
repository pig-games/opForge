// SPDX-License-Identifier: GPL-3.0-or-later

//! Shared compatibility validation helpers for the Motorola 68000 family.

use super::operand::{FamilyOperand, FullExtensionBase};
use super::{is_68080_address_bank_register, is_68080_data_bank_register, state};
use opcore::parser::Expr;
use registry::family::AssemblerContext;
use std::collections::HashSet;

fn collect_name(name: &str, out: &mut HashSet<String>) {
    if is_68080_data_bank_register(name) || is_68080_address_bank_register(name) {
        out.insert(name.to_ascii_uppercase());
    }
}

fn collect_expr(expr: &Expr, out: &mut HashSet<String>) {
    match expr {
        Expr::Register(name, _) | Expr::Identifier(name, _)
            if is_68080_data_bank_register(name) || is_68080_address_bank_register(name) =>
        {
            out.insert(name.to_ascii_uppercase());
        }
        Expr::Indirect(inner, _)
        | Expr::Immediate(inner, _)
        | Expr::IndirectLong(inner, _)
        | Expr::Unary { expr: inner, .. } => collect_expr(inner, out),
        Expr::List(items, _) | Expr::Tuple(items, _) => {
            for item in items {
                collect_expr(item, out);
            }
        }
        Expr::Index { base, index, .. }
        | Expr::Binary {
            left: base,
            right: index,
            ..
        } => {
            collect_expr(base, out);
            collect_expr(index, out);
        }
        Expr::Member { base, .. } => collect_expr(base, out),
        Expr::StructLiteral { fields, .. } => {
            for (_, value) in fields {
                collect_expr(value, out);
            }
        }
        Expr::Call { args, .. } => {
            for arg in args {
                collect_expr(arg, out);
            }
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
            ..
        } => {
            collect_expr(cond, out);
            collect_expr(then_expr, out);
            collect_expr(else_expr, out);
        }
        Expr::Range {
            start, end, step, ..
        } => {
            collect_expr(start, out);
            collect_expr(end, out);
            if let Some(step) = step {
                collect_expr(step, out);
            }
        }
        Expr::Register(_, _)
        | Expr::Identifier(_, _)
        | Expr::Dollar(_)
        | Expr::Number(_, _)
        | Expr::String(_, _)
        | Expr::Placeholder(_)
        | Expr::Error(_, _) => {}
    }
}

fn collect_68080_only_registers(operand: &FamilyOperand, out: &mut HashSet<String>) {
    match operand {
        FamilyOperand::DataRegister { register, .. }
        | FamilyOperand::AddressRegister { register, .. }
        | FamilyOperand::AddressIndirect { register, .. }
        | FamilyOperand::AddressPostincrement { register, .. }
        | FamilyOperand::AddressPredecrement { register, .. } => {
            collect_name(register, out);
        }
        FamilyOperand::AddressDisplacement { base, .. } => {
            collect_name(base, out);
        }
        FamilyOperand::AddressIndexed { base, index, .. } => {
            collect_name(base, out);
            collect_name(index, out);
        }
        FamilyOperand::RegisterPair { left, right, .. }
        | FamilyOperand::RegisterGroup {
            start: left,
            end: right,
            ..
        }
        | FamilyOperand::IndirectRegisterPair { left, right, .. } => {
            collect_name(left, out);
            collect_name(right, out);
        }
        FamilyOperand::PcIndexed { index, .. } => {
            collect_name(index, out);
        }
        FamilyOperand::FullExtension { base, index, .. } => {
            if let FullExtensionBase::Address(register) = base {
                collect_name(register, out);
            }
            if let Some(index) = index {
                collect_name(&index.register, out);
            }
        }
        FamilyOperand::BitField { base, .. } => {
            collect_68080_only_registers(base, out);
        }
        FamilyOperand::TextureOperand { expr, .. } => collect_expr(expr, out),
        FamilyOperand::SpecialRegister { .. }
        | FamilyOperand::ControlRegister { .. }
        | FamilyOperand::FpuDataRegister { .. }
        | FamilyOperand::FpuControlRegister { .. }
        | FamilyOperand::PcDisplacement { .. }
        | FamilyOperand::Absolute { .. }
        | FamilyOperand::RegisterList { .. }
        | FamilyOperand::BranchTarget { .. }
        | FamilyOperand::Immediate { .. } => {}
    }
}

pub(crate) fn validate_68080_register_compatibility(
    family_operands: &[FamilyOperand],
    ctx: &dyn AssemblerContext,
    cpu_name: &str,
) -> Result<(), String> {
    let is_68080 = ctx.cpu_state_flag(state::CPU_IS_68080_KEY).unwrap_or(0) != 0;
    if is_68080 {
        return Ok(());
    }

    let mut registers = HashSet::new();
    for operand in family_operands {
        collect_68080_only_registers(operand, &mut registers);
    }
    if registers.is_empty() {
        return Ok(());
    }

    let mut names = registers.into_iter().collect::<Vec<_>>();
    names.sort();
    Err(format!(
        "register {} requires .cpu 68080 and is not supported on {}",
        names.join(", "),
        cpu_name
    ))
}

#[cfg(test)]
mod tests {
    use super::{collect_68080_only_registers, validate_68080_register_compatibility};
    use crate::families::m68k::operand::{
        AbsoluteSize, BitFieldSelector, FamilyOperand, FullExtensionBase, FullExtensionIndex,
        IndexScale, IndexSize, MemoryIndirectionKind,
    };
    use crate::families::m68k::state;
    use opcore::parser::Expr;
    use opcore::tokenizer::Span;
    use registry::family::AssemblerContext;
    use std::collections::{HashMap, HashSet};
    use types::symbol::SymbolTable;

    fn span() -> Span {
        Span {
            line: 1,
            col_start: 1,
            col_end: 1,
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

    fn nested_bitfield_operand() -> FamilyOperand {
        FamilyOperand::BitField {
            base: Box::new(FamilyOperand::FullExtension {
                base_displacement: Some((
                    Expr::Number("4".to_string(), span()),
                    AbsoluteSize::Word,
                )),
                base: FullExtensionBase::Address("B2".to_string()),
                index: Some(FullExtensionIndex {
                    register: "E5".to_string(),
                    size: IndexSize::Long,
                    scale: IndexScale::Two,
                }),
                memory_indirection: Some(MemoryIndirectionKind::Preindexed),
                outer_displacement: None,
                span: span(),
            }),
            offset: BitFieldSelector::Immediate {
                expr: Expr::Number("1".to_string(), span()),
                span: span(),
            },
            width: BitFieldSelector::Immediate {
                expr: Expr::Number("8".to_string(), span()),
                span: span(),
            },
            span: span(),
        }
    }

    fn texture_operand() -> FamilyOperand {
        FamilyOperand::TextureOperand {
            expr: Expr::Tuple(
                vec![
                    Expr::Identifier("B3".to_string(), span()),
                    Expr::Indirect(
                        Box::new(Expr::Tuple(
                            vec![
                                Expr::Identifier("A0".to_string(), span()),
                                Expr::Identifier("E4".to_string(), span()),
                            ],
                            span(),
                        )),
                        span(),
                    ),
                ],
                span(),
            ),
            span: span(),
        }
    }

    #[test]
    fn m68080_register_collector_finds_nested_bitfield_registers() {
        let mut registers = HashSet::new();
        collect_68080_only_registers(&nested_bitfield_operand(), &mut registers);

        let mut names = registers.into_iter().collect::<Vec<_>>();
        names.sort();
        assert_eq!(names, vec!["B2", "E5"]);
    }

    #[test]
    fn m68080_register_collector_finds_nested_texture_registers() {
        let mut registers = HashSet::new();
        collect_68080_only_registers(&texture_operand(), &mut registers);

        let mut names = registers.into_iter().collect::<Vec<_>>();
        names.sort();
        assert_eq!(names, vec!["B3", "E4"]);
    }

    #[test]
    fn m68080_register_compatibility_reports_sorted_unique_nested_registers() {
        let ctx = TestContext::default();
        let operands = vec![
            FamilyOperand::RegisterPair {
                left: "E5".to_string(),
                right: "B1".to_string(),
                span: span(),
            },
            nested_bitfield_operand(),
            texture_operand(),
        ];

        let error = validate_68080_register_compatibility(&operands, &ctx, "m68040")
            .expect_err("expected non-68080 compatibility rejection");
        assert_eq!(
            error,
            "register B1, B2, B3, E4, E5 requires .cpu 68080 and is not supported on m68040"
        );
    }

    #[test]
    fn m68080_register_compatibility_allows_banked_registers_on_68080_cpu() {
        let ctx = TestContext::default().with_cpu_state_flag(state::CPU_IS_68080_KEY, 1);
        let operands = vec![nested_bitfield_operand(), texture_operand()];

        assert!(validate_68080_register_compatibility(&operands, &ctx, "m68040").is_ok());
    }
}
