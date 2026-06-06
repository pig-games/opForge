// SPDX-License-Identifier: GPL-3.0-or-later

use opcore::parser::Expr;
use registry::family::{FamilyHandler, FamilyParseError};

use super::{FamilyOperand, MOS6502FamilyHandler, OperandForce};

#[derive(Clone, Debug)]
pub struct VmSelectorInput {
    pub shape_key: String,
    pub expr0: Option<Expr>,
    pub expr1: Option<Expr>,
    pub force: Option<OperandForce>,
}

pub fn selector_input_from_exprs(
    mnemonic: &str,
    exprs: &[Expr],
) -> Result<Option<VmSelectorInput>, FamilyParseError> {
    let family = MOS6502FamilyHandler::new();
    let parsed = family.parse_operands(mnemonic, exprs)?;
    Ok(selector_input_from_family_operands(parsed.as_slice()))
}

fn selector_input_from_family_operands(operands: &[FamilyOperand]) -> Option<VmSelectorInput> {
    match operands {
        [] => Some(VmSelectorInput {
            shape_key: "implied".to_string(),
            expr0: None,
            expr1: None,
            force: None,
        }),
        [operand] => selector_input_from_family_operand(operand),
        [FamilyOperand::Direct(first), FamilyOperand::Direct(second)] => Some(VmSelectorInput {
            shape_key: "pair_direct".to_string(),
            expr0: Some(first.clone()),
            expr1: Some(second.clone()),
            force: None,
        }),
        _ => None,
    }
}

fn selector_input_from_family_operand(operand: &FamilyOperand) -> Option<VmSelectorInput> {
    match operand {
        FamilyOperand::Accumulator(_) => Some(VmSelectorInput {
            shape_key: "accumulator".to_string(),
            expr0: None,
            expr1: None,
            force: None,
        }),
        FamilyOperand::Immediate(expr) => Some(VmSelectorInput {
            shape_key: "immediate".to_string(),
            expr0: Some(expr.clone()),
            expr1: None,
            force: None,
        }),
        FamilyOperand::Direct(expr) => Some(single_expr_input("direct", expr)),
        FamilyOperand::DirectX(expr) => Some(single_expr_input("direct_x", expr)),
        FamilyOperand::DirectY(expr) => Some(single_expr_input("direct_y", expr)),
        FamilyOperand::IndexedIndirectX(expr) | FamilyOperand::IndirectX(expr) => {
            Some(single_expr_input("indexed_indirect_x", expr))
        }
        FamilyOperand::IndirectIndexedY(expr) => {
            Some(single_expr_input("indirect_indexed_y", expr))
        }
        FamilyOperand::IndirectIndexedZ(expr) => {
            Some(single_expr_input("indirect_indexed_z", expr))
        }
        FamilyOperand::Indirect(expr) => Some(single_expr_input("indirect", expr)),
        FamilyOperand::IndirectLong(expr) => Some(single_expr_input("indirect_long", expr)),
        FamilyOperand::IndirectLongY(expr) => Some(single_expr_input("indirect_long_y", expr)),
        FamilyOperand::IndirectLongZ(expr) => Some(single_expr_input("indirect_long_z", expr)),
        FamilyOperand::StackRelative(expr) => Some(single_expr_input("stack_relative", expr)),
        FamilyOperand::StackRelativeIndirectIndexedY(expr) => {
            Some(single_expr_input("stack_relative_indirect_y", expr))
        }
        FamilyOperand::BlockMove { src, dst, .. } => Some(VmSelectorInput {
            shape_key: "pair_direct".to_string(),
            expr0: Some(src.clone()),
            expr1: Some(dst.clone()),
            force: None,
        }),
        FamilyOperand::Forced { inner, force, .. } => {
            let mut nested = selector_input_from_family_operand(inner.as_ref())?;
            nested.shape_key = format!("{}:force_{}", nested.shape_key, force_suffix(*force));
            nested.force = Some(*force);
            Some(nested)
        }
    }
}

fn single_expr_input(shape_key: &str, expr: &Expr) -> VmSelectorInput {
    VmSelectorInput {
        shape_key: shape_key.to_string(),
        expr0: Some(expr.clone()),
        expr1: None,
        force: None,
    }
}

fn force_suffix(force: OperandForce) -> &'static str {
    match force {
        OperandForce::DirectPage => "d",
        OperandForce::DataBank => "b",
        OperandForce::ProgramBank => "k",
        OperandForce::Long => "l",
    }
}

#[cfg(test)]
mod tests {
    use super::selector_input_from_exprs;
    use opcore::parser::Expr;
    use opcore::tokenizer::Span;

    #[test]
    fn selector_input_uses_family_pair_forms_for_bit_branch() {
        let span = Span::default();
        let exprs = [
            Expr::Number("18".to_string(), span),
            Expr::Number("4660".to_string(), span),
        ];
        let input = selector_input_from_exprs("BBR0", &exprs)
            .expect("parse bit branch")
            .expect("selector input");
        assert_eq!(input.shape_key, "pair_direct");
        assert!(input.expr0.is_some());
        assert!(input.expr1.is_some());
    }

    #[test]
    fn selector_input_preserves_force_suffixes_in_family_layer() {
        let span = Span::default();
        let exprs = [
            Expr::Number("8192".to_string(), span),
            Expr::Identifier("l".to_string(), span),
        ];
        let input = selector_input_from_exprs("LDA", &exprs)
            .expect("parse forced operand")
            .expect("selector input");
        assert_eq!(input.shape_key, "direct:force_l");
        assert!(input.force.is_some());
    }
}
