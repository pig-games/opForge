// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Motorola 68000 family handler implementation.

use super::is_register;
use super::operand::{
    span_from_expr, span_from_exprs, AbsoluteSize, BitFieldSelector, ControlRegisterKind,
    FamilyOperand, FullExtensionBase, FullExtensionIndex, IndexScale, IndexSize,
    MemoryIndirectionKind, Operand, RegisterListRegister, SpecialRegisterKind,
};
use super::table::{
    parse_mnemonic, BitFieldMnemonic, BitMnemonic, ConditionCode, MnemonicKind, OperationSize,
    ShiftMnemonic,
};
use opcore::expression::expr_span;
use opcore::parser::{BinaryOp, Expr, UnaryOp};
use registry::family::{
    expr_has_unstable_symbols, AssemblerContext, EncodeResult, FamilyHandler, FamilyParseError,
};
use std::collections::HashSet;

#[derive(Debug, Default)]
pub struct M68KFamilyHandler;

const MAX_M68000_ABSOLUTE_ADDRESS: i64 = 0x00FF_FFFF;
type FullExtensionBaseDisplacement = Option<(Expr, AbsoluteSize)>;
type PreindexedIndirectInner = (
    FullExtensionBaseDisplacement,
    FullExtensionBase,
    Option<FullExtensionIndex>,
);
type PostindexedIndirectInner = (FullExtensionBaseDisplacement, FullExtensionBase);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum EffectiveAddressKind {
    DataRegister,
    AddressRegister,
    AddressIndirect,
    AddressPostincrement,
    AddressPredecrement,
    AddressDisplacement,
    AddressIndexed,
    PcDisplacement,
    PcIndexed,
    Absolute,
    Immediate,
}

#[derive(Debug)]
pub(crate) struct EncodedEffectiveAddress {
    pub(crate) bits: u16,
    pub(crate) extension: Vec<u8>,
    pub(crate) kind: EffectiveAddressKind,
}

#[derive(Clone, Copy, Debug)]
enum MovemRegisterToken {
    Register(RegisterListRegister, opcore::tokenizer::Span),
    Separator(opcore::tokenizer::Span),
    Range(opcore::tokenizer::Span),
}

impl M68KFamilyHandler {
    pub fn new() -> Self {
        Self
    }

    fn parse_register_name(expr: &Expr) -> Option<(String, opcore::tokenizer::Span)> {
        match expr {
            Expr::Register(name, span) | Expr::Identifier(name, span) if is_register(name) => {
                Some((name.to_ascii_uppercase(), *span))
            }
            _ => None,
        }
    }

    fn parse_data_register(expr: &Expr) -> Option<(String, opcore::tokenizer::Span)> {
        let (name, span) = Self::parse_register_name(expr)?;
        if name.starts_with('D') {
            Some((name, span))
        } else {
            None
        }
    }

    fn parse_address_register(expr: &Expr) -> Option<(String, opcore::tokenizer::Span)> {
        let (name, span) = Self::parse_register_name(expr)?;
        if name.starts_with('A') || name == "SP" {
            Some((name, span))
        } else {
            None
        }
    }

    fn parse_special_register(
        expr: &Expr,
    ) -> Option<(SpecialRegisterKind, opcore::tokenizer::Span)> {
        let (name, span) = match expr {
            Expr::Register(name, span) | Expr::Identifier(name, span) => {
                (name.to_ascii_uppercase(), *span)
            }
            _ => return None,
        };
        let register = match name.as_str() {
            "CCR" => SpecialRegisterKind::Ccr,
            "SR" => SpecialRegisterKind::Sr,
            "USP" => SpecialRegisterKind::Usp,
            _ => return None,
        };
        Some((register, span))
    }

    fn parse_control_register(
        expr: &Expr,
    ) -> Option<(ControlRegisterKind, opcore::tokenizer::Span)> {
        let (name, span) = match expr {
            Expr::Register(name, span) | Expr::Identifier(name, span) => {
                (name.to_ascii_uppercase(), *span)
            }
            _ => return None,
        };
        let register = match name.as_str() {
            "SFC" => ControlRegisterKind::Sfc,
            "DFC" => ControlRegisterKind::Dfc,
            "VBR" => ControlRegisterKind::Vbr,
            "CACR" => ControlRegisterKind::Cacr,
            "CAAR" => ControlRegisterKind::Caar,
            "MSP" => ControlRegisterKind::Msp,
            "ISP" => ControlRegisterKind::Isp,
            _ => return None,
        };
        Some((register, span))
    }

    fn parse_pc_register(expr: &Expr) -> Option<opcore::tokenizer::Span> {
        match expr {
            Expr::Register(name, span) | Expr::Identifier(name, span)
                if name.eq_ignore_ascii_case("PC") =>
            {
                Some(*span)
            }
            _ => None,
        }
    }

    fn parse_scaled_index_register(
        expr: &Expr,
    ) -> Option<(String, IndexSize, IndexScale, opcore::tokenizer::Span)> {
        if let Expr::Binary {
            op: BinaryOp::Multiply,
            left,
            right,
            ..
        } = expr
        {
            let scale = match right.as_ref() {
                Expr::Number(text, _) if text == "1" => IndexScale::One,
                Expr::Number(text, _) if text == "2" => IndexScale::Two,
                Expr::Number(text, _) if text == "4" => IndexScale::Four,
                Expr::Number(text, _) if text == "8" => IndexScale::Eight,
                _ => return None,
            };
            let (register, size, _, _) = Self::parse_scaled_index_register(left)?;
            return Some((register, size, scale, expr_span(expr)));
        }

        if let Expr::Register(name, span) | Expr::Identifier(name, span) = expr {
            let upper = name.to_ascii_uppercase();
            let (register, size) = if let Some(register) = upper.strip_suffix(".W") {
                (register.to_string(), IndexSize::Word)
            } else if let Some(register) = upper.strip_suffix(".L") {
                (register.to_string(), IndexSize::Long)
            } else if upper != "PC" && is_register(&upper) {
                (upper, IndexSize::Word)
            } else {
                return None;
            };
            if register != "PC" && is_register(&register) {
                return Some((register, size, IndexScale::One, *span));
            }
            return None;
        }

        let Expr::Member { base, field, span } = expr else {
            return None;
        };
        let (name, _) = Self::parse_register_name(base)?;
        let size = match field.to_ascii_uppercase().as_str() {
            "W" => IndexSize::Word,
            "L" => IndexSize::Long,
            _ => return None,
        };
        if name == "PC" {
            return None;
        }
        Some((name, size, IndexScale::One, *span))
    }

    fn parse_index_register(expr: &Expr) -> Option<(String, IndexSize, opcore::tokenizer::Span)> {
        let (register, size, scale, span) = Self::parse_scaled_index_register(expr)?;
        (scale == IndexScale::One).then_some((register, size, span))
    }

    fn parse_general_register(expr: &Expr) -> Option<(String, opcore::tokenizer::Span)> {
        Self::parse_data_register(expr).or_else(|| Self::parse_address_register(expr))
    }

    fn parse_pair_operand(
        &self,
        args: &[Expr],
        span: opcore::tokenizer::Span,
    ) -> Result<FamilyOperand, FamilyParseError> {
        let [left, right] = args else {
            return Err(FamilyParseError::new(
                "68020 register-pair syntax expects exactly two elements",
                span,
            ));
        };

        if let (Some((left_name, _)), Some((right_name, _))) = (
            Self::parse_general_register(left),
            Self::parse_general_register(right),
        ) {
            return Ok(FamilyOperand::RegisterPair {
                left: left_name,
                right: right_name,
                span,
            });
        }

        if let (Expr::Indirect(left_inner, _), Expr::Indirect(right_inner, _)) = (left, right) {
            let Some((left_name, _)) = Self::parse_general_register(left_inner.as_ref()) else {
                return Err(FamilyParseError::new(
                    "68020 indirect register pairs require simple (Rn) operands",
                    expr_span(left),
                ));
            };
            let Some((right_name, _)) = Self::parse_general_register(right_inner.as_ref()) else {
                return Err(FamilyParseError::new(
                    "68020 indirect register pairs require simple (Rn) operands",
                    expr_span(right),
                ));
            };
            return Ok(FamilyOperand::IndirectRegisterPair {
                left: left_name,
                right: right_name,
                span,
            });
        }

        Err(FamilyParseError::new(
            "68020 register-pair syntax requires Rn:Rn or (Rn):(Rn)",
            span,
        ))
    }

    fn parse_bit_field_selector(
        expr: &Expr,
        role: &str,
    ) -> Result<BitFieldSelector, FamilyParseError> {
        if let Some((register, span)) = Self::parse_data_register(expr) {
            return Ok(BitFieldSelector::DataRegister { register, span });
        }

        if Self::parse_register_name(expr).is_some() {
            return Err(FamilyParseError::new(
                format!("68020 bit-field {role} must be an expression or data register"),
                expr_span(expr),
            ));
        }

        Ok(BitFieldSelector::Immediate {
            expr: expr.clone(),
            span: expr_span(expr),
        })
    }

    fn parse_bit_field_operand(
        &self,
        args: &[Expr],
        span: opcore::tokenizer::Span,
    ) -> Result<FamilyOperand, FamilyParseError> {
        let [base, offset, width] = args else {
            return Err(FamilyParseError::new(
                "68020 bit-field syntax expects base, offset, and width",
                span,
            ));
        };
        let base = self.parse_single_operand(base)?;
        let offset = Self::parse_bit_field_selector(offset, "offset")?;
        let width = Self::parse_bit_field_selector(width, "width")?;

        Ok(FamilyOperand::BitField {
            base: Box::new(base),
            offset,
            width,
            span,
        })
    }

    fn parse_full_extension_displacement(
        expr: &Expr,
    ) -> Result<Option<(Expr, AbsoluteSize)>, FamilyParseError> {
        if matches!(expr, Expr::Placeholder(_)) {
            return Ok(None);
        }

        let (base, size) = match expr {
            Expr::Member { base, field, span } => {
                let size = match field.to_ascii_uppercase().as_str() {
                    "W" => AbsoluteSize::Word,
                    "L" => AbsoluteSize::Long,
                    _ => {
                        return Err(FamilyParseError::new(
                            "68020 full-extension base displacement requires explicit .W or .L",
                            *span,
                        ))
                    }
                };
                ((**base).clone(), size)
            }
            Expr::Identifier(name, span) => {
                let (base, size) = if let Some(base) = name.strip_suffix(".W") {
                    (base, AbsoluteSize::Word)
                } else if let Some(base) = name.strip_suffix(".L") {
                    (base, AbsoluteSize::Long)
                } else {
                    return Err(FamilyParseError::new(
                        "68020 full-extension base displacement requires explicit .W or .L",
                        *span,
                    ));
                };
                (Expr::Identifier(base.to_string(), *span), size)
            }
            _ => {
                return Err(FamilyParseError::new(
                    "68020 full-extension base displacement requires explicit .W or .L",
                    expr_span(expr),
                ))
            }
        };
        if Self::parse_register_name(&base).is_some() || matches!(base, Expr::Tuple(_, _)) {
            return Err(FamilyParseError::new(
                "68020 full-extension base displacement must be an expression, not a register form",
                expr_span(&base),
            ));
        }
        Ok(Some((base, size)))
    }

    fn parse_outer_displacement(
        expr: &Expr,
    ) -> Result<Option<(Expr, AbsoluteSize)>, FamilyParseError> {
        if matches!(expr, Expr::Placeholder(_)) {
            return Ok(None);
        }

        let (base, size) = match expr {
            Expr::Member { base, field, span } => {
                let size =
                    match field.to_ascii_uppercase().as_str() {
                        "W" => AbsoluteSize::Word,
                        "L" => AbsoluteSize::Long,
                        _ => return Err(FamilyParseError::new(
                            "68020 full-extension outer displacement requires explicit .W or .L",
                            *span,
                        )),
                    };
                ((**base).clone(), size)
            }
            Expr::Identifier(name, span) => {
                let (base, size) = if let Some(base) = name.strip_suffix(".W") {
                    (base, AbsoluteSize::Word)
                } else if let Some(base) = name.strip_suffix(".L") {
                    (base, AbsoluteSize::Long)
                } else {
                    return Err(FamilyParseError::new(
                        "68020 full-extension outer displacement requires explicit .W or .L",
                        *span,
                    ));
                };
                (Expr::Identifier(base.to_string(), *span), size)
            }
            _ => {
                return Err(FamilyParseError::new(
                    "68020 full-extension outer displacement requires explicit .W or .L",
                    expr_span(expr),
                ))
            }
        };
        if Self::parse_register_name(&base).is_some() || matches!(base, Expr::Tuple(_, _)) {
            return Err(FamilyParseError::new(
                "68020 full-extension outer displacement must be an expression, not a register form",
                expr_span(&base),
            ));
        }
        Ok(Some((base, size)))
    }

    fn parse_full_extension_base(expr: &Expr) -> Result<FullExtensionBase, FamilyParseError> {
        if matches!(expr, Expr::Placeholder(_)) {
            return Ok(FullExtensionBase::Suppressed);
        }
        if let Some((name, _)) = Self::parse_address_register(expr) {
            return Ok(FullExtensionBase::Address(name));
        }
        if Self::parse_pc_register(expr).is_some() {
            return Ok(FullExtensionBase::Pc);
        }
        Err(FamilyParseError::new(
            "invalid 68020 full-extension base register",
            expr_span(expr),
        ))
    }

    fn parse_full_extension_index(
        expr: &Expr,
    ) -> Result<Option<FullExtensionIndex>, FamilyParseError> {
        if matches!(expr, Expr::Placeholder(_)) {
            return Ok(None);
        }
        let Some((register, size, scale, _)) = Self::parse_scaled_index_register(expr) else {
            return Err(FamilyParseError::new(
                "invalid 68020 full-extension index register; expected Xn.W or Xn.L with optional *1, *2, *4, or *8",
                expr_span(expr),
            ));
        };
        Ok(Some(FullExtensionIndex {
            register,
            size,
            scale,
        }))
    }

    fn parse_full_extension_tuple(
        &self,
        elements: &[Expr],
        span: opcore::tokenizer::Span,
    ) -> Option<Result<FamilyOperand, FamilyParseError>> {
        let [displacement, base, index] = elements else {
            return None;
        };
        let has_later_family_scale = matches!(
            Self::parse_scaled_index_register(index),
            Some((_, _, scale, _)) if scale != IndexScale::One
        );
        if !matches!(displacement, Expr::Placeholder(_))
            && !matches!(base, Expr::Placeholder(_))
            && !matches!(index, Expr::Placeholder(_))
            && !matches!(displacement, Expr::Member { .. })
            && !matches!(
                displacement,
                Expr::Identifier(name, _) if name.to_ascii_uppercase().ends_with(".W")
                    || name.to_ascii_uppercase().ends_with(".L")
            )
            && !has_later_family_scale
        {
            return None;
        }

        Some((|| {
            let base_displacement = Self::parse_full_extension_displacement(displacement)?;
            let base = Self::parse_full_extension_base(base)?;
            let index = Self::parse_full_extension_index(index)?;

            if matches!(base, FullExtensionBase::Suppressed) && index.is_none() {
                return Err(FamilyParseError::new(
                    "68020 full-extension operand cannot suppress both base and index",
                    span,
                ));
            }

            Ok(FamilyOperand::FullExtension {
                base_displacement,
                base,
                index,
                memory_indirection: None,
                outer_displacement: None,
                span,
            })
        })())
    }

    fn build_full_extension_operand(
        base_displacement: Option<(Expr, AbsoluteSize)>,
        base: FullExtensionBase,
        index: Option<FullExtensionIndex>,
        memory_indirection: Option<MemoryIndirectionKind>,
        outer_displacement: Option<(Expr, AbsoluteSize)>,
        span: opcore::tokenizer::Span,
    ) -> Result<FamilyOperand, FamilyParseError> {
        if matches!(base, FullExtensionBase::Suppressed) && index.is_none() {
            return Err(FamilyParseError::new(
                "68020 full-extension operand cannot suppress both base and index",
                span,
            ));
        }

        Ok(FamilyOperand::FullExtension {
            base_displacement,
            base,
            index,
            memory_indirection,
            outer_displacement,
            span,
        })
    }

    fn parse_preindexed_indirect_inner(
        expr: &Expr,
    ) -> Result<PreindexedIndirectInner, FamilyParseError> {
        match expr {
            Expr::Tuple(elements, _) => match elements.as_slice() {
                [displacement, base, index] => Ok((
                    Self::parse_full_extension_displacement(displacement)?,
                    Self::parse_full_extension_base(base)?,
                    Self::parse_full_extension_index(index)?,
                )),
                [base, index] => {
                    let base = Self::parse_full_extension_base(base)?;
                    let Some(index) = Self::parse_full_extension_index(index)? else {
                        return Err(FamilyParseError::new(
                            "68020 preindexed alias requires an explicit index register",
                            expr_span(index),
                        ));
                    };
                    Ok((None, base, Some(index)))
                }
                _ => Err(FamilyParseError::new(
                    "invalid 68020 preindexed memory-indirect operand shape",
                    expr_span(expr),
                )),
            },
            _ => Err(FamilyParseError::new(
                "invalid 68020 preindexed memory-indirect operand shape",
                expr_span(expr),
            )),
        }
    }

    fn parse_postindexed_indirect_inner(
        expr: &Expr,
    ) -> Result<PostindexedIndirectInner, FamilyParseError> {
        match expr {
            Expr::Tuple(elements, _) => match elements.as_slice() {
                [displacement, base] => Ok((
                    Self::parse_full_extension_displacement(displacement)?,
                    Self::parse_full_extension_base(base)?,
                )),
                _ => Err(FamilyParseError::new(
                    "invalid 68020 postindexed memory-indirect operand shape",
                    expr_span(expr),
                )),
            },
            _ => Ok((None, Self::parse_full_extension_base(expr)?)),
        }
    }

    fn parse_preindexed_memory_indirect(
        &self,
        inner: &Expr,
        outer_displacement: Option<&Expr>,
        span: opcore::tokenizer::Span,
    ) -> Result<FamilyOperand, FamilyParseError> {
        let (base_displacement, base, index) = Self::parse_preindexed_indirect_inner(inner)?;
        let outer_displacement = match outer_displacement {
            Some(expr) => {
                let Some(displacement) = Self::parse_outer_displacement(expr)? else {
                    return Err(FamilyParseError::new(
                        "68020 preindexed memory-indirect outer displacement cannot be omitted explicitly",
                        expr_span(expr),
                    ));
                };
                Some(displacement)
            }
            None => None,
        };

        Self::build_full_extension_operand(
            base_displacement,
            base,
            index,
            Some(MemoryIndirectionKind::Preindexed),
            outer_displacement,
            span,
        )
    }

    fn parse_postindexed_memory_indirect(
        &self,
        inner: &Expr,
        index_expr: &Expr,
        outer_displacement: Option<&Expr>,
        span: opcore::tokenizer::Span,
    ) -> Result<FamilyOperand, FamilyParseError> {
        let (base_displacement, base) = Self::parse_postindexed_indirect_inner(inner)?;
        let index = Self::parse_full_extension_index(index_expr)?;
        let outer_displacement = match outer_displacement {
            Some(expr) => {
                let Some(displacement) = Self::parse_outer_displacement(expr)? else {
                    return Err(FamilyParseError::new(
                        "68020 postindexed memory-indirect outer displacement cannot be omitted explicitly",
                        expr_span(expr),
                    ));
                };
                Some(displacement)
            }
            None => None,
        };

        Self::build_full_extension_operand(
            base_displacement,
            base,
            index,
            Some(MemoryIndirectionKind::Postindexed),
            outer_displacement,
            span,
        )
    }

    fn parse_memory_indirect_tuple(
        &self,
        elements: &[Expr],
        span: opcore::tokenizer::Span,
    ) -> Result<FamilyOperand, FamilyParseError> {
        let Some((first, rest)) = elements.split_first() else {
            return Err(FamilyParseError::new(
                "invalid 68020 memory-indirect operand shape",
                span,
            ));
        };
        let Expr::IndirectLong(inner, _) = first else {
            return Err(FamilyParseError::new(
                "invalid 68020 memory-indirect operand shape",
                expr_span(first),
            ));
        };

        match rest {
            [second] => {
                if matches!(second, Expr::Placeholder(_))
                    || Self::parse_scaled_index_register(second).is_some()
                {
                    self.parse_postindexed_memory_indirect(inner, second, None, span)
                } else {
                    self.parse_preindexed_memory_indirect(inner, Some(second), span)
                }
            }
            [index, outer] => {
                self.parse_postindexed_memory_indirect(inner, index, Some(outer), span)
            }
            _ => Err(FamilyParseError::new(
                "invalid 68020 memory-indirect operand shape",
                span,
            )),
        }
    }

    fn parse_movem_register_list_register(
        expr: &Expr,
    ) -> Option<(RegisterListRegister, opcore::tokenizer::Span)> {
        if let Some((name, span)) = Self::parse_data_register(expr) {
            let reg = Self::data_register_number(&name)?;
            return Some((RegisterListRegister::Data(reg), span));
        }
        if let Some((name, span)) = Self::parse_address_register(expr) {
            let reg = Self::address_register_number(&name)?;
            return Some((RegisterListRegister::Address(reg), span));
        }
        None
    }

    fn movem_register_list_candidate(expr: &Expr) -> bool {
        if Self::parse_movem_register_list_register(expr).is_some() {
            return true;
        }

        matches!(
            expr,
            Expr::Binary {
                op: BinaryOp::Divide | BinaryOp::Subtract,
                ..
            }
        )
    }

    fn format_register_list_register(register: RegisterListRegister) -> String {
        match register {
            RegisterListRegister::Data(reg) => format!("D{reg}"),
            RegisterListRegister::Address(reg) => format!("A{reg}"),
        }
    }

    fn expand_movem_register_range(
        start: RegisterListRegister,
        end: RegisterListRegister,
        span: opcore::tokenizer::Span,
    ) -> Result<Vec<RegisterListRegister>, FamilyParseError> {
        match (start, end) {
            (RegisterListRegister::Data(start), RegisterListRegister::Data(end))
                if start <= end =>
            {
                Ok((start..=end).map(RegisterListRegister::Data).collect())
            }
            (RegisterListRegister::Address(start), RegisterListRegister::Address(end))
                if start <= end =>
            {
                Ok((start..=end).map(RegisterListRegister::Address).collect())
            }
            _ => Err(FamilyParseError::new(
                "MOVEM register ranges must stay within one ascending register family",
                span,
            )),
        }
    }

    fn flatten_movem_register_list_tokens(
        expr: &Expr,
        tokens: &mut Vec<MovemRegisterToken>,
    ) -> Result<(), FamilyParseError> {
        if let Some((register, span)) = Self::parse_movem_register_list_register(expr) {
            tokens.push(MovemRegisterToken::Register(register, span));
            return Ok(());
        }

        let Expr::Binary {
            op,
            left,
            right,
            span,
        } = expr
        else {
            return Err(FamilyParseError::new(
                "invalid MOVEM register list; expected data/address registers, ranges, and '/' separators",
                span_from_expr(expr),
            ));
        };

        match op {
            BinaryOp::Divide => {
                Self::flatten_movem_register_list_tokens(left, tokens)?;
                tokens.push(MovemRegisterToken::Separator(*span));
                Self::flatten_movem_register_list_tokens(right, tokens)
            }
            BinaryOp::Subtract => {
                Self::flatten_movem_register_list_tokens(left, tokens)?;
                tokens.push(MovemRegisterToken::Range(*span));
                Self::flatten_movem_register_list_tokens(right, tokens)
            }
            _ => Err(FamilyParseError::new(
                "invalid MOVEM register list; expected data/address registers, ranges, and '/' separators",
                *span,
            )),
        }
    }

    fn parse_movem_register_list(&self, expr: &Expr) -> Result<FamilyOperand, FamilyParseError> {
        let mut tokens = Vec::new();
        Self::flatten_movem_register_list_tokens(expr, &mut tokens)?;

        let mut registers = Vec::new();
        let mut seen = HashSet::new();
        let mut index = 0;

        while index < tokens.len() {
            let (start, start_span) = match tokens.get(index) {
                Some(MovemRegisterToken::Register(register, span)) => (*register, *span),
                Some(MovemRegisterToken::Separator(span))
                | Some(MovemRegisterToken::Range(span)) => {
                    return Err(FamilyParseError::new(
                        "MOVEM register list items must begin with a data or address register",
                        *span,
                    ))
                }
                None => break,
            };
            index += 1;

            let item_registers = if matches!(tokens.get(index), Some(MovemRegisterToken::Range(_)))
            {
                let range_span = match tokens.get(index) {
                    Some(MovemRegisterToken::Range(span)) => *span,
                    _ => unreachable!("checked above"),
                };
                index += 1;
                let Some(MovemRegisterToken::Register(end, _)) = tokens.get(index) else {
                    return Err(FamilyParseError::new(
                        "MOVEM register ranges must end with a data or address register",
                        range_span,
                    ));
                };
                index += 1;
                Self::expand_movem_register_range(start, *end, range_span)?
            } else {
                vec![start]
            };

            for register in item_registers {
                if !seen.insert(register) {
                    return Err(FamilyParseError::new(
                        format!(
                            "duplicate register in MOVEM list: {}",
                            Self::format_register_list_register(register)
                        ),
                        start_span,
                    ));
                }
                registers.push(register);
            }

            if index == tokens.len() {
                break;
            }

            match tokens.get(index) {
                Some(MovemRegisterToken::Separator(_)) => {
                    index += 1;
                }
                Some(MovemRegisterToken::Range(span)) => {
                    return Err(FamilyParseError::new(
                        "MOVEM register ranges must start with a data or address register",
                        *span,
                    ))
                }
                Some(MovemRegisterToken::Register(_, span)) => {
                    return Err(FamilyParseError::new(
                        "MOVEM register list items must be separated with '/'",
                        *span,
                    ))
                }
                None => break,
            }
        }

        Ok(FamilyOperand::RegisterList {
            registers,
            span: span_from_expr(expr),
        })
    }

    fn parse_movem_operands(&self, exprs: &[Expr]) -> Result<Vec<FamilyOperand>, FamilyParseError> {
        let [left, right] = exprs else {
            return Err(FamilyParseError::new(
                "MOVEM expects two operands",
                exprs.first().map(span_from_expr).unwrap_or_default(),
            ));
        };

        let left_candidate = Self::movem_register_list_candidate(left);
        let right_candidate = Self::movem_register_list_candidate(right);
        let mut deferred_error = None;

        if left_candidate {
            match (
                self.parse_movem_register_list(left),
                self.parse_single_operand(right),
            ) {
                (Ok(list), Ok(other)) => return Ok(vec![list, other]),
                (Err(err), _) => deferred_error = Some(err),
                (_, Err(err)) => deferred_error = Some(err),
            }
        }

        if right_candidate {
            match (
                self.parse_single_operand(left),
                self.parse_movem_register_list(right),
            ) {
                (Ok(other), Ok(list)) => return Ok(vec![other, list]),
                (Err(err), _) => {
                    deferred_error.get_or_insert(err);
                }
                (_, Err(err)) => {
                    deferred_error.get_or_insert(err);
                }
            }
        }

        if let Some(err) = deferred_error {
            return Err(err);
        }

        Ok(vec![
            self.parse_single_operand(left)?,
            self.parse_single_operand(right)?,
        ])
    }

    fn combined_unary_span(
        op_span: opcore::tokenizer::Span,
        expr: &Expr,
    ) -> opcore::tokenizer::Span {
        span_from_exprs(
            op_span,
            opcore::tokenizer::Span {
                line: expr_span(expr).line,
                col_start: expr_span(expr).col_start,
                col_end: expr_span(expr).col_end,
            },
        )
    }

    fn parse_indirect_tuple(
        &self,
        elements: &[Expr],
        span: opcore::tokenizer::Span,
    ) -> Result<FamilyOperand, FamilyParseError> {
        if let Some(result) = self.parse_full_extension_tuple(elements, span) {
            return result;
        }

        match elements {
            [first, second] => {
                if let Some((index_name, index_size, _)) = Self::parse_index_register(second) {
                    let zero = Expr::Number("0".to_string(), expr_span(first));
                    if let Some((name, _)) = Self::parse_address_register(first) {
                        return Ok(FamilyOperand::AddressIndexed {
                            displacement: zero,
                            base: name,
                            index: index_name,
                            index_size,
                            span,
                        });
                    }
                    if matches!(first, Expr::Register(name, _) | Expr::Identifier(name, _) if name.eq_ignore_ascii_case("PC"))
                    {
                        return Ok(FamilyOperand::PcIndexed {
                            displacement: zero,
                            index: index_name,
                            index_size,
                            span,
                        });
                    }
                }

                let displacement = first;
                let base = second;
                if let Some((name, _)) = Self::parse_address_register(base) {
                    return Ok(FamilyOperand::AddressDisplacement {
                        displacement: displacement.clone(),
                        base: name,
                        span,
                    });
                }
                if matches!(base, Expr::Register(name, _) | Expr::Identifier(name, _) if name.eq_ignore_ascii_case("PC"))
                {
                    return Ok(FamilyOperand::PcDisplacement {
                        displacement: displacement.clone(),
                        span,
                    });
                }
                Err(FamilyParseError::new(
                    "invalid 68000 displacement base register",
                    expr_span(base),
                ))
            }
            [displacement, base, index] => {
                let Some((index_name, index_size, _)) = Self::parse_index_register(index) else {
                    return Err(FamilyParseError::new(
                        "invalid 68000 index register; expected Xn.W or Xn.L",
                        expr_span(index),
                    ));
                };

                if let Some((name, _)) = Self::parse_address_register(base) {
                    return Ok(FamilyOperand::AddressIndexed {
                        displacement: displacement.clone(),
                        base: name,
                        index: index_name,
                        index_size,
                        span,
                    });
                }
                if matches!(base, Expr::Register(name, _) | Expr::Identifier(name, _) if name.eq_ignore_ascii_case("PC"))
                {
                    return Ok(FamilyOperand::PcIndexed {
                        displacement: displacement.clone(),
                        index: index_name,
                        index_size,
                        span,
                    });
                }
                Err(FamilyParseError::new(
                    "invalid 68000 indexed base register",
                    expr_span(base),
                ))
            }
            _ => Err(FamilyParseError::new(
                "invalid 68000 tuple operand shape",
                span,
            )),
        }
    }

    fn parse_single_operand(&self, expr: &Expr) -> Result<FamilyOperand, FamilyParseError> {
        if let Expr::Call { name, args, span } = expr {
            return match name.as_str() {
                ".pair" => self.parse_pair_operand(args, *span),
                ".bitfield" => self.parse_bit_field_operand(args, *span),
                _ => Err(FamilyParseError::new(
                    "unsupported Motorola 68000 operand form",
                    *span,
                )),
            };
        }

        if let Some((name, span)) = Self::parse_data_register(expr) {
            return Ok(FamilyOperand::DataRegister {
                register: name,
                span,
            });
        }
        if let Some((name, span)) = Self::parse_address_register(expr) {
            return Ok(FamilyOperand::AddressRegister {
                register: name,
                span,
            });
        }
        if let Some((register, span)) = Self::parse_special_register(expr) {
            return Ok(FamilyOperand::SpecialRegister { register, span });
        }
        if let Some((register, span)) = Self::parse_control_register(expr) {
            return Ok(FamilyOperand::ControlRegister { register, span });
        }

        match expr {
            Expr::Immediate(inner, span) => Ok(FamilyOperand::Immediate {
                expr: (**inner).clone(),
                span: *span,
            }),
            Expr::Indirect(inner, span) => match inner.as_ref() {
                Expr::Tuple(elements, _) => {
                    if matches!(elements.first(), Some(Expr::IndirectLong(_, _))) {
                        self.parse_memory_indirect_tuple(elements, *span)
                    } else {
                        self.parse_indirect_tuple(elements, *span)
                    }
                }
                Expr::IndirectLong(inner, _) => {
                    self.parse_preindexed_memory_indirect(inner, None, *span)
                }
                inner_expr => {
                    if Self::parse_pc_register(inner_expr).is_some() {
                        return Ok(FamilyOperand::PcDisplacement {
                            displacement: Expr::Number("0".to_string(), expr_span(inner_expr)),
                            span: *span,
                        });
                    }
                    let Some((name, _)) = Self::parse_address_register(inner_expr) else {
                        return Err(FamilyParseError::new(
                            "invalid 68000 indirect base register",
                            expr_span(inner_expr),
                        ));
                    };
                    Ok(FamilyOperand::AddressIndirect {
                        register: name,
                        span: *span,
                    })
                }
            },
            Expr::Unary {
                op: UnaryOp::Plus,
                expr: inner,
                span,
            } => {
                let Expr::Indirect(base, _) = inner.as_ref() else {
                    return Err(FamilyParseError::new(
                        "invalid 68000 postincrement operand",
                        expr_span(inner),
                    ));
                };
                let Some((name, _)) = Self::parse_address_register(base) else {
                    return Err(FamilyParseError::new(
                        "invalid 68000 postincrement base register",
                        expr_span(base),
                    ));
                };
                Ok(FamilyOperand::AddressPostincrement {
                    register: name,
                    span: Self::combined_unary_span(*span, inner),
                })
            }
            Expr::Unary {
                op: UnaryOp::Minus,
                expr: inner,
                span,
            } => {
                let Expr::Indirect(base, _) = inner.as_ref() else {
                    return Err(FamilyParseError::new(
                        "invalid 68000 predecrement operand",
                        expr_span(inner),
                    ));
                };
                let Some((name, _)) = Self::parse_address_register(base) else {
                    return Err(FamilyParseError::new(
                        "invalid 68000 predecrement base register",
                        expr_span(base),
                    ));
                };
                Ok(FamilyOperand::AddressPredecrement {
                    register: name,
                    span: Self::combined_unary_span(*span, inner),
                })
            }
            Expr::Member { base, field, span } => {
                let size = match field.to_ascii_uppercase().as_str() {
                    "W" => AbsoluteSize::Word,
                    "L" => AbsoluteSize::Long,
                    _ => {
                        return Err(FamilyParseError::new(
                            "invalid 68000 absolute size suffix; expected .W or .L",
                            *span,
                        ))
                    }
                };
                let inner = match base.as_ref() {
                    Expr::Indirect(inner, _) => inner,
                    other => {
                        if Self::parse_register_name(other).is_some()
                            || matches!(other, Expr::Tuple(_, _))
                        {
                            return Err(FamilyParseError::new(
                                "68000 absolute size suffix requires an expression, not a register form",
                                expr_span(other),
                            ));
                        }
                        return Ok(FamilyOperand::Absolute {
                            expr: other.clone(),
                            size,
                            span: *span,
                        });
                    }
                };
                if Self::parse_register_name(inner).is_some()
                    || matches!(inner.as_ref(), Expr::Tuple(_, _))
                {
                    return Err(FamilyParseError::new(
                        "68000 absolute size suffix requires an expression, not a register form",
                        expr_span(inner),
                    ));
                }
                Ok(FamilyOperand::Absolute {
                    expr: (**inner).clone(),
                    size,
                    span: *span,
                })
            }
            _ => Err(FamilyParseError::new(
                "unsupported Motorola 68000 operand form",
                span_from_expr(expr),
            )),
        }
    }

    fn is_branch_mnemonic(mnemonic: &str) -> bool {
        matches!(
            parse_mnemonic(mnemonic).map(|parsed| parsed.kind),
            Some(MnemonicKind::Bra | MnemonicKind::Bsr | MnemonicKind::Bcc(_))
        )
    }

    fn is_dbcc_mnemonic(mnemonic: &str) -> bool {
        matches!(
            parse_mnemonic(mnemonic).map(|parsed| parsed.kind),
            Some(MnemonicKind::Dbcc(_))
        )
    }

    fn encode_instruction_impl(
        &self,
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(parsed) = parse_mnemonic(mnemonic) else {
            return EncodeResult::NotFound;
        };

        if parsed.has_unknown_size_suffix {
            return EncodeResult::error(format!(
                "unsupported size suffix for {}",
                parsed.display_name
            ));
        }

        match parsed.kind {
            MnemonicKind::Move => self.encode_move(parsed.size, operands, ctx),
            MnemonicKind::MoveA => self.encode_movea(parsed.size, operands, ctx),
            MnemonicKind::Movem => self.encode_movem(parsed.size, operands, ctx),
            MnemonicKind::Movep => self.encode_movep(parsed.size, operands, ctx),
            MnemonicKind::Lea => self.encode_lea(parsed.size, operands, ctx),
            MnemonicKind::Pea => self.encode_pea(parsed.size, operands, ctx),
            MnemonicKind::Jmp => self.encode_jmp(parsed.size, operands, ctx),
            MnemonicKind::Jsr => self.encode_jsr(parsed.size, operands, ctx),
            MnemonicKind::Link if matches!(parsed.size, Some(OperationSize::Long)) => {
                EncodeResult::NotFound
            }
            MnemonicKind::Link => self.encode_link(parsed.size, operands, ctx),
            MnemonicKind::Unlk => self.encode_unlk(parsed.size, operands),
            MnemonicKind::Exg => self.encode_exg(parsed.size, operands),
            MnemonicKind::Swap => self.encode_swap(parsed.size, operands),
            MnemonicKind::Ext => self.encode_ext(parsed.size, operands),
            MnemonicKind::Trap => self.encode_trap(parsed.size, operands, ctx),
            MnemonicKind::Stop => self.encode_stop(parsed.size, operands, ctx),
            MnemonicKind::Nop => {
                self.encode_fixed_instruction("NOP", 0x4E71, parsed.size, operands)
            }
            MnemonicKind::Reset => {
                self.encode_fixed_instruction("RESET", 0x4E70, parsed.size, operands)
            }
            MnemonicKind::Rte => {
                self.encode_fixed_instruction("RTE", 0x4E73, parsed.size, operands)
            }
            MnemonicKind::Rtr => {
                self.encode_fixed_instruction("RTR", 0x4E77, parsed.size, operands)
            }
            MnemonicKind::Trapv => {
                self.encode_fixed_instruction("TRAPV", 0x4E76, parsed.size, operands)
            }
            MnemonicKind::Illegal => {
                self.encode_fixed_instruction("ILLEGAL", 0x4AFC, parsed.size, operands)
            }
            MnemonicKind::Add => {
                self.encode_data_register_binary_op("ADD", 0xD000, parsed.size, operands, ctx, true)
            }
            MnemonicKind::AddA => {
                self.encode_address_register_binary_op("ADDA", 0xD000, parsed.size, operands, ctx)
            }
            MnemonicKind::Addi => self.encode_immediate_binary_op(
                "ADDI",
                0x0600,
                parsed.size,
                operands,
                ctx,
                Self::data_alterable,
            ),
            MnemonicKind::Addx => {
                self.encode_extend_binary_op("ADDX", 0xD100, parsed.size, operands)
            }
            MnemonicKind::Abcd => {
                self.encode_decimal_adjust_instruction("ABCD", 0xC100, parsed.size, operands)
            }
            MnemonicKind::Chk => self.encode_chk(parsed.size, operands, ctx),
            MnemonicKind::Sub => {
                self.encode_data_register_binary_op("SUB", 0x9000, parsed.size, operands, ctx, true)
            }
            MnemonicKind::SubA => {
                self.encode_address_register_binary_op("SUBA", 0x9000, parsed.size, operands, ctx)
            }
            MnemonicKind::Subi => self.encode_immediate_binary_op(
                "SUBI",
                0x0400,
                parsed.size,
                operands,
                ctx,
                Self::data_alterable,
            ),
            MnemonicKind::Subx => {
                self.encode_extend_binary_op("SUBX", 0x9100, parsed.size, operands)
            }
            MnemonicKind::Sbcd => {
                self.encode_decimal_adjust_instruction("SBCD", 0x8100, parsed.size, operands)
            }
            MnemonicKind::Cmp => self.encode_cmp(parsed.size, operands, ctx),
            MnemonicKind::CmpA => {
                self.encode_address_register_binary_op("CMPA", 0xB000, parsed.size, operands, ctx)
            }
            MnemonicKind::Cmpi => self.encode_immediate_binary_op(
                "CMPI",
                0x0C00,
                parsed.size,
                operands,
                ctx,
                Self::data_addressing,
            ),
            MnemonicKind::Cmpm => self.encode_cmpm(parsed.size, operands),
            MnemonicKind::And => self.encode_data_register_binary_op(
                "AND",
                0xC000,
                parsed.size,
                operands,
                ctx,
                false,
            ),
            MnemonicKind::Andi => self.encode_immediate_binary_op(
                "ANDI",
                0x0200,
                parsed.size,
                operands,
                ctx,
                Self::data_alterable,
            ),
            MnemonicKind::Or => {
                self.encode_data_register_binary_op("OR", 0x8000, parsed.size, operands, ctx, false)
            }
            MnemonicKind::Ori => self.encode_immediate_binary_op(
                "ORI",
                0x0000,
                parsed.size,
                operands,
                ctx,
                Self::data_alterable,
            ),
            MnemonicKind::Eor => self.encode_eor(parsed.size, operands, ctx),
            MnemonicKind::Eori => self.encode_immediate_binary_op(
                "EORI",
                0x0A00,
                parsed.size,
                operands,
                ctx,
                Self::data_alterable,
            ),
            MnemonicKind::Divs => {
                self.encode_word_data_register_math("DIVS", 0x81C0, parsed.size, operands, ctx)
            }
            MnemonicKind::Divu => {
                self.encode_word_data_register_math("DIVU", 0x80C0, parsed.size, operands, ctx)
            }
            MnemonicKind::Bra if matches!(parsed.size, Some(OperationSize::Long)) => {
                EncodeResult::NotFound
            }
            MnemonicKind::Bra => {
                self.encode_branch(&parsed.display_name, None, parsed.size, operands, ctx)
            }
            MnemonicKind::Bsr if matches!(parsed.size, Some(OperationSize::Long)) => {
                EncodeResult::NotFound
            }
            MnemonicKind::Bsr => {
                self.encode_branch(&parsed.display_name, None, parsed.size, operands, ctx)
            }
            MnemonicKind::Bcc(_) if matches!(parsed.size, Some(OperationSize::Long)) => {
                EncodeResult::NotFound
            }
            MnemonicKind::Bcc(condition) => self.encode_branch(
                &parsed.display_name,
                Some(condition),
                parsed.size,
                operands,
                ctx,
            ),
            MnemonicKind::Dbcc(condition) => {
                self.encode_dbcc(&parsed.display_name, condition, parsed.size, operands, ctx)
            }
            MnemonicKind::Rts => self.encode_rts(parsed.size, operands),
            MnemonicKind::Moveq => self.encode_moveq(parsed.size, operands, ctx),
            MnemonicKind::Muls if matches!(parsed.size, Some(OperationSize::Long)) => {
                EncodeResult::NotFound
            }
            MnemonicKind::Muls => {
                self.encode_word_data_register_math("MULS", 0xC1C0, parsed.size, operands, ctx)
            }
            MnemonicKind::Mulu if matches!(parsed.size, Some(OperationSize::Long)) => {
                EncodeResult::NotFound
            }
            MnemonicKind::Mulu => {
                self.encode_word_data_register_math("MULU", 0xC0C0, parsed.size, operands, ctx)
            }
            MnemonicKind::Addq => self.encode_quick("ADDQ", false, parsed.size, operands, ctx),
            MnemonicKind::Subq => self.encode_quick("SUBQ", true, parsed.size, operands, ctx),
            MnemonicKind::Bit(kind) => self.encode_bit_op(kind, parsed.size, operands, ctx),
            MnemonicKind::Scc(condition) => {
                self.encode_scc(&parsed.display_name, condition, parsed.size, operands, ctx)
            }
            MnemonicKind::Clr => {
                self.encode_unary_data_instruction("CLR", 0x4200, parsed.size, operands, ctx)
            }
            MnemonicKind::Negx => {
                self.encode_unary_data_instruction("NEGX", 0x4000, parsed.size, operands, ctx)
            }
            MnemonicKind::Neg => {
                self.encode_unary_data_instruction("NEG", 0x4400, parsed.size, operands, ctx)
            }
            MnemonicKind::Nbcd => {
                self.encode_unsized_data_ea_instruction("NBCD", 0x4800, parsed.size, operands, ctx)
            }
            MnemonicKind::Not => {
                self.encode_unary_data_instruction("NOT", 0x4600, parsed.size, operands, ctx)
            }
            MnemonicKind::Tas => {
                self.encode_unsized_data_ea_instruction("TAS", 0x4AC0, parsed.size, operands, ctx)
            }
            MnemonicKind::Tst => {
                self.encode_unary_data_instruction("TST", 0x4A00, parsed.size, operands, ctx)
            }
            MnemonicKind::Shift(kind) => self.encode_shift(kind, parsed.size, operands, ctx),
        }
    }

    fn encode_move(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if operands
            .iter()
            .any(|operand| matches!(operand, Operand::SpecialRegister { .. }))
        {
            return self.encode_move_special(size, operands, ctx);
        }

        let Some(size) = size else {
            return EncodeResult::error("MOVE requires an explicit size suffix (.B, .W, or .L)");
        };
        let [src, dst] = operands else {
            return EncodeResult::error("MOVE expects two operands");
        };

        let src_ea = match self.encode_effective_address(src, Some(size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::move_allows_source(src_ea.kind, size) {
            return EncodeResult::error_with_span(
                format!("invalid source effective address for MOVE{}", size.suffix()),
                src.span(),
            );
        }

        let dst_ea = match self.encode_effective_address(dst, Some(size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::move_allows_destination(dst_ea.kind) {
            return EncodeResult::error_with_span(
                format!(
                    "invalid destination effective address for MOVE{}",
                    size.suffix()
                ),
                dst.span(),
            );
        }

        let opcode_base = match size {
            OperationSize::Byte => 0x1000,
            OperationSize::Long => 0x2000,
            OperationSize::Word => 0x3000,
        };
        let opcode = opcode_base
            | Self::move_destination_bits(dst_ea.bits)
            | Self::effective_address_bits(src_ea.bits);

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, opcode);
        bytes.extend_from_slice(&src_ea.extension);
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_move_special(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error("MOVE expects two operands");
        };

        let validate_size = |actual: Option<OperationSize>,
                             expected: OperationSize,
                             mnemonic: &str|
         -> Result<(), String> {
            match (actual, expected) {
                (None, _) => Ok(()),
                (Some(actual_size), expected_size) if actual_size == expected_size => Ok(()),
                (Some(OperationSize::Byte), _) => {
                    Err(format!("{mnemonic} does not support .B size"))
                }
                (Some(OperationSize::Word), _) => {
                    Err(format!("{mnemonic} does not support .W size"))
                }
                (Some(OperationSize::Long), _) => {
                    Err(format!("{mnemonic} does not support .L size"))
                }
            }
        };

        match (src, dst) {
            (
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Sr,
                    ..
                },
                dst,
            ) => {
                if let Err(message) = validate_size(size, OperationSize::Word, "MOVE SR") {
                    return EncodeResult::error(message);
                }
                let dst_ea =
                    match self.encode_effective_address(dst, Some(OperationSize::Word), ctx) {
                        Ok(ea) => ea,
                        Err(err) => return err,
                    };
                if !Self::data_alterable(dst_ea.kind) {
                    return EncodeResult::error_with_span(
                        "invalid destination effective address for MOVE SR",
                        dst.span(),
                    );
                }

                let mut bytes = Vec::new();
                Self::emit_word(
                    &mut bytes,
                    0x40C0 | Self::effective_address_bits(dst_ea.bits),
                );
                bytes.extend_from_slice(&dst_ea.extension);
                EncodeResult::ok(bytes)
            }
            (
                src,
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Ccr,
                    ..
                },
            ) => {
                if let Err(message) = validate_size(size, OperationSize::Word, "MOVE to CCR") {
                    return EncodeResult::error(message);
                }
                let src_ea =
                    match self.encode_effective_address(src, Some(OperationSize::Word), ctx) {
                        Ok(ea) => ea,
                        Err(err) => return err,
                    };
                if !Self::logic_allows_source(src_ea.kind) {
                    return EncodeResult::error_with_span(
                        "invalid source effective address for MOVE to CCR",
                        src.span(),
                    );
                }

                let mut bytes = Vec::new();
                Self::emit_word(
                    &mut bytes,
                    0x44C0 | Self::effective_address_bits(src_ea.bits),
                );
                bytes.extend_from_slice(&src_ea.extension);
                EncodeResult::ok(bytes)
            }
            (
                src,
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Sr,
                    ..
                },
            ) => {
                if let Err(message) = validate_size(size, OperationSize::Word, "MOVE to SR") {
                    return EncodeResult::error(message);
                }
                let src_ea =
                    match self.encode_effective_address(src, Some(OperationSize::Word), ctx) {
                        Ok(ea) => ea,
                        Err(err) => return err,
                    };
                if !Self::logic_allows_source(src_ea.kind) {
                    return EncodeResult::error_with_span(
                        "invalid source effective address for MOVE to SR",
                        src.span(),
                    );
                }

                let mut bytes = Vec::new();
                Self::emit_word(
                    &mut bytes,
                    0x46C0 | Self::effective_address_bits(src_ea.bits),
                );
                bytes.extend_from_slice(&src_ea.extension);
                EncodeResult::ok(bytes)
            }
            (
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Usp,
                    ..
                },
                Operand::AddressRegister { register, .. },
            ) => {
                if let Err(message) = validate_size(size, OperationSize::Long, "MOVE USP") {
                    return EncodeResult::error(message);
                }
                let Some(reg_bits) = Self::address_register_number(register) else {
                    return EncodeResult::error_with_span(
                        "invalid MOVE USP destination register",
                        dst.span(),
                    );
                };

                let mut bytes = Vec::new();
                Self::emit_word(&mut bytes, 0x4E68 | reg_bits as u16);
                EncodeResult::ok(bytes)
            }
            (
                Operand::AddressRegister { register, .. },
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Usp,
                    ..
                },
            ) => {
                if let Err(message) = validate_size(size, OperationSize::Long, "MOVE USP") {
                    return EncodeResult::error(message);
                }
                let Some(reg_bits) = Self::address_register_number(register) else {
                    return EncodeResult::error_with_span(
                        "invalid MOVE USP source register",
                        src.span(),
                    );
                };

                let mut bytes = Vec::new();
                Self::emit_word(&mut bytes, 0x4E60 | reg_bits as u16);
                EncodeResult::ok(bytes)
            }
            (
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Ccr,
                    ..
                },
                _,
            ) => EncodeResult::NotFound,
            (
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Usp,
                    ..
                },
                _,
            ) => EncodeResult::error_with_span(
                "MOVE USP destination must be an address register",
                dst.span(),
            ),
            (
                _,
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Usp,
                    ..
                },
            ) => EncodeResult::error_with_span(
                "MOVE USP source must be an address register",
                src.span(),
            ),
            _ => unreachable!("encode_move_special called without a special-register operand"),
        }
    }

    fn encode_movem(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error("MOVEM requires an explicit size suffix (.W or .L)");
        };
        if matches!(size, OperationSize::Byte) {
            return EncodeResult::error("MOVEM does not support .B size");
        }

        let [src, dst] = operands else {
            return EncodeResult::error("MOVEM expects two operands");
        };

        match (src, dst) {
            (Operand::RegisterList { .. }, Operand::RegisterList { .. }) => {
                EncodeResult::error("MOVEM expects exactly one register list operand")
            }
            (Operand::RegisterList { registers, .. }, dst) => {
                let dst_ea = match self.encode_effective_address(dst, Some(size), ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                if !Self::movem_register_to_memory_destination(dst_ea.kind) {
                    return EncodeResult::error_with_span(
                        "invalid destination effective address for MOVEM",
                        dst.span(),
                    );
                }

                let mut bytes = Vec::new();
                Self::emit_word(
                    &mut bytes,
                    0x4880 | Self::movem_size_bit(size) | Self::effective_address_bits(dst_ea.bits),
                );
                Self::emit_word(
                    &mut bytes,
                    Self::movem_register_mask(
                        registers,
                        matches!(dst_ea.kind, EffectiveAddressKind::AddressPredecrement),
                    ),
                );
                bytes.extend_from_slice(&dst_ea.extension);
                EncodeResult::ok(bytes)
            }
            (src, Operand::RegisterList { registers, .. }) => {
                let src_ea = match self.encode_effective_address(src, Some(size), ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };
                if !Self::movem_memory_to_register_source(src_ea.kind) {
                    return EncodeResult::error_with_span(
                        "invalid source effective address for MOVEM",
                        src.span(),
                    );
                }

                let mut bytes = Vec::new();
                Self::emit_word(
                    &mut bytes,
                    0x4C80 | Self::movem_size_bit(size) | Self::effective_address_bits(src_ea.bits),
                );
                Self::emit_word(&mut bytes, Self::movem_register_mask(registers, false));
                bytes.extend_from_slice(&src_ea.extension);
                EncodeResult::ok(bytes)
            }
            _ => EncodeResult::error("MOVEM expects exactly one register list operand"),
        }
    }

    fn encode_movep(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error("MOVEP requires an explicit size suffix (.W or .L)");
        };
        if matches!(size, OperationSize::Byte) {
            return EncodeResult::error("MOVEP does not support .B size");
        }

        let [src, dst] = operands else {
            return EncodeResult::error("MOVEP expects two operands");
        };

        let (data_register, memory_operand, opmode) = match (src, dst, size) {
            (
                Operand::DataRegister { register, .. },
                Operand::AddressDisplacement { .. },
                OperationSize::Word,
            ) => (register, dst, 0b110_u16),
            (
                Operand::DataRegister { register, .. },
                Operand::AddressDisplacement { .. },
                OperationSize::Long,
            ) => (register, dst, 0b111_u16),
            (
                Operand::AddressDisplacement { .. },
                Operand::DataRegister { register, .. },
                OperationSize::Word,
            ) => (register, src, 0b100_u16),
            (
                Operand::AddressDisplacement { .. },
                Operand::DataRegister { register, .. },
                OperationSize::Long,
            ) => (register, src, 0b101_u16),
            (Operand::DataRegister { .. }, _, _) | (_, Operand::DataRegister { .. }, _) => {
                return EncodeResult::error_with_span(
                    "MOVEP memory operand must use d16(An) addressing",
                    if matches!(src, Operand::DataRegister { .. }) {
                        dst.span()
                    } else {
                        src.span()
                    },
                );
            }
            _ => {
                return EncodeResult::error(
                    "MOVEP expects one data register operand and one d16(An) memory operand",
                )
            }
        };

        let Some(data_reg_bits) = Self::data_register_number(data_register) else {
            return EncodeResult::error_with_span(
                "MOVEP register operand must be a data register",
                if matches!(src, Operand::DataRegister { .. }) {
                    src.span()
                } else {
                    dst.span()
                },
            );
        };

        let Operand::AddressDisplacement {
            displacement, base, ..
        } = memory_operand
        else {
            unreachable!("MOVEP memory operand must be a displacement form");
        };
        let Some(address_reg_bits) = Self::address_register_number(base) else {
            return EncodeResult::error_with_span(
                "MOVEP memory operand base must be an address register",
                memory_operand.span(),
            );
        };
        let displacement = match Self::eval_expr(displacement, ctx) {
            Ok(value) => value,
            Err(err) => return EncodeResult::error_with_span(err, memory_operand.span()),
        };
        let Some(encoded_displacement) = Self::encode_signed_word(displacement) else {
            return EncodeResult::error_with_span(
                "MOVEP displacement out of 16-bit signed range",
                memory_operand.span(),
            );
        };

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            ((data_reg_bits as u16) << 9) | (opmode << 6) | 0x0008 | address_reg_bits as u16,
        );
        Self::emit_word(&mut bytes, encoded_displacement);
        EncodeResult::ok(bytes)
    }

    fn encode_movea(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error("MOVEA requires an explicit size suffix (.W or .L)");
        };
        if matches!(size, OperationSize::Byte) {
            return EncodeResult::error("MOVEA does not support .B size");
        }

        let [src, dst] = operands else {
            return EncodeResult::error("MOVEA expects two operands");
        };

        let dst_register = match dst {
            Operand::AddressRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    "MOVEA destination must be an address register",
                    dst.span(),
                )
            }
        };
        let Some(dst_reg) = Self::address_register_number(dst_register) else {
            return EncodeResult::error_with_span("invalid MOVEA destination register", dst.span());
        };

        let src_ea = match self.encode_effective_address(src, Some(size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::movea_allows_source(src_ea.kind) {
            return EncodeResult::error_with_span(
                format!(
                    "invalid source effective address for MOVEA{}",
                    size.suffix()
                ),
                src.span(),
            );
        }

        let opcode = match size {
            OperationSize::Word => 0x3040,
            OperationSize::Long => 0x2040,
            OperationSize::Byte => unreachable!("handled above"),
        } | ((dst_reg as u16) << 9)
            | Self::effective_address_bits(src_ea.bits);

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, opcode);
        bytes.extend_from_slice(&src_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_lea(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("LEA does not accept a size suffix");
        }

        let [src, dst] = operands else {
            return EncodeResult::error("LEA expects two operands");
        };

        let dst_register = match dst {
            Operand::AddressRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    "LEA destination must be an address register",
                    dst.span(),
                )
            }
        };
        let Some(dst_reg) = Self::address_register_number(dst_register) else {
            return EncodeResult::error_with_span("invalid LEA destination register", dst.span());
        };

        if !Self::single_ea_control_mode(Self::effective_address_kind(src)) {
            return EncodeResult::error_with_span(
                "invalid source effective address for LEA",
                src.span(),
            );
        }

        let src_ea = match self.encode_effective_address(src, None, ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0x41C0 | ((dst_reg as u16) << 9) | Self::effective_address_bits(src_ea.bits),
        );
        bytes.extend_from_slice(&src_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_pea(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.encode_single_ea_control_instruction(size, operands, ctx, "PEA", 0x4840)
    }

    fn encode_jmp(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.encode_single_ea_control_instruction(size, operands, ctx, "JMP", 0x4EC0)
    }

    fn encode_jsr(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.encode_single_ea_control_instruction(size, operands, ctx, "JSR", 0x4E80)
    }

    fn encode_single_ea_control_instruction(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        mnemonic: &str,
        opcode_base: u16,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
        }

        let [src] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects one operand"));
        };

        if !Self::single_ea_control_mode(Self::effective_address_kind(src)) {
            return EncodeResult::error_with_span(
                format!("invalid source effective address for {mnemonic}"),
                src.span(),
            );
        }

        let src_ea = match self.encode_effective_address(src, None, ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            opcode_base | Self::effective_address_bits(src_ea.bits),
        );
        bytes.extend_from_slice(&src_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_fixed_instruction(
        &self,
        mnemonic: &str,
        opcode: u16,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
        }
        if !operands.is_empty() {
            return EncodeResult::error(format!("{mnemonic} does not take operands"));
        }

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, opcode);
        EncodeResult::ok(bytes)
    }

    fn encode_link(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        match size {
            None | Some(OperationSize::Word) => {}
            Some(OperationSize::Long) => {
                return EncodeResult::error("LINK does not support .L size on baseline 68000");
            }
            Some(OperationSize::Byte) => {
                return EncodeResult::error("LINK does not support .B size");
            }
        }
        let [reg, displacement] = operands else {
            return EncodeResult::error(
                "LINK expects an address register and an immediate displacement",
            );
        };

        let reg_name = match reg {
            Operand::AddressRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    "LINK first operand must be an address register",
                    reg.span(),
                );
            }
        };
        let Some(reg_bits) = Self::address_register_number(reg_name) else {
            return EncodeResult::error_with_span("invalid LINK register", reg.span());
        };

        let Operand::Immediate { expr, .. } = displacement else {
            return EncodeResult::error_with_span(
                "LINK displacement must be an immediate value",
                displacement.span(),
            );
        };
        let (value, _) = match Self::eval_expr_or_placeholder(expr, ctx, 0) {
            Ok(result) => result,
            Err(err) => return EncodeResult::error_with_span(err, displacement.span()),
        };
        let Some(encoded) = Self::encode_signed_word(value) else {
            return EncodeResult::error_with_span(
                format!("LINK displacement {value} out of 16-bit signed range"),
                displacement.span(),
            );
        };

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x4E50 | reg_bits as u16);
        Self::emit_word(&mut bytes, encoded);
        EncodeResult::ok(bytes)
    }

    pub(crate) fn encode_link_long_instruction(
        &self,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [reg, displacement] = operands else {
            return EncodeResult::error(
                "LINK expects an address register and an immediate displacement",
            );
        };

        let reg_name = match reg {
            Operand::AddressRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    "LINK first operand must be an address register",
                    reg.span(),
                );
            }
        };
        let Some(reg_bits) = Self::address_register_number(reg_name) else {
            return EncodeResult::error_with_span("invalid LINK register", reg.span());
        };

        let Operand::Immediate { expr, .. } = displacement else {
            return EncodeResult::error_with_span(
                "LINK displacement must be an immediate value",
                displacement.span(),
            );
        };
        let (value, _) = match Self::eval_expr_or_placeholder(expr, ctx, 0) {
            Ok(result) => result,
            Err(err) => return EncodeResult::error_with_span(err, displacement.span()),
        };
        if !((i32::MIN as i64)..=(i32::MAX as i64)).contains(&value) {
            return EncodeResult::error_with_span(
                format!("LINK displacement {value} out of 32-bit signed range"),
                displacement.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x4808 | reg_bits as u16);
        Self::emit_long(&mut bytes, value as i32 as u32);
        EncodeResult::ok(bytes)
    }

    fn encode_unlk(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("UNLK does not accept a size suffix");
        }
        let [reg] = operands else {
            return EncodeResult::error("UNLK expects one address register operand");
        };
        let reg_name = match reg {
            Operand::AddressRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    "UNLK operand must be an address register",
                    reg.span(),
                );
            }
        };
        let Some(reg_bits) = Self::address_register_number(reg_name) else {
            return EncodeResult::error_with_span("invalid UNLK register", reg.span());
        };

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x4E58 | reg_bits as u16);
        EncodeResult::ok(bytes)
    }

    fn encode_swap(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("SWAP does not accept a size suffix");
        }
        let [reg] = operands else {
            return EncodeResult::error("SWAP expects one data register operand");
        };
        let reg_name = match reg {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    "SWAP operand must be a data register",
                    reg.span(),
                );
            }
        };
        let Some(reg_bits) = Self::data_register_number(reg_name) else {
            return EncodeResult::error_with_span("invalid SWAP register", reg.span());
        };

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x4840 | reg_bits as u16);
        EncodeResult::ok(bytes)
    }

    fn encode_exg(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("EXG does not accept a size suffix");
        }
        let [lhs, rhs] = operands else {
            return EncodeResult::error("EXG expects two register operands");
        };

        let encode_word = |opcode_base: u16, lhs_bits: u8, rhs_bits: u8| {
            let mut bytes = Vec::new();
            Self::emit_word(
                &mut bytes,
                opcode_base | ((lhs_bits as u16) << 9) | rhs_bits as u16,
            );
            EncodeResult::ok(bytes)
        };

        match (lhs, rhs) {
            (
                Operand::DataRegister {
                    register: lhs_register,
                    ..
                },
                Operand::DataRegister {
                    register: rhs_register,
                    ..
                },
            ) => {
                let Some(lhs_bits) = Self::data_register_number(lhs_register) else {
                    return EncodeResult::error_with_span("invalid EXG data register", lhs.span());
                };
                let Some(rhs_bits) = Self::data_register_number(rhs_register) else {
                    return EncodeResult::error_with_span("invalid EXG data register", rhs.span());
                };
                encode_word(0xC140, lhs_bits, rhs_bits)
            }
            (
                Operand::AddressRegister {
                    register: lhs_register,
                    ..
                },
                Operand::AddressRegister {
                    register: rhs_register,
                    ..
                },
            ) => {
                let Some(lhs_bits) = Self::address_register_number(lhs_register) else {
                    return EncodeResult::error_with_span(
                        "invalid EXG address register",
                        lhs.span(),
                    );
                };
                let Some(rhs_bits) = Self::address_register_number(rhs_register) else {
                    return EncodeResult::error_with_span(
                        "invalid EXG address register",
                        rhs.span(),
                    );
                };
                encode_word(0xC148, lhs_bits, rhs_bits)
            }
            (
                Operand::DataRegister {
                    register: data_register,
                    ..
                },
                Operand::AddressRegister {
                    register: address_register,
                    ..
                },
            ) => {
                let Some(data_bits) = Self::data_register_number(data_register) else {
                    return EncodeResult::error_with_span("invalid EXG data register", lhs.span());
                };
                let Some(address_bits) = Self::address_register_number(address_register) else {
                    return EncodeResult::error_with_span(
                        "invalid EXG address register",
                        rhs.span(),
                    );
                };
                encode_word(0xC188, data_bits, address_bits)
            }
            (
                Operand::AddressRegister {
                    register: address_register,
                    ..
                },
                Operand::DataRegister {
                    register: data_register,
                    ..
                },
            ) => {
                let Some(data_bits) = Self::data_register_number(data_register) else {
                    return EncodeResult::error_with_span("invalid EXG data register", rhs.span());
                };
                let Some(address_bits) = Self::address_register_number(address_register) else {
                    return EncodeResult::error_with_span(
                        "invalid EXG address register",
                        lhs.span(),
                    );
                };
                encode_word(0xC188, data_bits, address_bits)
            }
            _ => EncodeResult::error_with_span(
                "EXG operands must be data/address register pairs",
                lhs.span(),
            ),
        }
    }

    fn encode_ext(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        let opcode_base = match size {
            Some(OperationSize::Word) => 0x4880,
            Some(OperationSize::Long) => 0x48C0,
            Some(OperationSize::Byte) => {
                return EncodeResult::error("EXT does not support .B size")
            }
            None => return EncodeResult::error("EXT requires an explicit size suffix (.W or .L)"),
        };
        let [reg] = operands else {
            return EncodeResult::error("EXT expects one data register operand");
        };
        let reg_name = match reg {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    "EXT operand must be a data register",
                    reg.span(),
                );
            }
        };
        let Some(reg_bits) = Self::data_register_number(reg_name) else {
            return EncodeResult::error_with_span("invalid EXT register", reg.span());
        };

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, opcode_base | reg_bits as u16);
        EncodeResult::ok(bytes)
    }

    pub(crate) fn encode_extb_instruction(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        match size {
            Some(OperationSize::Long) => {}
            Some(OperationSize::Byte) => {
                return EncodeResult::error("EXTB does not support .B size");
            }
            Some(OperationSize::Word) => {
                return EncodeResult::error("EXTB does not support .W size");
            }
            None => return EncodeResult::error("EXTB requires an explicit .L size"),
        }

        let [reg] = operands else {
            return EncodeResult::error("EXTB expects one data register operand");
        };
        let reg_name = match reg {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    "EXTB operand must be a data register",
                    reg.span(),
                );
            }
        };
        let Some(reg_bits) = Self::data_register_number(reg_name) else {
            return EncodeResult::error_with_span("invalid EXTB register", reg.span());
        };

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x49C0 | reg_bits as u16);
        EncodeResult::ok(bytes)
    }

    fn encode_trap(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("TRAP does not accept a size suffix");
        }
        let [vector] = operands else {
            return EncodeResult::error("TRAP expects one immediate vector operand");
        };
        let Operand::Immediate { expr, .. } = vector else {
            return EncodeResult::error_with_span(
                "TRAP operand must be an immediate vector",
                vector.span(),
            );
        };
        let (value, _) = match Self::eval_expr_or_placeholder(expr, ctx, 0) {
            Ok(result) => result,
            Err(err) => return EncodeResult::error_with_span(err, vector.span()),
        };
        if !(0..=15).contains(&value) {
            return EncodeResult::error_with_span(
                format!("TRAP vector {value} out of range (0-15)"),
                vector.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x4E40 | value as u16);
        EncodeResult::ok(bytes)
    }

    fn encode_stop(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("STOP does not accept a size suffix");
        }
        let [value_operand] = operands else {
            return EncodeResult::error("STOP expects one immediate status word operand");
        };
        let Operand::Immediate { expr, .. } = value_operand else {
            return EncodeResult::error_with_span(
                "STOP operand must be an immediate status word",
                value_operand.span(),
            );
        };
        let (value, _) = match Self::eval_expr_or_placeholder(expr, ctx, 0) {
            Ok(result) => result,
            Err(err) => return EncodeResult::error_with_span(err, value_operand.span()),
        };
        let Some(immediate) = Self::encode_immediate(OperationSize::Word, value) else {
            return EncodeResult::error_with_span(
                format!("STOP immediate value {value} out of 16-bit range"),
                value_operand.span(),
            );
        };

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x4E72);
        bytes.extend_from_slice(&immediate);
        EncodeResult::ok(bytes)
    }

    fn encode_data_register_binary_op(
        &self,
        mnemonic: &str,
        opcode_base: u16,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        allow_address_register_source: bool,
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error(format!(
                "{mnemonic} requires an explicit size suffix (.B, .W, or .L)"
            ));
        };
        let [src, dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects two operands"));
        };

        if let Operand::DataRegister {
            register: src_register,
            ..
        } = src
        {
            let dst_kind = match dst {
                Operand::SpecialRegister { .. } => None,
                _ => Some(Self::effective_address_kind(dst)),
            };

            if dst_kind.is_some_and(Self::memory_alterable) {
                let Some(src_reg) = Self::data_register_number(src_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} source register"),
                        src.span(),
                    );
                };

                let dst_ea = match self.encode_effective_address(dst, Some(size), ctx) {
                    Ok(ea) => ea,
                    Err(err) => return err,
                };

                let opcode = opcode_base
                    | ((src_reg as u16) << 9)
                    | (Self::memory_destination_opmode(size) << 6)
                    | Self::effective_address_bits(dst_ea.bits);

                let mut bytes = Vec::new();
                Self::emit_word(&mut bytes, opcode);
                bytes.extend_from_slice(&dst_ea.extension);
                return EncodeResult::ok(bytes);
            }

            if dst_kind.is_none()
                || dst_kind.is_some_and(|kind| !matches!(kind, EffectiveAddressKind::DataRegister))
            {
                return EncodeResult::error_with_span(
                    format!(
                        "invalid destination effective address for {mnemonic}{}",
                        size.suffix()
                    ),
                    dst.span(),
                );
            }
        }

        let dst_register = match dst {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    format!("{mnemonic} destination must be a data register"),
                    dst.span(),
                )
            }
        };
        let Some(dst_reg) = Self::data_register_number(dst_register) else {
            return EncodeResult::error_with_span(
                format!("invalid {mnemonic} destination register"),
                dst.span(),
            );
        };

        let src_ea = match self.encode_effective_address(src, Some(size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        let src_ok = if allow_address_register_source {
            Self::alu_allows_source(src_ea.kind, size)
        } else {
            Self::logic_allows_source(src_ea.kind)
        };
        if !src_ok {
            return EncodeResult::error_with_span(
                format!(
                    "invalid source effective address for {mnemonic}{}",
                    size.suffix()
                ),
                src.span(),
            );
        }

        let opcode = opcode_base
            | ((dst_reg as u16) << 9)
            | (Self::data_register_opmode(size) << 6)
            | Self::effective_address_bits(src_ea.bits);

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, opcode);
        bytes.extend_from_slice(&src_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_cmp(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error("CMP requires an explicit size suffix (.B, .W, or .L)");
        };
        let [src, dst] = operands else {
            return EncodeResult::error("CMP expects two operands");
        };

        let dst_register = match dst {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    format!(
                        "invalid destination effective address for CMP{}",
                        size.suffix()
                    ),
                    dst.span(),
                )
            }
        };
        let Some(dst_reg) = Self::data_register_number(dst_register) else {
            return EncodeResult::error_with_span(
                format!(
                    "invalid destination effective address for CMP{}",
                    size.suffix()
                ),
                dst.span(),
            );
        };

        let src_ea = match self.encode_effective_address(src, Some(size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::alu_allows_source(src_ea.kind, size) {
            return EncodeResult::error_with_span(
                format!("invalid source effective address for CMP{}", size.suffix()),
                src.span(),
            );
        }

        let opcode = 0xB000
            | ((dst_reg as u16) << 9)
            | (Self::data_register_opmode(size) << 6)
            | Self::effective_address_bits(src_ea.bits);

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, opcode);
        bytes.extend_from_slice(&src_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_address_register_binary_op(
        &self,
        mnemonic: &str,
        opcode_base: u16,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error(format!(
                "{mnemonic} requires an explicit size suffix (.W or .L)"
            ));
        };
        if matches!(size, OperationSize::Byte) {
            return EncodeResult::error(format!("{mnemonic} does not support .B size"));
        }
        let [src, dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects two operands"));
        };

        let dst_register = match dst {
            Operand::AddressRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    format!("{mnemonic} destination must be an address register"),
                    dst.span(),
                )
            }
        };
        let Some(dst_reg) = Self::address_register_number(dst_register) else {
            return EncodeResult::error_with_span(
                format!("invalid {mnemonic} destination register"),
                dst.span(),
            );
        };

        let src_ea = match self.encode_effective_address(src, Some(size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::movea_allows_source(src_ea.kind) {
            return EncodeResult::error_with_span(
                format!(
                    "invalid source effective address for {mnemonic}{}",
                    size.suffix()
                ),
                src.span(),
            );
        }

        let opcode = opcode_base
            | ((dst_reg as u16) << 9)
            | (Self::address_register_opmode(size) << 6)
            | Self::effective_address_bits(src_ea.bits);

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, opcode);
        bytes.extend_from_slice(&src_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_eor(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error("EOR requires an explicit size suffix (.B, .W, or .L)");
        };
        let [src, dst] = operands else {
            return EncodeResult::error("EOR expects two operands");
        };

        let src_register = match src {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    "EOR source must be a data register",
                    src.span(),
                )
            }
        };
        let Some(src_reg) = Self::data_register_number(src_register) else {
            return EncodeResult::error_with_span("invalid EOR source register", src.span());
        };

        let dst_ea = match self.encode_effective_address(dst, Some(size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::eor_allows_destination(dst_ea.kind) {
            return EncodeResult::error_with_span(
                format!(
                    "invalid destination effective address for EOR{}",
                    size.suffix()
                ),
                dst.span(),
            );
        }

        let opcode = 0xB000
            | ((src_reg as u16) << 9)
            | (Self::eor_opmode(size) << 6)
            | Self::effective_address_bits(dst_ea.bits);

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, opcode);
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_word_data_register_math(
        &self,
        mnemonic: &str,
        opcode_base: u16,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        match size {
            None | Some(OperationSize::Word) => {}
            Some(OperationSize::Long) => {
                return EncodeResult::error(format!(
                    "{mnemonic} does not support .L size on baseline 68000"
                ))
            }
            Some(OperationSize::Byte) => {
                return EncodeResult::error(format!("{mnemonic} does not support .B size"))
            }
        }
        let [src, dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects two operands"));
        };

        let dst_register = match dst {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    format!("{mnemonic} destination must be a data register"),
                    dst.span(),
                )
            }
        };
        let Some(dst_reg) = Self::data_register_number(dst_register) else {
            return EncodeResult::error_with_span(
                format!("invalid {mnemonic} destination register"),
                dst.span(),
            );
        };

        let src_ea = match self.encode_effective_address(src, Some(OperationSize::Word), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::logic_allows_source(src_ea.kind) {
            return EncodeResult::error_with_span(
                format!("invalid source effective address for {mnemonic}"),
                src.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            opcode_base | ((dst_reg as u16) << 9) | Self::effective_address_bits(src_ea.bits),
        );
        bytes.extend_from_slice(&src_ea.extension);
        EncodeResult::ok(bytes)
    }

    pub(crate) fn encode_long_data_register_multiply(
        &self,
        mnemonic: &str,
        signed: bool,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects two operands"));
        };

        let dst_register = match dst {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    format!("{mnemonic} destination must be a data register"),
                    dst.span(),
                );
            }
        };
        let Some(dst_reg) = Self::data_register_number(dst_register) else {
            return EncodeResult::error_with_span(
                format!("invalid {mnemonic} destination register"),
                dst.span(),
            );
        };

        let src_ea = match self.encode_effective_address(src, Some(OperationSize::Long), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::logic_allows_source(src_ea.kind) {
            return EncodeResult::error_with_span(
                format!("invalid source effective address for {mnemonic}"),
                src.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0x4C00 | Self::effective_address_bits(src_ea.bits),
        );
        let extension = ((dst_reg as u16) << 12) | if signed { 1 << 11 } else { 0 } | (1 << 10);
        Self::emit_word(&mut bytes, extension);
        bytes.extend_from_slice(&src_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_chk(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        match size {
            None | Some(OperationSize::Word) => {}
            Some(OperationSize::Long) => {
                return EncodeResult::error("CHK does not support .L size on baseline 68000");
            }
            Some(OperationSize::Byte) => {
                return EncodeResult::error("CHK does not support .B size");
            }
        }
        let [src, dst] = operands else {
            return EncodeResult::error("CHK expects two operands");
        };

        let dst_register = match dst {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    "CHK destination must be a data register",
                    dst.span(),
                )
            }
        };
        let Some(dst_reg) = Self::data_register_number(dst_register) else {
            return EncodeResult::error_with_span("invalid CHK destination register", dst.span());
        };

        let src_ea = match self.encode_effective_address(src, Some(OperationSize::Word), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::logic_allows_source(src_ea.kind) {
            return EncodeResult::error_with_span(
                "invalid source effective address for CHK",
                src.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0x4180 | ((dst_reg as u16) << 9) | Self::effective_address_bits(src_ea.bits),
        );
        bytes.extend_from_slice(&src_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_extend_binary_op(
        &self,
        mnemonic: &str,
        opcode_base: u16,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error(format!(
                "{mnemonic} requires an explicit size suffix (.B, .W, or .L)"
            ));
        };
        let [src, dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects two operands"));
        };

        let (src_bits, dst_bits, mode_bits) = match (src, dst) {
            (
                Operand::DataRegister {
                    register: src_register,
                    ..
                },
                Operand::DataRegister {
                    register: dst_register,
                    ..
                },
            ) => {
                let Some(src_bits) = Self::data_register_number(src_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} source register"),
                        src.span(),
                    );
                };
                let Some(dst_bits) = Self::data_register_number(dst_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} destination register"),
                        dst.span(),
                    );
                };
                (src_bits, dst_bits, 0_u16)
            }
            (
                Operand::AddressPredecrement {
                    register: src_register,
                    ..
                },
                Operand::AddressPredecrement {
                    register: dst_register,
                    ..
                },
            ) => {
                let Some(src_bits) = Self::address_register_number(src_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} source register"),
                        src.span(),
                    );
                };
                let Some(dst_bits) = Self::address_register_number(dst_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} destination register"),
                        dst.span(),
                    );
                };
                (src_bits, dst_bits, 0x0008)
            }
            _ => {
                return EncodeResult::error_with_span(
                    format!(
                        "{mnemonic} operands must both be data registers or both be predecrement address operands"
                    ),
                    src.span(),
                )
            }
        };

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            opcode_base
                | ((dst_bits as u16) << 9)
                | (Self::size_bits(size) << 6)
                | mode_bits
                | src_bits as u16,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_decimal_adjust_instruction(
        &self,
        mnemonic: &str,
        opcode_base: u16,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
        }
        let [src, dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects two operands"));
        };

        let (src_bits, dst_bits, mode_bits) = match (src, dst) {
            (
                Operand::DataRegister {
                    register: src_register,
                    ..
                },
                Operand::DataRegister {
                    register: dst_register,
                    ..
                },
            ) => {
                let Some(src_bits) = Self::data_register_number(src_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} source register"),
                        src.span(),
                    );
                };
                let Some(dst_bits) = Self::data_register_number(dst_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} destination register"),
                        dst.span(),
                    );
                };
                (src_bits, dst_bits, 0_u16)
            }
            (
                Operand::AddressPredecrement {
                    register: src_register,
                    ..
                },
                Operand::AddressPredecrement {
                    register: dst_register,
                    ..
                },
            ) => {
                let Some(src_bits) = Self::address_register_number(src_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} source register"),
                        src.span(),
                    );
                };
                let Some(dst_bits) = Self::address_register_number(dst_register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} destination register"),
                        dst.span(),
                    );
                };
                (src_bits, dst_bits, 0x0008)
            }
            _ => {
                return EncodeResult::error_with_span(
                    format!(
                        "{mnemonic} operands must both be data registers or both be predecrement address operands"
                    ),
                    src.span(),
                )
            }
        };

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            opcode_base | ((dst_bits as u16) << 9) | mode_bits | src_bits as u16,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_cmpm(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error("CMPM requires an explicit size suffix (.B, .W, or .L)");
        };
        let [src, dst] = operands else {
            return EncodeResult::error("CMPM expects two operands");
        };

        let (
            Operand::AddressPostincrement {
                register: src_register,
                ..
            },
            Operand::AddressPostincrement {
                register: dst_register,
                ..
            },
        ) = (src, dst)
        else {
            return EncodeResult::error_with_span(
                "CMPM operands must both be postincrement address operands",
                src.span(),
            );
        };
        let Some(src_bits) = Self::address_register_number(src_register) else {
            return EncodeResult::error_with_span("invalid CMPM source register", src.span());
        };
        let Some(dst_bits) = Self::address_register_number(dst_register) else {
            return EncodeResult::error_with_span("invalid CMPM destination register", dst.span());
        };

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0xB108 | ((dst_bits as u16) << 9) | (Self::size_bits(size) << 6) | src_bits as u16,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_immediate_binary_op(
        &self,
        mnemonic: &str,
        opcode_base: u16,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
        destination_predicate: fn(EffectiveAddressKind) -> bool,
    ) -> EncodeResult<Vec<u8>> {
        if let [src, Operand::SpecialRegister { register, .. }] = operands {
            return self.encode_special_register_immediate_op(
                mnemonic,
                opcode_base,
                size,
                src,
                *register,
                ctx,
            );
        }

        let Some(size) = size else {
            return EncodeResult::error(format!(
                "{mnemonic} requires an explicit size suffix (.B, .W, or .L)"
            ));
        };
        let [src, dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects two operands"));
        };

        let Operand::Immediate { expr, .. } = src else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} source must be an immediate value"),
                src.span(),
            );
        };
        let (value, _) = match Self::eval_expr_or_placeholder(expr, ctx, 0) {
            Ok(result) => result,
            Err(err) => return EncodeResult::error_with_span(err, src.span()),
        };
        let Some(immediate) = Self::encode_immediate(size, value) else {
            return EncodeResult::error_with_span(
                format!("immediate value {value} out of range for {}", size.suffix()),
                src.span(),
            );
        };

        let dst_ea = match self.encode_effective_address(dst, Some(size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !destination_predicate(dst_ea.kind) {
            return EncodeResult::error_with_span(
                format!(
                    "invalid destination effective address for {mnemonic}{}",
                    size.suffix()
                ),
                dst.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            opcode_base | (Self::size_bits(size) << 6) | Self::effective_address_bits(dst_ea.bits),
        );
        bytes.extend_from_slice(&immediate);
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_special_register_immediate_op(
        &self,
        mnemonic: &str,
        opcode_base: u16,
        size: Option<OperationSize>,
        src: &Operand,
        register: SpecialRegisterKind,
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let (expected_size, opcode) = match register {
            SpecialRegisterKind::Ccr => (OperationSize::Byte, opcode_base | 0x003C),
            SpecialRegisterKind::Sr => (OperationSize::Word, opcode_base | 0x007C),
            SpecialRegisterKind::Usp => {
                return EncodeResult::error_with_span(
                    format!("{mnemonic} does not support USP destinations"),
                    src.span(),
                );
            }
        };

        match size {
            None => {}
            Some(actual) if actual == expected_size => {}
            Some(OperationSize::Byte) => {
                return EncodeResult::error(format!("{mnemonic} does not support .B size"));
            }
            Some(OperationSize::Word) => {
                return EncodeResult::error(format!("{mnemonic} does not support .W size"));
            }
            Some(OperationSize::Long) => {
                return EncodeResult::error(format!("{mnemonic} does not support .L size"));
            }
        }

        let Operand::Immediate { expr, .. } = src else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} source must be an immediate value"),
                src.span(),
            );
        };
        let (value, _) = match Self::eval_expr_or_placeholder(expr, ctx, 0) {
            Ok(result) => result,
            Err(err) => return EncodeResult::error_with_span(err, src.span()),
        };
        let Some(immediate) = Self::encode_immediate(expected_size, value) else {
            return EncodeResult::error_with_span(
                format!(
                    "immediate value {value} out of range for {}",
                    expected_size.suffix()
                ),
                src.span(),
            );
        };

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, opcode);
        bytes.extend_from_slice(&immediate);
        EncodeResult::ok(bytes)
    }

    fn encode_branch(
        &self,
        mnemonic: &str,
        condition: Option<ConditionCode>,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [target] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects one branch target"));
        };
        let Operand::BranchTarget { expr, .. } = target else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} requires a branch target expression"),
                target.span(),
            );
        };

        let condition_bits = match condition {
            Some(code) => code.opcode_bits(),
            None if mnemonic.eq_ignore_ascii_case("BRA") => 0x0,
            None => 0x1,
        };

        match size {
            Some(OperationSize::Long) => EncodeResult::error(format!(
                "{mnemonic} does not support .L size on baseline 68000"
            )),
            Some(OperationSize::Byte) => {
                let unresolved = Self::expr_is_unresolved(expr, ctx);
                let offset = if unresolved {
                    1
                } else {
                    let target_value = match ctx.eval_expr(expr) {
                        Ok(value) => Self::normalize_wrapped_i32(value),
                        Err(err) => {
                            return EncodeResult::error_with_span(err, target.span());
                        }
                    };
                    target_value - (ctx.current_address() as i64 + 2)
                };

                if !unresolved && offset == 0 {
                    return EncodeResult::error_with_span(
                        format!("{mnemonic}.B cannot encode a zero displacement; use {mnemonic}.W"),
                        target.span(),
                    );
                }
                let Some(encoded) = Self::encode_signed_byte(offset) else {
                    return EncodeResult::error_with_span(
                        format!("{mnemonic}.B branch displacement out of range: offset {offset}"),
                        target.span(),
                    );
                };

                let mut bytes = Vec::new();
                Self::emit_word(&mut bytes, 0x6000 | (condition_bits << 8) | encoded as u16);
                EncodeResult::ok(bytes)
            }
            Some(OperationSize::Word) => {
                let offset = if Self::expr_is_unresolved(expr, ctx) {
                    0
                } else {
                    let target_value = match ctx.eval_expr(expr) {
                        Ok(value) => Self::normalize_wrapped_i32(value),
                        Err(err) => {
                            return EncodeResult::error_with_span(err, target.span());
                        }
                    };
                    target_value - (ctx.current_address() as i64 + 2)
                };
                let Some(encoded) = Self::encode_signed_word(offset) else {
                    return EncodeResult::error_with_span(
                        format!("{mnemonic}.W branch displacement out of range: offset {offset}"),
                        target.span(),
                    );
                };

                let mut bytes = Vec::new();
                Self::emit_word(&mut bytes, 0x6000 | (condition_bits << 8));
                Self::emit_word(&mut bytes, encoded);
                EncodeResult::ok(bytes)
            }
            None => {
                let word_offset = if Self::expr_is_unresolved(expr, ctx) {
                    0
                } else {
                    let target_value = match ctx.eval_expr(expr) {
                        Ok(value) => Self::normalize_wrapped_i32(value),
                        Err(err) => {
                            return EncodeResult::error_with_span(err, target.span());
                        }
                    };
                    target_value - (ctx.current_address() as i64 + 2)
                };
                let Some(encoded) = Self::encode_signed_word(word_offset) else {
                    return EncodeResult::error_with_span(
                        format!(
                            "{mnemonic}.W branch displacement out of range: offset {word_offset}"
                        ),
                        target.span(),
                    );
                };

                let mut bytes = Vec::new();
                Self::emit_word(&mut bytes, 0x6000 | (condition_bits << 8));
                Self::emit_word(&mut bytes, encoded);
                EncodeResult::ok(bytes)
            }
        }
    }

    pub(crate) fn encode_long_branch_instruction(
        &self,
        mnemonic: &str,
        condition: Option<ConditionCode>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [target] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects one branch target"));
        };
        let Operand::BranchTarget { expr, .. } = target else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} requires a branch target expression"),
                target.span(),
            );
        };

        let condition_bits = match condition {
            Some(code) => code.opcode_bits(),
            None if mnemonic.eq_ignore_ascii_case("BRA") => 0x0,
            None => 0x1,
        };

        let offset = if Self::expr_is_unresolved(expr, ctx) {
            0
        } else {
            let target_value = match ctx.eval_expr(expr) {
                Ok(value) => Self::normalize_wrapped_i32(value),
                Err(err) => {
                    return EncodeResult::error_with_span(err, target.span());
                }
            };
            target_value - (ctx.current_address() as i64 + 2)
        };
        if !((i32::MIN as i64)..=(i32::MAX as i64)).contains(&offset) {
            return EncodeResult::error_with_span(
                format!("{mnemonic}.L branch displacement out of range: offset {offset}"),
                target.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x6000 | (condition_bits << 8) | 0x00FF);
        Self::emit_long(&mut bytes, offset as i32 as u32);
        EncodeResult::ok(bytes)
    }

    fn encode_dbcc(
        &self,
        mnemonic: &str,
        condition: ConditionCode,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
        }

        let [counter, target] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects a data register and target"));
        };
        let counter_register = match counter {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    format!("{mnemonic} counter must be a data register"),
                    counter.span(),
                );
            }
        };
        let Some(counter_bits) = Self::data_register_number(counter_register) else {
            return EncodeResult::error_with_span(
                format!("invalid {mnemonic} counter register"),
                counter.span(),
            );
        };
        let Operand::BranchTarget { expr, .. } = target else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} requires a branch target expression"),
                target.span(),
            );
        };

        let offset = if Self::expr_is_unresolved(expr, ctx) {
            0
        } else {
            let target_value = match ctx.eval_expr(expr) {
                Ok(value) => Self::normalize_wrapped_i32(value),
                Err(err) => {
                    return EncodeResult::error_with_span(err, target.span());
                }
            };
            target_value - (ctx.current_address() as i64 + 2)
        };
        let Some(encoded_displacement) = Self::encode_signed_word(offset) else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} branch displacement out of range: offset {offset}"),
                target.span(),
            );
        };

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0x50C8 | (condition.opcode_bits() << 8) | counter_bits as u16,
        );
        Self::emit_word(&mut bytes, encoded_displacement);
        EncodeResult::ok(bytes)
    }

    fn encode_scc(
        &self,
        mnemonic: &str,
        condition: ConditionCode,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
        }
        let [dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects one operand"));
        };

        let dst_ea = match self.encode_effective_address(dst, Some(OperationSize::Byte), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::data_alterable(dst_ea.kind) {
            return EncodeResult::error_with_span(
                format!("invalid destination effective address for {mnemonic}"),
                dst.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0x50C0 | (condition.opcode_bits() << 8) | Self::effective_address_bits(dst_ea.bits),
        );
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_rts(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("RTS does not accept a size suffix");
        }
        if !operands.is_empty() {
            return EncodeResult::error("RTS does not take operands");
        }

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x4E75);
        EncodeResult::ok(bytes)
    }

    fn encode_moveq(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("MOVEQ does not accept a size suffix");
        }
        let [src, dst] = operands else {
            return EncodeResult::error("MOVEQ expects two operands");
        };

        let value = match src {
            Operand::Immediate { expr, .. } => match Self::eval_expr(expr, ctx) {
                Ok(value) => value,
                Err(err) => return EncodeResult::error_with_span(err, src.span()),
            },
            _ => {
                return EncodeResult::error_with_span(
                    "MOVEQ source must be an immediate value",
                    src.span(),
                )
            }
        };
        if !(-128..=127).contains(&value) {
            return EncodeResult::error_with_span(
                format!("MOVEQ immediate value {value} out of signed 8-bit range"),
                src.span(),
            );
        }

        let dst_register = match dst {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    "MOVEQ destination must be a data register",
                    dst.span(),
                )
            }
        };
        let Some(dst_reg) = Self::data_register_number(dst_register) else {
            return EncodeResult::error_with_span("invalid MOVEQ destination register", dst.span());
        };

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0x7000 | ((dst_reg as u16) << 9) | value as i8 as u8 as u16,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_quick(
        &self,
        mnemonic: &str,
        subtract: bool,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error(format!(
                "{mnemonic} requires an explicit size suffix (.B, .W, or .L)"
            ));
        };
        let [src, dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects two operands"));
        };

        let Operand::Immediate { expr, .. } = src else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} source must be an immediate quick value"),
                src.span(),
            );
        };
        let (value, _) = match Self::eval_expr_or_placeholder(expr, ctx, 1) {
            Ok(result) => result,
            Err(err) => return EncodeResult::error_with_span(err, src.span()),
        };
        let Some(data_bits) = Self::quick_data_bits(value) else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} quick value {value} out of range (1-8)"),
                src.span(),
            );
        };

        let dst_ea = match self.encode_effective_address(dst, Some(size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::quick_allows_destination(dst_ea.kind, size) {
            return EncodeResult::error_with_span(
                format!(
                    "invalid destination effective address for {mnemonic}{}",
                    size.suffix()
                ),
                dst.span(),
            );
        }

        let mut bytes = Vec::new();
        let opcode_base = if subtract { 0x5100 } else { 0x5000 };
        Self::emit_word(
            &mut bytes,
            opcode_base
                | (data_bits << 9)
                | (Self::size_bits(size) << 6)
                | Self::effective_address_bits(dst_ea.bits),
        );
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_bit_op(
        &self,
        mnemonic: BitMnemonic,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(format!(
                "{} does not accept a size suffix",
                mnemonic.as_str()
            ));
        }
        let [bit, dst] = operands else {
            return EncodeResult::error(format!("{} expects two operands", mnemonic.as_str()));
        };

        let dst_kind = match dst {
            Operand::SpecialRegister { .. } => {
                return EncodeResult::error_with_span(
                    format!(
                        "invalid destination effective address for {}",
                        mnemonic.as_str()
                    ),
                    dst.span(),
                )
            }
            _ => Self::effective_address_kind(dst),
        };
        let dst_size = if matches!(dst_kind, EffectiveAddressKind::DataRegister) {
            OperationSize::Long
        } else {
            OperationSize::Byte
        };

        let dst_ea = match self.encode_effective_address(dst, Some(dst_size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };

        let destination_ok = if matches!(mnemonic, BitMnemonic::Btst) {
            Self::bit_test_allows_destination(dst_ea.kind)
        } else {
            Self::bit_modify_allows_destination(dst_ea.kind)
        };
        if !destination_ok {
            return EncodeResult::error_with_span(
                format!(
                    "invalid destination effective address for {}",
                    mnemonic.as_str()
                ),
                dst.span(),
            );
        }

        let mut bytes = Vec::new();
        match bit {
            Operand::DataRegister { register, .. } => {
                let Some(bit_register) = Self::data_register_number(register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {} bit-number register", mnemonic.as_str()),
                        bit.span(),
                    );
                };
                Self::emit_word(
                    &mut bytes,
                    mnemonic.dynamic_opcode_base()
                        | ((bit_register as u16) << 9)
                        | Self::effective_address_bits(dst_ea.bits),
                );
            }
            Operand::Immediate { expr, .. } => {
                let (value, _) = match Self::eval_expr_or_placeholder(expr, ctx, 0) {
                    Ok(result) => result,
                    Err(err) => return EncodeResult::error_with_span(err, bit.span()),
                };
                let Some(encoded_bit_number) = Self::encode_unsigned_byte(value) else {
                    return EncodeResult::error_with_span(
                        format!(
                            "{} bit number {value} out of range (0-255)",
                            mnemonic.as_str()
                        ),
                        bit.span(),
                    );
                };
                Self::emit_word(
                    &mut bytes,
                    mnemonic.static_opcode_base() | Self::effective_address_bits(dst_ea.bits),
                );
                Self::emit_word(&mut bytes, encoded_bit_number as u16);
            }
            _ => {
                return EncodeResult::error_with_span(
                    format!(
                        "{} bit number must be an immediate value or data register",
                        mnemonic.as_str()
                    ),
                    bit.span(),
                )
            }
        }
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_shift(
        &self,
        mnemonic: ShiftMnemonic,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if operands.len() == 1 {
            return self.encode_memory_shift(mnemonic, size, operands, ctx);
        }
        self.encode_register_shift(mnemonic, size, operands, ctx)
    }

    fn encode_register_shift(
        &self,
        mnemonic: ShiftMnemonic,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error(format!(
                "{} requires an explicit size suffix (.B, .W, or .L)",
                mnemonic.as_str()
            ));
        };
        let [count, dst] = operands else {
            return EncodeResult::error(format!("{} expects two operands", mnemonic.as_str()));
        };

        let dst_register = match dst {
            Operand::DataRegister { register, .. } => register,
            _ => {
                return EncodeResult::error_with_span(
                    format!("{} destination must be a data register", mnemonic.as_str()),
                    dst.span(),
                )
            }
        };
        let Some(dst_reg) = Self::data_register_number(dst_register) else {
            return EncodeResult::error_with_span(
                format!("invalid {} destination register", mnemonic.as_str()),
                dst.span(),
            );
        };

        let (count_bits, register_mode) = match count {
            Operand::Immediate { expr, .. } => {
                let (value, _) = match Self::eval_expr_or_placeholder(expr, ctx, 1) {
                    Ok(result) => result,
                    Err(err) => return EncodeResult::error_with_span(err, count.span()),
                };
                let Some(bits) = Self::quick_data_bits(value) else {
                    return EncodeResult::error_with_span(
                        format!(
                            "{} immediate shift count {value} out of range (1-8)",
                            mnemonic.as_str()
                        ),
                        count.span(),
                    );
                };
                (bits, false)
            }
            Operand::DataRegister { register, .. } => {
                let Some(bits) = Self::data_register_number(register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {} count register", mnemonic.as_str()),
                        count.span(),
                    );
                };
                (bits as u16, true)
            }
            _ => {
                return EncodeResult::error_with_span(
                    format!(
                        "{} count must be an immediate value or data register",
                        mnemonic.as_str()
                    ),
                    count.span(),
                )
            }
        };

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0xE000
                | (count_bits << 9)
                | (mnemonic.direction_bit() << 8)
                | (Self::size_bits(size) << 6)
                | ((register_mode as u16) << 5)
                | (mnemonic.kind_bits() << 3)
                | dst_reg as u16,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_memory_shift(
        &self,
        mnemonic: ShiftMnemonic,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        match size {
            None | Some(OperationSize::Word) => {}
            Some(OperationSize::Byte) => {
                return EncodeResult::error(format!(
                    "{} memory form does not support .B size",
                    mnemonic.as_str()
                ))
            }
            Some(OperationSize::Long) => {
                return EncodeResult::error(format!(
                    "{} memory form does not support .L size",
                    mnemonic.as_str()
                ))
            }
        }
        let [dst] = operands else {
            return EncodeResult::error(format!(
                "{} expects one operand for memory forms or two operands for register forms",
                mnemonic.as_str()
            ));
        };

        let dst_ea = match self.encode_effective_address(dst, Some(OperationSize::Word), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::memory_alterable(dst_ea.kind) {
            return EncodeResult::error_with_span(
                format!(
                    "invalid destination effective address for {}",
                    mnemonic.as_str()
                ),
                dst.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0xE0C0
                | (mnemonic.kind_bits() << 9)
                | (mnemonic.direction_bit() << 8)
                | Self::effective_address_bits(dst_ea.bits),
        );
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_unary_data_instruction(
        &self,
        mnemonic: &str,
        opcode_base: u16,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error(format!(
                "{mnemonic} requires an explicit size suffix (.B, .W, or .L)"
            ));
        };
        let [dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects one operand"));
        };

        let dst_ea = match self.encode_effective_address(dst, Some(size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::data_alterable(dst_ea.kind) {
            return EncodeResult::error_with_span(
                format!(
                    "invalid destination effective address for {mnemonic}{}",
                    size.suffix()
                ),
                dst.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            opcode_base | (Self::size_bits(size) << 6) | Self::effective_address_bits(dst_ea.bits),
        );
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_unsized_data_ea_instruction(
        &self,
        mnemonic: &str,
        opcode_base: u16,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
        }
        let [dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects one operand"));
        };

        let dst_ea = match self.encode_effective_address(dst, None, ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::data_alterable(dst_ea.kind) {
            return EncodeResult::error_with_span(
                format!("invalid destination effective address for {mnemonic}"),
                dst.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            opcode_base | Self::effective_address_bits(dst_ea.bits),
        );
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn m68020_cas_size_bits(
        mnemonic: &str,
        size: Option<OperationSize>,
    ) -> Result<u16, EncodeResult<Vec<u8>>> {
        let Some(size) = size else {
            return Err(EncodeResult::error(format!(
                "{mnemonic} requires an explicit size suffix"
            )));
        };
        match size {
            OperationSize::Byte => Ok(0b01),
            OperationSize::Word => Ok(0b10),
            OperationSize::Long => Ok(0b11),
        }
    }

    pub(crate) fn encode_cas_instruction(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let size_bits = match Self::m68020_cas_size_bits("CAS", size) {
            Ok(bits) => bits,
            Err(err) => return err,
        };
        let Some(size) = size else {
            unreachable!("validated above");
        };
        let [compare, update, destination] = operands else {
            return EncodeResult::error("CAS expects compare, update, and destination operands");
        };

        let Operand::DataRegister {
            register: compare_register,
            ..
        } = compare
        else {
            return EncodeResult::error_with_span(
                format!(
                    "CAS{} compare operand must be a data register",
                    size.suffix()
                ),
                compare.span(),
            );
        };
        let Some(compare_bits) = Self::data_register_number(compare_register) else {
            return EncodeResult::error_with_span(
                format!("invalid CAS{} compare register", size.suffix()),
                compare.span(),
            );
        };

        let Operand::DataRegister {
            register: update_register,
            ..
        } = update
        else {
            return EncodeResult::error_with_span(
                format!(
                    "CAS{} update operand must be a data register",
                    size.suffix()
                ),
                update.span(),
            );
        };
        let Some(update_bits) = Self::data_register_number(update_register) else {
            return EncodeResult::error_with_span(
                format!("invalid CAS{} update register", size.suffix()),
                update.span(),
            );
        };

        let destination_ea = match self.encode_effective_address(destination, Some(size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::memory_alterable(destination_ea.kind) {
            return EncodeResult::error_with_span(
                format!(
                    "invalid destination effective address for CAS{}",
                    size.suffix()
                ),
                destination.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0x08C0 | (size_bits << 9) | Self::effective_address_bits(destination_ea.bits),
        );
        Self::emit_word(
            &mut bytes,
            ((update_bits as u16) << 6) | compare_bits as u16,
        );
        bytes.extend_from_slice(&destination_ea.extension);
        EncodeResult::ok(bytes)
    }

    pub(crate) fn encode_cas2_instruction(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        let size_bits = match size {
            Some(OperationSize::Word) => 0b10,
            Some(OperationSize::Long) => 0b11,
            Some(OperationSize::Byte) => {
                return EncodeResult::error("CAS2 does not support .B size");
            }
            None => return EncodeResult::error("CAS2 requires an explicit .W or .L size suffix"),
        };
        let [compare_pair, update_pair, memory_pair] = operands else {
            return EncodeResult::error("CAS2 expects compare, update, and memory-pair operands");
        };

        let Operand::RegisterPair {
            left: compare_left,
            right: compare_right,
            ..
        } = compare_pair
        else {
            return EncodeResult::error_with_span(
                "CAS2 compare operand must be a data-register pair",
                compare_pair.span(),
            );
        };
        let (Some(compare_left_bits), Some(compare_right_bits)) = (
            Self::data_register_number(compare_left),
            Self::data_register_number(compare_right),
        ) else {
            return EncodeResult::error_with_span(
                "CAS2 compare operand must be a data-register pair",
                compare_pair.span(),
            );
        };

        let Operand::RegisterPair {
            left: update_left,
            right: update_right,
            ..
        } = update_pair
        else {
            return EncodeResult::error_with_span(
                "CAS2 update operand must be a data-register pair",
                update_pair.span(),
            );
        };
        let (Some(update_left_bits), Some(update_right_bits)) = (
            Self::data_register_number(update_left),
            Self::data_register_number(update_right),
        ) else {
            return EncodeResult::error_with_span(
                "CAS2 update operand must be a data-register pair",
                update_pair.span(),
            );
        };

        let Operand::IndirectRegisterPair {
            left: memory_left,
            right: memory_right,
            ..
        } = memory_pair
        else {
            return EncodeResult::error_with_span(
                "CAS2 memory operand must use (Rn):(Rn) register-pair syntax",
                memory_pair.span(),
            );
        };
        let (Some((memory_left_da, memory_left_bits)), Some((memory_right_da, memory_right_bits))) = (
            Self::general_register_name_descriptor(memory_left),
            Self::general_register_name_descriptor(memory_right),
        ) else {
            return EncodeResult::error_with_span(
                "CAS2 memory operand must use (Rn):(Rn) register-pair syntax",
                memory_pair.span(),
            );
        };

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x08FC | (size_bits << 9));
        Self::emit_word(
            &mut bytes,
            (memory_left_da << 15)
                | (memory_left_bits << 12)
                | ((update_left_bits as u16) << 6)
                | compare_left_bits as u16,
        );
        Self::emit_word(
            &mut bytes,
            (memory_right_da << 15)
                | (memory_right_bits << 12)
                | ((update_right_bits as u16) << 6)
                | compare_right_bits as u16,
        );
        EncodeResult::ok(bytes)
    }

    pub(crate) fn encode_chk2_cmp2_instruction(
        &self,
        mnemonic: &str,
        trap_on_failure: bool,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error(format!(
                "{mnemonic} requires an explicit size suffix (.B, .W, or .L)"
            ));
        };
        let [bounds, register] = operands else {
            return EncodeResult::error(format!(
                "{mnemonic} expects a bounds operand and register"
            ));
        };
        let Some((address_bit, register_bits)) = Self::general_register_descriptor(register) else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} register operand must be a data or address register"),
                register.span(),
            );
        };

        let bounds_ea = match self.encode_effective_address(bounds, Some(size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::single_ea_control_mode(bounds_ea.kind) {
            return EncodeResult::error_with_span(
                format!(
                    "invalid bounds effective address for {mnemonic}{}",
                    size.suffix()
                ),
                bounds.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0x00C0 | (Self::size_bits(size) << 9) | Self::effective_address_bits(bounds_ea.bits),
        );
        Self::emit_word(
            &mut bytes,
            (address_bit << 15) | (register_bits << 12) | ((trap_on_failure as u16) << 11),
        );
        bytes.extend_from_slice(&bounds_ea.extension);
        EncodeResult::ok(bytes)
    }

    fn encode_bit_field_selector_offset(
        selector: &BitFieldSelector,
        mnemonic: BitFieldMnemonic,
        ctx: &dyn AssemblerContext,
    ) -> Result<u16, EncodeResult<Vec<u8>>> {
        match selector {
            BitFieldSelector::DataRegister { register, span } => {
                let Some(bits) = Self::data_register_number(register) else {
                    return Err(EncodeResult::error_with_span(
                        format!("invalid {} offset register", mnemonic.as_str()),
                        *span,
                    ));
                };
                Ok((1_u16 << 11) | ((bits as u16) << 6))
            }
            BitFieldSelector::Immediate { expr, span } => {
                let (value, _) = match Self::eval_expr_or_placeholder(expr, ctx, 0) {
                    Ok(result) => result,
                    Err(err) => return Err(EncodeResult::error_with_span(err, *span)),
                };
                if !(0..=31).contains(&value) {
                    return Err(EncodeResult::error_with_span(
                        format!(
                            "{} bit-field offset {value} out of range (0-31)",
                            mnemonic.as_str()
                        ),
                        *span,
                    ));
                }
                Ok(((value as u16) & 0x1F) << 6)
            }
        }
    }

    fn encode_bit_field_selector_width(
        selector: &BitFieldSelector,
        mnemonic: BitFieldMnemonic,
        ctx: &dyn AssemblerContext,
    ) -> Result<u16, EncodeResult<Vec<u8>>> {
        match selector {
            BitFieldSelector::DataRegister { register, span } => {
                let Some(bits) = Self::data_register_number(register) else {
                    return Err(EncodeResult::error_with_span(
                        format!("invalid {} width register", mnemonic.as_str()),
                        *span,
                    ));
                };
                Ok((1_u16 << 5) | bits as u16)
            }
            BitFieldSelector::Immediate { expr, span } => {
                let (value, _) = match Self::eval_expr_or_placeholder(expr, ctx, 1) {
                    Ok(result) => result,
                    Err(err) => return Err(EncodeResult::error_with_span(err, *span)),
                };
                if !(1..=32).contains(&value) {
                    return Err(EncodeResult::error_with_span(
                        format!(
                            "{} bit-field width {value} out of range (1-32)",
                            mnemonic.as_str()
                        ),
                        *span,
                    ));
                }
                Ok((value as u16) & 0x1F)
            }
        }
    }

    pub(crate) fn encode_bit_field_instruction(
        &self,
        mnemonic: BitFieldMnemonic,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(format!(
                "{} does not accept a size suffix",
                mnemonic.as_str()
            ));
        }

        let (register_bits, bit_field_operand) = match mnemonic {
            BitFieldMnemonic::Bftst
            | BitFieldMnemonic::Bfchg
            | BitFieldMnemonic::Bfclr
            | BitFieldMnemonic::Bfset => {
                let [bit_field] = operands else {
                    return EncodeResult::error(format!(
                        "{} expects one operand",
                        mnemonic.as_str()
                    ));
                };
                (0_u16, bit_field)
            }
            BitFieldMnemonic::Bfextu | BitFieldMnemonic::Bfexts | BitFieldMnemonic::Bfffo => {
                let [bit_field, register_operand] = operands else {
                    return EncodeResult::error(format!(
                        "{} expects a bit-field operand and data-register destination",
                        mnemonic.as_str()
                    ));
                };
                let Operand::DataRegister { register, .. } = register_operand else {
                    return EncodeResult::error_with_span(
                        format!("{} destination must be a data register", mnemonic.as_str()),
                        register_operand.span(),
                    );
                };
                let Some(bits) = Self::data_register_number(register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {} destination register", mnemonic.as_str()),
                        register_operand.span(),
                    );
                };
                ((bits as u16) << 12, bit_field)
            }
            BitFieldMnemonic::Bfins => {
                let [register_operand, bit_field] = operands else {
                    return EncodeResult::error(
                        "BFINS expects a data-register source and bit-field destination",
                    );
                };
                let Operand::DataRegister { register, .. } = register_operand else {
                    return EncodeResult::error_with_span(
                        "BFINS source must be a data register",
                        register_operand.span(),
                    );
                };
                let Some(bits) = Self::data_register_number(register) else {
                    return EncodeResult::error_with_span(
                        "invalid BFINS source register",
                        register_operand.span(),
                    );
                };
                ((bits as u16) << 12, bit_field)
            }
        };

        let Operand::BitField {
            base,
            offset,
            width,
            ..
        } = bit_field_operand
        else {
            return EncodeResult::error_with_span(
                format!("{} expects bit-field brace syntax", mnemonic.as_str()),
                bit_field_operand.span(),
            );
        };

        let base_ea = match self.encode_effective_address(base.as_ref(), None, ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        let base_ok = match mnemonic {
            BitFieldMnemonic::Bftst
            | BitFieldMnemonic::Bfextu
            | BitFieldMnemonic::Bfexts
            | BitFieldMnemonic::Bfffo => Self::bit_field_read_mode(base_ea.kind),
            BitFieldMnemonic::Bfchg
            | BitFieldMnemonic::Bfclr
            | BitFieldMnemonic::Bfset
            | BitFieldMnemonic::Bfins => Self::bit_field_write_mode(base_ea.kind),
        };
        if !base_ok {
            return EncodeResult::error_with_span(
                format!(
                    "invalid bit-field effective address for {}",
                    mnemonic.as_str()
                ),
                base.span(),
            );
        }

        let offset_bits = match Self::encode_bit_field_selector_offset(offset, mnemonic, ctx) {
            Ok(bits) => bits,
            Err(err) => return err,
        };
        let width_bits = match Self::encode_bit_field_selector_width(width, mnemonic, ctx) {
            Ok(bits) => bits,
            Err(err) => return err,
        };

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            mnemonic.opcode_base() | Self::effective_address_bits(base_ea.bits),
        );
        Self::emit_word(&mut bytes, register_bits | offset_bits | width_bits);
        bytes.extend_from_slice(&base_ea.extension);
        EncodeResult::ok(bytes)
    }

    pub(crate) fn encode_pack_unpk_instruction(
        &self,
        mnemonic: &str,
        opcode_base: u16,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
        }
        let [src, dst, adjustment] = operands else {
            return EncodeResult::error(format!(
                "{mnemonic} expects source, destination, and adjustment operands"
            ));
        };
        let Operand::Immediate { expr, .. } = adjustment else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} adjustment must be an immediate value"),
                adjustment.span(),
            );
        };
        let (adjustment_value, _) = match Self::eval_expr_or_placeholder(expr, ctx, 0) {
            Ok(result) => result,
            Err(err) => return EncodeResult::error_with_span(err, adjustment.span()),
        };
        let Some(adjustment_word) = Self::encode_signed_word(adjustment_value) else {
            return EncodeResult::error_with_span(
                format!("{mnemonic} adjustment {adjustment_value} out of 16-bit signed range"),
                adjustment.span(),
            );
        };

        let (mode_bit, src_bits, dst_bits) = match (src, dst) {
            (
                Operand::DataRegister {
                    register: src_reg, ..
                },
                Operand::DataRegister {
                    register: dst_reg, ..
                },
            ) => {
                let Some(src_bits) = Self::data_register_number(src_reg) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} source register"),
                        src.span(),
                    );
                };
                let Some(dst_bits) = Self::data_register_number(dst_reg) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} destination register"),
                        dst.span(),
                    );
                };
                (0_u16, src_bits as u16, dst_bits as u16)
            }
            (
                Operand::AddressPredecrement {
                    register: src_reg, ..
                },
                Operand::AddressPredecrement {
                    register: dst_reg, ..
                },
            ) => {
                let Some(src_bits) = Self::address_register_number(src_reg) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} source register"),
                        src.span(),
                    );
                };
                let Some(dst_bits) = Self::address_register_number(dst_reg) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} destination register"),
                        dst.span(),
                    );
                };
                (1_u16, src_bits as u16, dst_bits as u16)
            }
            _ => {
                return EncodeResult::error(format!(
                    "{mnemonic} expects either Dx,Dy or -(Ax),-(Ay) operands"
                ));
            }
        };

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            opcode_base | (dst_bits << 9) | (mode_bit << 3) | src_bits,
        );
        Self::emit_word(&mut bytes, adjustment_word);
        EncodeResult::ok(bytes)
    }

    pub(crate) fn encode_trapcc_instruction(
        &self,
        condition: ConditionCode,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let opmode = match size {
            None => 0b100,
            Some(OperationSize::Word) => 0b010,
            Some(OperationSize::Long) => 0b011,
            Some(OperationSize::Byte) => {
                return EncodeResult::error("TRAPcc does not support .B size");
            }
        };

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x50F8 | (condition.opcode_bits() << 8) | opmode);

        match size {
            None => {
                if !operands.is_empty() {
                    return EncodeResult::error(
                        "unsized TRAPcc does not take an immediate operand",
                    );
                }
            }
            Some(size @ (OperationSize::Word | OperationSize::Long)) => {
                let [immediate] = operands else {
                    return EncodeResult::error(format!(
                        "TRAPcc{} expects one immediate operand",
                        size.suffix()
                    ));
                };
                let Operand::Immediate { expr, .. } = immediate else {
                    return EncodeResult::error_with_span(
                        format!("TRAPcc{} operand must be immediate", size.suffix()),
                        immediate.span(),
                    );
                };
                let (value, _) = match Self::eval_expr_or_placeholder(expr, ctx, 0) {
                    Ok(result) => result,
                    Err(err) => return EncodeResult::error_with_span(err, immediate.span()),
                };
                let Some(encoded) = Self::encode_immediate(size, value) else {
                    return EncodeResult::error_with_span(
                        format!("TRAPcc{} immediate {value} out of range", size.suffix()),
                        immediate.span(),
                    );
                };
                bytes.extend_from_slice(&encoded);
            }
            Some(OperationSize::Byte) => unreachable!("handled above"),
        }

        EncodeResult::ok(bytes)
    }

    pub(crate) fn encode_callm_instruction(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("CALLM does not accept a size suffix");
        }
        let [immediate, destination] = operands else {
            return EncodeResult::error("CALLM expects an immediate count and control-mode target");
        };
        let Operand::Immediate { expr, .. } = immediate else {
            return EncodeResult::error_with_span(
                "CALLM count operand must be immediate",
                immediate.span(),
            );
        };
        let (count, _) = match Self::eval_expr_or_placeholder(expr, ctx, 0) {
            Ok(result) => result,
            Err(err) => return EncodeResult::error_with_span(err, immediate.span()),
        };
        let Some(count_bits) = Self::encode_unsigned_byte(count) else {
            return EncodeResult::error_with_span(
                format!("CALLM count {count} out of range (0-255)"),
                immediate.span(),
            );
        };

        let destination_ea = match self.encode_effective_address(destination, None, ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::single_ea_control_mode(destination_ea.kind) {
            return EncodeResult::error_with_span(
                "invalid destination effective address for CALLM",
                destination.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0x06C0 | Self::effective_address_bits(destination_ea.bits),
        );
        Self::emit_word(&mut bytes, count_bits as u16);
        bytes.extend_from_slice(&destination_ea.extension);
        EncodeResult::ok(bytes)
    }

    pub(crate) fn encode_rtm_instruction(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("RTM does not accept a size suffix");
        }
        let [register] = operands else {
            return EncodeResult::error("RTM expects one data or address register operand");
        };
        let Some((address_bit, register_bits)) = Self::general_register_descriptor(register) else {
            return EncodeResult::error_with_span(
                "RTM operand must be a data or address register",
                register.span(),
            );
        };

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x06C0 | (address_bit << 3) | register_bits);
        EncodeResult::ok(bytes)
    }

    pub(crate) fn encode_effective_address(
        &self,
        operand: &Operand,
        size: Option<OperationSize>,
        ctx: &dyn AssemblerContext,
    ) -> Result<EncodedEffectiveAddress, EncodeResult<Vec<u8>>> {
        match operand {
            Operand::DataRegister { register, .. } => {
                let Some(reg) = Self::data_register_number(register) else {
                    return Err(EncodeResult::error_with_span(
                        "invalid data register",
                        operand.span(),
                    ));
                };
                Ok(EncodedEffectiveAddress {
                    bits: reg as u16,
                    extension: Vec::new(),
                    kind: EffectiveAddressKind::DataRegister,
                })
            }
            Operand::AddressRegister { register, .. } => {
                let Some(reg) = Self::address_register_number(register) else {
                    return Err(EncodeResult::error_with_span(
                        "invalid address register",
                        operand.span(),
                    ));
                };
                Ok(EncodedEffectiveAddress {
                    bits: (0b001_u16 << 3) | reg as u16,
                    extension: Vec::new(),
                    kind: EffectiveAddressKind::AddressRegister,
                })
            }
            Operand::SpecialRegister { .. } => Err(EncodeResult::error_with_span(
                "68000 special registers are not valid effective addresses",
                operand.span(),
            )),
            Operand::ControlRegister { .. } => Err(EncodeResult::error_with_span(
                "68000 control registers are not valid effective addresses",
                operand.span(),
            )),
            Operand::FullExtension { .. } => {
                self.encode_full_extension_effective_address(operand, ctx)
            }
            Operand::AddressIndirect { register, .. } => {
                let Some(reg) = Self::address_register_number(register) else {
                    return Err(EncodeResult::error_with_span(
                        "invalid address register",
                        operand.span(),
                    ));
                };
                Ok(EncodedEffectiveAddress {
                    bits: (0b010_u16 << 3) | reg as u16,
                    extension: Vec::new(),
                    kind: EffectiveAddressKind::AddressIndirect,
                })
            }
            Operand::AddressPostincrement { register, .. } => {
                let Some(reg) = Self::address_register_number(register) else {
                    return Err(EncodeResult::error_with_span(
                        "invalid address register",
                        operand.span(),
                    ));
                };
                Ok(EncodedEffectiveAddress {
                    bits: (0b011_u16 << 3) | reg as u16,
                    extension: Vec::new(),
                    kind: EffectiveAddressKind::AddressPostincrement,
                })
            }
            Operand::AddressPredecrement { register, .. } => {
                let Some(reg) = Self::address_register_number(register) else {
                    return Err(EncodeResult::error_with_span(
                        "invalid address register",
                        operand.span(),
                    ));
                };
                Ok(EncodedEffectiveAddress {
                    bits: (0b100_u16 << 3) | reg as u16,
                    extension: Vec::new(),
                    kind: EffectiveAddressKind::AddressPredecrement,
                })
            }
            Operand::AddressDisplacement {
                displacement, base, ..
            } => {
                let Some(reg) = Self::address_register_number(base) else {
                    return Err(EncodeResult::error_with_span(
                        "invalid address register",
                        operand.span(),
                    ));
                };
                let value = match Self::eval_expr(displacement, ctx) {
                    Ok(value) => value,
                    Err(err) => return Err(EncodeResult::error_with_span(err, operand.span())),
                };
                let Some(encoded) = Self::encode_signed_word(value) else {
                    return Err(EncodeResult::error_with_span(
                        "68000 displacement out of 16-bit signed range",
                        operand.span(),
                    ));
                };
                let mut extension = Vec::new();
                Self::emit_word(&mut extension, encoded);
                Ok(EncodedEffectiveAddress {
                    bits: (0b101_u16 << 3) | reg as u16,
                    extension,
                    kind: EffectiveAddressKind::AddressDisplacement,
                })
            }
            Operand::AddressIndexed {
                displacement,
                base,
                index,
                index_size,
                ..
            } => {
                let Some(base_reg) = Self::address_register_number(base) else {
                    return Err(EncodeResult::error_with_span(
                        "invalid base address register",
                        operand.span(),
                    ));
                };
                let value = match Self::eval_expr(displacement, ctx) {
                    Ok(value) => value,
                    Err(err) => return Err(EncodeResult::error_with_span(err, operand.span())),
                };
                let Some(encoded_disp) = Self::encode_signed_byte(value) else {
                    return Err(EncodeResult::error_with_span(
                        "68000 indexed displacement out of 8-bit signed range",
                        operand.span(),
                    ));
                };
                let extension = match Self::index_extension_word(index, *index_size, encoded_disp) {
                    Some(word) => word,
                    None => {
                        return Err(EncodeResult::error_with_span(
                            "invalid 68000 index register",
                            operand.span(),
                        ))
                    }
                };
                let mut words = Vec::new();
                Self::emit_word(&mut words, extension);
                Ok(EncodedEffectiveAddress {
                    bits: (0b110_u16 << 3) | base_reg as u16,
                    extension: words,
                    kind: EffectiveAddressKind::AddressIndexed,
                })
            }
            Operand::PcDisplacement { displacement, .. } => {
                let value = match Self::eval_pc_relative_displacement(displacement, ctx) {
                    Ok(value) => value,
                    Err(err) => return Err(EncodeResult::error_with_span(err, operand.span())),
                };
                let Some(encoded) = Self::encode_signed_word(value) else {
                    return Err(EncodeResult::error_with_span(
                        "68000 PC-relative displacement out of 16-bit signed range",
                        operand.span(),
                    ));
                };
                let mut extension = Vec::new();
                Self::emit_word(&mut extension, encoded);
                Ok(EncodedEffectiveAddress {
                    bits: (0b111_u16 << 3) | 0b010,
                    extension,
                    kind: EffectiveAddressKind::PcDisplacement,
                })
            }
            Operand::PcIndexed {
                displacement,
                index,
                index_size,
                ..
            } => {
                let value = match Self::eval_pc_relative_displacement(displacement, ctx) {
                    Ok(value) => value,
                    Err(err) => return Err(EncodeResult::error_with_span(err, operand.span())),
                };
                let Some(encoded_disp) = Self::encode_signed_byte(value) else {
                    return Err(EncodeResult::error_with_span(
                        "68000 PC-relative indexed displacement out of 8-bit signed range",
                        operand.span(),
                    ));
                };
                let extension = match Self::index_extension_word(index, *index_size, encoded_disp) {
                    Some(word) => word,
                    None => {
                        return Err(EncodeResult::error_with_span(
                            "invalid 68000 index register",
                            operand.span(),
                        ))
                    }
                };
                let mut words = Vec::new();
                Self::emit_word(&mut words, extension);
                Ok(EncodedEffectiveAddress {
                    bits: (0b111_u16 << 3) | 0b011,
                    extension: words,
                    kind: EffectiveAddressKind::PcIndexed,
                })
            }
            Operand::Absolute { expr, size, .. } => match size {
                AbsoluteSize::Word => {
                    let value = match Self::eval_expr(expr, ctx) {
                        Ok(value) => value,
                        Err(err) => {
                            return Err(EncodeResult::error_with_span(err, operand.span()));
                        }
                    };
                    let Some(encoded) = Self::encode_absolute_word(value) else {
                        return Err(EncodeResult::error_with_span(
                            "68000 absolute .W address out of 16-bit range",
                            operand.span(),
                        ));
                    };
                    let mut extension = Vec::new();
                    Self::emit_word(&mut extension, encoded);
                    Ok(EncodedEffectiveAddress {
                        bits: 0b111_u16 << 3,
                        extension,
                        kind: EffectiveAddressKind::Absolute,
                    })
                }
                AbsoluteSize::Long => {
                    let value = match Self::eval_expr(expr, ctx) {
                        Ok(value) => value,
                        Err(err) => {
                            return Err(EncodeResult::error_with_span(err, operand.span()));
                        }
                    };
                    let Some(encoded) = Self::encode_absolute_long(value) else {
                        return Err(EncodeResult::error_with_span(
                            "68000 absolute .L address out of 24-bit range",
                            operand.span(),
                        ));
                    };
                    let mut extension = Vec::new();
                    Self::emit_long(&mut extension, encoded);
                    Ok(EncodedEffectiveAddress {
                        bits: (0b111_u16 << 3) | 0b001,
                        extension,
                        kind: EffectiveAddressKind::Absolute,
                    })
                }
            },
            Operand::RegisterPair { .. } | Operand::IndirectRegisterPair { .. } => {
                Err(EncodeResult::error_with_span(
                    "68020 register pairs are not standalone effective addresses",
                    operand.span(),
                ))
            }
            Operand::BitField { .. } => Err(EncodeResult::error_with_span(
                "68020 bit-field operands are not standalone effective addresses",
                operand.span(),
            )),
            Operand::RegisterList { .. } => Err(EncodeResult::error_with_span(
                "68000 register lists are not valid effective addresses",
                operand.span(),
            )),
            Operand::BranchTarget { .. } => Err(EncodeResult::error_with_span(
                "68000 branch targets are not valid effective addresses",
                operand.span(),
            )),
            Operand::Immediate { expr, .. } => {
                let Some(size) = size else {
                    return Err(EncodeResult::error_with_span(
                        "68000 immediate operands require an explicit instruction size",
                        operand.span(),
                    ));
                };
                let value = match Self::eval_expr(expr, ctx) {
                    Ok(value) => value,
                    Err(err) => return Err(EncodeResult::error_with_span(err, operand.span())),
                };
                let extension = match Self::encode_immediate(size, value) {
                    Some(bytes) => bytes,
                    None => {
                        return Err(EncodeResult::error_with_span(
                            format!("immediate value {value} out of range for {}", size.suffix()),
                            operand.span(),
                        ))
                    }
                };
                Ok(EncodedEffectiveAddress {
                    bits: (0b111_u16 << 3) | 0b100,
                    extension,
                    kind: EffectiveAddressKind::Immediate,
                })
            }
        }
    }

    fn move_allows_source(kind: EffectiveAddressKind, size: OperationSize) -> bool {
        match kind {
            EffectiveAddressKind::DataRegister
            | EffectiveAddressKind::AddressIndirect
            | EffectiveAddressKind::AddressPostincrement
            | EffectiveAddressKind::AddressPredecrement
            | EffectiveAddressKind::AddressDisplacement
            | EffectiveAddressKind::AddressIndexed
            | EffectiveAddressKind::PcDisplacement
            | EffectiveAddressKind::PcIndexed
            | EffectiveAddressKind::Absolute
            | EffectiveAddressKind::Immediate => true,
            EffectiveAddressKind::AddressRegister => !matches!(size, OperationSize::Byte),
        }
    }

    fn move_allows_destination(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::DataRegister
                | EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressPostincrement
                | EffectiveAddressKind::AddressPredecrement
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    fn movea_allows_source(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::DataRegister
                | EffectiveAddressKind::AddressRegister
                | EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressPostincrement
                | EffectiveAddressKind::AddressPredecrement
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::PcDisplacement
                | EffectiveAddressKind::PcIndexed
                | EffectiveAddressKind::Absolute
                | EffectiveAddressKind::Immediate
        )
    }

    fn movem_register_to_memory_destination(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressPredecrement
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    fn movem_memory_to_register_source(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressPostincrement
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::PcDisplacement
                | EffectiveAddressKind::PcIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    fn alu_allows_source(kind: EffectiveAddressKind, size: OperationSize) -> bool {
        match kind {
            EffectiveAddressKind::DataRegister
            | EffectiveAddressKind::AddressIndirect
            | EffectiveAddressKind::AddressPostincrement
            | EffectiveAddressKind::AddressPredecrement
            | EffectiveAddressKind::AddressDisplacement
            | EffectiveAddressKind::AddressIndexed
            | EffectiveAddressKind::PcDisplacement
            | EffectiveAddressKind::PcIndexed
            | EffectiveAddressKind::Absolute
            | EffectiveAddressKind::Immediate => true,
            EffectiveAddressKind::AddressRegister => !matches!(size, OperationSize::Byte),
        }
    }

    fn logic_allows_source(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::DataRegister
                | EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressPostincrement
                | EffectiveAddressKind::AddressPredecrement
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::PcDisplacement
                | EffectiveAddressKind::PcIndexed
                | EffectiveAddressKind::Absolute
                | EffectiveAddressKind::Immediate
        )
    }

    pub(crate) fn data_alterable(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::DataRegister
                | EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressPostincrement
                | EffectiveAddressKind::AddressPredecrement
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    fn data_addressing(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::DataRegister
                | EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressPostincrement
                | EffectiveAddressKind::AddressPredecrement
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::PcDisplacement
                | EffectiveAddressKind::PcIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    fn control_alterable(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    pub(crate) fn memory_alterable(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressPostincrement
                | EffectiveAddressKind::AddressPredecrement
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    fn eor_allows_destination(kind: EffectiveAddressKind) -> bool {
        Self::data_alterable(kind)
    }

    fn bit_field_read_mode(kind: EffectiveAddressKind) -> bool {
        matches!(kind, EffectiveAddressKind::DataRegister) || Self::single_ea_control_mode(kind)
    }

    fn bit_field_write_mode(kind: EffectiveAddressKind) -> bool {
        matches!(kind, EffectiveAddressKind::DataRegister) || Self::control_alterable(kind)
    }

    fn quick_allows_destination(kind: EffectiveAddressKind, size: OperationSize) -> bool {
        Self::data_alterable(kind)
            || (matches!(kind, EffectiveAddressKind::AddressRegister)
                && !matches!(size, OperationSize::Byte))
    }

    fn bit_test_allows_destination(kind: EffectiveAddressKind) -> bool {
        Self::data_addressing(kind)
    }

    fn bit_modify_allows_destination(kind: EffectiveAddressKind) -> bool {
        Self::data_alterable(kind)
    }

    pub(crate) fn size_bits(size: OperationSize) -> u16 {
        match size {
            OperationSize::Byte => 0b00,
            OperationSize::Word => 0b01,
            OperationSize::Long => 0b10,
        }
    }

    fn data_register_opmode(size: OperationSize) -> u16 {
        Self::size_bits(size)
    }

    fn address_register_opmode(size: OperationSize) -> u16 {
        match size {
            OperationSize::Word => 0b011,
            OperationSize::Long => 0b111,
            OperationSize::Byte => unreachable!("handled by caller"),
        }
    }

    fn eor_opmode(size: OperationSize) -> u16 {
        match size {
            OperationSize::Byte => 0b100,
            OperationSize::Word => 0b101,
            OperationSize::Long => 0b110,
        }
    }

    fn memory_destination_opmode(size: OperationSize) -> u16 {
        match size {
            OperationSize::Byte => 0b100,
            OperationSize::Word => 0b101,
            OperationSize::Long => 0b110,
        }
    }

    fn quick_data_bits(value: i64) -> Option<u16> {
        match value {
            1..=7 => Some(value as u16),
            8 => Some(0),
            _ => None,
        }
    }

    fn movem_size_bit(size: OperationSize) -> u16 {
        match size {
            OperationSize::Word => 0,
            OperationSize::Long => 0x0040,
            OperationSize::Byte => unreachable!("handled by caller"),
        }
    }

    fn movem_register_mask(registers: &[RegisterListRegister], predecrement: bool) -> u16 {
        registers.iter().fold(0_u16, |mask, register| {
            let bit = match register {
                RegisterListRegister::Data(reg) => *reg as u16,
                RegisterListRegister::Address(reg) => 8 + *reg as u16,
            };
            let bit = if predecrement { 15 - bit } else { bit };
            mask | (1_u16 << bit)
        })
    }

    fn effective_address_kind(operand: &Operand) -> EffectiveAddressKind {
        match operand {
            Operand::DataRegister { .. } => EffectiveAddressKind::DataRegister,
            Operand::AddressRegister { .. } => EffectiveAddressKind::AddressRegister,
            Operand::SpecialRegister { .. } => {
                unreachable!("68000 special registers are not effective addresses")
            }
            Operand::ControlRegister { .. } => {
                unreachable!("68000 control registers are not effective addresses")
            }
            Operand::FullExtension { .. } => match operand {
                Operand::FullExtension { base, .. } => match base {
                    FullExtensionBase::Pc => EffectiveAddressKind::PcIndexed,
                    FullExtensionBase::Address(_) | FullExtensionBase::Suppressed => {
                        EffectiveAddressKind::AddressIndexed
                    }
                },
                _ => unreachable!("matched above"),
            },
            Operand::AddressIndirect { .. } => EffectiveAddressKind::AddressIndirect,
            Operand::AddressPostincrement { .. } => EffectiveAddressKind::AddressPostincrement,
            Operand::AddressPredecrement { .. } => EffectiveAddressKind::AddressPredecrement,
            Operand::AddressDisplacement { .. } => EffectiveAddressKind::AddressDisplacement,
            Operand::AddressIndexed { .. } => EffectiveAddressKind::AddressIndexed,
            Operand::PcDisplacement { .. } => EffectiveAddressKind::PcDisplacement,
            Operand::PcIndexed { .. } => EffectiveAddressKind::PcIndexed,
            Operand::Absolute { .. } => EffectiveAddressKind::Absolute,
            Operand::RegisterPair { .. } | Operand::IndirectRegisterPair { .. } => {
                unreachable!("68020 register pairs are not effective addresses")
            }
            Operand::BitField { .. } => {
                unreachable!("68020 bit-field wrappers are not direct effective addresses")
            }
            Operand::RegisterList { .. } => {
                unreachable!("68000 register lists are not effective addresses")
            }
            Operand::BranchTarget { .. } => {
                unreachable!("68000 branch targets are not effective addresses")
            }
            Operand::Immediate { .. } => EffectiveAddressKind::Immediate,
        }
    }

    fn single_ea_control_mode(kind: EffectiveAddressKind) -> bool {
        matches!(
            kind,
            EffectiveAddressKind::AddressIndirect
                | EffectiveAddressKind::AddressDisplacement
                | EffectiveAddressKind::AddressIndexed
                | EffectiveAddressKind::PcDisplacement
                | EffectiveAddressKind::PcIndexed
                | EffectiveAddressKind::Absolute
        )
    }

    pub(crate) fn data_register_number(name: &str) -> Option<u8> {
        let suffix = name.strip_prefix('D')?;
        let reg = suffix.parse::<u8>().ok()?;
        (reg <= 7).then_some(reg)
    }

    pub(crate) fn address_register_number(name: &str) -> Option<u8> {
        if name.eq_ignore_ascii_case("SP") {
            return Some(7);
        }
        let suffix = name.strip_prefix('A')?;
        let reg = suffix.parse::<u8>().ok()?;
        (reg <= 7).then_some(reg)
    }

    fn effective_address_bits(bits: u16) -> u16 {
        bits & 0x3F
    }

    fn move_destination_bits(bits: u16) -> u16 {
        let mode = (bits >> 3) & 0b111;
        let reg = bits & 0b111;
        (reg << 9) | (mode << 6)
    }

    pub(crate) fn emit_word(bytes: &mut Vec<u8>, value: u16) {
        bytes.push((value >> 8) as u8);
        bytes.push(value as u8);
    }

    fn emit_long(bytes: &mut Vec<u8>, value: u32) {
        bytes.push((value >> 24) as u8);
        bytes.push((value >> 16) as u8);
        bytes.push((value >> 8) as u8);
        bytes.push(value as u8);
    }

    fn encode_signed_byte(value: i64) -> Option<u8> {
        (-128..=127).contains(&value).then_some((value as i8) as u8)
    }

    fn encode_unsigned_byte(value: i64) -> Option<u8> {
        (0..=u8::MAX as i64).contains(&value).then_some(value as u8)
    }

    pub(crate) fn encode_signed_word(value: i64) -> Option<u16> {
        (-32768..=32767)
            .contains(&value)
            .then_some((value as i16) as u16)
    }

    fn encode_absolute_word(value: i64) -> Option<u16> {
        if !(0..=MAX_M68000_ABSOLUTE_ADDRESS).contains(&value) {
            return None;
        }

        let encoded = value as u16;
        let sign_extended = ((encoded as i16) as i32 as u32) & (MAX_M68000_ABSOLUTE_ADDRESS as u32);
        (i64::from(sign_extended) == value).then_some(encoded)
    }

    fn encode_absolute_long(value: i64) -> Option<u32> {
        (0..=MAX_M68000_ABSOLUTE_ADDRESS)
            .contains(&value)
            .then_some(value as u32)
    }

    fn encode_immediate(size: OperationSize, value: i64) -> Option<Vec<u8>> {
        let mut bytes = Vec::new();
        match size {
            OperationSize::Byte => {
                if !(-128..=255).contains(&value) {
                    return None;
                }
                Self::emit_word(&mut bytes, value as u8 as u16);
            }
            OperationSize::Word => {
                if !(-32768..=65535).contains(&value) {
                    return None;
                }
                Self::emit_word(&mut bytes, value as u16);
            }
            OperationSize::Long => {
                if !(-2_147_483_648..=4_294_967_295).contains(&value) {
                    return None;
                }
                Self::emit_long(&mut bytes, value as u32);
            }
        }
        Some(bytes)
    }

    fn full_extension_kind(base: &FullExtensionBase) -> EffectiveAddressKind {
        match base {
            FullExtensionBase::Pc => EffectiveAddressKind::PcIndexed,
            FullExtensionBase::Address(_) | FullExtensionBase::Suppressed => {
                EffectiveAddressKind::AddressIndexed
            }
        }
    }

    fn full_extension_ea_bits(base: &FullExtensionBase) -> Result<u16, EncodeResult<Vec<u8>>> {
        match base {
            FullExtensionBase::Address(register) => {
                let Some(reg) = Self::address_register_number(register) else {
                    return Err(EncodeResult::error_with_span(
                        "invalid 68020 full-extension base register",
                        opcore::tokenizer::Span::default(),
                    ));
                };
                Ok((0b110_u16 << 3) | reg as u16)
            }
            FullExtensionBase::Pc => Ok((0b111_u16 << 3) | 0b011),
            // The base register field is ignored when base suppression is active.
            FullExtensionBase::Suppressed => Ok(0b110_u16 << 3),
        }
    }

    fn full_extension_index_bits(index: &FullExtensionIndex) -> Result<u16, EncodeResult<Vec<u8>>> {
        let (register, address_bit) = if let Some(reg) = Self::data_register_number(&index.register)
        {
            (reg, 0_u16)
        } else if let Some(reg) = Self::address_register_number(&index.register) {
            (reg, 1_u16)
        } else {
            return Err(EncodeResult::error(
                "invalid 68020 full-extension index register",
            ));
        };
        let size_bit = match index.size {
            IndexSize::Word => 0_u16,
            IndexSize::Long => 1_u16,
        };
        let scale_bits = match index.scale {
            IndexScale::One => 0_u16,
            IndexScale::Two => 0b01,
            IndexScale::Four => 0b10,
            IndexScale::Eight => 0b11,
        };
        Ok((address_bit << 15) | ((register as u16) << 12) | (size_bit << 11) | (scale_bits << 9))
    }

    fn encode_full_extension_displacement(
        displacement: &(Expr, AbsoluteSize),
        pc_relative: bool,
        label: &str,
        span: opcore::tokenizer::Span,
        ctx: &dyn AssemblerContext,
    ) -> Result<(u16, Vec<u8>), EncodeResult<Vec<u8>>> {
        let (expr, size) = displacement;
        let value = if pc_relative {
            match Self::eval_pc_relative_displacement(expr, ctx) {
                Ok(value) => value,
                Err(err) => return Err(EncodeResult::error_with_span(err, span)),
            }
        } else {
            match Self::eval_expr(expr, ctx) {
                Ok(value) => value,
                Err(err) => return Err(EncodeResult::error_with_span(err, span)),
            }
        };

        match size {
            AbsoluteSize::Word => {
                let Some(encoded) = Self::encode_signed_word(value) else {
                    return Err(EncodeResult::error_with_span(
                        format!("68020 full-extension {label} out of 16-bit signed range"),
                        span,
                    ));
                };
                let mut bytes = Vec::new();
                Self::emit_word(&mut bytes, encoded);
                Ok((0b10_u16 << 4, bytes))
            }
            AbsoluteSize::Long => {
                if !((i32::MIN as i64)..=(i32::MAX as i64)).contains(&value) {
                    return Err(EncodeResult::error_with_span(
                        format!("68020 full-extension {label} out of 32-bit signed range"),
                        span,
                    ));
                }
                let mut bytes = Vec::new();
                Self::emit_long(&mut bytes, value as i32 as u32);
                Ok((0b11_u16 << 4, bytes))
            }
        }
    }

    fn encode_full_extension_effective_address(
        &self,
        operand: &Operand,
        ctx: &dyn AssemblerContext,
    ) -> Result<EncodedEffectiveAddress, EncodeResult<Vec<u8>>> {
        let Operand::FullExtension {
            base_displacement,
            base,
            index,
            memory_indirection,
            outer_displacement,
            ..
        } = operand
        else {
            unreachable!("full-extension encoder called with non full-extension operand")
        };
        let span = operand.span();

        if memory_indirection.is_none() && outer_displacement.is_some() {
            return Err(EncodeResult::error_with_span(
                "68020 full-extension outer displacement requires memory-indirect form",
                span,
            ));
        }

        let bits = Self::full_extension_ea_bits(base).map_err(|_| {
            EncodeResult::error_with_span("invalid 68020 full-extension base register", span)
        })?;

        let base_suppress_bit = matches!(base, FullExtensionBase::Suppressed) as u16;
        let index_suppress_bit = index.is_none() as u16;

        let index_bits = match index {
            Some(index) => Self::full_extension_index_bits(index).map_err(|_| {
                EncodeResult::error_with_span("invalid 68020 full-extension index register", span)
            })?,
            None => 0,
        };

        let (base_displacement_bits, mut base_displacement_bytes) = match base_displacement {
            Some(displacement) => Self::encode_full_extension_displacement(
                displacement,
                matches!(base, FullExtensionBase::Pc),
                "base displacement",
                span,
                ctx,
            )?,
            None => (0b01_u16 << 4, Vec::new()),
        };

        let (outer_displacement_selector, mut outer_displacement_bytes) =
            match (memory_indirection, outer_displacement) {
                (None, None) => (0_u16, Vec::new()),
                (Some(MemoryIndirectionKind::Preindexed), None) => (0b001, Vec::new()),
                (Some(MemoryIndirectionKind::Preindexed), Some(displacement)) => {
                    let selector = match displacement.1 {
                        AbsoluteSize::Word => 0b010,
                        AbsoluteSize::Long => 0b011,
                    };
                    let (_, bytes) = Self::encode_full_extension_displacement(
                        displacement,
                        false,
                        "outer displacement",
                        span,
                        ctx,
                    )?;
                    (selector, bytes)
                }
                (Some(MemoryIndirectionKind::Postindexed), None) => (0b101, Vec::new()),
                (Some(MemoryIndirectionKind::Postindexed), Some(displacement)) => {
                    let selector = match displacement.1 {
                        AbsoluteSize::Word => 0b110,
                        AbsoluteSize::Long => 0b111,
                    };
                    let (_, bytes) = Self::encode_full_extension_displacement(
                        displacement,
                        false,
                        "outer displacement",
                        span,
                        ctx,
                    )?;
                    (selector, bytes)
                }
                (None, Some(_)) => unreachable!("handled above"),
            };

        let extension_word = index_bits
            | 0x0100
            | (base_suppress_bit << 7)
            | (index_suppress_bit << 6)
            | base_displacement_bits
            | outer_displacement_selector;

        let mut extension = Vec::new();
        Self::emit_word(&mut extension, extension_word);
        extension.append(&mut base_displacement_bytes);
        extension.append(&mut outer_displacement_bytes);

        Ok(EncodedEffectiveAddress {
            bits,
            extension,
            kind: Self::full_extension_kind(base),
        })
    }

    pub(crate) fn general_register_descriptor(operand: &Operand) -> Option<(u16, u16)> {
        match operand {
            Operand::DataRegister { register, .. } => {
                Some(Self::general_register_name_descriptor(register)?)
            }
            Operand::AddressRegister { register, .. } => {
                Some(Self::general_register_name_descriptor(register)?)
            }
            _ => None,
        }
    }

    fn general_register_name_descriptor(name: &str) -> Option<(u16, u16)> {
        if let Some(reg) = Self::data_register_number(name) {
            return Some((0, reg as u16));
        }
        if let Some(reg) = Self::address_register_number(name) {
            return Some((1, reg as u16));
        }
        None
    }

    pub(crate) fn encode_moves_instruction(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let Some(size) = size else {
            return EncodeResult::error("MOVES requires an explicit size suffix (.B, .W, or .L)");
        };

        let [src, dst] = operands else {
            return EncodeResult::error("MOVES expects two operands");
        };

        let (dr_bit, register_operand, ea_operand) = if Self::general_register_descriptor(src)
            .is_some()
        {
            (1_u16, src, dst)
        } else if Self::general_register_descriptor(dst).is_some() {
            (0_u16, dst, src)
        } else {
            return EncodeResult::error(
                "MOVES expects one data/address register and one memory-alterable effective address",
            );
        };

        let Some((ad_bit, register_bits)) = Self::general_register_descriptor(register_operand)
        else {
            return EncodeResult::error_with_span(
                "MOVES register operand must be a data or address register",
                register_operand.span(),
            );
        };

        let ea = match self.encode_effective_address(ea_operand, Some(size), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::memory_alterable(ea.kind) {
            return EncodeResult::error_with_span(
                if dr_bit == 0 {
                    format!(
                        "invalid source effective address for MOVES{}",
                        size.suffix()
                    )
                } else {
                    format!(
                        "invalid destination effective address for MOVES{}",
                        size.suffix()
                    )
                },
                ea_operand.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x0E00 | (Self::size_bits(size) << 6) | ea.bits);
        Self::emit_word(
            &mut bytes,
            (ad_bit << 15) | (register_bits << 12) | (dr_bit << 11),
        );
        bytes.extend_from_slice(&ea.extension);
        EncodeResult::ok(bytes)
    }

    fn index_extension_word(index: &str, index_size: IndexSize, displacement: u8) -> Option<u16> {
        let (register, address_bit) = if let Some(reg) = Self::data_register_number(index) {
            (reg, 0_u16)
        } else {
            (Self::address_register_number(index)?, 1_u16)
        };
        let size_bit = match index_size {
            IndexSize::Word => 0_u16,
            IndexSize::Long => 1_u16,
        };
        Some(
            (address_bit << 15)
                | ((register as u16) << 12)
                | (size_bit << 11)
                | displacement as u16,
        )
    }

    fn normalize_wrapped_i32(value: i64) -> i64 {
        if ((i32::MAX as i64) + 1..=u32::MAX as i64).contains(&value) {
            value as u32 as i32 as i64
        } else {
            value
        }
    }

    fn expr_is_unresolved(expr: &Expr, ctx: &dyn AssemblerContext) -> bool {
        ctx.pass() == 1 && expr_has_unstable_symbols(expr, ctx)
    }

    fn expr_has_pc_relative_target_reference(expr: &Expr, ctx: &dyn AssemblerContext) -> bool {
        match expr {
            Expr::Dollar(_) => true,
            Expr::Identifier(name, _) | Expr::Register(name, _) => {
                if ctx.scalar_value_symbol(name).is_some() {
                    return false;
                }
                let Some(entry) = ctx.symbols().entry(name) else {
                    return true;
                };
                !entry.rw
            }
            Expr::Indirect(inner, _) | Expr::Immediate(inner, _) | Expr::IndirectLong(inner, _) => {
                Self::expr_has_pc_relative_target_reference(inner, ctx)
            }
            Expr::List(items, _) | Expr::Tuple(items, _) => items
                .iter()
                .any(|item| Self::expr_has_pc_relative_target_reference(item, ctx)),
            Expr::Index { base, index, .. } => {
                Self::expr_has_pc_relative_target_reference(base, ctx)
                    || Self::expr_has_pc_relative_target_reference(index, ctx)
            }
            Expr::Member { base, .. } => Self::expr_has_pc_relative_target_reference(base, ctx),
            Expr::StructLiteral { fields, .. } => fields
                .iter()
                .any(|(_, value)| Self::expr_has_pc_relative_target_reference(value, ctx)),
            Expr::Call { args, .. } => args
                .iter()
                .any(|arg| Self::expr_has_pc_relative_target_reference(arg, ctx)),
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
                ..
            } => {
                Self::expr_has_pc_relative_target_reference(cond, ctx)
                    || Self::expr_has_pc_relative_target_reference(then_expr, ctx)
                    || Self::expr_has_pc_relative_target_reference(else_expr, ctx)
            }
            Expr::Unary { expr, .. } => Self::expr_has_pc_relative_target_reference(expr, ctx),
            Expr::Binary { left, right, .. } => {
                Self::expr_has_pc_relative_target_reference(left, ctx)
                    || Self::expr_has_pc_relative_target_reference(right, ctx)
            }
            Expr::Range {
                start, end, step, ..
            } => {
                Self::expr_has_pc_relative_target_reference(start, ctx)
                    || Self::expr_has_pc_relative_target_reference(end, ctx)
                    || step
                        .as_ref()
                        .is_some_and(|expr| Self::expr_has_pc_relative_target_reference(expr, ctx))
            }
            Expr::Number(_, _) | Expr::String(_, _) | Expr::Placeholder(_) | Expr::Error(_, _) => {
                false
            }
        }
    }

    fn eval_expr_or_placeholder(
        expr: &Expr,
        ctx: &dyn AssemblerContext,
        placeholder: i64,
    ) -> Result<(i64, bool), String> {
        if Self::expr_is_unresolved(expr, ctx) {
            return Ok((placeholder, true));
        }
        ctx.eval_expr(expr)
            .map(|value| (Self::normalize_wrapped_i32(value), false))
    }

    pub(crate) fn eval_expr(expr: &Expr, ctx: &dyn AssemblerContext) -> Result<i64, String> {
        if Self::expr_is_unresolved(expr, ctx) {
            return Ok(0);
        }
        ctx.eval_expr(expr).map(Self::normalize_wrapped_i32)
    }

    fn eval_pc_relative_displacement(
        expr: &Expr,
        ctx: &dyn AssemblerContext,
    ) -> Result<i64, String> {
        if !Self::expr_has_pc_relative_target_reference(expr, ctx) {
            return Self::eval_expr(expr, ctx);
        }

        if Self::expr_is_unresolved(expr, ctx) {
            return Ok(0);
        }

        let target = ctx.eval_expr(expr).map(Self::normalize_wrapped_i32)?;
        Ok(target - (ctx.current_address() as i64 + 2))
    }
}

impl FamilyHandler for M68KFamilyHandler {
    type FamilyOperand = FamilyOperand;
    type Operand = Operand;

    fn parse_operands(
        &self,
        mnemonic: &str,
        exprs: &[Expr],
    ) -> Result<Vec<Self::FamilyOperand>, FamilyParseError> {
        if matches!(
            parse_mnemonic(mnemonic).map(|parsed| parsed.kind),
            Some(MnemonicKind::Movem)
        ) {
            return self.parse_movem_operands(exprs);
        }

        if Self::is_branch_mnemonic(mnemonic) {
            let [expr] = exprs else {
                return Err(FamilyParseError::new(
                    "68000 branches expect one target operand",
                    exprs.first().map(span_from_expr).unwrap_or_default(),
                ));
            };
            return Ok(vec![FamilyOperand::BranchTarget {
                expr: expr.clone(),
                span: span_from_expr(expr),
            }]);
        }

        if Self::is_dbcc_mnemonic(mnemonic) {
            let [counter, expr] = exprs else {
                return Err(FamilyParseError::new(
                    "68000 DBcc instructions expect a data register and target operand",
                    exprs.first().map(span_from_expr).unwrap_or_default(),
                ));
            };
            let counter = self.parse_single_operand(counter)?;
            return Ok(vec![
                counter,
                FamilyOperand::BranchTarget {
                    expr: expr.clone(),
                    span: span_from_expr(expr),
                },
            ]);
        }

        exprs
            .iter()
            .map(|expr| self.parse_single_operand(expr))
            .collect()
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        operands: &[Self::Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.encode_instruction_impl(mnemonic, operands, ctx)
    }

    fn is_register(&self, name: &str) -> bool {
        is_register(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use opcore::expression::expr_text;
    use opcore::parser::LineAst;
    use opcore::tokenizer::Span;
    use registry::syntax::{parser_from_line_with_registers, register_checker_from_fn};
    use std::collections::HashMap;
    use types::symbol::{SymbolTable, SymbolTableResult, SymbolVisibility};

    fn span() -> Span {
        Span {
            line: 1,
            col_start: 1,
            col_end: 1,
        }
    }

    fn parse_test_number(text: &str) -> Result<i64, String> {
        if let Some(hex) = text.strip_prefix('$') {
            i64::from_str_radix(hex, 16).map_err(|err| err.to_string())
        } else {
            text.parse::<i64>().map_err(|err| err.to_string())
        }
    }

    fn parse_operand_from_source(source: &str) -> FamilyOperand {
        let mut operands = parse_operands_from_source(source);
        assert_eq!(operands.len(), 2, "expected MOVE source and destination");
        operands.remove(0)
    }

    fn parse_operands_from_source(source: &str) -> Vec<FamilyOperand> {
        let mut parser = parser_from_line_with_registers(
            source,
            1,
            register_checker_from_fn(crate::m68k::is_register),
        )
        .expect("parser");
        let line = parser.parse_compat_mixed_line().expect("line parse");
        let LineAst::Statement(statement) = line else {
            panic!("expected statement, got {line:?}");
        };
        let mnemonic = statement.mnemonic.as_deref().expect("mnemonic");
        M68KFamilyHandler::new()
            .parse_operands(mnemonic, &statement.operands)
            .expect("operand parse")
    }

    fn parse_operand_error_from_source(source: &str) -> FamilyParseError {
        let mut parser = parser_from_line_with_registers(
            source,
            1,
            register_checker_from_fn(crate::m68k::is_register),
        )
        .expect("parser");
        let line = parser.parse_compat_mixed_line().expect("line parse");
        let LineAst::Statement(statement) = line else {
            panic!("expected statement, got {line:?}");
        };
        let mnemonic = statement.mnemonic.as_deref().expect("mnemonic");
        M68KFamilyHandler::new()
            .parse_operands(mnemonic, &statement.operands)
            .expect_err("expected operand parse failure")
    }

    fn assert_full_extension_operand(
        operand: &FamilyOperand,
        expected_base_displacement: Option<(&str, AbsoluteSize)>,
        expected_base: FullExtensionBase,
        expected_index: Option<(&str, IndexSize, IndexScale)>,
        expected_memory_indirection: Option<MemoryIndirectionKind>,
        expected_outer_displacement: Option<(&str, AbsoluteSize)>,
    ) {
        let FamilyOperand::FullExtension {
            base_displacement,
            base,
            index,
            memory_indirection,
            outer_displacement,
            ..
        } = operand
        else {
            panic!("expected full-extension operand, got {operand:?}");
        };

        match (base_displacement.as_ref(), expected_base_displacement) {
            (Some((expr, size)), Some((text, expected_size))) => {
                assert_eq!(expr_text(expr).as_deref(), Some(text));
                assert_eq!(*size, expected_size);
            }
            (None, None) => {}
            other => panic!("unexpected base displacement: {other:?}"),
        }
        assert_eq!(base, &expected_base);
        match (index.as_ref(), expected_index) {
            (Some(actual), Some((register, size, scale))) => {
                assert_eq!(actual.register, register);
                assert_eq!(actual.size, size);
                assert_eq!(actual.scale, scale);
            }
            (None, None) => {}
            other => panic!("unexpected index: {other:?}"),
        }
        assert_eq!(*memory_indirection, expected_memory_indirection);
        match (outer_displacement.as_ref(), expected_outer_displacement) {
            (Some((expr, size)), Some((text, expected_size))) => {
                assert_eq!(expr_text(expr).as_deref(), Some(text));
                assert_eq!(*size, expected_size);
            }
            (None, None) => {}
            other => panic!("unexpected outer displacement: {other:?}"),
        }
    }

    #[derive(Default)]
    struct TestContext {
        values: HashMap<String, i64>,
        scalar_symbols: HashMap<String, i64>,
        symbols: SymbolTable,
        current_address: u32,
        pass: u8,
    }

    impl TestContext {
        fn with_symbol(mut self, name: &str, value: i64) -> Self {
            self.values.insert(name.to_string(), value);
            self
        }

        fn with_symbol_entry(mut self, name: &str, value: i64, rw: bool, updated: bool) -> Self {
            self.values.insert(name.to_string(), value);
            assert_eq!(
                self.symbols
                    .add(name, value as u32, rw, SymbolVisibility::Private, None),
                SymbolTableResult::Ok
            );
            if updated {
                let entry = self.symbols.entry_mut(name).expect("symbol entry");
                entry.updated = true;
            }
            self
        }

        fn with_scalar_value_symbol(mut self, name: &str, value: i64) -> Self {
            self.values.insert(name.to_string(), value);
            self.scalar_symbols.insert(name.to_string(), value);
            self
        }
    }

    impl AssemblerContext for TestContext {
        fn eval_expr(&self, expr: &Expr) -> Result<i64, String> {
            match expr {
                Expr::Number(text, _) => parse_test_number(text),
                Expr::Identifier(name, _) => self
                    .values
                    .get(name)
                    .copied()
                    .ok_or_else(|| format!("unknown symbol '{name}'")),
                _ => Err(format!("unsupported test expression: {expr:?}")),
            }
        }

        fn symbols(&self) -> &SymbolTable {
            &self.symbols
        }

        fn has_symbol(&self, name: &str) -> bool {
            self.values.contains_key(name)
        }

        fn symbol_is_finalized(&self, name: &str) -> Option<bool> {
            self.values.contains_key(name).then_some(true)
        }

        fn current_address(&self) -> u32 {
            self.current_address
        }

        fn pass(&self) -> u8 {
            self.pass
        }

        fn scalar_value_symbol(&self, name: &str) -> Option<i64> {
            self.scalar_symbols.get(name).copied()
        }
    }

    fn expect_encoded(result: EncodeResult<Vec<u8>>, expected: &[u8]) {
        match result {
            EncodeResult::Ok(bytes) => assert_eq!(bytes, expected),
            other => panic!("expected encoded bytes, got {other:?}"),
        }
    }

    #[test]
    fn parses_baseline_register_operands() {
        let handler = M68KFamilyHandler::new();
        let operands = handler
            .parse_operands(
                "MOVE",
                &[
                    Expr::Register("D0".to_string(), span()),
                    Expr::Register("A1".to_string(), span()),
                ],
            )
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::DataRegister { register, .. } if register == "D0"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::AddressRegister { register, .. } if register == "A1"
        ));
    }

    #[test]
    fn pc_relative_scalar_symbols_encode_literal_displacements() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext {
            current_address: 0x1000,
            pass: 2,
            ..Default::default()
        }
        .with_scalar_value_symbol("CONST_DISP", 4)
        .with_scalar_value_symbol("SET_DISP", 2)
        .with_symbol_entry("TARGET", 0x1008, false, true);

        expect_encoded(
            handler.encode_instruction(
                "MOVE.W",
                &[
                    Operand::PcDisplacement {
                        displacement: Expr::Identifier("CONST_DISP".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x30, 0x3A, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE.W",
                &[
                    Operand::PcIndexed {
                        displacement: Expr::Identifier("SET_DISP".to_string(), span()),
                        index: "D1".to_string(),
                        index_size: IndexSize::Word,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x34, 0x3B, 0x10, 0x02],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE.W",
                &[
                    Operand::PcDisplacement {
                        displacement: Expr::Identifier("TARGET".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x30, 0x3A, 0x00, 0x06],
        );
    }

    #[test]
    fn parses_indirect_predec_and_postinc_operands() {
        let handler = M68KFamilyHandler::new();
        let indirect = Expr::Indirect(Box::new(Expr::Register("A0".to_string(), span())), span());
        let operands = handler
            .parse_operands(
                "MOVE",
                &[
                    Expr::Unary {
                        op: UnaryOp::Minus,
                        expr: Box::new(indirect.clone()),
                        span: span(),
                    },
                    Expr::Unary {
                        op: UnaryOp::Plus,
                        expr: Box::new(indirect),
                        span: Span {
                            line: 1,
                            col_start: 1,
                            col_end: 4,
                        },
                    },
                ],
            )
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::AddressPredecrement { register, .. } if register == "A0"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::AddressPostincrement { register, .. } if register == "A0"
        ));
    }

    #[test]
    fn parses_displacement_and_indexed_operands() {
        let handler = M68KFamilyHandler::new();
        let displacement = Expr::Number("4".to_string(), span());
        let indexed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    displacement.clone(),
                    Expr::Register("A0".to_string(), span()),
                    Expr::Identifier("D1.W".to_string(), span()),
                ],
                span(),
            )),
            span(),
        );
        let pc_relative = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    displacement.clone(),
                    Expr::Register("PC".to_string(), span()),
                ],
                span(),
            )),
            span(),
        );

        let operands = handler
            .parse_operands("MOVE", &[indexed, pc_relative])
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::AddressIndexed {
                base,
                index,
                index_size,
                ..
            } if base == "A0" && index == "D1" && *index_size == IndexSize::Word
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::PcDisplacement { displacement: Expr::Number(text, _), .. } if text == "4"
        ));
    }

    #[test]
    fn parses_zero_displacement_indexed_aliases_with_word_defaults() {
        let handler = M68KFamilyHandler::new();
        let address_indexed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Register("A0".to_string(), span()),
                    Expr::Register("D1".to_string(), span()),
                ],
                span(),
            )),
            span(),
        );
        let pc_indexed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Identifier("PC".to_string(), span()),
                    Expr::Identifier("D2".to_string(), span()),
                ],
                span(),
            )),
            span(),
        );

        let operands = handler
            .parse_operands("MOVE", &[address_indexed, pc_indexed])
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::AddressIndexed {
                displacement: Expr::Number(text, _),
                base,
                index,
                index_size,
                ..
            } if text == "0" && base == "A0" && index == "D1" && *index_size == IndexSize::Word
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::PcIndexed {
                displacement: Expr::Number(text, _),
                index,
                index_size,
                ..
            } if text == "0" && index == "D2" && *index_size == IndexSize::Word
        ));
    }

    #[test]
    fn parses_identity_scale_and_pc_shorthand_aliases() {
        let handler = M68KFamilyHandler::new();
        let address_indexed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Number("4".to_string(), span()),
                    Expr::Register("A0".to_string(), span()),
                    Expr::Binary {
                        op: BinaryOp::Multiply,
                        left: Box::new(Expr::Identifier("D1.L".to_string(), span())),
                        right: Box::new(Expr::Number("1".to_string(), span())),
                        span: span(),
                    },
                ],
                span(),
            )),
            span(),
        );
        let pc_indexed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Identifier("PC".to_string(), span()),
                    Expr::Binary {
                        op: BinaryOp::Multiply,
                        left: Box::new(Expr::Identifier("D2".to_string(), span())),
                        right: Box::new(Expr::Number("1".to_string(), span())),
                        span: span(),
                    },
                ],
                span(),
            )),
            span(),
        );
        let pc_shorthand =
            Expr::Indirect(Box::new(Expr::Identifier("PC".to_string(), span())), span());

        let operands = handler
            .parse_operands("MOVE", &[address_indexed, pc_indexed, pc_shorthand])
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::AddressIndexed {
                displacement: Expr::Number(text, _),
                base,
                index,
                index_size,
                ..
            } if text == "4" && base == "A0" && index == "D1" && *index_size == IndexSize::Long
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::PcIndexed {
                displacement: Expr::Number(text, _),
                index,
                index_size,
                ..
            } if text == "0" && index == "D2" && *index_size == IndexSize::Word
        ));
        assert!(matches!(
            &operands[2],
            FamilyOperand::PcDisplacement {
                displacement: Expr::Number(text, _),
                ..
            } if text == "0"
        ));
    }

    #[test]
    fn parses_canonical_68020_full_extension_operands() {
        let handler = M68KFamilyHandler::new();
        let address_full_extension = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Member {
                        base: Box::new(Expr::Number("4".to_string(), span())),
                        field: "W".to_string(),
                        span: span(),
                    },
                    Expr::Register("A0".to_string(), span()),
                    Expr::Binary {
                        op: BinaryOp::Multiply,
                        left: Box::new(Expr::Identifier("D1.L".to_string(), span())),
                        right: Box::new(Expr::Number("4".to_string(), span())),
                        span: span(),
                    },
                ],
                span(),
            )),
            span(),
        );
        let pc_full_extension = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Member {
                        base: Box::new(Expr::Identifier("disp".to_string(), span())),
                        field: "L".to_string(),
                        span: span(),
                    },
                    Expr::Identifier("PC".to_string(), span()),
                    Expr::Identifier("D2".to_string(), span()),
                ],
                span(),
            )),
            span(),
        );
        let base_suppressed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Member {
                        base: Box::new(Expr::Number("8".to_string(), span())),
                        field: "W".to_string(),
                        span: span(),
                    },
                    Expr::Placeholder(span()),
                    Expr::Identifier("D3.W".to_string(), span()),
                ],
                span(),
            )),
            span(),
        );
        let index_suppressed = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Placeholder(span()),
                    Expr::Register("A4".to_string(), span()),
                    Expr::Placeholder(span()),
                ],
                span(),
            )),
            span(),
        );

        let operands = handler
            .parse_operands(
                "MOVE",
                &[
                    address_full_extension,
                    pc_full_extension,
                    base_suppressed,
                    index_suppressed,
                ],
            )
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::FullExtension {
                base_displacement: Some((Expr::Number(text, _), AbsoluteSize::Word)),
                base: FullExtensionBase::Address(base),
                index: Some(FullExtensionIndex {
                    register,
                    size: IndexSize::Long,
                    scale: IndexScale::Four,
                }),
                ..
            } if text == "4" && base == "A0" && register == "D1"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::FullExtension {
                base_displacement: Some((Expr::Identifier(name, _), AbsoluteSize::Long)),
                base: FullExtensionBase::Pc,
                index: Some(FullExtensionIndex {
                    register,
                    size: IndexSize::Word,
                    scale: IndexScale::One,
                }),
                ..
            } if name == "disp" && register == "D2"
        ));
        assert!(matches!(
            &operands[2],
            FamilyOperand::FullExtension {
                base_displacement: Some((Expr::Number(text, _), AbsoluteSize::Word)),
                base: FullExtensionBase::Suppressed,
                index: Some(FullExtensionIndex {
                    register,
                    size: IndexSize::Word,
                    scale: IndexScale::One,
                }),
                ..
            } if text == "8" && register == "D3"
        ));
        assert!(matches!(
            &operands[3],
            FamilyOperand::FullExtension {
                base_displacement: None,
                base: FullExtensionBase::Address(base),
                index: None,
                ..
            } if base == "A4"
        ));
    }

    #[test]
    fn rejects_malformed_68020_full_extension_operands_deterministically() {
        let handler = M68KFamilyHandler::new();
        let err = handler
            .parse_operands(
                "MOVE",
                &[Expr::Indirect(
                    Box::new(Expr::Tuple(
                        vec![
                            Expr::Number("4".to_string(), span()),
                            Expr::Placeholder(span()),
                            Expr::Identifier("D1.W".to_string(), span()),
                        ],
                        span(),
                    )),
                    span(),
                )],
            )
            .expect_err("missing explicit full-extension displacement width should fail");
        assert!(err
            .message
            .contains("68020 full-extension base displacement requires explicit .W or .L"));

        let err = handler
            .parse_operands(
                "MOVE",
                &[Expr::Indirect(
                    Box::new(Expr::Tuple(
                        vec![
                            Expr::Member {
                                base: Box::new(Expr::Number("4".to_string(), span())),
                                field: "W".to_string(),
                                span: span(),
                            },
                            Expr::Placeholder(span()),
                            Expr::Placeholder(span()),
                        ],
                        span(),
                    )),
                    span(),
                )],
            )
            .expect_err("suppressing both base and index should fail");
        assert!(err.message.contains("cannot suppress both base and index"));
    }

    #[test]
    fn rejects_non_identity_scaled_index_aliases() {
        let handler = M68KFamilyHandler::new();
        let err = handler
            .parse_operands(
                "MOVE",
                &[Expr::Indirect(
                    Box::new(Expr::Tuple(
                        vec![
                            Expr::Number("4".to_string(), span()),
                            Expr::Register("A0".to_string(), span()),
                            Expr::Binary {
                                op: BinaryOp::Multiply,
                                left: Box::new(Expr::Identifier("D1.W".to_string(), span())),
                                right: Box::new(Expr::Number("2".to_string(), span())),
                                span: span(),
                            },
                        ],
                        span(),
                    )),
                    span(),
                )],
            )
            .expect_err("non-identity scales should stay rejected");

        assert!(err
            .message
            .contains("68020 full-extension base displacement requires explicit .W or .L"));
    }

    #[test]
    fn normalizes_68020_memory_indirect_aliases_to_canonical_operands() {
        let canonical_preindexed = parse_operand_from_source("    MOVE ([,A0,D1.L*4],8.W),D0");
        let alias_preindexed = parse_operand_from_source("    MOVE ([A0,D1.L*4],8.W),D0");
        let canonical_postindexed = parse_operand_from_source("    MOVE ([,A3],D2.W*2,outer.L),D0");
        let alias_postindexed = parse_operand_from_source("    MOVE ([A3],D2.W*2,outer.L),D0");

        assert_full_extension_operand(
            &canonical_preindexed,
            None,
            FullExtensionBase::Address("A0".to_string()),
            Some(("D1", IndexSize::Long, IndexScale::Four)),
            Some(MemoryIndirectionKind::Preindexed),
            Some(("8", AbsoluteSize::Word)),
        );
        assert_full_extension_operand(
            &alias_preindexed,
            None,
            FullExtensionBase::Address("A0".to_string()),
            Some(("D1", IndexSize::Long, IndexScale::Four)),
            Some(MemoryIndirectionKind::Preindexed),
            Some(("8", AbsoluteSize::Word)),
        );
        assert_full_extension_operand(
            &canonical_postindexed,
            None,
            FullExtensionBase::Address("A3".to_string()),
            Some(("D2", IndexSize::Word, IndexScale::Two)),
            Some(MemoryIndirectionKind::Postindexed),
            Some(("outer", AbsoluteSize::Long)),
        );
        assert_full_extension_operand(
            &alias_postindexed,
            None,
            FullExtensionBase::Address("A3".to_string()),
            Some(("D2", IndexSize::Word, IndexScale::Two)),
            Some(MemoryIndirectionKind::Postindexed),
            Some(("outer", AbsoluteSize::Long)),
        );
    }

    #[test]
    fn normalizes_68020_width_explicit_sugar_to_canonical_full_extension_operands() {
        let canonical = parse_operand_from_source("    MOVE (disp.W,A0,D1.L*4),D0");
        let alias = parse_operand_from_source("    MOVE disp.W(A0,D1.L*4),D0");

        assert_full_extension_operand(
            &canonical,
            Some(("disp", AbsoluteSize::Word)),
            FullExtensionBase::Address("A0".to_string()),
            Some(("D1", IndexSize::Long, IndexScale::Four)),
            None,
            None,
        );
        assert_full_extension_operand(
            &alias,
            Some(("disp", AbsoluteSize::Word)),
            FullExtensionBase::Address("A0".to_string()),
            Some(("D1", IndexSize::Long, IndexScale::Four)),
            None,
            None,
        );
    }

    #[test]
    fn rejects_widthless_68020_displacement_leading_sugar_deterministically() {
        let err = parse_operand_error_from_source("    MOVE disp(A0,D1.L*4),D0");
        assert!(err
            .message
            .contains("68020 full-extension base displacement requires explicit .W or .L"));
    }

    #[test]
    fn parses_absolute_and_immediate_operands() {
        let handler = M68KFamilyHandler::new();
        let absolute = Expr::Member {
            base: Box::new(Expr::Indirect(
                Box::new(Expr::Identifier("label".to_string(), span())),
                span(),
            )),
            field: "L".to_string(),
            span: span(),
        };
        let immediate = Expr::Immediate(Box::new(Expr::Number("1".to_string(), span())), span());

        let operands = handler
            .parse_operands("MOVE", &[absolute, immediate])
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::Absolute {
                size: AbsoluteSize::Long,
                expr: Expr::Identifier(name, _),
                ..
            } if name == "label"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::Immediate {
                expr: Expr::Number(text, _),
                ..
            } if text == "1"
        ));
    }

    #[test]
    fn parses_non_parenthesized_absolute_aliases() {
        let handler = M68KFamilyHandler::new();
        let operands = handler
            .parse_operands(
                "MOVE",
                &[
                    Expr::Member {
                        base: Box::new(Expr::Identifier("label".to_string(), span())),
                        field: "W".to_string(),
                        span: span(),
                    },
                    Expr::Member {
                        base: Box::new(Expr::Number("$123456".to_string(), span())),
                        field: "L".to_string(),
                        span: span(),
                    },
                ],
            )
            .expect("operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::Absolute {
                size: AbsoluteSize::Word,
                expr: Expr::Identifier(name, _),
                ..
            } if name == "label"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::Absolute {
                size: AbsoluteSize::Long,
                expr: Expr::Number(text, _),
                ..
            } if text == "$123456"
        ));
    }

    #[test]
    fn parses_branch_targets_as_expression_operands() {
        let handler = M68KFamilyHandler::new();
        let operands = handler
            .parse_operands("BRA", &[Expr::Identifier("target".to_string(), span())])
            .expect("branch target");

        assert!(matches!(
            &operands[0],
            FamilyOperand::BranchTarget {
                expr: Expr::Identifier(name, _),
                ..
            } if name == "target"
        ));
    }

    #[test]
    fn parses_dbcc_targets_as_expression_operands() {
        let handler = M68KFamilyHandler::new();
        let operands = handler
            .parse_operands(
                "DBRA",
                &[
                    Expr::Register("D1".to_string(), span()),
                    Expr::Identifier("loop".to_string(), span()),
                ],
            )
            .expect("dbcc operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::DataRegister { register, .. } if register == "D1"
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::BranchTarget {
                expr: Expr::Identifier(name, _),
                ..
            } if name == "loop"
        ));
    }

    #[test]
    fn parses_special_register_operands() {
        let handler = M68KFamilyHandler::new();
        let operands = handler
            .parse_operands(
                "MOVE",
                &[
                    Expr::Identifier("SR".to_string(), span()),
                    Expr::Identifier("CCR".to_string(), span()),
                    Expr::Identifier("USP".to_string(), span()),
                ],
            )
            .expect("special register operands");

        assert!(matches!(
            operands[0],
            FamilyOperand::SpecialRegister {
                register: SpecialRegisterKind::Sr,
                ..
            }
        ));
        assert!(matches!(
            operands[1],
            FamilyOperand::SpecialRegister {
                register: SpecialRegisterKind::Ccr,
                ..
            }
        ));
        assert!(matches!(
            operands[2],
            FamilyOperand::SpecialRegister {
                register: SpecialRegisterKind::Usp,
                ..
            }
        ));
    }

    #[test]
    fn parses_movem_register_list_operands() {
        let handler = M68KFamilyHandler::new();
        let register_list = Expr::Binary {
            op: BinaryOp::Divide,
            left: Box::new(Expr::Binary {
                op: BinaryOp::Subtract,
                left: Box::new(Expr::Register("D0".to_string(), span())),
                right: Box::new(Expr::Register("D2".to_string(), span())),
                span: span(),
            }),
            right: Box::new(Expr::Identifier("A6".to_string(), span())),
            span: span(),
        };
        let indirect = Expr::Indirect(Box::new(Expr::Register("A7".to_string(), span())), span());
        let operands = handler
            .parse_operands(
                "MOVEM.W",
                &[
                    register_list,
                    Expr::Unary {
                        op: UnaryOp::Minus,
                        expr: Box::new(indirect),
                        span: span(),
                    },
                ],
            )
            .expect("movem operands");

        assert!(matches!(
            &operands[0],
            FamilyOperand::RegisterList { registers, .. }
                if registers
                    == &[
                        RegisterListRegister::Data(0),
                        RegisterListRegister::Data(1),
                        RegisterListRegister::Data(2),
                        RegisterListRegister::Address(6),
                    ]
        ));
        assert!(matches!(
            &operands[1],
            FamilyOperand::AddressPredecrement { register, .. } if register == "A7"
        ));
    }

    #[test]
    fn rejects_duplicate_movem_registers() {
        let handler = M68KFamilyHandler::new();
        let duplicate_list = Expr::Binary {
            op: BinaryOp::Divide,
            left: Box::new(Expr::Register("D0".to_string(), span())),
            right: Box::new(Expr::Identifier("D0".to_string(), span())),
            span: span(),
        };

        let err = handler
            .parse_operands(
                "MOVEM.W",
                &[
                    duplicate_list,
                    Expr::Indirect(Box::new(Expr::Register("A0".to_string(), span())), span()),
                ],
            )
            .expect_err("duplicate MOVEM list should fail");
        assert!(err.message.contains("duplicate register in MOVEM list: D0"));
    }

    #[test]
    fn rejects_invalid_index_register_suffix() {
        let handler = M68KFamilyHandler::new();
        let operand = Expr::Indirect(
            Box::new(Expr::Tuple(
                vec![
                    Expr::Number("4".to_string(), span()),
                    Expr::Register("A0".to_string(), span()),
                    Expr::Member {
                        base: Box::new(Expr::Register("D1".to_string(), span())),
                        field: "Q".to_string(),
                        span: span(),
                    },
                ],
                span(),
            )),
            span(),
        );

        let err = handler
            .parse_operands("MOVE", &[operand])
            .expect_err("invalid suffix");
        assert!(err.message.contains("index register"));
    }

    #[test]
    fn parses_cas2_register_pair_operands() {
        let operands = parse_operands_from_source("    CAS2.L D0:D1,D2:D3,(A0):(A1)");
        assert!(matches!(
            operands.as_slice(),
            [
                FamilyOperand::RegisterPair { left, right, .. },
                FamilyOperand::RegisterPair {
                    left: update_left,
                    right: update_right,
                    ..
                },
                FamilyOperand::IndirectRegisterPair {
                    left: memory_left,
                    right: memory_right,
                    ..
                }
            ] if left == "D0"
                && right == "D1"
                && update_left == "D2"
                && update_right == "D3"
                && memory_left == "A0"
                && memory_right == "A1"
        ));
    }

    #[test]
    fn parses_bitfield_brace_operands() {
        let operands = parse_operands_from_source("    BFEXTU ($1234).W{D1:8},D2");
        let FamilyOperand::BitField {
            base,
            offset,
            width,
            ..
        } = &operands[0]
        else {
            panic!("expected bit-field operand, got {:?}", operands[0]);
        };
        assert!(matches!(
            base.as_ref(),
            FamilyOperand::Absolute {
                expr: Expr::Number(text, _),
                size: AbsoluteSize::Word,
                ..
            } if text == "$1234"
        ));
        assert!(matches!(
            offset,
            BitFieldSelector::DataRegister { register, .. } if register == "D1"
        ));
        assert!(matches!(
            width,
            BitFieldSelector::Immediate {
                expr: Expr::Number(text, _),
                ..
            } if text == "8"
        ));
    }

    #[test]
    fn rejects_absolute_suffix_on_register_indirect() {
        let handler = M68KFamilyHandler::new();
        let operand = Expr::Member {
            base: Box::new(Expr::Indirect(
                Box::new(Expr::Register("A0".to_string(), span())),
                span(),
            )),
            field: "W".to_string(),
            span: span(),
        };

        let err = handler
            .parse_operands("MOVE", &[operand])
            .expect_err("invalid absolute");
        assert!(err.message.contains("requires an expression"));
    }

    #[test]
    fn encodes_move_word_immediate_to_absolute_short() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();
        let operands = vec![
            Operand::Immediate {
                expr: Expr::Number("$1234".to_string(), span()),
                span: span(),
            },
            Operand::Absolute {
                expr: Expr::Number("$1234".to_string(), span()),
                size: AbsoluteSize::Word,
                span: span(),
            },
        ];

        expect_encoded(
            handler.encode_instruction("MOVE.W", &operands, &ctx),
            &[0x31, 0xFC, 0x12, 0x34, 0x12, 0x34],
        );
    }

    #[test]
    fn encodes_sign_extended_absolute_short_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "MOVE.W",
                &[
                    Operand::Absolute {
                        expr: Expr::Number("$FF8000".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x30, 0x38, 0x80, 0x00],
        );

        let invalid_absolute_short = handler.encode_instruction(
            "MOVE.W",
            &[
                Operand::Absolute {
                    expr: Expr::Number("$018000".to_string(), span()),
                    size: AbsoluteSize::Word,
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_absolute_short {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("68000 absolute .W address out of 16-bit range"));
            }
            other => panic!("expected absolute-short range error, got {other:?}"),
        }
    }

    #[test]
    fn encodes_movea_and_control_addressing_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default().with_symbol("target", 0x0012_3456);

        expect_encoded(
            handler.encode_instruction(
                "MOVEA.L",
                &[
                    Operand::Absolute {
                        expr: Expr::Identifier("target".to_string(), span()),
                        size: AbsoluteSize::Long,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x20, 0x79, 0x00, 0x12, 0x34, 0x56],
        );

        expect_encoded(
            handler.encode_instruction(
                "LEA",
                &[
                    Operand::AddressIndexed {
                        displacement: Expr::Number("4".to_string(), span()),
                        base: "A0".to_string(),
                        index: "D1".to_string(),
                        index_size: IndexSize::Word,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x43, 0xF0, 0x10, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "PEA",
                &[Operand::AddressDisplacement {
                    displacement: Expr::Number("4".to_string(), span()),
                    base: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0x68, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "JMP",
                &[Operand::Absolute {
                    expr: Expr::Identifier("target".to_string(), span()),
                    size: AbsoluteSize::Long,
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0xF9, 0x00, 0x12, 0x34, 0x56],
        );

        expect_encoded(
            handler.encode_instruction(
                "JSR",
                &[Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0x90],
        );
    }

    #[test]
    fn encodes_arithmetic_branch_quick_and_shift_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "ADD.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xD0, 0x7C, 0x00, 0x01],
        );

        expect_encoded(
            handler.encode_instruction(
                "ADDA.L",
                &[
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xD3, 0xD0],
        );

        expect_encoded(
            handler.encode_instruction(
                "AND.B",
                &[
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC0, 0x10],
        );

        expect_encoded(
            handler.encode_instruction(
                "SUB.L",
                &[
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x90, 0x81],
        );

        expect_encoded(
            handler.encode_instruction(
                "CMP.W",
                &[
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xB2, 0x50],
        );

        let invalid_cmp_memory_destination = handler.encode_instruction(
            "CMP.W",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_cmp_memory_destination {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for CMP.W"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        expect_encoded(
            handler.encode_instruction(
                "OR.L",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$12345678".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x84, 0xBC, 0x12, 0x34, 0x56, 0x78],
        );

        expect_encoded(
            handler.encode_instruction(
                "EOR.W",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xB1, 0x51],
        );

        expect_encoded(
            handler.encode_instruction(
                "BRA",
                &[Operand::BranchTarget {
                    expr: Expr::Number("4".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x60, 0x00, 0x00, 0x02],
        );

        expect_encoded(
            handler.encode_instruction(
                "BNE.W",
                &[Operand::BranchTarget {
                    expr: Expr::Number("8".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x66, 0x00, 0x00, 0x06],
        );

        expect_encoded(
            handler.encode_instruction(
                "BSR.W",
                &[Operand::BranchTarget {
                    expr: Expr::Number("8".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x61, 0x00, 0x00, 0x06],
        );

        expect_encoded(handler.encode_instruction("RTS", &[], &ctx), &[0x4E, 0x75]);

        expect_encoded(
            handler.encode_instruction(
                "MOVEQ",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("-1".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x70, 0xFF],
        );

        expect_encoded(
            handler.encode_instruction(
                "ADDQ.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("8".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x50, 0x40],
        );

        expect_encoded(
            handler.encode_instruction(
                "SUBQ.L",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x53, 0x90],
        );

        expect_encoded(
            handler.encode_instruction(
                "ASL.B",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xE3, 0x00],
        );

        expect_encoded(
            handler.encode_instruction(
                "ROR.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D3".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xE2, 0x5B],
        );
    }

    #[test]
    fn encodes_data_register_to_memory_binary_op_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "ADD.W",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xD1, 0x50],
        );

        expect_encoded(
            handler.encode_instruction(
                "SUB.W",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x91, 0x50],
        );

        expect_encoded(
            handler.encode_instruction(
                "AND.W",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC1, 0x50],
        );

        expect_encoded(
            handler.encode_instruction(
                "OR.W",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x81, 0x50],
        );
    }

    #[test]
    fn encodes_immediate_and_unary_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "ORI.B",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$12".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x00, 0x00, 0x00, 0x12],
        );

        expect_encoded(
            handler.encode_instruction(
                "ANDI.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$1234".to_string(), span()),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x02, 0x50, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "ADDI.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::AddressDisplacement {
                        displacement: Expr::Number("4".to_string(), span()),
                        base: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x06, 0x68, 0x00, 0x01, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "SUBI.B",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::AddressPostincrement {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x04, 0x19, 0x00, 0x01],
        );

        expect_encoded(
            handler.encode_instruction(
                "EORI.L",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$12345678".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0A, 0x81, 0x12, 0x34, 0x56, 0x78],
        );

        expect_encoded(
            handler.encode_instruction(
                "CMPI.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$1234".to_string(), span()),
                        span: span(),
                    },
                    Operand::Absolute {
                        expr: Expr::Number("$1234".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0C, 0x78, 0x12, 0x34, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "CMPI.W",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::PcDisplacement {
                        displacement: Expr::Number("4".to_string(), span()),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0C, 0x7A, 0x00, 0x01, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "CLR.W",
                &[Operand::DataRegister {
                    register: "D2".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x42, 0x42],
        );

        expect_encoded(
            handler.encode_instruction(
                "NEG.B",
                &[Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x44, 0x10],
        );

        expect_encoded(
            handler.encode_instruction(
                "NOT.L",
                &[Operand::DataRegister {
                    register: "D3".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x46, 0x83],
        );

        expect_encoded(
            handler.encode_instruction(
                "TST.W",
                &[Operand::AddressDisplacement {
                    displacement: Expr::Number("4".to_string(), span()),
                    base: "A3".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4A, 0x6B, 0x00, 0x04],
        );
    }

    #[test]
    fn encodes_condition_loop_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "SNE",
                &[Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x56, 0xC0],
        );

        expect_encoded(
            handler.encode_instruction(
                "ST",
                &[Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x50, 0xD0],
        );

        expect_encoded(
            handler.encode_instruction(
                "SF",
                &[Operand::Absolute {
                    expr: Expr::Number("$1234".to_string(), span()),
                    size: AbsoluteSize::Word,
                    span: span(),
                }],
                &ctx,
            ),
            &[0x51, 0xF8, 0x12, 0x34],
        );

        let loop_ctx = TestContext {
            current_address: 0,
            ..Default::default()
        };
        expect_encoded(
            handler.encode_instruction(
                "DBRA",
                &[
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                    Operand::BranchTarget {
                        expr: Expr::Number("0".to_string(), span()),
                        span: span(),
                    },
                ],
                &loop_ctx,
            ),
            &[0x51, 0xC9, 0xFF, 0xFE],
        );

        expect_encoded(
            handler.encode_instruction(
                "DBNE",
                &[
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                    Operand::BranchTarget {
                        expr: Expr::Number("8".to_string(), span()),
                        span: span(),
                    },
                ],
                &loop_ctx,
            ),
            &[0x56, 0xCA, 0x00, 0x06],
        );
    }

    #[test]
    fn encodes_bit_operation_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "BTST",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("3".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x08, 0x00, 0x00, 0x03],
        );

        expect_encoded(
            handler.encode_instruction(
                "BTST",
                &[
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x03, 0x10],
        );

        expect_encoded(
            handler.encode_instruction(
                "BTST",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::AddressDisplacement {
                        base: "A1".to_string(),
                        displacement: Expr::Number("4".to_string(), span()),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x08, 0x29, 0x00, 0x01, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "BTST",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::PcDisplacement {
                        displacement: Expr::Number("4".to_string(), span()),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x08, 0x3A, 0x00, 0x01, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "BCHG",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("5".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x08, 0x42, 0x00, 0x05],
        );

        expect_encoded(
            handler.encode_instruction(
                "BCHG",
                &[
                    Operand::DataRegister {
                        register: "D3".to_string(),
                        span: span(),
                    },
                    Operand::Absolute {
                        expr: Expr::Number("$1234".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x07, 0x78, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "BCLR",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::AddressIndirect {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x08, 0x91, 0x00, 0x01],
        );

        expect_encoded(
            handler.encode_instruction(
                "BCLR",
                &[
                    Operand::DataRegister {
                        register: "D4".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D5".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x09, 0x85],
        );

        expect_encoded(
            handler.encode_instruction(
                "BSET",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("7".to_string(), span()),
                        span: span(),
                    },
                    Operand::Absolute {
                        expr: Expr::Number("$123456".to_string(), span()),
                        size: AbsoluteSize::Long,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x08, 0xF9, 0x00, 0x07, 0x00, 0x12, 0x34, 0x56],
        );

        expect_encoded(
            handler.encode_instruction(
                "BSET",
                &[
                    Operand::DataRegister {
                        register: "D6".to_string(),
                        span: span(),
                    },
                    Operand::AddressDisplacement {
                        displacement: Expr::Number("4".to_string(), span()),
                        base: "A2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0D, 0xEA, 0x00, 0x04],
        );
    }

    #[test]
    fn encodes_multiply_divide_check_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "CHK",
                &[
                    Operand::Absolute {
                        expr: Expr::Number("$1234".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x41, 0xB8, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "MULU.W",
                &[
                    Operand::AddressIndirect {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC2, 0xD0],
        );

        expect_encoded(
            handler.encode_instruction(
                "MULS",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$00FF".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC5, 0xFC, 0x00, 0xFF],
        );

        expect_encoded(
            handler.encode_instruction(
                "DIVU",
                &[
                    Operand::Absolute {
                        expr: Expr::Number("$123456".to_string(), span()),
                        size: AbsoluteSize::Long,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D3".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x86, 0xF9, 0x00, 0x12, 0x34, 0x56],
        );

        expect_encoded(
            handler.encode_instruction(
                "DIVS.W",
                &[
                    Operand::PcDisplacement {
                        displacement: Expr::Number("4".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D4".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x89, 0xFA, 0x00, 0x04],
        );
    }

    #[test]
    fn encodes_extend_bcd_and_cmpm_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "ADDX.B",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xD3, 0x00],
        );

        expect_encoded(
            handler.encode_instruction(
                "ADDX.W",
                &[
                    Operand::AddressPredecrement {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::AddressPredecrement {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xD3, 0x48],
        );

        expect_encoded(
            handler.encode_instruction(
                "SUBX.L",
                &[
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D3".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x97, 0x82],
        );

        expect_encoded(
            handler.encode_instruction(
                "ABCD",
                &[
                    Operand::DataRegister {
                        register: "D4".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D5".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xCB, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "SBCD",
                &[
                    Operand::AddressPredecrement {
                        register: "A2".to_string(),
                        span: span(),
                    },
                    Operand::AddressPredecrement {
                        register: "A3".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x87, 0x0A],
        );

        expect_encoded(
            handler.encode_instruction(
                "CMPM.W",
                &[
                    Operand::AddressPostincrement {
                        register: "A4".to_string(),
                        span: span(),
                    },
                    Operand::AddressPostincrement {
                        register: "A5".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xBB, 0x4C],
        );
    }

    #[test]
    fn encodes_rotate_and_memory_shift_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "ROXL.B",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("1".to_string(), span()),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xE3, 0x10],
        );

        expect_encoded(
            handler.encode_instruction(
                "ROXR.W",
                &[
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xE2, 0x72],
        );

        expect_encoded(
            handler.encode_instruction(
                "ASL",
                &[Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0xE1, 0xD0],
        );

        expect_encoded(
            handler.encode_instruction(
                "LSR.W",
                &[Operand::Absolute {
                    expr: Expr::Number("$1234".to_string(), span()),
                    size: AbsoluteSize::Word,
                    span: span(),
                }],
                &ctx,
            ),
            &[0xE2, 0xF8, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "ROXL",
                &[Operand::AddressDisplacement {
                    displacement: Expr::Number("4".to_string(), span()),
                    base: "A1".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0xE5, 0xE9, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "ROR.W",
                &[Operand::AddressPredecrement {
                    register: "A2".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0xE6, 0xE2],
        );
    }

    #[test]
    fn encodes_movem_and_movep_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "MOVEM.W",
                &[
                    Operand::RegisterList {
                        registers: vec![
                            RegisterListRegister::Data(0),
                            RegisterListRegister::Data(1),
                            RegisterListRegister::Data(2),
                            RegisterListRegister::Address(6),
                        ],
                        span: span(),
                    },
                    Operand::AddressPredecrement {
                        register: "A7".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x48, 0xA7, 0xE0, 0x02],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVEM.L",
                &[
                    Operand::AddressPostincrement {
                        register: "A0".to_string(),
                        span: span(),
                    },
                    Operand::RegisterList {
                        registers: vec![
                            RegisterListRegister::Data(1),
                            RegisterListRegister::Data(3),
                            RegisterListRegister::Address(2),
                            RegisterListRegister::Address(3),
                            RegisterListRegister::Address(4),
                        ],
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4C, 0xD8, 0x1C, 0x0A],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVEP.W",
                &[
                    Operand::DataRegister {
                        register: "D5".to_string(),
                        span: span(),
                    },
                    Operand::AddressDisplacement {
                        displacement: Expr::Number("4".to_string(), span()),
                        base: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0B, 0x89, 0x00, 0x04],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVEP.L",
                &[
                    Operand::AddressDisplacement {
                        displacement: Expr::Number("6".to_string(), span()),
                        base: "A2".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D6".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0D, 0x4A, 0x00, 0x06],
        );
    }

    #[test]
    fn encodes_system_and_register_utility_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "LINK",
                &[
                    Operand::AddressRegister {
                        register: "A6".to_string(),
                        span: span(),
                    },
                    Operand::Immediate {
                        expr: Expr::Number("-8".to_string(), span()),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4E, 0x56, 0xFF, 0xF8],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Sr,
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x40, 0xC0],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::Absolute {
                        expr: Expr::Number("$1234".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Ccr,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x44, 0xF8, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$2700".to_string(), span()),
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Sr,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x46, 0xFC, 0x27, 0x00],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Usp,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4E, 0x69],
        );

        expect_encoded(
            handler.encode_instruction(
                "MOVE",
                &[
                    Operand::AddressRegister {
                        register: "A2".to_string(),
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Usp,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x4E, 0x62],
        );

        expect_encoded(
            handler.encode_instruction(
                "ANDI",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$1F".to_string(), span()),
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Ccr,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x02, 0x3C, 0x00, 0x1F],
        );

        expect_encoded(
            handler.encode_instruction(
                "ORI",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$2700".to_string(), span()),
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Sr,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x00, 0x7C, 0x27, 0x00],
        );

        expect_encoded(
            handler.encode_instruction(
                "EORI",
                &[
                    Operand::Immediate {
                        expr: Expr::Number("$0F".to_string(), span()),
                        span: span(),
                    },
                    Operand::SpecialRegister {
                        register: SpecialRegisterKind::Ccr,
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0x0A, 0x3C, 0x00, 0x0F],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXG",
                &[
                    Operand::DataRegister {
                        register: "D0".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC1, 0x41],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXG",
                &[
                    Operand::AddressRegister {
                        register: "A2".to_string(),
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A3".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC5, 0x4B],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXG",
                &[
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A3".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC5, 0x8B],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXG",
                &[
                    Operand::AddressRegister {
                        register: "A3".to_string(),
                        span: span(),
                    },
                    Operand::DataRegister {
                        register: "D2".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xC5, 0x8B],
        );

        expect_encoded(
            handler.encode_instruction(
                "SWAP",
                &[Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0x40],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXT.W",
                &[Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0x81],
        );

        expect_encoded(
            handler.encode_instruction(
                "EXT.L",
                &[Operand::DataRegister {
                    register: "D2".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0xC2],
        );

        expect_encoded(
            handler.encode_instruction(
                "TRAP",
                &[Operand::Immediate {
                    expr: Expr::Number("15".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0x4F],
        );

        expect_encoded(handler.encode_instruction("NOP", &[], &ctx), &[0x4E, 0x71]);

        expect_encoded(
            handler.encode_instruction(
                "STOP",
                &[Operand::Immediate {
                    expr: Expr::Number("$2700".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0x72, 0x27, 0x00],
        );

        expect_encoded(
            handler.encode_instruction(
                "UNLK",
                &[Operand::AddressRegister {
                    register: "A6".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4E, 0x5E],
        );

        expect_encoded(
            handler.encode_instruction("RESET", &[], &ctx),
            &[0x4E, 0x70],
        );
        expect_encoded(handler.encode_instruction("RTE", &[], &ctx), &[0x4E, 0x73]);
        expect_encoded(handler.encode_instruction("RTR", &[], &ctx), &[0x4E, 0x77]);
        expect_encoded(
            handler.encode_instruction("TRAPV", &[], &ctx),
            &[0x4E, 0x76],
        );
        expect_encoded(
            handler.encode_instruction("ILLEGAL", &[], &ctx),
            &[0x4A, 0xFC],
        );
    }

    #[test]
    fn encodes_compare_and_operand_state_slice() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        expect_encoded(
            handler.encode_instruction(
                "CMPA.W",
                &[
                    Operand::Absolute {
                        expr: Expr::Number("$1234".to_string(), span()),
                        size: AbsoluteSize::Word,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A0".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xB0, 0xF8, 0x12, 0x34],
        );

        expect_encoded(
            handler.encode_instruction(
                "CMPA.L",
                &[
                    Operand::Absolute {
                        expr: Expr::Number("$123456".to_string(), span()),
                        size: AbsoluteSize::Long,
                        span: span(),
                    },
                    Operand::AddressRegister {
                        register: "A1".to_string(),
                        span: span(),
                    },
                ],
                &ctx,
            ),
            &[0xB3, 0xF9, 0x00, 0x12, 0x34, 0x56],
        );

        expect_encoded(
            handler.encode_instruction(
                "NEGX.B",
                &[Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x40, 0x00],
        );

        expect_encoded(
            handler.encode_instruction(
                "NEGX.W",
                &[Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x40, 0x50],
        );

        expect_encoded(
            handler.encode_instruction(
                "NBCD",
                &[Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0x01],
        );

        expect_encoded(
            handler.encode_instruction(
                "NBCD",
                &[Operand::AddressPredecrement {
                    register: "A2".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x48, 0x22],
        );

        expect_encoded(
            handler.encode_instruction(
                "TAS",
                &[Operand::DataRegister {
                    register: "D2".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4A, 0xC2],
        );

        expect_encoded(
            handler.encode_instruction(
                "TAS",
                &[Operand::AddressIndirect {
                    register: "A3".to_string(),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x4A, 0xD3],
        );
    }

    #[test]
    fn rejects_invalid_effective_address_combinations() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        let invalid_move = handler.encode_instruction(
            "MOVE.W",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_move {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for MOVE.W"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_lea = handler.encode_instruction(
            "LEA",
            &[
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_lea {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid source effective address for LEA"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_addi = handler.encode_instruction(
            "ADDI.W",
            &[
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_addi {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for ADDI.W"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_tst = handler.encode_instruction(
            "TST.W",
            &[Operand::AddressRegister {
                register: "A0".to_string(),
                span: span(),
            }],
            &ctx,
        );
        match invalid_tst {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for TST.W"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_swap = handler.encode_instruction(
            "SWAP",
            &[Operand::AddressRegister {
                register: "A0".to_string(),
                span: span(),
            }],
            &ctx,
        );
        match invalid_swap {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("SWAP operand must be a data register"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_link = handler.encode_instruction(
            "LINK",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_link {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("LINK first operand must be an address register"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_trap = handler.encode_instruction(
            "TRAP",
            &[Operand::Immediate {
                expr: Expr::Number("16".to_string(), span()),
                span: span(),
            }],
            &ctx,
        );
        match invalid_trap {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("TRAP vector 16 out of range"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_cmpa = handler.encode_instruction(
            "CMPA.B",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_cmpa {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("does not support .B size"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_nbcd = handler.encode_instruction(
            "NBCD",
            &[Operand::AddressRegister {
                register: "A0".to_string(),
                span: span(),
            }],
            &ctx,
        );
        match invalid_nbcd {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for NBCD"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_exg = handler.encode_instruction(
            "EXG",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_exg {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("data/address register pairs"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_sne = handler.encode_instruction(
            "SNE",
            &[Operand::AddressRegister {
                register: "A0".to_string(),
                span: span(),
            }],
            &ctx,
        );
        match invalid_sne {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for SNE"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_dbra = handler.encode_instruction(
            "DBRA",
            &[
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
                Operand::BranchTarget {
                    expr: Expr::Number("0".to_string(), span()),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_dbra {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("counter must be a data register"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_move_ccr = handler.encode_instruction(
            "MOVE",
            &[
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Ccr,
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_move_ccr {
            EncodeResult::NotFound => {}
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_move_usp = handler.encode_instruction(
            "MOVE",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Usp,
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_move_usp {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("MOVE USP source must be an address register"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_andi_ccr = handler.encode_instruction(
            "ANDI.W",
            &[
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Ccr,
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_andi_ccr {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("ANDI does not support .W size"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_move_to_sr = handler.encode_instruction(
            "MOVE",
            &[
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
                Operand::SpecialRegister {
                    register: SpecialRegisterKind::Sr,
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_move_to_sr {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid source effective address for MOVE to SR"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_btst = handler.encode_instruction(
            "BTST",
            &[
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_btst {
            EncodeResult::Error(message, _) => {
                assert!(
                    message.contains("BTST bit number must be an immediate value or data register")
                );
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_bchg = handler.encode_instruction(
            "BCHG",
            &[
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
                Operand::PcDisplacement {
                    displacement: Expr::Number("4".to_string(), span()),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_bchg {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for BCHG"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_bclr = handler.encode_instruction(
            "BCLR",
            &[
                Operand::Immediate {
                    expr: Expr::Number("-1".to_string(), span()),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_bclr {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("BCLR bit number -1 out of range (0-255)"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_chk = handler.encode_instruction(
            "CHK.L",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_chk {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("CHK does not support .L size"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_mulu = handler.encode_instruction(
            "MULU",
            &[
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_mulu {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid source effective address for MULU"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_divu = handler.encode_instruction(
            "DIVU",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_divu {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("DIVU destination must be a data register"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_addx = handler.encode_instruction(
            "ADDX.W",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressPredecrement {
                    register: "A1".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_addx {
            EncodeResult::Error(message, _) => {
                assert!(message.contains(
                    "ADDX operands must both be data registers or both be predecrement address operands"
                ));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_abcd = handler.encode_instruction(
            "ABCD.B",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_abcd {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("ABCD does not accept a size suffix"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_cmpm = handler.encode_instruction(
            "CMPM.W",
            &[
                Operand::AddressRegister {
                    register: "A0".to_string(),
                    span: span(),
                },
                Operand::AddressRegister {
                    register: "A1".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_cmpm {
            EncodeResult::Error(message, _) => {
                assert!(
                    message.contains("CMPM operands must both be postincrement address operands")
                );
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_roxl_memory_size = handler.encode_instruction(
            "ROXL.L",
            &[Operand::AddressIndirect {
                register: "A0".to_string(),
                span: span(),
            }],
            &ctx,
        );
        match invalid_roxl_memory_size {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("ROXL memory form does not support .L size"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_roxr_register_shape = handler.encode_instruction(
            "ROXR.W",
            &[
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_roxr_register_shape {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("ROXR destination must be a data register"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_ror_memory_ea = handler.encode_instruction(
            "ROR",
            &[Operand::PcDisplacement {
                displacement: Expr::Number("4".to_string(), span()),
                span: span(),
            }],
            &ctx,
        );
        match invalid_ror_memory_ea {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for ROR"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_movem_size = handler.encode_instruction(
            "MOVEM.B",
            &[
                Operand::RegisterList {
                    registers: vec![RegisterListRegister::Data(0)],
                    span: span(),
                },
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_movem_size {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("MOVEM does not support .B size"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_movem_destination = handler.encode_instruction(
            "MOVEM.W",
            &[
                Operand::RegisterList {
                    registers: vec![RegisterListRegister::Data(0)],
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D1".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_movem_destination {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("invalid destination effective address for MOVEM"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_movep_addressing = handler.encode_instruction(
            "MOVEP.W",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressIndirect {
                    register: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_movep_addressing {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("MOVEP memory operand must use d16(An) addressing"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }

        let invalid_movep_size = handler.encode_instruction(
            "MOVEP.B",
            &[
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
                Operand::AddressDisplacement {
                    displacement: Expr::Number("4".to_string(), span()),
                    base: "A0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        );
        match invalid_movep_size {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("MOVEP does not support .B size"));
            }
            other => panic!("expected legality error, got {other:?}"),
        }
    }

    #[test]
    fn rejects_nondeterministic_or_out_of_range_baseline_forms() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext::default();

        match handler.encode_instruction(
            "ADD",
            &[
                Operand::Immediate {
                    expr: Expr::Number("1".to_string(), span()),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("explicit size suffix"));
            }
            other => panic!("expected ADD size diagnostic, got {other:?}"),
        }

        let pass1_ctx = TestContext {
            pass: 1,
            ..Default::default()
        };
        match handler.encode_instruction(
            "BRA",
            &[Operand::BranchTarget {
                expr: Expr::Identifier("later".to_string(), span()),
                span: span(),
            }],
            &pass1_ctx,
        ) {
            EncodeResult::Ok(bytes) => {
                assert_eq!(bytes, vec![0x60, 0x00, 0x00, 0x00]);
            }
            other => panic!("expected unresolved BRA word placeholder, got {other:?}"),
        }

        match handler.encode_instruction(
            "BRA.B",
            &[Operand::BranchTarget {
                expr: Expr::Number("2".to_string(), span()),
                span: span(),
            }],
            &ctx,
        ) {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("zero displacement"));
            }
            other => panic!("expected BRA.B zero displacement diagnostic, got {other:?}"),
        }

        match handler.encode_instruction(
            "MOVEQ",
            &[
                Operand::Immediate {
                    expr: Expr::Number("128".to_string(), span()),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("signed 8-bit range"));
            }
            other => panic!("expected MOVEQ range diagnostic, got {other:?}"),
        }

        match handler.encode_instruction(
            "ADDQ.W",
            &[
                Operand::Immediate {
                    expr: Expr::Number("9".to_string(), span()),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("out of range (1-8)"));
            }
            other => panic!("expected ADDQ range diagnostic, got {other:?}"),
        }

        match handler.encode_instruction(
            "ASL.B",
            &[
                Operand::Immediate {
                    expr: Expr::Number("9".to_string(), span()),
                    span: span(),
                },
                Operand::DataRegister {
                    register: "D0".to_string(),
                    span: span(),
                },
            ],
            &ctx,
        ) {
            EncodeResult::Error(message, _) => {
                assert!(message.contains("out of range (1-8)"));
            }
            other => panic!("expected ASL range diagnostic, got {other:?}"),
        }
    }

    #[test]
    fn branch_word_displacements_are_based_on_pc_plus_two() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext {
            current_address: 0x1000,
            pass: 2,
            ..Default::default()
        }
        .with_symbol("target", 0x1008);

        expect_encoded(
            handler.encode_instruction(
                "BRA.W",
                &[Operand::BranchTarget {
                    expr: Expr::Identifier("target".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x60, 0x00, 0x00, 0x06],
        );
    }

    #[test]
    fn branch_long_displacements_are_based_on_pc_plus_two() {
        let handler = M68KFamilyHandler::new();
        let ctx = TestContext {
            current_address: 0x1000,
            pass: 2,
            ..Default::default()
        }
        .with_symbol("target", 0x1008);

        expect_encoded(
            handler.encode_long_branch_instruction(
                "BRA",
                None,
                &[Operand::BranchTarget {
                    expr: Expr::Identifier("target".to_string(), span()),
                    span: span(),
                }],
                &ctx,
            ),
            &[0x60, 0xFF, 0x00, 0x00, 0x00, 0x06],
        );
    }
}
