// SPDX-License-Identifier: GPL-3.0-or-later

//! Operand parsing helpers extracted from the M68k family handler.

use super::*;

impl M68KFamilyHandler {
    pub(super) fn parse_register_name(expr: &Expr) -> Option<(String, opcore::tokenizer::Span)> {
        match expr {
            Expr::Register(name, span) | Expr::Identifier(name, span) if is_register(name) => {
                Some((name.to_ascii_uppercase(), *span))
            }
            _ => None,
        }
    }

    pub(super) fn parse_data_register(expr: &Expr) -> Option<(String, opcore::tokenizer::Span)> {
        let (name, span) = Self::parse_register_name(expr)?;
        if is_data_register(&name) {
            Some((name, span))
        } else {
            None
        }
    }

    pub(super) fn parse_address_register(expr: &Expr) -> Option<(String, opcore::tokenizer::Span)> {
        let (name, span) = Self::parse_register_name(expr)?;
        if is_address_register(&name) {
            Some((name, span))
        } else {
            None
        }
    }

    pub(super) fn parse_special_register(
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

    pub(super) fn parse_control_register(
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
            "TC" => ControlRegisterKind::Tc,
            "ITT0" => ControlRegisterKind::Itt0,
            "ITT1" => ControlRegisterKind::Itt1,
            "DTT0" => ControlRegisterKind::Dtt0,
            "DTT1" => ControlRegisterKind::Dtt1,
            "MMUSR" => ControlRegisterKind::Mmusr,
            "URP" => ControlRegisterKind::Urp,
            "SRP" => ControlRegisterKind::Srp,
            "PCR" => ControlRegisterKind::Pcr,
            "CCC" => ControlRegisterKind::Ccc,
            "IEP1" => ControlRegisterKind::Iep1,
            "IEP2" => ControlRegisterKind::Iep2,
            "BPC" => ControlRegisterKind::Bpc,
            "BPW" => ControlRegisterKind::Bpw,
            "DCH" => ControlRegisterKind::Dch,
            "DCM" => ControlRegisterKind::Dcm,
            "STR" => ControlRegisterKind::Str,
            "STC" => ControlRegisterKind::Stc,
            "IEP3" | "STH" => ControlRegisterKind::Iep3,
            "STB" => ControlRegisterKind::Stb,
            "MWR" => ControlRegisterKind::Mwr,
            _ => return None,
        };
        Some((register, span))
    }

    pub(super) fn parse_fpu_data_register(
        expr: &Expr,
    ) -> Option<(String, opcore::tokenizer::Span)> {
        let (name, span) = match expr {
            Expr::Register(name, span) | Expr::Identifier(name, span) => {
                (name.to_ascii_uppercase(), *span)
            }
            _ => return None,
        };
        if Self::fpu_data_register_number(&name).is_some()
            || Self::fpu_banked_data_register_number(&name).is_some()
        {
            Some((name, span))
        } else {
            None
        }
    }

    pub(super) fn parse_fpu_control_register(
        expr: &Expr,
    ) -> Option<(FpuControlRegisterKind, opcore::tokenizer::Span)> {
        let (name, span) = match expr {
            Expr::Register(name, span) | Expr::Identifier(name, span) => {
                (name.to_ascii_uppercase(), *span)
            }
            _ => return None,
        };
        let register = match name.as_str() {
            "FPCR" => FpuControlRegisterKind::Fpcr,
            "FPSR" => FpuControlRegisterKind::Fpsr,
            "FPIAR" => FpuControlRegisterKind::Fpiar,
            _ => return None,
        };
        Some((register, span))
    }

    pub(super) fn parse_pc_register(expr: &Expr) -> Option<opcore::tokenizer::Span> {
        match expr {
            Expr::Register(name, span) | Expr::Identifier(name, span)
                if name.eq_ignore_ascii_case("PC") =>
            {
                Some(*span)
            }
            _ => None,
        }
    }

    pub(super) fn parse_scaled_index_register(
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

    pub(super) fn parse_index_register(
        expr: &Expr,
    ) -> Option<(String, IndexSize, opcore::tokenizer::Span)> {
        let (register, size, scale, span) = Self::parse_scaled_index_register(expr)?;
        (scale == IndexScale::One).then_some((register, size, span))
    }

    pub(super) fn parse_general_register(expr: &Expr) -> Option<(String, opcore::tokenizer::Span)> {
        Self::parse_data_register(expr).or_else(|| Self::parse_address_register(expr))
    }

    pub(super) fn parse_pair_operand(
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

        if let (Some((left_name, _)), Some((right_name, _))) = (
            Self::parse_fpu_data_register(left),
            Self::parse_fpu_data_register(right),
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
            "68020 register-pair syntax requires Rn:Rn, FPn:FPn, or (Rn):(Rn)",
            span,
        ))
    }

    pub(super) fn parse_group_operand(
        &self,
        start: &Expr,
        end: &Expr,
        span: opcore::tokenizer::Span,
    ) -> Result<FamilyOperand, FamilyParseError> {
        let (Some((start_name, _)), Some((end_name, _))) = (
            Self::parse_general_register(start),
            Self::parse_general_register(end),
        ) else {
            return Err(FamilyParseError::new(
                "68000 register-group syntax requires Rn-Rn",
                span,
            ));
        };

        Ok(FamilyOperand::RegisterGroup {
            start: start_name,
            end: end_name,
            span,
        })
    }

    pub(super) fn parse_bit_field_selector(
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

    pub(super) fn parse_bit_field_operand(
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

    pub(super) fn parse_full_extension_displacement(
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

    pub(super) fn parse_outer_displacement(
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

    pub(super) fn parse_full_extension_base(
        expr: &Expr,
    ) -> Result<FullExtensionBase, FamilyParseError> {
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

    pub(super) fn parse_full_extension_index(
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

    pub(super) fn parse_full_extension_tuple(
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

    pub(super) fn build_full_extension_operand(
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

    pub(super) fn parse_preindexed_indirect_inner(
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

    pub(super) fn parse_postindexed_indirect_inner(
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

    pub(super) fn parse_preindexed_memory_indirect(
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

    pub(super) fn parse_postindexed_memory_indirect(
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

    pub(super) fn parse_memory_indirect_tuple(
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

    pub(super) fn parse_register_list_register(
        expr: &Expr,
        family: RegisterListFamily,
    ) -> Option<(RegisterListRegister, opcore::tokenizer::Span)> {
        match family {
            RegisterListFamily::Integer => {
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
            RegisterListFamily::Fpu => {
                if let Some((name, span)) = Self::parse_fpu_data_register(expr) {
                    let reg = Self::fpu_data_register_number(&name)?;
                    return Some((RegisterListRegister::FpuData(reg), span));
                }
                let (register, span) = Self::parse_fpu_control_register(expr)?;
                Some((RegisterListRegister::FpuControl(register), span))
            }
        }
    }

    pub(super) fn register_list_candidate(expr: &Expr, family: RegisterListFamily) -> bool {
        if Self::parse_register_list_register(expr, family).is_some() {
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

    pub(super) fn format_register_list_register(register: RegisterListRegister) -> String {
        match register {
            RegisterListRegister::Data(reg) => format!("D{reg}"),
            RegisterListRegister::Address(reg) => format!("A{reg}"),
            RegisterListRegister::FpuData(reg) => format!("FP{reg}"),
            RegisterListRegister::FpuControl(register) => match register {
                FpuControlRegisterKind::Fpcr => "FPCR".to_string(),
                FpuControlRegisterKind::Fpsr => "FPSR".to_string(),
                FpuControlRegisterKind::Fpiar => "FPIAR".to_string(),
            },
        }
    }

    pub(super) fn expected_register_list_item_description(
        family: RegisterListFamily,
    ) -> &'static str {
        match family {
            RegisterListFamily::Integer => "data or address register",
            RegisterListFamily::Fpu => "FPU data or control register",
        }
    }

    pub(super) fn expand_register_list_range(
        start: RegisterListRegister,
        end: RegisterListRegister,
        family: RegisterListFamily,
        mnemonic_name: &str,
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
            (RegisterListRegister::FpuData(start), RegisterListRegister::FpuData(end))
                if start <= end =>
            {
                Ok((start..=end).map(RegisterListRegister::FpuData).collect())
            }
            (RegisterListRegister::FpuControl(_), RegisterListRegister::FpuControl(_)) => {
                Err(FamilyParseError::new(
                    format!("{mnemonic_name} control-register lists do not support ranges"),
                    span,
                ))
            }
            _ => Err(FamilyParseError::new(
                format!(
                    "{mnemonic_name} register ranges must stay within one ascending {} family",
                    match family {
                        RegisterListFamily::Integer => "integer register",
                        RegisterListFamily::Fpu => "FPU register",
                    }
                ),
                span,
            )),
        }
    }

    pub(super) fn flatten_register_list_tokens(
        expr: &Expr,
        tokens: &mut Vec<MovemRegisterToken>,
        family: RegisterListFamily,
        mnemonic_name: &str,
    ) -> Result<(), FamilyParseError> {
        if let Some((register, span)) = Self::parse_register_list_register(expr, family) {
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
                format!(
                    "invalid {mnemonic_name} register list; expected {}s, ranges, and '/' separators",
                    Self::expected_register_list_item_description(family)
                ),
                span_from_expr(expr),
            ));
        };

        match op {
            BinaryOp::Divide => {
                Self::flatten_register_list_tokens(left, tokens, family, mnemonic_name)?;
                tokens.push(MovemRegisterToken::Separator(*span));
                Self::flatten_register_list_tokens(right, tokens, family, mnemonic_name)
            }
            BinaryOp::Subtract => {
                Self::flatten_register_list_tokens(left, tokens, family, mnemonic_name)?;
                tokens.push(MovemRegisterToken::Range(*span));
                Self::flatten_register_list_tokens(right, tokens, family, mnemonic_name)
            }
            _ => Err(FamilyParseError::new(
                format!(
                    "invalid {mnemonic_name} register list; expected {}s, ranges, and '/' separators",
                    Self::expected_register_list_item_description(family)
                ),
                *span,
            )),
        }
    }

    pub(super) fn parse_register_list(
        &self,
        expr: &Expr,
        family: RegisterListFamily,
        mnemonic_name: &str,
    ) -> Result<FamilyOperand, FamilyParseError> {
        let mut tokens = Vec::new();
        Self::flatten_register_list_tokens(expr, &mut tokens, family, mnemonic_name)?;

        let mut registers = Vec::new();
        let mut seen = HashSet::new();
        let mut index = 0;

        while index < tokens.len() {
            let (start, start_span) = match tokens.get(index) {
                Some(MovemRegisterToken::Register(register, span)) => (*register, *span),
                Some(MovemRegisterToken::Separator(span))
                | Some(MovemRegisterToken::Range(span)) => {
                    return Err(FamilyParseError::new(
                        format!(
                            "{mnemonic_name} register list items must begin with a {}",
                            Self::expected_register_list_item_description(family)
                        ),
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
                        format!(
                            "{mnemonic_name} register ranges must end with a {}",
                            Self::expected_register_list_item_description(family)
                        ),
                        range_span,
                    ));
                };
                index += 1;
                Self::expand_register_list_range(start, *end, family, mnemonic_name, range_span)?
            } else {
                vec![start]
            };

            for register in item_registers {
                if !seen.insert(register) {
                    return Err(FamilyParseError::new(
                        format!(
                            "duplicate register in {mnemonic_name} list: {}",
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
                        format!(
                            "{mnemonic_name} register ranges must start with a {}",
                            Self::expected_register_list_item_description(family)
                        ),
                        *span,
                    ))
                }
                Some(MovemRegisterToken::Register(_, span)) => {
                    return Err(FamilyParseError::new(
                        format!("{mnemonic_name} register list items must be separated with '/'"),
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

    pub(super) fn parse_movem_operands(
        &self,
        exprs: &[Expr],
    ) -> Result<Vec<FamilyOperand>, FamilyParseError> {
        let [left, right] = exprs else {
            return Err(FamilyParseError::new(
                "MOVEM expects two operands",
                exprs.first().map(span_from_expr).unwrap_or_default(),
            ));
        };

        let left_candidate = Self::register_list_candidate(left, RegisterListFamily::Integer);
        let right_candidate = Self::register_list_candidate(right, RegisterListFamily::Integer);
        let mut deferred_error = None;

        if left_candidate {
            match (
                self.parse_register_list(left, RegisterListFamily::Integer, "MOVEM"),
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
                self.parse_register_list(right, RegisterListFamily::Integer, "MOVEM"),
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

    pub(super) fn parse_fmovem_operands(
        &self,
        exprs: &[Expr],
    ) -> Result<Vec<FamilyOperand>, FamilyParseError> {
        let [left, right] = exprs else {
            return Err(FamilyParseError::new(
                "FMOVEM expects two operands",
                exprs.first().map(span_from_expr).unwrap_or_default(),
            ));
        };

        let left_candidate = Self::register_list_candidate(left, RegisterListFamily::Fpu);
        let right_candidate = Self::register_list_candidate(right, RegisterListFamily::Fpu);
        let mut deferred_error = None;

        if left_candidate {
            match (
                self.parse_register_list(left, RegisterListFamily::Fpu, "FMOVEM"),
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
                self.parse_register_list(right, RegisterListFamily::Fpu, "FMOVEM"),
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

    pub(super) fn combined_unary_span(
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

    pub(super) fn parse_indirect_tuple(
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

    pub(super) fn parse_single_operand_with_priority(
        &self,
        expr: &Expr,
        fpu_registers_first: bool,
    ) -> Result<FamilyOperand, FamilyParseError> {
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

        if let Expr::Binary {
            op,
            left,
            right,
            span,
        } = expr
        {
            match op {
                BinaryOp::Divide => {
                    let pair_like = (Self::parse_general_register(left).is_some()
                        && Self::parse_general_register(right).is_some())
                        || (Self::parse_fpu_data_register(left).is_some()
                            && Self::parse_fpu_data_register(right).is_some())
                        || matches!(
                            (left.as_ref(), right.as_ref()),
                            (Expr::Indirect(_, _), Expr::Indirect(_, _))
                        );
                    if pair_like {
                        return self.parse_pair_operand(&[*left.clone(), *right.clone()], *span);
                    }
                }
                BinaryOp::Subtract => {
                    let group_like = Self::parse_general_register(left).is_some()
                        && Self::parse_general_register(right).is_some();
                    if group_like {
                        return self.parse_group_operand(left, right, *span);
                    }
                }
                _ => {}
            }
        }

        if let Expr::Range {
            start,
            end,
            step,
            span,
            ..
        } = expr
        {
            if step.is_none() {
                return self.parse_group_operand(start, end, *span);
            }
        }

        if fpu_registers_first {
            if let Some((name, span)) = Self::parse_fpu_data_register(expr) {
                return Ok(FamilyOperand::FpuDataRegister {
                    register: name,
                    span,
                });
            }
            if let Some((register, span)) = Self::parse_fpu_control_register(expr) {
                return Ok(FamilyOperand::FpuControlRegister { register, span });
            }
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
        if !fpu_registers_first {
            if let Some((name, span)) = Self::parse_fpu_data_register(expr) {
                return Ok(FamilyOperand::FpuDataRegister {
                    register: name,
                    span,
                });
            }
            if let Some((register, span)) = Self::parse_fpu_control_register(expr) {
                return Ok(FamilyOperand::FpuControlRegister { register, span });
            }
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
            Expr::Number(text, span) => {
                let size = match parse_number(text) {
                    Some(value) if self.encode_absolute_word(value).is_some() => AbsoluteSize::Word,
                    Some(_) => AbsoluteSize::Long,
                    None => {
                        return Err(FamilyParseError::new(
                            "invalid 68000 absolute numeric operand",
                            *span,
                        ))
                    }
                };
                Ok(FamilyOperand::Absolute {
                    expr: expr.clone(),
                    size,
                    span: *span,
                })
            }
            Expr::Identifier(name, span) => {
                let (base, size) = if let Some(base) = name.strip_suffix(".W") {
                    (base, AbsoluteSize::Word)
                } else if let Some(base) = name.strip_suffix(".L") {
                    (base, AbsoluteSize::Long)
                } else {
                    return Err(FamilyParseError::new(
                        "unsupported Motorola 68000 operand form",
                        *span,
                    ));
                };

                Ok(FamilyOperand::Absolute {
                    expr: Expr::Identifier(base.to_string(), *span),
                    size,
                    span: *span,
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

    pub(super) fn parse_single_operand(
        &self,
        expr: &Expr,
    ) -> Result<FamilyOperand, FamilyParseError> {
        self.parse_single_operand_with_priority(expr, false)
    }

    pub(super) fn parse_fpu_operand(&self, expr: &Expr) -> Result<FamilyOperand, FamilyParseError> {
        self.parse_single_operand_with_priority(expr, true)
    }
}
