// SPDX-License-Identifier: GPL-3.0-or-later

//! Later-family M68k helpers extracted from the family handler.

use super::*;

impl M68KFamilyHandler {
    pub(crate) fn encode_bkpt_instruction(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("BKPT does not support size suffixes");
        }

        let [vector] = operands else {
            return EncodeResult::error("BKPT expects one immediate vector operand");
        };
        let Operand::Immediate { expr, .. } = vector else {
            return EncodeResult::error_with_span(
                "BKPT operand must be an immediate vector",
                vector.span(),
            );
        };

        let value = match Self::eval_expr(expr, ctx) {
            Ok(value) => value,
            Err(err) => return EncodeResult::error_with_span(err, vector.span()),
        };
        if !(0..=7).contains(&value) {
            return EncodeResult::error_with_span("BKPT vector out of range (0-7)", vector.span());
        }

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x4848 | value as u16);
        EncodeResult::ok(bytes)
    }

    pub(crate) fn encode_rtd_instruction(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        if size.is_some() {
            return EncodeResult::error("RTD does not support size suffixes");
        }

        let [displacement] = operands else {
            return EncodeResult::error("RTD expects one immediate displacement operand");
        };
        let Operand::Immediate { expr, .. } = displacement else {
            return EncodeResult::error_with_span(
                "RTD operand must be an immediate displacement",
                displacement.span(),
            );
        };

        let value = match Self::eval_expr(expr, ctx) {
            Ok(value) => value,
            Err(err) => return EncodeResult::error_with_span(err, displacement.span()),
        };
        let Some(encoded) = Self::encode_signed_word(value) else {
            return EncodeResult::error_with_span(
                "RTD displacement out of 16-bit signed range",
                displacement.span(),
            );
        };

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x4E74);
        Self::emit_word(&mut bytes, encoded);
        EncodeResult::ok(bytes)
    }

    pub(super) fn try_encode_m68080_extended_short_branch(
        &self,
        parsed: &ParsedMnemonic,
        mnemonic: &str,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> Option<EncodeResult<Vec<u8>>> {
        if ctx.cpu_state_flag(state::CPU_IS_68080_KEY).unwrap_or(0) == 0
            || !parsed.has_unknown_size_suffix
            || !mnemonic.to_ascii_uppercase().ends_with(".S+")
        {
            return None;
        }

        let condition_bits = match parsed.kind {
            MnemonicKind::Bra => 0x0,
            MnemonicKind::Bsr => 0x1,
            MnemonicKind::Bcc(condition) => condition.opcode_bits(),
            _ => return None,
        };

        Some(self.encode_m68080_extended_short_branch(mnemonic, condition_bits, operands, ctx))
    }

    fn encode_m68080_extended_short_branch(
        &self,
        mnemonic: &str,
        condition_bits: u16,
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

        let unresolved = Self::expr_is_unresolved(expr, ctx);
        let offset = if unresolved {
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

        if !unresolved && (offset & 1) != 0 {
            return EncodeResult::error_with_span(
                format!("{mnemonic} branch displacement must be even on m68080"),
                target.span(),
            );
        }

        let encoded_displacement = if unresolved {
            0x01_u8
        } else {
            match Self::encode_m68080_extended_short_displacement(offset) {
                Some(value) => value,
                None => {
                    return EncodeResult::error_with_span(
                        format!(
                            "{mnemonic} extended-short displacement out of range: offset {offset}"
                        ),
                        target.span(),
                    );
                }
            }
        };

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0x6000 | (condition_bits << 8) | encoded_displacement as u16,
        );
        EncodeResult::ok(bytes)
    }

    fn encode_m68080_extended_short_displacement(offset: i64) -> Option<u8> {
        if (offset & 1) != 0 {
            return None;
        }

        let encoded = if (128..=254).contains(&offset) {
            offset - 127
        } else if (-256..=-132).contains(&offset) {
            offset + 129
        } else {
            return None;
        };

        (-128..=127)
            .contains(&encoded)
            .then_some((encoded as i8) as u8)
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
        let extension =
            ((dst_reg as u16) << 12) | if signed { 1 << 11 } else { 0 } | dst_reg as u16;
        Self::emit_word(&mut bytes, extension);
        bytes.extend_from_slice(&src_ea.extension);
        EncodeResult::ok(bytes)
    }

    pub(crate) fn encode_long_data_register_divide(
        &self,
        mnemonic: &str,
        signed: bool,
        register_pair_uses_quadword_dividend: bool,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error(format!("{mnemonic} expects two operands"));
        };

        let (pair_dividend, remainder_reg, quotient_reg) = match dst {
            Operand::DataRegister { register, .. } => {
                let Some(reg) = Self::data_register_number(register) else {
                    return EncodeResult::error_with_span(
                        format!("invalid {mnemonic} destination register"),
                        dst.span(),
                    );
                };
                (false, reg, reg)
            }
            Operand::RegisterPair { left, right, .. } => {
                let (Some(remainder_reg), Some(quotient_reg)) = (
                    Self::data_register_number(left),
                    Self::data_register_number(right),
                ) else {
                    return EncodeResult::error_with_span(
                        format!(
                            "{mnemonic} destination must be a data register or data-register pair"
                        ),
                        dst.span(),
                    );
                };
                if remainder_reg == quotient_reg {
                    return EncodeResult::error_with_span(
                        format!("{mnemonic} register-pair destination requires distinct remainder and quotient registers"),
                        dst.span(),
                    );
                }
                (
                    register_pair_uses_quadword_dividend,
                    remainder_reg,
                    quotient_reg,
                )
            }
            _ => {
                return EncodeResult::error_with_span(
                    format!("{mnemonic} destination must be a data register or data-register pair"),
                    dst.span(),
                );
            }
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
            0x4C40 | Self::effective_address_bits(src_ea.bits),
        );
        let extension = ((quotient_reg as u16) << 12)
            | if signed { 1 << 11 } else { 0 }
            | if pair_dividend { 1 << 10 } else { 0 }
            | remainder_reg as u16;
        Self::emit_word(&mut bytes, extension);
        bytes.extend_from_slice(&src_ea.extension);
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
        let (Some(memory_left_bits), Some(memory_right_bits)) = (
            Self::address_register_number(memory_left),
            Self::address_register_number(memory_right),
        ) else {
            return EncodeResult::error_with_span(
                "CAS2 memory operand must use (An):(Am) address-register pair syntax",
                memory_pair.span(),
            );
        };

        let mut bytes = Vec::new();
        Self::emit_word(&mut bytes, 0x08FC | (size_bits << 9));
        Self::emit_word(
            &mut bytes,
            (1_u16 << 15)
                | ((memory_left_bits as u16) << 12)
                | ((update_left_bits as u16) << 6)
                | compare_left_bits as u16,
        );
        Self::emit_word(
            &mut bytes,
            (1_u16 << 15)
                | ((memory_right_bits as u16) << 12)
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
}
