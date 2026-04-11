// SPDX-License-Identifier: GPL-3.0-or-later

//! Move, transfer, and control-flow encoders extracted from the M68k family handler.

use super::*;

impl M68KFamilyHandler {
    pub(crate) fn encode_move_from_ccr_instruction(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let [src, dst] = operands else {
            return EncodeResult::error("MOVE expects two operands");
        };
        if !matches!(
            src,
            Operand::SpecialRegister {
                register: SpecialRegisterKind::Ccr,
                ..
            }
        ) {
            return EncodeResult::NotFound;
        }

        match size {
            None | Some(OperationSize::Word) => {}
            Some(OperationSize::Byte) => {
                return EncodeResult::error("MOVE from CCR does not support .B size");
            }
            Some(OperationSize::Long) => {
                return EncodeResult::error("MOVE from CCR does not support .L size");
            }
        }

        let dst_ea = match self.encode_effective_address(dst, Some(OperationSize::Word), ctx) {
            Ok(ea) => ea,
            Err(err) => return err,
        };
        if !Self::data_alterable(dst_ea.kind) {
            return EncodeResult::error_with_span(
                "invalid destination effective address for MOVE from CCR",
                dst.span(),
            );
        }

        let mut bytes = Vec::new();
        Self::emit_word(
            &mut bytes,
            0x42C0 | Self::effective_address_bits(dst_ea.bits),
        );
        bytes.extend_from_slice(&dst_ea.extension);
        EncodeResult::ok(bytes)
    }

    pub(super) fn encode_move(
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

        let src_b = Self::b_register_direct_operand(src);
        let dst_b = Self::b_register_direct_operand(dst);
        if src_b.is_some() || dst_b.is_some() {
            if !matches!(size, OperationSize::Long) {
                return EncodeResult::error_with_span(
                    "B-register MOVE forms require .L size on m68080",
                    if dst_b.is_some() {
                        dst.span()
                    } else {
                        src.span()
                    },
                );
            }
            if src_b.is_some() && dst_b.is_some() {
                return EncodeResult::error_with_span(
                    "MOVE does not support Bn-to-Bn transfers on m68080",
                    dst.span(),
                );
            }
            if let Some(dst_b) = dst_b {
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

                let mut bytes = Vec::new();
                Self::emit_word(
                    &mut bytes,
                    0x1040 | ((dst_b as u16) << 9) | Self::effective_address_bits(src_ea.bits),
                );
                bytes.extend_from_slice(&src_ea.extension);
                return EncodeResult::ok(bytes);
            }

            let src_b = src_b.expect("checked above");
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

            let mut bytes = Vec::new();
            Self::emit_word(
                &mut bytes,
                0x1000
                    | Self::move_destination_bits(dst_ea.bits)
                    | ((0b001_u16 << 3) | u16::from(src_b)),
            );
            bytes.extend_from_slice(&dst_ea.extension);
            return EncodeResult::ok(bytes);
        }

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
                if let Some(result) = Self::encode_move_sr_banked_destination(dst, ctx) {
                    return result;
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

    pub(super) fn encode_movep(
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

    pub(super) fn encode_movea(
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

        if let Some(dst_b) = Self::b_register_direct_operand(dst) {
            if !matches!(size, OperationSize::Long) {
                return EncodeResult::error_with_span(
                    "MOVEA B-register destination requires .L size on m68080",
                    dst.span(),
                );
            }
            if Self::b_register_direct_operand(src).is_some() {
                return EncodeResult::error_with_span(
                    "MOVEA.L Bn,Bm is not supported on m68080",
                    src.span(),
                );
            }

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

            let mut bytes = Vec::new();
            Self::emit_word(
                &mut bytes,
                0x1040 | ((dst_b as u16) << 9) | Self::effective_address_bits(src_ea.bits),
            );
            bytes.extend_from_slice(&src_ea.extension);
            return EncodeResult::ok(bytes);
        }

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

    pub(super) fn encode_lea(
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

        let src_b_indirect = Self::b_register_indirect_operand(src);
        let dst_b_direct = Self::b_register_direct_operand(dst);
        if src_b_indirect.is_some() || dst_b_direct.is_some() {
            if src_b_indirect.is_some() && dst_b_direct.is_some() {
                return EncodeResult::error_with_span(
                    "LEA (Bn),Bm is not supported on m68080",
                    dst.span(),
                );
            }

            if let Some(dst_b) = dst_b_direct {
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
                    0x4140 | ((dst_b as u16) << 9) | Self::effective_address_bits(src_ea.bits),
                );
                bytes.extend_from_slice(&src_ea.extension);
                return EncodeResult::ok(bytes);
            }

            let src_b = src_b_indirect.expect("checked above");
            let dst_register = match dst {
                Operand::AddressRegister { register, .. }
                    if Self::b_register_number(register).is_none() =>
                {
                    register
                }
                _ => {
                    return EncodeResult::error_with_span(
                        "LEA (Bn) destination must be A0-A7 or SP",
                        dst.span(),
                    )
                }
            };
            let Some(dst_reg) = Self::address_register_number(dst_register) else {
                return EncodeResult::error_with_span(
                    "invalid LEA destination register",
                    dst.span(),
                );
            };

            let mut bytes = Vec::new();
            Self::emit_word(
                &mut bytes,
                0x41C8 | ((dst_reg as u16) << 9) | u16::from(src_b),
            );
            return EncodeResult::ok(bytes);
        }

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

    pub(super) fn encode_pea(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.encode_single_ea_control_instruction(size, operands, ctx, "PEA", 0x4840)
    }

    pub(super) fn encode_jmp(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        self.encode_single_ea_control_instruction(size, operands, ctx, "JMP", 0x4EC0)
    }

    pub(super) fn encode_jsr(
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

    pub(super) fn encode_fixed_instruction(
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

    pub(super) fn encode_link(
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

    pub(super) fn encode_unlk(
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

    pub(super) fn encode_swap(
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

    pub(super) fn encode_exg(
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

    pub(super) fn encode_ext(
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

    pub(super) fn encode_trap(
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

    pub(super) fn encode_stop(
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

    pub(crate) fn move_allows_source(kind: EffectiveAddressKind, size: OperationSize) -> bool {
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

    pub(crate) fn move_allows_destination(kind: EffectiveAddressKind) -> bool {
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

    pub(super) fn movea_allows_source(kind: EffectiveAddressKind) -> bool {
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

    fn e_register_descriptor(name: &str) -> Option<(u16, u16)> {
        let upper = name.to_ascii_uppercase();
        let suffix = upper.strip_prefix('E')?;
        let reg = suffix.parse::<u8>().ok()?;
        if reg > 23 {
            return None;
        }
        Some((u16::from(reg / 8 + 1), u16::from(reg % 8)))
    }

    fn move_sr_bank_prefix_size_bits(body_len: usize) -> Option<u16> {
        match body_len {
            2 => Some(0),
            4 => Some(1),
            6 => Some(2),
            8 => Some(3),
            _ => None,
        }
    }

    fn encode_move_sr_banked_destination(
        dst: &Operand,
        ctx: &dyn AssemblerContext,
    ) -> Option<EncodeResult<Vec<u8>>> {
        if ctx.cpu_state_flag(state::CPU_IS_68080_KEY).unwrap_or(0) == 0 {
            return None;
        }

        let Operand::DataRegister { register, .. } = dst else {
            return None;
        };
        let (bank_bits, reg_bits) = Self::e_register_descriptor(register)?;

        let mut body = Vec::new();
        Self::emit_word(&mut body, 0x40C0 | reg_bits);
        let Some(size_bits) = Self::move_sr_bank_prefix_size_bits(body.len()) else {
            return Some(EncodeResult::error(
                "generated BANK prefix requires a 2, 4, 6, or 8 byte base instruction on m68080",
            ));
        };

        let prefix_word =
            0x7100 | ((size_bits & 0x3) << 6) | ((bank_bits & 0x3) << 2) | (bank_bits & 0x3);
        let mut bytes = Vec::with_capacity(body.len() + 2);
        Self::emit_word(&mut bytes, prefix_word);
        bytes.extend_from_slice(&body);
        Some(EncodeResult::ok(bytes))
    }

    fn move_destination_bits(bits: u16) -> u16 {
        let mode = (bits >> 3) & 0b111;
        let reg = bits & 0b111;
        (reg << 9) | (mode << 6)
    }
}
