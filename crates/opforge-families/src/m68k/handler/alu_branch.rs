// SPDX-License-Identifier: GPL-3.0-or-later

//! Arithmetic, immediate, unary, quick, shift, condition-code, and baseline branch encoders extracted from the M68k family handler.

use super::*;

impl M68KFamilyHandler {
    pub(super) fn encode_data_register_binary_op(
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

    pub(super) fn encode_cmp(
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

        if let Some(src_b) = Self::b_register_direct_operand(src) {
            if !matches!(size, OperationSize::Long) {
                return EncodeResult::error_with_span(
                    "CMP B-register source requires .L size on m68080",
                    src.span(),
                );
            }

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

            let mut bytes = Vec::new();
            Self::emit_word(
                &mut bytes,
                0xC180 | ((dst_reg as u16) << 9) | u16::from(src_b),
            );
            return EncodeResult::ok(bytes);
        }

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

    pub(super) fn encode_address_register_binary_op(
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

    pub(super) fn encode_eor(
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

    pub(super) fn encode_word_data_register_math(
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

    pub(super) fn encode_chk(
        &self,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        match size {
            None | Some(OperationSize::Word) => {}
            Some(OperationSize::Long) => {
                return EncodeResult::NotFound;
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

    pub(super) fn encode_extend_binary_op(
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

    pub(super) fn encode_decimal_adjust_instruction(
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

    pub(super) fn encode_cmpm(
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

    pub(super) fn encode_immediate_binary_op(
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

    pub(crate) fn encode_branch(
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

    pub(super) fn encode_dbcc(
        &self,
        mnemonic: &str,
        condition: ConditionCode,
        size: Option<OperationSize>,
        operands: &[Operand],
        ctx: &dyn AssemblerContext,
    ) -> EncodeResult<Vec<u8>> {
        let is_68080 = ctx.cpu_state_flag(state::CPU_IS_68080_KEY).unwrap_or(0) != 0;
        let long_counter = match size {
            None => false,
            Some(OperationSize::Long) if is_68080 => true,
            Some(_) => {
                return EncodeResult::error(format!("{mnemonic} does not accept a size suffix"));
            }
        };

        if matches!(size, Some(OperationSize::Long)) && !long_counter {
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

        let unresolved = Self::expr_is_unresolved(expr, ctx);
        let offset = if unresolved {
            if long_counter {
                1
            } else {
                0
            }
        } else {
            let target_value = match ctx.eval_expr(expr) {
                Ok(value) => Self::normalize_wrapped_i32(value),
                Err(err) => {
                    return EncodeResult::error_with_span(err, target.span());
                }
            };
            if long_counter {
                target_value - ctx.current_address() as i64
            } else {
                target_value - (ctx.current_address() as i64 + 2)
            }
        };
        if long_counter && !unresolved && (offset & 1) != 0 {
            return EncodeResult::error_with_span(
                format!(
                    "{mnemonic} branch displacement must be even before applying the long-counter signal"
                ),
                target.span(),
            );
        }

        let encoded_offset = if long_counter { offset | 1 } else { offset };

        let Some(encoded_displacement) = Self::encode_signed_word(encoded_offset) else {
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

    pub(super) fn encode_scc(
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

    pub(super) fn encode_moveq(
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

    pub(super) fn encode_quick(
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

        if let Some(dst_b) = Self::b_register_direct_operand(dst) {
            if !matches!(size, OperationSize::Long) {
                return EncodeResult::error_with_span(
                    format!("{mnemonic} B-register destination requires .L size on m68080"),
                    dst.span(),
                );
            }

            let mut bytes = Vec::new();
            let opcode_base = if subtract { 0x5108 } else { 0x5008 };
            Self::emit_word(
                &mut bytes,
                opcode_base | (data_bits << 9) | u16::from(dst_b),
            );
            return EncodeResult::ok(bytes);
        }

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

    pub(super) fn encode_bit_op(
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

    pub(super) fn encode_shift(
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

    pub(super) fn encode_unary_data_instruction(
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

    pub(super) fn encode_unsized_data_ea_instruction(
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
}
