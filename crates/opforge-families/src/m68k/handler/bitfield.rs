// SPDX-License-Identifier: GPL-3.0-or-later

//! Bitfield encoders extracted from the M68k family handler.

use super::*;

impl M68KFamilyHandler {
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
}
