// SPDX-License-Identifier: GPL-3.0-or-later

//! Effective-address encoding helpers extracted from the M68k family handler.

use super::{
    AbsoluteSize, AssemblerContext, EncodeResult, Expr, FullExtensionBase, FullExtensionIndex,
    IndexScale, IndexSize, M68KFamilyHandler, MemoryIndirectionKind, Operand, OperationSize,
};

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

impl M68KFamilyHandler {
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
            Operand::FpuDataRegister { .. } => Err(EncodeResult::error_with_span(
                "FPU data registers are not valid effective addresses",
                operand.span(),
            )),
            Operand::FpuControlRegister { .. } => Err(EncodeResult::error_with_span(
                "FPU control registers are not valid effective addresses",
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
                    let Some(encoded) = self.encode_absolute_word(value) else {
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
                    let Some(encoded) = self.encode_absolute_long(value) else {
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
            Operand::RegisterGroup { .. } => Err(EncodeResult::error_with_span(
                "register groups are not standalone effective addresses",
                operand.span(),
            )),
            Operand::TextureOperand { .. } => Err(EncodeResult::error_with_span(
                "TEX texture operands are not standalone effective addresses",
                operand.span(),
            )),
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

    pub(super) fn effective_address_kind(operand: &Operand) -> EffectiveAddressKind {
        match operand {
            Operand::DataRegister { .. } => EffectiveAddressKind::DataRegister,
            Operand::AddressRegister { .. } => EffectiveAddressKind::AddressRegister,
            Operand::SpecialRegister { .. } => {
                unreachable!("68000 special registers are not effective addresses")
            }
            Operand::ControlRegister { .. } => {
                unreachable!("68000 control registers are not effective addresses")
            }
            Operand::FpuDataRegister { .. } => {
                unreachable!("FPU data registers are not effective addresses")
            }
            Operand::FpuControlRegister { .. } => {
                unreachable!("FPU control registers are not effective addresses")
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
            Operand::RegisterGroup { .. } => {
                unreachable!("68080 register groups are not effective addresses")
            }
            Operand::TextureOperand { .. } => {
                unreachable!("TEX texture operands are not effective addresses")
            }
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

    pub(super) fn effective_address_bits(bits: u16) -> u16 {
        bits & 0x3F
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
}
