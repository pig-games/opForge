// SPDX-License-Identifier: GPL-3.0-or-later

//! MOVEM and MOVES helpers extracted from the M68k family handler.

use super::*;

impl M68KFamilyHandler {
    pub(super) fn encode_movem(
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
}
