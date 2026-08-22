// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use opcore::parser::Expr;
use registry::family::{AssemblerContext, CpuHandler, FamilyHandler};
use registry::registry::VmEncodeCandidate;

use super::handler::resolve_operands as resolve_intel8080_operands;
use super::table::{lookup_instruction, ArgType, InstructionEntry, Prefix};
use super::{FamilyOperand, Intel8080FamilyHandler, Operand};
use crate::i8085::{lookup_extension as lookup_i8085_extension, I8085CpuHandler};
use crate::z80::{lookup_extension as lookup_z80_extension, Z80CpuHandler};

pub fn vm_encode_candidates_from_exprs(
    cpu_id: &str,
    mnemonic: &str,
    exprs: &[Expr],
    ctx: &dyn AssemblerContext,
) -> Result<Option<Vec<VmEncodeCandidate>>, String> {
    let family = Intel8080FamilyHandler;
    let parsed = match family.parse_operands(mnemonic, exprs) {
        Ok(parsed) => parsed,
        Err(_) => return Ok(None),
    };

    for resolved_operands in resolved_operand_candidates(cpu_id, mnemonic, parsed.as_slice(), ctx)?
    {
        if let Some(candidate) =
            candidate_from_resolved_operands(mnemonic, cpu_id, resolved_operands.as_slice(), ctx)
        {
            return Ok(Some(vec![candidate]));
        }
    }

    Ok(None)
}

pub fn candidate_from_resolved_operands(
    mnemonic: &str,
    cpu_id: &str,
    operands: &[Operand],
    ctx: &dyn AssemblerContext,
) -> Option<VmEncodeCandidate> {
    let normalized_operands;
    let operands = if let Some(stripped) = strip_redundant_condition_operand(mnemonic, operands) {
        normalized_operands = stripped;
        normalized_operands.as_slice()
    } else {
        operands
    };

    if let Some(candidate) = ld_indirect_candidate(mnemonic, cpu_id, operands) {
        return Some(candidate);
    }
    if let Some(candidate) = half_index_candidate(mnemonic, cpu_id, operands) {
        return Some(candidate);
    }
    if let Some(candidate) = cb_candidate(mnemonic, cpu_id, operands) {
        return Some(candidate);
    }
    if let Some(candidate) = indexed_memory_candidate(mnemonic, cpu_id, operands) {
        return Some(candidate);
    }
    if let Some(candidate) = indexed_cb_candidate(mnemonic, cpu_id, operands) {
        return Some(candidate);
    }

    let entry = lookup_instruction_entry(mnemonic, cpu_id, operands)?;
    if matches!(entry.arg_type, ArgType::Im) {
        let mode = interrupt_mode_for_entry(entry, operands)?;
        let mode_key = mode_key_for_z80_interrupt_mode(mode)?;
        return Some(VmEncodeCandidate {
            mode_key,
            operand_bytes: Vec::new(),
        });
    }
    let operand_bytes = operand_bytes_for_entry(entry, operands, ctx)?;
    Some(VmEncodeCandidate {
        mode_key: mode_key_for_instruction_entry(entry),
        operand_bytes,
    })
}

pub fn ld_indirect_candidate(
    mnemonic: &str,
    cpu_id: &str,
    operands: &[Operand],
) -> Option<VmEncodeCandidate> {
    if !cpu_id.eq_ignore_ascii_case("z80")
        || !mnemonic.eq_ignore_ascii_case("ld")
        || operands.len() != 2
    {
        return None;
    }

    let (mode_key, addr) = match (&operands[0], &operands[1]) {
        (Operand::Register(dst, _), Operand::IndirectAddress16(addr, _)) => {
            (mode_key_for_z80_ld_indirect(dst.as_str(), false)?, *addr)
        }
        (Operand::IndirectAddress16(addr, _), Operand::Register(src, _)) => {
            (mode_key_for_z80_ld_indirect(src.as_str(), true)?, *addr)
        }
        _ => return None,
    };

    Some(VmEncodeCandidate {
        mode_key,
        operand_bytes: vec![vec![addr as u8, (addr >> 8) as u8]],
    })
}

fn resolved_operand_candidates(
    cpu_id: &str,
    mnemonic: &str,
    parsed: &[FamilyOperand],
    ctx: &dyn AssemblerContext,
) -> Result<Vec<Vec<Operand>>, String> {
    let mut resolved_candidates = Vec::new();

    if cpu_id.eq_ignore_ascii_case("z80") {
        if let Ok(ops) = Z80CpuHandler::new().resolve_operands(mnemonic, parsed, ctx) {
            resolved_candidates.push(ops);
        }
        if let Ok(ops) =
            resolve_intel8080_operands(mnemonic, parsed, ctx).map_err(|err| err.message)
        {
            resolved_candidates.push(ops);
        }
    } else if cpu_id.eq_ignore_ascii_case("8085") {
        resolved_candidates.push(I8085CpuHandler::new().resolve_operands(mnemonic, parsed, ctx)?);
    } else {
        resolved_candidates
            .push(resolve_intel8080_operands(mnemonic, parsed, ctx).map_err(|err| err.message)?);
    }

    Ok(resolved_candidates)
}

fn lookup_instruction_entry(
    mnemonic: &str,
    cpu_id: &str,
    operands: &[Operand],
) -> Option<&'static InstructionEntry> {
    let reg1 = operands.first().and_then(lookup_key);
    let reg2 = operands.get(1).and_then(lookup_key);

    if let Some(entry) = lookup_instruction(mnemonic, reg1.as_deref(), reg2.as_deref()) {
        return Some(entry);
    }
    if cpu_id.eq_ignore_ascii_case("8085") {
        return lookup_i8085_extension(mnemonic, reg1.as_deref(), reg2.as_deref());
    }
    if cpu_id.eq_ignore_ascii_case("z80") {
        return lookup_z80_extension(mnemonic, reg1.as_deref(), reg2.as_deref());
    }
    None
}

fn strip_redundant_condition_operand(mnemonic: &str, operands: &[Operand]) -> Option<Vec<Operand>> {
    let suffix = condition_suffix_for_mnemonic(mnemonic)?;
    let first = operands.first()?;
    let condition = match first {
        Operand::Condition(name, _) | Operand::Register(name, _) => name.as_str(),
        _ => return None,
    };
    if !condition.eq_ignore_ascii_case(suffix) {
        return None;
    }
    Some(operands[1..].to_vec())
}

fn condition_suffix_for_mnemonic(mnemonic: &str) -> Option<&'static str> {
    match mnemonic.to_ascii_uppercase().as_str() {
        "JNZ" | "CNZ" | "RNZ" => Some("NZ"),
        "JZ" | "CZ" | "RZ" => Some("Z"),
        "JNC" | "CNC" | "RNC" => Some("NC"),
        "JC" | "CC" | "RC" => Some("C"),
        "JPO" | "CPO" | "RPO" => Some("PO"),
        "JPE" | "CPE" | "RPE" => Some("PE"),
        "JP" | "CP" | "RP" => Some("P"),
        "JM" | "CM" | "RM" => Some("M"),
        _ => None,
    }
}

fn lookup_key(operand: &Operand) -> Option<String> {
    match operand {
        Operand::Register(name, _) => Some(name.to_string()),
        Operand::Indirect(name, _) if name.eq_ignore_ascii_case("hl") => Some("M".to_string()),
        Operand::Indirect(name, _) => Some(name.to_string()),
        Operand::Indexed { base, offset, .. } if *offset == 0 => Some(base.to_string()),
        Operand::Condition(name, _) => Some(name.to_string()),
        Operand::RstVector(value, _)
        | Operand::InterruptMode(value, _)
        | Operand::BitNumber(value, _) => Some(value.to_string()),
        _ => None,
    }
}

fn operand_bytes_for_entry(
    entry: &InstructionEntry,
    operands: &[Operand],
    ctx: &dyn AssemblerContext,
) -> Option<Vec<Vec<u8>>> {
    let imm_index = entry.num_regs as usize;
    match entry.arg_type {
        ArgType::None => Some(Vec::new()),
        ArgType::Byte => {
            let value = match operands.get(imm_index)? {
                Operand::Immediate8(value, _)
                | Operand::Port(value, _)
                | Operand::RstVector(value, _)
                | Operand::InterruptMode(value, _)
                | Operand::BitNumber(value, _) => *value,
                _ => return None,
            };
            Some(vec![vec![value]])
        }
        ArgType::Word => {
            let value = match operands.get(imm_index)? {
                Operand::Immediate16(value, _) | Operand::IndirectAddress16(value, _) => *value,
                _ => return None,
            };
            Some(vec![vec![value as u8, (value >> 8) as u8]])
        }
        ArgType::Relative => {
            let value = match operands.get(imm_index)? {
                Operand::Immediate8(value, _) => *value,
                Operand::Immediate16(target, _) => {
                    let next_pc =
                        ctx.current_address() as i64 + prefix_len(entry.prefix) as i64 + 2;
                    let delta = *target as i64 - next_pc;
                    if !(-128..=127).contains(&delta) {
                        return None;
                    }
                    delta as i8 as u8
                }
                _ => return None,
            };
            Some(vec![vec![value]])
        }
        ArgType::Im => None,
    }
}

fn interrupt_mode_for_entry(entry: &InstructionEntry, operands: &[Operand]) -> Option<u8> {
    if !matches!(entry.arg_type, ArgType::Im) {
        return None;
    }
    let imm_index = entry.num_regs as usize;
    let mode = match operands.get(imm_index)? {
        Operand::InterruptMode(value, _) | Operand::Immediate8(value, _) => *value,
        Operand::Immediate16(value, _) => (*value).try_into().ok()?,
        _ => return None,
    };
    if mode <= 2 {
        Some(mode)
    } else {
        None
    }
}

fn half_index_candidate(
    mnemonic: &str,
    cpu_id: &str,
    operands: &[Operand],
) -> Option<VmEncodeCandidate> {
    if !cpu_id.eq_ignore_ascii_case("z80") {
        return None;
    }

    let mut prefix: Option<&str> = None;
    for operand in operands {
        let Operand::Register(name, _) = operand else {
            continue;
        };
        let Some((current_prefix, _)) = half_index_parts(name) else {
            continue;
        };
        match prefix {
            None => prefix = Some(current_prefix),
            Some(existing) if existing.eq_ignore_ascii_case(current_prefix) => {}
            Some(_) => return None,
        }
    }
    let prefix = prefix?;
    let upper = mnemonic.to_ascii_uppercase();

    let (_opcode, operand_bytes, form) = match upper.as_str() {
        "LD" => {
            if operands.len() != 2 {
                return None;
            }
            match (&operands[0], &operands[1]) {
                (Operand::Register(dst, _), Operand::Register(src, _)) => {
                    let dst_code = half_index_reg_code(prefix, dst)?;
                    let src_code = half_index_reg_code(prefix, src)?;
                    (
                        0x40 | (dst_code << 3) | src_code,
                        Vec::new(),
                        format!("rr:{dst_code}:{src_code}"),
                    )
                }
                (Operand::Register(dst, _), Operand::Immediate8(value, _)) => {
                    let (dst_prefix, dst_code) = half_index_parts(dst)?;
                    if !dst_prefix.eq_ignore_ascii_case(prefix) {
                        return None;
                    }
                    (
                        0x06 | (dst_code << 3),
                        vec![vec![*value]],
                        format!("ri:{dst_code}"),
                    )
                }
                (Operand::Register(dst, _), Operand::Immediate16(value, _)) if *value <= 0xFF => {
                    let (dst_prefix, dst_code) = half_index_parts(dst)?;
                    if !dst_prefix.eq_ignore_ascii_case(prefix) {
                        return None;
                    }
                    (
                        0x06 | (dst_code << 3),
                        vec![vec![*value as u8]],
                        format!("ri:{dst_code}"),
                    )
                }
                _ => return None,
            }
        }
        "INC" | "DEC" => {
            if operands.len() != 1 {
                return None;
            }
            let code = match &operands[0] {
                Operand::Register(name, _) => {
                    let (reg_prefix, reg_code) = half_index_parts(name)?;
                    if !reg_prefix.eq_ignore_ascii_case(prefix) {
                        return None;
                    }
                    reg_code
                }
                _ => return None,
            };
            let base = if upper == "INC" { 0x04 } else { 0x05 };
            (base | (code << 3), Vec::new(), format!("r:{code}"))
        }
        "ADD" | "ADC" | "SBC" => {
            if operands.len() != 2 || !is_register_a(&operands[0]) {
                return None;
            }
            let code = match &operands[1] {
                Operand::Register(name, _) => {
                    let (reg_prefix, reg_code) = half_index_parts(name)?;
                    if !reg_prefix.eq_ignore_ascii_case(prefix) {
                        return None;
                    }
                    reg_code
                }
                _ => return None,
            };
            let base = match upper.as_str() {
                "ADD" => 0x80,
                "ADC" => 0x88,
                "SBC" => 0x98,
                _ => return None,
            };
            (base | code, Vec::new(), format!("r:{code}"))
        }
        "SUB" | "AND" | "XOR" | "OR" | "CP" => {
            let src = match operands {
                [src] => src,
                [dst, src] if is_register_a(dst) => src,
                _ => return None,
            };
            let code = match src {
                Operand::Register(name, _) => {
                    let (reg_prefix, reg_code) = half_index_parts(name)?;
                    if !reg_prefix.eq_ignore_ascii_case(prefix) {
                        return None;
                    }
                    reg_code
                }
                _ => return None,
            };
            let base = match upper.as_str() {
                "SUB" => 0x90,
                "AND" => 0xA0,
                "XOR" => 0xA8,
                "OR" => 0xB0,
                "CP" => 0xB8,
                _ => return None,
            };
            (base | code, Vec::new(), format!("r:{code}"))
        }
        _ => return None,
    };

    let mode_key = mode_key_for_z80_half_index(prefix, mnemonic, form.as_str())?;
    Some(VmEncodeCandidate {
        mode_key,
        operand_bytes,
    })
}

fn half_index_parts(name: &str) -> Option<(&'static str, u8)> {
    match name.to_ascii_uppercase().as_str() {
        "IXH" => Some(("IX", 4)),
        "IXL" => Some(("IX", 5)),
        "IYH" => Some(("IY", 4)),
        "IYL" => Some(("IY", 5)),
        _ => None,
    }
}

fn half_index_reg_code(prefix: &str, name: &str) -> Option<u8> {
    match name.to_ascii_uppercase().as_str() {
        "B" => Some(0),
        "C" => Some(1),
        "D" => Some(2),
        "E" => Some(3),
        "A" => Some(7),
        _ => {
            let (reg_prefix, reg_code) = half_index_parts(name)?;
            if reg_prefix.eq_ignore_ascii_case(prefix) {
                Some(reg_code)
            } else {
                None
            }
        }
    }
}

fn cb_candidate(mnemonic: &str, cpu_id: &str, operands: &[Operand]) -> Option<VmEncodeCandidate> {
    if !cpu_id.eq_ignore_ascii_case("z80") {
        return None;
    }

    let upper = mnemonic.to_ascii_uppercase();
    if matches!(
        upper.as_str(),
        "RLC" | "RRC" | "RL" | "RR" | "SLA" | "SRA" | "SLL" | "SRL"
    ) {
        if operands.len() != 1 {
            return None;
        }
        let register = match &operands[0] {
            Operand::Register(name, _) => name.as_str(),
            Operand::Indirect(name, _) if name.eq_ignore_ascii_case("hl") => "M",
            _ => return None,
        };
        let mode_key = mode_key_for_z80_cb_register(mnemonic, None, register)?;
        return Some(VmEncodeCandidate {
            mode_key,
            operand_bytes: Vec::new(),
        });
    }

    if matches!(upper.as_str(), "BIT" | "RES" | "SET") {
        if operands.len() != 2 {
            return None;
        }
        let bit = match &operands[0] {
            Operand::BitNumber(value, _)
            | Operand::Immediate8(value, _)
            | Operand::InterruptMode(value, _) => *value,
            Operand::Immediate16(value, _) if *value <= 7 => *value as u8,
            _ => return None,
        };
        let register = match &operands[1] {
            Operand::Register(name, _) => name.as_str(),
            Operand::Indirect(name, _) if name.eq_ignore_ascii_case("hl") => "M",
            _ => return None,
        };
        let mode_key = mode_key_for_z80_cb_register(mnemonic, Some(bit), register)?;
        return Some(VmEncodeCandidate {
            mode_key,
            operand_bytes: Vec::new(),
        });
    }

    None
}

fn indexed_memory_candidate(
    mnemonic: &str,
    cpu_id: &str,
    operands: &[Operand],
) -> Option<VmEncodeCandidate> {
    if !cpu_id.eq_ignore_ascii_case("z80") {
        return None;
    }

    let upper = mnemonic.to_ascii_uppercase();
    let (base, offset, opcode_form, extra_operand) = match upper.as_str() {
        "LD" => match operands {
            [Operand::Register(dst, _), Operand::Indexed { base, offset, .. }] => (
                base.as_str(),
                *offset,
                format!("ld_r_from_idx_{}", dst.to_ascii_lowercase()),
                None,
            ),
            [Operand::Indexed { base, offset, .. }, Operand::Register(src, _)] => (
                base.as_str(),
                *offset,
                format!("ld_idx_from_r_{}", src.to_ascii_lowercase()),
                None,
            ),
            [Operand::Indexed { base, offset, .. }, Operand::Immediate8(value, _)] => (
                base.as_str(),
                *offset,
                "ld_idx_imm".to_string(),
                Some(vec![*value]),
            ),
            [Operand::Indexed { base, offset, .. }, Operand::Immediate16(value, _)]
                if *value <= 0xFF =>
            {
                (
                    base.as_str(),
                    *offset,
                    "ld_idx_imm".to_string(),
                    Some(vec![*value as u8]),
                )
            }
            _ => return None,
        },
        "INC" | "DEC" => {
            let indexed = match operands {
                [Operand::Indexed { base, offset, .. }] => Some((base.as_str(), *offset)),
                _ => None,
            }?;
            (
                indexed.0,
                indexed.1,
                if upper == "INC" {
                    "inc_idx".to_string()
                } else {
                    "dec_idx".to_string()
                },
                None,
            )
        }
        "ADD" | "ADC" | "SUB" | "SBC" | "AND" | "XOR" | "OR" | "CP" => {
            let indexed = match operands {
                [Operand::Indexed { base, offset, .. }] => Some((base.as_str(), *offset)),
                [Operand::Register(dst, _), Operand::Indexed { base, offset, .. }]
                    if dst.eq_ignore_ascii_case("A") =>
                {
                    Some((base.as_str(), *offset))
                }
                _ => None,
            }?;
            (
                indexed.0,
                indexed.1,
                match upper.as_str() {
                    "ADD" => "add_a_idx".to_string(),
                    "ADC" => "adc_a_idx".to_string(),
                    "SUB" => "sub_idx".to_string(),
                    "SBC" => "sbc_a_idx".to_string(),
                    "AND" => "and_idx".to_string(),
                    "XOR" => "xor_idx".to_string(),
                    "OR" => "or_idx".to_string(),
                    "CP" => "cp_idx".to_string(),
                    _ => return None,
                },
                None,
            )
        }
        _ => return None,
    };

    let mode_key = mode_key_for_z80_indexed_memory(base, opcode_form.as_str())?;
    let mut operand_bytes = vec![vec![offset as u8]];
    if let Some(extra) = extra_operand {
        operand_bytes.push(extra);
    }
    Some(VmEncodeCandidate {
        mode_key,
        operand_bytes,
    })
}

fn indexed_cb_candidate(
    mnemonic: &str,
    cpu_id: &str,
    operands: &[Operand],
) -> Option<VmEncodeCandidate> {
    if !cpu_id.eq_ignore_ascii_case("z80") {
        return None;
    }

    let upper = mnemonic.to_ascii_uppercase();
    if matches!(
        upper.as_str(),
        "RLC" | "RRC" | "RL" | "RR" | "SLA" | "SRA" | "SLL" | "SRL"
    ) {
        if operands.len() != 1 {
            return None;
        }
        let (base, offset) = match &operands[0] {
            Operand::Indexed { base, offset, .. } => (base.as_str(), *offset),
            _ => return None,
        };
        let mode_key = mode_key_for_z80_indexed_cb(base, mnemonic, None)?;
        return Some(VmEncodeCandidate {
            mode_key,
            operand_bytes: vec![vec![offset as u8]],
        });
    }

    if matches!(upper.as_str(), "BIT" | "RES" | "SET") {
        if operands.len() != 2 {
            return None;
        }
        let bit = match &operands[0] {
            Operand::BitNumber(value, _)
            | Operand::Immediate8(value, _)
            | Operand::InterruptMode(value, _) => *value,
            Operand::Immediate16(value, _) if *value <= 7 => *value as u8,
            _ => return None,
        };
        let (base, offset) = match &operands[1] {
            Operand::Indexed { base, offset, .. } => (base.as_str(), *offset),
            _ => return None,
        };
        let mode_key = mode_key_for_z80_indexed_cb(base, mnemonic, Some(bit))?;
        return Some(VmEncodeCandidate {
            mode_key,
            operand_bytes: vec![vec![offset as u8]],
        });
    }

    None
}

fn is_register_a(operand: &Operand) -> bool {
    matches!(operand, Operand::Register(name, _) if name.eq_ignore_ascii_case("a"))
}

fn mode_key_for_instruction_entry(entry: &InstructionEntry) -> String {
    format!(
        "p={};n={};r1={};r2={};a={}",
        prefix_key(entry.prefix),
        entry.num_regs,
        reg_key(entry.reg1),
        reg_key(entry.reg2),
        arg_type_key(entry.arg_type),
    )
}

fn mode_key_for_z80_interrupt_mode(mode: u8) -> Option<String> {
    if mode > 2 {
        return None;
    }
    Some(format!("im={mode}"))
}

fn mode_key_for_z80_indexed_cb(base: &str, mnemonic: &str, bit: Option<u8>) -> Option<String> {
    let base_key = indexed_cb_base_key(base)?;
    let upper = mnemonic.to_ascii_uppercase();
    let mnemonic_key = upper.to_ascii_lowercase();
    if matches!(upper.as_str(), "BIT" | "RES" | "SET") {
        let bit = bit?;
        if bit > 7 {
            return None;
        }
        Some(format!("cbidx={base_key}:{mnemonic_key}:{bit}"))
    } else {
        if bit.is_some() {
            return None;
        }
        if !matches!(
            upper.as_str(),
            "RLC" | "RRC" | "RL" | "RR" | "SLA" | "SRA" | "SLL" | "SRL"
        ) {
            return None;
        }
        Some(format!("cbidx={base_key}:{mnemonic_key}"))
    }
}

fn mode_key_for_z80_cb_register(mnemonic: &str, bit: Option<u8>, register: &str) -> Option<String> {
    let reg = z80_cb_register_key(register)?;
    let upper = mnemonic.to_ascii_uppercase();
    let mnemonic_key = upper.to_ascii_lowercase();
    if matches!(upper.as_str(), "BIT" | "RES" | "SET") {
        let bit = bit?;
        if bit > 7 {
            return None;
        }
        Some(format!("cbreg={mnemonic_key}:{bit}:{reg}"))
    } else {
        if bit.is_some() {
            return None;
        }
        if !matches!(
            upper.as_str(),
            "RLC" | "RRC" | "RL" | "RR" | "SLA" | "SRA" | "SLL" | "SRL"
        ) {
            return None;
        }
        Some(format!("cbreg={mnemonic_key}:{reg}"))
    }
}

fn mode_key_for_z80_indexed_memory(base: &str, form: &str) -> Option<String> {
    let base_key = indexed_cb_base_key(base)?;
    Some(format!("idxmem={base_key}:{}", form.to_ascii_lowercase()))
}

fn mode_key_for_z80_ld_indirect(register: &str, store: bool) -> Option<String> {
    let reg = z80_ld_indirect_register_key(register)?;
    let dir = if store { "store" } else { "load" };
    Some(format!("ldind={dir}:{reg}"))
}

fn mode_key_for_z80_half_index(prefix: &str, mnemonic: &str, form: &str) -> Option<String> {
    let prefix_key = z80_half_index_prefix_key(prefix)?;
    Some(format!(
        "halfidx={prefix_key}:{}:{}",
        mnemonic.to_ascii_lowercase(),
        form.to_ascii_lowercase()
    ))
}

fn prefix_len(prefix: Prefix) -> usize {
    match prefix {
        Prefix::None => 0,
        Prefix::Cb | Prefix::Dd | Prefix::Ed | Prefix::Fd => 1,
        Prefix::DdCb | Prefix::FdCb => 2,
    }
}

fn prefix_key(prefix: Prefix) -> &'static str {
    match prefix {
        Prefix::None => "none",
        Prefix::Cb => "cb",
        Prefix::Dd => "dd",
        Prefix::Ed => "ed",
        Prefix::Fd => "fd",
        Prefix::DdCb => "ddcb",
        Prefix::FdCb => "fdcb",
    }
}

fn arg_type_key(arg_type: ArgType) -> &'static str {
    match arg_type {
        ArgType::None => "none",
        ArgType::Byte => "byte",
        ArgType::Word => "word",
        ArgType::Relative => "rel",
        ArgType::Im => "im",
    }
}

fn reg_key(reg: &str) -> String {
    if reg.trim().is_empty() {
        "_".to_string()
    } else {
        reg.to_ascii_lowercase()
    }
}

fn indexed_cb_base_key(base: &str) -> Option<&'static str> {
    match base.to_ascii_uppercase().as_str() {
        "IX" => Some("ix"),
        "IY" => Some("iy"),
        _ => None,
    }
}

fn z80_cb_register_key(register: &str) -> Option<&'static str> {
    match register.to_ascii_uppercase().as_str() {
        "B" => Some("b"),
        "C" => Some("c"),
        "D" => Some("d"),
        "E" => Some("e"),
        "H" => Some("h"),
        "L" => Some("l"),
        "M" => Some("m"),
        "A" => Some("a"),
        _ => None,
    }
}

fn z80_ld_indirect_register_key(register: &str) -> Option<&'static str> {
    match register.to_ascii_uppercase().as_str() {
        "A" => Some("a"),
        "HL" => Some("hl"),
        "BC" => Some("bc"),
        "DE" => Some("de"),
        "SP" => Some("sp"),
        "IX" => Some("ix"),
        "IY" => Some("iy"),
        _ => None,
    }
}

fn z80_half_index_prefix_key(prefix: &str) -> Option<&'static str> {
    match prefix.to_ascii_uppercase().as_str() {
        "IX" => Some("ix"),
        "IY" => Some("iy"),
        _ => None,
    }
}
