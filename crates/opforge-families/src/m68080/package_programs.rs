// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! CPU-owned portable operand records for the Motorola 68080 extension.

use package::{
    compile_numeric_encoding_program, compile_operand_record_program,
    compile_parameterized_encoding_program, compile_projected_fixup_program,
    compile_structured_encoding_program, compile_value_program, DiagnosticDescriptor,
    EncodingEndian, EncodingFieldSpec, EncodingStep, FixupBase, FixupEncodingStep, FixupRange,
    FixupRangeMapping, FixupTransform, ModeSelectorDescriptor, OpcpuCodecError,
    OperandRecordProgram, OperandRecordProgramDescriptor, PortableRelocationKind,
    RegisterEncodingDescriptor, SemanticProgramDescriptor, StructuredEncodingStep,
    UnresolvedValuePolicy, ValueConstraint, ValueProgramDescriptor, ValueProgramSource,
    VmProgramDescriptor, MODE_SELECTOR_PLAN_BOUNDED_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_SEQUENCE_PREFIX, MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR,
    MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX, MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX, MODE_SELECTOR_PLAN_INPUT_SEPARATOR,
    MODE_SELECTOR_PLAN_LITERAL_PREFIX, MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR,
    MODE_SELECTOR_PLAN_MEMBER_PREFIX, MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX,
    MODE_SELECTOR_PLAN_NAMED_REGISTER_RANGE_COUNT_PREFIX, MODE_SELECTOR_PLAN_OUT_OF_RANGE_PREFIX,
    MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX, MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX,
    MODE_SELECTOR_PLAN_REGISTER_INDEX_XOR_PREFIX,
    MODE_SELECTOR_PLAN_REGISTER_OR_NAMED_RANGE_PREFIX,
    MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR, MODE_SELECTOR_PLAN_REGISTER_SEQUENCE_PREFIX,
    MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX, MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX,
    MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX, MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX,
    MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX, MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR,
    MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX, OPERAND_RECORD_VM_VERSION_V1,
    OPERAND_RECORD_VM_VERSION_V3, SEMANTIC_VM_OPCODE_VERSION_V3, SEMANTIC_VM_OPCODE_VERSION_V6,
    SEMANTIC_VM_OPCODE_VERSION_V7, SEMANTIC_VM_OPCODE_VERSION_V8, SEMANTIC_VM_OP_EMIT_OPERAND,
    SEMANTIC_VM_OP_END, VALUE_VM_OPCODE_VERSION_V1,
};
use types::hierarchy::ScopedOwner;

use crate::families::m68k::package_programs::{
    DIAG_NO_SIZE_SUFFIX, FIXUP_PC_WORD, VALUE_IMMEDIATE_WORD,
};

pub const RECORD_AMMX_DATA_REGISTER: &str = "operand.ammx-data-register";
pub const RECORD_AMMX_VEA: &str = "operand.ammx-vea";
pub const RECORD_AMMX_PAIR: &str = "operand.ammx-pair";
pub const RECORD_AMMX_GROUP: &str = "operand.ammx-group";
pub const RECORD_TEXTURE_NESTED: &str = "operand.texture-nested";
pub const RECORD_TEXTURE_EXTERNAL_SCALE: &str = "operand.texture-external-scale";
pub const RECORD_TEXTURE_SCALED_INSIDE: &str = "operand.texture-scaled-inside";
pub const RECORD_TEXTURE_FLAT: &str = "operand.texture-flat";

pub const FORMAT_AMMX_VEA: u16 = 0;
pub const FORMAT_AMMX_PAIR: u16 = 1;
pub const FORMAT_AMMX_GROUP: u16 = 2;
pub const FORMAT_TEXTURE_NESTED: u16 = 16;
pub const FORMAT_TEXTURE_EXTERNAL_SCALE: u16 = 17;
pub const FORMAT_TEXTURE_SCALED_INSIDE: u16 = 18;
pub const FORMAT_TEXTURE_FLAT: u16 = 19;
pub const ENCODING_AMMX_GROUP: &str = "enc.ammx-group";
const PARAM_IMMEDIATE_WORD_FIELD_0: &str = "enc.template.immediate-word-field-0";
const PARAM_IMMEDIATE_WORD_FIELD_9: &str = "enc.template.immediate-word-field-9";
const PARAM_FIELD_0: &str = "enc.template.field-0";
const PARAM_FIELDS_0_9: &str = "enc.template.fields-0-9";
const PARAM_FIELDS_9_0: &str = "enc.template.fields-9-0";
const PARAM_FIELDS_12_0: &str = "enc.template.fields-12-0";
const PARAM_FIELDS_12_6_0: &str = "enc.template.fields-12-6-0";
const PARAM_FIELDS_10_7_0: &str = "enc.template.fields-10-7-0";
const PARAM_FULL_EXTENSION: &str = "enc.template.full-extension";
const PARAM_INDEX_EXTENSION: &str = "enc.template.index-extension";
const PARAM_SCALAR_WORD: &str = "enc.template.scalar-word";
const PARAM_SCALAR_LONG: &str = "enc.template.scalar-long";
const PARAM_INTEGER_IEEE_SINGLE: &str = "enc.numeric.integer-ieee-single";
const PARAM_INTEGER_IEEE_DOUBLE: &str = "enc.numeric.integer-ieee-double";
const PARAM_BANK_PREFIX: &str = "enc.m68080.bank-prefix";
const PARAM_BANK_PREFIX_FPU: &str = "enc.m68080.bank-prefix-fpu";
const PARAM_PERM_FIRST: &str = "enc.m68080.perm-first";
const PARAM_PERM_SECOND: &str = "enc.m68080.perm-second";
const PARAM_AMMX_THREE_REGISTER_FIRST: &str = "enc.m68080.ammx-three-register-first";
const PARAM_AMMX_THREE_REGISTER_SECOND: &str = "enc.m68080.ammx-three-register-second";
const PARAM_AMMX_C2P_SECOND: &str = "enc.m68080.ammx-c2p-second";
const PARAM_AMMX_GROUP_FIRST: &str = "enc.m68080.ammx-group-first";
const PARAM_AMMX_GROUP_SECOND: &str = "enc.m68080.ammx-group-second";
const PARAM_AMMX_INDIRECT_FIRST: &str = "enc.m68080.ammx-indirect-first";
const PARAM_AMMX_VEA_FIRST: &str = "enc.m68080.ammx-vea-first";
const PARAM_AMMX_VEA_BASE_FIRST: &str = "enc.m68080.ammx-vea-base-first";
const PARAM_AMMX_FIXED_EA_FIRST: &str = "enc.m68080.ammx-fixed-ea-first";
const PARAM_AMMX_VPERM_SECOND: &str = "enc.m68080.ammx-vperm-second";
const PARAM_TEX_SECOND: &str = "enc.m68080.tex-second";
const PARAM_TEX_STANDARD: &str = "enc.m68080.tex-standard";
const PARAM_TEX_BYTE: &str = "enc.m68080.tex-byte";
const FIXUP_LONG_COUNTER: &str = "fix.m68080.long-counter";
const FIXUP_EXTENDED_SHORT: &str = "fix.m68080.extended-short";
const PARAM_SCALAR_BYTE: &str = "enc.template.scalar-byte";
const VALUE_MOV3Q_MINUS_ONE: &str = "scalar.mov3q-minus-one";
const VALUE_MOV3Q_POSITIVE: &str = "scalar.mov3q-positive";
const VALUE_STOREM3_MODE: &str = "scalar.m68080.storem3-mode";
const VALUE_PERM_SELECTOR: &str = "scalar.m68080.perm-selector";
const VALUE_PACKED_THREE_BIT_COUNT: &str = "scalar.packed-three-bit-count";
const DIAG_ADDIW_IMMEDIATE_RANGE: &str = "encoding.addiw.immediate.range";
const DIAG_CMPIW_IMMEDIATE_RANGE: &str = "encoding.cmpiw.immediate.range";
const DIAG_MOVIW_IMMEDIATE_RANGE: &str = "encoding.moviw.immediate.range";
const DIAG_MOV3Q_APOLLO_DISABLED: &str = "encoding.mov3q.apollo-disabled";
const DIAG_MOVS_APOLLO_DISABLED: &str = "encoding.movs.apollo-disabled";
const DIAG_MOVZ_APOLLO_DISABLED: &str = "encoding.movz.apollo-disabled";
const DIAG_CLRQ_APOLLO_DISABLED: &str = "encoding.clrq.apollo-disabled";
const DIAG_EXTUB_WORD: &str = "encoding.extub.word-size";
const DIAG_EXTUW_MISSING_LONG: &str = "encoding.extuw.missing-long-size";
const DIAG_PERM_SELECTOR_RANGE: &str = "encoding.perm.selector-range";
const DIAG_PERM_LEFT_REGISTER: &str = "encoding.perm.left-register";
const DIAG_MOVE_SR_LONG: &str = "encoding.move-sr.long-size";
const DIAG_MOVE2_MISSING_SIZE: &str = "encoding.move2.missing-size";
const DIAG_MOVEX_MISSING_SIZE: &str = "encoding.movex.missing-size";
const DIAG_MOVEH_SIZE: &str = "encoding.moveh.size";
const DIAG_MOVZ2_LONG_SIZE: &str = "encoding.movz2.long-size";
const DIAG_TOUCH_ADDRESSING: &str = "encoding.touch.addressing";
const DIAG_MOVE2_SOURCE: &str = "encoding.move2.source";
const DIAG_MOVEH_SOURCE: &str = "encoding.moveh.source";
const DIAG_LONG_COUNTER_DISPLACEMENT: &str = "encoding.m68080.long-counter-displacement";
const DIAG_EXTENDED_SHORT_DISPLACEMENT: &str = "encoding.m68080.extended-short-displacement";
const DIAG_AMMX_PAIR_ALIGNMENT: &str = "encoding.m68080.ammx-pair-alignment";
const DIAG_AMMX_PAIR_SEQUENCE: &str = "encoding.m68080.ammx-pair-sequence";
const DIAG_AMMX_GROUP_ALIGNMENT: &str = "encoding.m68080.ammx-group-alignment";
const DIAG_AMMX_GROUP_SEQUENCE: &str = "encoding.m68080.ammx-group-sequence";
const DIAG_STOREM3_MODE: &str = "encoding.m68080.storem3-mode";
const DIAG_LEA_B_TO_B: &str = "encoding.m68080.lea-b-to-b";
const DIAG_QUICK_B_SIZE: &str = "encoding.m68080.quick-b-size";
const DIAG_SUBQ_B_SIZE: &str = "encoding.m68080.subq-b-size";
const DIAG_CMP_B_SIZE: &str = "encoding.m68080.cmp-b-size";
const DIAG_MOVE_B_SIZE: &str = "encoding.m68080.move-b-size";
const DIAG_MOVE_B_TO_B: &str = "encoding.m68080.move-b-to-b";
const DIAG_MOVEA_B_SIZE: &str = "encoding.m68080.movea-b-size";
const DIAG_MOVEA_B_TO_B: &str = "encoding.m68080.movea-b-to-b";
const DIAG_PACKUSWB_VEA: &str = "encoding.m68080.packuswb-vea";
const DIAG_PADD_FIRST_VEA: &str = "encoding.m68080.padd-first-vea";
const DIAG_AMMX_SELECTOR_REGISTER: &str = "encoding.m68080.ammx-selector-register";
const DIAG_AMMX_VPERM_OPERANDS: &str = "encoding.m68080.ammx-vperm-operands";
const DIAG_FMOVE_D_INVALID_SOURCE: &str = "encoding.m68080.fmove-d-invalid-source";
const DIAG_FMOVE_D_INVALID_DESTINATION: &str = "encoding.m68080.fmove-d-invalid-destination";
const DIAG_AMMX_LOAD_IMMEDIATE_SIZE: &str = "encoding.m68080.ammx-load-immediate-size";
const DIAG_FPU_EXTENDED_LITERAL: &str = "encoding.m68080.fpu-extended-literal";
const DIAG_FPU_REGISTER_FORM: &str = "encoding.m68080.fpu-register-form";
const DIAG_TEX8_NESTED_SHAPE: &str = "encoding.m68080.tex8-nested-shape";
const DIAG_TEX16_NESTED_SHAPE: &str = "encoding.m68080.tex16-nested-shape";
const DIAG_TEX_EXTERNAL_D0_SHAPE: &str = "encoding.m68080.tex-external-d0-shape";
const DIAG_TEX_BYTE_SHAPE: &str = "encoding.m68080.tex-byte-shape";

const EXTENDED_SHORT_BRANCHES: &[(&str, u32)] = &[
    ("BRA.S+", 0x60),
    ("BSR.S+", 0x61),
    ("BHI.S+", 0x62),
    ("BLS.S+", 0x63),
    ("BCC.S+", 0x64),
    ("BHS.S+", 0x64),
    ("BCS.S+", 0x65),
    ("BLO.S+", 0x65),
    ("BNE.S+", 0x66),
    ("BEQ.S+", 0x67),
    ("BVC.S+", 0x68),
    ("BVS.S+", 0x69),
    ("BPL.S+", 0x6a),
    ("BMI.S+", 0x6b),
    ("BGE.S+", 0x6c),
    ("BLT.S+", 0x6d),
    ("BGT.S+", 0x6e),
    ("BLE.S+", 0x6f),
];

const AMMX_THREE_REGISTER_OPCODES: &[(&str, u32)] = &[
    ("PADD.B", 0x10),
    ("PADD.W", 0x11),
    ("PADDB", 0x10),
    ("PADDW", 0x11),
    ("PADDUSB", 0x14),
    ("PADDUSW", 0x15),
    ("PSUB.B", 0x12),
    ("PSUB.W", 0x13),
    ("PSUBB", 0x12),
    ("PSUBW", 0x13),
    ("PSUBUSB", 0x16),
    ("PSUBUSW", 0x17),
    ("PAVGB", 0x0c),
    ("PMUL88", 0x18),
    ("PMULH", 0x1a),
    ("PMULL", 0x1b),
    ("PMULA", 0x19),
    ("PAND", 0x08),
    ("POR", 0x09),
    ("PEOR", 0x0a),
    ("PANDN", 0x0b),
    ("BSEL", 0x29),
    ("PCMPEQB", 0x20),
    ("PCMPHIB", 0x22),
    ("PCMPGEB", 0x2c),
    ("PCMPGTB", 0x2e),
    ("PCMPEQW", 0x21),
    ("PCMPHIW", 0x23),
    ("PCMPGEW", 0x2d),
    ("PCMPGTW", 0x2f),
    ("PMINSB", 0x30),
    ("PMINSW", 0x31),
    ("PMINUB", 0x32),
    ("PMINUW", 0x33),
    ("PMAXSB", 0x34),
    ("PMAXSW", 0x35),
    ("PMAXUB", 0x36),
    ("PMAXUW", 0x37),
    ("LSLQ", 0x38),
    ("LSRQ", 0x39),
];
const AMMX_PAIR_DESTINATION_OPCODES: &[(&str, u32)] = &[("BFLYB", 0x1c), ("BFLYW", 0x1d)];
const AMMX_GROUP_PAIR_OPCODES: &[(&str, u32)] = &[("TRANSHI", 0x02), ("TRANSLO", 0x03)];
const AMMX_UNSIZED_ALIASES: &[&str] = &[
    "PADDB", "PADDW", "PADDUSB", "PADDUSW", "PSUBB", "PSUBW", "PSUBUSB", "PSUBUSW", "PAVGB",
    "PMINSB", "PMINSW", "PMINUB", "PMINUW", "PMAXSB", "PMAXSW", "PMAXUB", "PMAXUW", "LSLQ", "LSRQ",
];

const FPU_DATA_REGISTER_TRANSFER_FORMATS: &[(&str, u32)] =
    &[("B", 6), ("W", 4), ("L", 0), ("S", 1), ("D", 5), ("X", 2)];
const FPU_ROUND_ZERO_FORMATS: &[(&str, u32)] = &[("B", 6), ("W", 4), ("L", 0)];
const FPU_BANKED_THREE_OPERAND_OPS: &[(&str, u32)] = &[
    ("FADD", 0x22),
    ("FCMP", 0x38),
    ("FDIV", 0x20),
    ("FMUL", 0x23),
    ("FREM", 0x25),
    ("FSCALE", 0x26),
    ("FSUB", 0x28),
];
const FPU_CONDITIONS: &[(&str, u32)] = &[
    ("F", 0x00),
    ("EQ", 0x01),
    ("OGT", 0x02),
    ("OGE", 0x03),
    ("OLT", 0x04),
    ("OLE", 0x05),
    ("OGL", 0x06),
    ("OR", 0x07),
    ("UN", 0x08),
    ("UEQ", 0x09),
    ("UGT", 0x0a),
    ("UGE", 0x0b),
    ("ULT", 0x0c),
    ("ULE", 0x0d),
    ("NE", 0x0e),
    ("T", 0x0f),
    ("SF", 0x10),
    ("SEQ", 0x11),
    ("GT", 0x12),
    ("GE", 0x13),
    ("LT", 0x14),
    ("LE", 0x15),
    ("GL", 0x16),
    ("GLE", 0x17),
    ("NGLE", 0x18),
    ("NGL", 0x19),
    ("NLE", 0x1a),
    ("NLT", 0x1b),
    ("NGE", 0x1c),
    ("NGT", 0x1d),
    ("SNE", 0x1e),
    ("ST", 0x1f),
];

pub const M68080_REGISTER_IDS: &[&str] = &[
    "E0", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12", "E13", "E14",
    "E15", "E16", "E17", "E18", "E19", "E20", "E21", "E22", "E23", "B0", "B1", "B2", "B3", "B4",
    "B5", "B6", "B7", "SFC", "DFC", "VBR", "CACR", "MSP", "ISP", "TC", "ITT0", "ITT1", "DTT0",
    "DTT1", "MMUSR", "URP", "SRP", "PCR", "CCC", "IEP1", "IEP2", "BPC", "BPW", "DCH", "DCM", "STR",
    "STC", "IEP3", "STH", "STB", "MWR",
];

/// Convert CPU-owned AMMX register spelling to an opaque class/index pair.
pub fn compile_register_input(register: &str) -> Option<(u16, u16)> {
    if register.eq_ignore_ascii_case("SP") {
        return Some((1, 7));
    }
    let (prefix, suffix) = register.split_at_checked(1)?;
    let index = suffix.parse::<u16>().ok()?;
    match prefix.to_ascii_uppercase().as_str() {
        "D" if index <= 7 => Some((0, index)),
        "A" if index <= 7 => Some((1, index)),
        "E" if index <= 23 => Some((4, index + 8)),
        "B" if index <= 7 => Some((5, index)),
        _ => None,
    }
}

pub fn register_encodings() -> Vec<RegisterEncodingDescriptor> {
    let owner = ScopedOwner::Cpu("m68080".to_string());
    let mut encodings = (0_u16..=23)
        .map(|index| RegisterEncodingDescriptor {
            owner: owner.clone(),
            id: format!("E{index}"),
            class: 4,
            index: index + 8,
        })
        .chain((0_u16..=7).map(|index| RegisterEncodingDescriptor {
            owner: owner.clone(),
            id: format!("B{index}"),
            class: 5,
            index,
        }))
        .collect::<Vec<_>>();
    encodings.extend(
        [
            ("SFC", 0x000_u16),
            ("DFC", 0x001),
            ("VBR", 0x801),
            ("CACR", 0x002),
            ("MSP", 0x803),
            ("ISP", 0x804),
            ("TC", 0x003),
            ("ITT0", 0x004),
            ("ITT1", 0x005),
            ("DTT0", 0x006),
            ("DTT1", 0x007),
            ("MMUSR", 0x805),
            ("URP", 0x806),
            ("SRP", 0x807),
            ("PCR", 0x808),
            ("CCC", 0x809),
            ("IEP1", 0x80a),
            ("IEP2", 0x80b),
            ("BPC", 0x80c),
            ("BPW", 0x80d),
            ("DCH", 0x80e),
            ("DCM", 0x80f),
            ("STR", 0x00a),
            ("STC", 0x00b),
            ("IEP3", 0x00c),
            ("STH", 0x00c),
            ("STB", 0x00d),
            ("MWR", 0x00e),
        ]
        .into_iter()
        .map(|(id, index)| RegisterEncodingDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            class: 7,
            index,
        }),
    );
    encodings
}

fn record(
    id: &str,
    schema_version: u16,
    program: OperandRecordProgram,
) -> Result<OperandRecordProgramDescriptor, OpcpuCodecError> {
    Ok(OperandRecordProgramDescriptor {
        owner: ScopedOwner::Cpu("m68080".to_string()),
        id: id.to_string(),
        schema_version,
        program: compile_operand_record_program(program)?,
    })
}

fn composite(id: &str, format: u16) -> Result<OperandRecordProgramDescriptor, OpcpuCodecError> {
    record(
        id,
        OPERAND_RECORD_VM_VERSION_V3,
        OperandRecordProgram::Composite {
            format,
            first_record_input: Some(0),
        },
    )
}

/// Compile CPU-scoped AMMX wrappers from neutral nested records.
pub fn operand_record_programs() -> Result<Vec<OperandRecordProgramDescriptor>, OpcpuCodecError> {
    Ok(vec![
        record(
            RECORD_AMMX_DATA_REGISTER,
            OPERAND_RECORD_VM_VERSION_V1,
            OperandRecordProgram::Register { register_input: 0 },
        )?,
        composite(RECORD_AMMX_VEA, FORMAT_AMMX_VEA)?,
        composite(RECORD_AMMX_PAIR, FORMAT_AMMX_PAIR)?,
        composite(RECORD_AMMX_GROUP, FORMAT_AMMX_GROUP)?,
        composite(RECORD_TEXTURE_NESTED, FORMAT_TEXTURE_NESTED)?,
        composite(RECORD_TEXTURE_EXTERNAL_SCALE, FORMAT_TEXTURE_EXTERNAL_SCALE)?,
        composite(RECORD_TEXTURE_SCALED_INSIDE, FORMAT_TEXTURE_SCALED_INSIDE)?,
        composite(RECORD_TEXTURE_FLAT, FORMAT_TEXTURE_FLAT)?,
    ])
}

/// Compile the CPU-owned AMMX group projection into a neutral composite emitter.
pub fn semantic_programs() -> Result<Vec<SemanticProgramDescriptor>, OpcpuCodecError> {
    let owner = ScopedOwner::Cpu("m68080".to_string());
    Ok(vec![
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_AMMX_GROUP.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V3,
            program: compile_structured_encoding_program(&[
                StructuredEncodingStep::CompositeValues {
                    record: 0,
                    format: FORMAT_AMMX_GROUP,
                    width: 2,
                    endian: EncodingEndian::Big,
                    item_bits: 4,
                    max_items: 4,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_BANK_PREFIX.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 1,
                        shift: 2,
                        bits: 2,
                        min: 0,
                        max: 3,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 0,
                        bits: 2,
                        min: 0,
                        max: 3,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_BANK_PREFIX_FPU.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0x7140,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: 2,
                        bits: 2,
                        min: 0,
                        max: 3,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: 0,
                        bits: 2,
                        min: 0,
                        max: 3,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 4,
                        bits: 2,
                        min: 0,
                        max: 3,
                    },
                    EncodingFieldSpec {
                        input: 3,
                        shift: 9,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_PERM_FIRST.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0x4cc0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: 3,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_PERM_SECOND.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: 0,
                        bits: 12,
                        min: 0,
                        max: 4095,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: 12,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 15,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_AMMX_THREE_REGISTER_FIRST.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0xfe00,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: 8,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: 7,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 6,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 3,
                        shift: 0,
                        bits: 4,
                        min: 0,
                        max: 15,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_AMMX_THREE_REGISTER_SECOND.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 1,
                        shift: 12,
                        bits: 4,
                        min: 0,
                        max: 15,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 8,
                        bits: 4,
                        min: 0,
                        max: 15,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_AMMX_C2P_SECOND.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0x00a8,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![EncodingFieldSpec {
                    input: 0,
                    shift: 10,
                    bits: 4,
                    min: 0,
                    max: 15,
                }],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_AMMX_GROUP_FIRST.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0xfe00,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: 8,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: 6,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 2,
                        bits: 2,
                        min: 0,
                        max: 3,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_AMMX_GROUP_SECOND.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![EncodingFieldSpec {
                    input: 1,
                    shift: 10,
                    bits: 4,
                    min: 0,
                    max: 15,
                }],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_AMMX_INDIRECT_FIRST.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0xfe10,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: 8,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: 7,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 6,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 3,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_AMMX_VEA_FIRST.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0xfe00,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: 8,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: 7,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 6,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 3,
                        shift: 0,
                        bits: 6,
                        min: 0,
                        max: 63,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_AMMX_VEA_BASE_FIRST.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0xfe00,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: 8,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: 7,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 6,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 3,
                        shift: 3,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 4,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_AMMX_FIXED_EA_FIRST.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0xfe3f,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: 8,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: 7,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 6,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_AMMX_VPERM_SECOND.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: 12,
                        bits: 4,
                        min: 0,
                        max: 15,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: 8,
                        bits: 4,
                        min: 0,
                        max: 15,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 0,
                        bits: 4,
                        min: 0,
                        max: 15,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_TEX_SECOND.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0x003e,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: 12,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: 8,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_TEX_STANDARD.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![EncodingFieldSpec {
                    input: 1,
                    shift: 12,
                    bits: 3,
                    min: 0,
                    max: 7,
                }],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_TEX_BYTE.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0x8000,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: 12,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: 4,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: FIXUP_LONG_COUNTER.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V7,
            program: compile_projected_fixup_program(&[FixupEncodingStep {
                input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                base: FixupBase::Position {
                    adjustment: 0,
                    target_references_only: true,
                },
                range: FixupRange::Signed,
                unresolved: UnresolvedValuePolicy::Placeholder(1),
                relocation: PortableRelocationKind::None,
                transform: FixupTransform::AlignedBitOr {
                    alignment: 2,
                    mask: 1,
                },
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: FIXUP_EXTENDED_SHORT.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V7,
            program: compile_projected_fixup_program(&[FixupEncodingStep {
                input: 0,
                width: 1,
                endian: EncodingEndian::Big,
                base: FixupBase::Position {
                    adjustment: 2,
                    target_references_only: true,
                },
                range: FixupRange::Signed,
                unresolved: UnresolvedValuePolicy::Placeholder(1),
                relocation: PortableRelocationKind::None,
                transform: FixupTransform::RangeMap {
                    alignment: 2,
                    mappings: vec![
                        FixupRangeMapping {
                            min: -256,
                            max: -132,
                            adjustment: 129,
                        },
                        FixupRangeMapping {
                            min: 128,
                            max: 254,
                            adjustment: -127,
                        },
                    ],
                },
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_INTEGER_IEEE_SINGLE.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V8,
            program: compile_numeric_encoding_program(&[EncodingStep::IntegerToIeee754 {
                input: 0,
                width: 4,
                endian: EncodingEndian::Big,
            }])?,
        },
        SemanticProgramDescriptor {
            owner,
            id: PARAM_INTEGER_IEEE_DOUBLE.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V8,
            program: compile_numeric_encoding_program(&[EncodingStep::IntegerToIeee754 {
                input: 0,
                width: 8,
                endian: EncodingEndian::Big,
            }])?,
        },
    ])
}

/// Mnemonics whose 68080-only encodings are fully package selected.
pub fn instruction_form_mnemonics() -> Vec<String> {
    let mut mnemonics = [
        "ADDIW.L",
        "CMPIW.L",
        "MOVIW.L",
        "MOVEX.L",
        "MOVEH",
        "MOVE2.W",
        "MOVZ2.W",
        "TOUCH",
        "MOV3Q",
        "MOVS.B",
        "MOVZ.W",
        "CLR.Q",
        "EXTUB.L",
        "EXTUB.W",
        "EXTUW.L",
        "EXTUW",
        "PERM",
        "MOVE16",
        "MOVE2",
        "MOVEX",
        "MOVEH.W",
        "MOVZ2.L",
        "DBRA.L",
        "C2P",
        "MINTERM",
        "STOREM",
        "STOREM3",
        "LOAD",
        "LOAD.W",
        "LOADI",
        "LOADI.W",
        "STORE",
        "STOREI",
        "STOREC",
        "STOREILM",
        "PACK3216",
        "PACKUSWB",
        "PACKUSBW",
        "UNPACK1632",
        "VPERM",
        "TEX8.512",
        "TEX16.256",
        "TEX24.64",
        "TEX.B",
    ]
    .into_iter()
    .map(str::to_string)
    .collect::<Vec<_>>();
    mnemonics.extend(
        EXTENDED_SHORT_BRANCHES
            .iter()
            .map(|(mnemonic, _)| (*mnemonic).to_string()),
    );
    mnemonics.extend(
        AMMX_THREE_REGISTER_OPCODES
            .iter()
            .map(|(mnemonic, _)| (*mnemonic).to_string()),
    );
    mnemonics.extend(
        AMMX_PAIR_DESTINATION_OPCODES
            .iter()
            .map(|(mnemonic, _)| (*mnemonic).to_string()),
    );
    mnemonics.extend(
        AMMX_GROUP_PAIR_OPCODES
            .iter()
            .map(|(mnemonic, _)| (*mnemonic).to_string()),
    );
    mnemonics.extend(
        AMMX_UNSIZED_ALIASES
            .iter()
            .flat_map(|mnemonic| [format!("{mnemonic}.B"), format!("{mnemonic}.W")]),
    );
    for base in ["FLOADI", "FSTOREI"] {
        mnemonics.extend(
            FPU_DATA_REGISTER_TRANSFER_FORMATS
                .iter()
                .map(|(suffix, _)| format!("{base}.{suffix}")),
        );
    }
    for base in ["FMOVERZ", "FMOVEURZ"] {
        mnemonics.extend(
            FPU_ROUND_ZERO_FORMATS
                .iter()
                .map(|(suffix, _)| format!("{base}.{suffix}")),
        );
    }
    mnemonics.extend(FPU_BANKED_THREE_OPERAND_OPS.iter().flat_map(|(base, _)| {
        FPU_DATA_REGISTER_TRANSFER_FORMATS
            .iter()
            .map(move |(suffix, _)| format!("{base}.{suffix}"))
    }));
    mnemonics.extend(
        FPU_CONDITIONS
            .iter()
            .map(|(suffix, _)| format!("FDB{suffix}.L")),
    );
    mnemonics
}

pub fn instruction_programs() -> Vec<VmProgramDescriptor> {
    let owner = ScopedOwner::Cpu("m68080".to_string());
    instruction_form_mnemonics()
        .into_iter()
        .map(|mnemonic| VmProgramDescriptor {
            owner: owner.clone(),
            mnemonic,
            mode_key: "semantic".to_string(),
            program: vec![SEMANTIC_VM_OP_EMIT_OPERAND, 0, SEMANTIC_VM_OP_END],
        })
        .collect()
}

/// Completed m68080-only forms that older CPU packages must reject without
/// reaching Rust CPU/family callbacks.
pub fn legacy_rejection_form_mnemonics() -> Vec<String> {
    let mut forms = Vec::new();
    forms.extend(
        AMMX_THREE_REGISTER_OPCODES
            .iter()
            .map(|(mnemonic, _)| (*mnemonic).to_string()),
    );
    forms.extend(
        AMMX_PAIR_DESTINATION_OPCODES
            .iter()
            .map(|(mnemonic, _)| (*mnemonic).to_string()),
    );
    forms.extend(
        AMMX_GROUP_PAIR_OPCODES
            .iter()
            .map(|(mnemonic, _)| (*mnemonic).to_string()),
    );
    forms.extend(
        [
            "ADDQ.L",
            "SUBQ.L",
            "CMP.L",
            "LEA",
            "MOVE.L",
            "MOVEA.L",
            "C2P",
            "MINTERM",
            "STOREM",
            "STOREM3",
            "LOAD",
            "LOAD.W",
            "LOADI",
            "LOADI.W",
            "STORE",
            "STOREI",
            "STOREC",
            "STOREILM",
            "PACK3216",
            "PACKUSWB",
            "PACKUSBW",
            "UNPACK1632",
            "VPERM",
            "TEX8.512",
            "TEX16.256",
            "TEX24.64",
            "TEX.B",
        ]
        .map(str::to_string),
    );
    for base in ["FLOADI", "FSTOREI"] {
        forms.extend(
            FPU_DATA_REGISTER_TRANSFER_FORMATS
                .iter()
                .map(|(suffix, _)| format!("{base}.{suffix}")),
        );
    }
    for base in ["FMOVERZ", "FMOVEURZ"] {
        forms.extend(
            FPU_ROUND_ZERO_FORMATS
                .iter()
                .map(|(suffix, _)| format!("{base}.{suffix}")),
        );
    }
    forms.extend(FPU_BANKED_THREE_OPERAND_OPS.iter().flat_map(|(base, _)| {
        FPU_DATA_REGISTER_TRANSFER_FORMATS
            .iter()
            .map(move |(suffix, _)| format!("{base}.{suffix}"))
    }));
    forms.extend(
        FPU_CONDITIONS
            .iter()
            .map(|(suffix, _)| format!("FDB{suffix}.L")),
    );
    forms
}

pub fn legacy_rejection_instruction_programs(cpu_id: &str) -> Vec<VmProgramDescriptor> {
    let owner = ScopedOwner::Cpu(cpu_id.to_string());
    legacy_rejection_form_mnemonics()
        .into_iter()
        .map(|mnemonic| VmProgramDescriptor {
            owner: owner.clone(),
            mnemonic,
            mode_key: "semantic".to_string(),
            program: vec![SEMANTIC_VM_OP_EMIT_OPERAND, 0, SEMANTIC_VM_OP_END],
        })
        .collect()
}

pub fn legacy_rejection_mode_selectors(cpu_id: &str) -> Vec<ModeSelectorDescriptor> {
    let owner = ScopedOwner::Cpu(cpu_id.to_string());
    let reject = |mnemonic: String, shape_key: &str, inputs: &str| {
        ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic,
        shape_key: shape_key.to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}encoding.m68080-only{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    }
    };
    let mut selectors = Vec::new();
    let reject_legacy_b_register = |mnemonic: &str, shape_key: &str, inputs: String| {
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: shape_key.to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}encoding.m68080-register.m68040{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
            ),
            priority: 2000,
            unstable_widen: false,
            width_rank: 0,
        }
    };
    for mnemonic in ["ADDQ.L", "SUBQ.L"] {
        selectors.push(reject_legacy_b_register(
            mnemonic,
            "immediate_register|immediate_direct",
            format!("expr0,{MODE_SELECTOR_PLAN_REGISTER_OR_NAMED_RANGE_PREFIX}1.classes5.prefixB.min0.max7"),
        ));
    }
    selectors.push(reject_legacy_b_register(
        "CMP.L",
        "register_register|direct_register",
        format!("{MODE_SELECTOR_PLAN_REGISTER_OR_NAMED_RANGE_PREFIX}0.classes5.prefixB.min0.max7,reg1.class0"),
    ));
    selectors.push(reject_legacy_b_register(
        "MOVE.L",
        "register_register|direct_register",
        format!("{MODE_SELECTOR_PLAN_REGISTER_OR_NAMED_RANGE_PREFIX}0.classes5.prefixB.min0.max7,reg1.class0"),
    ));
    selectors.push(reject_legacy_b_register(
        "MOVEA.L",
        "register_register|register_direct",
        format!("reg0.class0,{MODE_SELECTOR_PLAN_REGISTER_OR_NAMED_RANGE_PREFIX}1.classes5.prefixB.min0.max7"),
    ));
    selectors.push(reject_legacy_b_register(
        "LEA",
        "direct_register|direct_direct",
        format!(
            "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,{MODE_SELECTOR_PLAN_REGISTER_OR_NAMED_RANGE_PREFIX}1.classes5.prefixB.min0.max7"
        ),
    ));
    selectors.extend(AMMX_THREE_REGISTER_OPCODES.iter().map(|(mnemonic, _)| {
        reject(
            (*mnemonic).to_string(),
            "register_register_register",
            "reg0.class0,reg1.class0,reg2.class0",
        )
    }));
    selectors.extend(AMMX_PAIR_DESTINATION_OPCODES.iter().map(|(mnemonic, _)| {
        reject(
            (*mnemonic).to_string(),
            "register_register_direct",
            "reg0.class0,reg1.class0,call_arg_register2.arg0.class0,call_arg_register2.arg1.class0",
        )
    }));
    for (mnemonic, shape, inputs) in [
        ("C2P", "register_register", "reg0.class0,reg1.class0"),
        (
            "MINTERM",
            "direct_register",
            "register_sequence0.class0.count4.align4,reg1.class0",
        ),
        (
            "TRANSHI",
            "direct_direct",
            "register_sequence0.class0.count4.align4,call_arg_register_sequence1.arg0.arg1.class0.align2",
        ),
        (
            "TRANSLO",
            "direct_direct",
            "register_sequence0.class0.count4.align4,call_arg_register_sequence1.arg0.arg1.class0.align2",
        ),
        (
            "STOREM",
            "register_register_direct",
            "reg0.class0,reg1.class0,indirect_reg2.class1",
        ),
        (
            "STOREM3",
            "register_immediate_direct",
            "reg0.class0,expr1,indirect_reg2.class1",
        ),
        (
            "STOREM3",
            "register_register_direct",
            "reg0.class0,reg1.class0,indirect_reg2.class1",
        ),
        ("LOAD", "direct_register", "indirect_reg0.class1,reg1.class0"),
        ("LOAD.W", "immediate_register", "expr0,reg1.class0"),
        ("LOADI", "direct_register", "indirect_reg0.class1,reg1.class0"),
        ("STORE", "register_direct", "reg0.class0,indirect_reg1.class1"),
        ("STOREI", "register_direct", "reg0.class0,indirect_reg1.class1"),
        (
            "STOREC",
            "register_register_direct",
            "reg0.class0,reg1.class0,indirect_reg2.class1",
        ),
        (
            "STOREILM",
            "register_register_direct",
            "reg0.class0,reg1.class0,indirect_reg2.class1",
        ),
        (
            "PACK3216",
            "register_register_register",
            "reg0.class0,reg1.class0,reg2.class0",
        ),
        (
            "PACKUSWB",
            "register_register_direct",
            "reg0.class0,reg1.class0,indirect_reg2.class1",
        ),
        (
            "PACKUSBW",
            "register_register_direct",
            "reg0.class0,reg1.class0,indirect_reg2.class1",
        ),
        (
            "UNPACK1632",
            "register_direct",
            "reg0.class0,call_arg_register1.arg0.class0,call_arg_register1.arg1.class0",
        ),
        (
            "VPERM",
            "immediate_register_register_register",
            "expr0,reg1.class0,reg2.class0,reg3.class0",
        ),
    ] {
        selectors.push(reject(mnemonic.to_string(), shape, inputs));
    }
    for base in ["FLOADI", "FSTOREI"] {
        selectors.extend(
            FPU_DATA_REGISTER_TRANSFER_FORMATS
                .iter()
                .map(|(suffix, _)| {
                    let inputs = if base == "FLOADI" {
                        "reg0.class0,reg1.class2"
                    } else {
                        "reg0.class2,reg1.class0"
                    };
                    reject(format!("{base}.{suffix}"), "register_register", inputs)
                }),
        );
    }
    for base in ["FMOVERZ", "FMOVEURZ"] {
        for (suffix, _) in FPU_ROUND_ZERO_FORMATS {
            let mnemonic = format!("{base}.{suffix}");
            selectors.push(reject(
                mnemonic.clone(),
                "register_register",
                "reg0.class2,reg1.class0",
            ));
            selectors.push(reject(mnemonic, "register_direct", "reg0.class2,expr1"));
        }
    }
    for &(base, _) in FPU_BANKED_THREE_OPERAND_OPS {
        for (mnemonic, source_class) in std::iter::once((base.to_string(), 2_u16)).chain(
            FPU_DATA_REGISTER_TRANSFER_FORMATS
                .iter()
                .map(|(suffix, _)| (format!("{base}.{suffix}"), 0_u16)),
        ) {
            let operand_plan = format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}encoding.m68080-only{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_OR_NAMED_RANGE_PREFIX}0.classes{source_class}.prefixE.min0.max23,{MODE_SELECTOR_PLAN_REGISTER_OR_NAMED_RANGE_PREFIX}1.classes2.prefixE.min0.max23,{MODE_SELECTOR_PLAN_REGISTER_OR_NAMED_RANGE_PREFIX}2.classes2.prefixE.min0.max23,{MODE_SELECTOR_PLAN_NAMED_REGISTER_RANGE_COUNT_PREFIX}0+1+2.prefixE.min0.max23.atleast1"
            );
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic,
                shape_key: [
                    "register_register_direct",
                    "register_direct_register",
                    "register_direct_direct",
                    "direct_register_register",
                    "direct_register_direct",
                    "direct_direct_register",
                    "direct_direct_direct",
                ]
                .join("|"),
                mode_key: "semantic".to_string(),
                operand_plan,
                priority: 0,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (suffix, _) in FPU_CONDITIONS {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: format!("FDB{suffix}.L"),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_NO_SIZE_SUFFIX}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,expr1"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for mnemonic in ["TEX8.512", "TEX16.256"] {
        let inputs = format!(
            "{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/r1,{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/i/t0/r1,{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/i/t1/r1,reg1.class0"
        );
        selectors.push(reject(
            mnemonic.to_string(),
            "direct_register",
            inputs.as_str(),
        ));
    }
    let tex24_inputs = format!(
        "{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/l/i/t0/r1,{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/l/i/t1/i/t0/r1,{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/l/i/t1/i/t1/r1,{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/r/nD0,reg1.class0"
    );
    selectors.push(reject(
        "TEX24.64".to_string(),
        "direct_register",
        tex24_inputs.as_str(),
    ));
    let tex_byte_inputs = format!(
        "{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/r1,{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/l/r1,{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/r/r0,{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t2/r1,reg1.class0"
    );
    selectors.push(reject(
        "TEX.B".to_string(),
        "direct_register",
        tex_byte_inputs.as_str(),
    ));
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FMOVE.D".to_string(),
        shape_key: "register_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_FMOVE_D_INVALID_SOURCE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class2"
        ),
        priority: 100,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner,
        mnemonic: "FMOVE.D".to_string(),
        shape_key: "register_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_FMOVE_D_INVALID_DESTINATION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,reg1.class0"
        ),
        priority: 101,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors
}

pub fn value_programs() -> Result<Vec<ValueProgramDescriptor>, OpcpuCodecError> {
    let owner = ScopedOwner::Cpu("m68080".to_string());
    Ok(vec![
        ValueProgramDescriptor {
            owner: owner.clone(),
            id: VALUE_MOV3Q_MINUS_ONE.to_string(),
            opcode_version: VALUE_VM_OPCODE_VERSION_V1,
            program: compile_value_program(
                ValueProgramSource::Input(0),
                &[ValueConstraint::InclusiveRange { min: -1, max: -1 }],
            )?,
        },
        ValueProgramDescriptor {
            owner,
            id: VALUE_MOV3Q_POSITIVE.to_string(),
            opcode_version: VALUE_VM_OPCODE_VERSION_V1,
            program: compile_value_program(
                ValueProgramSource::Input(0),
                &[ValueConstraint::InclusiveRange { min: 1, max: 7 }],
            )?,
        },
        ValueProgramDescriptor {
            owner: ScopedOwner::Cpu("m68080".to_string()),
            id: VALUE_STOREM3_MODE.to_string(),
            opcode_version: VALUE_VM_OPCODE_VERSION_V1,
            program: compile_value_program(
                ValueProgramSource::Input(0),
                &[ValueConstraint::InclusiveRange { min: 0, max: 3 }],
            )?,
        },
        ValueProgramDescriptor {
            owner: ScopedOwner::Cpu("m68080".to_string()),
            id: VALUE_PERM_SELECTOR.to_string(),
            opcode_version: VALUE_VM_OPCODE_VERSION_V1,
            program: compile_value_program(
                ValueProgramSource::Input(0),
                &[ValueConstraint::InclusiveRange { min: 0, max: 4095 }],
            )?,
        },
    ])
}

/// Select the first package-owned 68080 integer-extension slice.
pub fn mode_selectors() -> Vec<ModeSelectorDescriptor> {
    let owner = ScopedOwner::Cpu("m68080".to_string());
    let mut selectors = Vec::new();

    for (priority, (mnemonic, base)) in [("ADDQ.L", 0x5008_u32), ("SUBQ.L", 0x5108_u32)]
        .into_iter()
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_PACKED_THREE_BIT_COUNT}:expr0,reg1.class5{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for mnemonic in ["ADDQ.B", "ADDQ.W", "SUBQ.B", "SUBQ.W"] {
        let diagnostic = if mnemonic.starts_with("ADDQ") {
            DIAG_QUICK_B_SIZE
        } else {
            DIAG_SUBQ_B_SIZE
        };
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,reg1.class5"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for mnemonic in ["CMP.B", "CMP.W"] {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_CMP_B_SIZE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class5,reg1.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for mnemonic in ["MOVE.B", "MOVE.W"] {
        for inputs in ["reg0.class5,reg1.class0", "reg0.class0,reg1.class5"] {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: "register_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVE_B_SIZE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
                ),
                priority: 0,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    selectors.extend([
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "CMP.L".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class0,reg0.class5{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7",
                0xc180_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVE.L".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class0,reg0.class5{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7",
                0x1008_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVE.L".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class5{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7,reg0.class0",
                0x1040_u32
            ),
            priority: 1,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVEA.L".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class5{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7,reg0.class0",
                0x1040_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVE.L".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVE_B_TO_B}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class5,reg1.class5"
            ),
            priority: 100,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVEA.W".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVEA_B_SIZE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class5"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVEA.L".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVEA_B_TO_B}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class5,reg1.class5"
            ),
            priority: 100,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "LEA".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class5;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0;match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2",
                0x4168_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "LEA".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class5,reg1.class1",
                0x41c8_u32
            ),
            priority: 1,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "LEA".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_LEA_B_TO_B}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class5,reg1.class5"
            ),
            priority: 100,
            unstable_widen: false,
            width_rank: 0,
        },
    ]);

    for (mnemonic, diagnostic) in [
        ("ADDIW.L", DIAG_ADDIW_IMMEDIATE_RANGE),
        ("CMPIW.L", DIAG_CMPIW_IMMEDIATE_RANGE),
        ("MOVIW.L", DIAG_MOVIW_IMMEDIATE_RANGE),
    ] {
        for shape_key in ["immediate_register", "immediate_direct"] {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: shape_key.to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_OUT_OF_RANGE_PREFIX}0.min-32768.max65535"
                ),
                priority: 0,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }

    for (mnemonic, base, diagnostic) in [
        ("ADDIW.L", 0x06c0_u32, DIAG_ADDIW_IMMEDIATE_RANGE),
        ("CMPIW.L", 0x4e00_u32, DIAG_CMPIW_IMMEDIATE_RANGE),
    ] {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_IMMEDIATE_WORD_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_IMMEDIATE_WORD}:expr0,reg1.class0{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{diagnostic}"
            ),
            priority: 1,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/r1;encode:{PARAM_IMMEDIATE_WORD_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_IMMEDIATE_WORD}:expr0,{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/r1{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{diagnostic}",
                base + 0x10
            ),
            priority: 1,
            unstable_widen: false,
            width_rank: 0,
        });
        for (mode_priority, (mode_bits, ea_source)) in [
            (
                0x18_u32,
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                0x20_u32,
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: "immediate_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_IMMEDIATE_WORD_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_IMMEDIATE_WORD}:expr0,{ea_source}{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{diagnostic}",
                    base + mode_bits
                ),
                priority: (2 + mode_priority) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2;encode:{PARAM_IMMEDIATE_WORD_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_IMMEDIATE_WORD}:expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{diagnostic}",
                base + 0x28
            ),
            priority: 4,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3;encode:{PARAM_IMMEDIATE_WORD_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_IMMEDIATE_WORD}:expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{diagnostic}",
                base + 0x30
            ),
            priority: 5,
            unstable_widen: false,
            width_rank: 0,
        });
        for (absolute_priority, (field, mode_bits, value_program)) in [
            ("W", 0x38_u32, PARAM_SCALAR_WORD),
            ("L", 0x39_u32, PARAM_SCALAR_LONG),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: "immediate_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_IMMEDIATE_WORD}:expr0;encode:{value_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field}{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{diagnostic}",
                    base + mode_bits
                ),
                priority: (6 + absolute_priority) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }

    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "MOVIW.L".to_string(),
        shape_key: "immediate_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_IMMEDIATE_WORD_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_IMMEDIATE_WORD}:expr0,reg1.class0{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_MOVIW_IMMEDIATE_RANGE}",
            0x303d_u32
        ),
        priority: 1,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "MOVIW.L".to_string(),
        shape_key: "immediate_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_IMMEDIATE_WORD_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}12477,{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_IMMEDIATE_WORD}:expr0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_MOVIW_IMMEDIATE_RANGE}"
        ),
        priority: 2,
        unstable_widen: false,
        width_rank: 0,
    });
    for (priority, (field, first_word, value_program)) in [
        ("W", 0x31fd_u32, PARAM_SCALAR_WORD),
        ("L", 0x33fd_u32, PARAM_SCALAR_LONG),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVIW.L".to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{first_word};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_IMMEDIATE_WORD}:expr0;encode:{value_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field}{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_MOVIW_IMMEDIATE_RANGE}"
            ),
            priority: (3 + priority) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }

    selectors.extend([
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVEX.L".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0;encode:{PARAM_FIELDS_12_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0",
                0x0e80_u32, 0x0010_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVEH".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_FIELDS_12_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}3",
                0x0e50_u32, 0x0010_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "TOUCH".to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1",
                0xf610_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVE2.W".to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_FIELDS_12_6_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1",
                0x0e50_u32, 0x0020_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVZ2.W".to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_FIELDS_12_6_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2",
                0x0e50_u32, 0x0010_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOV3Q".to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.apollo_mode=1?{DIAG_MOV3Q_APOLLO_DISABLED};{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_MOV3Q_MINUS_ONE}:expr0;encode:{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,reg1.class0",
                0xa040_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOV3Q".to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.apollo_mode=1?{DIAG_MOV3Q_APOLLO_DISABLED};{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_MOV3Q_POSITIVE}:expr0,reg1.class0",
                0xa040_u32
            ),
            priority: 1,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVS.B".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.apollo_mode=1?{DIAG_MOVS_APOLLO_DISABLED};{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0,reg1.class0",
                0xa100_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVZ.W".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.apollo_mode=1?{DIAG_MOVZ_APOLLO_DISABLED};{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0",
                0xa1d0_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
    ]);
    for (suffix, format_field) in FPU_DATA_REGISTER_TRANSFER_FORMATS {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: format!("FLOADI.{suffix}"),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class2;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0",
                0xf200_u32, 0x4000_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: format!("FSTOREI.{suffix}"),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,reg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class0;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg0.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0",
                0xf200_u32, 0x6000_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        });
    }

    for (base, low_bits) in [("FMOVERZ", 1_u32), ("FMOVEURZ", 3_u32)] {
        for (suffix, format_field) in FPU_ROUND_ZERO_FORMATS {
            let mnemonic = format!("{base}.{suffix}");
            let second_word = 0x6000_u32;

            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.clone(),
                shape_key: "register_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,reg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class0;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{second_word},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg0.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{low_bits}",
                    0xf200_u32
                ),
                priority: 0,
                unstable_widen: false,
                width_rank: 0,
            });

            for (priority, first_word, ea_source, trailing) in [
                (
                    1_u16,
                    0xf210_u32,
                    format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
                    None,
                ),
                (
                    2,
                    0xf218,
                    format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"),
                    None,
                ),
                (
                    3,
                    0xf220,
                    format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"),
                    None,
                ),
                (
                    4,
                    0xf228,
                    format!("{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2"),
                    Some(format!("encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0")),
                ),
            ] {
                let trailing = trailing.map(|step| format!(";{step}")).unwrap_or_default();
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: mnemonic.clone(),
                    shape_key: "register_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,{ea_source};encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{first_word},{ea_source};encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{second_word},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg0.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{low_bits}{trailing}"
                    ),
                    priority,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }

            for (priority, index_class, index_long) in
                [(5_u16, 0_u16, 0_u16), (6, 0, 1), (7, 1, 0), (8, 1, 1)]
            {
                let qualifier = if index_long == 0 { "W" } else { "L" };
                let base_register = format!("{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1");
                let index_register = format!("{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class}");
                let displacement = format!("{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0");
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: mnemonic.clone(),
                    shape_key: "register_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,{base_register},{index_register},{displacement},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{base_register};encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{second_word},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg0.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{low_bits};encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{index_register},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_long},{displacement}",
                        0xf230_u32, index_class.min(1)
                    ),
                    priority,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }

            for (priority, suffix, first_word, scalar_program) in [
                (9_u16, "W", 0xf238_u32, PARAM_SCALAR_WORD),
                (10, "L", 0xf239, PARAM_SCALAR_LONG),
            ] {
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: mnemonic.clone(),
                    shape_key: "register_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{suffix};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{first_word};encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{second_word},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg0.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{low_bits};encode:{scalar_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{suffix}"
                    ),
                    priority,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }

            let full_base = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/t1/r1");
            let full_index = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/t2/qL.c0");
            let full_scale = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/t2/s");
            let full_displacement = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/t0/mW");
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.clone(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,{full_base},{full_index},{full_scale},{full_displacement};encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{full_base};encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{second_word},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg0.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{low_bits};encode:{PARAM_FULL_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2336,{full_index},{full_scale};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{full_displacement}",
                    0xf230_u32
                ),
                priority: 11,
                unstable_widen: false,
                width_rank: 0,
            });

            for (priority, (base_item, index_item)) in
                [(1_usize, 2_usize), (0, 1)].into_iter().enumerate()
            {
                let base = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/t0/b/t{base_item}/r1");
                let index =
                    format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/t0/b/t{index_item}/qL.c0");
                let scale =
                    format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/t0/b/t{index_item}/s");
                let outer = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/t1/mW");
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: mnemonic.clone(),
                    shape_key: "register_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,{base},{index},{scale},{outer};encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{base};encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{second_word},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg0.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{low_bits};encode:{PARAM_FULL_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2322,{index},{scale};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{outer}",
                        0xf230_u32
                    ),
                    priority: 12 + priority as u16,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
    }

    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "DBRA.L".to_string(),
        shape_key: "register_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0;fixup:{FIXUP_LONG_COUNTER}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_LONG_COUNTER_DISPLACEMENT}",
            0x51c8_u32
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 1,
    });
    selectors.extend(FPU_CONDITIONS.iter().map(|(suffix, condition)| {
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: format!("FDB{suffix}.L"),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{condition};fixup:{FIXUP_LONG_COUNTER}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_LONG_COUNTER_DISPLACEMENT}",
                0xf248_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 1,
        }
    }));
    selectors.extend(EXTENDED_SHORT_BRANCHES.iter().map(|(mnemonic, opcode)| {
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: (*mnemonic).to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_SCALAR_BYTE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{opcode};fixup:{FIXUP_EXTENDED_SHORT}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_EXTENDED_SHORT_DISPLACEMENT}"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        }
    }));
    selectors.extend([
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "FMOVE.D".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class2;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}5,reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0",
                0xf200_u32, 0x4000_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "FMOVE.D".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,reg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class0;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}5,reg0.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0",
                0xf200_u32, 0x6000_u32
            ),
            priority: 1,
            unstable_widen: false,
            width_rank: 0,
        },
    ]);
    for (priority, (mnemonic, operation)) in [("FADD.D", 0x22_u16), ("FMOVE.X", 0x00)]
        .into_iter()
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,reg1.class2;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}62012;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{operation};encode:{PARAM_INTEGER_IEEE_SINGLE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0"
            ),
            priority: (20 + priority) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, operation, suffix, format_field, numeric_program)) in [
        ("FADD.D", 0x22_u16, "D", 5_u16, PARAM_INTEGER_IEEE_DOUBLE),
        ("FMOVE.S", 0x00, "S", 1, PARAM_INTEGER_IEEE_SINGLE),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{suffix},reg1.class2;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}62012;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{operation};encode:{numeric_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{suffix}"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FMOVE.X".to_string(),
        shape_key: "immediate_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_FPU_EXTENDED_LITERAL}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}X,reg1.class2"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FSIN".to_string(),
        shape_key: "register_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_FPU_REGISTER_FORM}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,reg1.class0"
        ),
        priority: 100,
        unstable_widen: false,
        width_rank: 0,
    });

    for &(mnemonic, operation) in FPU_BANKED_THREE_OPERAND_OPS {
        for (priority, (source_class, middle_class, destination_class)) in [0_u16, 4_u16]
            .into_iter()
            .flat_map(|source| {
                [2_u16, 4_u16].into_iter().flat_map(move |middle| {
                    [2_u16, 4_u16]
                        .into_iter()
                        .map(move |destination| (source, middle, destination))
                })
            })
            .filter(|&(source, middle, destination)| source == 4 || middle == 4 || destination == 4)
            .enumerate()
        {
            let source_bank = if source_class == 4 {
                format!("reg0.class4{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}3")
            } else {
                format!("{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0")
            };
            let middle_bank = if middle_class == 4 {
                format!("reg1.class4{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}3")
            } else {
                format!("{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0")
            };
            let source_low =
                format!("reg0.class{source_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7");
            let middle_low =
                format!("reg1.class{middle_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7");
            let xor = format!(
                "{MODE_SELECTOR_PLAN_REGISTER_INDEX_XOR_PREFIX}1.class{middle_class}.with2.class{destination_class}"
            );
            for (suffix, format_field) in FPU_DATA_REGISTER_TRANSFER_FORMATS {
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: format!("{mnemonic}.{suffix}"),
                    shape_key: "register_register_register".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{source_class},reg1.class{middle_class},reg2.class{destination_class};encode:{PARAM_BANK_PREFIX_FPU}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{source_bank},{middle_bank},{xor}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}3,{xor}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61952,{source_low};encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},{middle_low},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{operation}"
                    ),
                    priority: priority as u16,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }

        for (priority, (source_class, middle_class, destination_class)) in [2_u16, 4_u16]
            .into_iter()
            .flat_map(|source| {
                [2_u16, 4_u16].into_iter().flat_map(move |middle| {
                    [2_u16, 4_u16]
                        .into_iter()
                        .map(move |destination| (source, middle, destination))
                })
            })
            .filter(|&(source, middle, destination)| source == 4 || middle == 4 || destination == 4)
            .enumerate()
        {
            let source_bank = if source_class == 4 {
                format!("reg0.class4{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}3")
            } else {
                format!("{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0")
            };
            let middle_bank = if middle_class == 4 {
                format!("reg1.class4{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}3")
            } else {
                format!("{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0")
            };
            let source_low =
                format!("reg0.class{source_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7");
            let middle_low =
                format!("reg1.class{middle_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7");
            let xor = format!(
                "{MODE_SELECTOR_PLAN_REGISTER_INDEX_XOR_PREFIX}1.class{middle_class}.with2.class{destination_class}"
            );
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: "register_register_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.fpu_target=4?encoding.fpu-disabled.m68080;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{source_class},reg1.class{middle_class},reg2.class{destination_class};encode:{PARAM_BANK_PREFIX_FPU}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{source_bank},{middle_bank},{xor}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}3,{xor}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61952;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{source_low},{middle_low},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{operation}"
                ),
                priority: priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (mnemonic, opcode) in AMMX_THREE_REGISTER_OPCODES {
        for (priority, (first_class, second_class, third_class)) in [0_u16, 4_u16]
            .into_iter()
            .flat_map(|first| {
                [0_u16, 4_u16].into_iter().flat_map(move |second| {
                    [0_u16, 4_u16]
                        .into_iter()
                        .map(move |third| (first, second, third))
                })
            })
            .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: (*mnemonic).to_string(),
                shape_key: "register_register_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{first_class},reg1.class{second_class},reg2.class{third_class};encode:{PARAM_AMMX_THREE_REGISTER_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{first_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,reg1.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,reg2.class{third_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,reg0.class{first_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15;encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{opcode},reg1.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15,reg2.class{third_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15"
                ),
                priority: priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (mnemonic, first_class, second_class, third_class)) in ["PADD.B", "PADD.W"]
        .into_iter()
        .flat_map(|mnemonic| {
            [1_u16, 2, 3, 5, 6, 7, 8]
                .into_iter()
                .flat_map(move |first| {
                    [0_u16, 4].into_iter().flat_map(move |second| {
                        [0_u16, 4]
                            .into_iter()
                            .map(move |third| (mnemonic, first, second, third))
                    })
                })
        })
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_PADD_FIRST_VEA}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{first_class},reg1.class{second_class},reg2.class{third_class}"
            ),
            priority: 500 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, opcode, first_word)) in
        [("PADD.B", 0x10_u32, 0xfe3c_u32), ("PADD.W", 0x11, 0xff3c)]
            .into_iter()
            .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,reg1.class0,reg2.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{first_word};encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{opcode},reg1.class0,reg2.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0"
            ),
            priority: (100 + priority) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "LOAD.W".to_string(),
        shape_key: "immediate_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,reg1.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}65340;encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,reg1.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "LOAD".to_string(),
        shape_key: "immediate_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_AMMX_LOAD_IMMEDIATE_SIZE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,reg1.class0"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.extend(AMMX_UNSIZED_ALIASES.iter().flat_map(|mnemonic| {
        let selector_owner = owner.clone();
        ["B", "W"].into_iter().map(move |suffix| ModeSelectorDescriptor {
            owner: selector_owner.clone(),
            mnemonic: format!("{mnemonic}.{suffix}"),
            shape_key: "register_register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_NO_SIZE_SUFFIX}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class0,reg2.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        })
    }));
    for (priority, (mnemonic, variant)) in [("LOAD", 0_u32), ("LOADI", 1)].into_iter().enumerate() {
        for address_class in [1_u16, 5_u16] {
            for destination_class in [0_u16, 4_u16] {
                let address_high = u16::from(address_class == 5);
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "direct_register".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class{address_class},reg1.class{destination_class};encode:{PARAM_AMMX_INDIRECT_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{address_high},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,reg1.class{destination_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class{address_class};encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{variant},reg1.class{destination_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15"
                    ),
                    priority: (priority * 4 + usize::from(address_class == 5) * 2 + usize::from(destination_class == 4)) as u16,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
    }
    for address_class in [1_u16, 5_u16] {
        for destination_class in [0_u16, 4_u16] {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "LOADI.W".to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_NO_SIZE_SUFFIX}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class{address_class},reg1.class{destination_class}"
                ),
                priority: 0,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for address_class in [1_u16, 5_u16] {
        for invalid_class in [1_u16, 5_u16] {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "LOADI".to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_AMMX_SELECTOR_REGISTER}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class{address_class},reg1.class{invalid_class}"
                ),
                priority: 100,
                unstable_widen: false,
                width_rank: 0,
            });
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "STOREI".to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_AMMX_SELECTOR_REGISTER}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{invalid_class},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class{address_class}"
                ),
                priority: 100,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (mnemonic, variant)) in [("STORE", 0_u32), ("STOREI", 1)].into_iter().enumerate()
    {
        for source_class in [0_u16, 4_u16] {
            for address_class in [1_u16, 5_u16] {
                let address_high = u16::from(address_class == 5);
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "register_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{source_class},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class{address_class};encode:{PARAM_AMMX_INDIRECT_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{address_high},reg0.class{source_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class{address_class};encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}4,reg0.class{source_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{variant}"
                    ),
                    priority: (priority * 4 + usize::from(source_class == 4) * 2 + usize::from(address_class == 5)) as u16,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
    }

    // Expanded AMMX vector effective addresses remain package-owned.  These
    // selectors compose generic field, scalar, index-extension, and fixup VM
    // programs; no AMMX addressing semantics are required in the runtime.
    for register_class in [0_u16, 4_u16] {
        let register_high =
            format!("reg1.class{register_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4");
        let register_low =
            format!("reg1.class{register_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15");
        for address_class in [1_u16, 5_u16] {
            let address_high = u16::from(address_class == 5);
            let displacement = format!(
                "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
            );
            let base = format!(
                "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class{address_class}"
            );
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "LOAD".to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{displacement},{base},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,reg1.class{register_class};encode:{PARAM_AMMX_VEA_BASE_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{address_high},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{register_high},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}5,{base};encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{register_low};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{displacement}"
                ),
                priority: 20 + address_high + u16::from(register_class == 4) * 2,
                unstable_widen: false,
                width_rank: 0,
            });

            for (index_class, index_long, qualifier) in
                [(0_u16, 0_u16, "W"), (0, 1, "L"), (1, 0, "W"), (1, 1, "L")]
            {
                let index = format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class}"
                );
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: "LOAD".to_string(),
                    shape_key: "direct_register".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{displacement},{base},{index},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3,reg1.class{register_class};encode:{PARAM_AMMX_VEA_BASE_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{address_high},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{register_high},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}6,{base};encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{register_low};encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{index},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_long},{displacement}"
                    ),
                    priority: 24
                        + address_high
                        + u16::from(register_class == 4) * 2
                        + index_class * 4
                        + index_long * 8,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }

        let pc_displacement = format!(
            "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
        );
        let pc_base = format!(
            "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8"
        );
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "LOAD".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{pc_displacement},{pc_base},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,reg1.class{register_class};encode:{PARAM_AMMX_VEA_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{register_high},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}58;encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{register_low};fixup:{FIXUP_PC_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{pc_displacement}"
            ),
            priority: 40 + u16::from(register_class == 4),
            unstable_widen: false,
            width_rank: 0,
        });
        for (index_long, qualifier) in [(0_u16, "W"), (1, "L")] {
            let index = format!(
                "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class0"
            );
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "LOAD".to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{pc_displacement},{pc_base},{index},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3,reg1.class{register_class};encode:{PARAM_AMMX_VEA_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{register_high},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}59;encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{register_low};encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{index},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_long},{pc_displacement}"
                ),
                priority: 42 + index_long + u16::from(register_class == 4) * 2,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for (suffix, ea_bits, scalar_program, priority) in [
            ("W", 56_u16, PARAM_SCALAR_WORD, 46_u16),
            ("L", 57_u16, PARAM_SCALAR_LONG, 48_u16),
        ] {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "LOAD".to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{suffix},reg1.class{register_class};encode:{PARAM_AMMX_VEA_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{register_high},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{ea_bits};encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{register_low};encode:{scalar_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{suffix}"
                ),
                priority: priority + u16::from(register_class == 4),
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }

    for register_class in [0_u16, 4_u16] {
        let register_high =
            format!("reg0.class{register_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4");
        let register_low =
            format!("reg0.class{register_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15");
        for address_class in [1_u16, 5_u16] {
            let address_high = u16::from(address_class == 5);
            let displacement = format!(
                "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
            );
            let base = format!(
                "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class{address_class}"
            );
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "STORE".to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{register_class},{displacement},{base},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2;encode:{PARAM_AMMX_VEA_BASE_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{address_high},{register_high},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}5,{base};encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}4,{register_low},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{displacement}"
                ),
                priority: 20 + address_high + u16::from(register_class == 4) * 2,
                unstable_widen: false,
                width_rank: 0,
            });

            for (index_class, index_long, qualifier) in
                [(0_u16, 0_u16, "W"), (0, 1, "L"), (1, 0, "W"), (1, 1, "L")]
            {
                let index = format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class}"
                );
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: "STORE".to_string(),
                    shape_key: "register_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{register_class},{displacement},{base},{index},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3;encode:{PARAM_AMMX_VEA_BASE_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{address_high},{register_high},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}6,{base};encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}4,{register_low},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{index},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_long},{displacement}"
                    ),
                    priority: 24
                        + address_high
                        + u16::from(register_class == 4) * 2
                        + index_class * 4
                        + index_long * 8,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
        for (suffix, ea_bits, scalar_program, priority) in [
            ("W", 56_u16, PARAM_SCALAR_WORD, 46_u16),
            ("L", 57_u16, PARAM_SCALAR_LONG, 48_u16),
        ] {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "STORE".to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{register_class},{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{suffix};encode:{PARAM_AMMX_VEA_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{register_high},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{ea_bits};encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}4,{register_low},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;encode:{scalar_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{suffix}"
                ),
                priority: priority + u16::from(register_class == 4),
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (mnemonic, second_base)) in [("STOREC", 0x0024_u32), ("STOREILM", 0x0005)]
        .into_iter()
        .enumerate()
    {
        for first_class in [0_u16, 4_u16] {
            for second_class in [0_u16, 4_u16] {
                for address_class in [1_u16, 5_u16] {
                    let address_high = u16::from(address_class == 5);
                    selectors.push(ModeSelectorDescriptor {
                        owner: owner.clone(),
                        mnemonic: mnemonic.to_string(),
                        shape_key: "register_register_direct".to_string(),
                        mode_key: "semantic".to_string(),
                        operand_plan: format!(
                            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{first_class},reg1.class{second_class},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}2.class{address_class};encode:{PARAM_AMMX_INDIRECT_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{address_high},reg0.class{first_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,reg1.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}2.class{address_class};encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{second_base},reg0.class{first_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15,reg1.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15"
                        ),
                        priority: (priority * 8 + usize::from(first_class == 4) * 4 + usize::from(second_class == 4) * 2 + usize::from(address_class == 5)) as u16,
                        unstable_widen: false,
                        width_rank: 0,
                    });
                }
            }
        }
    }
    for (priority, (first_class, second_class, destination_class)) in [0_u16, 4_u16]
        .into_iter()
        .flat_map(|first| {
            [0_u16, 4_u16].into_iter().flat_map(move |second| {
                [0_u16, 4_u16]
                    .into_iter()
                    .map(move |destination| (first, second, destination))
            })
        })
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "PACK3216".to_string(),
            shape_key: "register_register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{first_class},reg1.class{second_class},reg2.class{destination_class};encode:{PARAM_AMMX_THREE_REGISTER_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg2.class{destination_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,reg0.class{first_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,reg1.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,reg2.class{destination_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15;encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}7,reg0.class{first_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15,reg1.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (first_class, second_class, destination_class)) in [0_u16, 4_u16]
        .into_iter()
        .flat_map(|first| {
            [0_u16, 4_u16].into_iter().flat_map(move |second| {
                [0_u16, 4_u16]
                    .into_iter()
                    .map(move |destination| (first, second, destination))
            })
        })
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "VPERM".to_string(),
            shape_key: "register_register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_AMMX_VPERM_OPERANDS}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{first_class},reg1.class{second_class},reg2.class{destination_class}"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for first_class in [0_u16, 4_u16] {
        for second_class in [0_u16, 4_u16] {
            for address_class in [1_u16, 5_u16] {
                for mnemonic in ["PACKUSWB", "PACKUSBW"] {
                    selectors.push(ModeSelectorDescriptor {
                        owner: owner.clone(),
                        mnemonic: mnemonic.to_string(),
                        shape_key: "register_register_direct".to_string(),
                        mode_key: "semantic".to_string(),
                        operand_plan: format!(
                            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{first_class},reg1.class{second_class},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}2.class{address_class};encode:{PARAM_AMMX_INDIRECT_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class{first_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,reg1.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}2.class{address_class};encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}6,reg0.class{first_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15,reg1.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15",
                            u16::from(address_class == 5)
                        ),
                        priority: 0,
                        unstable_widen: false,
                        width_rank: 0,
                    });
                }
            }
        }
    }
    for (priority, (first_class, second_class, invalid_class)) in [0_u16, 4_u16]
        .into_iter()
        .flat_map(|first| {
            [0_u16, 4_u16].into_iter().flat_map(move |second| {
                [1_u16, 5_u16]
                    .into_iter()
                    .map(move |invalid| (first, second, invalid))
            })
        })
        .enumerate()
    {
        for mnemonic in ["PACKUSWB", "PACKUSBW"] {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: "register_register_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_PACKUSWB_VEA}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{first_class},reg1.class{second_class},reg2.class{invalid_class}"
                ),
                priority: priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for source_class in [0_u16, 4_u16] {
        for pair_class in [0_u16, 4_u16] {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "UNPACK1632".to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{source_class},{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_SEQUENCE_PREFIX}1.arg0.arg1.class{pair_class}.align2;encode:{PARAM_AMMX_THREE_REGISTER_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{source_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,reg0.class{source_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15;encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}30,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class{pair_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15"
                ),
                priority: 0,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for pair_class in [0_u16, 4_u16] {
        for (priority, (violation, diagnostic)) in [
            ("alignment", DIAG_AMMX_PAIR_ALIGNMENT),
            ("sequence", DIAG_AMMX_PAIR_SEQUENCE),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "UNPACK1632".to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_SEQUENCE_PREFIX}1.arg0.arg1.class{pair_class}.align2.violation-{violation}"
                ),
                priority: (100 + priority) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (first_class, second_class, destination_class)) in [0_u16, 4_u16]
        .into_iter()
        .flat_map(|first| {
            [0_u16, 4_u16].into_iter().flat_map(move |second| {
                [0_u16, 4_u16]
                    .into_iter()
                    .map(move |destination| (first, second, destination))
            })
        })
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "VPERM".to_string(),
            shape_key: "immediate_register_register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,reg1.class{first_class},reg2.class{second_class},reg3.class{destination_class};encode:{PARAM_AMMX_FIXED_EA_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg1.class{first_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,reg2.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,reg3.class{destination_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4;encode:{PARAM_AMMX_VPERM_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg2.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15,reg3.class{destination_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15,reg1.class{first_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15;encode:{PARAM_SCALAR_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, third_base)) in [("TEX8.512", 0x8860_u32), ("TEX16.256", 0x8a51)]
        .into_iter()
        .enumerate()
    {
        let base = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/r1");
        let v = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/i/t0/r1");
        let u = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/i/t1/r1");
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{base},{v},{u},reg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}65072,{base};encode:{PARAM_TEX_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{u},reg1.class0;encode:{PARAM_TEX_STANDARD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{third_base},{v}"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    {
        let base = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/l/i/t0/r1");
        let v = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/l/i/t1/i/t0/r1");
        let u = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/l/i/t1/i/t1/r1");
        let modifier = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/r/nD0");
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "TEX24.64".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{base},{v},{u},{modifier},reg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}65072,{base};encode:{PARAM_TEX_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{u},reg1.class0;encode:{PARAM_TEX_STANDARD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}36482,{v}"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (v_side, modifier_side)) in [("l", "r"), ("r", "l")].into_iter().enumerate() {
        let base = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/r1");
        let v = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/{v_side}/r1");
        let modifier = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/{modifier_side}/r0");
        let u = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t2/r1");
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "TEX.B".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{base},{v},{modifier},{u},reg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}65072,{base};encode:{PARAM_TEX_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{u},reg1.class0;encode:{PARAM_TEX_BYTE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{v},{modifier}"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    {
        let base = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/r1");
        let v = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/r1");
        let u = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t2/r1");
        for (priority, (mnemonic, diagnostic)) in [
            ("TEX8.512", DIAG_TEX8_NESTED_SHAPE),
            ("TEX16.256", DIAG_TEX16_NESTED_SHAPE),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{base},{v},{u},reg1.class0"
                ),
                priority: (100 + priority) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "TEX.B".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_TEX_BYTE_SHAPE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{base},{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/i/t0/r1,{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/i/t1/r1,reg1.class0"
            ),
            priority: 100,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    {
        let base = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/l/i/t0/r1");
        let v = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/l/i/t1/i/t0/r1");
        let u = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/l/i/t1/i/t1/r1");
        let modifier = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/r/r0");
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "TEX24.64".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_TEX_EXTERNAL_D0_SHAPE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{base},{v},{u},{modifier},reg1.class0"
            ),
            priority: 100,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (mnemonic, opcode) in AMMX_PAIR_DESTINATION_OPCODES {
        for (priority, (first_class, second_class, pair_class)) in [0_u16, 4_u16]
            .into_iter()
            .flat_map(|first| {
                [0_u16, 4_u16].into_iter().flat_map(move |second| {
                    [0_u16, 4_u16]
                        .into_iter()
                        .map(move |pair| (first, second, pair))
                })
            })
            .enumerate()
        {
            let pair_high = format!(
                "{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_SEQUENCE_PREFIX}2.arg0.arg1.class{pair_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4.align2"
            );
            let pair_low = format!(
                "{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_SEQUENCE_PREFIX}2.arg0.arg1.class{pair_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15.align2"
            );
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: (*mnemonic).to_string(),
                shape_key: "register_register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{first_class},reg1.class{second_class},{pair_high};encode:{PARAM_AMMX_THREE_REGISTER_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{first_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,reg1.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,{pair_high},reg0.class{first_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15;encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{opcode},reg1.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15,{pair_low}"
                ),
                priority: priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for (priority, pair_class) in [0_u16, 4_u16].into_iter().enumerate() {
            for (violation_priority, (violation, diagnostic)) in [
                ("alignment", DIAG_AMMX_PAIR_ALIGNMENT),
                ("sequence", DIAG_AMMX_PAIR_SEQUENCE),
            ]
            .into_iter()
            .enumerate()
            {
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: (*mnemonic).to_string(),
                    shape_key: "register_register_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_SEQUENCE_PREFIX}2.arg0.arg1.class{pair_class}.align2.violation-{violation}"
                    ),
                    priority: (100 + priority * 2 + violation_priority) as u16,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
    }
    for (priority, (source_class, destination_class)) in [0_u16, 4_u16]
        .into_iter()
        .flat_map(|source| {
            [0_u16, 4_u16]
                .into_iter()
                .map(move |destination| (source, destination))
        })
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "C2P".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{source_class},reg1.class{destination_class};encode:{PARAM_AMMX_THREE_REGISTER_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{source_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,reg1.class{destination_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,reg0.class{source_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15;encode:{PARAM_AMMX_C2P_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg1.class{destination_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (group_class, destination_class)) in [0_u16, 4_u16]
        .into_iter()
        .flat_map(|group| {
            [0_u16, 4_u16]
                .into_iter()
                .map(move |destination| (group, destination))
        })
        .enumerate()
    {
        let group_high = format!(
            "{MODE_SELECTOR_PLAN_REGISTER_SEQUENCE_PREFIX}0.class{group_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}1.count4.align4"
        );
        let group_low = format!(
            "{MODE_SELECTOR_PLAN_REGISTER_SEQUENCE_PREFIX}0.class{group_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}2{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}3.count4.align4"
        );
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MINTERM".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{group_high},{group_low},reg1.class{destination_class};encode:{PARAM_AMMX_GROUP_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{group_high},reg1.class{destination_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,{group_low};encode:{PARAM_AMMX_GROUP_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}170,reg1.class{destination_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for group_class in [0_u16, 4_u16] {
        for (priority, (violation, diagnostic)) in [
            ("alignment", DIAG_AMMX_GROUP_ALIGNMENT),
            ("sequence", DIAG_AMMX_GROUP_SEQUENCE),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "MINTERM".to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_SEQUENCE_PREFIX}0.class{group_class}.count4.align4.violation-{violation}"
                ),
                priority: (100 + priority) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (mnemonic, opcode) in AMMX_GROUP_PAIR_OPCODES {
        for (priority, (group_class, pair_class)) in [0_u16, 4_u16]
            .into_iter()
            .flat_map(|group| [0_u16, 4_u16].into_iter().map(move |pair| (group, pair)))
            .enumerate()
        {
            let group_high = format!(
                "{MODE_SELECTOR_PLAN_REGISTER_SEQUENCE_PREFIX}0.class{group_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}1.count4.align4"
            );
            let group_low = format!(
                "{MODE_SELECTOR_PLAN_REGISTER_SEQUENCE_PREFIX}0.class{group_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}2{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}3.count4.align4"
            );
            let pair_high = format!(
                "{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_SEQUENCE_PREFIX}1.arg0.arg1.class{pair_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4.align2"
            );
            let pair_low = format!(
                "{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_SEQUENCE_PREFIX}1.arg0.arg1.class{pair_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15.align2"
            );
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: (*mnemonic).to_string(),
                shape_key: "direct_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{group_high},{group_low},{pair_high};encode:{PARAM_AMMX_GROUP_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{group_high},{pair_high},{group_low};encode:{PARAM_AMMX_GROUP_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{opcode},{pair_low}"
                ),
                priority: priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for class in [0_u16, 4_u16] {
            for (priority, (violation, diagnostic)) in [
                ("alignment", DIAG_AMMX_GROUP_ALIGNMENT),
                ("sequence", DIAG_AMMX_GROUP_SEQUENCE),
            ]
            .into_iter()
            .enumerate()
            {
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: (*mnemonic).to_string(),
                    shape_key: "direct_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_SEQUENCE_PREFIX}0.class{class}.count4.align4.violation-{violation}"
                    ),
                    priority: (100 + priority) as u16,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
            for (priority, (violation, diagnostic)) in [
                ("alignment", DIAG_AMMX_PAIR_ALIGNMENT),
                ("sequence", DIAG_AMMX_PAIR_SEQUENCE),
            ]
            .into_iter()
            .enumerate()
            {
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: (*mnemonic).to_string(),
                    shape_key: "direct_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_SEQUENCE_PREFIX}1.arg0.arg1.class{class}.align2.violation-{violation}"
                    ),
                    priority: (110 + priority) as u16,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
    }
    for (priority, (source_class, second_class, address_class)) in [0_u16, 4_u16]
        .into_iter()
        .flat_map(|source| {
            [0_u16, 4_u16].into_iter().flat_map(move |second| {
                [1_u16, 5_u16]
                    .into_iter()
                    .map(move |address| (source, second, address))
            })
        })
        .enumerate()
    {
        let address_high = u16::from(address_class == 5);
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "STOREM".to_string(),
            shape_key: "register_register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{source_class},reg1.class{second_class},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}2.class{address_class};encode:{PARAM_AMMX_INDIRECT_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{address_high},reg0.class{source_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,reg1.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}2.class{address_class};encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}37,reg0.class{source_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15,reg1.class{second_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (source_class, address_class)) in [0_u16, 4_u16]
        .into_iter()
        .flat_map(|source| {
            [1_u16, 5_u16]
                .into_iter()
                .map(move |address| (source, address))
        })
        .enumerate()
    {
        let address_high = u16::from(address_class == 5);
        for (shape_key, mode_source) in [
            (
                "register_immediate_direct",
                format!(
                    "{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_STOREM3_MODE}:expr1"
                ),
            ),
            (
                "register_register_direct",
                format!("{MODE_SELECTOR_PLAN_BOUNDED_REGISTER_PREFIX}1.class0.min0.max3"),
            ),
        ] {
            let diagnostic = if shape_key == "register_immediate_direct" {
                format!("{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_STOREM3_MODE}")
            } else {
                String::new()
            };
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "STOREM3".to_string(),
                shape_key: shape_key.to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{source_class},{mode_source},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}2.class{address_class};encode:{PARAM_AMMX_INDIRECT_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{address_high},reg0.class{source_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}4,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}2.class{address_class};encode:{PARAM_AMMX_THREE_REGISTER_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}37,reg0.class{source_class}{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}15,{mode_source}{diagnostic}"
                ),
                priority: priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "STOREM3".to_string(),
        shape_key: "register_register_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_STOREM3_MODE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_BOUNDED_REGISTER_PREFIX}1.class0.min0.max3.outside"
        ),
        priority: 100,
        unstable_widen: false,
        width_rank: 0,
    });

    selectors.extend([
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "CLR.Q".to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}m68k.apollo_mode=1?{DIAG_CLRQ_APOLLO_DISABLED};{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0",
                0xae00_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "EXTUB.L".to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0",
                0x4bc0_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "EXTUW.L".to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0",
                0x4dc0_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "EXTUB.W".to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_EXTUB_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "EXTUW".to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_EXTUW_MISSING_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "PERM".to_string(),
            shape_key: "immediate_register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_PERM_SELECTOR_RANGE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_OUT_OF_RANGE_PREFIX}0.min0.max4095,reg1.class0,reg2.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "PERM".to_string(),
            shape_key: "immediate_direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_PERM_LEFT_REGISTER}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1,reg2.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVE".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class5,reg1.class4;encode:{PARAM_BANK_PREFIX}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class4{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}3,reg1.class4{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}3;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class4{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7",
                0x7100_u32, 0x40c0_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVE.L".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVE_SR_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class5,reg1.class4"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVE16".to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}member_shape0.fieldL,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_SCALAR_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}member0.fieldL",
                0xf618_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVE2".to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVE2_MISSING_SIZE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVEX".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVEX_MISSING_SIZE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVEH.W".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVEH_SIZE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVZ2.L".to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVZ2_LONG_SIZE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "TOUCH".to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_TOUCH_ADDRESSING}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0"
            ),
            priority: 100,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVE2.W".to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVE2_SOURCE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0"
            ),
            priority: 100,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVEH".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVEH_SOURCE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class0"
            ),
            priority: 100,
            unstable_widen: false,
            width_rank: 0,
        },
    ]);

    for (mnemonic, base) in [("EXTUB.L", 0x4bc0_u32), ("EXTUW.L", 0x4dc0_u32)] {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class4;encode:{PARAM_BANK_PREFIX}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class4{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}3,reg0.class4{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}3;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},reg0.class4{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7",
                0x7100_u32
            ),
            priority: 1,
            unstable_widen: false,
            width_rank: 0,
        });
    }

    for (priority, (left_class, right_class)) in [
        (0_u16, 0_u16),
        (0, 1),
        (0, 4),
        (1, 0),
        (1, 1),
        (1, 4),
        (4, 0),
        (4, 1),
        (4, 4),
    ]
    .into_iter()
    .enumerate()
    {
        let source = |operand: usize, class: u16| {
            if class == 4 {
                format!("reg{operand}.class4{MODE_SELECTOR_PLAN_REGISTER_INDEX_MASK_SUFFIX}7")
            } else {
                format!("reg{operand}.class{class}")
            }
        };
        let bank = |operand: usize, class: u16| {
            if class == 4 {
                format!("reg{operand}.class4{MODE_SELECTOR_PLAN_REGISTER_INDEX_SHIFT_SUFFIX}3")
            } else {
                format!("{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0")
            }
        };
        let left = source(1, left_class);
        let right = source(2, right_class);
        let left_bank = bank(1, left_class);
        let right_bank = bank(2, right_class);
        let prefix = if left_class == 4 || right_class == 4 {
            format!(
                "encode:{PARAM_BANK_PREFIX}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{left_bank},{right_bank};",
                0x7100_u32
            )
        } else {
            String::new()
        };
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "PERM".to_string(),
            shape_key: "immediate_register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_PERM_SELECTOR}:expr0,reg1.class{left_class},reg2.class{right_class};{prefix}encode:{PARAM_PERM_FIRST}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{left},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{};encode:{PARAM_PERM_SECOND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{right},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{}",
                u8::from(left_class == 1),
                u8::from(right_class == 1),
            ),
            priority: 1 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }

    selectors
}

pub fn diagnostics() -> Vec<DiagnosticDescriptor> {
    vec![
        DiagnosticDescriptor {
            code: DIAG_ADDIW_IMMEDIATE_RANGE.to_string(),
            message_template:
                "ADDIW immediate {value} out of range for 16-bit word pattern (-32768..65535)"
                    .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_CMPIW_IMMEDIATE_RANGE.to_string(),
            message_template:
                "CMPIW immediate {value} out of range for 16-bit word pattern (-32768..65535)"
                    .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVIW_IMMEDIATE_RANGE.to_string(),
            message_template:
                "MOVIW immediate {value} out of range for 16-bit word pattern (-32768..65535)"
                    .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOV3Q_APOLLO_DISABLED.to_string(),
            message_template: "MOV3Q is Apollo-gated on m68080; enable .apollo on".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVS_APOLLO_DISABLED.to_string(),
            message_template: "MOVS is Apollo-gated on m68080; enable .apollo on".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVZ_APOLLO_DISABLED.to_string(),
            message_template: "MOVZ is Apollo-gated on m68080; enable .apollo on".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_CLRQ_APOLLO_DISABLED.to_string(),
            message_template: "CLR.Q is Apollo-gated on m68080; enable .apollo on".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_EXTUB_WORD.to_string(),
            message_template: "EXTUB does not support .W size".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_EXTUW_MISSING_LONG.to_string(),
            message_template: "EXTUW requires an explicit .L size".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_PERM_SELECTOR_RANGE.to_string(),
            message_template: "PERM selector {value} out of range (0-4095)".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_PERM_LEFT_REGISTER.to_string(),
            message_template: "PERM left register must be D0-D7 or A0-A7".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_AMMX_VPERM_OPERANDS.to_string(),
            message_template:
                "AMMX VPERM expects four operands: #imm,Dn/En,Dn/En,Dn/En".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVE_SR_LONG.to_string(),
            message_template: "MOVE SR does not support .L size".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVE2_MISSING_SIZE.to_string(),
            message_template: "MOVE2 requires an explicit .B, .W, or .L size suffix".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVEX_MISSING_SIZE.to_string(),
            message_template: "MOVEX requires an explicit .W or .L size suffix".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVEH_SIZE.to_string(),
            message_template: "MOVEH does not accept a size suffix on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVZ2_LONG_SIZE.to_string(),
            message_template: "MOVZ2 requires .B or .W size on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_TOUCH_ADDRESSING.to_string(),
            message_template: "TOUCH expects address-indirect or indexed memory syntax".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVE2_SOURCE.to_string(),
            message_template: "invalid source effective address for MOVE2.W".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVEH_SOURCE.to_string(),
            message_template: "invalid source effective address for MOVEH".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_LONG_COUNTER_DISPLACEMENT.to_string(),
            message_template: "{mnemonic} branch displacement must be even before applying the long-counter signal".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_EXTENDED_SHORT_DISPLACEMENT.to_string(),
            message_template: "{mnemonic} extended-short displacement out of range; displacement must be even on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_AMMX_PAIR_ALIGNMENT.to_string(),
            message_template: "AMMX {mnemonic} destination pair must start at an even register"
                .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_AMMX_PAIR_SEQUENCE.to_string(),
            message_template: "AMMX {mnemonic} destination pair must be consecutive".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_AMMX_GROUP_ALIGNMENT.to_string(),
            message_template:
                "AMMX {mnemonic} source group must start at a multiple-of-four register"
                    .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_AMMX_GROUP_SEQUENCE.to_string(),
            message_template:
                "AMMX {mnemonic} source group must cover four consecutive registers".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_STOREM3_MODE.to_string(),
            message_template: "STOREM3 mode must be in range 0-3".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_LEA_B_TO_B.to_string(),
            message_template: "LEA (Bn),Bm is not supported on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_QUICK_B_SIZE.to_string(),
            message_template: "ADDQ B-register destination requires .L size on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_SUBQ_B_SIZE.to_string(),
            message_template: "SUBQ B-register destination requires .L size on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_CMP_B_SIZE.to_string(),
            message_template: "CMP B-register source requires .L size on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVE_B_SIZE.to_string(),
            message_template: "B-register MOVE forms require .L size on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVE_B_TO_B.to_string(),
            message_template: "MOVE does not support Bn-to-Bn transfers on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVEA_B_SIZE.to_string(),
            message_template: "MOVEA B-register destination requires .L size on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVEA_B_TO_B.to_string(),
            message_template: "MOVEA.L Bn,Bm is not supported on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_PADD_FIRST_VEA.to_string(),
            message_template: "AMMX PADD first operand must be a vector effective address (D0-D7, E0-E23, (An)/(Bn), (An)/(Bn)+, -(An)/(Bn), d16(An)/(Bn), d8(An)/(Bn,Xn), d16(PC), d8(PC,Xn), Abs.W, or Abs.L)".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_PACKUSWB_VEA.to_string(),
            message_template: "AMMX {mnemonic} source must be a vector effective address"
                .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_AMMX_SELECTOR_REGISTER.to_string(),
            message_template: "AMMX {mnemonic} selector register must be D0-D7 or E0-E23; selector values map modulo 64 to D/A/B/E banks".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_FMOVE_D_INVALID_SOURCE.to_string(),
            message_template: "invalid source effective address for FMOVE.D".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_FMOVE_D_INVALID_DESTINATION.to_string(),
            message_template: "invalid destination effective address for FMOVE.D".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_AMMX_LOAD_IMMEDIATE_SIZE.to_string(),
            message_template: "AMMX LOAD immediate source requires .W size".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_FPU_EXTENDED_LITERAL.to_string(),
            message_template:
                "extended floating-point immediate literals are not yet implemented on m68080"
                    .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_FPU_REGISTER_FORM.to_string(),
            message_template: "FSIN currently supports FP-register forms on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_TEX8_NESTED_SHAPE.to_string(),
            message_template: "TEX8.512 source must use (An,(Av,Au)) syntax".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_TEX16_NESTED_SHAPE.to_string(),
            message_template: "TEX16.256 source must use (An,(Av,Au)) syntax".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_TEX_EXTERNAL_D0_SHAPE.to_string(),
            message_template: "TEX24.64 source must use (An,(Av,Au))*D0 syntax".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_TEX_BYTE_SHAPE.to_string(),
            message_template: "TEX.B source must use (An,Av*Dm,Au) syntax".to_string(),
        },
    ]
}
