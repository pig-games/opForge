// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Package compilation adapter for Motorola 68000 scalar semantics.

use package::{
    compile_branch_program, compile_encoding_program, compile_fixup_program,
    compile_operand_record_program, compile_parameterized_encoding_program,
    compile_selector_map_program, compile_selector_suffix_program, compile_state_program,
    compile_structured_encoding_program, compile_value_program, compile_value_program_v2,
    BranchCandidateSpec, BranchProgramSpec, DiagnosticDescriptor, EncodingEndian,
    EncodingFieldSpec, EncodingStep, FixupBase, FixupEncodingStep, FixupRange, FixupTransform,
    ModeSelectorDescriptor, OpcpuCodecError, OperandRecordBaseSource, OperandRecordFieldSource,
    OperandRecordIndirection, OperandRecordOptionalIndexSource, OperandRecordOptionalValueSource,
    OperandRecordProgram, OperandRecordProgramDescriptor, OperandRecordUpdate,
    PortableRelocationKind, RegisterClassProjection, RegisterEncodingDescriptor,
    SelectorProgramDescriptor, SemanticProgramDescriptor, StateArgumentSpec,
    StateCapabilityRuleSpec, StateCapabilitySpec, StateDirectiveSpec, StateKeySpec,
    StateProgramDescriptor, StateProgramSpec, StructuredEncodingStep, UnresolvedValuePolicy,
    ValueConstraint, ValueProgramDescriptor, ValueProgramSource, VmProgramDescriptor,
    MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_TUPLE_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_CALL_ARG_MEMBER_PREFIX, MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX, MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR,
    MODE_SELECTOR_PLAN_DISTINCT_REGISTER_PREFIX, MODE_SELECTOR_PLAN_DUPLICATE_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX, MODE_SELECTOR_PLAN_IMMEDIATE_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX, MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_IDENTITY_SCALE_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_NONIDENTITY_SCALE_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX, MODE_SELECTOR_PLAN_INPUT_SEPARATOR,
    MODE_SELECTOR_PLAN_LITERAL_PREFIX, MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR,
    MODE_SELECTOR_PLAN_MEMBER_PREFIX, MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX,
    MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX, MODE_SELECTOR_PLAN_OUT_OF_RANGE_PREFIX,
    MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX, MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR,
    MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX, MODE_SELECTOR_PLAN_SEMANTIC_BRANCH_PREFIX,
    MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX, MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX,
    MODE_SELECTOR_PLAN_SEMANTIC_SCALAR_PREFIX, MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX,
    MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX, MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR,
    MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX,
    MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX, MODE_SELECTOR_PLAN_VALUE_PROGRAM_SEPARATOR,
    OPERAND_RECORD_VM_VERSION_V1, OPERAND_RECORD_VM_VERSION_V2, OPERAND_RECORD_VM_VERSION_V3,
    SELECTOR_VM_OPCODE_VERSION_V1, SEMANTIC_VM_OPCODE_VERSION_V2, SEMANTIC_VM_OPCODE_VERSION_V3,
    SEMANTIC_VM_OPCODE_VERSION_V4, SEMANTIC_VM_OPCODE_VERSION_V5, SEMANTIC_VM_OPCODE_VERSION_V6,
    SEMANTIC_VM_OP_EMIT_OPERAND, SEMANTIC_VM_OP_EMIT_U8, SEMANTIC_VM_OP_END,
    STATE_VM_OPCODE_VERSION_V1, VALUE_VM_OPCODE_VERSION_V1, VALUE_VM_OPCODE_VERSION_V2,
};
use types::hierarchy::ScopedOwner;

use super::state::{APOLLO_MODE_KEY, CPU_IS_68080_KEY, CPU_LEVEL_KEY, FPU_TARGET_KEY};
use super::M68KFamilyHandler;

const FIXED_INSTRUCTION_PROGRAMS: &[(&str, u16)] = &[
    ("ILLEGAL", 0x4AFC),
    ("NOP", 0x4E71),
    ("RESET", 0x4E70),
    ("RTE", 0x4E73),
    ("RTR", 0x4E77),
    ("RTS", 0x4E75),
    ("TRAPV", 0x4E76),
];

const SINGLE_REGISTER_INSTRUCTION_PROGRAMS: &[(&str, &str, u32, u16)] = &[
    ("SWAP", "enc.swap", 0x4840, 0),
    ("EXT.W", "enc.ext.w", 0x4880, 0),
    ("EXT.L", "enc.ext.l", 0x48c0, 0),
    ("UNLK", "enc.unlk", 0x4e58, 1),
];

const EXG_DATA_DATA: &str = "enc.exg.data-data";
const EXG_ADDRESS_ADDRESS: &str = "enc.exg.address-address";
const EXG_DATA_ADDRESS: &str = "enc.exg.data-address";
const ENCODING_BKPT: &str = "enc.bkpt";
const ENCODING_RTD: &str = "enc.rtd";
const ENCODING_LINK_LONG: &str = "enc.link.long";
const ENCODING_EXTB_LONG: &str = "enc.extb.long";
const ENCODING_RTM_DATA: &str = "enc.rtm.data";
const ENCODING_RTM_ADDRESS: &str = "enc.rtm.address";
const ENCODING_PFLUSH_68030: &str = "enc.pflush.68030";
const ENCODING_PFLUSH_68040: &str = "enc.pflush.68040";
const ENCODING_MOVE_USP_TO_ADDRESS: &str = "enc.move.usp-to-address";
const ENCODING_MOVE_ADDRESS_TO_USP: &str = "enc.move.address-to-usp";
const ENCODING_MOVE_SR_TO_DATA: &str = "enc.move.sr-to-data";
const ENCODING_MOVE_CCR_TO_DATA: &str = "enc.move.ccr-to-data";
const ENCODING_MOVE_DATA_TO_CCR: &str = "enc.move.data-to-ccr";
const ENCODING_MOVE_DATA_TO_SR: &str = "enc.move.data-to-sr";
const ENCODING_MOVE_IMMEDIATE_TO_CCR: &str = "enc.move.immediate-to-ccr";
const ENCODING_MOVE_IMMEDIATE_TO_SR: &str = "enc.move.immediate-to-sr";
const ENCODING_MOVE_ABSOLUTE_WORD_TO_CCR: &str = "enc.move.absolute-word-to-ccr";
const ENCODING_MOVE_CCR_TO_ABSOLUTE_WORD: &str = "enc.move.ccr-to-absolute-word";
const ENCODING_ANDI_TO_CCR: &str = "enc.andi.to-ccr";
const ENCODING_ANDI_TO_SR: &str = "enc.andi.to-sr";
const ENCODING_ORI_TO_CCR: &str = "enc.ori.to-ccr";
const ENCODING_ORI_TO_SR: &str = "enc.ori.to-sr";
const ENCODING_EORI_TO_CCR: &str = "enc.eori.to-ccr";
const ENCODING_EORI_TO_SR: &str = "enc.eori.to-sr";
const ENCODING_MOVEC_CONTROL_TO_DATA: &str = "enc.movec.control-to-data";
const ENCODING_MOVEC_CONTROL_TO_ADDRESS: &str = "enc.movec.control-to-address";
const ENCODING_MOVEC_DATA_TO_CONTROL: &str = "enc.movec.data-to-control";
const ENCODING_MOVEC_ADDRESS_TO_CONTROL: &str = "enc.movec.address-to-control";
const ENCODING_MOVES_INDIRECT: &str = "enc.moves.indirect";
const ENCODING_MOVE_BYTE_INDIRECT_TO_DATA: &str = "enc.move.b.indirect-to-data";
const ENCODING_MOVE_WORD_INDIRECT_TO_DATA: &str = "enc.move.w.indirect-to-data";
const ENCODING_MOVE_LONG_INDIRECT_TO_DATA: &str = "enc.move.l.indirect-to-data";
const ENCODING_MOVE_BYTE_DATA_TO_INDIRECT: &str = "enc.move.b.data-to-indirect";
const ENCODING_MOVE_WORD_DATA_TO_INDIRECT: &str = "enc.move.w.data-to-indirect";
const ENCODING_MOVE_LONG_DATA_TO_INDIRECT: &str = "enc.move.l.data-to-indirect";
const ENCODING_MOVEA_WORD_INDIRECT: &str = "enc.movea.w.indirect";
const ENCODING_MOVEA_LONG_INDIRECT: &str = "enc.movea.l.indirect";
const ENCODING_LEA_INDIRECT: &str = "enc.lea.indirect";
const ENCODING_PEA_INDIRECT: &str = "enc.pea.indirect";
const ENCODING_JMP_INDIRECT: &str = "enc.jmp.indirect";
const ENCODING_JSR_INDIRECT: &str = "enc.jsr.indirect";
const MOVE_UPDATE_SOURCE_PROGRAMS: &[(&str, &str, u32, &str)] = &[
    (
        "MOVE.B",
        "enc.move.b.postincrement-to-data",
        0x1018,
        MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX,
    ),
    (
        "MOVE.W",
        "enc.move.w.postincrement-to-data",
        0x3018,
        MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX,
    ),
    (
        "MOVE.L",
        "enc.move.l.postincrement-to-data",
        0x2018,
        MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX,
    ),
    (
        "MOVE.B",
        "enc.move.b.predecrement-to-data",
        0x1020,
        MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX,
    ),
    (
        "MOVE.W",
        "enc.move.w.predecrement-to-data",
        0x3020,
        MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX,
    ),
    (
        "MOVE.L",
        "enc.move.l.predecrement-to-data",
        0x2020,
        MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX,
    ),
];
const MOVE_UPDATE_DESTINATION_PROGRAMS: &[(&str, &str, u32, &str)] = &[
    (
        "MOVE.B",
        "enc.move.b.data-to-postincrement",
        0x10c0,
        MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX,
    ),
    (
        "MOVE.W",
        "enc.move.w.data-to-postincrement",
        0x30c0,
        MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX,
    ),
    (
        "MOVE.L",
        "enc.move.l.data-to-postincrement",
        0x20c0,
        MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX,
    ),
    (
        "MOVE.B",
        "enc.move.b.data-to-predecrement",
        0x1100,
        MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX,
    ),
    (
        "MOVE.W",
        "enc.move.w.data-to-predecrement",
        0x3100,
        MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX,
    ),
    (
        "MOVE.L",
        "enc.move.l.data-to-predecrement",
        0x2100,
        MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX,
    ),
];
const MOVE_DISPLACEMENT_SOURCE_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVE.B", "enc.move.b.displacement-to-data", 0x1028),
    ("MOVE.W", "enc.move.w.displacement-to-data", 0x3028),
    ("MOVE.L", "enc.move.l.displacement-to-data", 0x2028),
];
const MOVE_DISPLACEMENT_DESTINATION_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVE.B", "enc.move.b.data-to-displacement", 0x1140),
    ("MOVE.W", "enc.move.w.data-to-displacement", 0x3140),
    ("MOVE.L", "enc.move.l.data-to-displacement", 0x2140),
];
const REGISTER_DISPLACEMENT_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVEA.W", "enc.movea.w.displacement", 0x3068),
    ("MOVEA.L", "enc.movea.l.displacement", 0x2068),
    ("LEA", "enc.lea.displacement", 0x41e8),
];
const CONTROL_DISPLACEMENT_PROGRAMS: &[(&str, &str, u32)] = &[
    ("PEA", "enc.pea.displacement", 0x4868),
    ("JMP", "enc.jmp.displacement", 0x4ee8),
    ("JSR", "enc.jsr.displacement", 0x4ea8),
];
const MOVE_INDEXED_SOURCE_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVE.B", "enc.move.b.indexed-word-to-data", 0x1030),
    ("MOVE.W", "enc.move.w.indexed-word-to-data", 0x3030),
    ("MOVE.L", "enc.move.l.indexed-word-to-data", 0x2030),
];
const MOVE_INDEXED_DESTINATION_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVE.B", "enc.move.b.data-to-indexed-word", 0x1180),
    ("MOVE.W", "enc.move.w.data-to-indexed-word", 0x3180),
    ("MOVE.L", "enc.move.l.data-to-indexed-word", 0x2180),
];
const REGISTER_INDEXED_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVEA.W", "enc.movea.w.indexed-word", 0x3070),
    ("MOVEA.L", "enc.movea.l.indexed-word", 0x2070),
    ("LEA", "enc.lea.indexed-word", 0x41f0),
];
const CONTROL_INDEXED_PROGRAMS: &[(&str, &str, u32)] = &[
    ("PEA", "enc.pea.indexed-word", 0x4870),
    ("JMP", "enc.jmp.indexed-word", 0x4ef0),
    ("JSR", "enc.jsr.indexed-word", 0x4eb0),
];
const MOVE_PC_DISPLACEMENT_SOURCE_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVE.B", "enc.move.b.pc-displacement-to-data", 0x103a),
    ("MOVE.W", "enc.move.w.pc-displacement-to-data", 0x303a),
    ("MOVE.L", "enc.move.l.pc-displacement-to-data", 0x203a),
];
const REGISTER_PC_DISPLACEMENT_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVEA.W", "enc.movea.w.pc-displacement", 0x307a),
    ("MOVEA.L", "enc.movea.l.pc-displacement", 0x207a),
    ("LEA", "enc.lea.pc-displacement", 0x41fa),
];
const CONTROL_PC_DISPLACEMENT_PROGRAMS: &[(&str, &str, u32)] = &[
    ("PEA", "enc.pea.pc-displacement", 0x487a),
    ("JMP", "enc.jmp.pc-displacement", 0x4efa),
    ("JSR", "enc.jsr.pc-displacement", 0x4eba),
];
const MOVE_PC_INDEXED_SOURCE_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVE.B", "enc.move.b.pc-indexed-word-to-data", 0x103b),
    ("MOVE.W", "enc.move.w.pc-indexed-word-to-data", 0x303b),
    ("MOVE.L", "enc.move.l.pc-indexed-word-to-data", 0x203b),
];
const REGISTER_PC_INDEXED_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVEA.W", "enc.movea.w.pc-indexed-word", 0x307b),
    ("MOVEA.L", "enc.movea.l.pc-indexed-word", 0x207b),
    ("LEA", "enc.lea.pc-indexed-word", 0x41fb),
];
const CONTROL_PC_INDEXED_PROGRAMS: &[(&str, &str, u32)] = &[
    ("PEA", "enc.pea.pc-indexed-word", 0x487b),
    ("JMP", "enc.jmp.pc-indexed-word", 0x4efb),
    ("JSR", "enc.jsr.pc-indexed-word", 0x4ebb),
];
const MOVE_ABSOLUTE_WORD_SOURCE_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVE.B", "enc.move.b.absolute-word-to-data", 0x1038),
    ("MOVE.W", "enc.move.w.absolute-word-to-data", 0x3038),
    ("MOVE.L", "enc.move.l.absolute-word-to-data", 0x2038),
];
const MOVE_ABSOLUTE_LONG_SOURCE_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVE.B", "enc.move.b.absolute-long-to-data", 0x1039),
    ("MOVE.W", "enc.move.w.absolute-long-to-data", 0x3039),
    ("MOVE.L", "enc.move.l.absolute-long-to-data", 0x2039),
];
const MOVE_ABSOLUTE_WORD_DESTINATION_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVE.B", "enc.move.b.data-to-absolute-word", 0x11c0),
    ("MOVE.W", "enc.move.w.data-to-absolute-word", 0x31c0),
    ("MOVE.L", "enc.move.l.data-to-absolute-word", 0x21c0),
];
const MOVE_ABSOLUTE_LONG_DESTINATION_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVE.B", "enc.move.b.data-to-absolute-long", 0x13c0),
    ("MOVE.W", "enc.move.w.data-to-absolute-long", 0x33c0),
    ("MOVE.L", "enc.move.l.data-to-absolute-long", 0x23c0),
];
const REGISTER_ABSOLUTE_WORD_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVEA.W", "enc.movea.w.absolute-word", 0x3078),
    ("MOVEA.L", "enc.movea.l.absolute-word", 0x2078),
    ("LEA", "enc.lea.absolute-word", 0x41f8),
];
const REGISTER_ABSOLUTE_LONG_PROGRAMS: &[(&str, &str, u32)] = &[
    ("MOVEA.W", "enc.movea.w.absolute-long", 0x3079),
    ("MOVEA.L", "enc.movea.l.absolute-long", 0x2079),
    ("LEA", "enc.lea.absolute-long", 0x41f9),
];
const CONTROL_ABSOLUTE_WORD_PROGRAMS: &[(&str, &str, u32)] = &[
    ("PEA", "enc.pea.absolute-word", 0x4878),
    ("JMP", "enc.jmp.absolute-word", 0x4ef8),
    ("JSR", "enc.jsr.absolute-word", 0x4eb8),
];
const CONTROL_ABSOLUTE_LONG_PROGRAMS: &[(&str, &str, u32)] = &[
    ("PEA", "enc.pea.absolute-long", 0x4879),
    ("JMP", "enc.jmp.absolute-long", 0x4ef9),
    ("JSR", "enc.jsr.absolute-long", 0x4eb9),
];
const MOVE_IMMEDIATE_PROGRAMS: &[(&str, &str, u32, u8, i64, i64)] = &[
    (
        "MOVE.B",
        "enc.move.b.immediate-to-data",
        0x103c,
        2,
        -128,
        255,
    ),
    (
        "MOVE.W",
        "enc.move.w.immediate-to-data",
        0x303c,
        2,
        -32_768,
        65_535,
    ),
    (
        "MOVE.L",
        "enc.move.l.immediate-to-data",
        0x203c,
        4,
        -2_147_483_648,
        4_294_967_295,
    ),
    (
        "MOVEA.W",
        "enc.movea.w.immediate",
        0x307c,
        2,
        -32_768,
        65_535,
    ),
    (
        "MOVEA.L",
        "enc.movea.l.immediate",
        0x207c,
        4,
        -2_147_483_648,
        4_294_967_295,
    ),
];
const MOVE_REGISTER_PROGRAMS: &[(&str, &str, u32, u16, u16)] = &[
    ("MOVE.B", "enc.move.b.data-to-data", 0x1000, 0, 0),
    ("MOVE.W", "enc.move.w.data-to-data", 0x3000, 0, 0),
    ("MOVE.L", "enc.move.l.data-to-data", 0x2000, 0, 0),
    ("MOVE.W", "enc.move.w.address-to-data", 0x3008, 1, 0),
    ("MOVE.L", "enc.move.l.address-to-data", 0x2008, 1, 0),
    ("MOVEA.W", "enc.movea.w.data", 0x3040, 0, 1),
    ("MOVEA.L", "enc.movea.l.data", 0x2040, 0, 1),
    ("MOVEA.W", "enc.movea.w.address", 0x3048, 1, 1),
    ("MOVEA.L", "enc.movea.l.address", 0x2048, 1, 1),
];
const BINARY_REGISTER_PROGRAMS: &[(&str, &str, u32, u8, u8)] = &[
    ("ADD.B", "enc.add.b.data-data", 0xd000, 0, 9),
    ("ADD.W", "enc.add.w.data-data", 0xd040, 0, 9),
    ("ADD.L", "enc.add.l.data-data", 0xd080, 0, 9),
    ("SUB.B", "enc.sub.b.data-data", 0x9000, 0, 9),
    ("SUB.W", "enc.sub.w.data-data", 0x9040, 0, 9),
    ("SUB.L", "enc.sub.l.data-data", 0x9080, 0, 9),
    ("AND.B", "enc.and.b.data-data", 0xc000, 0, 9),
    ("AND.W", "enc.and.w.data-data", 0xc040, 0, 9),
    ("AND.L", "enc.and.l.data-data", 0xc080, 0, 9),
    ("OR.B", "enc.or.b.data-data", 0x8000, 0, 9),
    ("OR.W", "enc.or.w.data-data", 0x8040, 0, 9),
    ("OR.L", "enc.or.l.data-data", 0x8080, 0, 9),
    ("CMP.B", "enc.cmp.b.data-data", 0xb000, 0, 9),
    ("CMP.W", "enc.cmp.w.data-data", 0xb040, 0, 9),
    ("CMP.L", "enc.cmp.l.data-data", 0xb080, 0, 9),
    ("EOR.B", "enc.eor.b.data-data", 0xb100, 9, 0),
    ("EOR.W", "enc.eor.w.data-data", 0xb140, 9, 0),
    ("EOR.L", "enc.eor.l.data-data", 0xb180, 9, 0),
];
const IMMEDIATE_REGISTER_PROGRAMS: &[(&str, &str, &str, u32, u8, i64, i64)] = &[
    ("ORI.B", "OR.B", "enc.ori.b.data", 0x0000, 2, -128, 255),
    (
        "ORI.W",
        "OR.W",
        "enc.ori.w.data",
        0x0040,
        2,
        -32_768,
        65_535,
    ),
    (
        "ORI.L",
        "OR.L",
        "enc.ori.l.data",
        0x0080,
        4,
        i32::MIN as i64,
        u32::MAX as i64,
    ),
    ("ANDI.B", "AND.B", "enc.andi.b.data", 0x0200, 2, -128, 255),
    (
        "ANDI.W",
        "AND.W",
        "enc.andi.w.data",
        0x0240,
        2,
        -32_768,
        65_535,
    ),
    (
        "ANDI.L",
        "AND.L",
        "enc.andi.l.data",
        0x0280,
        4,
        i32::MIN as i64,
        u32::MAX as i64,
    ),
    ("SUBI.B", "SUB.B", "enc.subi.b.data", 0x0400, 2, -128, 255),
    (
        "SUBI.W",
        "SUB.W",
        "enc.subi.w.data",
        0x0440,
        2,
        -32_768,
        65_535,
    ),
    (
        "SUBI.L",
        "SUB.L",
        "enc.subi.l.data",
        0x0480,
        4,
        i32::MIN as i64,
        u32::MAX as i64,
    ),
    ("ADDI.B", "ADD.B", "enc.addi.b.data", 0x0600, 2, -128, 255),
    (
        "ADDI.W",
        "ADD.W",
        "enc.addi.w.data",
        0x0640,
        2,
        -32_768,
        65_535,
    ),
    (
        "ADDI.L",
        "ADD.L",
        "enc.addi.l.data",
        0x0680,
        4,
        i32::MIN as i64,
        u32::MAX as i64,
    ),
    ("EORI.B", "EOR.B", "enc.eori.b.data", 0x0a00, 2, -128, 255),
    (
        "EORI.W",
        "EOR.W",
        "enc.eori.w.data",
        0x0a40,
        2,
        -32_768,
        65_535,
    ),
    (
        "EORI.L",
        "EOR.L",
        "enc.eori.l.data",
        0x0a80,
        4,
        i32::MIN as i64,
        u32::MAX as i64,
    ),
    ("CMPI.B", "CMP.B", "enc.cmpi.b.data", 0x0c00, 2, -128, 255),
    (
        "CMPI.W",
        "CMP.W",
        "enc.cmpi.w.data",
        0x0c40,
        2,
        -32_768,
        65_535,
    ),
    (
        "CMPI.L",
        "CMP.L",
        "enc.cmpi.l.data",
        0x0c80,
        4,
        i32::MIN as i64,
        u32::MAX as i64,
    ),
];
const UNARY_REGISTER_PROGRAMS: &[(&str, &str, u32)] = &[
    ("NEGX.B", "enc.negx.b.data", 0x4000),
    ("NEGX.W", "enc.negx.w.data", 0x4040),
    ("NEGX.L", "enc.negx.l.data", 0x4080),
    ("CLR.B", "enc.clr.b.data", 0x4200),
    ("CLR.W", "enc.clr.w.data", 0x4240),
    ("CLR.L", "enc.clr.l.data", 0x4280),
    ("NEG.B", "enc.neg.b.data", 0x4400),
    ("NEG.W", "enc.neg.w.data", 0x4440),
    ("NEG.L", "enc.neg.l.data", 0x4480),
    ("NOT.B", "enc.not.b.data", 0x4600),
    ("NOT.W", "enc.not.w.data", 0x4640),
    ("NOT.L", "enc.not.l.data", 0x4680),
    ("TST.B", "enc.tst.b.data", 0x4a00),
    ("TST.W", "enc.tst.w.data", 0x4a40),
    ("TST.L", "enc.tst.l.data", 0x4a80),
    ("NBCD", "enc.nbcd.data", 0x4800),
    ("TAS", "enc.tas.data", 0x4ac0),
    ("ST", "enc.st.data", 0x50c0),
    ("SF", "enc.sf.data", 0x51c0),
    ("SHI", "enc.shi.data", 0x52c0),
    ("SLS", "enc.sls.data", 0x53c0),
    ("SCC", "enc.scc.data", 0x54c0),
    ("SHS", "enc.shs.data", 0x54c0),
    ("SCS", "enc.scs.data", 0x55c0),
    ("SLO", "enc.slo.data", 0x55c0),
    ("SNE", "enc.sne.data", 0x56c0),
    ("SEQ", "enc.seq.data", 0x57c0),
    ("SVC", "enc.svc.data", 0x58c0),
    ("SVS", "enc.svs.data", 0x59c0),
    ("SPL", "enc.spl.data", 0x5ac0),
    ("SMI", "enc.smi.data", 0x5bc0),
    ("SGE", "enc.sge.data", 0x5cc0),
    ("SLT", "enc.slt.data", 0x5dc0),
    ("SGT", "enc.sgt.data", 0x5ec0),
    ("SLE", "enc.sle.data", 0x5fc0),
];
const EA_TO_DATA_ALU_PROGRAMS: &[(&str, u32)] = &[
    ("ADD.B", 0xd000),
    ("ADD.W", 0xd040),
    ("ADD.L", 0xd080),
    ("SUB.B", 0x9000),
    ("SUB.W", 0x9040),
    ("SUB.L", 0x9080),
    ("AND.B", 0xc000),
    ("AND.W", 0xc040),
    ("AND.L", 0xc080),
    ("OR.B", 0x8000),
    ("OR.W", 0x8040),
    ("OR.L", 0x8080),
    ("CMP.B", 0xb000),
    ("CMP.W", 0xb040),
    ("CMP.L", 0xb080),
];
const DATA_TO_EA_ALU_PROGRAMS: &[(&str, u32)] = &[
    ("ADD.B", 0xd100),
    ("ADD.W", 0xd140),
    ("ADD.L", 0xd180),
    ("SUB.B", 0x9100),
    ("SUB.W", 0x9140),
    ("SUB.L", 0x9180),
    ("AND.B", 0xc100),
    ("AND.W", 0xc140),
    ("AND.L", 0xc180),
    ("OR.B", 0x8100),
    ("OR.W", 0x8140),
    ("OR.L", 0x8180),
    ("EOR.B", 0xb100),
    ("EOR.W", 0xb140),
    ("EOR.L", 0xb180),
];
const REGISTER_COUNT_SHIFT_PROGRAMS: &[(&str, u32)] = &[
    ("ASR.B", 0xe020),
    ("ASR.W", 0xe060),
    ("ASR.L", 0xe0a0),
    ("ASL.B", 0xe120),
    ("ASL.W", 0xe160),
    ("ASL.L", 0xe1a0),
    ("LSR.B", 0xe028),
    ("LSR.W", 0xe068),
    ("LSR.L", 0xe0a8),
    ("LSL.B", 0xe128),
    ("LSL.W", 0xe168),
    ("LSL.L", 0xe1a8),
    ("ROXR.B", 0xe030),
    ("ROXR.W", 0xe070),
    ("ROXR.L", 0xe0b0),
    ("ROXL.B", 0xe130),
    ("ROXL.W", 0xe170),
    ("ROXL.L", 0xe1b0),
    ("ROR.B", 0xe038),
    ("ROR.W", 0xe078),
    ("ROR.L", 0xe0b8),
    ("ROL.B", 0xe138),
    ("ROL.W", 0xe178),
    ("ROL.L", 0xe1b8),
];
const IMMEDIATE_COUNT_SHIFT_PROGRAMS: &[(&str, u32)] = &[
    ("ASR.B", 0xe000),
    ("ASR.W", 0xe040),
    ("ASR.L", 0xe080),
    ("ASL.B", 0xe100),
    ("ASL.W", 0xe140),
    ("ASL.L", 0xe180),
    ("LSR.B", 0xe008),
    ("LSR.W", 0xe048),
    ("LSR.L", 0xe088),
    ("LSL.B", 0xe108),
    ("LSL.W", 0xe148),
    ("LSL.L", 0xe188),
    ("ROXR.B", 0xe010),
    ("ROXR.W", 0xe050),
    ("ROXR.L", 0xe090),
    ("ROXL.B", 0xe110),
    ("ROXL.W", 0xe150),
    ("ROXL.L", 0xe190),
    ("ROR.B", 0xe018),
    ("ROR.W", 0xe058),
    ("ROR.L", 0xe098),
    ("ROL.B", 0xe118),
    ("ROL.W", 0xe158),
    ("ROL.L", 0xe198),
];
const MEMORY_SHIFT_PROGRAMS: &[(&str, u32)] = &[
    ("ASR", 0xe0c0),
    ("ASR.W", 0xe0c0),
    ("ASL", 0xe1c0),
    ("ASL.W", 0xe1c0),
    ("LSR", 0xe2c0),
    ("LSR.W", 0xe2c0),
    ("LSL", 0xe3c0),
    ("LSL.W", 0xe3c0),
    ("ROXR", 0xe4c0),
    ("ROXR.W", 0xe4c0),
    ("ROXL", 0xe5c0),
    ("ROXL.W", 0xe5c0),
    ("ROR", 0xe6c0),
    ("ROR.W", 0xe6c0),
    ("ROL", 0xe7c0),
    ("ROL.W", 0xe7c0),
];
const DYNAMIC_BIT_PROGRAMS: &[(&str, u32)] = &[
    ("BTST", 0x0100),
    ("BCHG", 0x0140),
    ("BCLR", 0x0180),
    ("BSET", 0x01c0),
];
const STATIC_BIT_PROGRAMS: &[(&str, u32)] = &[
    ("BTST", 0x0800),
    ("BCHG", 0x0840),
    ("BCLR", 0x0880),
    ("BSET", 0x08c0),
];
const QUICK_PROGRAMS: &[(&str, u32, bool)] = &[
    ("ADDQ.B", 0x5000, false),
    ("ADDQ.W", 0x5040, true),
    ("ADDQ.L", 0x5080, true),
    ("SUBQ.B", 0x5100, false),
    ("SUBQ.W", 0x5140, true),
    ("SUBQ.L", 0x5180, true),
];
const BRANCH_OPCODE_BASES: &[(&str, u8)] = &[
    ("BRA", 0x60),
    ("BSR", 0x61),
    ("BHI", 0x62),
    ("BLS", 0x63),
    ("BCC", 0x64),
    ("BHS", 0x64),
    ("BCS", 0x65),
    ("BLO", 0x65),
    ("BNE", 0x66),
    ("BEQ", 0x67),
    ("BVC", 0x68),
    ("BVS", 0x69),
    ("BPL", 0x6a),
    ("BMI", 0x6b),
    ("BGE", 0x6c),
    ("BLT", 0x6d),
    ("BGT", 0x6e),
    ("BLE", 0x6f),
];
const TRAPCC_CONDITIONS: &[(&str, u32)] = &[
    ("TRAPT", 0),
    ("TRAPF", 1),
    ("TRAPHI", 2),
    ("TRAPLS", 3),
    ("TRAPCC", 4),
    ("TRAPHS", 4),
    ("TRAPCS", 5),
    ("TRAPLO", 5),
    ("TRAPNE", 6),
    ("TRAPEQ", 7),
    ("TRAPVC", 8),
    ("TRAPVS", 9),
    ("TRAPPL", 10),
    ("TRAPMI", 11),
    ("TRAPGE", 12),
    ("TRAPLT", 13),
    ("TRAPGT", 14),
    ("TRAPLE", 15),
];
const DBCC_OPCODE_BASES: &[(&str, u32)] = &[
    ("DBT", 0x50c8),
    ("DBF", 0x51c8),
    ("DBRA", 0x51c8),
    ("DBHI", 0x52c8),
    ("DBLS", 0x53c8),
    ("DBCC", 0x54c8),
    ("DBHS", 0x54c8),
    ("DBCS", 0x55c8),
    ("DBLO", 0x55c8),
    ("DBNE", 0x56c8),
    ("DBEQ", 0x57c8),
    ("DBVC", 0x58c8),
    ("DBVS", 0x59c8),
    ("DBPL", 0x5ac8),
    ("DBMI", 0x5bc8),
    ("DBGE", 0x5cc8),
    ("DBLT", 0x5dc8),
    ("DBGT", 0x5ec8),
    ("DBLE", 0x5fc8),
];
const EXTEND_REGISTER_BASES: &[(&str, u32)] = &[
    ("ADDX.B", 0xd100),
    ("ADDX.W", 0xd140),
    ("ADDX.L", 0xd180),
    ("SUBX.B", 0x9100),
    ("SUBX.W", 0x9140),
    ("SUBX.L", 0x9180),
    ("ABCD", 0xc100),
    ("SBCD", 0x8100),
];
const CMPM_BASES: &[(&str, u32)] = &[("CMPM.B", 0xb108), ("CMPM.W", 0xb148), ("CMPM.L", 0xb188)];
const ADDRESS_ALU_BASES: &[(&str, u32)] = &[
    ("ADDA.W", 0xd0c0),
    ("ADDA.L", 0xd1c0),
    ("SUBA.W", 0x90c0),
    ("SUBA.L", 0x91c0),
    ("CMPA.W", 0xb0c0),
    ("CMPA.L", 0xb1c0),
];
const WORD_SOURCE_TO_DATA_BASES: &[(&str, u32)] = &[
    ("CHK", 0x4180),
    ("CHK.W", 0x4180),
    ("MULU", 0xc0c0),
    ("MULU.W", 0xc0c0),
    ("MULS", 0xc1c0),
    ("MULS.W", 0xc1c0),
    ("DIVU", 0x80c0),
    ("DIVU.W", 0x80c0),
    ("DIVS", 0x81c0),
    ("DIVS.W", 0x81c0),
];
const PARAM_FIELDS_9_0: &str = "enc.template.fields-9-0";
const PARAM_FIELDS_0_9: &str = "enc.template.fields-0-9";
const PARAM_FIELDS_12_0: &str = "enc.template.fields-12-0";
const PARAM_FIELDS_6_0: &str = "enc.template.fields-6-0";
const PARAM_FIELDS_12_6_0: &str = "enc.template.fields-12-6-0";
const PARAM_FULL_EXTENSION: &str = "enc.template.full-extension";
const PARAM_INDEX_EXTENSION: &str = "enc.template.index-extension";
const PARAM_MOVES_EXTENSION: &str = "enc.template.moves-extension";
const PARAM_FIELDS_10_7_0: &str = "enc.template.fields-10-7-0";
const PARAM_FIELDS_10_7_0_3: &str = "enc.template.fields-10-7-0-3";
const PARAM_FIELD_0_8: &str = "enc.template.field-0-8";
const PARAM_FIELDS_7_0_7: &str = "enc.template.fields-7-0-7";
const PARAM_BITFIELD_EXTENSION: &str = "enc.template.bitfield-extension";
const PARAM_FIELD_0: &str = "enc.template.field-0";
const PARAM_FIELD_9: &str = "enc.template.field-9";
const PARAM_DISPLACEMENT_0: &str = "enc.template.displacement-0";
const PARAM_INDEXED_0: &str = "enc.template.indexed-0";
const PARAM_FIXED_EXTENSION_WORD: &str = "enc.template.fixed-extension-word";
const PARAM_FIXED_EXTENSION_LONG: &str = "enc.template.fixed-extension-long";
const PARAM_STATIC_FIELD_0: &str = "enc.template.static-field-0";
const PARAM_STATIC_DISPLACEMENT_0: &str = "enc.template.static-displacement-0";
const PARAM_STATIC_INDEXED_0: &str = "enc.template.static-indexed-0";
const PARAM_STATIC_EXTENSION_WORD: &str = "enc.template.static-extension-word";
const PARAM_STATIC_EXTENSION_LONG: &str = "enc.template.static-extension-long";
const PARAM_STATIC_PC_DISPLACEMENT: &str = "enc.template.static-pc-displacement";
const PARAM_STATIC_PC_INDEXED: &str = "enc.template.static-pc-indexed";
const PARAM_IMMEDIATE_BYTE_FIELD_0: &str = "enc.template.immediate-byte-field-0";
const PARAM_IMMEDIATE_WORD_FIELD_0: &str = "enc.template.immediate-word-field-0";
const PARAM_IMMEDIATE_LONG_FIELD_0: &str = "enc.template.immediate-long-field-0";
const PARAM_IMMEDIATE_BYTE_FIELD_9: &str = "enc.template.immediate-byte-field-9";
const PARAM_IMMEDIATE_WORD_FIELD_9: &str = "enc.template.immediate-word-field-9";
const PARAM_IMMEDIATE_LONG_FIELD_9: &str = "enc.template.immediate-long-field-9";
const PARAM_DISPLACEMENT_9_0: &str = "enc.template.displacement-9-0";
const PARAM_INDEXED_9_0: &str = "enc.template.indexed-9-0";
const PARAM_DISPLACEMENT_0_9: &str = "enc.template.displacement-0-9";
const PARAM_INDEXED_0_9: &str = "enc.template.indexed-0-9";
const PARAM_INDEXED_LONG_0_9: &str = "enc.template.indexed-long-0-9";
const PARAM_INDEX_PREFIX: &str = "enc.template.index-prefix";
const PARAM_FIELD_9_DISPLACEMENT: &str = "enc.template.field-9-displacement";
const PARAM_FIELD_9_PC_INDEXED: &str = "enc.template.field-9-pc-indexed";
const PARAM_EXTENSION_WORD_9: &str = "enc.template.extension-word-9";
const PARAM_EXTENSION_LONG_9: &str = "enc.template.extension-long-9";
const PARAM_SCALAR_WORD: &str = "enc.template.scalar-word";
const PARAM_SCALAR_LONG: &str = "enc.template.scalar-long";
const PARAM_SCALAR_BYTE: &str = "enc.template.scalar-byte";

pub const M68010_CONTROL_REGISTER_IDS: &[&str] = &["SFC", "DFC", "VBR"];
pub const M68020_CONTROL_REGISTER_IDS: &[&str] =
    &["SFC", "DFC", "CACR", "CAAR", "MSP", "ISP", "VBR"];
pub const M68040_CONTROL_REGISTER_IDS: &[&str] = &[
    "SFC", "DFC", "CACR", "CAAR", "MSP", "ISP", "VBR", "TC", "ITT0", "ITT1", "DTT0", "DTT1",
    "MMUSR", "URP", "SRP",
];

/// Mnemonics with at least one package-owned form in the current cutover slice.
pub fn instruction_form_mnemonics() -> Vec<String> {
    let mut forms = std::collections::BTreeSet::new();
    forms.extend(
        FIXED_INSTRUCTION_PROGRAMS
            .iter()
            .map(|(mnemonic, _)| (*mnemonic).to_string()),
    );
    forms.extend(
        [
            "TRAP", "MOVEQ", "EXG", "STOP", "LINK", "LINK.W", "MOVE", "ANDI", "ORI", "EORI",
            "MOVE.B", "MOVE.W", "MOVE.L", "MOVE.S", "MOVEA.W", "MOVEA.L", "MOVEM.W", "MOVEM.L",
            "MOVEP.W", "MOVEP.L", "CMPA.B", "CHK.L", "ABCD.B", "ROXL.L", "LEA", "PEA", "JMP",
            "JSR",
        ]
        .into_iter()
        .map(str::to_string),
    );
    forms.extend(
        SINGLE_REGISTER_INSTRUCTION_PROGRAMS
            .iter()
            .map(|(mnemonic, _, _, _)| (*mnemonic).to_string()),
    );
    forms.extend(
        BINARY_REGISTER_PROGRAMS
            .iter()
            .map(|(mnemonic, _, _, _, _)| (*mnemonic).to_string()),
    );
    for (canonical, alias, _, _, _, _, _) in IMMEDIATE_REGISTER_PROGRAMS {
        forms.insert((*canonical).to_string());
        forms.insert((*alias).to_string());
    }
    forms.extend(
        UNARY_REGISTER_PROGRAMS
            .iter()
            .map(|(mnemonic, _, _)| (*mnemonic).to_string()),
    );
    forms.extend(
        REGISTER_COUNT_SHIFT_PROGRAMS
            .iter()
            .map(|(mnemonic, _)| (*mnemonic).to_string()),
    );
    forms.extend(
        MEMORY_SHIFT_PROGRAMS
            .iter()
            .map(|(mnemonic, _)| (*mnemonic).to_string()),
    );
    forms.extend(
        DYNAMIC_BIT_PROGRAMS
            .iter()
            .map(|(mnemonic, _)| (*mnemonic).to_string()),
    );
    forms.extend(
        QUICK_PROGRAMS
            .iter()
            .map(|(mnemonic, _, _)| (*mnemonic).to_string()),
    );
    forms.extend(BRANCH_OPCODE_BASES.iter().flat_map(|(mnemonic, _)| {
        [
            (*mnemonic).to_string(),
            format!("{mnemonic}.B"),
            format!("{mnemonic}.S"),
            format!("{mnemonic}.W"),
        ]
    }));
    forms.extend(
        DBCC_OPCODE_BASES
            .iter()
            .map(|(mnemonic, _)| (*mnemonic).to_string()),
    );
    forms.extend(
        EXTEND_REGISTER_BASES
            .iter()
            .chain(CMPM_BASES)
            .chain(ADDRESS_ALU_BASES)
            .chain(WORD_SOURCE_TO_DATA_BASES)
            .map(|(mnemonic, _)| (*mnemonic).to_string()),
    );
    forms.into_iter().collect()
}

/// Portable encoding identities for registers consumed by package programs.
pub fn register_encodings() -> Vec<RegisterEncodingDescriptor> {
    (0_u16..=7)
        .map(|index| RegisterEncodingDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            id: format!("D{index}"),
            class: 0,
            index,
        })
        .chain((0_u16..=7).map(|index| RegisterEncodingDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            id: format!("A{index}"),
            class: 1,
            index,
        }))
        .chain(std::iter::once(RegisterEncodingDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            id: "SP".to_string(),
            class: 1,
            index: 7,
        }))
        .chain(std::iter::once(RegisterEncodingDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            id: "PC".to_string(),
            class: 8,
            index: 0,
        }))
        .chain((0_u16..=7).map(|index| RegisterEncodingDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            id: format!("FP{index}"),
            class: 2,
            index,
        }))
        .chain(
            [("FPCR", 0_u16), ("FPSR", 1_u16), ("FPIAR", 2_u16)]
                .into_iter()
                .map(|(id, index)| RegisterEncodingDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    id: id.to_string(),
                    class: 3,
                    index,
                }),
        )
        .chain(
            [("USP", 4_u16), ("SR", 5_u16), ("CCR", 6_u16)]
                .into_iter()
                .map(|(id, class)| RegisterEncodingDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    id: id.to_string(),
                    class,
                    index: 0,
                }),
        )
        .collect()
}

/// Compile fixed instructions into the portable instruction VM table.
pub fn instruction_programs() -> Vec<VmProgramDescriptor> {
    let owner = ScopedOwner::Family("motorola68000".to_string());
    let mut programs = FIXED_INSTRUCTION_PROGRAMS
        .iter()
        .map(|(mnemonic, opcode)| VmProgramDescriptor {
            owner: owner.clone(),
            mnemonic: (*mnemonic).to_string(),
            mode_key: "implied".to_string(),
            program: vec![
                SEMANTIC_VM_OP_EMIT_U8,
                (opcode >> 8) as u8,
                SEMANTIC_VM_OP_EMIT_U8,
                *opcode as u8,
                SEMANTIC_VM_OP_END,
            ],
        })
        .collect::<Vec<_>>();
    let fixed = FIXED_INSTRUCTION_PROGRAMS
        .iter()
        .map(|(mnemonic, _)| *mnemonic)
        .collect::<std::collections::BTreeSet<_>>();
    programs.extend(
        instruction_form_mnemonics()
            .into_iter()
            .filter(|mnemonic| !fixed.contains(mnemonic.as_str()))
            .map(|mnemonic| semantic_instruction_program(&owner, mnemonic.as_str())),
    );
    programs
}

/// CPU-scoped forms introduced by the 68010 and inherited by later profiles.
pub fn m68010_instruction_form_mnemonics() -> Vec<String> {
    ["BKPT", "RTD", "MOVEC", "MOVES.W", "MOVES.L"]
        .into_iter()
        .map(str::to_string)
        .collect()
}

/// CPU-scoped control-register identities inherited from the 68010 profile.
pub fn m68010_register_encodings(cpu_id: &str) -> Vec<RegisterEncodingDescriptor> {
    [("SFC", 0x000_u16), ("DFC", 0x001), ("VBR", 0x801)]
        .into_iter()
        .map(|(id, index)| RegisterEncodingDescriptor {
            owner: ScopedOwner::Cpu(cpu_id.to_string()),
            id: id.to_string(),
            class: 7,
            index,
        })
        .collect()
}

pub fn m68020_register_encodings(cpu_id: &str) -> Vec<RegisterEncodingDescriptor> {
    let mut registers = vec![
        ("SFC", 0x000_u16),
        ("DFC", 0x001),
        ("CACR", 0x002),
        ("CAAR", 0x802),
        ("MSP", 0x803),
        ("ISP", 0x804),
        ("VBR", 0x801),
    ];
    if cpu_id == "m68040" {
        registers.extend([
            ("TC", 0x003),
            ("ITT0", 0x004),
            ("ITT1", 0x005),
            ("DTT0", 0x006),
            ("DTT1", 0x007),
            ("MMUSR", 0x805),
            ("URP", 0x806),
            ("SRP", 0x807),
        ]);
    }
    registers
        .into_iter()
        .filter(|(id, _)| cpu_id != "m68040" || *id != "CAAR")
        .map(|(id, index)| RegisterEncodingDescriptor {
            owner: ScopedOwner::Cpu(cpu_id.to_string()),
            id: id.to_string(),
            class: 7,
            index,
        })
        .collect()
}

/// CPU-scoped forms introduced by the 68020 and inherited by later profiles.
pub fn m68020_instruction_form_mnemonics(include_rtm: bool) -> Vec<String> {
    let mut forms = vec![
        "LINK.L".to_string(),
        "EXTB.L".to_string(),
        "MULU.L".to_string(),
        "MULS.L".to_string(),
        "CAS.B".to_string(),
        "CAS.W".to_string(),
        "CAS.L".to_string(),
        "CAS2.B".to_string(),
        "CAS2.W".to_string(),
        "CAS2.L".to_string(),
        "CHK2.B".to_string(),
        "CHK2.W".to_string(),
        "CHK2.L".to_string(),
        "CMP2.B".to_string(),
        "CMP2.W".to_string(),
        "CMP2.L".to_string(),
        "BFTST".to_string(),
        "BFEXTU".to_string(),
        "BFINS".to_string(),
        "PACK".to_string(),
        "UNPK".to_string(),
        "CALLM".to_string(),
        "DIVS.L".to_string(),
        "DIVU.L".to_string(),
        "DIVSL.L".to_string(),
        "DIVUL.L".to_string(),
        "ADDIW.L".to_string(),
        "CMPIW.L".to_string(),
        "MOVIW.L".to_string(),
    ];
    forms.extend(
        BRANCH_OPCODE_BASES
            .iter()
            .map(|(mnemonic, _)| format!("{mnemonic}.L")),
    );
    for (mnemonic, _) in TRAPCC_CONDITIONS {
        forms.push((*mnemonic).to_string());
        forms.push(format!("{mnemonic}.W"));
        forms.push(format!("{mnemonic}.L"));
    }
    if include_rtm {
        forms.push("RTM".to_string());
    }
    forms.extend(
        [
            "FMOVE",
            "FADD",
            "FSUB",
            "FMUL",
            "FDIV",
            "FSQRT",
            "FABS",
            "FNEG",
            "FCMP",
            "FTST",
            "FINT",
            "FINTRZ",
            "FMOVE.B",
            "FMOVE.W",
            "FMOVE.L",
            "FTST.W",
            "FSIN",
            "FTAN",
            "FASIN",
            "FACOS",
            "FATAN",
            "FSINH",
            "FCOSH",
            "FTANH",
            "FATANH",
            "FETOX",
            "FETOXM1",
            "FTENTOX",
            "FTWOTOX",
            "FLOGN",
            "FLOGNP1",
            "FLOG10",
            "FLOG2",
            "FGETEXP",
            "FGETMAN",
            "FSCALE",
            "FMOD",
            "FREM",
            "FCOS.W",
            "FSINCOS",
            "FMOVEM",
            "FMOVE.S",
            "FMOVE.D",
            "FMOVE.X",
            "FMOVE.P",
            "FADD.B",
            "FADD.W",
            "FADD.L",
            "FADD.S",
            "FADD.D",
            "FADD.X",
            "FADD.P",
            "FMOVEM.L",
            "FBEQ",
            "FDBNE",
            "FSNE",
            "FTRAPGT.W",
            "FSAVE",
            "FRESTORE",
            "FNOP",
            "FMOVECR",
            "FSGLDIV",
            "FSGLMUL",
        ]
        .into_iter()
        .map(str::to_string),
    );
    forms
}

fn semantic_instruction_program(owner: &ScopedOwner, mnemonic: &str) -> VmProgramDescriptor {
    VmProgramDescriptor {
        owner: owner.clone(),
        mnemonic: mnemonic.to_string(),
        mode_key: "semantic".to_string(),
        program: vec![SEMANTIC_VM_OP_EMIT_OPERAND, 0, SEMANTIC_VM_OP_END],
    }
}

/// Package tables for the 68010 instruction delta.
pub fn m68010_instruction_programs(cpu_id: &str) -> Vec<VmProgramDescriptor> {
    let owner = ScopedOwner::Cpu(cpu_id.to_string());
    ["BKPT", "RTD", "MOVEC", "MOVES.W", "MOVES.L"]
        .into_iter()
        .map(|mnemonic| semantic_instruction_program(&owner, mnemonic))
        .collect()
}

/// Package tables for the scalar/register subset of the 68020 instruction delta.
pub fn m68020_instruction_programs(cpu_id: &str, include_rtm: bool) -> Vec<VmProgramDescriptor> {
    let owner = ScopedOwner::Cpu(cpu_id.to_string());
    let mut programs = m68020_instruction_form_mnemonics(include_rtm)
        .into_iter()
        .map(|mnemonic| semantic_instruction_program(&owner, mnemonic.as_str()))
        .collect::<Vec<_>>();
    if matches!(cpu_id, "m68030" | "m68040") {
        programs.push(semantic_instruction_program(&owner, "PFLUSH"));
    }
    if cpu_id == "m68040" {
        programs.push(semantic_instruction_program(&owner, "MOVE16"));
    }
    programs
}

fn baseline_rejection_selectors() -> Vec<ModeSelectorDescriptor> {
    let family = || ScopedOwner::Family("motorola68000".to_string());
    let mut selectors = vec![
        ModeSelectorDescriptor {
            owner: family(),
            mnemonic: "MOVE.S".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_TRAP_VECTOR}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16,reg0.class0,reg1.class0{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_MOVE_UNSUPPORTED_SIZE}"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: family(),
            mnemonic: "STOP".to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_STOP_OPERAND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: family(),
            mnemonic: "CMPA.B".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_UNSUPPORTED_BYTE_SIZE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class1"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
    ];
    for (priority, cpu_id) in ["m68000", "m68010"].into_iter().enumerate() {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Cpu(cpu_id.to_string()),
            mnemonic: "PFLUSH".to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_PFLUSH_BASELINE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_IMMEDIATE_PREFIX}1"
            ),
            priority: 1300 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (owner, mnemonic, shape, inputs, diagnostic)) in [
        (
            ScopedOwner::Cpu("m68000".to_string()),
            "MOVE",
            "register_register",
            "reg0.class6,reg1.class0",
            DIAG_MOVE_FROM_CCR,
        ),
        (
            family(),
            "MOVE",
            "register_register",
            "reg0.class0,reg1.class4",
            DIAG_MOVE_USP_SOURCE,
        ),
        (
            family(),
            "ANDI.W",
            "immediate_register",
            "expr0,reg1.class6",
            DIAG_ANDI_WORD_CCR,
        ),
        (
            family(),
            "MOVE",
            "register_register",
            "reg0.class1,reg1.class5",
            DIAG_MOVE_TO_SR_SOURCE,
        ),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner,
            mnemonic: mnemonic.to_string(),
            shape_key: shape.to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, shape, inputs, diagnostic)) in [
        (
            "BTST",
            "register_register",
            "reg0.class1,reg1.class0".to_string(),
            DIAG_BIT_NUMBER,
        ),
        (
            "BTST",
            "immediate_direct",
            format!("expr0,{MODE_SELECTOR_PLAN_IMMEDIATE_PREFIX}1"),
            DIAG_INVALID_DESTINATION,
        ),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: family(),
            mnemonic: mnemonic.to_string(),
            shape_key: shape.to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
            ),
            priority: 900 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, mnemonic) in ["BCHG", "BCLR", "BSET"].into_iter().enumerate() {
        selectors.push(ModeSelectorDescriptor {
            owner: family(),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_INVALID_DESTINATION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2"
            ),
            priority: 900 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, shape, inputs, diagnostic)) in [
        ("CHK.L", "register_register", "reg0.class0,reg1.class0".to_string(), DIAG_UNSUPPORTED_LONG_SIZE),
        ("MULU", "register_register", "reg0.class1,reg1.class0".to_string(), DIAG_INVALID_SOURCE),
        ("DIVU", "register_register", "reg0.class0,reg1.class1".to_string(), DIAG_DESTINATION_DATA_REGISTER),
        (
            "ADDX.W",
            "register_direct",
            format!("reg0.class0,{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            DIAG_EXTEND_SHAPE,
        ),
        ("ABCD.B", "register_register", "reg0.class0,reg1.class0".to_string(), DIAG_NO_SIZE_SUFFIX),
        ("CMPM.W", "register_register", "reg0.class1,reg1.class1".to_string(), DIAG_CMPM_SHAPE),
        ("ROXL.L", "direct", format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1"), DIAG_MEMORY_LONG_SIZE),
        ("ROXR.W", "immediate_direct", format!("expr0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"), DIAG_DESTINATION_DATA_REGISTER),
        (
            "ROR",
            "direct",
            format!("{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2"),
            DIAG_INVALID_DESTINATION,
        ),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: family(),
            mnemonic: mnemonic.to_string(),
            shape_key: shape.to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
            ),
            priority: 920 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, shape, inputs, diagnostic)) in [
        (
            "MOVEM.W",
            "direct_direct",
            format!(
                "{MODE_SELECTOR_PLAN_DUPLICATE_REGISTER_PREFIX}0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"
            ),
            DIAG_DUPLICATE_REGISTER,
        ),
        (
            "MOVEM.B",
            "register_direct",
            format!("reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
            DIAG_UNSUPPORTED_BYTE_SIZE,
        ),
        (
            "MOVEP.W",
            "register_direct",
            format!("reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
            DIAG_MOVEP_ADDRESSING,
        ),
        (
            "MOVEP.B",
            "register_direct",
            format!(
                "reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2"
            ),
            DIAG_UNSUPPORTED_BYTE_SIZE,
        ),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: family(),
            mnemonic: mnemonic.to_string(),
            shape_key: shape.to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
            ),
            priority: 940 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, shape, inputs, diagnostic)) in [
        (
            "MOVE.W",
            "register_register",
            "reg0.class0,reg1.class1".to_string(),
            DIAG_INVALID_DESTINATION_FORM,
        ),
        (
            "LEA",
            "immediate_register",
            "expr0,reg1.class1".to_string(),
            DIAG_INVALID_SOURCE,
        ),
        ("JMP", "register", "reg0.class0".to_string(), DIAG_INVALID_SOURCE),
        (
            "ADD.W",
            "register_direct",
            format!(
                "reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2"
            ),
            DIAG_INVALID_DESTINATION_FORM,
        ),
        (
            "SUB.W",
            "register_immediate",
            "reg0.class0,expr1".to_string(),
            DIAG_INVALID_DESTINATION_FORM,
        ),
        (
            "AND.W",
            "register_register",
            "reg0.class0,reg1.class1".to_string(),
            DIAG_INVALID_DESTINATION_FORM,
        ),
        (
            "OR.W",
            "register_direct",
            format!(
                "reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3"
            ),
            DIAG_INVALID_DESTINATION_FORM,
        ),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: family(),
            mnemonic: mnemonic.to_string(),
            shape_key: shape.to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
            ),
            priority: 960 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, base_class, destination_class, diagnostic)) in [
        ("LEA", 1_u16, 1_u16, DIAG_FULL_EXTENSION_DISPLACEMENT),
        ("MOVE.W", 8_u16, 0_u16, DIAG_INVALID_DISPLACEMENT_BASE),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: family(),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class{base_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_NONIDENTITY_SCALE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3,reg1.class{destination_class}"
            ),
            priority: 980 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: family(),
        mnemonic: "MOVE.W".to_string(),
        shape_key: "direct_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_INVALID_DISPLACEMENT_BASE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_NONIDENTITY_SCALE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,reg1.class0"
        ),
        priority: 982,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: family(),
        mnemonic: "MOVE.W".to_string(),
        shape_key: "direct_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_FULL_EXTENSION_UNSUPPORTED}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_NONIDENTITY_SCALE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}any,reg1.class0"
        ),
        priority: 970,
        unstable_widen: false,
        width_rank: 0,
    });
    for (priority, (mnemonic, shape, inputs, diagnostic)) in [
        (
            "BRA.L",
            "direct",
            "expr0".to_string(),
            DIAG_UNSUPPORTED_LONG_SIZE,
        ),
        (
            "LINK.L",
            "register_immediate",
            "reg0.class1,expr1".to_string(),
            DIAG_UNSUPPORTED_LONG_SIZE,
        ),
        (
            "EXTB.L",
            "register",
            "reg0.class0".to_string(),
            DIAG_M68020_REQUIRED,
        ),
        (
            "MULU.L",
            "direct_register",
            format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0"),
            DIAG_UNSUPPORTED_LONG_SIZE,
        ),
        (
            "MOVEC",
            "direct_register",
            format!("{MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX}0=CACR,reg1.class0"),
            DIAG_MOVEC_BASELINE,
        ),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: family(),
            mnemonic: mnemonic.to_string(),
            shape_key: shape.to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
            ),
            priority: (990 + priority) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: family(),
        mnemonic: "DIVS.L".to_string(),
        shape_key: "direct_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_DIVS_LONG_BASELINE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0"
        ),
        priority: 1100,
        unstable_widen: false,
        width_rank: 0,
    });
    for (priority, (mnemonic, shape, inputs)) in [
        (
            "CAS.W",
            "register_register_direct",
            format!(
                "reg0.class0,reg1.class0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}2.class1"
            ),
        ),
        (
            "CAS2.W",
            "direct_direct_direct",
            format!(
                "{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX}2.arg0.class1,{MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX}2.arg1.class1"
            ),
        ),
        (
            "CHK2.W",
            "direct_register",
            format!(
                "{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W,reg1.class0"
            ),
        ),
        (
            "BFTST",
            "direct",
            format!(
                "{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg1,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg2"
            ),
        ),
        (
            "PACK",
            "register_register_immediate",
            "reg0.class0,reg1.class0,expr2".to_string(),
        ),
        (
            "TRAPNE",
            "implied",
            format!("{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"),
        ),
        (
            "CALLM",
            "immediate_direct",
            format!(
                "expr0,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W"
            ),
        ),
        ("RTM", "register", "reg0.class1".to_string()),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: family(),
            mnemonic: mnemonic.to_string(),
            shape_key: shape.to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_M68020_REQUIRED}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
            ),
            priority: 1110 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    if let Some(rtm) = selectors
        .iter_mut()
        .find(|selector| selector.mnemonic == "RTM")
    {
        rtm.operand_plan = format!(
            "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}{CPU_LEVEL_KEY}=0+1;{}",
            rtm.operand_plan
        );
    }
    selectors
}

/// Package selectors for the 68010 instruction delta.
pub fn m68010_mode_selectors(cpu_id: &str) -> Vec<ModeSelectorDescriptor> {
    let owner = ScopedOwner::Cpu(cpu_id.to_string());
    let mut selectors = vec![
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "BKPT".to_string(),
            shape_key: "immediate".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SCALAR_PREFIX}{ENCODING_BKPT}{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_BKPT_VECTOR_RANGE}"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner,
            mnemonic: "RTD".to_string(),
            shape_key: "immediate".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SCALAR_PREFIX}{ENCODING_RTD}{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_RTD_DISPLACEMENT_RANGE}"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: ScopedOwner::Cpu(cpu_id.to_string()),
            mnemonic: "MOVE".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_MOVE_CCR_TO_DATA}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg1.class0,reg0.class6"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
    ];
    for (priority, (program, inputs)) in [
        (ENCODING_MOVEC_CONTROL_TO_DATA, "reg0.class7,reg1.class0"),
        (ENCODING_MOVEC_CONTROL_TO_ADDRESS, "reg0.class7,reg1.class1"),
        (ENCODING_MOVEC_DATA_TO_CONTROL, "reg1.class7,reg0.class0"),
        (ENCODING_MOVEC_ADDRESS_TO_CONTROL, "reg1.class7,reg0.class1"),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Cpu(cpu_id.to_string()),
            mnemonic: "MOVEC".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: ScopedOwner::Cpu(cpu_id.to_string()),
        mnemonic: "MOVEC".to_string(),
        shape_key: "direct_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVEC_CONTROL_M68010}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX}0=CACR,reg1.class0"
        ),
        priority: 100,
        unstable_widen: false,
        width_rank: 0,
    });
    for (priority, mnemonic) in ["MOVE", "MOVE.W"].into_iter().enumerate() {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Cpu(cpu_id.to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}17120,{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1,reg0.class6"
            ),
            priority: 8 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Cpu(cpu_id.to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class6,target:expr1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}17145;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1"
            ),
            priority: 16 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: ScopedOwner::Cpu(cpu_id.to_string()),
        mnemonic: "MOVE".to_string(),
        shape_key: "register_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_MOVE_CCR_TO_ABSOLUTE_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W,reg0.class6{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_ABSOLUTE_WORD_RANGE}"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    for (mnemonic, size) in [("MOVES.W", 1), ("MOVES.L", 2)] {
        for (priority, (shape, inputs)) in [
            (
                "register_direct",
                format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1,reg0.class0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{size},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1"
                ),
            ),
            (
                "register_direct",
                format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1,reg0.class1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{size},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1"
                ),
            ),
            (
                "direct_register",
                format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{size},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
                ),
            ),
            (
                "direct_register",
                format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{size},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
                ),
            ),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Cpu(cpu_id.to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: shape.to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_MOVES_INDIRECT}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
                ),
                priority: priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        let absolute_long_opcode = 0x0e39_u32 + (size << 6);
        for register_class in [0_u16, 1_u16] {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Cpu(cpu_id.to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{register_class},target:expr1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{absolute_long_opcode};encode:{PARAM_MOVES_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,reg0.class{register_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{register_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1"
                ),
                priority: 8 + register_class,
                unstable_widen: false,
                width_rank: 0,
            });
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Cpu(cpu_id.to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,reg1.class{register_class};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{absolute_long_opcode};encode:{PARAM_MOVES_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,reg1.class{register_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{register_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0"
                ),
                priority: 10 + register_class,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, mnemonic) in ["MOVES.W", "MOVES.L"].into_iter().enumerate() {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Cpu(cpu_id.to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_INVALID_DESTINATION_FORM}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2"
            ),
            priority: 100 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    if cpu_id == "m68010" {
        for (priority, (mnemonic, shape, operand)) in [
            ("MOVES.W", "register_direct", 1_usize),
            ("MOVES.L", "direct_register", 0_usize),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Cpu(cpu_id.to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: shape.to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_FULL_EXTENSION_UNSUPPORTED}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_NONIDENTITY_SCALE_PREFIX}{operand}{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}any"
                ),
                priority: 1050 + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    if cpu_id == "m68010" {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Cpu(cpu_id.to_string()),
            mnemonic: "DIVS.L".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_DIVS_LONG_M68010}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0"
            ),
            priority: 1100,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    if matches!(cpu_id, "m68010" | "m68020" | "m68030") {
        for (priority, register) in ["TC", "ITT0", "ITT1", "DTT0", "DTT1", "MMUSR", "URP", "SRP"]
            .into_iter()
            .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Cpu(cpu_id.to_string()),
                mnemonic: "MOVEC".to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVEC_CONTROL_M68010}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX}0={register},reg1.class0"
                ),
                priority: 1200 + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    selectors
}

/// Package selectors for the scalar/register subset of the 68020 delta.
pub fn m68020_mode_selectors(cpu_id: &str, include_rtm: bool) -> Vec<ModeSelectorDescriptor> {
    let owner = ScopedOwner::Cpu(cpu_id.to_string());
    let mut selectors = vec![
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "LINK.L".to_string(),
            shape_key: "register_immediate".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_LINK_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr1,reg0.class1{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_LINK_LONG_DISPLACEMENT_RANGE}"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "EXTB.L".to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_EXTB_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
    ];
    if cpu_id == "m68040" {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVE.W".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_M68080_REGISTER_M68040}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX}0=E10,reg1.class0"
            ),
            priority: 1300,
            unstable_widen: false,
            width_rank: 0,
        });
        for (priority, inputs) in [
            format!("{MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX}0=CAAR,reg1.class0"),
            format!("{MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX}0=CAAR,reg1.class1"),
            format!("reg0.class0,{MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX}1=CAAR"),
            format!("reg0.class1,{MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX}1=CAAR"),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "MOVEC".to_string(),
                shape_key: "register_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVEC_CAAR_M68040}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
                ),
                priority: 1400 + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for (priority, register) in [
            "PCR", "CCC", "IEP1", "IEP2", "BPC", "BPW", "DCH", "DCM", "STR", "STC", "IEP3", "STH",
            "STB", "MWR",
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "MOVEC".to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVEC_CONTROL_M68040}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX}0={register},reg1.class0"
                ),
                priority: priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    if cpu_id != "m68080" {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "DBRA.L".to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_NO_SIZE_SUFFIX}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,expr1"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        });
        for mnemonic in [
            "BRA.S+", "BSR.S+", "BHI.S+", "BLS.S+", "BCC.S+", "BHS.S+", "BCS.S+", "BLO.S+",
            "BNE.S+", "BEQ.S+", "BVC.S+", "BVS.S+", "BPL.S+", "BMI.S+", "BGE.S+", "BLT.S+",
            "BGT.S+", "BLE.S+",
        ] {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_MOVE_UNSUPPORTED_SIZE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0"
                ),
                priority: 0,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for mnemonic in ["ADDIW.L", "CMPIW.L", "MOVIW.L"] {
            for (shape_key, inputs) in [
                ("immediate_register", "expr0,reg1.class0"),
                ("immediate_direct", "expr0,indirect_reg1.class1"),
            ] {
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: mnemonic.to_string(),
                    shape_key: shape_key.to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_M68080_ONLY}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
                    ),
                    priority: 0,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
        for (mnemonic, shape_key, inputs) in [
            (
                "MOVE2.W",
                "direct_direct",
                "indirect_reg0.class1,call_arg_register1.arg0.class0,call_arg_register1.arg1.class0",
            ),
            ("MOVEX.L", "register_register", "reg0.class0,reg1.class0"),
            ("MOVEH", "direct_register", "indirect_reg0.class1,reg1.class0"),
            (
                "MOVZ2.B",
                "direct_direct",
                "indirect_reg0.class1,call_arg_register1.arg0.class0,call_arg_register1.arg1.class0",
            ),
            ("TOUCH", "direct", "indirect_reg0.class1"),
        ] {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: shape_key.to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_M68080_ONLY}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
                ),
                priority: 0,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    let full_base = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/r1");
    let full_index = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t2/qL.c0");
    let full_scale = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t2/s");
    let full_displacement = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/mW");
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "MOVE.W".to_string(),
        shape_key: "direct_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{full_base},{full_index},{full_scale},{full_displacement},reg1.class0;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}12336,{full_base},reg1.class0;encode:{PARAM_FULL_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2336,{full_index},{full_scale};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{full_displacement}"
        ),
        priority: 10,
        unstable_widen: false,
        width_rank: 0,
    });
    for (priority, (base_item, index_item)) in [(1_usize, 2_usize), (0, 1)].into_iter().enumerate()
    {
        let base = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/b/t{base_item}/r1");
        let index = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/b/t{index_item}/qL.c0");
        let scale = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/b/t{index_item}/s");
        let outer = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/mW");
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVE.W".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{base},{index},{scale},{outer},reg1.class0;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}12336,{base},reg1.class0;encode:{PARAM_FULL_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2322,{index},{scale};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{outer}"
            ),
            priority: 11 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, base_item) in [1_usize, 0_usize].into_iter().enumerate() {
        let base = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/b/t{base_item}/r1");
        let index = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/qW.c0");
        let scale = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/s");
        let outer = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t2/mL");
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVE.W".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{base},{index},{scale},{outer},reg1.class0;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}12336,{base},reg1.class0;encode:{PARAM_FULL_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}279,{index},{scale};encode:{PARAM_SCALAR_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{outer}"
            ),
            priority: 13 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    let postindexed_alias_base = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/b/r1");
    let postindexed_alias_index = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/qW.c0");
    let postindexed_alias_scale = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/s");
    let postindexed_alias_outer = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t2/mL");
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "MOVE.W".to_string(),
        shape_key: "direct_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{postindexed_alias_base},{postindexed_alias_index},{postindexed_alias_scale},{postindexed_alias_outer},reg1.class0;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}12336,{postindexed_alias_base},reg1.class0;encode:{PARAM_FULL_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}279,{postindexed_alias_index},{postindexed_alias_scale};encode:{PARAM_SCALAR_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{postindexed_alias_outer}"
        ),
        priority: 15,
        unstable_widen: false,
        width_rank: 0,
    });

    let moves_base = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/t1/r1");
    let moves_index = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/t2/qL.c0");
    let moves_scale = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/t2/s");
    let moves_displacement = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}1/i/t0/mW");
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "MOVES.W".to_string(),
        shape_key: "register_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,{moves_base},{moves_index},{moves_scale},{moves_displacement};encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}3696,{moves_base};encode:{PARAM_MOVES_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,reg0.class0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1;encode:{PARAM_FULL_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2336,{moves_index},{moves_scale};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{moves_displacement}"
        ),
        priority: 20,
        unstable_widen: false,
        width_rank: 0,
    });
    let moves_load_base = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/b/t0/r1");
    let moves_load_index = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/b/t1/qL.c0");
    let moves_load_scale = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t0/b/t1/s");
    let moves_load_outer = format!("{MODE_SELECTOR_PLAN_EXPR_PATH_PREFIX}0/i/t1/mW");
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "MOVES.L".to_string(),
        shape_key: "direct_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{moves_load_base},{moves_load_index},{moves_load_scale},{moves_load_outer},reg1.class1;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}3760,{moves_load_base};encode:{PARAM_MOVES_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,reg1.class1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;encode:{PARAM_FULL_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2322,{moves_load_index},{moves_load_scale};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{moves_load_outer}"
        ),
        priority: 21,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.extend(BRANCH_OPCODE_BASES.iter().flat_map(|(mnemonic, opcode)| {
        [
            ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: (*mnemonic).to_string(),
                shape_key: "direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_BRANCH_PREFIX}{BRANCH_SIZED}@{opcode},expr0,auto,1"
                ),
                priority: 0,
                unstable_widen: true,
                width_rank: 0,
            },
            ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: format!("{mnemonic}.L"),
                shape_key: "direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_BRANCH_PREFIX}{BRANCH_SIZED}@{opcode},expr0,{BRANCH_CANDIDATE_LONG},1"
                ),
                priority: 0,
                unstable_widen: false,
                width_rank: 2,
            },
        ]
    }));
    for (priority, (mnemonic, signed_extension)) in [("MULU.L", 0_u32), ("MULS.L", 0x0800)]
        .into_iter()
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}19456,reg0.class0;encode:{PARAM_FIELDS_12_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{signed_extension},reg1.class0,reg1.class0"
            ),
            priority: 8 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}19516;encode:{PARAM_FIELDS_12_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{signed_extension},reg1.class0,reg1.class0;encode:{PARAM_SCALAR_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_IMMEDIATE_LONG}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_SEPARATOR}expr0"
            ),
            priority: 10 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}19472,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_FIELDS_12_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{signed_extension},reg1.class0,reg1.class0"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W,reg1.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}19512;encode:{PARAM_FIELDS_12_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{signed_extension},reg1.class0,reg1.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W"
            ),
            priority: 4 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, opcode)) in
        [("CAS.B", 0x0ad0_u32), ("CAS.W", 0x0cd0), ("CAS.L", 0x0ed0)]
            .into_iter()
            .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}2.class1;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{opcode},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}2.class1;encode:{PARAM_FIELDS_6_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,reg1.class0,reg0.class0"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        let absolute_long_opcode = (opcode & 0xffc0) + 0x39;
        for (target_priority, target_plan) in [
            format!(
                "{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}2{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
            ),
            "target:expr2".to_string(),
        ]
        .into_iter()
        .enumerate()
        {
            let fixup_source = if target_priority == 0 {
                format!(
                    "target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}2{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                )
            } else {
                "target:expr2".to_string()
            };
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: "register_register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class0,{target_plan};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{absolute_long_opcode};encode:{PARAM_FIELDS_6_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,reg1.class0,reg0.class0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{fixup_source}"
                ),
                priority: 8 + priority as u16 * 2 + target_priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_INVALID_DESTINATION_FORM}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class0,reg2.class0"
            ),
            priority: 100 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, opcode)) in [("CAS2.W", 0x0cfc_u32), ("CAS2.L", 0x0efc)]
        .into_iter()
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX}2.arg0.class1,{MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX}2.arg1.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{opcode};encode:{PARAM_FIELDS_12_6_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}32768,{MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX}2.arg0.class1,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg0.class0;encode:{PARAM_FIELDS_12_6_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}32768,{MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX}2.arg1.class1,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg1.class0"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (mnemonic, opcode, trap_extension) in [
        ("CHK2.B", 0x00f8_u32, 0x0800_u32),
        ("CHK2.W", 0x02f8, 0x0800),
        ("CHK2.L", 0x04f8, 0x0800),
        ("CMP2.B", 0x00f8, 0),
        ("CMP2.W", 0x02f8, 0),
        ("CMP2.L", 0x04f8, 0),
    ] {
        for (priority, (class, address_extension)) in
            [(0_u16, 0_u32), (1, 0x8000)].into_iter().enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W,reg1.class{class};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{opcode};encode:{PARAM_FIELDS_12_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class{class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W",
                    trap_extension + address_extension
                ),
                priority: priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
            for (target_priority, target_plan) in [
                format!(
                    "{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                ),
                "target:expr0".to_string(),
            ]
            .into_iter()
            .enumerate()
            {
                let fixup_source = if target_priority == 0 {
                    format!(
                        "target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                    )
                } else {
                    "target:expr0".to_string()
                };
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "direct_register".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{target_plan},reg1.class{class};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{};encode:{PARAM_FIELDS_12_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class{class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{fixup_source}",
                        opcode + 1,
                        trap_extension + address_extension
                    ),
                    priority: 4 + priority as u16 * 2 + target_priority as u16,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
    }
    selectors.extend([
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "BFTST".to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg1,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg2;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}59584;encode:{PARAM_BITFIELD_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg1,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg2"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "BFEXTU".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_CALL_ARG_MEMBER_PREFIX}0.arg0.fieldW,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg2,reg1.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}59896;encode:{PARAM_BITFIELD_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2048,reg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg2;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_CALL_ARG_MEMBER_PREFIX}0.arg0.fieldW"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "BFINS".to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX}1.arg0.class1,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}1.arg1,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg2.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61392,{MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX}1.arg0.class1;encode:{PARAM_BITFIELD_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}32,reg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}1.arg1,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg2.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
    ]);
    for (priority, (target_plan, fixup_source)) in [
        (
            format!("target:{MODE_SELECTOR_PLAN_CALL_ARG_MEMBER_PREFIX}0.arg0.fieldL"),
            format!("target:{MODE_SELECTOR_PLAN_CALL_ARG_MEMBER_PREFIX}0.arg0.fieldL"),
        ),
        (
            format!("target:{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg0"),
            format!("target:{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg0"),
        ),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "BFTST".to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{target_plan},{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg1,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg2;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}59641;encode:{PARAM_BITFIELD_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg1,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg2;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{fixup_source}"
            ),
            priority: 4 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "BFEXTU".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{target_plan},{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg2,reg1.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}59897;encode:{PARAM_BITFIELD_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2048,reg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}0.arg2;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{fixup_source}"
            ),
            priority: 4 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (target_plan, fixup_source)) in [
        (
            format!("target:{MODE_SELECTOR_PLAN_CALL_ARG_MEMBER_PREFIX}1.arg0.fieldL"),
            format!("target:{MODE_SELECTOR_PLAN_CALL_ARG_MEMBER_PREFIX}1.arg0.fieldL"),
        ),
        (
            format!("target:{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}1.arg0"),
            format!("target:{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}1.arg0"),
        ),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "BFINS".to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,{target_plan},{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}1.arg1,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg2.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61433;encode:{PARAM_BITFIELD_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}32,reg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_VALUE_PREFIX}1.arg1,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg2.class0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{fixup_source}"
            ),
            priority: 4 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, opcode)) in [("PACK", 0x8140_u32), ("UNPK", 0x8180)]
        .into_iter()
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register_immediate".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class0,expr2;encode:{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{opcode},reg1.class0,reg0.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr2"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_direct_immediate".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}0.class1,{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1,expr2;encode:{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{}, {MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1,{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr2",
                opcode + 8
            ).replace(", ", ","),
            priority: 4 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (mnemonic, condition) in TRAPCC_CONDITIONS {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: (*mnemonic).to_string(),
            shape_key: "implied".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{}",
                0x50fc_u32 + (condition << 8)
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        });
        for (suffix, opcode_low, scalar_program) in [
            ("W", 0xfa_u32, PARAM_SCALAR_WORD),
            ("L", 0xfb_u32, PARAM_SCALAR_LONG),
        ] {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: format!("{mnemonic}.{suffix}"),
                shape_key: "immediate".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{};encode:{scalar_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0",
                    0x5000_u32 + (condition << 8) + opcode_low
                ),
                priority: 0,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "CALLM".to_string(),
        shape_key: "immediate_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: if cpu_id == "m68040" {
            format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_CALLM_M68040}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W"
            )
        } else {
            format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_UNSIGNED_BYTE}:expr0,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1784;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_UNSIGNED_BYTE}:expr0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W"
            )
        },
        priority: if cpu_id == "m68040" { 1300 } else { 0 },
        unstable_widen: false,
        width_rank: 0,
    });
    if cpu_id != "m68040" {
        for (priority, (target_plan, fixup_source)) in [
            (
                format!(
                    "{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                ),
                format!(
                    "target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                ),
            ),
            ("target:expr1".to_string(), "target:expr1".to_string()),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "CALLM".to_string(),
                shape_key: "immediate_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_UNSIGNED_BYTE}:expr0,{target_plan};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1785;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_UNSIGNED_BYTE}:expr0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{fixup_source}"
                ),
                priority: 4 + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (mnemonic, extension_base)) in [("DIVS.L", 0x0800_u32), ("DIVU.L", 0_u32)]
        .into_iter()
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}19520,reg0.class0;encode:{PARAM_FIELDS_12_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{extension_base},reg1.class0,reg1.class0"
            ),
            priority: 8 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}19536,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_FIELDS_12_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{extension_base},reg1.class0,reg1.class0"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, extension_base)) in [
        ("DIVS.L", 0x0c00_u32),
        ("DIVU.L", 0x0400),
        ("DIVSL.L", 0x0800),
        ("DIVUL.L", 0),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,{MODE_SELECTOR_PLAN_DISTINCT_REGISTER_PREFIX}1,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}19520,reg0.class0;encode:{PARAM_FIELDS_12_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{extension_base},{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0"
            ),
            priority: 20 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,{MODE_SELECTOR_PLAN_DISTINCT_REGISTER_PREFIX}1,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}19536,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_FIELDS_12_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{extension_base},{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0"
            ),
            priority: 10 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    let cas2_structure = format!(
        "{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX}2.arg0.class1,{MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX}2.arg1.class1"
    );
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "CAS2.B".to_string(),
        shape_key: "direct_direct_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_CAS2_BYTE_SIZE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{cas2_structure}"
        ),
        priority: 100,
        unstable_widen: false,
        width_rank: 0,
    });
    for mnemonic in ["CAS2.W", "CAS2.L"] {
        for (priority, (left_class, right_class)) in
            [(0_u16, 1_u16), (1, 0), (0, 0)].into_iter().enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct_direct_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_CAS2_MEMORY_PAIR}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}0.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class0,{MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX}2.arg0.class{left_class},{MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_REGISTER_PREFIX}2.arg1.class{right_class}"
                ),
                priority: 100 + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for mnemonic in ["DIVS.L", "DIVU.L", "DIVSL.L", "DIVUL.L"] {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_DIV_PAIR_DISTINCT}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,{MODE_SELECTOR_PLAN_DUPLICATE_REGISTER_PREFIX}1"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for mnemonic in ["DIVSL.L", "DIVUL.L"] {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_DIVSL_PAIR_REQUIRED}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0"
            ),
            priority: 100,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for mnemonic in ["CMP2.B", "CMP2.W", "CMP2.L"] {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_CMP2_BOUNDS}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,reg1.class0"
            ),
            priority: 100,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "BFINS".to_string(),
        shape_key: "register_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_BITFIELD_EA}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,{MODE_SELECTOR_PLAN_CALL_ARG_INDIRECT_TUPLE_REGISTER_PREFIX}1.arg0.item1.class8"
        ),
        priority: 100,
        unstable_widen: false,
        width_rank: 0,
    });
    for (mnemonic, _) in TRAPCC_CONDITIONS {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: (*mnemonic).to_string(),
            shape_key: "immediate".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_TRAPCC_UNSIZED_OPERAND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0"
            ),
            priority: 100,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "RTM".to_string(),
        shape_key: "immediate".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_RTM_OPERAND}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0"
        ),
        priority: 100,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "CALLM".to_string(),
        shape_key: "immediate_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_CALLM_COUNT}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_OUT_OF_RANGE_PREFIX}0.min0.max255,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W"
        ),
        priority: 100,
        unstable_widen: false,
        width_rank: 0,
    });
    for (priority, (mnemonic, operation)) in [
        ("FMOVE", 0x00_u16),
        ("FADD", 0x22),
        ("FSUB", 0x28),
        ("FMUL", 0x23),
        ("FDIV", 0x20),
        ("FNEG", 0x1A),
        ("FCMP", 0x38),
        ("FINT", 0x01),
        ("FINTRZ", 0x03),
        ("FSIN", 0x0E),
        ("FTAN", 0x0F),
        ("FASIN", 0x0C),
        ("FACOS", 0x1C),
        ("FATAN", 0x0A),
        ("FSINH", 0x02),
        ("FCOSH", 0x19),
        ("FTANH", 0x09),
        ("FATANH", 0x0D),
        ("FETOX", 0x10),
        ("FETOXM1", 0x08),
        ("FTENTOX", 0x12),
        ("FTWOTOX", 0x11),
        ("FLOGN", 0x14),
        ("FLOGNP1", 0x06),
        ("FLOG10", 0x15),
        ("FLOG2", 0x16),
        ("FGETEXP", 0x1E),
        ("FGETMAN", 0x1F),
        ("FSCALE", 0x26),
        ("FMOD", 0x21),
        ("FREM", 0x25),
        ("FSGLDIV", 0x24),
        ("FSGLMUL", 0x27),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,reg1.class2;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61952;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,reg0.class2,reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{operation}"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, operation)) in [
        ("FSQRT", 0x04_u16),
        ("FABS", 0x18),
        ("FNEG", 0x1A),
        ("FTST", 0x3A),
        ("FINT", 0x01),
        ("FINTRZ", 0x03),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61952;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,reg0.class2,reg0.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{operation}"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, inputs, first_base, extension_base, format_field)) in [
        (
            "FMOVE.B",
            "reg0.class0,reg1.class2",
            0xF200_u16,
            0x4000_u16,
            6_u16,
        ),
        ("FMOVE.W", "reg0.class2,reg1.class0", 0xF200, 0x6000, 4),
    ]
    .into_iter()
    .enumerate()
    {
        let (ea_input, fp_input) = if mnemonic == "FMOVE.B" {
            ("reg0.class0", "reg1.class2")
        } else {
            ("reg1.class0", "reg0.class2")
        };
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs};encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{first_base},{ea_input};encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{extension_base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},{fp_input},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FTST.W".to_string(),
        shape_key: "register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61952,reg0.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}20538"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FCOS.W".to_string(),
        shape_key: "direct_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class2;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61968,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}4,reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}29"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FSINCOS".to_string(),
        shape_key: "register_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class2,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class2;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61952;encode:{PARAM_FIELDS_10_7_0_3}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}48,reg0.class2,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg1.class2,{MODE_SELECTOR_PLAN_CALL_ARG_REGISTER_PREFIX}1.arg0.class2"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    for (priority, (control_register, control_mask)) in
        [("FPCR", 0x1000_u16), ("FPSR", 0x0800), ("FPIAR", 0x0400)]
            .into_iter()
            .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "FMOVE.L".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,{MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX}1={control_register};encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61952,reg0.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{}",
                0x8000_u16 | control_mask
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "FMOVE.L".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX}0={control_register},reg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61952,reg1.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{}",
                0xA000_u16 | control_mask
            ),
            priority: (priority + 3) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FMOVEM".to_string(),
        shape_key: "direct_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61968,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_FIELD_0_8}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61440,{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map2=0"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FMOVEM".to_string(),
        shape_key: "direct_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map2=0,{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61984,{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_FIELD_0_8}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}57344,{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map2=8.reverse16"
        ),
        priority: 2,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FMOVEM".to_string(),
        shape_key: "direct_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61976,{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_FIELD_0_8}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}53248,{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}1.map2=0"
        ),
        priority: 1,
        unstable_widen: false,
        width_rank: 0,
    });
    for (priority, (mnemonic, format_field)) in [
        ("FMOVE.B", 6_u16),
        ("FMOVE.W", 4),
        ("FMOVE.L", 0),
        ("FMOVE.S", 1_u16),
        ("FMOVE.D", 5),
        ("FMOVE.X", 2),
        ("FMOVE.P", 3),
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
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class2;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61968,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        for (mode_priority, (first_word, ea_source)) in [
            (
                0xF218_u16,
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1"),
            ),
            (
                0xF220_u16,
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}0.class1"),
            ),
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
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{ea_source},reg1.class2;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{first_word},{ea_source};encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
                ),
                priority: (8 + priority * 3 + mode_priority) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,reg1.class2;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61992,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
            ),
            priority: (20 + priority) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        for (index_priority, (qualifier, long_bit)) in
            [("W", 0_u32), ("L", 1_u32)].into_iter().enumerate()
        {
            for index_class in [0_u16, 1_u16] {
                selectors.push(ModeSelectorDescriptor {
                    owner: owner.clone(),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "direct_register".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3,reg1.class2;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}62000,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
                    ),
                    priority: (32 + priority * 4 + index_priority * 2 + index_class as usize)
                        as u16,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
        for (absolute_priority, (field, first_word, value_program)) in [
            ("W", 0xF238_u16, PARAM_SCALAR_WORD),
            ("L", 0xF239_u16, PARAM_SCALAR_LONG),
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
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field},reg1.class2;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{first_word};encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;encode:{value_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field}"
                ),
                priority: (64 + priority * 2 + absolute_priority) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,reg1.class2;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}62010;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
            ),
            priority: (80 + priority) as u16,
            unstable_widen: false,
            width_rank: 0,
        });

        for (form_priority, (first_word, ea_source, trailing_program)) in [
            (
                0xF210_u16,
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
                None,
            ),
            (
                0xF228,
                format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1"
                ),
                Some(format!(
                    "encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
                )),
            ),
            (
                0xF220,
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"),
                None,
            ),
        ]
        .into_iter()
        .enumerate()
        {
            let trailing = trailing_program
                .map(|step| format!(";{step}"))
                .unwrap_or_default();
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,{ea_source};encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{first_word},{ea_source};encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}24576,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg0.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0{trailing}"
                ),
                priority: (40 + priority * 4 + form_priority) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}62009;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}24576,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg0.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;encode:{PARAM_SCALAR_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
            ),
            priority: (56 + priority) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, format_field)) in [("FMOVE.B", 6_u16), ("FMOVE.L", 0_u16)]
        .into_iter()
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class2,reg1.class0;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61952,reg1.class0;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}24576,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg0.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
            ),
            priority: (70 + priority) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FMOVE.D".to_string(),
        shape_key: "direct_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,reg1.class2;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61992,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}5,reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
        ),
        priority: 10,
        unstable_widen: false,
        width_rank: 0,
    });
    for (priority, (mnemonic, suffix, first_word, format_field, value_program)) in [
        ("FMOVE.X", "W", 0xF238_u16, 2_u16, PARAM_SCALAR_WORD),
        ("FMOVE.P", "L", 0xF239_u16, 3_u16, PARAM_SCALAR_LONG),
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
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{suffix},reg1.class2;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{first_word};encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;encode:{value_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{suffix}"
            ),
            priority: (20 + priority) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FADD.X".to_string(),
        shape_key: "direct_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class2;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61968,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2,reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}34"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    for (format_priority, (mnemonic, format_field)) in [
        ("FADD.B", 6_u16),
        ("FADD.W", 4),
        ("FADD.L", 0),
        ("FADD.S", 1),
        ("FADD.D", 5),
        ("FADD.X", 2),
        ("FADD.P", 3),
    ]
    .into_iter()
    .enumerate()
    {
        for (mode_priority, (first_word, ea_source, trailing_program)) in [
            (
                0xF210_u16,
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1"),
                None,
            ),
            (
                0xF218_u16,
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1"),
                None,
            ),
            (
                0xF220_u16,
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}0.class1"),
                None,
            ),
            (
                0xF228_u16,
                format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1"
                ),
                Some(format!(
                    "encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
                )),
            ),
        ]
        .into_iter()
        .enumerate()
        {
            let trailing = trailing_program
                .map(|step| format!(";{step}"))
                .unwrap_or_default();
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{ea_source},reg1.class2;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{first_word},{ea_source};encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16384,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{format_field},reg1.class2,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}34{trailing}"
                ),
                priority: (10 + format_priority * 4 + mode_priority) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (control_register, control_mask)) in
        [("FPCR", 4_u16), ("FPSR", 2_u16), ("FPIAR", 1_u16)]
            .into_iter()
            .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "FMOVE.L".to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,{MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX}1={control_register};encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61968,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}32768,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{control_mask},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
            ),
            priority: (10 + priority) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "FMOVE.L".to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_NAMED_REGISTER_PREFIX}0={control_register},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61968,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}40960,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{control_mask},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
            ),
            priority: (20 + priority) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FMOVEM.L".to_string(),
        shape_key: "direct_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61968,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}32768,{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}1.map3=0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FMOVEM.L".to_string(),
        shape_key: "direct_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61968,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_FIELDS_10_7_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}40960,{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map3=0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
        ),
        priority: 1,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.extend([
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "FBEQ".to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}62081;fixup:{FIXUP_PC_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "FDBNE".to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}62024,reg0.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}14;fixup:{FIXUP_PC_WORD_BASE4}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "FSNE".to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}62016,reg0.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}14"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "FTRAPGT.W".to_string(),
            shape_key: "immediate".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}62074;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}18;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "FSAVE".to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}62224,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "FRESTORE".to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}62296,{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
    ]);
    for (priority, (mnemonic, shape, inputs, diagnostic)) in [
        (
            "FMOVE.S",
            "register_register",
            "reg0.class2,reg1.class0".to_string(),
            DIAG_INVALID_DESTINATION_FORM,
        ),
        (
            "FMOVE.X",
            "register_direct",
            format!(
                "reg0.class2,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8"
            ),
            DIAG_INVALID_DESTINATION_FORM,
        ),
        (
            "FMOVE.X",
            "register_register",
            "reg0.class2,reg1.class2".to_string(),
            DIAG_FPU_DATA_REGISTER_EA,
        ),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: mnemonic.to_string(),
            shape_key: shape.to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
            ),
            priority: (100 + priority) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FNOP".to_string(),
        shape_key: "implied".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}{FPU_TARGET_KEY}=1+2+3+4;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}62080;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: owner.clone(),
        mnemonic: "FMOVECR".to_string(),
        shape_key: "immediate_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}{FPU_TARGET_KEY}=1+2+4;{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,reg1.class2;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}61952;encode:{PARAM_FIELDS_7_0_7}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}23552,reg1.class2,expr0"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    if include_rtm {
        for (priority, (program, class)) in [(ENCODING_RTM_DATA, 0), (ENCODING_RTM_ADDRESS, 1)]
            .into_iter()
            .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "RTM".to_string(),
                shape_key: "register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{class}"
                ),
                priority: priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    } else if let Some(diagnostic) = match cpu_id {
        "m68030" => Some(DIAG_RTM_M68030),
        "m68040" => Some(DIAG_RTM_M68040),
        _ => None,
    } {
        for (priority, class) in [0_u16, 1].into_iter().enumerate() {
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "RTM".to_string(),
                shape_key: "register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{diagnostic}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{class}"
                ),
                priority: 1300 + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    if cpu_id == "m68030" {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "PFLUSH".to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_PFLUSH_68030}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_IMMEDIATE_PREFIX}1"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        });
    } else if cpu_id == "m68040" {
        selectors.extend([
            ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "PFLUSH".to_string(),
                shape_key: "direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_PFLUSH_68040}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}indirect_reg0.class1"
                ),
                priority: 0,
                unstable_widen: false,
                width_rank: 0,
            },
            ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "PFLUSH".to_string(),
                shape_key: "immediate_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_PFLUSH_M68040_ARITY}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_IMMEDIATE_PREFIX}1"
                ),
                priority: 1300,
                unstable_widen: false,
                width_rank: 0,
            },
        ]);
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "MOVE16".to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1,{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_FIELDS_12_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0",
                0xf620_u32, 0x8000_u32
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        });
        for (priority, (register_source, absolute_source, base)) in [
            (
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1"),
                format!(
                    "{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                ),
                0xf610_u32,
            ),
            (
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1"),
                format!(
                    "{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                ),
                0xf600_u32,
            ),
            (
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
                format!(
                    "{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                ),
                0xf618_u32,
            ),
            (
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"),
                format!(
                    "{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                ),
                0xf608_u32,
            ),
        ]
        .into_iter()
        .enumerate()
        {
            let absolute_value = if priority < 2 {
                format!(
                    "{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                )
            } else {
                format!(
                    "{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                )
            };
            selectors.push(ModeSelectorDescriptor {
                owner: owner.clone(),
                mnemonic: "MOVE16".to_string(),
                shape_key: "direct_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{register_source},{absolute_source};encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{register_source};encode:{PARAM_SCALAR_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{absolute_value}"
                ),
                priority: 1 + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    } else if cpu_id == "m68020" {
        selectors.push(ModeSelectorDescriptor {
            owner: owner.clone(),
            mnemonic: "PFLUSH".to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_REJECT_PREFIX}{DIAG_PFLUSH_M68020}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_IMMEDIATE_PREFIX}1"
            ),
            priority: 1300,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    let active_targets = match cpu_id {
        "m68020" | "m68030" => "1+2",
        "m68040" => "3",
        "m68080" => "4",
        _ => "1+2+3+4",
    };
    if cpu_id == "m68040" {
        const EXTERNAL_ONLY_MNEMONICS: &[&str] = &[
            "FMOVECR", "FSGLDIV", "FSGLMUL", "FSIN", "FCOS", "FSINCOS", "FTAN", "FASIN", "FACOS",
            "FATAN", "FSINH", "FCOSH", "FTANH", "FATANH", "FETOX", "FETOXM1", "FTENTOX", "FTWOTOX",
            "FLOGN", "FLOGNP1", "FLOG10", "FLOG2", "FGETEXP", "FGETMAN", "FSCALE", "FMOD", "FREM",
        ];
        for selector in selectors.iter_mut().filter(|selector| {
            EXTERNAL_ONLY_MNEMONICS.contains(
                &selector
                    .mnemonic
                    .split_once('.')
                    .map_or(selector.mnemonic.as_str(), |(base, _)| base),
            )
        }) {
            selector.operand_plan = format!(
                "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}{FPU_TARGET_KEY}=1+2?encoding.fpu-integrated-unsupported.m68040;{}",
                selector.operand_plan
            );
        }
    }
    let disabled_code = format!("encoding.fpu-disabled.{cpu_id}");
    for selector in selectors
        .iter_mut()
        .filter(|selector| selector.mnemonic.starts_with('F'))
    {
        selector.operand_plan = format!(
            "{MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX}{FPU_TARGET_KEY}={active_targets}?{disabled_code};{}",
            selector.operand_plan
        );
    }
    selectors
}

/// Select the neutral no-operand shape for fixed instruction programs.
pub fn mode_selectors() -> Vec<ModeSelectorDescriptor> {
    let mut selectors = vec![
        ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: "TRAP".to_string(),
            shape_key: "immediate".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SCALAR_PREFIX}{ENCODING_TRAP_VECTOR}{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_TRAP_VECTOR_RANGE}"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
        ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: "MOVEQ".to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_MOVEQ}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_NORMALIZED_INPUT}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_SEPARATOR}expr0,reg1.class0{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_MOVEQ_IMMEDIATE_RANGE}"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
    ];
    selectors.extend(baseline_rejection_selectors());
    selectors.extend(BRANCH_OPCODE_BASES.iter().flat_map(|(mnemonic, opcode)| {
        [
            ((*mnemonic).to_string(), "auto", true, 0_u8),
            (format!("{mnemonic}.B"), "0", false, 0_u8),
            (format!("{mnemonic}.S"), "0", false, 0_u8),
            (format!("{mnemonic}.W"), "1", false, 1_u8),
        ]
        .into_iter()
        .map(move |(form, candidate, unstable_widen, width_rank)| ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: form,
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_BRANCH_PREFIX}{BRANCH_SIZED}@{opcode},expr0,{candidate},0"
            ),
            priority: 0,
            unstable_widen,
            width_rank,
        })
    }));
    selectors.extend(DBCC_OPCODE_BASES.iter().map(|(mnemonic, base)| {
        ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: (*mnemonic).to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},reg0.class0;fixup:{FIXUP_PC_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 1,
        }
    }));
    selectors.extend(DBCC_OPCODE_BASES.iter().map(|(mnemonic, _)| {
        ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: (*mnemonic).to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_TRAP_VECTOR}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16,reg0.class1{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_DBCC_COUNTER}"
            ),
            priority: 1,
            unstable_widen: false,
            width_rank: 0,
        }
    }));
    for (mnemonic, base) in EXTEND_REGISTER_BASES.iter().copied() {
        for (priority, (shape, inputs, mode_bit)) in [
            ("register_register", "reg0.class0,reg1.class0", 0_u32),
            (
                "direct_direct",
                "unary_minus_indirect_reg0.class1,unary_minus_indirect_reg1.class1",
                8_u32,
            ),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: shape.to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{inputs}",
                    base + mode_bit
                ),
                priority: priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    selectors.extend(CMPM_BASES.iter().map(|(mnemonic, base)| ModeSelectorDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        mnemonic: (*mnemonic).to_string(),
        shape_key: "direct_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},unary_plus_indirect_reg0.class1,unary_plus_indirect_reg1.class1"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    }));
    for (mnemonic, base) in ADDRESS_ALU_BASES.iter().copied() {
        for (priority, (source_class, mode_bits)) in
            [(0_u16, 0_u32), (1, 0x08)].into_iter().enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "register_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class{source_class},reg1.class1",
                    base + mode_bits
                ),
                priority: priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        let immediate_program = if mnemonic.ends_with(".W") {
            PARAM_IMMEDIATE_WORD_FIELD_9
        } else {
            PARAM_IMMEDIATE_LONG_FIELD_9
        };
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},expr0,reg1.class1",
                base + 0x3c
            ),
            priority: 2,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},indirect_reg0.class1,reg1.class1",
                base + 0x10
            ),
            priority: 3,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_DISPLACEMENT_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2",
                base + 0x28
            ),
            priority: 4,
            unstable_widen: false,
            width_rank: 0,
        });
        for (priority, (qualifier, long_bit)) in
            [("W", 0_u32), ("L", 1_u32)].into_iter().enumerate()
        {
            for index_class in [0_u16, 1_u16] {
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "direct_register".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class1;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                        base + 0x30
                    ),
                    priority: 5 + priority as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
        for (priority, (field, program, mode_bits)) in [
            ("W", PARAM_EXTENSION_WORD_9, 0x38_u32),
            ("L", PARAM_EXTENSION_LONG_9, 0x39_u32),
        ]
        .into_iter()
        .enumerate()
        {
            let operand_plan = if field == "L" {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L,reg1.class1;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L",
                    base + mode_bits
                )
            } else {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class1,{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}scalar.absolute-{field}:member0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field}",
                    base + mode_bits
                )
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan,
                priority: (priority + 4) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (mnemonic, base) in WORD_SOURCE_TO_DATA_BASES.iter().copied() {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},reg0.class0,reg1.class0"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},indirect_reg0.class1,reg1.class0",
                base + 0x10
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_IMMEDIATE_WORD_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},expr0,reg1.class0",
                base + 0x3c
            ),
            priority: 1,
            unstable_widen: false,
            width_rank: 0,
        });
        for (priority, (field, program, mode_bits)) in [
            ("W", PARAM_EXTENSION_WORD_9, 0x38_u32),
            ("L", PARAM_EXTENSION_LONG_9, 0x39_u32),
        ]
        .into_iter()
        .enumerate()
        {
            let operand_plan = if field == "L" {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L,reg1.class0;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L",
                    base + mode_bits
                )
            } else {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class0,{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}scalar.absolute-{field}:member0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field}",
                    base + mode_bits
                )
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan,
                priority: (priority + 2) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELD_9_DISPLACEMENT}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2",
                base + 0x3a
            ),
            priority: 4,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.extend(SINGLE_REGISTER_INSTRUCTION_PROGRAMS.iter().map(
        |(mnemonic, program_id, _, register_class)| ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: (*mnemonic).to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program_id}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{register_class}"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        },
    ));
    selectors.push(ModeSelectorDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        mnemonic: "SWAP".to_string(),
        shape_key: "register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_TRAP_VECTOR}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16,reg0.class1{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_SWAP_REGISTER}"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    for (priority, (mnemonic, register_to_memory, memory_to_register)) in [
        ("MOVEP.W", 0x0188_u32, 0x0108_u32),
        ("MOVEP.L", 0x01c8_u32, 0x0148_u32),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{register_to_memory},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg0.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0;match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{memory_to_register},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0;match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2"
            ),
            priority: (priority + 2) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, register_to_memory, memory_to_register)) in [
        ("MOVEM.W", 0x48a0_u32, 0x4c98_u32),
        ("MOVEM.L", 0x48e0_u32, 0x4cd8_u32),
    ]
    .into_iter()
    .enumerate()
    {
        let register_to_absolute_word = (register_to_memory & 0xffc0) + 0x38;
        let register_to_absolute_long = (register_to_memory & 0xffc0) + 0x39;
        let absolute_long_to_register = (memory_to_register & 0xffc0) + 0x39;
        let register_to_displacement = (register_to_memory & 0xffc0) + 0x28;
        let displacement_to_register = (memory_to_register & 0xffc0) + 0x28;
        let pc_displacement_to_register = (memory_to_register & 0xffc0) + 0x3a;
        for (shape_priority, shape_key) in
            ["direct_direct", "register_direct"].into_iter().enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: shape_key.to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{register_to_memory},{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map0=0+1=8.reverse16"
                ),
                priority: priority as u16 + shape_priority as u16 * 20,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map0=0+1=8,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{register_to_absolute_word};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map0=0+1=8;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}scalar.absolute-W:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_ABSOLUTE_WORD_RANGE}"
            ),
            priority: 5 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        for (shape_priority, shape_key) in
            ["direct_direct", "register_direct"].into_iter().enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: shape_key.to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map0=0+1=8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{register_to_displacement},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map0=0+1=8;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
                ),
                priority: 28 + priority as u16 + shape_priority as u16 * 2,
                unstable_widen: false,
                width_rank: 0,
            });
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: shape_key.to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}1.map0=0+1=8;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{displacement_to_register},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}1.map0=0+1=8;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
                ),
                priority: 32 + priority as u16 + shape_priority as u16 * 2,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for (shape_priority, shape_key) in
            ["direct_direct", "direct_register"].into_iter().enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: shape_key.to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{memory_to_register},{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}1.map0=0+1=8"
                ),
                priority: (priority + 2) as u16 + shape_priority as u16 * 20,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        let indirect_to_register = memory_to_register - 8;
        for (shape_priority, shape_key) in
            ["direct_direct", "direct_register"].into_iter().enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: shape_key.to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{indirect_to_register},{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}1.map0=0+1=8"
                ),
                priority: 24 + priority as u16 + shape_priority as u16 * 2,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map0=0+1=8,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{register_to_absolute_long};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map0=0+1=8;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
            ),
            priority: 7 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        for (shape_priority, shape_key) in
            ["direct_direct", "register_direct"].into_iter().enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: shape_key.to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map0=0+1=8,target:expr1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{register_to_absolute_long};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map0=0+1=8;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1"
                ),
                priority: 9 + priority as u16 + shape_priority as u16 * 2,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for (shape_priority, shape_key) in
            ["direct_direct", "direct_register"].into_iter().enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: shape_key.to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}1.map0=0+1=8;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{absolute_long_to_register};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}1.map0=0+1=8;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0"
                ),
                priority: 13 + priority as u16 + shape_priority as u16 * 2,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map0=0+1=8,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{register_to_absolute_word};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map0=0+1=8;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}scalar.absolute-W:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_ABSOLUTE_WORD_RANGE}"
            ),
            priority: 4 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map0=0+1=8,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{register_to_absolute_long};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}0.map0=0+1=8;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
            ),
            priority: 6 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}1.map0=0+1=8;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{pc_displacement_to_register};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REGISTER_MASK_PREFIX}1.map0=0+1=8;fixup:{FIXUP_PC_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
            ),
            priority: 8 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (program_id, input_plan)) in [
        (EXG_DATA_DATA, "reg0.class0,reg1.class0"),
        (EXG_ADDRESS_ADDRESS, "reg0.class1,reg1.class1"),
        (EXG_DATA_ADDRESS, "reg0.class0,reg1.class1"),
        (EXG_DATA_ADDRESS, "reg1.class0,reg0.class1"),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: "EXG".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program_id}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{input_plan}"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        mnemonic: "EXG".to_string(),
        shape_key: "register_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_TRAP_VECTOR}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16,reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_EXG_PAIR}"
        ),
        priority: 8,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        mnemonic: "MOVE.L".to_string(),
        shape_key: "immediate_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L;encode:{PARAM_FIXED_EXTENSION_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}9212,expr0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
        ),
        priority: 99,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        mnemonic: "STOP".to_string(),
        shape_key: "immediate".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SCALAR_PREFIX}{ENCODING_STOP}{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_STOP_IMMEDIATE_RANGE}"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.extend(["LINK", "LINK.W"].into_iter().map(|mnemonic| {
        ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_immediate".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_LINK_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr1,reg0.class1{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_LINK_DISPLACEMENT_RANGE}"
            ),
            priority: 0,
            unstable_widen: false,
            width_rank: 0,
        }
    }));
    for (priority, (program, inputs)) in [
        (ENCODING_MOVE_USP_TO_ADDRESS, "reg1.class1,reg0.class4"),
        (ENCODING_MOVE_ADDRESS_TO_USP, "reg0.class1,reg1.class4"),
        (ENCODING_MOVE_SR_TO_DATA, "reg1.class0,reg0.class5"),
        (ENCODING_MOVE_DATA_TO_CCR, "reg0.class0,reg1.class6"),
        (ENCODING_MOVE_DATA_TO_SR, "reg0.class0,reg1.class5"),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: "MOVE".to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs}"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (shape_key, inputs, opcode)) in [
        ("register_direct", "reg0.class5,target:expr1", 0x40f9_u32),
        ("direct_register", "target:expr0,reg1.class5", 0x46f9_u32),
        ("direct_register", "target:expr0,reg1.class6", 0x44f9_u32),
    ]
    .into_iter()
    .enumerate()
    {
        let target = if shape_key == "register_direct" {
            "target:expr1"
        } else {
            "target:expr0"
        };
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: "MOVE".to_string(),
            shape_key: shape_key.to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{inputs};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{opcode};fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{target}"
            ),
            priority: 8 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (program, class, diagnostic)) in [
        (
            ENCODING_MOVE_IMMEDIATE_TO_CCR,
            6,
            DIAG_STATUS_BYTE_IMMEDIATE_RANGE,
        ),
        (
            ENCODING_MOVE_IMMEDIATE_TO_SR,
            5,
            DIAG_STATUS_WORD_IMMEDIATE_RANGE,
        ),
    ]
    .into_iter()
    .enumerate()
    {
        for mnemonic in ["MOVE", "MOVE.W"] {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "immediate_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,reg1.class{class}{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{diagnostic}"
                ),
                priority: (priority + 5) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, mnemonic) in ["MOVE", "MOVE.W"].into_iter().enumerate() {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}17624,{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class6"
            ),
            priority: 8 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        mnemonic: "MOVE".to_string(),
        shape_key: "direct_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_MOVE_ABSOLUTE_WORD_TO_CCR}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W,reg1.class6{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_ABSOLUTE_WORD_RANGE}"
        ),
        priority: 0,
        unstable_widen: false,
        width_rank: 0,
    });
    for (mnemonic, ccr_program, sr_program) in [
        ("ANDI", ENCODING_ANDI_TO_CCR, ENCODING_ANDI_TO_SR),
        ("ORI", ENCODING_ORI_TO_CCR, ENCODING_ORI_TO_SR),
        ("EORI", ENCODING_EORI_TO_CCR, ENCODING_EORI_TO_SR),
    ] {
        for (priority, (program, class, diagnostic)) in [
            (ccr_program, 6, DIAG_STATUS_BYTE_IMMEDIATE_RANGE),
            (sr_program, 5, DIAG_STATUS_WORD_IMMEDIATE_RANGE),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "immediate_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,reg1.class{class}{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{diagnostic}"
                ),
                priority: priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (mnemonic, program)) in [
        ("MOVE.B", ENCODING_MOVE_BYTE_INDIRECT_TO_DATA),
        ("MOVE.W", ENCODING_MOVE_WORD_INDIRECT_TO_DATA),
        ("MOVE.L", ENCODING_MOVE_LONG_INDIRECT_TO_DATA),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program, _, structural_prefix)) in
        MOVE_UPDATE_SOURCE_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{structural_prefix}0.class1,reg1.class0"
            ),
            priority: (priority + 16) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, base, structural_prefix)) in [
        (
            "MOVEA.W",
            0x3058_u32,
            MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX,
        ),
        (
            "MOVEA.L",
            0x2058,
            MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX,
        ),
        (
            "MOVEA.W",
            0x3060,
            MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX,
        ),
        (
            "MOVEA.L",
            0x2060,
            MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX,
        ),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{structural_prefix}0.class1,reg1.class1"
            ),
            priority: 24 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, base)) in [
        ("MOVE.B", 0x1098_u32),
        ("MOVE.W", 0x3098),
        ("MOVE.L", 0x2098),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"
            ),
            priority: (24 + priority) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program, _)) in MOVE_DISPLACEMENT_SOURCE_PROGRAMS
        .iter()
        .copied()
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2"
            ),
            priority: (priority + 32) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program, base)) in
        MOVE_INDEXED_SOURCE_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3"
            ),
            priority: (priority + 48) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_INDEXED_LONG_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}L.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3",
                base
            ),
            priority: (priority + 52) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_INDEXED_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0.class1,reg1.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2"
            ),
            priority: (priority + 55) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        for (qualifier_index, (qualifier, long_bit)) in
            [("W", 0_u32), ("L", 1_u32)].into_iter().enumerate()
        {
            for index_class in [0_u16, 1_u16] {
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "direct_register".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_IDENTITY_SCALE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0.class1,reg1.class0;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
                    ),
                    priority: 60 + qualifier_index as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
    }
    for (priority, (mnemonic, _program, base)) in MOVE_PC_DISPLACEMENT_SOURCE_PROGRAMS
        .iter()
        .copied()
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2,reg1.class0;fixup:{FIXUP_PC_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
            ),
            priority: 56 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        for (destination_priority, destination_plan) in [
            "target:expr1".to_string(),
            format!(
                "target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
            ),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,{destination_plan};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{};fixup:{FIXUP_PC_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{destination_plan}",
                    base + 0x3c0
                ),
                priority: 70 + priority as u16 * 2 + destination_priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (mnemonic, _program, base)) in
        MOVE_PC_INDEXED_SOURCE_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}3,reg1.class0;encode:{PARAM_INDEX_PREFIX}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class0;encode:{PARAM_SCALAR_BYTE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
            ),
            priority: 58 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}3,reg1.class0;encode:{PARAM_INDEX_PREFIX}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}L.class0;encode:{PARAM_SCALAR_BYTE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
            ),
            priority: 59 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}3,reg1.class0;encode:{PARAM_INDEX_PREFIX}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}L.class0;fixup:{FIXUP_PC_BYTE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
            ),
            priority: 60 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}3,reg1.class0;encode:{PARAM_INDEX_PREFIX}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0;fixup:{FIXUP_PC_BYTE}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
            ),
            priority: 61 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        for (qualifier_index, (qualifier, long_bit)) in
            [("W", 0_u32), ("L", 1_u32)].into_iter().enumerate()
        {
            for index_class in [0_u16, 1_u16] {
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "direct_register".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_IDENTITY_SCALE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}3,reg1.class0;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0"
                    ),
                    priority: 64 + qualifier_index as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
    }
    for (field, programs, priority_base) in [
        ("W", MOVE_ABSOLUTE_WORD_SOURCE_PROGRAMS, 64_u16),
        ("L", MOVE_ABSOLUTE_LONG_SOURCE_PROGRAMS, 72_u16),
    ] {
        for (priority, (mnemonic, program, base)) in programs.iter().copied().enumerate() {
            let value_source = if field == "W" {
                MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX
            } else {
                MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX
            };
            let diagnostic = if field == "W" {
                format!("{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_ABSOLUTE_WORD_RANGE}")
            } else {
                String::new()
            };
            let operand_plan = if field == "L" {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L,reg1.class0;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,reg1.class0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                )
            } else {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{value_source}scalar.absolute-{field}:member0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field},reg1.class0{diagnostic}"
                )
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan,
                priority: priority_base + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (mnemonic, _, base)) in MOVE_ABSOLUTE_LONG_SOURCE_PROGRAMS
        .iter()
        .copied()
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,reg1.class0;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,reg1.class0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0"
            ),
            priority: 76 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program)) in [
        ("MOVE.B", "enc.move.b.absolute-word-to-data"),
        ("MOVE.W", "enc.move.w.absolute-word-to-data"),
        ("MOVE.L", "enc.move.l.absolute-long-to-data"),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,reg1.class0"
            ),
            priority: 80 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program, _, width, min, max)) in
        MOVE_IMMEDIATE_PROGRAMS.iter().copied().enumerate()
    {
        let destination_class = u16::from(mnemonic.starts_with("MOVEA"));
        let value_program = match (width, min, max) {
            (2, -128, 255) => VALUE_IMMEDIATE_BYTE,
            (2, -32_768, 65_535) => VALUE_IMMEDIATE_WORD,
            (4, -2_147_483_648, 4_294_967_295) => VALUE_IMMEDIATE_LONG,
            _ => unreachable!("declared MOVE immediate range"),
        };
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{value_program}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_SEPARATOR}expr0,reg1.class{destination_class}"
            ),
            priority: 80 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        mnemonic: "MOVE.L".to_string(),
        shape_key: "immediate_register".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target_atom:expr0,reg1.class0;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}8252,reg1.class0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0"
        ),
        priority: 76,
        unstable_widen: false,
        width_rank: 0,
    });
    for (mnemonic, _, immediate_base, _, _, _) in MOVE_IMMEDIATE_PROGRAMS.iter().copied().take(3) {
        let immediate_program = match mnemonic {
            "MOVE.B" => PARAM_IMMEDIATE_BYTE_FIELD_9,
            "MOVE.W" => PARAM_IMMEDIATE_WORD_FIELD_9,
            "MOVE.L" => PARAM_IMMEDIATE_LONG_FIELD_9,
            _ => unreachable!("declared MOVE immediate width"),
        };
        let value_program = match mnemonic {
            "MOVE.B" => VALUE_IMMEDIATE_BYTE,
            "MOVE.W" => VALUE_IMMEDIATE_WORD,
            "MOVE.L" => VALUE_IMMEDIATE_LONG,
            _ => unreachable!("declared MOVE immediate width"),
        };
        let immediate_value_plan = format!(
            "{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{value_program}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_SEPARATOR}expr0"
        );
        for (priority, (destination_mode_bits, destination_plan)) in [
            (
                0x80_u32,
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                0xc0,
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                0x100,
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "immediate_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{immediate_value_plan},{destination_plan}",
                    immediate_base + destination_mode_bits
                ),
                priority: 112 + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        if mnemonic == "MOVE.L" {
            for (priority, (destination_mode_bits, destination_plan)) in [
                (
                    0x80_u32,
                    format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
                ),
                (
                    0xc0,
                    format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"),
                ),
                (
                    0x100,
                    format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"),
                ),
            ]
            .into_iter()
            .enumerate()
            {
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "immediate_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target_atom:expr0,{destination_plan};encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{destination_plan};fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0",
                        immediate_base + destination_mode_bits
                    ),
                    priority: 108 + priority as u16,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2;encode:{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{immediate_value_plan},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                immediate_base + 0x140
            ),
            priority: 115,
            unstable_widen: false,
            width_rank: 0,
        });
        for (priority, (qualifier, long_bit)) in
            [("W", 0_u32), ("L", 1_u32)].into_iter().enumerate()
        {
            for index_class in [0_u16, 1_u16] {
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "immediate_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3;encode:{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{immediate_value_plan},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                        immediate_base + 0x180
                    ),
                    priority: 116 + priority as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W;encode:{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{immediate_value_plan},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}scalar.absolute-W:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_ABSOLUTE_WORD_RANGE}",
                immediate_base + 0x1c0
            ),
            priority: 120,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L;encode:{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{immediate_value_plan},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L",
                immediate_base + 0x1c0
            ),
            priority: 121,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,target:expr1;encode:{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{immediate_value_plan},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1",
                immediate_base + 0x1c0
            ),
            priority: 122,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors.push(ModeSelectorDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        mnemonic: "MOVE.L".to_string(),
        shape_key: "immediate_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target_atom:expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
            0x203c_u32 + 0x140
        ),
        priority: 123,
        unstable_widen: false,
        width_rank: 0,
    });
    for (mnemonic, source_base) in [
        ("MOVE.B", 0x1039_u32),
        ("MOVE.W", 0x3039_u32),
        ("MOVE.L", 0x2039_u32),
    ] {
        for (priority, (destination_mode_bits, destination_plan, destination_register)) in [
            (
                0x80_u32,
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                0xc0,
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"),
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                0x100,
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"),
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,{destination_plan};encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{destination_register};fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0",
                    source_base + destination_mode_bits
                ),
                priority: 124 + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    selectors.push(ModeSelectorDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        mnemonic: "MOVE.L".to_string(),
        shape_key: "direct_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,target:expr1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}9209;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1"
        ),
        priority: 127,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        mnemonic: "MOVE.L".to_string(),
        shape_key: "direct_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,expr1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}9209;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0;encode:{PARAM_SCALAR_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr1"
        ),
        priority: 128,
        unstable_widen: false,
        width_rank: 0,
    });
    selectors.push(ModeSelectorDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        mnemonic: "MOVE.L".to_string(),
        shape_key: "immediate_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_FIXED_EXTENSION_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}9212,expr0;encode:{PARAM_SCALAR_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr1"
        ),
        priority: 100,
        unstable_widen: false,
        width_rank: 0,
    });
    for (priority, (mnemonic, _program, base, source_class, destination_class)) in
        MOVE_REGISTER_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},reg0.class{source_class},reg1.class{destination_class}"
            ),
            priority: 96 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (mnemonic, size_base) in [
        ("MOVE.B", 0x1000_u32),
        ("MOVE.W", 0x3000_u32),
        ("MOVE.L", 0x2000_u32),
    ] {
        for (source_mode_bits, source_plan) in [
            (
                0x10_u32,
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1"),
            ),
            (
                0x18,
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1"),
            ),
            (
                0x20,
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}0.class1"),
            ),
        ] {
            for (destination_mode_bits, destination_plan) in [
                (
                    0x80_u32,
                    format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
                ),
                (
                    0xc0,
                    format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"),
                ),
                (
                    0x100,
                    format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"),
                ),
            ] {
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "direct_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{source_plan},{destination_plan}",
                        size_base + source_mode_bits + destination_mode_bits
                    ),
                    priority: 96,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{source_plan},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{source_plan},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                    size_base + source_mode_bits + 0x140
                ),
                priority: 97,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (mnemonic, size_base) in [
        ("MOVE.B", 0x1000_u32),
        ("MOVE.W", 0x3000_u32),
        ("MOVE.L", 0x2000_u32),
    ] {
        for (priority, (destination_mode_bits, destination_plan)) in [
            (
                0x80_u32,
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                0xc0,
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                0x100,
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,{destination_plan};encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{destination_plan};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                    size_base + 0x28 + destination_mode_bits
                ),
                priority: 156 + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,target:expr1;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1",
                size_base + 0x28 + 0x3c0
            ),
            priority: 159,
            unstable_widen: false,
            width_rank: 0,
        });
        for (destination_offset, (destination_mode_bits, destination_plan)) in [
            (
                0x80_u32,
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                0xc0,
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                0x100,
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
        ]
        .into_iter()
        .enumerate()
        {
            for (qualifier_offset, (qualifier, long_bit)) in
                [("W", 0_u32), ("L", 1_u32)].into_iter().enumerate()
            {
                for index_class in [0_u16, 1_u16] {
                    selectors.push(ModeSelectorDescriptor {
                        owner: ScopedOwner::Family("motorola68000".to_string()),
                        mnemonic: mnemonic.to_string(),
                        shape_key: "direct_direct".to_string(),
                        mode_key: "semantic".to_string(),
                        operand_plan: format!(
                            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3,{destination_plan};encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{destination_plan};encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                            size_base + 0x30 + destination_mode_bits
                        ),
                        priority: 180
                            + destination_offset as u16 * 4
                            + qualifier_offset as u16 * 2
                            + index_class,
                        unstable_widen: false,
                        width_rank: 0,
                    });
                }
            }
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                size_base + 0x28 + 0x140
            ),
            priority: 159,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                size_base + 0x39 + 0x140
            ),
            priority: 160,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                size_base + 0x28 + 0xc0
            ),
            priority: 160,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L,{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L",
                size_base + 0x39 + 0xc0
            ),
            priority: 161,
            unstable_widen: false,
            width_rank: 0,
        });
        for (priority, (qualifier, long_bit)) in
            [("W", 0_u32), ("L", 1_u32)].into_iter().enumerate()
        {
            for index_class in [0_u16, 1_u16] {
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "direct_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                        size_base + 0x28 + 0x180
                    ),
                    priority: 162 + priority as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "direct_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                        size_base + 0x30 + 0x140
                    ),
                    priority: 166 + priority as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "direct_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3,target:expr1;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1",
                        size_base + 0x30 + 0x3c0
                    ),
                    priority: 168 + priority as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "direct_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                        size_base + 0x39 + 0x180
                    ),
                    priority: 170 + priority as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "direct_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                        size_base + 0x39 + 0x180
                    ),
                    priority: 174 + priority as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
    }
    for (mnemonic, source_base) in [("MOVE.W", 0x3008_u32), ("MOVE.L", 0x2008_u32)] {
        for (priority, (destination_mode_bits, destination_plan)) in [
            (
                0x80_u32,
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                0xc0,
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                0x100,
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class1,{destination_plan}",
                    source_base + destination_mode_bits
                ),
                priority: 128 + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_DISPLACEMENT_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2",
                source_base + 0x140
            ),
            priority: 131,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class1,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}scalar.absolute-W:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_ABSOLUTE_WORD_RANGE}",
                source_base + 0x1c0
            ),
            priority: 132,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class1,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L",
                source_base + 0x3c0
            ),
            priority: 133,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, _program, base, source_shift, destination_shift)) in
        BINARY_REGISTER_PROGRAMS.iter().copied().enumerate()
    {
        let program = match (source_shift, destination_shift) {
            (0, 9) => PARAM_FIELDS_0_9,
            (9, 0) => PARAM_FIELDS_9_0,
            _ => unreachable!("declared binary-register field layout"),
        };
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},reg0.class0,reg1.class0"
            ),
            priority: 128 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, mnemonic) in ["CMP.B", "CMP.W", "CMP.L"].into_iter().enumerate() {
        for (mode, structural_destination) in [
            (
                0_u16,
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                1,
                format!(
                    "{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W"
                ),
            ),
            (
                2,
                format!(
                    "{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                ),
            ),
        ] {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_TRAP_VECTOR}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16,reg0.class0,{structural_destination}{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_INVALID_DESTINATION}"
                ),
                priority: 144 + priority as u16 * 3 + mode,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (canonical, _alias, _program, base, width, min, max)) in
        IMMEDIATE_REGISTER_PROGRAMS.iter().copied().enumerate()
    {
        let program = match (width, min, max) {
            (2, -128, 255) => PARAM_IMMEDIATE_BYTE_FIELD_0,
            (2, -32_768, 65_535) => PARAM_IMMEDIATE_WORD_FIELD_0,
            (4, min, max) if min == i32::MIN as i64 && max == u32::MAX as i64 => {
                PARAM_IMMEDIATE_LONG_FIELD_0
            }
            _ => unreachable!("declared immediate-register range"),
        };
        let value_program = match (width, min, max) {
            (2, -128, 255) => VALUE_IMMEDIATE_BYTE,
            (2, -32_768, 65_535) => VALUE_IMMEDIATE_WORD,
            (4, min, max) if min == i32::MIN as i64 && max == u32::MAX as i64 => {
                VALUE_IMMEDIATE_LONG
            }
            _ => unreachable!("declared immediate-register range"),
        };
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: canonical.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{value_program}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_SEPARATOR}expr0,reg1.class0"
            ),
            priority: 160 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: canonical.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_TRAP_VECTOR}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16,expr0,reg1.class1{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_INVALID_DESTINATION}"
            ),
            priority: 176 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, _program, base)) in
        UNARY_REGISTER_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},reg0.class0"
            ),
            priority: 192 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{ENCODING_TRAP_VECTOR}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}16,reg0.class1{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_INVALID_DESTINATION}"
            ),
            priority: 208 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (instruction, _, base) in UNARY_REGISTER_PROGRAMS.iter().copied() {
        for (mode_offset, mode, structural_plan) in [
            (
                0_u16,
                "indirect",
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1"),
            ),
            (
                1,
                "postincrement",
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1"),
            ),
            (
                2,
                "predecrement",
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}0.class1"),
            ),
            (
                3,
                "displacement",
                format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2"
                ),
            ),
            (
                4,
                "indexed-word",
                format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3"
                ),
            ),
        ] {
            let (program, mode_bits) = match mode {
                "indirect" => (PARAM_FIELD_0, 0x10_u32),
                "postincrement" => (PARAM_FIELD_0, 0x18),
                "predecrement" => (PARAM_FIELD_0, 0x20),
                "displacement" => (PARAM_DISPLACEMENT_0, 0x28),
                "indexed-word" => (PARAM_INDEXED_0, 0x30),
                _ => unreachable!("declared unary addressing mode"),
            };
            let operand_plan = if mode == "displacement" {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2;encode:{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                    base + mode_bits
                )
            } else {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{structural_plan}",
                    base + mode_bits
                )
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan,
                priority: 224 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for (priority, (qualifier, long_bit)) in
            [("W", 0_u32), ("L", 1_u32)].into_iter().enumerate()
        {
            for index_class in [0_u16, 1_u16] {
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: instruction.to_string(),
                    shape_key: "direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                        base + 0x30
                    ),
                    priority: 232 + priority as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: instruction.to_string(),
                    shape_key: "direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0.class1;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0",
                        base + 0x30
                    ),
                    priority: 236 + priority as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
        for (mode_offset, field, mode) in [(5_u16, "W", "absolute-word"), (6, "L", "absolute-long")]
        {
            let (program, mode_bits) = match mode {
                "absolute-word" => (PARAM_FIXED_EXTENSION_WORD, 0x38_u32),
                "absolute-long" => (PARAM_FIXED_EXTENSION_LONG, 0x39),
                _ => unreachable!("declared unary absolute mode"),
            };
            let operand_plan = if mode == "absolute-long" {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{};fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L",
                    base + mode_bits
                )
            } else {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}scalar.absolute-{field}:member0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field}",
                    base + mode_bits
                )
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan,
                priority: 224 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{};fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0",
                base + 0x39
            ),
            priority: 231,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (instruction, base)) in REGISTER_COUNT_SHIFT_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},reg0.class0,reg1.class0"
            ),
            priority: 224 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (instruction, base)) in
        IMMEDIATE_COUNT_SHIFT_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_PACKED_THREE_BIT_COUNT}:expr0,reg1.class0{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_COUNT_RANGE}"
            ),
            priority: 248 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (instruction, base) in MEMORY_SHIFT_PROGRAMS.iter().copied() {
        for (mode_offset, mode, structural_plan) in [
            (
                0_u16,
                "indirect",
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1"),
            ),
            (
                1,
                "postincrement",
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1"),
            ),
            (
                2,
                "predecrement",
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}0.class1"),
            ),
            (
                3,
                "displacement",
                format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2"
                ),
            ),
            (
                4,
                "indexed-word",
                format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3"
                ),
            ),
        ] {
            let (program, mode_bits) = match mode {
                "indirect" => (PARAM_FIELD_0, 0x10_u32),
                "postincrement" => (PARAM_FIELD_0, 0x18),
                "predecrement" => (PARAM_FIELD_0, 0x20),
                "displacement" => (PARAM_DISPLACEMENT_0, 0x28),
                "indexed-word" => (PARAM_INDEXED_0, 0x30),
                _ => unreachable!("declared memory-shift addressing mode"),
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{structural_plan}",
                    base + mode_bits
                ),
                priority: 240 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for (mode_offset, field, mode) in [(5_u16, "W", "absolute-word"), (6, "L", "absolute-long")]
        {
            let (program, mode_bits) = match mode {
                "absolute-word" => (PARAM_FIXED_EXTENSION_WORD, 0x38_u32),
                "absolute-long" => (PARAM_FIXED_EXTENSION_LONG, 0x39),
                _ => unreachable!("declared memory-shift absolute mode"),
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}scalar.absolute-{field}:member0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field}",
                    base + mode_bits
                ),
                priority: 240 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{};fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0",
                base + 0x39
            ),
            priority: 247,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (instruction, base) in DYNAMIC_BIT_PROGRAMS.iter().copied() {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "register_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},reg0.class0,reg1.class0"
            ),
            priority: 248,
            unstable_widen: false,
            width_rank: 0,
        });
        for (mode_offset, mode, structural_plan) in [
            (
                0_u16,
                "indirect",
                format!("reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                1,
                "postincrement",
                format!(
                    "reg0.class0,{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"
                ),
            ),
            (
                2,
                "predecrement",
                format!(
                    "reg0.class0,{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"
                ),
            ),
            (
                3,
                "displacement",
                format!(
                    "reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2"
                ),
            ),
            (
                4,
                "indexed-word",
                format!(
                    "reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3"
                ),
            ),
        ] {
            let (program, mode_bits) = match mode {
                "indirect" => (PARAM_FIELDS_9_0, 0x10_u32),
                "postincrement" => (PARAM_FIELDS_9_0, 0x18),
                "predecrement" => (PARAM_FIELDS_9_0, 0x20),
                "displacement" => (PARAM_DISPLACEMENT_9_0, 0x28),
                "indexed-word" => (PARAM_INDEXED_9_0, 0x30),
                _ => unreachable!("declared dynamic-bit mode"),
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{structural_plan}",
                    base + mode_bits
                ),
                priority: 249 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for (mode_offset, field, mode) in [(5_u16, "W", "absolute-word"), (6, "L", "absolute-long")]
        {
            let (program, mode_bits) = match mode {
                "absolute-word" => (PARAM_EXTENSION_WORD_9, 0x38_u32),
                "absolute-long" => (PARAM_EXTENSION_LONG_9, 0x39),
                _ => unreachable!("declared dynamic-bit absolute mode"),
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0,{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}scalar.absolute-{field}:member1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field}",
                    base + mode_bits
                ),
                priority: 249 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,target:expr1;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1",
                base + 0x39
            ),
            priority: 256,
            unstable_widen: false,
            width_rank: 0,
        });
        if instruction == "BTST" {
            for (mode_offset, mode, structural_plan) in [
                (
                    7_u16,
                    "pc-displacement",
                    format!(
                        "reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2"
                    ),
                ),
                (
                    8,
                    "pc-indexed-word",
                    format!(
                        "reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3"
                    ),
                ),
            ] {
                let (program, mode_bits) = match mode {
                    "pc-displacement" => (PARAM_FIELD_9_DISPLACEMENT, 0x3a_u32),
                    "pc-indexed-word" => (PARAM_FIELD_9_PC_INDEXED, 0x3b),
                    _ => unreachable!("declared dynamic-bit PC mode"),
                };
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: instruction.to_string(),
                    shape_key: "register_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{structural_plan}",
                        base + mode_bits
                    ),
                    priority: 249 + mode_offset,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
    }
    for (instruction, base) in STATIC_BIT_PROGRAMS.iter().copied() {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_STATIC_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},expr0,reg1.class0"
            ),
            priority: 258,
            unstable_widen: false,
            width_rank: 0,
        });
        for (mode_offset, mode, structural_plan) in [
            (
                0_u16,
                "indirect",
                format!("expr0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                1,
                "postincrement",
                format!(
                    "expr0,{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"
                ),
            ),
            (
                2,
                "predecrement",
                format!(
                    "expr0,{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"
                ),
            ),
            (
                3,
                "displacement",
                format!(
                    "expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2"
                ),
            ),
            (
                4,
                "indexed-word",
                format!(
                    "expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3"
                ),
            ),
        ] {
            let (program, mode_bits) = match mode {
                "indirect" => (PARAM_STATIC_FIELD_0, 0x10_u32),
                "postincrement" => (PARAM_STATIC_FIELD_0, 0x18),
                "predecrement" => (PARAM_STATIC_FIELD_0, 0x20),
                "displacement" => (PARAM_STATIC_DISPLACEMENT_0, 0x28),
                "indexed-word" => (PARAM_STATIC_INDEXED_0, 0x30),
                _ => unreachable!("declared static-bit mode"),
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "immediate_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{structural_plan}",
                    base + mode_bits
                ),
                priority: 259 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for (mode_offset, field, mode) in [(5_u16, "W", "absolute-word"), (6, "L", "absolute-long")]
        {
            let (program, mode_bits) = match mode {
                "absolute-word" => (PARAM_STATIC_EXTENSION_WORD, 0x38_u32),
                "absolute-long" => (PARAM_STATIC_EXTENSION_LONG, 0x39),
                _ => unreachable!("declared static-bit absolute mode"),
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "immediate_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},expr0,{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}scalar.absolute-{field}:member1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field}",
                    base + mode_bits
                ),
                priority: 259 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,target:expr1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{};encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1",
                base + 0x39
            ),
            priority: 266,
            unstable_widen: false,
            width_rank: 0,
        });
        if instruction == "BTST" {
            for (mode_offset, mode, structural_plan) in [
                (
                    7_u16,
                    "pc-displacement",
                    format!(
                        "expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2"
                    ),
                ),
                (
                    8,
                    "pc-indexed-word",
                    format!(
                        "expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3"
                    ),
                ),
            ] {
                let (program, mode_bits) = match mode {
                    "pc-displacement" => (PARAM_STATIC_PC_DISPLACEMENT, 0x3a_u32),
                    "pc-indexed-word" => (PARAM_STATIC_PC_INDEXED, 0x3b),
                    _ => unreachable!("declared static-bit PC mode"),
                };
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: instruction.to_string(),
                    shape_key: "immediate_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{structural_plan}",
                        base + mode_bits
                    ),
                    priority: 259 + mode_offset,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
    }
    for (instruction, base, allow_address_register) in QUICK_PROGRAMS.iter().copied() {
        for (class, _mode, mode_bits, priority) in
            [(0_u16, "data", 0_u32, 280_u16), (1, "address", 0x08, 281)]
        {
            if class == 1 && !allow_address_register {
                continue;
            }
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "immediate_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_PACKED_THREE_BIT_COUNT}:expr0,reg1.class{class}{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_COUNT_RANGE}",
                    base + mode_bits
                ),
                priority: priority + mode_bits as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for (mode_offset, mode, structural_plan) in [
            (
                0_u16,
                "indirect",
                format!(
                    "{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_PACKED_THREE_BIT_COUNT}:expr0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"
                ),
            ),
            (
                1,
                "postincrement",
                format!(
                    "{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_PACKED_THREE_BIT_COUNT}:expr0,{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"
                ),
            ),
            (
                2,
                "predecrement",
                format!(
                    "{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_PACKED_THREE_BIT_COUNT}:expr0,{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"
                ),
            ),
            (
                3,
                "displacement",
                format!(
                    "{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_PACKED_THREE_BIT_COUNT}:expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2"
                ),
            ),
            (
                4,
                "indexed-word",
                format!(
                    "{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_PACKED_THREE_BIT_COUNT}:expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3"
                ),
            ),
        ] {
            let (program, mode_bits) = match mode {
                "indirect" => (PARAM_FIELDS_9_0, 0x10_u32),
                "postincrement" => (PARAM_FIELDS_9_0, 0x18),
                "predecrement" => (PARAM_FIELDS_9_0, 0x20),
                "displacement" => (PARAM_DISPLACEMENT_9_0, 0x28),
                "indexed-word" => (PARAM_INDEXED_9_0, 0x30),
                _ => unreachable!("declared quick addressing mode"),
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "immediate_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{structural_plan}{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_COUNT_RANGE}",
                    base + mode_bits
                ),
                priority: 282 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for (qualifier, long_bit) in [("W", 0_u32), ("L", 1_u32)] {
            for index_class in [0_u16, 1_u16] {
                if qualifier == "W" && index_class == 0 {
                    continue;
                }
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: instruction.to_string(),
                    shape_key: "immediate_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_PACKED_THREE_BIT_COUNT}:expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3;encode:{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_PACKED_THREE_BIT_COUNT}:expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                        base + 0x30
                    ),
                    priority: 288 + long_bit as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
        for (mode_offset, field, mode) in [(5_u16, "W", "absolute-word"), (6, "L", "absolute-long")]
        {
            let mode_bits = match mode {
                "absolute-word" => 0x38_u32,
                "absolute-long" => 0x39,
                _ => unreachable!("declared quick absolute mode"),
            };
            let operand_plan = if field == "L" {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_PACKED_THREE_BIT_COUNT}:expr0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_COUNT_RANGE}",
                    base + mode_bits
                )
            } else {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_EXTENSION_WORD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_PACKED_THREE_BIT_COUNT}:expr0,{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}scalar.absolute-W:member1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_COUNT_RANGE}",
                    base + mode_bits
                )
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "immediate_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan,
                priority: 282 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,target:expr1;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_PACKED_THREE_BIT_COUNT}:expr0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_COUNT_RANGE}",
                base + 0x39
            ),
            priority: 289,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (instruction, base) in EA_TO_DATA_ALU_PROGRAMS.iter().copied() {
        let immediate_program = if instruction.ends_with(".B") {
            PARAM_IMMEDIATE_BYTE_FIELD_9
        } else if instruction.ends_with(".W") {
            PARAM_IMMEDIATE_WORD_FIELD_9
        } else {
            PARAM_IMMEDIATE_LONG_FIELD_9
        };
        if !instruction.ends_with(".B") {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "register_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class1,reg1.class0",
                    base + 0x08
                ),
                priority: 222,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "immediate_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},expr0,reg1.class0",
                base + 0x3c
            ),
            priority: 223,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,reg1.class0;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0",
                base + 0x39
            ),
            priority: 223,
            unstable_widen: false,
            width_rank: 0,
        });
        for (mode_offset, mode, structural_plan) in [
            (
                0_u16,
                "indirect-to-data",
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0"),
            ),
            (
                1,
                "postincrement-to-data",
                format!(
                    "{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0"
                ),
            ),
            (
                2,
                "predecrement-to-data",
                format!(
                    "{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class0"
                ),
            ),
            (
                3,
                "displacement-to-data",
                format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2"
                ),
            ),
            (
                4,
                "indexed-word-to-data",
                format!(
                    "{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3"
                ),
            ),
            (
                7,
                "pc-displacement-to-data",
                format!(
                    "reg1.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2"
                ),
            ),
            (
                8,
                "pc-indexed-word-to-data",
                format!(
                    "reg1.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3"
                ),
            ),
        ] {
            let (program, mode_bits) = match mode {
                "indirect-to-data" => (PARAM_FIELDS_0_9, 0x10_u32),
                "postincrement-to-data" => (PARAM_FIELDS_0_9, 0x18),
                "predecrement-to-data" => (PARAM_FIELDS_0_9, 0x20),
                "displacement-to-data" => (PARAM_DISPLACEMENT_0_9, 0x28),
                "indexed-word-to-data" => (PARAM_INDEXED_0_9, 0x30),
                "pc-displacement-to-data" => (PARAM_FIELD_9_DISPLACEMENT, 0x3a),
                "pc-indexed-word-to-data" => (PARAM_FIELD_9_PC_INDEXED, 0x3b),
                _ => unreachable!("declared EA-to-data mode"),
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{structural_plan}",
                    base + mode_bits
                ),
                priority: 224 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for (priority, (qualifier, long_bit)) in
            [("W", 0_u32), ("L", 1_u32)].into_iter().enumerate()
        {
            for index_class in [0_u16, 1_u16] {
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: instruction.to_string(),
                    shape_key: "direct_register".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class0;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                        base + 0x30
                    ),
                    priority: 232 + priority as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
        for (mode_offset, field, mode) in [
            (5_u16, "W", "absolute-word-to-data"),
            (6, "L", "absolute-long-to-data"),
        ] {
            let (program, mode_bits) = match mode {
                "absolute-word-to-data" => (PARAM_EXTENSION_WORD_9, 0x38_u32),
                "absolute-long-to-data" => (PARAM_EXTENSION_LONG_9, 0x39),
                _ => unreachable!("declared EA-to-data absolute mode"),
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class0,{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}scalar.absolute-{field}:member0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field}",
                    base + mode_bits
                ),
                priority: 224 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (instruction, _, _, base, immediate_width, _, _) in
        IMMEDIATE_REGISTER_PROGRAMS.iter().copied()
    {
        let immediate_program = match immediate_width {
            2 if instruction.ends_with(".B") => PARAM_IMMEDIATE_BYTE_FIELD_0,
            2 => PARAM_IMMEDIATE_WORD_FIELD_0,
            4 => PARAM_IMMEDIATE_LONG_FIELD_0,
            _ => unreachable!("declared immediate extension width"),
        };
        for (priority, (mode_bits, structural_plan)) in [
            (
                0x10_u32,
                format!("{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                0x18,
                format!("{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                0x20,
                format!("{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
        ]
        .into_iter()
        .enumerate()
        {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "immediate_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},expr0,{structural_plan}",
                    base + mode_bits
                ),
                priority: (320 + priority) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2;encode:{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                base + 0x28
            ),
            priority: 323,
            unstable_widen: false,
            width_rank: 0,
        });
        for (priority, (qualifier, long_bit)) in
            [("W", 0_u32), ("L", 1_u32)].into_iter().enumerate()
        {
            for index_class in [0_u16, 1_u16] {
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: instruction.to_string(),
                    shape_key: "immediate_direct".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3;encode:{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                        base + 0x30
                    ),
                    priority: 327 + priority as u16 * 2 + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
        for (priority, (field, mode_bits, scalar_program)) in [
            ("W", 0x38_u32, PARAM_SCALAR_WORD),
            ("L", 0x39_u32, PARAM_SCALAR_LONG),
        ]
        .into_iter()
        .enumerate()
        {
            let mode_register = mode_bits & 0x07;
            let operand_plan = if field == "L" {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L;encode:{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},expr0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{mode_register};fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L",
                    base + mode_bits
                )
            } else {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W;encode:{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},expr0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{mode_register};encode:{scalar_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}scalar.absolute-W:member1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W",
                    base + mode_bits
                )
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "immediate_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan,
                priority: (324 + priority) as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "immediate_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,target:expr1;encode:{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},expr0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1",
                base + 0x39
            ),
            priority: 336,
            unstable_widen: false,
            width_rank: 0,
        });
        if instruction.starts_with("CMPI.") {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "immediate_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2;encode:{immediate_program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},expr0,{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                    base + 0x38
                ),
                priority: 326,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (instruction, base) in DATA_TO_EA_ALU_PROGRAMS.iter().copied() {
        for (mode_offset, mode, structural_plan) in [
            (
                0_u16,
                "indirect",
                format!("reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1"),
            ),
            (
                1,
                "postincrement",
                format!(
                    "reg0.class0,{MODE_SELECTOR_PLAN_UNARY_PLUS_INDIRECT_REGISTER_PREFIX}1.class1"
                ),
            ),
            (
                2,
                "predecrement",
                format!(
                    "reg0.class0,{MODE_SELECTOR_PLAN_UNARY_MINUS_INDIRECT_REGISTER_PREFIX}1.class1"
                ),
            ),
            (
                3,
                "displacement",
                format!(
                    "reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2"
                ),
            ),
            (
                4,
                "indexed-word",
                format!(
                    "reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3"
                ),
            ),
        ] {
            let (program, mode_bits) = match mode {
                "indirect" => (PARAM_FIELDS_9_0, 0x10_u32),
                "postincrement" => (PARAM_FIELDS_9_0, 0x18),
                "predecrement" => (PARAM_FIELDS_9_0, 0x20),
                "displacement" => (PARAM_DISPLACEMENT_9_0, 0x28),
                "indexed-word" => (PARAM_INDEXED_9_0, 0x30),
                _ => unreachable!("declared data-to-EA mode"),
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{structural_plan}",
                    base + mode_bits
                ),
                priority: 224 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        for (mode_offset, field, mode) in [(5_u16, "W", "absolute-word"), (6, "L", "absolute-long")]
        {
            let (program, mode_bits) = match mode {
                "absolute-word" => (PARAM_EXTENSION_WORD_9, 0x38_u32),
                "absolute-long" => (PARAM_EXTENSION_LONG_9, 0x39),
                _ => unreachable!("declared data-to-EA absolute mode"),
            };
            let operand_plan = if field == "L" {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L",
                    base + mode_bits
                )
            } else {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0,{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}scalar.absolute-{field}:member1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field}",
                    base + mode_bits
                )
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: instruction.to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan,
                priority: 224 + mode_offset,
                unstable_widen: false,
                width_rank: 0,
            });
        }
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: instruction.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,target:expr1;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1",
                base + 0x39
            ),
            priority: 231,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program)) in [
        ("MOVE.B", ENCODING_MOVE_BYTE_DATA_TO_INDIRECT),
        ("MOVE.W", ENCODING_MOVE_WORD_DATA_TO_INDIRECT),
        ("MOVE.L", ENCODING_MOVE_LONG_DATA_TO_INDIRECT),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}1.class1,reg0.class0"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program, _, structural_prefix)) in
        MOVE_UPDATE_DESTINATION_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{structural_prefix}1.class1,reg0.class0"
            ),
            priority: (priority + 16) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, _, base)) in MOVE_DISPLACEMENT_DESTINATION_PROGRAMS
        .iter()
        .copied()
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2;encode:{PARAM_DISPLACEMENT_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
            ),
            priority: (priority + 32) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program, _)) in MOVE_INDEXED_DESTINATION_PROGRAMS
        .iter()
        .copied()
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "register_direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg0.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3"
            ),
            priority: (priority + 48) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (mnemonic, _, base) in MOVE_INDEXED_DESTINATION_PROGRAMS.iter().copied() {
        let source_classes: &[u16] = if mnemonic == "MOVE.B" { &[0] } else { &[0, 1] };
        for source_class in source_classes.iter().copied() {
            for (priority, (qualifier, long_bit)) in
                [("W", 0_u32), ("L", 1_u32)].into_iter().enumerate()
            {
                for index_class in [0_u16, 1_u16] {
                    selectors.push(ModeSelectorDescriptor {
                        owner: ScopedOwner::Family("motorola68000".to_string()),
                        mnemonic: mnemonic.to_string(),
                        shape_key: "register_direct".to_string(),
                        mode_key: "semantic".to_string(),
                        operand_plan: format!(
                            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{source_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value3;encode:{PARAM_FIELDS_9_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg0.class{source_class};encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
                            base + if source_class == 1 { 0x08 } else { 0 }
                        ),
                        priority: 52 + priority as u16 * 2 + index_class + source_class * 4,
                        unstable_widen: false,
                        width_rank: 0,
                    });
                }
            }
        }
    }
    for (field, programs, priority_base) in [
        ("W", MOVE_ABSOLUTE_WORD_DESTINATION_PROGRAMS, 64_u16),
        ("L", MOVE_ABSOLUTE_LONG_DESTINATION_PROGRAMS, 72_u16),
    ] {
        for (priority, (mnemonic, program, base)) in programs.iter().copied().enumerate() {
            let value_source = if field == "W" {
                MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX
            } else {
                MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX
            };
            let diagnostic = if field == "W" {
                format!("{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_ABSOLUTE_WORD_RANGE}")
            } else {
                String::new()
            };
            let operand_plan = if field == "L" {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class0,{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},reg0.class0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                )
            } else {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{value_source}scalar.absolute-{field}:member1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field},reg0.class0{diagnostic}"
                )
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan,
                priority: priority_base + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (mnemonic, _, base)) in MOVE_ABSOLUTE_LONG_DESTINATION_PROGRAMS
        .iter()
        .copied()
        .enumerate()
    {
        let source_classes: &[u16] = if mnemonic == "MOVE.B" { &[0] } else { &[0, 1] };
        for source_class in source_classes.iter().copied() {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "register_direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg0.class{source_class},target:expr1;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg0.class{source_class};fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1",
                    base + if source_class == 1 { 0x08 } else { 0 }
                ),
                priority: 76 + priority as u16 + source_class * 4,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (mnemonic, program)) in [
        ("MOVEA.W", ENCODING_MOVEA_WORD_INDIRECT),
        ("MOVEA.L", ENCODING_MOVEA_LONG_INDIRECT),
        ("LEA", ENCODING_LEA_INDIRECT),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1,reg1.class1"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program, _)) in
        REGISTER_DISPLACEMENT_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2"
            ),
            priority: (priority + 32) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program, _)) in REGISTER_INDEXED_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3"
            ),
            priority: (priority + 48) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, _, base)) in REGISTER_INDEXED_PROGRAMS.iter().copied().enumerate() {
        for (qualifier_offset, (qualifier, long_bit)) in
            [("W", 0_u32), ("L", 1_u32)].into_iter().enumerate()
        {
            for index_class in [0_u16, 1_u16] {
                selectors.push(ModeSelectorDescriptor {
                    owner: ScopedOwner::Family("motorola68000".to_string()),
                    mnemonic: mnemonic.to_string(),
                    shape_key: "direct_register".to_string(),
                    mode_key: "semantic".to_string(),
                    operand_plan: format!(
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3,reg1.class1;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,reg1.class1;encode:{PARAM_INDEX_EXTENSION}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}{qualifier}.class{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{index_class},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{long_bit},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
                    ),
                    priority: 64
                        + priority as u16 * 4
                        + qualifier_offset as u16 * 2
                        + index_class,
                    unstable_widen: false,
                    width_rank: 0,
                });
            }
        }
    }
    for (priority, (mnemonic, _program, base)) in REGISTER_PC_DISPLACEMENT_PROGRAMS
        .iter()
        .copied()
        .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2,reg1.class1;fixup:{FIXUP_PC_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
            ),
            priority: 56 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program, _)) in
        REGISTER_PC_INDEXED_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}reg1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3"
            ),
            priority: 60 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (field, programs, priority_base) in [
        ("W", REGISTER_ABSOLUTE_WORD_PROGRAMS, 64_u16),
        ("L", REGISTER_ABSOLUTE_LONG_PROGRAMS, 72_u16),
    ] {
        for (priority, (mnemonic, program, base)) in programs.iter().copied().enumerate() {
            let value_source = if field == "W" {
                MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX
            } else {
                MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX
            };
            let diagnostic = if field == "W" {
                format!("{MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR}{DIAG_ABSOLUTE_WORD_RANGE}")
            } else {
                String::new()
            };
            let operand_plan = if field == "L" {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L,reg1.class1;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,reg1.class1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                )
            } else {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{value_source}scalar.absolute-{field}:member0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field},reg1.class1{diagnostic}"
                )
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan,
                priority: priority_base + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (mnemonic, _, base)) in
        REGISTER_ABSOLUTE_LONG_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,reg1.class1;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,reg1.class1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0"
            ),
            priority: 76 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0,reg1.class1;encode:{PARAM_FIELDS_0_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}1,reg1.class1;encode:{PARAM_SCALAR_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0"
            ),
            priority: 80 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program)) in [
        ("PEA", ENCODING_PEA_INDIRECT),
        ("JMP", ENCODING_JMP_INDIRECT),
        ("JSR", ENCODING_JSR_INDIRECT),
    ]
    .into_iter()
    .enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class1"
            ),
            priority: priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program, _)) in
        CONTROL_DISPLACEMENT_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{VALUE_NORMALIZED_INPUT}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2"
            ),
            priority: (priority + 32) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program, _)) in CONTROL_INDEXED_PROGRAMS.iter().copied().enumerate() {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3"
            ),
            priority: (priority + 48) as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, _program, base)) in
        CONTROL_PC_DISPLACEMENT_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{PARAM_FIXED_EXTENSION_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}0,{MODE_SELECTOR_PLAN_INDIRECT_REGISTER_PREFIX}0.class8"
            ),
            priority: 55 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value2;encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base},{MODE_SELECTOR_PLAN_LITERAL_PREFIX}2;fixup:{FIXUP_PC_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0"
            ),
            priority: 56 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (priority, (mnemonic, program, _)) in
        CONTROL_PC_INDEXED_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_QUALIFIED_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}2{MODE_SELECTOR_PLAN_REGISTER_QUALIFIER_SEPARATOR}W.class0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}0{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class8,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}0.value3"
            ),
            priority: 60 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    for (field, programs, priority_base) in [
        ("W", CONTROL_ABSOLUTE_WORD_PROGRAMS, 64_u16),
        ("L", CONTROL_ABSOLUTE_LONG_PROGRAMS, 72_u16),
    ] {
        for (priority, (mnemonic, program, base)) in programs.iter().copied().enumerate() {
            let operand_plan = if field == "L" {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base};fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L"
                )
            } else {
                format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_INPUTS_PREFIX}{program}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}scalar.absolute-{field}:member0{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field}"
                )
            };
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "direct".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan,
                priority: priority_base + priority as u16,
                unstable_widen: false,
                width_rank: 0,
            });
        }
    }
    for (priority, (mnemonic, _, base)) in
        CONTROL_ABSOLUTE_LONG_PROGRAMS.iter().copied().enumerate()
    {
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{base};fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0"
            ),
            priority: 76 + priority as u16,
            unstable_widen: false,
            width_rank: 0,
        });
    }
    selectors
}

pub const VALUE_NORMALIZED_INPUT: &str = "scalar.normalized-input";
pub const VALUE_SIGNED_BYTE: &str = "scalar.signed-byte";
pub const VALUE_UNSIGNED_BYTE: &str = "scalar.unsigned-byte";
pub const VALUE_SIGNED_WORD: &str = "scalar.signed-word";
pub const VALUE_IMMEDIATE_BYTE: &str = "scalar.immediate-byte";
pub const VALUE_IMMEDIATE_WORD: &str = "scalar.immediate-word";
pub const VALUE_IMMEDIATE_LONG: &str = "scalar.immediate-long";
pub const VALUE_ABSOLUTE_WORD: &str = "scalar.absolute-W";
pub const VALUE_ABSOLUTE_LONG: &str = "scalar.absolute-L";
pub const VALUE_LITERAL_ZERO: &str = "scalar.literal-zero";
pub const VALUE_BIT_FIELD_OFFSET: &str = "scalar.bit-field-offset";
pub const VALUE_BIT_FIELD_WIDTH: &str = "scalar.bit-field-width";
pub const VALUE_PACKED_THREE_BIT_COUNT: &str = "scalar.packed-three-bit-count";
pub const SIGNED_BYTE_RANGE: (i64, i64) = (-128, 127);
pub const UNSIGNED_BYTE_RANGE: (i64, i64) = (0, 255);
pub const SIGNED_WORD_RANGE: (i64, i64) = (-32_768, 32_767);
pub const IMMEDIATE_BYTE_RANGE: (i64, i64) = (-128, 255);
pub const IMMEDIATE_WORD_RANGE: (i64, i64) = (-32_768, 65_535);
pub const IMMEDIATE_LONG_RANGE: (i64, i64) = (-2_147_483_648, 4_294_967_295);
pub const RECORD_DATA_REGISTER: &str = "operand.data-register";
pub const RECORD_ADDRESS_REGISTER: &str = "operand.address-register";
pub const RECORD_ADDRESS_INDIRECT: &str = "operand.address-indirect";
pub const RECORD_ADDRESS_POSTINCREMENT: &str = "operand.address-postincrement";
pub const RECORD_ADDRESS_PREDECREMENT: &str = "operand.address-predecrement";
pub const RECORD_ADDRESS_DISPLACEMENT: &str = "operand.address-displacement";
pub const RECORD_ADDRESS_INDEXED_WORD: &str = "operand.address-indexed-word";
pub const RECORD_ADDRESS_INDEXED_LONG: &str = "operand.address-indexed-long";
pub const RECORD_PC_DISPLACEMENT: &str = "operand.pc-displacement";
pub const RECORD_PC_INDEXED_WORD: &str = "operand.pc-indexed-word";
pub const RECORD_PC_INDEXED_LONG: &str = "operand.pc-indexed-long";
pub const RECORD_ABSOLUTE_WORD: &str = "operand.absolute-word";
pub const RECORD_ABSOLUTE_LONG: &str = "operand.absolute-long";
pub const RECORD_IMMEDIATE: &str = "operand.immediate";
pub const RECORD_FULL_ADDRESS_PREINDEXED: &str = "operand.full-address-preindexed";
pub const RECORD_FULL_PC_POSTINDEXED: &str = "operand.full-pc-postindexed";
pub const RECORD_FULL_SUPPRESSED_INDEX: &str = "operand.full-suppressed-index";
pub const RECORD_FULL_ADDRESS_BASE_ONLY: &str = "operand.full-address-base-only";
pub const RECORD_REGISTER_PAIR: &str = "operand.register-pair";
pub const RECORD_REGISTER_GROUP: &str = "operand.register-group";
pub const RECORD_INDIRECT_REGISTER_PAIR: &str = "operand.indirect-register-pair";
pub const RECORD_REGISTER_LIST: &str = "operand.register-list";
pub const RECORD_BIT_FIELD_REGISTER_OFFSET: &str = "operand.bit-field-register-offset";
pub const RECORD_BIT_FIELD_IMMEDIATE: &str = "operand.bit-field-immediate";
pub const RECORD_BIT_FIELD_VALUE_REGISTER: &str = "operand.bit-field-value-register";
pub const RECORD_BIT_FIELD_REGISTERS: &str = "operand.bit-field-registers";
pub const RECORD_FPU_DATA_REGISTER: &str = "operand.fpu-data-register";
pub const RECORD_FPU_CONTROL_REGISTER: &str = "operand.fpu-control-register";
pub const RECORD_FPU_REGISTER_LIST: &str = "operand.fpu-register-list";
pub const RECORD_FPU_FORMAT_BYTE: &str = "operand.fpu-format-byte";
pub const RECORD_FPU_FORMAT_WORD: &str = "operand.fpu-format-word";
pub const RECORD_FPU_FORMAT_LONG: &str = "operand.fpu-format-long";
pub const RECORD_FPU_FORMAT_SINGLE: &str = "operand.fpu-format-single";
pub const RECORD_FPU_FORMAT_DOUBLE: &str = "operand.fpu-format-double";
pub const RECORD_FPU_FORMAT_EXTENDED: &str = "operand.fpu-format-extended";
pub const RECORD_FPU_FORMAT_PACKED: &str = "operand.fpu-format-packed";
pub const FPU_FORMAT_BYTE: u16 = 0;
pub const FPU_FORMAT_WORD: u16 = 1;
pub const FPU_FORMAT_LONG: u16 = 2;
pub const FPU_FORMAT_SINGLE: u16 = 3;
pub const FPU_FORMAT_DOUBLE: u16 = 4;
pub const FPU_FORMAT_EXTENDED: u16 = 5;
pub const FPU_FORMAT_PACKED: u16 = 6;
pub const DIAG_SELECTOR_UNSUPPORTED_QUALIFIER: &str = "selector.q";
pub const DIAG_TRAP_VECTOR_RANGE: &str = "encoding.trap.range";
pub const DIAG_MOVEQ_IMMEDIATE_RANGE: &str = "encoding.moveq.immediate.range";
pub const DIAG_MOVE_UNSUPPORTED_SIZE: &str = "encoding.move.unsupported-size";
pub const DIAG_COUNT_RANGE: &str = "encoding.count.range";
pub const DIAG_INVALID_DESTINATION: &str = "encoding.invalid-destination";
pub const DIAG_SWAP_REGISTER: &str = "encoding.swap.register";
pub const DIAG_EXG_PAIR: &str = "encoding.exg.pair";
pub const DIAG_DBCC_COUNTER: &str = "encoding.dbcc.counter";
pub const DIAG_STOP_OPERAND: &str = "encoding.stop.operand";
pub const DIAG_UNSUPPORTED_BYTE_SIZE: &str = "encoding.unsupported-byte-size";
pub const DIAG_MOVE_FROM_CCR: &str = "encoding.move.from-ccr";
pub const DIAG_MOVE_USP_SOURCE: &str = "encoding.move.usp-source";
pub const DIAG_ANDI_WORD_CCR: &str = "encoding.andi.word-ccr";
pub const DIAG_MOVE_TO_SR_SOURCE: &str = "encoding.move.to-sr-source";
pub const DIAG_BIT_NUMBER: &str = "encoding.bit-number";
pub const DIAG_UNSUPPORTED_LONG_SIZE: &str = "encoding.unsupported-long-size";
pub const DIAG_INVALID_SOURCE: &str = "encoding.invalid-source";
pub const DIAG_DESTINATION_DATA_REGISTER: &str = "encoding.destination-data-register";
pub const DIAG_EXTEND_SHAPE: &str = "encoding.extend-shape";
pub const DIAG_NO_SIZE_SUFFIX: &str = "encoding.no-size-suffix";
pub const DIAG_CMPM_SHAPE: &str = "encoding.cmpm-shape";
pub const DIAG_MEMORY_LONG_SIZE: &str = "encoding.memory-long-size";
pub const DIAG_DUPLICATE_REGISTER: &str = "encoding.duplicate-register";
pub const DIAG_MOVEP_ADDRESSING: &str = "encoding.movep-addressing";
pub const DIAG_MOVEC_CONTROL_M68010: &str = "encoding.movec-control-m68010";
pub const DIAG_MOVEC_CONTROL_M68040: &str = "encoding.movec-control-m68040";
pub const DIAG_MOVEC_CAAR_M68040: &str = "encoding.movec-caar-m68040";
pub const DIAG_MOVEC_BASELINE: &str = "encoding.movec-baseline";
pub const DIAG_M68020_REQUIRED: &str = "encoding.m68020-required";
pub const DIAG_DIVS_LONG_BASELINE: &str = "encoding.divs-long.baseline";
pub const DIAG_DIVS_LONG_M68010: &str = "encoding.divs-long.m68010";
pub const DIAG_M68080_REGISTER_M68040: &str = "encoding.m68080-register.m68040";
pub const DIAG_M68080_ONLY: &str = "encoding.m68080-only";
pub const DIAG_FPU_DATA_REGISTER_EA: &str = "encoding.fpu-data-register-ea";
pub const DIAG_INVALID_DESTINATION_FORM: &str = "encoding.invalid-destination-form";
pub const DIAG_CAS2_BYTE_SIZE: &str = "encoding.cas2.byte-size";
pub const DIAG_CAS2_MEMORY_PAIR: &str = "encoding.cas2.memory-pair";
pub const DIAG_DIV_PAIR_DISTINCT: &str = "encoding.div.pair-distinct";
pub const DIAG_DIVSL_PAIR_REQUIRED: &str = "encoding.divsl.pair-required";
pub const DIAG_CMP2_BOUNDS: &str = "encoding.cmp2.bounds";
pub const DIAG_TRAPCC_UNSIZED_OPERAND: &str = "encoding.trapcc.unsized-operand";
pub const DIAG_CALLM_COUNT: &str = "encoding.callm.count";
pub const DIAG_CALLM_M68040: &str = "encoding.callm.m68040";
pub const DIAG_RTM_OPERAND: &str = "encoding.rtm.operand";
pub const DIAG_RTM_M68030: &str = "encoding.rtm.m68030";
pub const DIAG_RTM_M68040: &str = "encoding.rtm.m68040";
pub const DIAG_PFLUSH_BASELINE: &str = "encoding.pflush.baseline";
pub const DIAG_PFLUSH_M68020: &str = "encoding.pflush.m68020";
pub const DIAG_PFLUSH_M68040_ARITY: &str = "encoding.pflush.m68040-arity";
pub const DIAG_BITFIELD_EA: &str = "encoding.bitfield.ea";
pub const DIAG_FULL_EXTENSION_DISPLACEMENT: &str = "encoding.full-extension-displacement";
pub const DIAG_FULL_EXTENSION_UNSUPPORTED: &str = "encoding.full-extension-unsupported";
pub const DIAG_INVALID_DISPLACEMENT_BASE: &str = "encoding.invalid-displacement-base";
pub const DIAG_STOP_IMMEDIATE_RANGE: &str = "encoding.stop.immediate.range";
pub const DIAG_LINK_DISPLACEMENT_RANGE: &str = "encoding.link.displacement.range";
pub const DIAG_BKPT_VECTOR_RANGE: &str = "encoding.bkpt.vector.range";
pub const DIAG_RTD_DISPLACEMENT_RANGE: &str = "encoding.rtd.displacement.range";
pub const DIAG_LINK_LONG_DISPLACEMENT_RANGE: &str = "encoding.link-long.displacement.range";
pub const DIAG_STATUS_BYTE_IMMEDIATE_RANGE: &str = "encoding.status-byte.immediate.range";
pub const DIAG_STATUS_WORD_IMMEDIATE_RANGE: &str = "encoding.status-word.immediate.range";
pub const DIAG_ABSOLUTE_WORD_RANGE: &str = "encoding.absolute-word.range";
pub const STATE_RUNTIME: &str = "runtime";
pub const ENCODING_TRAP_VECTOR: &str = "enc.trap";
pub const ENCODING_MOVEQ: &str = "enc.moveq";
pub const ENCODING_STOP: &str = "enc.stop";
pub const ENCODING_LINK_WORD: &str = "enc.link.word";
pub const ENCODING_REGISTER_MASK: &str = "enc.mask";
pub const ENCODING_REVERSED_REGISTER_MASK: &str = "enc.mask-rev";
pub const ENCODING_FPU_REGISTER_MASK: &str = "enc.fpu-mask";
pub const ENCODING_REGISTER_PAIR: &str = "enc.pair";
pub const ENCODING_BIT_FIELD: &str = "enc.bit-field";
pub const FIXUP_PC_BYTE: &str = "fix.pc8";
pub const FIXUP_PC_WORD: &str = "fix.pc16";
pub const FIXUP_PC_WORD_BASE4: &str = "fix.pc16.base4";
pub const FIXUP_ABSOLUTE_LONG: &str = "fix.abs32";
pub const BRANCH_SIZED: &str = "branch.sized";
pub const BRANCH_CANDIDATE_BYTE: u8 = 0;
pub const BRANCH_CANDIDATE_WORD: u8 = 1;
pub const BRANCH_CANDIDATE_LONG: u8 = 2;

fn branch_candidate(
    id: u8,
    automatic_classes: u8,
    suffix: &[u8],
    displacement_width: u8,
    unresolved_placeholder: i32,
    reserved_values: &[i32],
) -> BranchCandidateSpec {
    BranchCandidateSpec {
        id,
        automatic_classes,
        suffix: suffix.to_vec(),
        displacement_width,
        endian: EncodingEndian::Big,
        position_adjustment: 2,
        unresolved_placeholder,
        reserved_values: reserved_values.to_vec(),
    }
}

fn branch_program(owner: &ScopedOwner) -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
    let candidates = vec![
        branch_candidate(BRANCH_CANDIDATE_BYTE, 0, &[], 1, 1, &[0]),
        branch_candidate(BRANCH_CANDIDATE_WORD, 0b11, &[0], 2, 0, &[]),
        branch_candidate(BRANCH_CANDIDATE_LONG, 0b10, &[0xff], 4, 0, &[]),
    ];
    Ok(SemanticProgramDescriptor {
        owner: owner.clone(),
        id: BRANCH_SIZED.to_string(),
        opcode_version: SEMANTIC_VM_OPCODE_VERSION_V5,
        program: compile_branch_program(&BranchProgramSpec {
            opcode_input: 0,
            target_input: 0,
            unresolved_candidate: BRANCH_CANDIDATE_WORD,
            candidates,
        })?,
    })
}

/// Compile one directly resolvable instruction form into the neutral fixed-field VM.
pub fn semantic_programs() -> Result<Vec<SemanticProgramDescriptor>, OpcpuCodecError> {
    let owner = ScopedOwner::Family("motorola68000".to_string());
    let structured = |id: &str,
                      step: StructuredEncodingStep|
     -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
        Ok(SemanticProgramDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V3,
            program: compile_structured_encoding_program(&[step])?,
        })
    };
    let register_field =
        |id: &str, base: u32| -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
            Ok(SemanticProgramDescriptor {
                owner: owner.clone(),
                id: id.to_string(),
                opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
                program: compile_encoding_program(&[EncodingStep::Fields {
                    base,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 0,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                }])?,
            })
        };
    let two_register_fields = |id: &str,
                               base: u32,
                               first_shift: u8,
                               second_shift: u8|
     -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
        Ok(SemanticProgramDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[EncodingStep::Fields {
                base,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: first_shift,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: second_shift,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                ],
            }])?,
        })
    };
    let two_register_displacement = |id: &str,
                                     base: u32,
                                     first_shift: u8,
                                     second_shift: u8|
     -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
        Ok(SemanticProgramDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Fields {
                    base,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 0,
                            shift: first_shift,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 1,
                            shift: second_shift,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                    ],
                },
                EncodingStep::Scalar {
                    input: 2,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 32_767,
                },
            ])?,
        })
    };
    let one_register_displacement =
        |id: &str, base: u32| -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
            Ok(SemanticProgramDescriptor {
                owner: owner.clone(),
                id: id.to_string(),
                opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
                program: compile_encoding_program(&[
                    EncodingStep::Fields {
                        base,
                        width: 2,
                        endian: EncodingEndian::Big,
                        fields: vec![EncodingFieldSpec {
                            input: 0,
                            shift: 0,
                            bits: 3,
                            min: 0,
                            max: 7,
                        }],
                    },
                    EncodingStep::Scalar {
                        input: 1,
                        width: 2,
                        endian: EncodingEndian::Big,
                        min: -32_768,
                        max: 32_767,
                    },
                ])?,
            })
        };
    let destination_register_displacement =
        |id: &str, base: u32| -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
            Ok(SemanticProgramDescriptor {
                owner: owner.clone(),
                id: id.to_string(),
                opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
                program: compile_encoding_program(&[
                    EncodingStep::Fields {
                        base,
                        width: 2,
                        endian: EncodingEndian::Big,
                        fields: vec![EncodingFieldSpec {
                            input: 0,
                            shift: 9,
                            bits: 3,
                            min: 0,
                            max: 7,
                        }],
                    },
                    EncodingStep::Scalar {
                        input: 1,
                        width: 2,
                        endian: EncodingEndian::Big,
                        min: -32_768,
                        max: 32_767,
                    },
                ])?,
            })
        };
    let two_register_indexed = |id: &str,
                                base: u32,
                                first_shift: u8,
                                second_shift: u8|
     -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
        Ok(SemanticProgramDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Fields {
                    base,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 0,
                            shift: first_shift,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 1,
                            shift: second_shift,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                    ],
                },
                EncodingStep::Fields {
                    base: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 2,
                            shift: 12,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 3,
                            shift: 0,
                            bits: 8,
                            min: -128,
                            max: 127,
                        },
                    ],
                },
            ])?,
        })
    };
    let one_register_indexed =
        |id: &str, base: u32| -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
            Ok(SemanticProgramDescriptor {
                owner: owner.clone(),
                id: id.to_string(),
                opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
                program: compile_encoding_program(&[
                    EncodingStep::Fields {
                        base,
                        width: 2,
                        endian: EncodingEndian::Big,
                        fields: vec![EncodingFieldSpec {
                            input: 0,
                            shift: 0,
                            bits: 3,
                            min: 0,
                            max: 7,
                        }],
                    },
                    EncodingStep::Fields {
                        base: 0,
                        width: 2,
                        endian: EncodingEndian::Big,
                        fields: vec![
                            EncodingFieldSpec {
                                input: 1,
                                shift: 12,
                                bits: 3,
                                min: 0,
                                max: 7,
                            },
                            EncodingFieldSpec {
                                input: 2,
                                shift: 0,
                                bits: 8,
                                min: -128,
                                max: 127,
                            },
                        ],
                    },
                ])?,
            })
        };
    let destination_register_pc_indexed =
        |id: &str, base: u32| -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
            Ok(SemanticProgramDescriptor {
                owner: owner.clone(),
                id: id.to_string(),
                opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
                program: compile_encoding_program(&[
                    EncodingStep::Fields {
                        base,
                        width: 2,
                        endian: EncodingEndian::Big,
                        fields: vec![EncodingFieldSpec {
                            input: 0,
                            shift: 9,
                            bits: 3,
                            min: 0,
                            max: 7,
                        }],
                    },
                    EncodingStep::Fields {
                        base: 0,
                        width: 2,
                        endian: EncodingEndian::Big,
                        fields: vec![
                            EncodingFieldSpec {
                                input: 1,
                                shift: 12,
                                bits: 3,
                                min: 0,
                                max: 7,
                            },
                            EncodingFieldSpec {
                                input: 2,
                                shift: 0,
                                bits: 8,
                                min: -128,
                                max: 127,
                            },
                        ],
                    },
                ])?,
            })
        };
    let fixed_pc_indexed =
        |id: &str, base: u32| -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
            Ok(SemanticProgramDescriptor {
                owner: owner.clone(),
                id: id.to_string(),
                opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
                program: compile_encoding_program(&[
                    EncodingStep::Literal {
                        value: base,
                        width: 2,
                        endian: EncodingEndian::Big,
                    },
                    EncodingStep::Fields {
                        base: 0,
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
                                shift: 0,
                                bits: 8,
                                min: -128,
                                max: 127,
                            },
                        ],
                    },
                ])?,
            })
        };
    let immediate_word = |id: &str,
                          opcode: u32,
                          min: i64,
                          max: i64|
     -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
        Ok(SemanticProgramDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Literal {
                    value: opcode,
                    width: 2,
                    endian: EncodingEndian::Big,
                },
                EncodingStep::Scalar {
                    input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min,
                    max,
                },
            ])?,
        })
    };
    let register_extension = |id: &str,
                              opcode: u32,
                              register_shift: u8,
                              extension_width: u8,
                              min: i64,
                              max: i64|
     -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
        Ok(SemanticProgramDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Fields {
                    base: opcode,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 1,
                        shift: register_shift,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 0,
                    width: extension_width,
                    endian: EncodingEndian::Big,
                    min,
                    max,
                },
            ])?,
        })
    };
    let fixed_extension = |id: &str,
                           opcode: u32,
                           extension_width: u8,
                           min: i64,
                           max: i64|
     -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
        Ok(SemanticProgramDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Literal {
                    value: opcode,
                    width: 2,
                    endian: EncodingEndian::Big,
                },
                EncodingStep::Scalar {
                    input: 0,
                    width: extension_width,
                    endian: EncodingEndian::Big,
                    min,
                    max,
                },
            ])?,
        })
    };
    let movec = |id: &str,
                 opcode: u32,
                 address_bit: u32|
     -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
        Ok(SemanticProgramDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Literal {
                    value: opcode,
                    width: 2,
                    endian: EncodingEndian::Big,
                },
                EncodingStep::Fields {
                    base: address_bit,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 0,
                            shift: 0,
                            bits: 12,
                            min: 0,
                            max: 0x0fff,
                        },
                        EncodingFieldSpec {
                            input: 1,
                            shift: 12,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                    ],
                },
            ])?,
        })
    };
    let mut programs = vec![
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_TRAP_VECTOR.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[EncodingStep::Fields {
                base: 0x4e40,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![EncodingFieldSpec {
                    input: 0,
                    shift: 0,
                    bits: 4,
                    min: 0,
                    max: 15,
                }],
            }])?,
        },
        register_field(ENCODING_MOVE_USP_TO_ADDRESS, 0x4e68)?,
        register_field(ENCODING_MOVE_ADDRESS_TO_USP, 0x4e60)?,
        register_field(ENCODING_MOVE_SR_TO_DATA, 0x40c0)?,
        register_field(ENCODING_MOVE_CCR_TO_DATA, 0x42c0)?,
        register_field(ENCODING_MOVE_DATA_TO_CCR, 0x44c0)?,
        register_field(ENCODING_MOVE_DATA_TO_SR, 0x46c0)?,
        two_register_fields(ENCODING_MOVE_BYTE_INDIRECT_TO_DATA, 0x1010, 0, 9)?,
        two_register_fields(ENCODING_MOVE_WORD_INDIRECT_TO_DATA, 0x3010, 0, 9)?,
        two_register_fields(ENCODING_MOVE_LONG_INDIRECT_TO_DATA, 0x2010, 0, 9)?,
        two_register_fields(ENCODING_MOVE_BYTE_DATA_TO_INDIRECT, 0x1080, 9, 0)?,
        two_register_fields(ENCODING_MOVE_WORD_DATA_TO_INDIRECT, 0x3080, 9, 0)?,
        two_register_fields(ENCODING_MOVE_LONG_DATA_TO_INDIRECT, 0x2080, 9, 0)?,
        two_register_fields(ENCODING_MOVEA_WORD_INDIRECT, 0x3050, 0, 9)?,
        two_register_fields(ENCODING_MOVEA_LONG_INDIRECT, 0x2050, 0, 9)?,
        two_register_fields(ENCODING_LEA_INDIRECT, 0x41d0, 0, 9)?,
        register_field(ENCODING_PEA_INDIRECT, 0x4850)?,
        register_field(ENCODING_JMP_INDIRECT, 0x4ed0)?,
        register_field(ENCODING_JSR_INDIRECT, 0x4e90)?,
        immediate_word(ENCODING_MOVE_IMMEDIATE_TO_CCR, 0x44fc, -128, 255)?,
        immediate_word(ENCODING_MOVE_IMMEDIATE_TO_SR, 0x46fc, -32_768, 65_535)?,
        immediate_word(ENCODING_MOVE_ABSOLUTE_WORD_TO_CCR, 0x44f8, -32_768, 65_535)?,
        immediate_word(ENCODING_MOVE_CCR_TO_ABSOLUTE_WORD, 0x42f8, -32_768, 65_535)?,
        immediate_word(ENCODING_ANDI_TO_CCR, 0x023c, -128, 255)?,
        immediate_word(ENCODING_ANDI_TO_SR, 0x027c, -32_768, 65_535)?,
        immediate_word(ENCODING_ORI_TO_CCR, 0x003c, -128, 255)?,
        immediate_word(ENCODING_ORI_TO_SR, 0x007c, -32_768, 65_535)?,
        immediate_word(ENCODING_EORI_TO_CCR, 0x0a3c, -128, 255)?,
        immediate_word(ENCODING_EORI_TO_SR, 0x0a7c, -32_768, 65_535)?,
        movec(ENCODING_MOVEC_CONTROL_TO_DATA, 0x4e7a, 0)?,
        movec(ENCODING_MOVEC_CONTROL_TO_ADDRESS, 0x4e7a, 0x8000)?,
        movec(ENCODING_MOVEC_DATA_TO_CONTROL, 0x4e7b, 0)?,
        movec(ENCODING_MOVEC_ADDRESS_TO_CONTROL, 0x4e7b, 0x8000)?,
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_MOVES_INDIRECT.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Fields {
                    base: 0x0e10,
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
                            input: 2,
                            shift: 6,
                            bits: 2,
                            min: 0,
                            max: 3,
                        },
                    ],
                },
                EncodingStep::Fields {
                    base: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 1,
                            shift: 12,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 3,
                            shift: 15,
                            bits: 1,
                            min: 0,
                            max: 1,
                        },
                        EncodingFieldSpec {
                            input: 4,
                            shift: 11,
                            bits: 1,
                            min: 0,
                            max: 1,
                        },
                    ],
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_STOP.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Literal {
                    value: 0x4e72,
                    width: 2,
                    endian: EncodingEndian::Big,
                },
                EncodingStep::Scalar {
                    input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 65_535,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_LINK_WORD.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Fields {
                    base: 0x4e50,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 1,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 32_767,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_MOVEQ.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[EncodingStep::Fields {
                base: 0x7000,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: 0,
                        bits: 8,
                        min: -128,
                        max: 127,
                    },
                    EncodingFieldSpec {
                        input: 1,
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
            id: ENCODING_BKPT.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[EncodingStep::Fields {
                base: 0x4848,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![EncodingFieldSpec {
                    input: 0,
                    shift: 0,
                    bits: 3,
                    min: 0,
                    max: 7,
                }],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_RTD.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Literal {
                    value: 0x4e74,
                    width: 2,
                    endian: EncodingEndian::Big,
                },
                EncodingStep::Scalar {
                    input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 32_767,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_LINK_LONG.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Fields {
                    base: 0x4808,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 1,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 0,
                    width: 4,
                    endian: EncodingEndian::Big,
                    min: i32::MIN as i64,
                    max: u32::MAX as i64,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_EXTB_LONG.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[EncodingStep::Fields {
                base: 0x49c0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![EncodingFieldSpec {
                    input: 0,
                    shift: 0,
                    bits: 3,
                    min: 0,
                    max: 7,
                }],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_RTM_DATA.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[EncodingStep::Fields {
                base: 0x06c0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![EncodingFieldSpec {
                    input: 0,
                    shift: 0,
                    bits: 3,
                    min: 0,
                    max: 7,
                }],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_RTM_ADDRESS.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[EncodingStep::Fields {
                base: 0x06c8,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![EncodingFieldSpec {
                    input: 0,
                    shift: 0,
                    bits: 3,
                    min: 0,
                    max: 7,
                }],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_PFLUSH_68030.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Literal {
                    value: 0xf000,
                    width: 2,
                    endian: EncodingEndian::Big,
                },
                EncodingStep::Fields {
                    base: 0x3010,
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
                            shift: 5,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                    ],
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: ENCODING_PFLUSH_68040.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[EncodingStep::Fields {
                base: 0xf508,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![EncodingFieldSpec {
                    input: 0,
                    shift: 0,
                    bits: 3,
                    min: 0,
                    max: 7,
                }],
            }])?,
        },
        structured(
            ENCODING_REGISTER_MASK,
            StructuredEncodingStep::RegisterMask {
                record: 0,
                width: 2,
                endian: EncodingEndian::Big,
                reverse_bits: false,
                classes: vec![
                    RegisterClassProjection {
                        class: 0,
                        offset: 0,
                    },
                    RegisterClassProjection {
                        class: 1,
                        offset: 8,
                    },
                ],
            },
        )?,
        structured(
            ENCODING_REVERSED_REGISTER_MASK,
            StructuredEncodingStep::RegisterMask {
                record: 0,
                width: 2,
                endian: EncodingEndian::Big,
                reverse_bits: true,
                classes: vec![
                    RegisterClassProjection {
                        class: 0,
                        offset: 0,
                    },
                    RegisterClassProjection {
                        class: 1,
                        offset: 8,
                    },
                ],
            },
        )?,
        structured(
            ENCODING_FPU_REGISTER_MASK,
            StructuredEncodingStep::RegisterMask {
                record: 0,
                width: 1,
                endian: EncodingEndian::Big,
                reverse_bits: true,
                classes: vec![RegisterClassProjection {
                    class: 2,
                    offset: 0,
                }],
            },
        )?,
        structured(
            ENCODING_REGISTER_PAIR,
            StructuredEncodingStep::RegisterPair {
                record: 0,
                base: 0,
                width: 2,
                endian: EncodingEndian::Big,
                left_shift: 6,
                right_shift: 0,
                bits: 3,
                indirect: None,
            },
        )?,
        structured(
            ENCODING_BIT_FIELD,
            StructuredEncodingStep::FieldSelectors {
                record: 0,
                base: 0,
                width: 2,
                endian: EncodingEndian::Big,
                offset_shift: 6,
                width_shift: 0,
                bits: 5,
                offset_full_width_zero: false,
                width_full_width_zero: true,
            },
        )?,
    ];
    programs.extend(
        MOVE_UPDATE_SOURCE_PROGRAMS
            .iter()
            .map(|(_, id, base, _)| two_register_fields(id, *base, 0, 9))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        MOVE_UPDATE_DESTINATION_PROGRAMS
            .iter()
            .map(|(_, id, base, _)| two_register_fields(id, *base, 9, 0))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        MOVE_DISPLACEMENT_SOURCE_PROGRAMS
            .iter()
            .map(|(_, id, base)| two_register_displacement(id, *base, 0, 9))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        MOVE_DISPLACEMENT_DESTINATION_PROGRAMS
            .iter()
            .map(|(_, id, base)| two_register_displacement(id, *base, 9, 0))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        REGISTER_DISPLACEMENT_PROGRAMS
            .iter()
            .map(|(_, id, base)| two_register_displacement(id, *base, 0, 9))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        CONTROL_DISPLACEMENT_PROGRAMS
            .iter()
            .map(|(_, id, base)| one_register_displacement(id, *base))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        MOVE_PC_DISPLACEMENT_SOURCE_PROGRAMS
            .iter()
            .map(|(_, id, base)| destination_register_displacement(id, *base))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        REGISTER_PC_DISPLACEMENT_PROGRAMS
            .iter()
            .map(|(_, id, base)| destination_register_displacement(id, *base))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        CONTROL_PC_DISPLACEMENT_PROGRAMS
            .iter()
            .map(|(_, id, base)| fixed_extension(id, *base, 2, -32_768, 32_767))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        MOVE_PC_INDEXED_SOURCE_PROGRAMS
            .iter()
            .map(|(_, id, base)| destination_register_pc_indexed(id, *base))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        REGISTER_PC_INDEXED_PROGRAMS
            .iter()
            .map(|(_, id, base)| destination_register_pc_indexed(id, *base))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        CONTROL_PC_INDEXED_PROGRAMS
            .iter()
            .map(|(_, id, base)| fixed_pc_indexed(id, *base))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        MOVE_INDEXED_SOURCE_PROGRAMS
            .iter()
            .map(|(_, id, base)| two_register_indexed(id, *base, 0, 9))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        MOVE_INDEXED_DESTINATION_PROGRAMS
            .iter()
            .map(|(_, id, base)| two_register_indexed(id, *base, 9, 0))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        REGISTER_INDEXED_PROGRAMS
            .iter()
            .map(|(_, id, base)| two_register_indexed(id, *base, 0, 9))
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend(
        CONTROL_INDEXED_PROGRAMS
            .iter()
            .map(|(_, id, base)| one_register_indexed(id, *base))
            .collect::<Result<Vec<_>, _>>()?,
    );
    for (specs, register_shift, width, min, max) in [
        (
            MOVE_ABSOLUTE_WORD_SOURCE_PROGRAMS,
            9_u8,
            2_u8,
            -32_768_i64,
            65_535_i64,
        ),
        (
            MOVE_ABSOLUTE_LONG_SOURCE_PROGRAMS,
            9,
            4,
            i32::MIN as i64,
            u32::MAX as i64,
        ),
        (
            MOVE_ABSOLUTE_WORD_DESTINATION_PROGRAMS,
            0,
            2,
            -32_768,
            65_535,
        ),
        (
            MOVE_ABSOLUTE_LONG_DESTINATION_PROGRAMS,
            0,
            4,
            i32::MIN as i64,
            u32::MAX as i64,
        ),
        (REGISTER_ABSOLUTE_WORD_PROGRAMS, 9, 2, -32_768, 65_535),
        (
            REGISTER_ABSOLUTE_LONG_PROGRAMS,
            9,
            4,
            i32::MIN as i64,
            u32::MAX as i64,
        ),
    ] {
        programs.extend(
            specs
                .iter()
                .map(|(_, id, opcode)| {
                    register_extension(id, *opcode, register_shift, width, min, max)
                })
                .collect::<Result<Vec<_>, _>>()?,
        );
    }
    for (specs, width, min, max) in [
        (
            CONTROL_ABSOLUTE_WORD_PROGRAMS,
            2_u8,
            -32_768_i64,
            65_535_i64,
        ),
        (
            CONTROL_ABSOLUTE_LONG_PROGRAMS,
            4,
            i32::MIN as i64,
            u32::MAX as i64,
        ),
    ] {
        programs.extend(
            specs
                .iter()
                .map(|(_, id, opcode)| fixed_extension(id, *opcode, width, min, max))
                .collect::<Result<Vec<_>, _>>()?,
        );
    }
    programs.extend(
        MOVE_IMMEDIATE_PROGRAMS
            .iter()
            .map(|(_, id, opcode, width, min, max)| {
                register_extension(id, *opcode, 9, *width, *min, *max)
            })
            .collect::<Result<Vec<_>, _>>()?,
    );
    programs.extend([
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_IMMEDIATE_BYTE_FIELD_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 2,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -128,
                    max: 255,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_IMMEDIATE_WORD_FIELD_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 2,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 65_535,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_IMMEDIATE_LONG_FIELD_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 2,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 4,
                    endian: EncodingEndian::Big,
                    min: i32::MIN as i64,
                    max: u32::MAX as i64,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_SCALAR_WORD.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Scalar {
                input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                min: -32_768,
                max: 65_535,
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_SCALAR_BYTE.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Scalar {
                input: 0,
                width: 1,
                endian: EncodingEndian::Big,
                min: -128,
                max: 255,
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_SCALAR_LONG.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Scalar {
                input: 0,
                width: 4,
                endian: EncodingEndian::Big,
                min: i32::MIN as i64,
                max: u32::MAX as i64,
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_IMMEDIATE_BYTE_FIELD_9.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 2,
                        shift: 9,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -128,
                    max: 255,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_INDEX_PREFIX.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 1,
                endian: EncodingEndian::Big,
                fields: vec![EncodingFieldSpec {
                    input: 1,
                    shift: 4,
                    bits: 3,
                    min: 0,
                    max: 7,
                }],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_IMMEDIATE_WORD_FIELD_9.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 2,
                        shift: 9,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 65_535,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_IMMEDIATE_LONG_FIELD_9.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 2,
                        shift: 9,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 4,
                    endian: EncodingEndian::Big,
                    min: i32::MIN as i64,
                    max: u32::MAX as i64,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_STATIC_FIELD_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 2,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: 0,
                    max: 255,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_STATIC_DISPLACEMENT_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 2,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: 0,
                    max: 255,
                },
                EncodingStep::Scalar {
                    input: 3,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 32_767,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_STATIC_INDEXED_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 2,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: 0,
                    max: 255,
                },
                EncodingStep::Fields {
                    base: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 3,
                            shift: 12,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 4,
                            shift: 0,
                            bits: 8,
                            min: -128,
                            max: 127,
                        },
                    ],
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_STATIC_EXTENSION_WORD.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Scalar {
                    input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: 0,
                    max: 65_535,
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: 0,
                    max: 255,
                },
                EncodingStep::Scalar {
                    input: 2,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 65_535,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_STATIC_EXTENSION_LONG.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Scalar {
                    input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: 0,
                    max: 65_535,
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: 0,
                    max: 255,
                },
                EncodingStep::Scalar {
                    input: 2,
                    width: 4,
                    endian: EncodingEndian::Big,
                    min: i32::MIN as i64,
                    max: u32::MAX as i64,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_STATIC_PC_DISPLACEMENT.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Scalar {
                    input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: 0,
                    max: 65_535,
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: 0,
                    max: 255,
                },
                EncodingStep::Scalar {
                    input: 2,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 32_767,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_STATIC_PC_INDEXED.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Scalar {
                    input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: 0,
                    max: 65_535,
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: 0,
                    max: 255,
                },
                EncodingStep::Fields {
                    base: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 2,
                            shift: 12,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 3,
                            shift: 0,
                            bits: 8,
                            min: -128,
                            max: 127,
                        },
                    ],
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_FIELD_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![EncodingFieldSpec {
                    input: 1,
                    shift: 0,
                    bits: 3,
                    min: 0,
                    max: 7,
                }],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_FIELD_9.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![EncodingFieldSpec {
                    input: 1,
                    shift: 9,
                    bits: 3,
                    min: 0,
                    max: 7,
                }],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_DISPLACEMENT_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 1,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 2,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 32_767,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_INDEXED_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 1,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Fields {
                    base: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 2,
                            shift: 12,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 3,
                            shift: 0,
                            bits: 8,
                            min: -128,
                            max: 127,
                        },
                    ],
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_FIXED_EXTENSION_WORD.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Scalar {
                    input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: 0,
                    max: 65_535,
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 65_535,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_FIXED_EXTENSION_LONG.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[
                EncodingStep::Scalar {
                    input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: 0,
                    max: 65_535,
                },
                EncodingStep::Scalar {
                    input: 1,
                    width: 4,
                    endian: EncodingEndian::Big,
                    min: i32::MIN as i64,
                    max: u32::MAX as i64,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_FIELDS_0_9.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 1,
                        shift: 0,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 2,
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
            id: PARAM_FIELDS_12_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 1,
                        shift: 12,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 2,
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
            id: PARAM_FIELDS_6_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 1,
                        shift: 6,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 2,
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
            id: PARAM_FIELDS_12_6_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 1,
                        shift: 12,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 6,
                        bits: 3,
                        min: 0,
                        max: 7,
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
            id: PARAM_FULL_EXTENSION.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 1,
                        shift: 12,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 9,
                        bits: 2,
                        min: 0,
                        max: 3,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_INDEX_EXTENSION.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::Fields {
                base: 0,
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
                        shift: 15,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 11,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                    EncodingFieldSpec {
                        input: 3,
                        shift: 0,
                        bits: 8,
                        min: -128,
                        max: 127,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_MOVES_EXTENSION.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
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
                    EncodingFieldSpec {
                        input: 3,
                        shift: 11,
                        bits: 1,
                        min: 0,
                        max: 1,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_FIELDS_10_7_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 1,
                        shift: 10,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 7,
                        bits: 3,
                        min: 0,
                        max: 7,
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
            id: PARAM_FIELDS_10_7_0_3.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 1,
                        shift: 10,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 7,
                        bits: 3,
                        min: 0,
                        max: 7,
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
            id: PARAM_FIELD_0_8.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![EncodingFieldSpec {
                    input: 1,
                    shift: 0,
                    bits: 8,
                    min: 0,
                    max: 255,
                }],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_FIELDS_7_0_7.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 1,
                        shift: 7,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 0,
                        bits: 7,
                        min: 0,
                        max: 127,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_BITFIELD_EXTENSION.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 1,
                        shift: 12,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 2,
                        shift: 6,
                        bits: 5,
                        min: 0,
                        max: 31,
                    },
                    EncodingFieldSpec {
                        input: 3,
                        shift: 0,
                        bits: 5,
                        min: 0,
                        max: 31,
                    },
                ],
            }])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_DISPLACEMENT_0_9.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 1,
                            shift: 0,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 2,
                            shift: 9,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                    ],
                },
                EncodingStep::Scalar {
                    input: 3,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 32_767,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_INDEXED_0_9.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 1,
                            shift: 0,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 2,
                            shift: 9,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                    ],
                },
                EncodingStep::Fields {
                    base: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 3,
                            shift: 12,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 4,
                            shift: 0,
                            bits: 8,
                            min: -128,
                            max: 127,
                        },
                    ],
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_INDEXED_LONG_0_9.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 1,
                            shift: 0,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 2,
                            shift: 9,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                    ],
                },
                EncodingStep::Fields {
                    base: 0x0800,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 3,
                            shift: 12,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 4,
                            shift: 0,
                            bits: 8,
                            min: -128,
                            max: 127,
                        },
                    ],
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_FIELD_9_DISPLACEMENT.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 1,
                        shift: 9,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 2,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 32_767,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_FIELD_9_PC_INDEXED.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 1,
                        shift: 9,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Fields {
                    base: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 2,
                            shift: 12,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 3,
                            shift: 0,
                            bits: 8,
                            min: -128,
                            max: 127,
                        },
                    ],
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_FIELDS_9_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[EncodingStep::InputFields {
                base_input: 0,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 1,
                        shift: 9,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 2,
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
            id: PARAM_DISPLACEMENT_9_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 1,
                            shift: 9,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 2,
                            shift: 0,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                    ],
                },
                EncodingStep::Scalar {
                    input: 3,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 32_767,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_INDEXED_9_0.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 1,
                            shift: 9,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 2,
                            shift: 0,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                    ],
                },
                EncodingStep::Fields {
                    base: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![
                        EncodingFieldSpec {
                            input: 3,
                            shift: 12,
                            bits: 3,
                            min: 0,
                            max: 7,
                        },
                        EncodingFieldSpec {
                            input: 4,
                            shift: 0,
                            bits: 8,
                            min: -128,
                            max: 127,
                        },
                    ],
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_EXTENSION_WORD_9.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 1,
                        shift: 9,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 2,
                    width: 2,
                    endian: EncodingEndian::Big,
                    min: -32_768,
                    max: 65_535,
                },
            ])?,
        },
        SemanticProgramDescriptor {
            owner: owner.clone(),
            id: PARAM_EXTENSION_LONG_9.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V6,
            program: compile_parameterized_encoding_program(&[
                EncodingStep::InputFields {
                    base_input: 0,
                    width: 2,
                    endian: EncodingEndian::Big,
                    fields: vec![EncodingFieldSpec {
                        input: 1,
                        shift: 9,
                        bits: 3,
                        min: 0,
                        max: 7,
                    }],
                },
                EncodingStep::Scalar {
                    input: 2,
                    width: 4,
                    endian: EncodingEndian::Big,
                    min: i32::MIN as i64,
                    max: u32::MAX as i64,
                },
            ])?,
        },
    ]);
    programs.extend(
        SINGLE_REGISTER_INSTRUCTION_PROGRAMS
            .iter()
            .map(|(_, id, base, _)| {
                Ok(SemanticProgramDescriptor {
                    owner: owner.clone(),
                    id: (*id).to_string(),
                    opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
                    program: compile_encoding_program(&[EncodingStep::Fields {
                        base: *base,
                        width: 2,
                        endian: EncodingEndian::Big,
                        fields: vec![EncodingFieldSpec {
                            input: 0,
                            shift: 0,
                            bits: 3,
                            min: 0,
                            max: 7,
                        }],
                    }])?,
                })
            })
            .collect::<Result<Vec<_>, OpcpuCodecError>>()?,
    );
    for (id, base, left_shift, right_shift) in [
        (EXG_DATA_DATA, 0xc140, 9, 0),
        (EXG_ADDRESS_ADDRESS, 0xc148, 9, 0),
        (EXG_DATA_ADDRESS, 0xc188, 9, 0),
    ] {
        programs.push(SemanticProgramDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V2,
            program: compile_encoding_program(&[EncodingStep::Fields {
                base,
                width: 2,
                endian: EncodingEndian::Big,
                fields: vec![
                    EncodingFieldSpec {
                        input: 0,
                        shift: left_shift,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                    EncodingFieldSpec {
                        input: 1,
                        shift: right_shift,
                        bits: 3,
                        min: 0,
                        max: 7,
                    },
                ],
            }])?,
        });
    }
    let fixup = |id: &str,
                 width: u8,
                 base: FixupBase,
                 range: FixupRange,
                 relocation: PortableRelocationKind|
     -> Result<SemanticProgramDescriptor, OpcpuCodecError> {
        Ok(SemanticProgramDescriptor {
            owner: owner.clone(),
            id: id.to_string(),
            opcode_version: SEMANTIC_VM_OPCODE_VERSION_V4,
            program: compile_fixup_program(&[FixupEncodingStep {
                input: 0,
                width,
                endian: EncodingEndian::Big,
                base,
                range,
                unresolved: UnresolvedValuePolicy::Placeholder(0),
                relocation,
                transform: FixupTransform::Identity,
            }])?,
        })
    };
    programs.extend([
        fixup(
            FIXUP_PC_BYTE,
            1,
            FixupBase::Position {
                adjustment: 2,
                target_references_only: true,
            },
            FixupRange::Signed,
            PortableRelocationKind::None,
        )?,
        fixup(
            FIXUP_PC_WORD,
            2,
            FixupBase::Position {
                adjustment: 2,
                target_references_only: true,
            },
            FixupRange::Signed,
            PortableRelocationKind::None,
        )?,
        fixup(
            FIXUP_PC_WORD_BASE4,
            2,
            FixupBase::Position {
                adjustment: 4,
                target_references_only: true,
            },
            FixupRange::Signed,
            PortableRelocationKind::None,
        )?,
        fixup(
            FIXUP_ABSOLUTE_LONG,
            4,
            FixupBase::Value,
            FixupRange::BitPattern,
            PortableRelocationKind::Absolute,
        )?,
    ]);
    programs.push(branch_program(&owner)?);
    programs.extend(crate::m68080::package_programs::semantic_programs()?);
    Ok(programs)
}

/// Compile target-state defaults, transitions, and capability legality as one
/// family-owned matrix consumed by the CPU-neutral state runtime.
pub fn state_programs() -> Result<Vec<StateProgramDescriptor>, OpcpuCodecError> {
    let profiles = ["m68000", "m68010", "m68020", "m68030", "m68040", "m68080"];
    let all_profiles = profiles.iter().map(|value| value.to_string()).collect();
    let program = compile_state_program(&StateProgramSpec {
        profiles: profiles.iter().map(|value| value.to_string()).collect(),
        keys: vec![
            StateKeySpec {
                id: FPU_TARGET_KEY.to_string(),
                default: 0,
                overrides: vec![("m68080".to_string(), 4)],
            },
            StateKeySpec {
                id: APOLLO_MODE_KEY.to_string(),
                default: 0,
                overrides: vec![],
            },
            StateKeySpec {
                id: CPU_IS_68080_KEY.to_string(),
                default: 0,
                overrides: vec![("m68080".to_string(), 1)],
            },
            StateKeySpec {
                id: CPU_LEVEL_KEY.to_string(),
                default: 0,
                overrides: [
                    ("m68010", 1_u32),
                    ("m68020", 2),
                    ("m68030", 3),
                    ("m68040", 4),
                    ("m68080", 5),
                ]
                .into_iter()
                .map(|(profile, value)| (profile.to_string(), value))
                .collect(),
            },
        ],
        directives: vec![
            StateDirectiveSpec {
                id: "fpu".to_string(),
                key: FPU_TARGET_KEY.to_string(),
                arguments: vec![
                    StateArgumentSpec {
                        id: "none".to_string(),
                        value: 0,
                        allowed_profiles: all_profiles,
                    },
                    StateArgumentSpec {
                        id: "68881".to_string(),
                        value: 1,
                        allowed_profiles: vec!["m68020".to_string(), "m68030".to_string()],
                    },
                    StateArgumentSpec {
                        id: "68882".to_string(),
                        value: 2,
                        allowed_profiles: vec!["m68020".to_string(), "m68030".to_string()],
                    },
                    StateArgumentSpec {
                        id: "68040".to_string(),
                        value: 3,
                        allowed_profiles: vec!["m68040".to_string()],
                    },
                    StateArgumentSpec {
                        id: "68080".to_string(),
                        value: 4,
                        allowed_profiles: vec!["m68080".to_string()],
                    },
                ],
            },
            StateDirectiveSpec {
                id: "apollo".to_string(),
                key: APOLLO_MODE_KEY.to_string(),
                arguments: [("on", 1), ("1", 1), ("off", 0), ("0", 0)]
                    .into_iter()
                    .map(|(id, value)| StateArgumentSpec {
                        id: id.to_string(),
                        value,
                        allowed_profiles: vec!["m68080".to_string()],
                    })
                    .collect(),
            },
        ],
        capabilities: vec![
            StateCapabilitySpec {
                id: "fpu".to_string(),
                key: FPU_TARGET_KEY.to_string(),
                rules: vec![
                    StateCapabilityRuleSpec {
                        allowed_profiles: vec!["m68020".to_string(), "m68030".to_string()],
                        allowed_values: vec![1, 2],
                    },
                    StateCapabilityRuleSpec {
                        allowed_profiles: vec!["m68040".to_string()],
                        allowed_values: vec![3],
                    },
                    StateCapabilityRuleSpec {
                        allowed_profiles: vec!["m68080".to_string()],
                        allowed_values: vec![4],
                    },
                ],
            },
            StateCapabilitySpec {
                id: "apollo".to_string(),
                key: APOLLO_MODE_KEY.to_string(),
                rules: vec![StateCapabilityRuleSpec {
                    allowed_profiles: vec!["m68080".to_string()],
                    allowed_values: vec![1],
                }],
            },
        ],
    })?;
    Ok(vec![StateProgramDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        id: STATE_RUNTIME.to_string(),
        opcode_version: STATE_VM_OPCODE_VERSION_V1,
        program,
    }])
}

/// Convert a family-owned register spelling to the opaque class/index pair
/// consumed by the neutral operand-record runtime.
pub fn compile_register_input(register: &str) -> Option<(u16, u16)> {
    for (name, class) in [("USP", 4), ("SR", 5), ("CCR", 6)] {
        if register.eq_ignore_ascii_case(name) {
            return Some((class, 0));
        }
    }
    if register.eq_ignore_ascii_case("SP") {
        return Some((1, 7));
    }
    for (name, index) in [("FPCR", 0), ("FPSR", 1), ("FPIAR", 2)] {
        if register.eq_ignore_ascii_case(name) {
            return Some((3, index));
        }
    }
    if let Some(suffix) = register
        .strip_prefix("FP")
        .or_else(|| register.strip_prefix("fp"))
    {
        let index = suffix.parse::<u16>().ok()?;
        return (index <= 7).then_some((2, index));
    }
    let (prefix, suffix) = register.split_at_checked(1)?;
    let index = suffix.parse::<u16>().ok()?;
    if index > 7 {
        return None;
    }
    match prefix.to_ascii_uppercase().as_str() {
        "D" => Some((0, index)),
        "A" => Some((1, index)),
        _ => None,
    }
}

fn input_program_for_owner(
    owner: ScopedOwner,
    id: &str,
    constraints: &[ValueConstraint],
) -> Result<ValueProgramDescriptor, OpcpuCodecError> {
    Ok(ValueProgramDescriptor {
        owner,
        id: id.to_string(),
        opcode_version: VALUE_VM_OPCODE_VERSION_V1,
        program: compile_value_program(ValueProgramSource::Input(0), constraints)?,
    })
}

fn input_program(
    id: &str,
    constraints: &[ValueConstraint],
) -> Result<ValueProgramDescriptor, OpcpuCodecError> {
    input_program_for_owner(
        ScopedOwner::Family("motorola68000".to_string()),
        id,
        constraints,
    )
}

fn absolute_value_programs_for_owner(
    owner: ScopedOwner,
    address_bits: u8,
) -> Result<Vec<ValueProgramDescriptor>, OpcpuCodecError> {
    Ok(vec![
        input_program_for_owner(
            owner.clone(),
            VALUE_ABSOLUTE_WORD,
            &[
                ValueConstraint::NormalizeTwosComplement(address_bits),
                ValueConstraint::SignedBits(16),
            ],
        )?,
        input_program_for_owner(
            owner,
            VALUE_ABSOLUTE_LONG,
            &[ValueConstraint::UnsignedBits(address_bits)],
        )?,
    ])
}

/// CPU-owned address-width overrides inherited by 68020 and later profiles.
pub fn m68020_value_programs(cpu_id: &str) -> Result<Vec<ValueProgramDescriptor>, OpcpuCodecError> {
    absolute_value_programs_for_owner(ScopedOwner::Cpu(cpu_id.to_string()), 32)
}

/// Compile the scalar rules currently owned by the Rust m68k family handler.
pub fn value_programs() -> Result<Vec<ValueProgramDescriptor>, OpcpuCodecError> {
    let normalize = ValueConstraint::NormalizeTwosComplement(32);
    let mut programs = vec![
        input_program(VALUE_NORMALIZED_INPUT, &[normalize])?,
        input_program(
            VALUE_SIGNED_BYTE,
            &[normalize, ValueConstraint::SignedBits(8)],
        )?,
        input_program(
            VALUE_UNSIGNED_BYTE,
            &[normalize, ValueConstraint::UnsignedBits(8)],
        )?,
        input_program(
            VALUE_SIGNED_WORD,
            &[normalize, ValueConstraint::SignedBits(16)],
        )?,
        input_program(
            VALUE_IMMEDIATE_BYTE,
            &[
                normalize,
                ValueConstraint::InclusiveRange {
                    min: IMMEDIATE_BYTE_RANGE.0,
                    max: IMMEDIATE_BYTE_RANGE.1,
                },
            ],
        )?,
        input_program(
            VALUE_IMMEDIATE_WORD,
            &[
                normalize,
                ValueConstraint::InclusiveRange {
                    min: IMMEDIATE_WORD_RANGE.0,
                    max: IMMEDIATE_WORD_RANGE.1,
                },
            ],
        )?,
        input_program(
            VALUE_IMMEDIATE_LONG,
            &[
                normalize,
                ValueConstraint::InclusiveRange {
                    min: IMMEDIATE_LONG_RANGE.0,
                    max: IMMEDIATE_LONG_RANGE.1,
                },
            ],
        )?,
        ValueProgramDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            id: VALUE_LITERAL_ZERO.to_string(),
            opcode_version: VALUE_VM_OPCODE_VERSION_V1,
            program: compile_value_program(ValueProgramSource::Literal(0), &[])?,
        },
        input_program(
            VALUE_BIT_FIELD_OFFSET,
            &[
                normalize,
                ValueConstraint::InclusiveRange { min: 0, max: 31 },
            ],
        )?,
        input_program(
            VALUE_BIT_FIELD_WIDTH,
            &[
                normalize,
                ValueConstraint::InclusiveRange { min: 1, max: 32 },
            ],
        )?,
        ValueProgramDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            id: VALUE_PACKED_THREE_BIT_COUNT.to_string(),
            opcode_version: VALUE_VM_OPCODE_VERSION_V2,
            program: compile_value_program_v2(
                ValueProgramSource::Input(0),
                &[ValueConstraint::EncodeUpperBoundAsZero(3)],
            )?,
        },
    ];
    programs.extend(absolute_value_programs_for_owner(
        ScopedOwner::Family("motorola68000".to_string()),
        24,
    )?);
    Ok(programs)
}

fn record(
    id: &str,
    program: OperandRecordProgram,
) -> Result<OperandRecordProgramDescriptor, OpcpuCodecError> {
    Ok(OperandRecordProgramDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        id: id.to_string(),
        schema_version: OPERAND_RECORD_VM_VERSION_V1,
        program: compile_operand_record_program(program)?,
    })
}

fn structured_record(
    id: &str,
    program: OperandRecordProgram,
) -> Result<OperandRecordProgramDescriptor, OpcpuCodecError> {
    Ok(OperandRecordProgramDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        id: id.to_string(),
        schema_version: OPERAND_RECORD_VM_VERSION_V2,
        program: compile_operand_record_program(program)?,
    })
}

fn composite_record(
    id: &str,
    format: u16,
) -> Result<OperandRecordProgramDescriptor, OpcpuCodecError> {
    Ok(OperandRecordProgramDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        id: id.to_string(),
        schema_version: OPERAND_RECORD_VM_VERSION_V3,
        program: compile_operand_record_program(OperandRecordProgram::Composite {
            format,
            first_record_input: Some(0),
        })?,
    })
}

/// Compile base addressing shapes to neutral operand-record constructors.
pub fn operand_record_programs() -> Result<Vec<OperandRecordProgramDescriptor>, OpcpuCodecError> {
    let register = |register_input| OperandRecordBaseSource::Register(register_input);
    let indexed = |base, index_width_bits| OperandRecordProgram::Indexed {
        base,
        index_register_input: 1,
        index_width_bits,
        scale: 1,
        value_input: 0,
    };
    let mut programs = vec![
        record(
            RECORD_DATA_REGISTER,
            OperandRecordProgram::Register { register_input: 0 },
        )?,
        record(
            RECORD_ADDRESS_REGISTER,
            OperandRecordProgram::Register { register_input: 0 },
        )?,
        record(
            RECORD_ADDRESS_INDIRECT,
            OperandRecordProgram::Indirect {
                register_input: 0,
                update: OperandRecordUpdate::None,
            },
        )?,
        record(
            RECORD_ADDRESS_POSTINCREMENT,
            OperandRecordProgram::Indirect {
                register_input: 0,
                update: OperandRecordUpdate::Postincrement,
            },
        )?,
        record(
            RECORD_ADDRESS_PREDECREMENT,
            OperandRecordProgram::Indirect {
                register_input: 0,
                update: OperandRecordUpdate::Predecrement,
            },
        )?,
        record(
            RECORD_ADDRESS_DISPLACEMENT,
            OperandRecordProgram::Displacement {
                base: register(0),
                value_input: 0,
            },
        )?,
        record(RECORD_ADDRESS_INDEXED_WORD, indexed(register(0), 16))?,
        record(RECORD_ADDRESS_INDEXED_LONG, indexed(register(0), 32))?,
        record(
            RECORD_PC_DISPLACEMENT,
            OperandRecordProgram::Displacement {
                base: OperandRecordBaseSource::ProgramCounter,
                value_input: 0,
            },
        )?,
        record(
            RECORD_PC_INDEXED_WORD,
            indexed(OperandRecordBaseSource::ProgramCounter, 16),
        )?,
        record(
            RECORD_PC_INDEXED_LONG,
            indexed(OperandRecordBaseSource::ProgramCounter, 32),
        )?,
        record(
            RECORD_ABSOLUTE_WORD,
            OperandRecordProgram::Absolute {
                value_input: 0,
                width_bits: 16,
            },
        )?,
        record(
            RECORD_ABSOLUTE_LONG,
            OperandRecordProgram::Absolute {
                value_input: 0,
                width_bits: 32,
            },
        )?,
        record(
            RECORD_IMMEDIATE,
            OperandRecordProgram::Immediate { value_input: 0 },
        )?,
    ];
    programs.extend([
        structured_record(
            RECORD_FULL_ADDRESS_PREINDEXED,
            OperandRecordProgram::NestedAddress {
                base: OperandRecordBaseSource::Register(0),
                base_displacement: OperandRecordOptionalValueSource::Input {
                    index: 0,
                    width_bits: 16,
                },
                index: OperandRecordOptionalIndexSource::Input {
                    index: 1,
                    width_bits: 32,
                    scale: 4,
                },
                indirection: OperandRecordIndirection::Preindexed,
                outer_displacement: OperandRecordOptionalValueSource::Input {
                    index: 1,
                    width_bits: 32,
                },
            },
        )?,
        structured_record(
            RECORD_FULL_PC_POSTINDEXED,
            OperandRecordProgram::NestedAddress {
                base: OperandRecordBaseSource::ProgramCounter,
                base_displacement: OperandRecordOptionalValueSource::None,
                index: OperandRecordOptionalIndexSource::Input {
                    index: 0,
                    width_bits: 16,
                    scale: 2,
                },
                indirection: OperandRecordIndirection::Postindexed,
                outer_displacement: OperandRecordOptionalValueSource::Input {
                    index: 0,
                    width_bits: 16,
                },
            },
        )?,
        structured_record(
            RECORD_FULL_SUPPRESSED_INDEX,
            OperandRecordProgram::NestedAddress {
                base: OperandRecordBaseSource::Suppressed,
                base_displacement: OperandRecordOptionalValueSource::None,
                index: OperandRecordOptionalIndexSource::Input {
                    index: 0,
                    width_bits: 32,
                    scale: 8,
                },
                indirection: OperandRecordIndirection::None,
                outer_displacement: OperandRecordOptionalValueSource::None,
            },
        )?,
        structured_record(
            RECORD_FULL_ADDRESS_BASE_ONLY,
            OperandRecordProgram::NestedAddress {
                base: OperandRecordBaseSource::Register(0),
                base_displacement: OperandRecordOptionalValueSource::Input {
                    index: 0,
                    width_bits: 32,
                },
                index: OperandRecordOptionalIndexSource::None,
                indirection: OperandRecordIndirection::None,
                outer_displacement: OperandRecordOptionalValueSource::None,
            },
        )?,
        structured_record(
            RECORD_REGISTER_PAIR,
            OperandRecordProgram::RegisterPair {
                left_register_input: 0,
                right_register_input: 1,
                indirect: false,
            },
        )?,
        structured_record(
            RECORD_REGISTER_GROUP,
            OperandRecordProgram::RegisterRange {
                start_register_input: 0,
                end_register_input: 1,
            },
        )?,
        structured_record(
            RECORD_INDIRECT_REGISTER_PAIR,
            OperandRecordProgram::RegisterPair {
                left_register_input: 0,
                right_register_input: 1,
                indirect: true,
            },
        )?,
        structured_record(
            RECORD_REGISTER_LIST,
            OperandRecordProgram::RegisterList {
                first_register_input: 0,
            },
        )?,
        structured_record(
            RECORD_BIT_FIELD_REGISTER_OFFSET,
            OperandRecordProgram::Field {
                record_input: 0,
                offset: OperandRecordFieldSource::Register(0),
                width: OperandRecordFieldSource::Value(0),
            },
        )?,
        structured_record(
            RECORD_BIT_FIELD_IMMEDIATE,
            OperandRecordProgram::Field {
                record_input: 0,
                offset: OperandRecordFieldSource::Value(0),
                width: OperandRecordFieldSource::Value(1),
            },
        )?,
        structured_record(
            RECORD_BIT_FIELD_VALUE_REGISTER,
            OperandRecordProgram::Field {
                record_input: 0,
                offset: OperandRecordFieldSource::Value(0),
                width: OperandRecordFieldSource::Register(0),
            },
        )?,
        structured_record(
            RECORD_BIT_FIELD_REGISTERS,
            OperandRecordProgram::Field {
                record_input: 0,
                offset: OperandRecordFieldSource::Register(0),
                width: OperandRecordFieldSource::Register(1),
            },
        )?,
        record(
            RECORD_FPU_DATA_REGISTER,
            OperandRecordProgram::Register { register_input: 0 },
        )?,
        record(
            RECORD_FPU_CONTROL_REGISTER,
            OperandRecordProgram::Register { register_input: 0 },
        )?,
        structured_record(
            RECORD_FPU_REGISTER_LIST,
            OperandRecordProgram::RegisterList {
                first_register_input: 0,
            },
        )?,
    ]);
    programs.extend(
        [
            (RECORD_FPU_FORMAT_BYTE, FPU_FORMAT_BYTE),
            (RECORD_FPU_FORMAT_WORD, FPU_FORMAT_WORD),
            (RECORD_FPU_FORMAT_LONG, FPU_FORMAT_LONG),
            (RECORD_FPU_FORMAT_SINGLE, FPU_FORMAT_SINGLE),
            (RECORD_FPU_FORMAT_DOUBLE, FPU_FORMAT_DOUBLE),
            (RECORD_FPU_FORMAT_EXTENDED, FPU_FORMAT_EXTENDED),
            (RECORD_FPU_FORMAT_PACKED, FPU_FORMAT_PACKED),
        ]
        .into_iter()
        .map(|(id, format)| composite_record(id, format))
        .collect::<Result<Vec<_>, _>>()?,
    );
    Ok(programs)
}

fn exact_aliases(
    owner: ScopedOwner,
    id: &str,
    aliases: &[(&str, &str)],
) -> Result<SelectorProgramDescriptor, OpcpuCodecError> {
    Ok(SelectorProgramDescriptor {
        owner,
        id: id.to_string(),
        opcode_version: SELECTOR_VM_OPCODE_VERSION_V1,
        priority: 0,
        cpu_allow_list: None,
        program: compile_selector_map_program(
            &aliases
                .iter()
                .map(|(input, target)| ((*input).to_string(), (*target).to_string()))
                .collect::<Vec<_>>(),
        )?,
    })
}

/// Compile the family-owned spelling aliases and branch-short ordering rules.
pub fn selector_programs() -> Result<Vec<SelectorProgramDescriptor>, OpcpuCodecError> {
    let owner = ScopedOwner::Dialect("motorola68k".to_string());
    let aliases = [
        ("BHS", "BCC"),
        ("BLO", "BCS"),
        ("DBRA", "DBF"),
        ("DBHS", "DBCC"),
        ("DBLO", "DBCS"),
        ("SHS", "SCC"),
        ("SLO", "SCS"),
        ("PACKUSBW", "PACKUSWB"),
    ];
    let branch_bases = [
        ("BRA", "BRA"),
        ("BSR", "BSR"),
        ("BHI", "BHI"),
        ("BLS", "BLS"),
        ("BCC", "BCC"),
        ("BHS", "BCC"),
        ("BCS", "BCS"),
        ("BLO", "BCS"),
        ("BNE", "BNE"),
        ("BEQ", "BEQ"),
        ("BVC", "BVC"),
        ("BVS", "BVS"),
        ("BPL", "BPL"),
        ("BMI", "BMI"),
        ("BGE", "BGE"),
        ("BLT", "BLT"),
        ("BGT", "BGT"),
        ("BLE", "BLE"),
    ];
    Ok(vec![
        exact_aliases(owner.clone(), "a", &aliases)?,
        SelectorProgramDescriptor {
            owner,
            id: "b".to_string(),
            opcode_version: SELECTOR_VM_OPCODE_VERSION_V1,
            priority: 0,
            cpu_allow_list: None,
            program: compile_selector_suffix_program(
                &branch_bases
                    .iter()
                    .map(|(input, target)| ((*input).to_string(), (*target).to_string()))
                    .collect::<Vec<_>>(),
                ".",
                ".S",
                ".B",
                DIAG_SELECTOR_UNSUPPORTED_QUALIFIER,
            )?,
        },
    ])
}

/// Compile aliases introduced by the 68020 instruction surface for one CPU
/// profile that inherits that surface.
pub fn m68020_selector_programs() -> Result<Vec<SelectorProgramDescriptor>, OpcpuCodecError> {
    let mut program = exact_aliases(
        ScopedOwner::Dialect("motorola68k".to_string()),
        "t",
        &[("TRAPHS", "TRAPCC"), ("TRAPLO", "TRAPCS")],
    )?;
    program.cpu_allow_list = Some(
        ["m68020", "m68030", "m68040", "m68080"]
            .into_iter()
            .map(str::to_string)
            .collect(),
    );
    Ok(vec![program])
}

pub fn diagnostics() -> Vec<DiagnosticDescriptor> {
    vec![
        DiagnosticDescriptor {
            code: DIAG_SELECTOR_UNSUPPORTED_QUALIFIER.to_string(),
            message_template: "unsupported selector qualifier".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_STOP_IMMEDIATE_RANGE.to_string(),
            message_template: "STOP immediate value {value} out of 16-bit range".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_LINK_DISPLACEMENT_RANGE.to_string(),
            message_template: "LINK displacement {value} out of 16-bit signed range".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_TRAP_VECTOR_RANGE.to_string(),
            message_template: "TRAP vector {value} out of range (0-15)".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVEQ_IMMEDIATE_RANGE.to_string(),
            message_template: "MOVEQ immediate value {value} out of signed 8-bit range".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVE_UNSUPPORTED_SIZE.to_string(),
            message_template: "unsupported size suffix for MOVE".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_COUNT_RANGE.to_string(),
            message_template: "{mnemonic} count out of range (1-8)".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_INVALID_DESTINATION.to_string(),
            message_template: "invalid destination effective address for {mnemonic}".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_SWAP_REGISTER.to_string(),
            message_template: "SWAP operand must be a data register".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_EXG_PAIR.to_string(),
            message_template: "EXG requires data/address register pairs".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_DBCC_COUNTER.to_string(),
            message_template: "{mnemonic} counter must be a data register".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_STOP_OPERAND.to_string(),
            message_template: "STOP operand must be an immediate status word".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_UNSUPPORTED_BYTE_SIZE.to_string(),
            message_template: "{mnemonic} does not support .B size".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVE_FROM_CCR.to_string(),
            message_template: "MOVE from CCR is not supported".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVE_USP_SOURCE.to_string(),
            message_template: "MOVE USP source must be an address register".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_ANDI_WORD_CCR.to_string(),
            message_template: "ANDI does not support .W size for CCR".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVE_TO_SR_SOURCE.to_string(),
            message_template: "invalid source effective address for MOVE to SR".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_BIT_NUMBER.to_string(),
            message_template: "{mnemonic} bit number must be an immediate value or data register"
                .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_UNSUPPORTED_LONG_SIZE.to_string(),
            message_template: "{mnemonic} does not support .L size".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_INVALID_SOURCE.to_string(),
            message_template: "invalid source effective address for {mnemonic}".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_DESTINATION_DATA_REGISTER.to_string(),
            message_template: "{mnemonic} destination must be a data register".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_EXTEND_SHAPE.to_string(),
            message_template: "{mnemonic} operands must both be data registers or both be predecrement address operands".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_NO_SIZE_SUFFIX.to_string(),
            message_template: "{mnemonic} does not accept a size suffix".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_CMPM_SHAPE.to_string(),
            message_template: "CMPM operands must both be postincrement address operands"
                .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MEMORY_LONG_SIZE.to_string(),
            message_template: "{mnemonic} memory form does not support .L size".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_DUPLICATE_REGISTER.to_string(),
            message_template: "duplicate register in {mnemonic} list: {register}".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVEP_ADDRESSING.to_string(),
            message_template: "MOVEP memory operand must use d16(An) addressing".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVEC_CONTROL_M68010.to_string(),
            message_template: "unsupported MOVEC control register for m68010".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVEC_CONTROL_M68040.to_string(),
            message_template: "unsupported MOVEC control register for m68040".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVEC_CAAR_M68040.to_string(),
            message_template: "MOVEC CAAR is not supported on m68040".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_MOVEC_BASELINE.to_string(),
            message_template: "MOVEC is not supported on baseline 68000".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_M68020_REQUIRED.to_string(),
            message_template: "{mnemonic} is only supported on m68020 and later".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_DIVS_LONG_BASELINE.to_string(),
            message_template: "DIVS does not support .L size on baseline 68000".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_DIVS_LONG_M68010.to_string(),
            message_template: "DIVS does not support .L size on m68010".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_M68080_REGISTER_M68040.to_string(),
            message_template: "E/B register requires .cpu 68080 and is unavailable on m68040"
                .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_M68080_ONLY.to_string(),
            message_template: "{mnemonic} is only supported on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_FPU_DATA_REGISTER_EA.to_string(),
            message_template: "FPU data registers are not valid effective addresses for {form}"
                .to_string(),
        },
        DiagnosticDescriptor {
            code: "encoding.fpu-disabled.m68020".to_string(),
            message_template:
                "{mnemonic} requires an active .fpu target on m68020".to_string(),
        },
        DiagnosticDescriptor {
            code: "encoding.fpu-disabled.m68030".to_string(),
            message_template:
                "{mnemonic} requires an active .fpu target on m68030".to_string(),
        },
        DiagnosticDescriptor {
            code: "encoding.fpu-disabled.m68040".to_string(),
            message_template: "{mnemonic} requires an active .fpu target on m68040; legal .fpu targets for m68040 FPU instructions: 68040".to_string(),
        },
        DiagnosticDescriptor {
            code: "encoding.fpu-disabled.m68080".to_string(),
            message_template:
                "{mnemonic} requires an active .fpu target on m68080".to_string(),
        },
        DiagnosticDescriptor {
            code: "encoding.fpu-integrated-unsupported.m68040".to_string(),
            message_template:
                "{mnemonic} is not supported by the integrated 68040 FPU target".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_INVALID_DESTINATION_FORM.to_string(),
            message_template: "invalid destination effective address for {form}".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_FULL_EXTENSION_DISPLACEMENT.to_string(),
            message_template:
                "68020 full-extension base displacement requires explicit .W or .L".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_FULL_EXTENSION_UNSUPPORTED.to_string(),
            message_template:
                "68020+ full-extension addressing is not supported on this CPU".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_INVALID_DISPLACEMENT_BASE.to_string(),
            message_template: "invalid 68000 displacement base register".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_BKPT_VECTOR_RANGE.to_string(),
            message_template: "BKPT vector out of range (0-7)".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_RTD_DISPLACEMENT_RANGE.to_string(),
            message_template: "RTD displacement out of 16-bit signed range".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_LINK_LONG_DISPLACEMENT_RANGE.to_string(),
            message_template: "LINK.L displacement {value} out of 32-bit range".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_STATUS_BYTE_IMMEDIATE_RANGE.to_string(),
            message_template: "status-register immediate value {value} out of 8-bit range"
                .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_STATUS_WORD_IMMEDIATE_RANGE.to_string(),
            message_template: "status-register immediate value {value} out of 16-bit range"
                .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_ABSOLUTE_WORD_RANGE.to_string(),
            message_template: "68000 absolute .W address out of 16-bit range".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_CAS2_BYTE_SIZE.to_string(),
            message_template: "CAS2 does not support .B size".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_CAS2_MEMORY_PAIR.to_string(),
            message_template:
                "CAS2 memory operand must use (An):(Am) address-register pair syntax"
                    .to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_DIV_PAIR_DISTINCT.to_string(),
            message_template: "{mnemonic} register-pair destination requires distinct remainder and quotient registers".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_DIVSL_PAIR_REQUIRED.to_string(),
            message_template: "{mnemonic} destination must be a data-register pair".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_CMP2_BOUNDS.to_string(),
            message_template: "invalid bounds effective address for {form}".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_TRAPCC_UNSIZED_OPERAND.to_string(),
            message_template: "unsized TRAPcc does not take an immediate operand".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_CALLM_COUNT.to_string(),
            message_template: "CALLM count {value} out of range (0-255)".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_CALLM_M68040.to_string(),
            message_template: "CALLM is not supported on m68040".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_RTM_OPERAND.to_string(),
            message_template: "RTM operand must be a data or address register".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_RTM_M68030.to_string(),
            message_template: "RTM is only supported on m68020".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_RTM_M68040.to_string(),
            message_template: "RTM is not supported on m68040".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_PFLUSH_BASELINE.to_string(),
            message_template: "PFLUSH is only supported on m68020 and later".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_PFLUSH_M68020.to_string(),
            message_template: "PFLUSH is not supported on m68020".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_PFLUSH_M68040_ARITY.to_string(),
            message_template:
                "PFLUSH expects exactly one address-indirect operand on m68040".to_string(),
        },
        DiagnosticDescriptor {
            code: DIAG_BITFIELD_EA.to_string(),
            message_template: "invalid bit-field effective address for {mnemonic}".to_string(),
        },
    ]
}

/// Existing family scalar normalization retained as the differential oracle
/// while package-first conversion is incomplete.
pub fn oracle_normalize_wrapped_i32(value: i64) -> i64 {
    M68KFamilyHandler::normalize_wrapped_i32(value)
}
