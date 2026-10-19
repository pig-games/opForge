// SPDX-License-Identifier: GPL-3.0-or-later
//! MOVE effective-address combinations requiring memory extensions.
//! Register-only pairs stay with the package register-route owner.
use super::*;

fn selector(mnemonic: &str, shape: &str, plan: String, priority: u16) -> ModeSelectorDescriptor {
    ModeSelectorDescriptor {
        owner: ScopedOwner::Family("motorola68000".into()),
        mnemonic: mnemonic.into(),
        shape_key: shape.into(),
        mode_key: "semantic".into(),
        operand_plan: plan,
        priority,
        unstable_widen: false,
        width_rank: 0,
    }
}

pub(super) fn append_absolute_destinations(selectors: &mut Vec<ModeSelectorDescriptor>) {
    for (mnemonic, size_base) in [
        ("MOVE.B", 0x1000u32),
        ("MOVE.W", 0x3000),
        ("MOVE.L", 0x2000),
    ] {
        for (mode, source) in [
            (
                0x10,
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
            let opcode = |destination| {
                format!("encode:{PARAM_FIELD_0}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{source}", size_base + mode + destination)
            };
            // Explicit qualifiers retain their own range/relocation policy.
            for (field, destination) in [("W", 0x1c0), ("L", 0x3c0)] {
                let payload = if field == "W" {
                    format!("encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_REQUIRED_VALUE_PROGRAM_PREFIX}{VALUE_ABSOLUTE_WORD}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_SEPARATOR}{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}W")
                } else {
                    format!("fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:{MODE_SELECTOR_PLAN_MEMBER_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}L")
                };
                selectors.push(selector(mnemonic, "direct_direct", format!("{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{source},{MODE_SELECTOR_PLAN_MEMBER_SHAPE_PREFIX}1{MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR}{field};{};{payload}", opcode(destination)), 94));
            }
            selectors.push(selector(mnemonic,"direct_direct",format!("{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{source},target:expr1;{};fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1",opcode(0x3c0)),95));
            // Numeric addresses choose the sign-extended absolute-word form
            // when possible; larger addresses use the CPU-owned long bound.
            for (destination, value, priority) in [
                (0x1c0, VALUE_ABSOLUTE_WORD, 96),
                (0x3c0, VALUE_ABSOLUTE_LONG, 97),
            ] {
                let scalar = if destination == 0x1c0 {
                    PARAM_SCALAR_WORD
                } else {
                    PARAM_SCALAR_LONG
                };
                selectors.push(selector(mnemonic,"direct_direct",format!("{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{source},{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{value}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_SEPARATOR}expr1;{};encode:{scalar}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_PREFIX}{value}{MODE_SELECTOR_PLAN_VALUE_PROGRAM_SEPARATOR}expr1",opcode(destination)),priority));
            }
        }
    }
}

pub(super) fn append_immediate_and_absolute_sources(selectors: &mut Vec<ModeSelectorDescriptor>) {
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
                        "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,{destination_plan};encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{destination_plan};fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0",
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
        if mnemonic == "MOVE.L" {
            continue;
        }
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
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1,{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_ARITY_PREFIX}1.value2;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_REGISTER_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}1.class1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_INDIRECT_TUPLE_VALUE_PREFIX}1{MODE_SELECTOR_PLAN_TUPLE_ITEM_SEPARATOR}0",
            0x203c_u32 + 0x140
        ),
        // A symbolic immediate, including an addend, needs its relocation step before
        // the generic numeric-immediate tuple fallback can consume the value.
        priority: 111,
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
    // Both effective addresses are long even when the transferred value is a
    // byte or word. Retain each target's independent relocation and addend.
    for (mnemonic, opcode) in [
        ("MOVE.B", 0x13f9_u32),
        ("MOVE.W", 0x33f9),
        ("MOVE.L", 0x23f9),
    ] {
        selectors.push(selector(
            mnemonic,
            "direct_direct",
            format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,target:expr1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{opcode};fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr1"
            ),
            127,
        ));
    }
    selectors.push(ModeSelectorDescriptor {
        owner: ScopedOwner::Family("motorola68000".to_string()),
        mnemonic: "MOVE.L".to_string(),
        shape_key: "direct_direct".to_string(),
        mode_key: "semantic".to_string(),
        operand_plan: format!(
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,scalar_expr1;encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}9209;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0;encode:{PARAM_SCALAR_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}scalar_expr1"
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
            "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}encode:{PARAM_SCALAR_WORD}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}9212;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr0;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}expr1"
        ),
        // Both scalar immediates and memory addresses need the same bounded
        // fixup path so section identities cannot disappear during evaluation.
        priority: 130,
        unstable_widen: false,
        width_rank: 0,
    });
}

pub(super) fn append_memory_and_address_sources(selectors: &mut Vec<ModeSelectorDescriptor>) {
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
}
