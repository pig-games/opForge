// SPDX-License-Identifier: GPL-3.0-or-later
//! Address-register ALU source effective addresses and immediate pointers.
use super::*;

pub(super) fn append(selectors: &mut Vec<ModeSelectorDescriptor>) {
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
        // Pointer immediates require a fixup, not scalar encoding which
        // deliberately refuses section targets. Numeric immediates retain
        // their existing width/range path below.
        if mnemonic.ends_with(".L") {
            selectors.push(ModeSelectorDescriptor {
                owner: ScopedOwner::Family("motorola68000".to_string()),
                mnemonic: mnemonic.to_string(),
                shape_key: "immediate_register".to_string(),
                mode_key: "semantic".to_string(),
                operand_plan: format!(
                    "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,reg1.class1;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0",
                    base + 0x3c
                ),
                priority: 1,
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
        // Symbol-bearing scalar sources use an absolute-long address for both
        // operation widths. The target predicate preserves numeric/register
        // candidates and the fixup program owns relocation identity.
        selectors.push(ModeSelectorDescriptor {
            owner: ScopedOwner::Family("motorola68000".to_string()),
            mnemonic: mnemonic.to_string(),
            shape_key: "direct_register".to_string(),
            mode_key: "semantic".to_string(),
            operand_plan: format!(
                "{MODE_SELECTOR_PLAN_SEMANTIC_SEQUENCE_PREFIX}match:_{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0,reg1.class1;encode:{PARAM_FIELD_9}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}{MODE_SELECTOR_PLAN_LITERAL_PREFIX}{},reg1.class1;fixup:{FIXUP_ABSOLUTE_LONG}{MODE_SELECTOR_PLAN_INPUT_SEPARATOR}target:expr0",
                base + 0x39
            ),
            priority: 9,
            unstable_widen: false,
            width_rank: 0,
        });
    }
}
