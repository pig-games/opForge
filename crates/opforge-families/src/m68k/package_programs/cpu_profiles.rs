// SPDX-License-Identifier: GPL-3.0-or-later
//! CPU-profile additions layered on the shared Motorola baseline.
//! Descriptor construction order is retained by the public profile entry points.
use super::*;

/// Forms introduced by the 68010 and inherited by later profiles.
pub fn m68010_instruction_form_mnemonics() -> Vec<String> {
    ["BKPT", "RTD", "MOVEC", "MOVES.W", "MOVES.L"]
        .into_iter()
        .map(str::to_string)
        .collect()
}

/// Control-register identities inherited from the 68010 profile.
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

/// Forms introduced by the 68020 and inherited by later profiles.
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

/// Instruction programs for the 68010 delta.
pub fn m68010_instruction_programs(cpu_id: &str) -> Vec<VmProgramDescriptor> {
    let owner = ScopedOwner::Cpu(cpu_id.to_string());
    ["BKPT", "RTD", "MOVEC", "MOVES.W", "MOVES.L"]
        .into_iter()
        .map(|mnemonic| semantic_instruction_program(&owner, mnemonic))
        .collect()
}

/// Instruction programs for the 68020 delta.
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

/// Operand selectors for the 68010 delta.
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

/// Operand selectors for the 68020 delta.
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

/// Address-width override inherited by 68020 and later profiles.
pub fn m68020_value_programs(cpu_id: &str) -> Result<Vec<ValueProgramDescriptor>, OpcpuCodecError> {
    absolute_value_programs_for_owner(ScopedOwner::Cpu(cpu_id.to_string()), 32)
}

/// Spelling aliases introduced by the 68020 instruction surface.
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
