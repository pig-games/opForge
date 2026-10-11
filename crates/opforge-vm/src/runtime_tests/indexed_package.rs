use super::*;

#[test]
fn mos6502_force_rejection_preserves_canonical_diagnostics_and_valid_encodings() {
    let registry = mos6502_family_registry();
    let chunks = build_hierarchy_chunks_from_registry(&registry).unwrap();
    let bytes = encode_hierarchy_chunks_from_chunks(&chunks).unwrap();
    let models = [
        HierarchyExecutionModel::from_chunks(chunks).unwrap(),
        HierarchyExecutionModel::from_package_bytes(&bytes).unwrap(),
    ];
    let mut ctx = TestAssemblerContext::new();
    ctx.cpu_flags.insert(state::DIRECT_PAGE_KEY.into(), 0);
    ctx.cpu_flags.insert(state::DIRECT_PAGE_KNOWN_KEY.into(), 1);
    for model in &models {
        for suffix in ["d", "D", "b", "B", "k", "K", "l", "L"] {
            for operand in [
                Expr::Identifier(suffix.into(), Span::default()),
                Expr::Register(suffix.into(), Span::default()),
            ] {
                let error = model
                    .encode_instruction_from_exprs(
                        "m6502",
                        None,
                        "LDA",
                        &[Expr::Number("16".into(), Span::default()), operand],
                        &ctx,
                    )
                    .expect_err("base CPU must reject every explicit bank/width suffix");
                assert_eq!(
                    error,
                    RuntimeBridgeError::Resolve(
                        "65816-only addressing mode not supported on base 6502".into()
                    )
                );
            }
        }
        // An unrelated identifier is not an override and must not acquire OTR004.
        assert_eq!(
            model
                .encode_instruction_from_exprs(
                    "m6502",
                    None,
                    "LDA",
                    &[
                        Expr::Number("16".into(), Span::default()),
                        Expr::Identifier("other".into(), Span::default())
                    ],
                    &ctx,
                )
                .unwrap(),
            None
        );
        for (mnemonic, value, register, expected) in [
            ("LDA", "16", "X", vec![0xb5, 0x10]),
            ("LDA", "16", "Y", vec![0xb9, 0x10, 0]),
            ("LDX", "16", "Y", vec![0xb6, 0x10]),
        ] {
            assert_eq!(
                model
                    .encode_instruction_from_exprs(
                        "m6502",
                        None,
                        mnemonic,
                        &indexed(value, register),
                        &ctx,
                    )
                    .unwrap(),
                Some(expected)
            );
        }
        for (mnemonic, operands, expected) in [
            ("NOP", vec![], vec![0xea]),
            ("RTS", vec![], vec![0x60]),
            (
                "ASL",
                vec![Expr::Register("A".into(), Span::default())],
                vec![0x0a],
            ),
        ] {
            assert_eq!(
                model
                    .encode_instruction_from_exprs("m6502", None, mnemonic, &operands, &ctx,)
                    .unwrap(),
                Some(expected)
            );
        }
        let scalar = || Expr::Number("16".into(), Span::default());
        let name = |value: &str| Expr::Identifier(value.into(), Span::default());
        for (mnemonic, operands) in [
            ("BRA", vec![scalar()]),
            ("BRA", vec![scalar(), name("d")]),
            ("STZ", vec![scalar()]),
            ("STZ", vec![scalar(), name("l")]),
            ("LDA", vec![]),
            ("LDA", vec![scalar(), name("d"), scalar()]),
            ("NOP", vec![scalar()]),
            ("NOP", vec![scalar(), name("other")]),
            ("ASL", vec![name("other")]),
            (
                "LDA",
                vec![Expr::Tuple(vec![scalar(), scalar()], Span::default())],
            ),
        ] {
            assert!(
                !matches!(
                    model.encode_instruction_from_exprs("m6502", None, mnemonic, &operands, &ctx,),
                    Ok(Some(_))
                ),
                "invalid base form {mnemonic} {operands:?} encoded"
            );
        }
        for (mnemonic, suffix, expected) in [
            ("LDA", "d", vec![0xa5, 0x10]),
            ("LDA", "b", vec![0xad, 0x10, 0]),
            ("JMP", "k", vec![0x4c, 0x10, 0]),
            ("LDA", "l", vec![0xaf, 0x10, 0, 0]),
        ] {
            assert_eq!(
                model
                    .encode_instruction_from_exprs(
                        "65816",
                        None,
                        mnemonic,
                        &indexed("16", suffix),
                        &ctx,
                    )
                    .unwrap(),
                Some(expected),
                "65816 {mnemonic} ,{suffix}"
            );
        }
    }
}

#[test]
fn mos6502_force_rejection_compact_package_retains_exact_predicates() {
    use crate::binary_source_package::{BinarySourcePackage, CandidateRecipe, Projection};
    use crate::runtime_model_core::RuntimeModelCore;
    let chunks = build_hierarchy_chunks_from_registry(&mos6502_family_registry()).unwrap();
    let cpu_programs: Vec<_> = chunks
        .tables
        .iter()
        .filter(|row| row.owner == ScopedOwner::Cpu("m6502".into()))
        .collect();
    assert!(!cpu_programs.is_empty());
    for row in cpu_programs {
        let original = chunks
            .tables
            .iter()
            .find(|family| {
                family.owner == ScopedOwner::Family("mos6502".into())
                    && family.mnemonic.eq_ignore_ascii_case(&row.mnemonic)
                    && family.mode_key.eq_ignore_ascii_case(&row.mode_key)
            })
            .expect("every CPU rejection mode must already exist in the base table");
        assert_eq!(
            row.program, original.program,
            "{} {}",
            row.mnemonic, row.mode_key
        );
    }
    let bytes = encode_hierarchy_chunks_from_chunks(&chunks).unwrap();
    let canonical = RuntimeModelCore::from_chunks(chunks).unwrap();
    let decoded = RuntimeModelCore::from_package_bytes(&bytes).unwrap();
    let prepare = |core: &RuntimeModelCore| {
        let resolved = core.resolve_pipeline("m6502", None).unwrap();
        BinarySourcePackage::prepare(core, &resolved).unwrap()
    };
    let numeric = prepare(&canonical);
    assert_eq!(numeric, prepare(&decoded));
    let rows: Vec<_> = numeric.candidates.iter().filter(|row| {
        numeric.names[row.mnemonic as usize] == "lda"
            && matches!(row.recipe, CandidateRecipe::Unsupported { plan }
                if numeric.names[plan as usize].to_ascii_lowercase().starts_with("semv.reject.v1:otr004@"))
    }).collect();
    assert_eq!(rows.len(), 4);
    for suffix in ["d", "b", "k", "l"] {
        let row = rows
            .iter()
            .find(|row| {
                matches!(row.recipe, CandidateRecipe::Unsupported { plan }
                if numeric.names[plan as usize].eq_ignore_ascii_case(
                    &format!("semv.reject.v1:OTR004@named_register1={suffix}")))
            })
            .unwrap();
        assert_eq!(numeric.names[row.shape as usize], "direct_direct");
        let name = numeric
            .names
            .iter()
            .position(|name| name == suffix)
            .unwrap() as u16;
        assert_eq!(
            row.roles,
            vec![Projection::NamedRegister { operand: 1, name }]
        );
        // Compact transport retains the exact predicate and explicit unsupported
        // barrier. Legal X/Y forms use direct_register, outside this row's shape.
    }
}

fn indexed(value: &str, register: &str) -> [Expr; 2] {
    [
        Expr::Number(value.to_string(), Span::default()),
        Expr::Register(register.to_string(), Span::default()),
    ]
}

#[test]
fn mos6502_indexed_package_covers_widths_and_register_predicates() {
    let registry = mos6502_family_registry();
    let bytes = build_hierarchy_package_from_registry(&registry).unwrap();
    let model = HierarchyExecutionModel::from_package_bytes(&bytes).unwrap();
    let ctx = TestAssemblerContext::new();
    for (mnemonic, value, register, expected) in [
        ("LDA", "32", "X", vec![0xb5, 0x20]),
        ("LDA", "8192", "X", vec![0xbd, 0, 0x20]),
        ("LDX", "32", "Y", vec![0xb6, 0x20]),
        ("LDX", "8192", "Y", vec![0xbe, 0, 0x20]),
        ("LDA", "32", "Y", vec![0xb9, 0x20, 0]),
        ("STA", "8192", "X", vec![0x9d, 0, 0x20]),
        ("STA", "8192", "Y", vec![0x99, 0, 0x20]),
        ("LDY", "255", "X", vec![0xb4, 0xff]),
        ("LDY", "256", "X", vec![0xbc, 0, 1]),
    ] {
        assert_eq!(
            model
                .encode_instruction_from_exprs(
                    "m6502",
                    None,
                    mnemonic,
                    &indexed(value, register),
                    &ctx
                )
                .unwrap(),
            Some(expected),
            "{mnemonic} {value},{register}"
        );
    }
    for (mnemonic, value, register) in [
        ("LDA", "32", "A"),
        ("LDX", "32", "X"),
        ("LDY", "32", "Y"),
        ("STX", "256", "Y"),
        ("LDA", "65536", "X"),
    ] {
        assert!(
            !matches!(
                model.encode_instruction_from_exprs(
                    "m6502",
                    None,
                    mnemonic,
                    &indexed(value, register),
                    &ctx
                ),
                Ok(Some(_))
            ),
            "invalid {mnemonic} {value},{register} encoded"
        );
    }
}

#[test]
fn mos6502_indexed_package_predicate_changes_actual_selection() {
    let registry = mos6502_family_registry();
    let mut chunks = build_hierarchy_chunks_from_registry(&registry).unwrap();
    // Remove other candidate routes, then change package policy. The normal
    // public encoding entry must honor that change, not reconstruct X from the
    // mode's spelling or silently use the previous family-parser candidate.
    chunks.selectors.retain(|row| {
        !row.mnemonic.eq_ignore_ascii_case("LDA")
            || (row.shape_key == "direct_register"
                && row.mode_key.eq_ignore_ascii_case("ZeroPageX"))
    });
    let row = chunks
        .selectors
        .iter_mut()
        .find(|row| row.mnemonic.eq_ignore_ascii_case("LDA") && row.shape_key == "direct_register")
        .unwrap();
    row.operand_plan =
        "semv.inputs.v1:enc.u8@required_value_program:scalar.unsigned-byte:expr0,named_register1=Y"
            .to_string();
    let bytes = encode_hierarchy_chunks_from_chunks(&chunks).unwrap();
    let model = HierarchyExecutionModel::from_package_bytes(&bytes).unwrap();
    let ctx = TestAssemblerContext::new();
    assert_eq!(
        model
            .encode_instruction_from_exprs("m6502", None, "LDA", &indexed("32", "X"), &ctx)
            .unwrap(),
        None
    );
    assert_eq!(
        model
            .encode_instruction_from_exprs("m6502", None, "LDA", &indexed("32", "Y"), &ctx)
            .unwrap(),
        Some(vec![0xb5, 0x20])
    );
}

#[test]
fn mos6502_indexed_package_preserves_unresolved_and_unstable_address_widths() {
    let model = HierarchyExecutionModel::from_registry(&mos6502_family_registry()).unwrap();
    let resolved = model.resolve_pipeline("m6502", None).unwrap();
    let operands = [
        Expr::Identifier("target".to_string(), Span::default()),
        Expr::Register("X".to_string(), Span::default()),
    ];
    let mut ctx = TestAssemblerContext::new();
    ctx.pass = 1;
    assert_eq!(
        model
            .encode_instruction_from_exprs("m6502", None, "LDA", &operands, &ctx)
            .unwrap(),
        Some(vec![0xbd, 0, 0]),
        "unresolved indexed target reserves the wide package candidate"
    );
    ctx.pass = 2;
    ctx.values.insert("target".to_string(), 0x10);
    ctx.finalized.insert("target".to_string(), false);
    let candidates = model
        .select_candidates_from_exprs_mos6502(&resolved, "LDA", &operands, &ctx)
        .unwrap()
        .unwrap();
    assert!(candidates.iter().all(|row| row.mode_key == "absolutex"));
    ctx.finalized.insert("target".to_string(), true);
    assert_eq!(
        model
            .encode_instruction_from_exprs("m6502", None, "LDA", &operands, &ctx)
            .unwrap(),
        Some(vec![0xb5, 0x10])
    );
    ctx.values.insert("target".to_string(), 0x1003);
    assert_eq!(
        model
            .encode_instruction_from_exprs("m6502", None, "LDA", &operands, &ctx)
            .unwrap(),
        Some(vec![0xbd, 3, 0x10])
    );
    ctx.values.insert("target".to_string(), 65536);
    assert!(model
        .encode_instruction_from_exprs("m6502", None, "LDA", &operands, &ctx)
        .is_err());
}

fn scalar(value: &str) -> Expr {
    Expr::Number(value.to_string(), Span::default())
}
fn indirect(value: Expr) -> Expr {
    Expr::Indirect(Box::new(value), Span::default())
}
fn indirect_tuple(value: &str, register: &str) -> Expr {
    indirect(Expr::Tuple(
        vec![
            scalar(value),
            Expr::Register(register.to_string(), Span::default()),
        ],
        Span::default(),
    ))
}

#[test]
fn mos6502_structural_package_preserves_wrappers_and_accumulator_identity() {
    let model = HierarchyExecutionModel::from_registry(&mos6502_family_registry()).unwrap();
    let ctx = TestAssemblerContext::new();
    let register = |name: &str| Expr::Register(name.to_string(), Span::default());
    for (mnemonic, operands, expected) in [
        (
            "JMP",
            vec![indirect(scalar("4660"))],
            vec![0x6c, 0x34, 0x12],
        ),
        ("BNE", vec![scalar("5")], vec![0xd0, 3]),
        ("LDA", vec![indirect_tuple("32", "X")], vec![0xa1, 0x20]),
        ("STA", vec![indirect_tuple("255", "X")], vec![0x81, 0xff]),
        (
            "LDA",
            vec![indirect(scalar("32")), register("Y")],
            vec![0xb1, 0x20],
        ),
        (
            "ADC",
            vec![indirect(scalar("255")), register("Y")],
            vec![0x71, 0xff],
        ),
        ("ASL", vec![register("A")], vec![0x0a]),
        ("LSR", vec![register("A")], vec![0x4a]),
        ("ROL", vec![register("A")], vec![0x2a]),
        ("ROR", vec![register("A")], vec![0x6a]),
        ("EOR", vec![scalar("48350")], vec![0x4d, 0xde, 0xbc]),
    ] {
        assert_eq!(
            model
                .encode_instruction_from_exprs("m6502", None, mnemonic, &operands, &ctx)
                .unwrap(),
            Some(expected),
            "{mnemonic} {operands:?}"
        );
    }
    for (mnemonic, operands) in [
        ("LDA", vec![indirect(scalar("32"))]),
        ("BNE", vec![indirect(scalar("32"))]),
        ("LDA", vec![indirect_tuple("32", "X"), register("Y")]),
        ("JMP", vec![indirect_tuple("32", "X")]),
        ("LDA", vec![indirect_tuple("32", "Y")]),
        ("LDA", vec![indirect_tuple("32", "A")]),
        ("LDA", vec![indirect_tuple("256", "X")]),
        ("LDA", vec![indirect(scalar("256")), register("Y")]),
        ("LDA", vec![indirect(scalar("32")), register("X")]),
        ("ASL", vec![register("X")]),
        ("LDA", vec![indirect(indirect(scalar("32"))), register("Y")]),
        (
            "LDA",
            vec![indirect(Expr::Tuple(
                vec![scalar("32"), register("X"), register("Y")],
                Span::default(),
            ))],
        ),
    ] {
        assert!(
            !matches!(
                model.encode_instruction_from_exprs("m6502", None, mnemonic, &operands, &ctx),
                Ok(Some(_))
            ),
            "invalid {mnemonic} {operands:?} encoded"
        );
    }
}

#[test]
fn mos6502_structural_package_nested_name_policy_controls_selection() {
    let mut chunks = build_hierarchy_chunks_from_registry(&mos6502_family_registry()).unwrap();
    chunks.selectors.retain(|row| {
        !row.mnemonic.eq_ignore_ascii_case("LDA")
            || row.operand_plan.contains("indirect_tuple_named_register")
    });
    let row = chunks
        .selectors
        .iter_mut()
        .find(|row| {
            row.mnemonic.eq_ignore_ascii_case("LDA")
                && row.operand_plan.contains("indirect_tuple_named_register")
        })
        .unwrap();
    let previous = row.operand_plan.clone();
    row.operand_plan = row
        .operand_plan
        .replace(".item1=X", ".item1=Y")
        .replace(".item1=x", ".item1=Y");
    assert_ne!(row.operand_plan, previous, "package predicate must change");
    let bytes = encode_hierarchy_chunks_from_chunks(&chunks).unwrap();
    let model = HierarchyExecutionModel::from_package_bytes(&bytes).unwrap();
    let ctx = TestAssemblerContext::new();
    assert_eq!(
        model
            .encode_instruction_from_exprs("m6502", None, "LDA", &[indirect_tuple("32", "X")], &ctx)
            .unwrap(),
        None
    );
    assert_eq!(
        model
            .encode_instruction_from_exprs("m6502", None, "LDA", &[indirect_tuple("32", "Y")], &ctx)
            .unwrap(),
        Some(vec![0xa1, 0x20])
    );
}

#[test]
fn mos6502_structural_package_retains_specialized_variant_inputs() {
    let model = HierarchyExecutionModel::from_registry(&mos6502_family_registry()).unwrap();
    let ctx = TestAssemblerContext::new();
    for cpu in ["65c02", "65816"] {
        for (mnemonic, operands, expected) in [
            ("LDA", vec![scalar("8192")], vec![0xad, 0, 0x20]),
            ("LDA", indexed("32", "X").to_vec(), vec![0xb5, 0x20]),
            ("LDA", vec![indirect_tuple("32", "X")], vec![0xa1, 0x20]),
            (
                "LDA",
                vec![
                    indirect(scalar("32")),
                    Expr::Register("Y".to_string(), Span::default()),
                ],
                vec![0xb1, 0x20],
            ),
            (
                "ASL",
                vec![Expr::Register("A".to_string(), Span::default())],
                vec![0x0a],
            ),
        ] {
            assert_eq!(
                model
                    .encode_instruction_from_exprs(cpu, None, mnemonic, &operands, &ctx)
                    .unwrap(),
                Some(expected),
                "{cpu} {mnemonic} {operands:?}"
            );
        }
    }
}
