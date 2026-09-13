use super::*;
use crate::prepared_encoding::{EncodingPreparation, ProgramRef};
use crate::runtime_model_core::RuntimeModelCore;

#[test]
fn prepared_encoding_reuses_binding_and_accepts_new_values_without_model() {
    let model = RuntimeModelCore::from_registry(&mos6502_family_registry()).unwrap();
    let resolved = model.bridge.resolve_pipeline("m6502", None).unwrap();
    let mut preparation = EncodingPreparation::new(&model);
    let reference = preparation
        .bind(&resolved, "LDA", "immediate")
        .unwrap()
        .unwrap();
    assert_eq!(
        Some(reference),
        preparation.bind(&resolved, "lda", "IMMEDIATE").unwrap()
    );
    assert_eq!(
        None,
        preparation.bind(&resolved, "missing", "immediate").unwrap()
    );
    assert_eq!(None, preparation.bind(&resolved, "lda", "missing").unwrap());
    let prepared = preparation.finish();
    assert_eq!(prepared.program_count(), 1);
    assert_eq!(std::mem::size_of::<ProgramRef>(), 4);
    let expected: Vec<_> = [0, 1, 42, 255]
        .map(|value| {
            model
                .encode_candidates(
                    &resolved,
                    "LDA",
                    &[VmEncodeCandidate {
                        mode_key: "immediate".into(),
                        operand_bytes: vec![vec![value]],
                    }],
                )
                .unwrap()
                .unwrap()
        })
        .into();
    drop(resolved);
    drop(model);
    for (value, expected) in [0, 1, 42, 255].into_iter().zip(expected) {
        assert_eq!(prepared.encode(reference, &[&[value]]).unwrap(), expected);
    }
}

#[test]
fn prepared_encoding_preserves_owner_precedence_and_vm_failure() {
    let mut model = RuntimeModelCore::from_registry(&mos6502_family_registry()).unwrap();
    let resolved = model.bridge.resolve_pipeline("m6502", None).unwrap();
    let mnemonic = model.interned_id("lda").unwrap();
    let mode = model.interned_id("immediate").unwrap();
    // Deliberately give all three owners conflicting programs. Preparation must
    // choose exactly the same first owner as canonical execution.
    for (owner, id) in model.scoped_owner_lookup_order(&resolved) {
        if let Some(id) = id {
            model
                .vm_programs
                .insert((owner, id, mnemonic, mode), vec![OP_EMIT_U8, owner, OP_END]);
        }
    }
    let candidate = VmEncodeCandidate {
        mode_key: "immediate".into(),
        operand_bytes: vec![],
    };
    let expected = model
        .encode_candidates(&resolved, "lda", std::slice::from_ref(&candidate))
        .unwrap()
        .unwrap();
    let mut preparation = EncodingPreparation::new(&model);
    let reference = preparation
        .bind(&resolved, "lda", "immediate")
        .unwrap()
        .unwrap();
    assert_eq!(
        preparation.finish().encode(reference, &[]).unwrap(),
        expected
    );

    for (owner, id) in model.scoped_owner_lookup_order(&resolved) {
        if let Some(id) = id {
            model.vm_programs.insert(
                (owner, id, mnemonic, mode),
                vec![OP_EMIT_OPERAND, 0, OP_END],
            );
        }
    }
    let expected = model
        .encode_candidates(&resolved, "lda", &[candidate])
        .unwrap_err();
    let mut preparation = EncodingPreparation::new(&model);
    let reference = preparation
        .bind(&resolved, "lda", "immediate")
        .unwrap()
        .unwrap();
    assert_eq!(
        preparation.finish().encode(reference, &[]).unwrap_err(),
        expected
    );
}

#[test]
fn prepared_encoding_enforces_program_and_operand_limits() {
    let mut model = RuntimeModelCore::from_registry(&mos6502_family_registry()).unwrap();
    let resolved = model.bridge.resolve_pipeline("m6502", None).unwrap();
    model.budget_limits.max_vm_program_bytes = 0;
    assert!(EncodingPreparation::new(&model)
        .bind(&resolved, "lda", "immediate")
        .is_err());
    model.budget_limits.max_vm_program_bytes = 64;
    model.budget_limits.max_operand_count_per_candidate = 1;
    model.budget_limits.max_operand_bytes_per_operand = 1;
    let mut preparation = EncodingPreparation::new(&model);
    let reference = preparation
        .bind(&resolved, "lda", "immediate")
        .unwrap()
        .unwrap();
    let prepared = preparation.finish();
    assert!(prepared.encode(reference, &[&[1], &[2]]).is_err());
    assert!(prepared.encode(reference, &[&[1, 2]]).is_err());
    assert_eq!(
        prepared.encode(reference, &[&[42]]).unwrap(),
        vec![0xa9, 42]
    );
}

#[cfg(not(feature = "prepared-telemetry"))]
#[test]
fn prepared_encoding_disabled_telemetry_does_not_evaluate_arguments() {
    crate::prepared_event!("must-not-run", panic!("disabled telemetry evaluated"));
}

#[cfg(feature = "prepared-telemetry")]
#[test]
fn prepared_encoding_moves_name_lookups_out_of_replay() {
    let model = RuntimeModelCore::from_registry(&mos6502_family_registry()).unwrap();
    let resolved = model.bridge.resolve_pipeline("m6502", None).unwrap();
    let _session = types::vm_work::install();
    let phase = types::vm_work::phase("prepare");
    let mut preparation = EncodingPreparation::new(&model);
    let reference = preparation
        .bind(&resolved, "lda", "immediate")
        .unwrap()
        .unwrap();
    let prepared = preparation.finish();
    drop(phase);
    for value in 0..8 {
        let phase = types::vm_work::phase("reference");
        let expected = model
            .encode_candidates(
                &resolved,
                "lda",
                &[VmEncodeCandidate {
                    mode_key: "immediate".into(),
                    operand_bytes: vec![vec![value]],
                }],
            )
            .unwrap()
            .unwrap();
        drop(phase);
        let _phase = types::vm_work::phase("replay");
        assert_eq!(prepared.encode(reference, &[&[value]]).unwrap(), expected);
    }
    let snapshot = types::vm_work::snapshot().unwrap();
    assert_eq!(snapshot["overflow"], false);
    let count = |phase: &str, label: &str| -> u64 {
        snapshot["events"]
            .as_array()
            .unwrap()
            .iter()
            .find(|row| row["phase"] == phase && row["label"] == label)
            .map_or(0, |row| row["count"].as_u64().unwrap())
    };
    assert_eq!(count("prepare", "package.name_lookup"), 5);
    assert_eq!(count("reference", "package.name_lookup"), 40);
    assert_eq!(count("replay", "package.name_lookup"), 0);
    assert_eq!(count("replay", "prepared.encoding.execute"), 8);
}
