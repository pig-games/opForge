use super::*;

fn catalog() -> (RuntimeModelCore, Vec<ResolvedHierarchy>, Vec<String>) {
    let mut registry = registry::ModuleRegistry::new();
    families::register_intel8080_family_stack(&mut registry);
    families::register_motorola6800_family_stack(&mut registry);
    families::register_motorola68000_family_stack(&mut registry);
    families::register_mos6502_family_stack(&mut registry);
    let core = RuntimeModelCore::from_registry(&registry).unwrap();
    let mut pipelines = BTreeMap::new();
    for cpu in registry.cpu_ids() {
        let family = registry.cpu_family_id(cpu).unwrap();
        for dialect in registry.dialect_ids_for_family(family) {
            let resolved = core.resolve_pipeline(cpu.as_str(), Some(&dialect)).unwrap();
            pipelines.insert(
                (resolved.cpu_id.clone(), resolved.dialect_id.clone()),
                resolved,
            );
        }
    }
    let pipelines = pipelines.into_values().collect::<Vec<_>>();
    // Obtain the prefix from its existing owner; never sort or reconstruct its IDs.
    let mut seed = prepare_package_with_names(&core, &pipelines[0])
        .unwrap()
        .names;
    seed.truncate(core.interned_ids.len());
    let mut seen = seed.iter().cloned().collect::<BTreeSet<_>>();
    for pipeline in &pipelines {
        for name in prepare_package_with_names(&core, pipeline).unwrap().names {
            if seen.insert(name.clone()) {
                seed.push(name);
            }
        }
    }
    (core, pipelines, seed)
}

fn remap_projection(projection: &mut Projection, map: &impl Fn(u16) -> u16) {
    match projection {
        Projection::ExpressionPath { operations, .. } => {
            for operation in operations {
                match operation {
                    ExpressionPathOperation::Member(name)
                    | ExpressionPathOperation::QualifiedRegister {
                        qualifier: name, ..
                    } => *name = map(*name),
                    _ => {}
                }
            }
        }
        Projection::TupleNamedRegister { name, .. }
        | Projection::NamedRegister { name, .. }
        | Projection::TargetMember {
            qualifier: name, ..
        }
        | Projection::Member {
            qualifier: name, ..
        }
        | Projection::MemberShape {
            qualifier: name, ..
        }
        | Projection::TupleQualifiedRegister {
            qualifier: name, ..
        } => *name = map(*name),
        Projection::ValueProgram { program, source }
        | Projection::RequiredValueProgram { program, source } => {
            *program = map(*program);
            remap_projection(source, map);
        }
        _ => {}
    }
}

// Test-only normalization exhaustively rebases name references. Numeric facets
// remain untouched, making full equality sensitive to any facet/order change.
fn normalize(package: &mut BinarySourcePackage, baseline: &BinarySourcePackage) {
    let ids = baseline
        .names
        .iter()
        .enumerate()
        .map(|(id, name)| (name.as_str(), id as u16))
        .collect::<BTreeMap<_, _>>();
    let names = package.names.clone();
    let map = |id: u16| ids[names[usize::from(id)].as_str()];
    for directive in &mut package.state.directives {
        directive.head = map(directive.head);
        for argument in &mut directive.arguments {
            if argument.kind == 0 {
                argument.matched = u32::from(map(argument.matched.try_into().unwrap()));
            }
        }
    }
    for row in &mut package.aliases {
        row.spelling = map(row.spelling);
        row.mnemonic = map(row.mnemonic);
    }
    for row in &mut package.registers {
        row.name = map(row.name);
    }
    for row in &mut package.table_programs {
        row.mnemonic = map(row.mnemonic);
        row.mode = map(row.mode);
    }
    for row in package
        .semantic_programs
        .iter_mut()
        .chain(&mut package.value_programs)
    {
        row.name = map(row.name);
    }
    for row in &mut package.member_bindings {
        row.mnemonic = map(row.mnemonic);
        row.field = map(row.field);
    }
    for candidate in &mut package.candidates {
        candidate.mnemonic = map(candidate.mnemonic);
        candidate.shape = map(candidate.shape);
        candidate.mode = map(candidate.mode);
        for (_, name) in &mut candidate.known_name_excluded {
            *name = map(*name);
        }
        for role in &mut candidate.roles {
            remap_projection(role, &map);
        }
        match &mut candidate.recipe {
            CandidateRecipe::SemanticInputs { program, inputs }
            | CandidateRecipe::SemanticBranch { program, inputs } => {
                *program = map(*program);
                for input in inputs {
                    remap_projection(input, &map);
                }
            }
            CandidateRecipe::SemanticSequence { stages } => {
                for stage in stages {
                    stage.program = stage.program.map(map);
                    for input in &mut stage.inputs {
                        remap_projection(input, &map);
                    }
                }
            }
            CandidateRecipe::Unsupported { plan } => *plan = map(*plan),
            _ => {}
        }
    }
    package.names = baseline.names.clone();
    package
        .aliases
        .sort_by_key(|row| (row.spelling, row.spelling_qualifier));
    package
        .registers
        .sort_by_key(|row| (row.owner_rank, row.name));
    package
        .table_programs
        .sort_by_key(|row| (row.owner_rank, row.mnemonic, row.qualifier, row.mode));
    package
        .semantic_programs
        .sort_by_key(|row| (row.owner_rank, row.name));
    package
        .value_programs
        .sort_by_key(|row| (row.owner_rank, row.name));
    package.member_bindings.sort();
    package.candidates.sort_by_key(|row| {
        (
            row.owner_rank,
            row.mnemonic,
            row.qualifier,
            row.shape,
            row.priority,
            row.width_rank,
            row.mode,
        )
    });
}

#[test]
fn seeded_catalog_preserves_pipeline_semantics_and_wire_size() {
    let (core, pipelines, seed) = catalog();
    assert!(seed.len() < u16::MAX as usize);
    eprintln!(
        "seed: {} names, {} spelling bytes, {} pipelines; core prefix {}",
        seed.len(),
        seed.iter().map(String::len).sum::<usize>(),
        pipelines.len(),
        core.interned_ids.len()
    );
    for resolved in &pipelines {
        let baseline = BinarySourcePackage::prepare(&core, resolved).unwrap();
        let mut seeded =
            BinarySourcePackage::prepare_with_name_seed(&core, resolved, &seed).unwrap();
        assert_eq!(seeded.names, seed);
        // The candidate order within each selectable mnemonic/qualifier/shape
        // must be unchanged before normalization reorders unrelated groups.
        let groups = |package: &BinarySourcePackage| {
            let mut groups = BTreeMap::<_, Vec<_>>::new();
            for row in &package.candidates {
                groups
                    .entry((
                        package.names[row.mnemonic as usize].clone(),
                        row.qualifier,
                        package.names[row.shape as usize].clone(),
                    ))
                    .or_default()
                    .push((
                        row.owner_rank,
                        row.priority,
                        row.width_rank,
                        package.names[row.mode as usize].clone(),
                    ));
            }
            groups
        };
        assert_eq!(groups(&seeded), groups(&baseline));
        normalize(&mut seeded, &baseline);
        assert_eq!(
            seeded, baseline,
            "{} {}",
            resolved.cpu_id, resolved.dialect_id
        );
        let old_wire = prepare_package(&core, resolved).unwrap();
        let wire = prepare_package_with_name_seed(&core, resolved, &seed).unwrap();
        assert_eq!(wire.names, seed);
        assert_eq!(
            wire.bytes.len(),
            old_wire.len(),
            "host-only names must add no spelling payload"
        );
        assert_eq!(&wire.bytes[..4], b"BS38");
    }
    for (cpu, mnemonic, opcode) in [
        ("8085", "nop", 0x00),
        ("8085", "rim", 0x20),
        ("8085", "sim", 0x30),
        ("m6502", "nop", 0xea),
        ("m68000", "nop", 0x4e),
    ] {
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let package = BinarySourcePackage::prepare_with_name_seed(&core, &resolved, &seed).unwrap();
        let row = package
            .table_programs
            .iter()
            .find(|row| package.names[row.mnemonic as usize] == mnemonic)
            .unwrap();
        let bytes = vm::bytecode::execute_program(&row.bytes, &[]).unwrap();
        assert_eq!(bytes[0], opcode);
        if cpu == "m68000" {
            assert_eq!(bytes, [0x4e, 0x71]);
        }
    }
}

#[test]
fn seeded_catalog_rejects_invalid_prefix_capacity_and_missing_identities() {
    let (core, _, seed) = catalog();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let mut invalid = seed.clone();
    invalid.swap(0, 1);
    assert!(
        BinarySourcePackage::prepare_with_name_seed(&core, &resolved, &invalid)
            .unwrap_err()
            .contains("core prefix")
    );
    let mut invalid = seed.clone();
    invalid.push(seed[0].clone());
    assert!(
        BinarySourcePackage::prepare_with_name_seed(&core, &resolved, &invalid)
            .unwrap_err()
            .contains("unique")
    );
    let mut invalid = seed.clone();
    invalid.push("MixedCaseTail".into());
    assert!(
        BinarySourcePackage::prepare_with_name_seed(&core, &resolved, &invalid)
            .unwrap_err()
            .contains("canonical")
    );
    let mut capacity = seed.clone();
    while capacity.len() < u16::MAX as usize {
        capacity.push(format!("capacity.identity.{}", capacity.len()));
    }
    assert_eq!(
        BinarySourcePackage::prepare_with_name_seed(&core, &resolved, &capacity)
            .unwrap()
            .names
            .len(),
        u16::MAX as usize
    );
    let mut invalid = capacity;
    invalid.push("overflow".into());
    assert!(
        BinarySourcePackage::prepare_with_name_seed(&core, &resolved, &invalid)
            .unwrap_err()
            .contains("capacity")
    );
    let prefix = seed[..core.interned_ids.len()].to_vec();
    assert!(
        BinarySourcePackage::prepare_with_name_seed(&core, &resolved, &prefix)
            .unwrap_err()
            .contains("missing")
    );
    let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let acquisition = prepare_package_with_names(&core, &resolved).unwrap();
    let acquisition_only = acquisition
        .names
        .iter()
        .find(|name| !numeric.names.contains(name))
        .expect("acquisition must own appended identities");
    let missing_acquisition = seed
        .iter()
        .filter(|name| *name != acquisition_only)
        .cloned()
        .collect::<Vec<_>>();
    assert!(missing_acquisition.starts_with(&prefix));
    assert!(
        prepare_package_with_name_seed(&core, &resolved, &missing_acquisition)
            .err()
            .unwrap()
            .contains("acquisition identity")
    );
}
