use super::*;

fn registry_and_core() -> (registry::ModuleRegistry, RuntimeModelCore) {
    let mut registry = registry::ModuleRegistry::new();
    families::register_intel8080_family_stack(&mut registry);
    families::register_mos6502_family_stack(&mut registry);
    families::register_motorola6800_family_stack(&mut registry);
    families::register_motorola68000_family_stack(&mut registry);
    let core = RuntimeModelCore::from_registry(&registry).unwrap();
    (registry, core)
}

fn pipelines(
    registry: &registry::ModuleRegistry,
    core: &RuntimeModelCore,
) -> Vec<ResolvedHierarchy> {
    let families = [
        families::intel8080::module::FAMILY_ID,
        families::mos6502::module::FAMILY_ID,
        families::m6800::module::FAMILY_ID,
        families::m68k::module::FAMILY_ID,
    ];
    let mut pipelines = Vec::new();
    for (cpu, family, _) in core.supported_cpus() {
        let family = families.iter().find(|id| id.as_str() == family).unwrap();
        for dialect in registry.dialect_ids_for_family(*family) {
            pipelines.push(core.resolve_pipeline(&cpu, Some(&dialect)).unwrap());
        }
    }
    pipelines
}

#[test]
fn acquisition_names_preserve_package_ids_and_wrapper_bytes_across_pipelines() {
    let (registry, core) = registry_and_core();
    let pipelines = pipelines(&registry, &core);
    for resolved in &pipelines {
        let bytes = prepare_package(&core, resolved).unwrap();
        assert_eq!(&bytes[..4], b"BS38");
        let prepared = prepare_package_with_names(&core, resolved).unwrap();
        assert_eq!(prepared.bytes, bytes);
        let package = BinarySourcePackage::prepare(&core, resolved).unwrap();
        assert_eq!(&prepared.names[..package.names.len()], &package.names);
        let count = usize::from(u16::from_be_bytes(
            prepared.bytes[62..64].try_into().unwrap(),
        ));
        assert_eq!(count, prepared.names.len());
        assert_eq!(prepared.names.iter().collect::<BTreeSet<_>>().len(), count);
        for (offset, spelling) in [
            (48, "cpu"),
            (50, "org"),
            (52, "byte"),
            (54, "word"),
            (56, "long"),
            (58, "end"),
            (208, "len"),
        ] {
            let id = usize::from(u16::from_be_bytes(
                prepared.bytes[offset..offset + 2].try_into().unwrap(),
            ));
            assert_eq!(prepared.names[id], spelling);
        }
        let cpu = usize::from(u16::from_be_bytes(
            prepared.bytes[60..62].try_into().unwrap(),
        ));
        assert_eq!(prepared.names[cpu], resolved.cpu_id);
        // Internal mode identities are present even when dictionary spellings
        // do not provide witnesses for them.
        for table in &package.table_programs {
            assert_eq!(
                prepared.names[usize::from(table.mode)],
                package.names[usize::from(table.mode)]
            );
        }
    }
    assert!(!pipelines.is_empty());
}
