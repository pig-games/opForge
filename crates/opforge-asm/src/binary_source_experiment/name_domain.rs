//! Host-only catalog identity experiment. This does not merge lexical grammars,
//! remap candidate/qualifier facets, or participate in BS38 preparation.
//! Complete identities come from the acquisition owner without changing package
//! bytes. Internal plan names are measured too; runtime lexical storage need not
//! retain that entire spelling union. No dictionary witnesses are inferred.

use std::collections::{BTreeMap, BTreeSet};

use registry::ModuleRegistry;
use types::hierarchy::ResolvedHierarchy;
use vm::runtime_model_core::RuntimeModelCore;

use super::prepare_package_with_names;

// The existing compact format reserves 0xffff for MISSING, including count fields.
const NAME_CAPACITY: usize = u16::MAX as usize;

#[derive(Debug, PartialEq, Eq)]
struct NameDomain {
    names: Vec<String>,
    ids: BTreeMap<String, u16>,
}

#[derive(Debug, PartialEq, Eq)]
struct LocalMap {
    local_to_domain: Vec<u16>,
}

struct PipelineNames {
    resolved: ResolvedHierarchy,
    local: Vec<String>,
}

impl NameDomain {
    fn collect<'a>(groups: impl IntoIterator<Item = &'a [String]>) -> Result<Self, String> {
        let names = groups
            .into_iter()
            .flatten()
            .map(|name| name.to_ascii_lowercase())
            .collect::<BTreeSet<_>>();
        if names.len() > NAME_CAPACITY {
            return Err("catalog name domain exceeds compact u16 capacity".into());
        }
        let names = names.into_iter().collect::<Vec<_>>();
        let ids = names
            .iter()
            .enumerate()
            .map(|(index, name)| (name.clone(), index as u16))
            .collect();
        Ok(Self { names, ids })
    }

    fn map_local(&self, names: &[String]) -> Result<LocalMap, String> {
        if names.len() > NAME_CAPACITY {
            return Err("local name domain exceeds compact u16 capacity".into());
        }
        let local_to_domain = names
            .iter()
            .map(|name| {
                self.ids
                    .get(&name.to_ascii_lowercase())
                    .copied()
                    .ok_or_else(|| format!("local name {name:?} absent from catalog domain"))
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(LocalMap { local_to_domain })
    }

    // Diagnostic hash only. The canonical length-framed names themselves remain
    // the authoritative equality check, so hash collisions cannot prove identity.
    fn fingerprint(&self) -> String {
        let mut framed = Vec::new();
        framed.extend_from_slice(b"opforge-name-domain\0");
        framed.extend_from_slice(&(self.names.len() as u32).to_be_bytes());
        for name in &self.names {
            framed.extend_from_slice(&(name.len() as u64).to_be_bytes());
            framed.extend_from_slice(name.as_bytes());
        }
        crate::fs_uae_smoke::opforge_self_host_package_digest(&framed)
    }

    fn string_bytes(&self) -> usize {
        self.names.iter().map(String::len).sum()
    }

    // Proposed immutable spelling storage: UTF-8 bytes plus u32 offset/length
    // rows. A local map separately costs two bytes per local identity.
    fn table_bytes(&self) -> usize {
        self.names.len() * 8
    }
}

fn catalog_names(
    core: &RuntimeModelCore,
    registry: &ModuleRegistry,
) -> Result<Vec<PipelineNames>, String> {
    let metadata = core.bridge.package();
    let model_cpu_names = metadata
        .cpu_descriptors()
        .into_iter()
        .map(|cpu| cpu.id.to_ascii_lowercase())
        .collect::<BTreeSet<_>>();
    let registry_cpu_names = registry
        .cpu_name_list()
        .into_iter()
        .map(|name| name.to_ascii_lowercase())
        .collect::<BTreeSet<_>>();
    if model_cpu_names != registry_cpu_names
        || metadata.family_ids()
            != registry
                .family_ids()
                .into_iter()
                .map(|family| family.as_str().to_string())
                .collect::<Vec<_>>()
    {
        return Err("runtime model and registry catalog identities do not match".into());
    }
    let mut pipelines = BTreeMap::new();
    for cpu in registry.cpu_ids() {
        let family = registry
            .cpu_family_id(cpu)
            .ok_or("catalog CPU has no family")?;
        for dialect in registry.dialect_ids_for_family(family) {
            // Runtime metadata validates dialect compatibility; registration's
            // order and the user's selected CPU/source never select this pool.
            let resolved = core
                .resolve_pipeline(cpu.as_str(), Some(&dialect))
                .map_err(|e| e.to_string())?;
            let names = prepare_package_with_names(core, &resolved)?.names;
            pipelines.insert(
                (resolved.cpu_id.clone(), resolved.dialect_id.clone()),
                PipelineNames {
                    resolved,
                    local: names,
                },
            );
        }
    }
    Ok(pipelines.into_values().collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn registry(reverse: bool) -> ModuleRegistry {
        let mut result = ModuleRegistry::new();
        let mut register: Vec<fn(&mut ModuleRegistry)> = vec![
            families::register_intel8080_family_stack,
            families::register_motorola6800_family_stack,
            families::register_motorola68000_family_stack,
            families::register_mos6502_family_stack,
        ];
        if reverse {
            register.reverse();
        }
        for register in register {
            register(&mut result);
        }
        result
    }

    #[test]
    fn sorted_unique_ids_cross_local_domains_and_framed_fingerprints() {
        let first = vec!["NOP".into(), "x".into(), "a".into()];
        let second = vec!["a".into(), "nop".into(), "z".into()];
        let domain = NameDomain::collect([first.as_slice(), second.as_slice()]).unwrap();
        let reversed = NameDomain::collect([second.as_slice(), first.as_slice()]).unwrap();
        assert_eq!(domain, reversed);
        assert_eq!(domain.names, ["a", "nop", "x", "z"]);
        let left = domain.map_local(&first).unwrap();
        let right = domain.map_local(&second).unwrap();
        assert_eq!(left.local_to_domain[0], right.local_to_domain[1]);
        assert_eq!(left.local_to_domain[2], right.local_to_domain[0]);
        assert_ne!(left.local_to_domain[1], right.local_to_domain[2]);
        assert_eq!(domain.fingerprint(), reversed.fingerprint());
        let one = vec!["ab".into(), "c".into()];
        let two = vec!["a".into(), "bc".into()];
        assert_ne!(
            NameDomain::collect([one.as_slice()]).unwrap().fingerprint(),
            NameDomain::collect([two.as_slice()]).unwrap().fingerprint()
        );
        assert!(domain.map_local(&["foreign".into()]).is_err());
    }

    #[test]
    fn compact_capacity_preserves_missing_sentinel() {
        let mut names = (0..NAME_CAPACITY)
            .map(|index| format!("n{index:05}"))
            .collect::<Vec<_>>();
        let domain = NameDomain::collect([names.as_slice()]).unwrap();
        assert_eq!(
            domain.map_local(&names).unwrap().local_to_domain.len(),
            NAME_CAPACITY
        );
        assert!(!domain.ids.values().any(|id| *id == u16::MAX));
        names.push("overflow".into());
        assert!(NameDomain::collect([names.as_slice()]).is_err());
        assert!(domain.map_local(&names).is_err());
    }

    #[test]
    fn registered_pipeline_pool_matches_model_and_ignores_registration_order() {
        let registry = registry(false);
        let core = RuntimeModelCore::from_registry(&registry).unwrap();
        let pipelines = catalog_names(&core, &registry).unwrap();
        let prefix = core
            .interned_ids
            .keys()
            .cloned()
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();
        for pipeline in &pipelines {
            assert!(
                pipeline.local.starts_with(&prefix),
                "catalog-wide core prefix changed"
            );
        }
        eprintln!(
            "model core prefix: names={} string_bytes={} table_bytes={}",
            prefix.len(),
            prefix.iter().map(String::len).sum::<usize>(),
            prefix.len() * 8
        );
        let domain =
            NameDomain::collect(pipelines.iter().map(|pipeline| pipeline.local.as_slice()))
                .unwrap();
        let reversed_registry = self::registry(true);
        let reversed_core = RuntimeModelCore::from_registry(&reversed_registry).unwrap();
        let reversed = catalog_names(&reversed_core, &reversed_registry).unwrap();
        assert_eq!(
            domain,
            NameDomain::collect(reversed.iter().map(|pipeline| pipeline.local.as_slice())).unwrap()
        );
        for pipeline in &pipelines {
            let map = domain.map_local(&pipeline.local).unwrap();
            for (local, id) in pipeline.local.iter().zip(map.local_to_domain) {
                assert_eq!(local.to_ascii_lowercase(), domain.names[usize::from(id)]);
            }
        }
        let mut other = ModuleRegistry::new();
        families::register_mos6502_family_stack(&mut other);
        assert!(catalog_names(&core, &other).is_err());
        eprintln!("catalog acquisition-package pool: pipelines={} names={} string_bytes={} table_bytes={} fingerprint={}",
            pipelines.len(), domain.names.len(), domain.string_bytes(), domain.table_bytes(), domain.fingerprint());
        for cpu in ["6502", "65c02", "68020"] {
            let pipeline = pipelines
                .iter()
                .find(|pipeline| {
                    pipeline.resolved.cpu_id == core.resolve_pipeline(cpu, None).unwrap().cpu_id
                        && core.resolve_pipeline(cpu, None).unwrap().dialect_id
                            == pipeline.resolved.dialect_id
                })
                .unwrap();
            eprintln!(
                "{cpu}: local_names={} string_bytes={} table_bytes={} map_bytes={}",
                pipeline.local.len(),
                pipeline.local.iter().map(String::len).sum::<usize>(),
                pipeline.local.len() * 8,
                pipeline.local.len() * 2
            );
        }
    }
}
