//! Resolved package state: profile names and state keys never enter the wire.
use super::NameTable;
use crate::runtime_model_core::RuntimeModelCore;
use types::hierarchy::ResolvedHierarchy;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct NumericStatePlan {
    pub defaults: Vec<u32>,
    pub directives: Vec<NumericStateDirective>,
    pub guards: Vec<NumericStateGuard>,
    keys: Vec<String>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NumericStateDirective {
    pub head: u16,
    pub key: u16,
    pub arguments: Vec<NumericStateArgument>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NumericStateArgument {
    /// Contextual package name identity, including decimal spellings.
    pub kind: u16,
    pub allowed: bool,
    pub matched: u32,
    pub value: u32,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NumericStateGuard {
    pub clauses: Vec<NumericStateClause>,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NumericStateClause {
    /// True preserves a package diagnostic mismatch; false permits the next candidate.
    pub reject: bool,
    pub key: u16,
    pub values: Vec<u32>,
}

pub(super) fn prepare(
    core: &RuntimeModelCore,
    resolved: &ResolvedHierarchy,
    names: &mut NameTable,
) -> Result<NumericStatePlan, String> {
    for (tag, owner) in core.scoped_owner_lookup_order(resolved) {
        let Some(owner) = owner else { continue };
        let mut found = core
            .state_programs
            .iter()
            .filter_map(|(&(t, o, _), p)| ((t, o) == (tag, owner)).then_some(p));
        let Some(program) = found.next() else {
            continue;
        };
        if found.next().is_some() {
            return Err("binary-source state program owner is ambiguous".into());
        }
        return lower(program, &resolved.cpu_id, names);
    }
    Ok(NumericStatePlan::default())
}

fn lower(
    program: &package::DecodedStateProgram,
    profile: &str,
    names: &mut NameTable,
) -> Result<NumericStatePlan, String> {
    let profile = program
        .profiles
        .iter()
        .position(|p| p.eq_ignore_ascii_case(profile))
        .ok_or("binary-source state profile unknown")?;
    if program.keys.len() > 255 {
        return Err("binary-source state keys exceed 255".into());
    }
    let mut plan = NumericStatePlan::default();
    for key in &program.keys {
        plan.keys.push(key.id.clone());
        plan.defaults.push(
            key.overrides
                .iter()
                .find_map(|(p, v)| (usize::from(*p) == profile).then_some(*v))
                .unwrap_or(key.default),
        );
    }
    for rule in &program.directives {
        let key = u16::from(rule.key_index);
        if usize::from(key) >= plan.keys.len() {
            return Err("binary-source state directive key invalid".into());
        }
        let mut arguments = Vec::new();
        for arg in &rule.arguments {
            if arg.id.is_empty()
                || !arg
                    .id
                    .bytes()
                    .all(|b| b.is_ascii_alphanumeric() || b == b'_')
            {
                return Err("binary-source state argument spelling unsupported".into());
            }
            let (kind, matched) = (0, u32::from(names.id(&arg.id)));
            arguments.push(NumericStateArgument {
                kind,
                matched,
                value: arg.value,
                allowed: arg
                    .profile_mask
                    .get(profile / 8)
                    .is_some_and(|b| b & (1 << (profile % 8)) != 0),
            });
        }
        plan.directives.push(NumericStateDirective {
            head: names.id(&rule.id),
            key,
            arguments,
        });
    }
    Ok(plan)
}

pub(super) fn unwrap_guard<'a>(
    mut plan: &'a str,
    state: &mut NumericStatePlan,
) -> Result<(u16, &'a str), String> {
    let mut clauses = Vec::new();
    while let Some(body) = plan.strip_prefix(package::MODE_SELECTOR_PLAN_STATE_REQUIRE_PREFIX) {
        let (condition, nested) = body
            .split_once(';')
            .ok_or("binary-source state guard missing nested plan")?;
        if nested.is_empty() {
            return Err("binary-source state guard nested plan empty".into());
        }
        let reject = condition.contains('?');
        let requirement = if let Some((requirement, diagnostic)) = condition.split_once('?') {
            if diagnostic.is_empty() {
                return Err("binary-source state mismatch diagnostic empty".into());
            }
            requirement
        } else {
            condition
        };
        let (key, values) = requirement
            .split_once('=')
            .ok_or("binary-source state guard malformed")?;
        let key = state
            .keys
            .iter()
            .position(|k| k == key)
            .ok_or("binary-source state guard key unknown")?;
        let values = values
            .split('+')
            .map(|v| {
                v.parse::<u32>()
                    .map_err(|_| "binary-source state guard value invalid".to_string())
            })
            .collect::<Result<Vec<_>, _>>()?;
        clauses.push(NumericStateClause {
            reject,
            key: u16::try_from(key).map_err(|_| "binary-source state keys exhausted")?,
            values,
        });
        plan = nested;
    }
    if clauses.is_empty() {
        return Ok((0, plan));
    }
    let guard = NumericStateGuard { clauses };
    let index = if let Some(index) = state.guards.iter().position(|g| g == &guard) {
        index
    } else {
        state.guards.push(guard);
        state.guards.len() - 1
    };
    Ok((
        u16::try_from(index + 1).map_err(|_| "binary-source state guards exhausted")?,
        plan,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use package::{
        compile_state_program, decode_state_program, StateArgumentSpec, StateDirectiveSpec,
        StateKeySpec, StateProgramSpec,
    };
    fn program() -> package::DecodedStateProgram {
        decode_state_program(
            package::STATE_VM_OPCODE_VERSION_V1,
            &compile_state_program(&StateProgramSpec {
                profiles: (0..6).map(|i| format!("profile{i}")).collect(),
                keys: vec![StateKeySpec {
                    id: "feature".into(),
                    default: 0,
                    overrides: vec![("profile5".into(), 4)],
                }],
                directives: vec![StateDirectiveSpec {
                    id: "feature".into(),
                    key: "feature".into(),
                    arguments: vec![
                        StateArgumentSpec {
                            id: "off".into(),
                            value: 0,
                            allowed_profiles: (0..6).map(|i| format!("profile{i}")).collect(),
                        },
                        StateArgumentSpec {
                            id: "123".into(),
                            value: 1,
                            allowed_profiles: vec!["profile2".into(), "profile3".into()],
                        },
                    ],
                }],
                capabilities: vec![],
            })
            .unwrap(),
        )
        .unwrap()
    }
    #[test]
    fn resolved_state_matches_vm_defaults_and_transitions_for_every_profile() {
        let program = program();
        for profile in &program.profiles {
            let mut names = NameTable {
                names: Vec::new(),
                ids: std::collections::BTreeMap::new(),
                reverse: std::collections::BTreeMap::new(),
                overflow: false,
            };
            let plan = lower(&program, profile, &mut names).unwrap();
            let mut reference = crate::state_vm::initial_state(&program, profile).unwrap();
            assert_eq!(plan.defaults, vec![reference["feature"]]);
            for (argument, rule) in program.directives[0]
                .arguments
                .iter()
                .zip(&plan.directives[0].arguments)
            {
                assert_eq!(rule.kind, 0);
                assert_eq!(names.names[rule.matched as usize], argument.id);
                let result = crate::state_vm::apply_directive(
                    &program,
                    profile,
                    "feature",
                    std::slice::from_ref(&argument.id),
                    &mut reference,
                );
                assert_eq!(result.is_ok(), rule.allowed);
                if rule.allowed {
                    assert_eq!(reference["feature"], rule.value);
                }
            }
        }
    }
    #[test]
    fn package_state_matches_reference_for_all_declared_profiles() {
        let mut registry = registry::ModuleRegistry::new();
        families::register_motorola68000_family_stack(&mut registry);
        let core = RuntimeModelCore::from_registry(&registry).unwrap();
        for program in core.state_programs.values() {
            for profile in &program.profiles {
                let resolved = core.resolve_pipeline(profile, None).unwrap();
                let package = super::super::BinarySourcePackage::prepare(&core, &resolved).unwrap();
                let defaults = crate::state_vm::initial_state(program, profile).unwrap();
                assert_eq!(
                    package.state.defaults,
                    program
                        .keys
                        .iter()
                        .map(|k| defaults[&k.id])
                        .collect::<Vec<_>>()
                );
                for (directive, numeric) in program.directives.iter().zip(&package.state.directives)
                {
                    for (arg, lowered) in directive.arguments.iter().zip(&numeric.arguments) {
                        let mut reference = defaults.clone();
                        let outcome = crate::state_vm::apply_directive(
                            program,
                            profile,
                            &directive.id,
                            std::slice::from_ref(&arg.id),
                            &mut reference,
                        );
                        assert_eq!(outcome.is_ok(), lowered.allowed);
                        assert_eq!(package.names[lowered.matched as usize], arg.id);
                        if lowered.allowed {
                            assert_eq!(
                                reference[&program.keys[directive.key_index as usize].id],
                                lowered.value
                            );
                        }
                    }
                }
                for candidate in &package.candidates {
                    assert!(usize::from(candidate.state_guard) <= package.state.guards.len());
                }
            }
        }
    }

    #[test]
    fn ambiguous_owner_and_unknown_profile_fail_closed() {
        let mut registry = registry::ModuleRegistry::new();
        families::register_motorola68000_family_stack(&mut registry);
        let mut core = RuntimeModelCore::from_registry(&registry).unwrap();
        let (&(tag, owner, id), program) = core.state_programs.iter().next().unwrap();
        let program = program.clone();
        let resolved = core.resolve_pipeline(&program.profiles[0], None).unwrap();
        core.state_programs
            .insert((tag, owner, id.wrapping_add(1)), program.clone());
        let mut names = NameTable::from_core(&core).unwrap();
        assert!(prepare(&core, &resolved, &mut names)
            .unwrap_err()
            .contains("ambiguous"));
        assert!(lower(&program, "unknown profile", &mut names)
            .unwrap_err()
            .contains("profile unknown"));
    }

    #[test]
    fn flattened_guards_preserve_soft_hard_failure_and_clause_order() {
        let mut state = NumericStatePlan {
            keys: vec!["feature".into()],
            defaults: vec![0],
            ..Default::default()
        };
        let (soft, _) = unwrap_guard("state.require.v1:feature=1;future", &mut state).unwrap();
        let (hard, _) =
            unwrap_guard("state.require.v1:feature=1?disabled;future", &mut state).unwrap();
        assert_ne!(soft, hard);
        assert!(!state.guards[soft as usize - 1].clauses[0].reject);
        assert!(state.guards[hard as usize - 1].clauses[0].reject);
        let (nested, _) = unwrap_guard(
            "state.require.v1:feature=1;state.require.v1:feature=2?disabled;future",
            &mut state,
        )
        .unwrap();
        let clauses = &state.guards[nested as usize - 1].clauses;
        assert_eq!(
            clauses
                .iter()
                .map(|c| (c.values[0], c.reject))
                .collect::<Vec<_>>(),
            vec![(1, false), (2, true)]
        );
        for (value, expected) in [(0, 2), (1, 1)] {
            let status = clauses
                .iter()
                .find(|c| !c.values.contains(&value))
                .map_or(0, |c| if c.reject { 1 } else { 2 });
            assert_eq!(status, expected);
        }
    }

    #[test]
    fn guards_fail_closed_and_keep_unsupported_nested_plan() {
        let mut state = NumericStatePlan {
            keys: vec!["feature".into()],
            defaults: vec![0],
            ..Default::default()
        };
        assert_eq!(
            unwrap_guard(
                "state.require.v1:feature=1+2?disabled;future.recipe",
                &mut state
            )
            .unwrap(),
            (1, "future.recipe")
        );
        assert_eq!(state.guards[0].clauses[0].values, vec![1, 2]);
        assert_eq!(
            unwrap_guard(
                "state.require.v1:feature=1;state.require.v1:feature=2;future",
                &mut state
            )
            .unwrap(),
            (2, "future")
        );
        assert_eq!(state.guards[1].clauses.len(), 2);
        for plan in [
            "state.require.v1:missing=1;future",
            "state.require.v1:feature=;future",
            "state.require.v1:feature=x;future",
            "state.require.v1:feature=1?;future",
        ] {
            assert!(unwrap_guard(plan, &mut state).is_err(), "{plan}");
        }
    }
}
