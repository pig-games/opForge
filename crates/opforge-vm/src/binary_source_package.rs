//! Numeric package view used by the experimental binary-source frontend.

use std::collections::{BTreeMap, BTreeSet};

use package::{
    ModeSelectorDescriptor, MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR,
    MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR,
};
use types::hierarchy::ResolvedHierarchy;

use crate::runtime_model_core::RuntimeModelCore;
use crate::selector_vm::PortableSelectorOutcome;

mod state;
pub use state::{
    NumericStateArgument, NumericStateClause, NumericStateDirective, NumericStateGuard,
    NumericStatePlan,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinarySourcePackage {
    pub names: Vec<String>,
    pub state: NumericStatePlan,
    pub qualifiers: Vec<String>,
    pub aliases: Vec<NumericAlias>,
    pub registers: Vec<NumericRegister>,
    pub table_programs: Vec<NumericTableProgram>,
    pub semantic_programs: Vec<NumericProgram>,
    pub value_programs: Vec<NumericProgram>,
    pub candidates: Vec<NumericCandidate>,
    pub member_bindings: Vec<NumericMemberBinding>,
}

/// Package-selected member fields needed before lexical source names are bound.
/// These survive unsupported candidate recipes because binding precedes selection.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NumericMemberBinding {
    pub mnemonic: u16,
    pub qualifier: Option<u8>,
    pub operand: u8,
    pub field: u16,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NumericAlias {
    pub spelling: u16,
    pub spelling_qualifier: Option<u8>,
    pub mnemonic: u16,
    pub qualifier: Option<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NumericRegister {
    pub name: u16,
    pub class: u16,
    pub index: u16,
    pub owner_rank: u8,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NumericProgram {
    pub name: u16,
    pub owner_rank: u8,
    pub version: u16,
    pub bytes: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NumericTableProgram {
    pub mnemonic: u16,
    pub qualifier: Option<u8>,
    pub mode: u16,
    pub owner_rank: u8,
    pub bytes: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NumericCandidate {
    pub state_guard: u16,
    pub mnemonic: u16,
    pub qualifier: Option<u8>,
    pub shape: u16,
    pub mode: u16,
    pub owner_rank: u8,
    pub priority: u16,
    pub width_rank: u8,
    pub unstable_widen: bool,
    /// Bit N proves operand N cannot have a top-level Member root.
    pub member_excluded: u8,
    /// Known register names that disprove a rejection match conjunct.
    /// Other names and operand forms provide no evidence of a mismatch.
    pub known_name_excluded: Vec<(u8, u16)>,
    pub recipe: CandidateRecipe,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CandidateRecipe {
    None,
    Scalar(ScalarPlan),
    SemanticInputs {
        program: u16,
        inputs: Vec<Projection>,
    },
    SemanticBranch {
        program: u16,
        inputs: Vec<Projection>,
    },
    SemanticSequence {
        stages: Vec<SemanticStage>,
    },
    Unsupported {
        plan: u16,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SemanticStage {
    pub program: Option<u16>,
    pub fixup: bool,
    pub inputs: Vec<Projection>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ScalarPlan {
    U8,
    U16,
    Rel8,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Projection {
    Expression(u8),
    ScalarExpression(u8),
    ImmediateExpression(u8),
    IndirectValue {
        operand: u8,
    },
    TupleNamedRegister {
        operand: u8,
        name: u16,
    },
    TargetExpression(u8),
    AtomicTargetExpression(u8),
    TargetMember {
        operand: u8,
        qualifier: u16,
    },
    Register {
        operand: u8,
        class: u16,
    },
    IndirectRegister {
        operand: u8,
        class: u16,
    },
    UpdatedIndirectRegister {
        operand: u8,
        class: u16,
        token: u8,
    },
    NamedRegister {
        operand: u8,
        name: u16,
    },
    Member {
        operand: u8,
        qualifier: u16,
    },
    MemberShape {
        operand: u8,
        qualifier: u16,
    },
    TupleRegister {
        operand: u8,
        item: u8,
        class: u16,
    },
    TupleValue {
        operand: u8,
        item: u8,
    },
    TupleIdentityScale {
        operand: u8,
        item: u8,
    },
    TupleArity {
        operand: u8,
    },
    TupleQualifiedRegister {
        operand: u8,
        item: u8,
        class: u16,
        qualifier: u16,
    },
    TupleArityThree {
        operand: u8,
    },
    CallArgumentRegister {
        operand: u8,
        argument: u8,
        class: u16,
    },
    RegisterMask {
        operand: u8,
        first_class: u16,
        first_shift: u8,
        second_class: u16,
        second_shift: u8,
        reverse: bool,
    },
    ValueProgram {
        program: u16,
        source: Box<Projection>,
    },
    RequiredValueProgram {
        program: u16,
        source: Box<Projection>,
    },
    Constant(i64),
}

impl BinarySourcePackage {
    pub fn prepare(core: &RuntimeModelCore, resolved: &ResolvedHierarchy) -> Result<Self, String> {
        let mut names = NameTable::from_core(core)?;
        let mut qualifiers = QualifierTable::default();
        let owners = core.scoped_owner_lookup_order(resolved);
        let mut state = state::prepare(core, resolved, &mut names)?;
        let mut aliases = Vec::new();
        let mut spellings = BTreeSet::new();
        for (forms, owner_name) in [
            (&core.family_forms, &resolved.family_id),
            (&core.cpu_forms, &resolved.cpu_id),
            (&core.dialect_forms, &resolved.dialect_id),
        ] {
            if let Some(active) = forms.get(&owner_name.to_ascii_lowercase()) {
                for form in active {
                    spellings.insert(form.to_ascii_lowercase());
                }
            }
        }
        for spelling in spellings {
            let (spelling_base, spelling_qualifier) = split_qualifier(&spelling);
            let target = match core
                .resolve_selector_choice(resolved, &spelling)
                .map_err(|e| e.to_string())?
            {
                Some(PortableSelectorOutcome::Target(target)) => target,
                Some(PortableSelectorOutcome::Diagnostic(_)) => continue,
                None => spelling.clone(),
            };
            let (base, qualifier) = split_qualifier(&target);
            aliases.push(NumericAlias {
                spelling: names.id(spelling_base),
                spelling_qualifier: spelling_qualifier.map(|value| qualifiers.id(value)),
                mnemonic: names.id(base),
                qualifier: qualifier.map(|value| qualifiers.id(value)),
            });
        }

        let mut registers = Vec::new();
        let mut table_programs = Vec::new();
        let mut semantic_programs = Vec::new();
        let mut value_programs = Vec::new();
        let mut candidates = Vec::new();
        let mut candidate_plans = Vec::new();
        let mut member_bindings = BTreeSet::new();
        for (rank, (tag, owner)) in owners.into_iter().enumerate() {
            let Some(owner) = owner else { continue };
            for (&(key_tag, key_owner, name), value) in &core.register_encodings {
                if (key_tag, key_owner) == (tag, owner) {
                    registers.push(NumericRegister {
                        name: names.core_id(name),
                        class: value.class,
                        index: value.index,
                        owner_rank: rank as u8,
                    });
                }
            }
            let mut tables = core.vm_programs.iter().collect::<Vec<_>>();
            tables.sort_by_key(|(key, _)| **key);
            for (&(key_tag, key_owner, mnemonic, mode), bytes) in tables {
                if (key_tag, key_owner) == (tag, owner) {
                    let spelling = names.name(mnemonic).to_string();
                    let (base, qualifier) = split_qualifier(&spelling);
                    table_programs.push(NumericTableProgram {
                        mnemonic: names.id(base),
                        qualifier: qualifier.map(|value| qualifiers.id(value)),
                        mode: names.core_id(mode),
                        owner_rank: rank as u8,
                        bytes: bytes.clone(),
                    });
                }
            }
            collect_programs(
                &core.semantic_programs,
                tag,
                owner,
                rank,
                &mut semantic_programs,
                &names,
            );
            collect_programs(
                &core.value_programs,
                tag,
                owner,
                rank,
                &mut value_programs,
                &names,
            );
            let mut selectors = core.mode_selectors.iter().collect::<Vec<_>>();
            selectors.sort_by_key(|(key, _)| **key);
            for (&(key_tag, key_owner, mnemonic, shape), rows) in selectors {
                if (key_tag, key_owner) != (tag, owner) {
                    continue;
                }
                for row in rows {
                    let (guard, nested) = state::unwrap_guard(&row.operand_plan, &mut state)?;
                    let mut row = row.clone();
                    row.operand_plan = nested.to_string();
                    let spelling = names.name(mnemonic).to_string();
                    let (base, qualifier) = split_qualifier(&spelling);
                    let bound_mnemonic = names.id(base);
                    let bound_qualifier = qualifier.map(|value| qualifiers.id(value));
                    for (operand, field) in member_binding_fields(&row.operand_plan) {
                        member_bindings.insert(NumericMemberBinding {
                            mnemonic: bound_mnemonic,
                            qualifier: bound_qualifier,
                            operand,
                            field: names.id(field),
                        });
                    }
                    candidate_plans.push(row.operand_plan.clone());
                    let mut lowered = candidate(
                        &row,
                        mnemonic,
                        shape,
                        rank as u8,
                        &mut names,
                        &mut qualifiers,
                    );
                    lowered.state_guard = guard;
                    candidates.push(lowered);
                }
            }
        }
        registers.sort_by_key(|row| (row.owner_rank, row.name));
        for (candidate, plan) in candidates.iter_mut().zip(candidate_plans) {
            candidate.known_name_excluded = known_name_exclusions(&plan, &registers, &names.names);
        }
        table_programs.sort_by_key(|row| (row.owner_rank, row.mnemonic, row.qualifier, row.mode));
        semantic_programs.sort_by_key(|row| (row.owner_rank, row.name));
        value_programs.sort_by_key(|row| (row.owner_rank, row.name));
        candidates.sort_by_key(|row| {
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
        aliases.sort_by_key(|row| (row.spelling, row.spelling_qualifier));
        if names.overflow {
            return Err("binary-source package name dictionary exceeds u16".into());
        }
        if qualifiers.overflow {
            return Err("binary-source qualifier dictionary exceeds u8".into());
        }
        Ok(Self {
            state,
            names: names.finish(),
            qualifiers: qualifiers.finish(),
            aliases,
            registers,
            table_programs,
            semantic_programs,
            value_programs,
            candidates,
            member_bindings: member_bindings.into_iter().collect(),
        })
    }
}

fn collect_programs(
    map: &std::collections::HashMap<(u8, u32, u32), (u16, Vec<u8>)>,
    tag: u8,
    owner: u32,
    rank: usize,
    out: &mut Vec<NumericProgram>,
    names: &NameTable,
) {
    for (&(key_tag, key_owner, name), (version, bytes)) in map {
        if (key_tag, key_owner) == (tag, owner) {
            out.push(NumericProgram {
                name: names.core_id(name),
                owner_rank: rank as u8,
                version: *version,
                bytes: bytes.clone(),
            });
        }
    }
}

fn candidate(
    row: &ModeSelectorDescriptor,
    mnemonic: u32,
    shape: u32,
    owner_rank: u8,
    names: &mut NameTable,
    qualifiers: &mut QualifierTable,
) -> NumericCandidate {
    let spelling = names.name(mnemonic).to_string();
    let (base, qualifier) = split_qualifier(&spelling);
    NumericCandidate {
        state_guard: 0,
        mnemonic: names.id(base),
        qualifier: qualifier.map(|value| qualifiers.id(value)),
        shape: names.core_id(shape),
        mode: names.id(&row.mode_key),
        owner_rank,
        priority: row.priority,
        width_rank: row.width_rank,
        unstable_widen: row.unstable_widen,
        member_excluded: member_excluded(&row.operand_plan),
        known_name_excluded: Vec::new(),
        recipe: parse_recipe(&row.operand_plan, names),
    }
}

/// A deliberately partial proof, not an alternative rejection executor. Matching
/// one conjunct cannot establish acceptance; disproving one establishes mismatch.
fn known_name_exclusions(
    plan: &str,
    registers: &[NumericRegister],
    names: &[String],
) -> Vec<(u8, u16)> {
    if !plan.starts_with("semv.reject.v1:") {
        return Vec::new();
    }
    let Some(inputs) = match_inputs(plan) else {
        return Vec::new();
    };
    let mut active = BTreeMap::new();
    // Scope lookup is first-wins even when the shadowed class would match.
    for register in registers {
        active.entry(register.name).or_insert(register);
    }
    let mut excluded = BTreeSet::new();
    for input in inputs.split(',') {
        let Some((operand, predicate)) = KnownRegisterPredicate::parse(input) else {
            continue;
        };
        for (&name, register) in &active {
            if !predicate.matches(register.class, &names[usize::from(name)]) {
                excluded.insert((operand, name));
            }
        }
    }
    excluded.into_iter().collect()
}

enum KnownRegisterPredicate<'a> {
    Class(u16),
    ClassOrRange {
        classes: Vec<u16>,
        prefix: &'a str,
        minimum: u32,
        maximum: u32,
    },
}

impl<'a> KnownRegisterPredicate<'a> {
    // Mirror only these two exact canonical forms. Malformed or future forms
    // yield no proof and remain the unsupported-candidate barrier.
    fn parse(value: &'a str) -> Option<(u8, Self)> {
        if let Some((operand, class)) = value
            .strip_prefix("reg")
            .and_then(|rest| rest.split_once(".class"))
            .filter(|(operand, _)| operand.bytes().all(|b| b.is_ascii_digit()))
        {
            return Some((operand.parse().ok()?, Self::Class(class.parse().ok()?)));
        }
        let rest = value.strip_prefix("register_or_named_range")?;
        let (index_and_classes, pattern) = rest.split_once(".prefix")?;
        let (operand, classes) = index_and_classes.split_once(".classes")?;
        let classes = classes
            .split('+')
            .map(str::parse)
            .collect::<Result<Vec<u16>, _>>()
            .ok()?;
        let (prefix, bounds) = pattern.split_once(".min")?;
        let (minimum, maximum) = bounds.split_once(".max")?;
        let minimum = minimum.parse().ok()?;
        let maximum = maximum.parse().ok()?;
        if prefix.is_empty() || minimum > maximum {
            return None;
        }
        Some((
            operand.parse().ok()?,
            Self::ClassOrRange {
                classes,
                prefix,
                minimum,
                maximum,
            },
        ))
    }

    fn matches(&self, class: u16, name: &str) -> bool {
        match self {
            Self::Class(expected) => class == *expected,
            Self::ClassOrRange {
                classes,
                prefix,
                minimum,
                maximum,
            } => {
                classes.contains(&class)
                    || name
                        .get(..prefix.len())
                        .is_some_and(|head| head.eq_ignore_ascii_case(prefix))
                        && name.get(prefix.len()..).is_some_and(|suffix| {
                            !suffix.is_empty()
                                && suffix.bytes().all(|b| b.is_ascii_digit())
                                && suffix
                                    .parse::<u32>()
                                    .is_ok_and(|value| (*minimum..=*maximum).contains(&value))
                        })
            }
        }
    }
}

fn member_excluded(plan: &str) -> u8 {
    match_inputs(plan)
        .map(|inputs| {
            inputs
                .split(',')
                .filter_map(non_member_operand)
                .filter(|operand| *operand < 2)
                .fold(0, |mask, operand| mask | (1 << operand))
        })
        .unwrap_or(0)
}

fn match_inputs(plan: &str) -> Option<&str> {
    for prefix in ["semv.inputs.v1:", "semv.reject.v1:", "semv.branch.v1:"] {
        if let Some(body) = plan.strip_prefix(prefix) {
            let (_, inputs) = body.split_once('@')?;
            return Some(inputs.split('|').next().unwrap_or(inputs));
        }
    }
    let body = plan.strip_prefix("semv.sequence.v1:")?;
    let inputs = body.strip_prefix("match:_@")?;
    inputs.split_once(';').map(|(inputs, _)| inputs)
}

fn non_member_operand(value: &str) -> Option<u8> {
    if let Some(rest) = value.strip_prefix("required_value_program:") {
        let (program, source) = rest.split_once(':')?;
        if program.is_empty() || source.is_empty() {
            return None;
        }
        return non_member_operand(source);
    }
    if let Some(rest) = value.strip_prefix("value_program:") {
        let (program, source) = rest.split_once(':')?;
        if program.is_empty() || source.is_empty() {
            return None;
        }
        return non_member_operand(source);
    }
    if let Some(rest) = value.strip_prefix("indirect_value") {
        return rest.parse().ok();
    }
    if let Some(rest) = value.strip_prefix("indirect_tuple_named_register") {
        let (operand, name) = rest.split_once(".item1=")?;
        if name.is_empty() {
            return None;
        }
        return operand.parse().ok();
    }
    for prefix in [
        "reg",
        "indirect_reg",
        "unary_plus_indirect_reg",
        "unary_minus_indirect_reg",
    ] {
        if let Some(rest) = value.strip_prefix(prefix) {
            let (operand, class) = rest.split_once(".class")?;
            if !operand.is_empty()
                && operand.bytes().all(|byte| byte.is_ascii_digit())
                && !class.is_empty()
                && class.bytes().all(|byte| byte.is_ascii_digit())
            {
                return operand.parse().ok();
            }
            return None;
        }
    }
    let rest = value.strip_prefix("indirect_tuple_")?;
    let (head, tail) = rest.split_once('.')?;
    if head.is_empty() || tail.is_empty() {
        return None;
    }
    let digits = head
        .rfind(|character: char| !character.is_ascii_digit())
        .map_or(head, |index| &head[index + 1..]);
    if digits.is_empty() {
        return None;
    }
    let kind = &head[..head.len() - digits.len()];
    let fields = tail.split('.').collect::<Vec<_>>();
    let numbered = |field: &str, prefix: &str| {
        field.strip_prefix(prefix).is_some_and(|number| {
            !number.is_empty() && number.bytes().all(|byte| byte.is_ascii_digit())
        })
    };
    let known = match (kind, fields.as_slice()) {
        ("reg", [item, class]) => numbered(item, "item") && numbered(class, "class"),
        ("qualified_reg", [item, qualifier, class]) => {
            numbered(item, "item") && !qualifier.is_empty() && numbered(class, "class")
        }
        ("value" | "identity_scale", [item]) => numbered(item, "item"),
        ("arity", [arity]) => numbered(arity, "value"),
        _ => false,
    };
    if !known {
        return None;
    }
    digits.parse().ok()
}

fn parse_recipe(plan: &str, names: &mut NameTable) -> CandidateRecipe {
    match plan {
        "none" => CandidateRecipe::None,
        "u8" => CandidateRecipe::Scalar(ScalarPlan::U8),
        "u16" => CandidateRecipe::Scalar(ScalarPlan::U16),
        "rel8" => CandidateRecipe::Scalar(ScalarPlan::Rel8),
        _ if plan.starts_with("semv.sequence.v1:") => parse_sequence(plan, names),
        _ if plan.starts_with("semv.inputs.v1:") => {
            parse_semantic(plan, "semv.inputs.v1:", false, names)
        }
        _ if plan.starts_with("semv.branch.v1:") => {
            parse_semantic(plan, "semv.branch.v1:", true, names)
        }
        _ => CandidateRecipe::Unsupported {
            plan: names.id(plan),
        },
    }
}

// Only bounded match/encode/fixup sequences are executable. Unknown stages remain
// explicit unsupported candidates; match stages have no executable program.
fn parse_sequence(plan: &str, names: &mut NameTable) -> CandidateRecipe {
    let parsed = (|| {
        let mut stages: Vec<SemanticStage> = Vec::new();
        let mut encoded = false;
        let body = plan.strip_prefix("semv.sequence.v1:")?;
        let sequence = body
            .split_once(MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR)
            .map_or(body, |(sequence, _)| sequence);
        for stage in sequence.split(';') {
            let (kind, body) = stage.split_once(':')?;
            let (program, inputs) = body.split_once('@')?;
            let program = match kind {
                "match" if program == "_" && !encoded => None,
                "encode" if !program.is_empty() && program != "_" => {
                    encoded = true;
                    Some(names.id(program))
                }
                "fixup" if encoded && !program.is_empty() && program != "_" => {
                    Some(names.id(program))
                }
                _ => return None,
            };
            let inputs = inputs
                .split(',')
                .map(|value| parse_projection(value, names))
                .collect::<Option<Vec<_>>>()?;
            if inputs.is_empty() || inputs.len() > 16 || stages.len() == 8 {
                return None;
            }
            if kind == "fixup"
                && !inputs.iter().all(|input| {
                    matches!(
                        input,
                        Projection::Expression(_)
                            | Projection::TargetExpression(_)
                            | Projection::TargetMember { .. }
                    ) || matches!(input, Projection::TupleValue { operand, item: 0 } if has_bounded_tuple_match(&stages, *operand))
                })
            {
                return None;
            }
            if kind == "encode"
                && inputs.iter().any(|input| {
                    matches!(
                        input,
                        Projection::TargetExpression(_)
                            | Projection::AtomicTargetExpression(_)
                            | Projection::TargetMember { .. }
                    )
                })
            {
                return None;
            }
            stages.push(SemanticStage {
                program,
                fixup: kind == "fixup",
                inputs,
            });
        }
        encoded.then_some(CandidateRecipe::SemanticSequence { stages })
    })();
    parsed.unwrap_or_else(|| CandidateRecipe::Unsupported {
        plan: names.id(plan),
    })
}

fn has_bounded_tuple_match(stages: &[SemanticStage], operand: u8) -> bool {
    stages.iter().any(|stage| {
        !stage.fixup
            && stage.program.is_none()
            && stage.inputs.contains(&Projection::TupleArity { operand })
            && stage.inputs.iter().any(|input| {
                matches!(input, Projection::TupleRegister { operand: other, .. } | Projection::TupleNamedRegister { operand: other, .. } if *other == operand)
            })
    })
}

fn parse_semantic(
    plan: &str,
    prefix: &str,
    branch: bool,
    names: &mut NameTable,
) -> CandidateRecipe {
    let body = &plan[prefix.len()..];
    let Some((program, inputs)) = body.split_once('@') else {
        return CandidateRecipe::Unsupported {
            plan: names.id(plan),
        };
    };
    let inputs = inputs.split('|').next().unwrap_or(inputs);
    let parts = inputs.split(',').collect::<Vec<_>>();
    // The branch plan's third field is a requested candidate. Its `auto`
    // spelling becomes the existing numeric SEMV request sentinel.
    if branch
        && (parts.len() != 4
            || parts[0].parse::<u8>().is_err()
            || parts[1] != "expr0"
            || (parts[2] != "auto" && parts[2].parse::<u8>().is_err())
            || parts[3].parse::<u8>().is_err())
    {
        return CandidateRecipe::Unsupported {
            plan: names.id(plan),
        };
    }
    let inputs = parts
        .into_iter()
        .enumerate()
        .map(|(index, value)| {
            if branch && index == 2 && value == "auto" {
                Some(Projection::Constant(-1))
            } else {
                parse_projection(value, names)
            }
        })
        .collect::<Option<Vec<_>>>();
    let Some(inputs) = inputs else {
        return CandidateRecipe::Unsupported {
            plan: names.id(plan),
        };
    };
    if inputs.iter().any(|input| {
        matches!(
            input,
            Projection::TargetExpression(_)
                | Projection::AtomicTargetExpression(_)
                | Projection::TargetMember { .. }
        )
    }) {
        return CandidateRecipe::Unsupported {
            plan: names.id(plan),
        };
    }
    let program = names.id(program);
    if branch {
        CandidateRecipe::SemanticBranch { program, inputs }
    } else {
        CandidateRecipe::SemanticInputs { program, inputs }
    }
}

fn bounded_tuple_item(value: &str) -> Option<u8> {
    let item = value.parse().ok()?;
    (item <= 2).then_some(item)
}

fn parse_projection(value: &str, names: &mut NameTable) -> Option<Projection> {
    if let Some(mask) = parse_register_mask(value) {
        return Some(mask);
    }
    if let Some(rest) = value.strip_prefix("target_atom:expr") {
        return rest.parse().ok().map(Projection::AtomicTargetExpression);
    }
    if let Some(rest) = value.strip_prefix("target:expr") {
        return rest.parse().ok().map(Projection::TargetExpression);
    }
    if let Some(rest) = value.strip_prefix("target:") {
        let (operand, qualifier) = parse_member_projection(rest)?;
        return Some(Projection::TargetMember {
            operand,
            qualifier: names.id(qualifier),
        });
    }
    if let Some(rest) = value.strip_prefix("value_program:") {
        let (program, source) = rest.split_once(':')?;
        return Some(Projection::ValueProgram {
            program: names.id(program),
            source: Box::new(parse_projection(source, names)?),
        });
    }
    if let Some(rest) = value.strip_prefix("required_value_program:") {
        let (program, source) = rest.split_once(':')?;
        return Some(Projection::RequiredValueProgram {
            program: names.id(program),
            source: Box::new(parse_projection(source, names)?),
        });
    }
    if let Some(rest) = value.strip_prefix("literal:") {
        return rest.parse().ok().map(Projection::Constant);
    }
    if let Ok(value) = value.parse() {
        return Some(Projection::Constant(value));
    }
    if let Some(rest) = value.strip_prefix("immediate") {
        let operand = rest.parse::<u8>().ok()?;
        return (operand <= 1).then_some(Projection::ImmediateExpression(operand));
    }
    if let Some(rest) = value.strip_prefix("scalar_expr") {
        return rest.parse().ok().map(Projection::ScalarExpression);
    }
    if let Some(rest) = value.strip_prefix("expr") {
        return rest.parse().ok().map(Projection::Expression);
    }
    if let Some(rest) = value.strip_prefix("indirect_value") {
        return rest
            .parse()
            .ok()
            .map(|operand| Projection::IndirectValue { operand });
    }
    if let Some(rest) = value.strip_prefix("indirect_tuple_named_register") {
        let (operand, name) = rest.split_once(".item1=")?;
        if name.is_empty() {
            return None;
        }
        return Some(Projection::TupleNamedRegister {
            operand: operand.parse().ok()?,
            name: names.id(name),
        });
    }
    if let Some(rest) = value.strip_prefix("named_register") {
        let (operand, name) = rest.split_once('=')?;
        if name.is_empty() {
            return None;
        }
        return Some(Projection::NamedRegister {
            operand: operand.parse().ok()?,
            name: names.id(name),
        });
    }
    if let Some(rest) = value.strip_prefix("call_arg_register") {
        let (operand, rest) = rest.split_once(".arg")?;
        let (argument, class) = rest.split_once(".class")?;
        let operand = operand.parse::<u8>().ok()?;
        let argument = argument.parse::<u8>().ok()?;
        let class = class.parse::<u16>().ok()?;
        if operand > 1 || argument > 1 || class == u16::MAX {
            return None;
        }
        return Some(Projection::CallArgumentRegister {
            operand,
            argument,
            class,
        });
    }
    if let Some(rest) = value.strip_prefix("reg") {
        let (operand, class) = rest.split_once(".class")?;
        return Some(Projection::Register {
            operand: operand.parse().ok()?,
            class: class.parse().ok()?,
        });
    }
    if let Some(rest) = value.strip_prefix("indirect_reg") {
        let (operand, class) = rest.split_once(".class")?;
        return Some(Projection::IndirectRegister {
            operand: operand.parse().ok()?,
            class: class.parse().ok()?,
        });
    }
    for (prefix, token) in [
        ("unary_plus_indirect_reg", 18),
        ("unary_minus_indirect_reg", 19),
    ] {
        if let Some(rest) = value.strip_prefix(prefix) {
            let (operand, class) = rest.split_once(".class")?;
            return Some(Projection::UpdatedIndirectRegister {
                operand: operand.parse().ok()?,
                class: class.parse().ok()?,
                token,
            });
        }
    }
    if let Some(rest) = value.strip_prefix("member_shape") {
        let (operand, qualifier) = parse_member_spec(rest)?;
        return Some(Projection::MemberShape {
            operand,
            qualifier: names.id(qualifier),
        });
    }
    if let Some((operand, qualifier)) = parse_member_projection(value) {
        return Some(Projection::Member {
            operand,
            qualifier: names.id(qualifier),
        });
    }
    if let Some(rest) = value.strip_prefix("indirect_tuple_qualified_reg") {
        let (operand, rest) = rest.split_once(".item")?;
        let (item, rest) = rest.split_once(".qualifier")?;
        let item = bounded_tuple_item(item)?;
        let (qualifier, class) = rest.split_once(".class")?;
        if qualifier.is_empty() {
            return None;
        }
        return Some(Projection::TupleQualifiedRegister {
            operand: operand.parse().ok()?,
            item,
            class: class.parse().ok()?,
            qualifier: names.id(qualifier),
        });
    }
    if let Some(rest) = value.strip_prefix("indirect_tuple_reg") {
        let (operand, rest) = rest.split_once(".item")?;
        let (item, tail) = rest.split_once(".class")?;
        let item = bounded_tuple_item(item)?;
        return Some(Projection::TupleRegister {
            operand: operand.parse().ok()?,
            item,
            class: tail.parse().ok()?,
        });
    }
    if let Some(rest) = value.strip_prefix("indirect_tuple_value") {
        let (operand, item) = rest.split_once(".item")?;
        let item = bounded_tuple_item(item)?;
        return Some(Projection::TupleValue {
            operand: operand.parse().ok()?,
            item,
        });
    }
    if let Some(rest) = value.strip_prefix("indirect_tuple_identity_scale") {
        let (operand, item) = rest.split_once(".item")?;
        return Some(Projection::TupleIdentityScale {
            operand: operand.parse().ok()?,
            item: bounded_tuple_item(item)?,
        });
    }
    if let Some(rest) = value.strip_prefix("indirect_tuple_arity") {
        let (operand, arity) = rest.split_once(".value")?;
        if arity == "3" {
            return Some(Projection::TupleArityThree {
                operand: operand.parse().ok()?,
            });
        }
        if arity != "2" {
            return None;
        }
        return Some(Projection::TupleArity {
            operand: operand.parse().ok()?,
        });
    }
    None
}

fn parse_register_mask(value: &str) -> Option<Projection> {
    let rest = value.strip_prefix("register_mask")?;
    let (operand, mapping) = rest.split_once(".map")?;
    let operand = operand.parse::<u8>().ok()?;
    let (mapping, reverse) = if let Some(mapping) = mapping.strip_suffix(".reverse16") {
        (mapping, true)
    } else {
        (mapping, false)
    };
    let (first, second) = mapping
        .split_once('+')
        .map_or((mapping, None), |(first, second)| (first, Some(second)));
    let (first_class, first_shift) = first.split_once('=')?;
    let first_class = first_class.parse::<u16>().ok()?;
    let first_shift = first_shift.parse::<u8>().ok()?;
    let (second_class, second_shift) = if let Some(second) = second {
        let (class, shift) = second.split_once('=')?;
        let class = class.parse::<u16>().ok()?;
        if class == u16::MAX {
            return None;
        }
        (class, shift.parse::<u8>().ok()?)
    } else {
        (u16::MAX, 0)
    };
    if operand > 1
        || first_class == u16::MAX
        || first_class == second_class
        || first_shift > 15
        || second_shift > 15
    {
        return None;
    }
    Some(Projection::RegisterMask {
        operand,
        first_class,
        first_shift,
        second_class,
        second_shift,
        reverse,
    })
}

fn parse_member_projection(value: &str) -> Option<(u8, &str)> {
    let rest = value.strip_prefix("member")?;
    parse_member_spec(rest)
}

fn parse_member_spec(rest: &str) -> Option<(u8, &str)> {
    let (operand, qualifier) = rest.split_once(MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR)?;
    if operand.is_empty()
        || !operand.bytes().all(|byte| byte.is_ascii_digit())
        || qualifier.is_empty()
    {
        return None;
    }
    Some((operand.parse().ok()?, qualifier))
}

fn member_binding_fields(plan: &str) -> BTreeSet<(u8, &str)> {
    let plan = plan
        .split_once(MODE_SELECTOR_PLAN_DIAGNOSTIC_SEPARATOR)
        .map_or(plan, |(body, _)| body);
    let bodies: Vec<_> = if let Some(sequence) = plan.strip_prefix("semv.sequence.v1:") {
        sequence
            .split(';')
            .filter_map(|stage| {
                let (kind, body) = stage.split_once(':')?;
                matches!(kind, "match" | "encode" | "fixup").then_some(body)
            })
            .collect()
    } else {
        ["semv.inputs.v1:", "semv.reject.v1:", "semv.branch.v1:"]
            .into_iter()
            .find_map(|prefix| plan.strip_prefix(prefix))
            .into_iter()
            .collect()
    };
    bodies
        .into_iter()
        .filter_map(|body| {
            let (program, inputs) = body.split_once('@')?;
            (!program.is_empty()).then_some(inputs)
        })
        .flat_map(|inputs| inputs.split(','))
        .filter_map(member_binding_field)
        .collect()
}

fn member_binding_field(source: &str) -> Option<(u8, &str)> {
    for prefix in ["value_program:", "required_value_program:"] {
        if let Some(rest) = source.strip_prefix(prefix) {
            let (program, source) = rest.split_once(':')?;
            return if program.is_empty() {
                None
            } else {
                member_binding_field(source)
            };
        }
    }
    if let Some(source) = source.strip_prefix("target:") {
        return parse_member_projection(source);
    }
    if let Some(rest) = source.strip_prefix("member_shape") {
        return parse_member_spec(rest);
    }
    parse_member_projection(source)
}

fn split_qualifier(value: &str) -> (&str, Option<&str>) {
    value
        .rsplit_once('.')
        .map_or((value, None), |(base, qualifier)| (base, Some(qualifier)))
}

struct NameTable {
    names: Vec<String>,
    ids: BTreeMap<String, u16>,
    reverse: BTreeMap<u32, String>,
    overflow: bool,
}
impl NameTable {
    fn from_core(core: &RuntimeModelCore) -> Result<Self, String> {
        let mut reverse = BTreeMap::new();
        for (name, id) in &core.interned_ids {
            reverse.insert(*id, name.clone());
        }
        let mut names = reverse.values().cloned().collect::<Vec<_>>();
        names.sort();
        names.dedup();
        if names.len() > u16::MAX as usize + 1 {
            return Err("binary-source package name dictionary exceeds u16".into());
        }
        let ids = names
            .iter()
            .enumerate()
            .map(|(id, name)| (name.clone(), id as u16))
            .collect();
        Ok(Self {
            names,
            ids,
            reverse,
            overflow: false,
        })
    }
    fn id(&mut self, value: &str) -> u16 {
        let key = value.to_ascii_lowercase();
        if let Some(id) = self.ids.get(&key) {
            return *id;
        }
        let Ok(id) = u16::try_from(self.names.len()) else {
            self.overflow = true;
            return 0;
        };
        self.names.push(key.clone());
        self.ids.insert(key, id);
        id
    }
    fn name(&self, id: u32) -> &str {
        self.reverse.get(&id).map(String::as_str).unwrap_or("")
    }
    fn core_id(&self, id: u32) -> u16 {
        self.ids[self.name(id)]
    }
    fn finish(self) -> Vec<String> {
        self.names
    }
}

#[derive(Default)]
struct QualifierTable {
    values: Vec<String>,
    ids: BTreeMap<String, u8>,
    overflow: bool,
}
impl QualifierTable {
    fn id(&mut self, value: &str) -> u8 {
        let key = value.to_ascii_lowercase();
        if let Some(id) = self.ids.get(&key) {
            return *id;
        }
        let Ok(id) = u8::try_from(self.values.len()) else {
            self.overflow = true;
            return 0;
        };
        self.values.push(key.clone());
        self.ids.insert(key, id);
        id
    }
    fn finish(self) -> Vec<String> {
        self.values
    }
}

#[cfg(test)]
mod tests {
    use super::{
        known_name_exclusions, member_excluded, parse_member_projection, parse_projection,
        parse_register_mask, BTreeMap, BTreeSet, CandidateRecipe, NameTable, NumericRegister,
        Projection,
    };

    #[test]
    fn immediate_projection_lowers_only_two_numeric_operand_slots() {
        let mut names = NameTable {
            names: Vec::new(),
            ids: BTreeMap::new(),
            reverse: BTreeMap::new(),
            overflow: false,
        };
        for operand in 0..=1 {
            assert_eq!(
                parse_projection(&format!("immediate{operand}"), &mut names),
                Some(Projection::ImmediateExpression(operand))
            );
        }
        for source in [
            "immediate",
            "immediate2",
            "immediate256",
            "immediate-1",
            "immediate0.extra",
        ] {
            assert_eq!(parse_projection(source, &mut names), None, "{source}");
        }
        assert!(
            matches!(super::parse_recipe("semv.inputs.v1:pflush@expr0,immediate1", &mut names),
            CandidateRecipe::SemanticInputs { inputs, .. }
                if inputs == [Projection::Expression(0), Projection::ImmediateExpression(1)])
        );
        assert!(
            matches!(parse_projection("value_program:mask:immediate1", &mut names),
            Some(Projection::ValueProgram { source, .. }) if *source == Projection::ImmediateExpression(1))
        );
        assert!(matches!(
            super::parse_recipe("semv.inputs.v1:pflush@expr0,immediate2", &mut names),
            CandidateRecipe::Unsupported { .. }
        ));
    }

    #[test]
    fn register_mask_projection_requires_bounded_unambiguous_map() {
        assert_eq!(
            parse_register_mask("register_mask1.map0=0+1=8"),
            Some(Projection::RegisterMask {
                operand: 1,
                first_class: 0,
                first_shift: 0,
                second_class: 1,
                second_shift: 8,
                reverse: false,
            })
        );
        assert_eq!(
            parse_register_mask("register_mask0.map0=0+1=8.reverse16"),
            Some(Projection::RegisterMask {
                operand: 0,
                first_class: 0,
                first_shift: 0,
                second_class: 1,
                second_shift: 8,
                reverse: true,
            })
        );
        for invalid in [
            "register_mask2.map0=0+1=8",
            "register_mask1.map0=0+0=8",
            "register_mask1.map0=32+1=8",
            "register_mask1.map0=0+1=32",
            "register_mask1.map0=16+1=8",
            "register_mask1.map0=16+1=8.reverse16",
            "register_mask1.map0=0+1=8.reverse8",
            "register_mask1.map0=0+1=8+2=12",
        ] {
            assert_eq!(parse_register_mask(invalid), None, "{invalid}");
        }
    }

    #[test]
    fn compact_call_argument_register_projection_is_bounded() {
        let mut names = NameTable {
            names: Vec::new(),
            ids: BTreeMap::new(),
            reverse: BTreeMap::new(),
            overflow: false,
        };
        assert_eq!(
            parse_projection("call_arg_register1.arg1.class2", &mut names),
            Some(Projection::CallArgumentRegister {
                operand: 1,
                argument: 1,
                class: 2
            })
        );
        for invalid in [
            "call_arg_register2.arg0.class2",
            "call_arg_register1.arg2.class2",
            "call_arg_register1.arg1.class65535",
            "call_arg_register1.arg1.class2.extra",
            "call_arg_register_sequence1.arg0.arg1.class2.align2",
        ] {
            assert_eq!(parse_projection(invalid, &mut names), None, "{invalid}");
        }
    }

    #[test]
    fn single_class_register_mask_uses_absent_class_sentinel() {
        assert_eq!(
            parse_register_mask("register_mask0.map2=0"),
            Some(Projection::RegisterMask {
                operand: 0,
                first_class: 2,
                first_shift: 0,
                second_class: u16::MAX,
                second_shift: 0,
                reverse: false
            })
        );
        for invalid in [
            "register_mask0.map65535=0",
            "register_mask0.map2=16",
            "register_mask0.map2=0+65535=0",
            "register_mask0.map2=0+",
            "register_mask0.map2=0.reverse8",
        ] {
            assert_eq!(parse_register_mask(invalid), None, "{invalid}");
        }
    }

    #[test]
    fn named_register_projection_normalizes_the_name() {
        let mut names = NameTable {
            names: Vec::new(),
            ids: BTreeMap::new(),
            reverse: BTreeMap::new(),
            overflow: false,
        };
        assert_eq!(
            parse_projection("named_register1=INDEX", &mut names),
            Some(Projection::NamedRegister {
                operand: 1,
                name: 0
            })
        );
        assert_eq!(names.names, ["index"]);
        assert_eq!(parse_projection("named_register1=", &mut names), None);
    }

    #[test]
    fn known_name_rejection_proofs_respect_classes_ranges_and_scope() {
        let names = ["r0", "r1", "b0007", "b8"].map(str::to_string);
        let registers = vec![
            NumericRegister {
                name: 0,
                class: 0,
                index: 0,
                owner_rank: 0,
            },
            NumericRegister {
                name: 1,
                class: 1,
                index: 1,
                owner_rank: 0,
            },
            NumericRegister {
                name: 2,
                class: 9,
                index: 7,
                owner_rank: 0,
            },
            NumericRegister {
                name: 3,
                class: 9,
                index: 8,
                owner_rank: 0,
            },
            NumericRegister {
                name: 0,
                class: 1,
                index: 0,
                owner_rank: 1,
            },
        ];
        assert_eq!(
            known_name_exclusions("semv.reject.v1:bad@reg0.class1", &registers, &names),
            [(0, 0), (0, 2), (0, 3)]
        );
        assert_eq!(
            known_name_exclusions(
                "semv.reject.v1:bad@register_or_named_range1.classes0+1.prefixB.min0.max7",
                &registers,
                &names
            ),
            [(1, 3)]
        );
        assert_eq!(
            known_name_exclusions(
                "semv.reject.v1:bad@register_or_named_range0.classes1.prefixb.min0.max7",
                &registers,
                &names
            ),
            [(0, 0), (0, 3)]
        );
        for plan in [
            "semv.reject.v2:bad@reg0.class0",
            "semv.inputs.v1:enc@reg0.class0",
            "semv.reject.v1:bad@unknown0",
            "semv.reject.v1:bad@register_or_named_range0.classes1.prefixb.min8.max7",
            "semv.reject.v1:bad@register_or_named_range0.classes1.prefixb.min0.max7.extra",
        ] {
            assert!(
                known_name_exclusions(plan, &registers, &names).is_empty(),
                "{plan}"
            );
        }
        // Unknown conjuncts do not erase an independently proven mismatch.
        assert_eq!(
            known_name_exclusions("semv.reject.v1:bad@future0,reg1.class1", &registers, &names),
            [(1, 0), (1, 2), (1, 3)]
        );
    }

    #[test]
    fn canonical_member_projection_excludes_field_marker_from_name() {
        assert_eq!(parse_member_projection("member1.fieldW"), Some((1, "W")));
        assert_eq!(parse_member_projection("member1.field"), None);
        assert_eq!(parse_member_projection("member1.W"), None);
    }

    #[test]
    fn member_shape_sequence_preserves_package_field_identity() {
        let mut names = NameTable {
            names: Vec::new(),
            ids: BTreeMap::new(),
            reverse: BTreeMap::new(),
            overflow: false,
        };
        let plan = "semv.sequence.v1:match:_@expr0,member_shape1.fieldWidth;encode:word@expr0;fixup:fix.abs32@target:member1.fieldWidth";
        let CandidateRecipe::SemanticSequence { stages } = super::parse_recipe(plan, &mut names)
        else {
            panic!("member-shape match and member target must lower together");
        };
        let field = names.id("Width");
        assert_eq!(
            stages[0].inputs,
            [
                Projection::Expression(0),
                Projection::MemberShape {
                    operand: 1,
                    qualifier: field,
                },
            ]
        );
        assert_eq!(
            stages[2].inputs,
            [Projection::TargetMember {
                operand: 1,
                qualifier: field,
            }]
        );
        for source in [
            "member_shape.fieldWidth",
            "member_shape1.field",
            "member_shape256.fieldWidth",
            "member_shape1.Width",
            "target:member_shape1.fieldWidth",
        ] {
            assert_eq!(
                super::parse_projection(source, &mut names),
                None,
                "{source}"
            );
        }
        for plan in [
            "semv.sequence.v1:encode:word@target:member1.fieldWidth",
            "semv.sequence.v1:encode:word@expr0;fixup:fix.abs32@member_shape1.fieldWidth",
        ] {
            assert!(matches!(
                super::parse_recipe(plan, &mut names),
                CandidateRecipe::Unsupported { .. }
            ));
        }
    }

    #[test]
    fn member_binding_metadata_survives_unsupported_sequence_steps() {
        let plan = "semv.sequence.v1:match:_@member_shape1.fieldWidth;encode:future@unknown;fixup:fix@target:member1.fieldWidth,member0.fieldAddress|diagnostic";
        assert_eq!(
            super::member_binding_fields(plan),
            BTreeSet::from([(0, "Address"), (1, "Width")])
        );
        let mut names = NameTable {
            names: Vec::new(),
            ids: BTreeMap::new(),
            reverse: BTreeMap::new(),
            overflow: false,
        };
        assert!(matches!(
            super::parse_recipe(plan, &mut names),
            CandidateRecipe::Unsupported { .. }
        ));
        assert_eq!(
            super::member_binding_fields(
                "semv.inputs.v1:encode@value_program:check:member1.fieldWidth,required_value_program:check:member0.fieldAddress"
            ),
            BTreeSet::from([(0, "Address"), (1, "Width")])
        );
        for plan in [
            "semv.inputs.v2:encode@member1.fieldWidth",
            "semv.sequence.v1:future:encode@member1.fieldWidth",
            "semv.inputs.v1:encode@member1.Width,target:member_shape1.fieldWidth,value_program::member0.fieldAddress",
        ] {
            assert!(super::member_binding_fields(plan).is_empty(), "{plan}");
        }
    }

    #[test]
    fn member_exclusions_follow_only_canonical_match_inputs() {
        assert_eq!(
            member_excluded("semv.inputs.v1:enc@indirect_reg1.class1,reg0.class0"),
            0b11
        );
        assert_eq!(
            member_excluded("semv.branch.v1:branch@96,expr0,unary_plus_indirect_reg1.class1,1"),
            0b10
        );
        assert_eq!(
            member_excluded(
                "semv.sequence.v1:match:_@reg0.class0,indirect_tuple_reg1.item1.class1;encode:x@reg1.class0"
            ),
            0b11
        );
        assert_eq!(
            member_excluded("semv.reject.v1:diagnostic@required_value_program:check:reg1.class0"),
            0b10
        );
        assert_eq!(
            member_excluded("semv.inputs.v1:enc@value_program:check:reg0.class0"),
            0b01
        );
        assert_eq!(
            member_excluded(
                "semv.sequence.v1:match:_@indirect_tuple_identity_scale1.item1;encode:x@literal:0"
            ),
            0b10
        );
    }

    #[test]
    fn member_exclusions_stay_conservative_for_unknown_or_later_steps() {
        assert_eq!(member_excluded("semv.inputs.v1:enc@member1.W,expr0"), 0);
        assert_eq!(member_excluded("semv.inputs.v2:enc@reg1.class0"), 0);
        assert_eq!(member_excluded("semv.inputs.v1:enc@unknown_reg1.class0"), 0);
        assert_eq!(
            member_excluded("semv.inputs.v1:enc@indirect_tuple_unknown1.item0"),
            0
        );
        assert_eq!(
            member_excluded("semv.inputs.v1:enc@indirect_tuple_arity1.valueX"),
            0
        );
        assert_eq!(
            member_excluded("semv.inputs.v1:enc@value_program::reg1.class0"),
            0
        );
        assert_eq!(
            member_excluded("semv.sequence.v1:encode:x@reg1.class0;fixup:y@reg0.class0"),
            0
        );
        assert_eq!(
            member_excluded("semv.sequence.v1:match:_@expr0;encode:x@reg1.class0"),
            0
        );
    }
    #[test]
    fn tuple_projections_preserve_bounded_item_indices() {
        let mut names = NameTable {
            names: Vec::new(),
            ids: BTreeMap::new(),
            reverse: BTreeMap::new(),
            overflow: false,
        };
        assert_eq!(
            parse_projection("indirect_tuple_reg0.item0.class1", &mut names),
            Some(Projection::TupleRegister {
                operand: 0,
                item: 0,
                class: 1
            })
        );
        assert!(matches!(
            parse_projection(
                "indirect_tuple_qualified_reg0.item1.qualifierw.class0",
                &mut names
            ),
            Some(Projection::TupleQualifiedRegister {
                operand: 0,
                item: 1,
                class: 0,
                ..
            })
        ));
        for source in [
            "indirect_tuple_reg0.item3.class1",
            "indirect_tuple_reg0.item-1.class1",
            "indirect_tuple_value0.item255",
            "indirect_tuple_qualified_reg0.item3.qualifierw.class0",
        ] {
            assert_eq!(parse_projection(source, &mut names), None, "{source}");
        }
    }

    #[test]
    fn indexed_sequence_keeps_match_and_encoder_inputs_separate() {
        let mut names = NameTable {
            names: Vec::new(),
            ids: BTreeMap::new(),
            reverse: BTreeMap::new(),
            overflow: false,
        };
        let plan = "semv.sequence.v1:match:_@indirect_tuple_reg0.item1.class1,indirect_tuple_qualified_reg0.item2.qualifierw.class0,indirect_tuple_value0.item0,indirect_tuple_arity0.value3;encode:fields@literal:48,indirect_tuple_reg0.item1.class1;encode:index@indirect_tuple_qualified_reg0.item2.qualifierw.class0,literal:0,literal:0,indirect_tuple_value0.item0";
        let super::CandidateRecipe::SemanticSequence { stages } =
            super::parse_recipe(plan, &mut names)
        else {
            panic!("indexed sequence must lower");
        };
        assert_eq!(stages.len(), 3);
        assert_eq!(stages[0].program, None);
        assert_eq!(stages[0].inputs.len(), 4);
        assert_eq!(stages[1].inputs.len(), 2);
        assert_eq!(stages[2].inputs.len(), 4);
        assert!(stages[1].program.is_some());
        assert!(matches!(
            stages[0].inputs[1],
            Projection::TupleQualifiedRegister {
                operand: 0,
                class: 0,
                ..
            }
        ));
        assert_eq!(
            stages[0].inputs[3],
            Projection::TupleArityThree { operand: 0 }
        );
    }

    #[test]
    fn identity_scale_projection_preserves_item_and_match_predicate() {
        let mut names = NameTable {
            names: Vec::new(),
            ids: BTreeMap::new(),
            reverse: BTreeMap::new(),
            overflow: false,
        };
        for item in 0..=2 {
            assert_eq!(
                parse_projection(
                    &format!("indirect_tuple_identity_scale1.item{item}"),
                    &mut names
                ),
                Some(Projection::TupleIdentityScale { operand: 1, item })
            );
        }
        let plan = "semv.sequence.v1:match:_@indirect_tuple_reg0.item0.class1,indirect_tuple_qualified_reg0.item1.qualifierW.class0,indirect_tuple_identity_scale0.item1,indirect_tuple_arity0.value2;encode:fields@literal:48,indirect_tuple_reg0.item0.class1;encode:index@indirect_tuple_qualified_reg0.item1.qualifierW.class0,literal:0,literal:0,literal:0";
        let super::CandidateRecipe::SemanticSequence { stages } =
            super::parse_recipe(plan, &mut names)
        else {
            panic!("canonical identity-scale sequence must lower");
        };
        assert_eq!(
            stages[0].inputs[2],
            Projection::TupleIdentityScale {
                operand: 0,
                item: 1
            }
        );
        assert_eq!(stages[0].inputs[3], Projection::TupleArity { operand: 0 });
        assert_eq!(stages[0].program, None);
        assert!(stages[1..].iter().all(|stage| stage
            .inputs
            .iter()
            .all(|input| { !matches!(input, Projection::TupleIdentityScale { .. }) })));
        for source in [
            "indirect_tuple_identity_scale0.item3",
            "indirect_tuple_identity_scale0.item-1",
            "indirect_tuple_identity_scale0.itemany",
            "indirect_tuple_identity_scale0.item1.left",
            "indirect_tuple_nonidentity_scale0.item1",
        ] {
            assert_eq!(parse_projection(source, &mut names), None, "{source}");
            assert!(
                matches!(
                    super::parse_recipe(
                        &format!("semv.sequence.v1:match:_@{source};encode:x@literal:0"),
                        &mut names
                    ),
                    super::CandidateRecipe::Unsupported { .. }
                ),
                "{source}"
            );
        }
    }

    #[test]
    fn bounded_sequences_reject_unimplemented_or_unsafe_stages() {
        let over_stages = format!("semv.sequence.v1:{}", ["encode:x@expr0"; 9].join(";"));
        let over_inputs = format!("semv.sequence.v1:encode:x@{}", ["expr0"; 17].join(","));
        for plan in [
            "semv.sequence.v1:match:named@expr0;encode:x@expr0",
            "semv.sequence.v1:encode:x@expr0;match:_@expr0",
            "semv.sequence.v1:match:_@expr0",
            "semv.sequence.v1:encode:x@expr0;fixup:y@literal:0",
            "semv.sequence.v1:encode:x@literal:0;fixup:y@indirect_tuple_value0.item0",
            "semv.sequence.v1:match:_@indirect_tuple_reg0.item1.class1,indirect_tuple_arity0.value3;encode:x@literal:0;fixup:y@indirect_tuple_value0.item0",
            "semv.sequence.v1:match:_@indirect_tuple_reg1.item1.class1,indirect_tuple_arity0.value2;encode:x@literal:0;fixup:y@indirect_tuple_value0.item0",
            "semv.sequence.v1:fixup:y@target:expr0",
            "semv.sequence.v1:encode:x@target:expr0;fixup:y@target:expr0",
            "semv.sequence.v1:encode:x@expr0;fixup:y@target:expr2.more",
            "semv.sequence.v1:encode:x@expr0;unknown:y@expr0",
            "semv.sequence.v1:encode:x@",
            &over_stages,
            &over_inputs,
        ] {
            assert!(
                matches!(
                    super::parse_recipe(
                        plan,
                        &mut NameTable {
                            names: Vec::new(),
                            ids: BTreeMap::new(),
                            reverse: BTreeMap::new(),
                            overflow: false
                        }
                    ),
                    CandidateRecipe::Unsupported { .. }
                ),
                "unexpected lowering: {plan}"
            );
        }
        let within_bounds = format!("semv.sequence.v1:{}", ["encode:x@expr0"; 8].join(";"));
        assert!(matches!(
            super::parse_recipe(
                &within_bounds,
                &mut NameTable {
                    names: Vec::new(),
                    ids: BTreeMap::new(),
                    reverse: BTreeMap::new(),
                    overflow: false
                }
            ),
            CandidateRecipe::SemanticSequence { .. }
        ));
    }

    #[test]
    fn scalar_expression_fixup_sequence_lowers_both_operands() {
        let mut names = NameTable {
            names: Vec::new(),
            ids: BTreeMap::new(),
            reverse: BTreeMap::new(),
            overflow: false,
        };
        let plan = "semv.sequence.v1:match:_@expr0,expr1;encode:word@literal:9212;fixup:fix.abs32@expr0;fixup:fix.abs32@expr1";
        let CandidateRecipe::SemanticSequence { stages } = super::parse_recipe(plan, &mut names)
        else {
            panic!("scalar expression fixup sequence must lower");
        };
        assert_eq!(stages.len(), 4);
        assert_eq!(
            stages[0].inputs,
            [Projection::Expression(0), Projection::Expression(1)]
        );
        assert_eq!(stages[1].inputs, [Projection::Constant(9212)]);
        for (operand, stage) in stages[2..].iter().enumerate() {
            assert!(stage.fixup);
            assert_eq!(stage.inputs, [Projection::Expression(operand as u8)]);
        }
    }

    #[test]
    fn target_fixup_sequence_preserves_stage_and_projection_kinds() {
        let mut names = NameTable {
            names: Vec::new(),
            ids: BTreeMap::new(),
            reverse: BTreeMap::new(),
            overflow: false,
        };
        let plan = "semv.sequence.v1:match:_@target:expr0;encode:word@literal:20081;fixup:fix.abs32@target:expr0";
        let CandidateRecipe::SemanticSequence { stages } = super::parse_recipe(plan, &mut names)
        else {
            panic!("target fixup sequence must lower");
        };
        assert_eq!(stages.len(), 3);
        assert!(!stages[0].fixup);
        assert!(!stages[1].fixup);
        assert!(stages[2].fixup);
        assert_eq!(stages[0].inputs, vec![Projection::TargetExpression(0)]);
        assert_eq!(stages[2].inputs, vec![Projection::TargetExpression(0)]);
        assert!(matches!(
            super::parse_projection("target:member1.fieldL", &mut names),
            Some(Projection::TargetMember { operand: 1, .. })
        ));
    }

    #[test]
    fn target_fixup_sequence_ignores_diagnostic_suffix() {
        let mut names = NameTable {
            names: Vec::new(),
            ids: BTreeMap::new(),
            reverse: BTreeMap::new(),
            overflow: false,
        };
        let plan = "semv.sequence.v1:match:_@expr0,target:expr1;encode:enc.template.field-9@literal:20665,required_value_program:scalar.packed-three-bit-count:expr0;fixup:fix.abs32@target:expr1|encoding.count.range";
        let CandidateRecipe::SemanticSequence { stages } = super::parse_recipe(plan, &mut names)
        else {
            panic!("diagnostic suffix must not obscure an executable fixup sequence");
        };
        assert_eq!(stages.len(), 3);
        assert_eq!(stages[2].inputs, [Projection::TargetExpression(1)]);
    }

    #[test]
    fn atomic_target_match_keeps_its_distinct_projection() {
        let mut names = NameTable {
            names: Vec::new(),
            ids: BTreeMap::new(),
            reverse: BTreeMap::new(),
            overflow: false,
        };
        let plan = "semv.sequence.v1:match:_@target_atom:expr0,reg1.class0;encode:enc.template.field-9@literal:8252,reg1.class0;fixup:fix.abs32@target:expr0";
        let CandidateRecipe::SemanticSequence { stages } = super::parse_recipe(plan, &mut names)
        else {
            panic!("atomic symbolic target must lower as a bounded sequence");
        };
        assert_eq!(stages[0].inputs[0], Projection::AtomicTargetExpression(0));
        assert_eq!(stages[2].inputs, [Projection::TargetExpression(0)]);
    }

    #[test]
    fn m68020_branch_candidates_preserve_auto_and_explicit_requests() {
        let mut registry = registry::ModuleRegistry::new();
        families::register_motorola68000_family_stack(&mut registry);
        let core = super::RuntimeModelCore::from_registry(&registry).unwrap();
        let resolved = core.resolve_pipeline("m68020", None).unwrap();
        let package = super::BinarySourcePackage::prepare(&core, &resolved).unwrap();
        let branch = |qualifier: Option<&str>| {
            package
                .candidates
                .iter()
                .find(|candidate| {
                    package.names[usize::from(candidate.mnemonic)] == "beq"
                        && candidate
                            .qualifier
                            .map(|index| &package.qualifiers[usize::from(index)][..])
                            == qualifier
                        && matches!(candidate.recipe, CandidateRecipe::SemanticBranch { .. })
                })
                .expect("BEQ candidate")
        };
        assert!(branch(None).unstable_widen);
        assert!(
            matches!(&branch(None).recipe, CandidateRecipe::SemanticBranch { inputs, .. }
            if inputs == &[Projection::Constant(103), Projection::Expression(0),
                Projection::Constant(-1), Projection::Constant(1)])
        );
        assert!(
            matches!(&branch(Some("w")).recipe, CandidateRecipe::SemanticBranch { inputs, .. }
            if inputs == &[Projection::Constant(103), Projection::Expression(0),
                Projection::Constant(1), Projection::Constant(0)])
        );
        assert!(
            matches!(&branch(Some("s")).recipe, CandidateRecipe::SemanticBranch { inputs, .. }
            if inputs == &[Projection::Constant(103), Projection::Expression(0),
                Projection::Constant(0), Projection::Constant(0)])
        );
    }

    #[test]
    fn auto_request_is_only_valid_in_the_branch_candidate_field() {
        let mut names = NameTable {
            names: Vec::new(),
            ids: BTreeMap::new(),
            reverse: BTreeMap::new(),
            overflow: false,
        };
        for plan in [
            "semv.branch.v1:branch.sized@auto,expr0,1,0",
            "semv.branch.v1:branch.sized@103,auto,1,0",
            "semv.branch.v1:branch.sized@103,expr0,1,auto",
            "semv.branch.v1:branch.sized@103,expr0,auto",
            "semv.inputs.v1:other@auto",
        ] {
            assert!(
                matches!(
                    super::parse_recipe(plan, &mut names),
                    CandidateRecipe::Unsupported { .. }
                ),
                "{plan}"
            );
        }
    }

    #[test]
    fn bare_lea_symbol_candidate_is_an_executable_sequence() {
        let mut registry = registry::ModuleRegistry::new();
        families::register_motorola68000_family_stack(&mut registry);
        let core = super::RuntimeModelCore::from_registry(&registry).unwrap();
        let resolved = core.resolve_pipeline("m68020", None).unwrap();
        let package = super::BinarySourcePackage::prepare(&core, &resolved).unwrap();
        assert!(package.candidates.iter().any(|candidate| {
            candidate.priority == 78
                && package.names[usize::from(candidate.mnemonic)] == "lea"
                && matches!(&candidate.recipe, CandidateRecipe::SemanticSequence { stages }
                    if stages.iter().any(|stage| stage.fixup
                        && stage.inputs == [Projection::TargetExpression(0)]))
        }));
    }

    #[test]
    fn qualified_immediate_symbol_candidate_is_an_executable_sequence() {
        let mut registry = registry::ModuleRegistry::new();
        families::register_motorola68000_family_stack(&mut registry);
        let core = super::RuntimeModelCore::from_registry(&registry).unwrap();
        let resolved = core.resolve_pipeline("m68020", None).unwrap();
        let package = super::BinarySourcePackage::prepare(&core, &resolved).unwrap();
        let field = package.names.iter().position(|name| name == "l").unwrap() as u16;
        let mnemonic = package
            .names
            .iter()
            .position(|name| name == "cmpi")
            .unwrap() as u16;
        let qualifier = package
            .qualifiers
            .iter()
            .position(|name| name == "w")
            .unwrap() as u8;
        assert!(package
            .member_bindings
            .contains(&super::NumericMemberBinding {
                mnemonic,
                qualifier: Some(qualifier),
                operand: 1,
                field,
            }));
        assert!(package
            .member_bindings
            .windows(2)
            .all(|pair| pair[0] < pair[1]));
        assert!(package.candidates.iter().any(|candidate| {
            candidate.priority == 325
                && package.names[usize::from(candidate.mnemonic)] == "cmpi"
                && candidate
                    .qualifier
                    .is_some_and(|qualifier| package.qualifiers[usize::from(qualifier)] == "w")
                && matches!(&candidate.recipe, CandidateRecipe::SemanticSequence { stages }
                if stages.len() == 3 && stages[0].inputs.contains(&Projection::MemberShape {
                    operand: 1, qualifier: field,
                }) && stages[2].fixup
                    && stages[2].inputs == [Projection::TargetMember {
                        operand: 1, qualifier: field,
                    }])
        }));
    }
}
