//! Numeric package view used by the experimental binary-source frontend.

use std::collections::{BTreeMap, BTreeSet};

use package::{ModeSelectorDescriptor, MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR};
use types::hierarchy::ResolvedHierarchy;

use crate::runtime_model_core::RuntimeModelCore;
use crate::selector_vm::PortableSelectorOutcome;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BinarySourcePackage {
    pub names: Vec<String>,
    pub qualifiers: Vec<String>,
    pub aliases: Vec<NumericAlias>,
    pub registers: Vec<NumericRegister>,
    pub table_programs: Vec<NumericTableProgram>,
    pub semantic_programs: Vec<NumericProgram>,
    pub value_programs: Vec<NumericProgram>,
    pub candidates: Vec<NumericCandidate>,
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
    Unsupported {
        plan: u16,
    },
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
    Register {
        operand: u8,
        class: u16,
    },
    Member {
        operand: u8,
        qualifier: u16,
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
                    candidates.push(candidate(
                        row,
                        mnemonic,
                        shape,
                        rank as u8,
                        &mut names,
                        &mut qualifiers,
                    ));
                }
            }
        }
        registers.sort_by_key(|row| (row.owner_rank, row.name));
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
            names: names.finish(),
            qualifiers: qualifiers.finish(),
            aliases,
            registers,
            table_programs,
            semantic_programs,
            value_programs,
            candidates,
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
        mnemonic: names.id(base),
        qualifier: qualifier.map(|value| qualifiers.id(value)),
        shape: names.core_id(shape),
        mode: names.id(&row.mode_key),
        owner_rank,
        priority: row.priority,
        width_rank: row.width_rank,
        unstable_widen: row.unstable_widen,
        member_excluded: member_excluded(&row.operand_plan),
        recipe: parse_recipe(&row.operand_plan, names),
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
        ("value", [item]) => numbered(item, "item"),
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
    let inputs = inputs
        .split('|')
        .next()
        .unwrap_or(inputs)
        .split(',')
        .map(|value| parse_projection(value, names))
        .collect::<Option<Vec<_>>>();
    let Some(inputs) = inputs else {
        return CandidateRecipe::Unsupported {
            plan: names.id(plan),
        };
    };
    let program = names.id(program);
    if branch {
        CandidateRecipe::SemanticBranch { program, inputs }
    } else {
        CandidateRecipe::SemanticInputs { program, inputs }
    }
}

fn parse_projection(value: &str, names: &mut NameTable) -> Option<Projection> {
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
    if let Some(rest) = value.strip_prefix("expr") {
        return rest.parse().ok().map(Projection::Expression);
    }
    if let Some(rest) = value.strip_prefix("reg") {
        let (operand, class) = rest.split_once(".class")?;
        return Some(Projection::Register {
            operand: operand.parse().ok()?,
            class: class.parse().ok()?,
        });
    }
    if let Some((operand, qualifier)) = parse_member_projection(value) {
        return Some(Projection::Member {
            operand,
            qualifier: names.id(qualifier),
        });
    }
    None
}

fn parse_member_projection(value: &str) -> Option<(u8, &str)> {
    let rest = value.strip_prefix("member")?;
    let (operand, qualifier) = rest.split_once(MODE_SELECTOR_PLAN_MEMBER_FIELD_SEPARATOR)?;
    if operand.is_empty()
        || !operand.bytes().all(|byte| byte.is_ascii_digit())
        || qualifier.is_empty()
    {
        return None;
    }
    Some((operand.parse().ok()?, qualifier))
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
    use super::{member_excluded, parse_member_projection};

    #[test]
    fn canonical_member_projection_excludes_field_marker_from_name() {
        assert_eq!(parse_member_projection("member1.fieldW"), Some((1, "W")));
        assert_eq!(parse_member_projection("member1.field"), None);
        assert_eq!(parse_member_projection("member1.W"), None);
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
}
