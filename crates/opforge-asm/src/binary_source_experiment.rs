//! Provisional native binary-source package wire format. No source or CPU semantics
//! are inferred here: selection metadata and executable bodies come from the package.

use std::collections::{BTreeMap, BTreeSet};

use types::hierarchy::ResolvedHierarchy;
use vm::binary_source_package::{
    BinarySourcePackage, CandidateRecipe, NumericCandidate, Projection, ScalarPlan,
};
use vm::runtime_model_core::RuntimeModelCore;

const MISSING: u16 = u16::MAX;
const HEADER: usize = 76;
const ROW: usize = 32;

struct Program<'a> {
    kind: u16,
    version: u16,
    bytes: &'a [u8],
}

#[derive(Default)]
struct Programs<'a> {
    rows: Vec<Program<'a>>,
    tables: BTreeMap<(u16, Option<u8>, u16), u16>,
    semantics: BTreeMap<u16, u16>,
    values: BTreeMap<u16, u16>,
}

impl<'a> Programs<'a> {
    fn add(&mut self, kind: u16, version: u16, bytes: &'a [u8]) -> Result<u16, String> {
        let id = word(self.rows.len())?;
        if id == MISSING {
            return Err("binary-source program index exhausted".into());
        }
        self.rows.push(Program {
            kind,
            version,
            bytes,
        });
        Ok(id)
    }

    fn prepare(package: &'a BinarySourcePackage) -> Result<Self, String> {
        let mut result = Self::default();
        for row in &package.table_programs {
            let key = (row.mnemonic, row.qualifier, row.mode);
            if !result.tables.contains_key(&key) {
                let id = result.add(1, 1, &row.bytes)?;
                result.tables.insert(key, id);
            }
        }
        for row in &package.semantic_programs {
            if !result.semantics.contains_key(&row.name) {
                let id = result.add(2, row.version, &row.bytes)?;
                result.semantics.insert(row.name, id);
            }
        }
        for row in &package.value_programs {
            if !result.values.contains_key(&row.name) {
                let id = result.add(3, row.version, &row.bytes)?;
                result.values.insert(row.name, id);
            }
        }
        Ok(result)
    }
}

/// Prepare a self-contained BSP3 block for one resolved package hierarchy.
/// Offsets and lengths are big-endian and relative to the block start.
/// Unsupported candidate recipes remain explicit rows, never silent omissions.
pub fn prepare_package(
    core: &RuntimeModelCore,
    resolved: &ResolvedHierarchy,
) -> Result<Vec<u8>, String> {
    let package = BinarySourcePackage::prepare(core, resolved)?;
    let mut names = package.names.clone();
    let mut dictionary = BTreeMap::new();
    for alias in &package.aliases {
        let mut spelling = name(&names, alias.spelling)?.to_string();
        if let Some(q) = alias.spelling_qualifier {
            spelling.push('.');
            spelling.push_str(
                package
                    .qualifiers
                    .get(usize::from(q))
                    .ok_or("invalid spelling qualifier")?,
            );
        }
        bind(
            &mut dictionary,
            spelling,
            alias.mnemonic,
            qualifier(alias.qualifier)?,
        )?;
    }
    let mut registers = BTreeMap::new();
    for row in &package.registers {
        registers.entry(row.name).or_insert((row.class, row.index));
        bind(&mut dictionary, name(&names, row.name)?.into(), row.name, 0)?;
    }
    for row in &package.candidates {
        if let CandidateRecipe::SemanticInputs { inputs, .. }
        | CandidateRecipe::SemanticBranch { inputs, .. } = &row.recipe
        {
            for input in inputs {
                bind_member(input, &names, &mut dictionary)?;
            }
        }
    }
    let mut directive_ids = Vec::new();
    for directive in ["cpu", "org", "byte", "word", "long", "end"] {
        let id = intern(&mut names, directive)?;
        bind(&mut dictionary, directive.into(), id, 0)?;
        directive_ids.push(id);
    }
    let cpu_id = intern(&mut names, &resolved.cpu_id)?;
    bind(
        &mut dictionary,
        resolved.cpu_id.to_ascii_lowercase(),
        cpu_id,
        0,
    )?;
    for (spelling, _, _) in core.supported_cpus() {
        if core
            .canonical_cpu_id_for_input(&spelling)
            .is_some_and(|id| id.eq_ignore_ascii_case(&resolved.cpu_id))
        {
            bind(&mut dictionary, spelling.to_ascii_lowercase(), cpu_id, 0)?;
        }
    }
    let properties = core
        .cpu_execution_properties(&resolved.cpu_id)
        .map_err(|e| e.to_string())?
        .ok_or("binary-source package requires explicit CPU execution properties")?;
    let tokenizer = core
        .tokenizer_vm_program_for_resolved(resolved)
        .ok_or("binary-source package has no tokenizer")?;
    if tokenizer.opcode_version != 1 {
        return Err("binary-source tokenizer opcode version unsupported".into());
    }
    let programs = Programs::prepare(&package)?;
    let mut candidates = package.candidates.clone();
    let mut implied = BTreeSet::new();
    for table in &package.table_programs {
        if name(&names, table.mode)? != "implied"
            || !implied.insert((table.mnemonic, table.qualifier, table.mode))
        {
            continue;
        }
        if candidates.iter().any(|row| {
            row.mnemonic == table.mnemonic
                && row.qualifier == table.qualifier
                && row.mode == table.mode
        }) {
            continue;
        }
        let shape = intern(&mut names, "implied")?;
        candidates.push(NumericCandidate {
            mnemonic: table.mnemonic,
            qualifier: table.qualifier,
            shape,
            mode: table.mode,
            owner_rank: table.owner_rank,
            priority: 0,
            width_rank: 0,
            unstable_widen: false,
            member_excluded: 0,
            known_name_excluded: Vec::new(),
            recipe: CandidateRecipe::None,
        });
    }
    let total_names = word(names.len())?;
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
    let mut out = vec![0; HEADER];
    out[..4].copy_from_slice(b"BSP3");
    let rows_offset = out.len();
    reserve(&mut out, candidates.len(), ROW)?;
    let registers_offset = out.len();
    for (name, (class, index)) in &registers {
        for value in [*name, *class, *index] {
            push_word(&mut out, value);
        }
    }
    let programs_offset = out.len();
    reserve(&mut out, programs.rows.len(), 12)?;
    let mut exclusions = BTreeMap::new();
    for (index, candidate) in candidates.iter().enumerate() {
        write_candidate(
            &mut out,
            rows_offset + index * ROW,
            candidate,
            &names,
            &programs,
        )?;
        // This runtime has at most two operands. Predicates on other operands
        // cannot disprove a candidate here and remain unsupported barriers.
        let supported_exclusions: Vec<_> = candidate
            .known_name_excluded
            .iter()
            .copied()
            .filter(|(operand, _)| *operand < 2)
            .collect();
        if !supported_exclusions.is_empty() {
            let offset = if let Some(offset) = exclusions.get(&supported_exclusions) {
                *offset
            } else {
                let offset = long(out.len())?;
                push_word(&mut out, word(supported_exclusions.len())?);
                for (operand, name) in &supported_exclusions {
                    push_word(&mut out, u16::from(*operand));
                    push_word(&mut out, *name);
                }
                exclusions.insert(supported_exclusions, offset);
                offset
            };
            set_long(&mut out, rows_offset + index * ROW + 24, offset);
        }
    }
    for (index, program) in programs.rows.iter().enumerate() {
        let offset = out.len();
        out.extend_from_slice(program.bytes);
        align(&mut out);
        let row = programs_offset + index * 12;
        set_word(&mut out, row, program.kind);
        set_word(&mut out, row + 2, program.version);
        set_long(&mut out, row + 4, long(offset)?);
        set_long(&mut out, row + 8, long(program.bytes.len())?);
    }
    align(&mut out);
    let runtime_bytes = long(out.len())?;
    let dictionary_offset = out.len();
    for (spelling, (id, q)) in &dictionary {
        push_word(&mut out, word(spelling.len())?);
        push_word(&mut out, *id);
        out.extend_from_slice(&[*q, 0]);
        out.extend_from_slice(spelling.as_bytes());
        align(&mut out);
    }
    let tokenizer_offset = out.len();
    let state_count = word(tokenizer.state_entry_offsets.len())?;
    if state_count == 0
        || tokenizer.start_state >= state_count
        || tokenizer
            .state_entry_offsets
            .iter()
            .any(|offset| *offset as usize >= tokenizer.program.len())
    {
        return Err("binary-source tokenizer state table is invalid".into());
    }
    push_word(&mut out, tokenizer.opcode_version);
    push_word(&mut out, tokenizer.start_state);
    push_word(&mut out, state_count);
    push_word(&mut out, 0);
    out.extend_from_slice(&tokenizer.limits.max_steps_per_line.to_be_bytes());
    for offset in &tokenizer.state_entry_offsets {
        out.extend_from_slice(&offset.to_be_bytes());
    }
    out.extend_from_slice(&tokenizer.program);
    let tokenizer_length = out.len() - tokenizer_offset;
    align(&mut out);
    let total = long(out.len())?;
    for (offset, value) in [
        (4, total),
        (8, long(dictionary_offset)?),
        (12, long(dictionary.len())?),
        (16, long(rows_offset)?),
        (20, long(candidates.len())?),
        (24, long(registers_offset)?),
        (28, long(registers.len())?),
        (32, long(programs_offset)?),
        (36, long(programs.rows.len())?),
        (40, long(tokenizer_offset)?),
        (44, long(tokenizer_length)?),
        (68, properties.max_program_address),
        (72, runtime_bytes),
    ] {
        set_long(&mut out, offset, value);
    }
    for (index, id) in directive_ids.iter().enumerate() {
        set_word(&mut out, 48 + index * 2, *id);
    }
    set_word(&mut out, 60, cpu_id);
    set_word(&mut out, 62, total_names);
    set_word(&mut out, 64, u16::from(properties.data_little_endian));
    Ok(out)
}

fn write_candidate(
    out: &mut Vec<u8>,
    row: usize,
    candidate: &NumericCandidate,
    names: &[String],
    programs: &Programs<'_>,
) -> Result<(), String> {
    let table = programs
        .tables
        .get(&(candidate.mnemonic, candidate.qualifier, candidate.mode))
        .copied()
        .unwrap_or(MISSING);
    let (mut recipe, program, inputs) = match &candidate.recipe {
        CandidateRecipe::None => (0, table, &[][..]),
        CandidateRecipe::Scalar(plan) => (
            match plan {
                ScalarPlan::U8 => 1,
                ScalarPlan::U16 => 2,
                ScalarPlan::Rel8 => 3,
            },
            table,
            &[][..],
        ),
        CandidateRecipe::SemanticInputs { program, inputs } => (
            4,
            programs.semantics.get(program).copied().unwrap_or(MISSING),
            inputs.as_slice(),
        ),
        CandidateRecipe::SemanticBranch { program, inputs } => (
            5,
            programs.semantics.get(program).copied().unwrap_or(MISSING),
            inputs.as_slice(),
        ),
        CandidateRecipe::Unsupported { .. } => (6, MISSING, &[][..]),
    };
    // Only an exact identity TABL may be elided. SEMV normally supplies the
    // operand payload; TABL still owns the surrounding instruction bytes.
    let identity_table = programs
        .rows
        .get(usize::from(table))
        .is_some_and(|row| row.bytes == [vm::bytecode::OP_EMIT_OPERAND, 0, vm::bytecode::OP_END]);
    if recipe == 4 && !identity_table {
        recipe = if table == MISSING { 6 } else { 7 };
    }
    if recipe == 5 && !identity_table {
        recipe = 6;
    }
    let shape = match name(names, candidate.shape)? {
        "implied" => 0,
        "direct" => 1,
        "immediate" => 2,
        "immediate_register" => 3,
        "register_direct" => 4,
        "register_register" => 5,
        "direct_register" => 6,
        _ => 255,
    };
    if program == MISSING || shape == 255 {
        recipe = 6;
    }
    let projection_start = out.len();
    if recipe != 6 {
        for projection in inputs {
            if !write_projection(out, projection, programs)? {
                out.truncate(projection_start);
                recipe = 6;
                break;
            }
        }
    }
    set_word(out, row, candidate.mnemonic);
    out[row + 2] = qualifier(candidate.qualifier)?;
    out[row + 3] = shape;
    out[row + 4] = candidate.owner_rank;
    out[row + 5] = recipe;
    set_word(out, row + 6, candidate.priority);
    set_word(out, row + 8, program);
    set_word(
        out,
        row + 10,
        if recipe == 6 { 0 } else { word(inputs.len())? },
    );
    set_long(
        out,
        row + 12,
        if recipe == 6 || inputs.is_empty() {
            0
        } else {
            long(projection_start)?
        },
    );
    out[row + 16] = candidate.width_rank;
    out[row + 17] = u8::from(candidate.unstable_widen);
    out[row + 18] = candidate.member_excluded;
    set_word(out, row + 20, candidate.mode);
    set_word(out, row + 28, if recipe == 7 { table } else { MISSING });
    Ok(())
}

fn write_projection(
    out: &mut Vec<u8>,
    projection: &Projection,
    programs: &Programs<'_>,
) -> Result<bool, String> {
    let (projection, value_program) = match projection {
        Projection::RequiredValueProgram { program, source } => {
            let Some(index) = programs.values.get(program) else {
                return Ok(false);
            };
            (source.as_ref(), *index)
        }
        projection => (projection, MISSING),
    };
    let (kind, operand, field, literal) = match projection {
        Projection::Expression(operand) => (0, *operand, 0, 0),
        Projection::Register { operand, class } => (1, *operand, *class, 0),
        Projection::Member { operand, qualifier } => (2, *operand, *qualifier, 0),
        Projection::NamedRegister { operand, name } => (4, *operand, *name, 0),
        Projection::Constant(value) => {
            let Ok(value) = i32::try_from(*value) else {
                return Ok(false);
            };
            (3, 0, 0, value)
        }
        Projection::RequiredValueProgram { .. } => return Ok(false),
    };
    out.extend_from_slice(&[kind, operand]);
    push_word(out, field);
    out.extend_from_slice(&literal.to_be_bytes());
    push_word(out, value_program);
    push_word(out, 0);
    Ok(true)
}

fn bind_member(
    projection: &Projection,
    names: &[String],
    dictionary: &mut BTreeMap<String, (u16, u8)>,
) -> Result<(), String> {
    match projection {
        Projection::NamedRegister {
            name: qualifier, ..
        }
        | Projection::Member { qualifier, .. } => {
            bind(dictionary, name(names, *qualifier)?.into(), *qualifier, 0)
        }
        Projection::RequiredValueProgram { source, .. } => bind_member(source, names, dictionary),
        _ => Ok(()),
    }
}
fn bind(
    dictionary: &mut BTreeMap<String, (u16, u8)>,
    spelling: String,
    id: u16,
    qualifier: u8,
) -> Result<(), String> {
    let spelling = spelling.to_ascii_lowercase();
    if let Some(previous) = dictionary.insert(spelling.clone(), (id, qualifier)) {
        if previous != (id, qualifier) {
            return Err(format!("conflicting binary lexical binding for {spelling}"));
        }
    }
    Ok(())
}
fn name(names: &[String], id: u16) -> Result<&str, String> {
    names
        .get(usize::from(id))
        .map(String::as_str)
        .ok_or_else(|| "binary package name index out of bounds".into())
}
fn intern(names: &mut Vec<String>, value: &str) -> Result<u16, String> {
    if let Some(index) = names
        .iter()
        .position(|name| name.eq_ignore_ascii_case(value))
    {
        return word(index);
    }
    let id = word(names.len())?;
    if id == MISSING {
        return Err("binary package name space exhausted".into());
    }
    names.push(value.to_ascii_lowercase());
    Ok(id)
}
fn qualifier(value: Option<u8>) -> Result<u8, String> {
    value.map_or(Ok(0), |value| {
        value
            .checked_add(1)
            .ok_or_else(|| "binary qualifier overflow".into())
    })
}
fn word(value: usize) -> Result<u16, String> {
    u16::try_from(value).map_err(|_| "binary package u16 overflow".into())
}
fn long(value: usize) -> Result<u32, String> {
    u32::try_from(value).map_err(|_| "binary package u32 overflow".into())
}
fn push_word(out: &mut Vec<u8>, value: u16) {
    out.extend_from_slice(&value.to_be_bytes());
}
fn set_word(out: &mut [u8], offset: usize, value: u16) {
    out[offset..offset + 2].copy_from_slice(&value.to_be_bytes());
}
fn set_long(out: &mut [u8], offset: usize, value: u32) {
    out[offset..offset + 4].copy_from_slice(&value.to_be_bytes());
}
fn align(out: &mut Vec<u8>) {
    if !out.len().is_multiple_of(2) {
        out.push(0);
    }
}
fn reserve(out: &mut Vec<u8>, count: usize, width: usize) -> Result<(), String> {
    let end = count
        .checked_mul(width)
        .and_then(|size| out.len().checked_add(size))
        .ok_or("binary package size overflow")?;
    long(end)?;
    out.resize(end, 0);
    Ok(())
}
