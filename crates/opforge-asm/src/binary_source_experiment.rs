//! Provisional native binary-source package wire format. No source or CPU semantics
//! are inferred here: selection metadata and executable bodies come from the package.

use std::collections::{BTreeMap, BTreeSet};

use package::{decode_encoding_program, EncodingStep};
use types::hierarchy::ResolvedHierarchy;
use vm::binary_source_package::{
    BinarySourcePackage, CandidateRecipe, NumericCandidate, Projection, ScalarPlan, SemanticStage,
};
use vm::runtime_model_core::RuntimeModelCore;

const MISSING: u16 = u16::MAX;
const HEADER: usize = 80;
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
    qualifiers: BTreeMap<u16, u8>,
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
        for (index, spelling) in package.qualifiers.iter().enumerate() {
            for (id, name) in package.names.iter().enumerate() {
                if name.eq_ignore_ascii_case(spelling) {
                    result.qualifiers.insert(
                        word(id)?,
                        qualifier(Some(u8::try_from(index).map_err(|_| "qualifier overflow")?))?,
                    );
                }
            }
        }
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
    // The alias table does not enumerate every canonical qualified spelling
    // (for example, bsr.w). Explicit aliases retain precedence where present.
    for candidate in &package.candidates {
        let Some(q) = candidate.qualifier else {
            continue;
        };
        let mut spelling = name(&names, candidate.mnemonic)?.to_string();
        spelling.push('.');
        spelling.push_str(
            package
                .qualifiers
                .get(usize::from(q))
                .ok_or("invalid candidate qualifier")?,
        );
        if !dictionary.contains_key(&spelling.to_ascii_lowercase()) {
            bind(
                &mut dictionary,
                spelling,
                candidate.mnemonic,
                qualifier(Some(q))?,
            )?;
        }
    }
    let mut qualified_classes = BTreeSet::new();
    for candidate in &package.candidates {
        match &candidate.recipe {
            CandidateRecipe::SemanticInputs { inputs, .. }
            | CandidateRecipe::SemanticBranch { inputs, .. } => {
                for input in inputs {
                    collect_qualified_classes(input, &mut qualified_classes);
                }
            }
            CandidateRecipe::SemanticSequence { stages } => {
                for stage in stages {
                    for input in &stage.inputs {
                        collect_qualified_classes(input, &mut qualified_classes);
                    }
                }
            }
            _ => {}
        }
    }
    let mut registers = BTreeMap::new();
    for row in &package.registers {
        registers.entry(row.name).or_insert((row.class, row.index));
        bind(&mut dictionary, name(&names, row.name)?.into(), row.name, 0)?;
        for (class, qualifier_name) in &qualified_classes {
            if row.class != *class {
                continue;
            }
            let suffix = name(&names, *qualifier_name)?;
            let Some(index) = package
                .qualifiers
                .iter()
                .position(|value| value.eq_ignore_ascii_case(suffix))
            else {
                continue;
            };
            bind(
                &mut dictionary,
                format!("{}.{suffix}", name(&names, row.name)?),
                row.name,
                qualifier(Some(u8::try_from(index).map_err(|_| "qualifier overflow")?))?,
            )?;
        }
    }
    for row in &package.candidates {
        if let CandidateRecipe::SemanticSequence { stages } = &row.recipe {
            for stage in stages {
                for input in &stage.inputs {
                    bind_member(input, &names, &mut dictionary)?;
                }
            }
        }
        if let CandidateRecipe::SemanticInputs { inputs, .. }
        | CandidateRecipe::SemanticBranch { inputs, .. } = &row.recipe
        {
            for input in inputs {
                bind_member(input, &names, &mut dictionary)?;
            }
        }
    }
    let mut directive_ids = Vec::new();
    for directive in ["cpu", "org", "byte", "word", "long", "end", "align", "res"] {
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
        let offset = if index < 6 {
            48 + index * 2
        } else {
            76 + (index - 6) * 2
        };
        set_word(&mut out, offset, *id);
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
        CandidateRecipe::SemanticSequence { .. } => (9, MISSING, &[][..]),
        CandidateRecipe::PackedMaskIndirect { .. } => (8, MISSING, &[][..]),
        CandidateRecipe::Unsupported { .. } => (6, MISSING, &[][..]),
    };
    // Only an exact identity TABL may be elided. A semantic program without
    // TABL must itself start by emitting an opcode, not just operand payload.
    let identity_table = programs
        .rows
        .get(usize::from(table))
        .is_some_and(|row| row.bytes == [vm::bytecode::OP_EMIT_OPERAND, 0, vm::bytecode::OP_END]);
    if recipe == 4 && !identity_table {
        recipe = if table != MISSING {
            7
        } else if !semantic_emits_opcode(programs, program) {
            6
        } else {
            4
        };
    }
    if recipe == 5 && !identity_table {
        recipe = 6;
    }
    let shape = match name(names, candidate.shape)? {
        "implied" => 0,
        "direct" => 1,
        "immediate" => 2,
        "immediate_register" => 3,
        "immediate_direct" => 8,
        "register" => 9,
        "direct_direct" => 10,
        "register_direct" => 4,
        "register_register" => 5,
        "direct_register" => 6,
        _ => 255,
    };
    let shape = if recipe == 8 { 7 } else { shape };
    if (program == MISSING && recipe != 8 && recipe != 9) || shape == 255 {
        recipe = 6;
    }
    // Tuple arity is a match predicate, not a scalar input to the SEMV
    // encoder. The two projected tuple fields each validate the exact packed
    // two-item shape before execution.
    let execution_inputs = inputs
        .iter()
        .filter(|input| {
            !matches!(
                input,
                Projection::TupleArity { .. } | Projection::TupleArityThree { .. }
            )
        })
        .collect::<Vec<_>>();
    for input in inputs {
        if let Projection::TupleArity { operand } | Projection::TupleArityThree { operand } = input
        {
            let has_register = inputs.iter().any(|projection| {
                matches!(projection, Projection::TupleRegister { operand: other, .. } if other == operand)
            });
            let has_value = inputs.iter().any(|projection| {
                matches!(projection, Projection::TupleValue { operand: other } if other == operand)
                    || matches!(projection, Projection::ValueProgram { source, .. } | Projection::RequiredValueProgram { source, .. }
                        if matches!(source.as_ref(), Projection::TupleValue { operand: other } if other == operand))
            });
            let has_index = !matches!(input, Projection::TupleArityThree { .. }) || inputs.iter().any(|projection| {
                matches!(projection, Projection::TupleQualifiedRegister { operand: other, .. } if other == operand)
            });
            if !has_register || !has_value || !has_index {
                recipe = 6;
            }
        }
    }
    let structured_offset = if let CandidateRecipe::PackedMaskIndirect {
        opcode,
        mask_operand,
        indirect_operand,
        indirect_token,
        reverse_mask,
        indirect_class,
        first_class,
        first_shift,
        second_class,
        second_shift,
    } = &candidate.recipe
    {
        if recipe == 8 {
            align(out);
            let offset = long(out.len())?;
            push_word(out, *opcode);
            out.extend_from_slice(&[*mask_operand, *indirect_operand]);
            push_word(out, *indirect_class);
            push_word(out, *first_class);
            out.extend_from_slice(&[*first_shift, *second_shift]);
            push_word(out, *second_class);
            push_word(out, u16::from(*reverse_mask));
            push_word(out, u16::from(*indirect_token));
            Some(offset)
        } else {
            None
        }
    } else {
        None
    };
    let sequence_offset = if let CandidateRecipe::SemanticSequence { stages } = &candidate.recipe {
        let offset = write_sequence(out, stages, programs)?;
        if offset.is_none() {
            recipe = 6;
        }
        offset
    } else {
        None
    };
    let arity_three = tuple_arity_three(inputs.iter());
    let projection_start = out.len();
    if recipe != 6 {
        for projection in &execution_inputs {
            if !write_bound_projection(out, projection, programs, &arity_three)? {
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
        if recipe == 6 {
            0
        } else {
            if let CandidateRecipe::SemanticSequence { stages } = &candidate.recipe {
                word(stages.len())?
            } else {
                word(execution_inputs.len())?
            }
        },
    );
    set_long(
        out,
        row + 12,
        sequence_offset.or(structured_offset).unwrap_or(
            if recipe == 6 || execution_inputs.is_empty() {
                0
            } else {
                long(projection_start)?
            },
        ),
    );
    out[row + 16] = candidate.width_rank;
    out[row + 17] = u8::from(candidate.unstable_widen);
    out[row + 18] = candidate.member_excluded;
    out[row + 19] = match &candidate.recipe {
        CandidateRecipe::Unsupported { plan } => required_operand_forms(name(names, *plan)?),
        _ => 0,
    };
    set_word(out, row + 20, candidate.mode);
    set_word(out, row + 28, if recipe == 7 { table } else { MISSING });
    Ok(())
}

fn semantic_emits_opcode(programs: &Programs<'_>, index: u16) -> bool {
    programs
        .rows
        .get(usize::from(index))
        .is_some_and(|program| {
            program.kind == 2
                && decode_encoding_program(program.version, program.bytes)
                    .ok()
                    .and_then(|steps| steps.into_iter().next())
                    .is_some_and(|step| {
                        matches!(
                            step,
                            EncodingStep::Literal { .. }
                                | EncodingStep::Fields { .. }
                                | EncodingStep::InputFields { .. }
                        )
                    })
        })
}

// Each nibble is a necessary packed-operand wrapper for an unsupported
// sequence candidate. Native selection may skip a disproven candidate, but
// still fails closed when its wrapper can match.
fn required_operand_forms(plan: &str) -> u8 {
    let predicates = if let Some(body) = plan.strip_prefix("semv.sequence.v1:match:_@") {
        let Some((predicates, _)) = body.split_once(';') else {
            return 0;
        };
        predicates
    } else if let Some(body) = plan
        .strip_prefix("semv.reject.v1:")
        .or_else(|| plan.strip_prefix("semv.inputs.v1:"))
    {
        let Some((_, predicates)) = body.split_once('@') else {
            return 0;
        };
        predicates.split('|').next().unwrap_or(predicates)
    } else {
        return 0;
    };
    let mut forms = [0u8; 2];
    for predicate in predicates.split(',') {
        if let Some(operand) = necessary_scalar_root(predicate) {
            forms[operand] = 6;
        }
        if let Some(operand) = necessary_named_root(predicate) {
            forms[operand] = 7;
        }
        if let Some((operand, form)) = necessary_path_form(predicate) {
            forms[operand] = form;
        }
        if let Some(rest) = predicate.strip_prefix("member_shape") {
            if let Some((operand, field)) = rest.split_once('.') {
                if !operand.is_empty()
                    && operand.bytes().all(|byte| byte.is_ascii_digit())
                    && !field.is_empty()
                    && field
                        .bytes()
                        .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
                {
                    if let Ok(operand @ 0..=1) = operand.parse::<usize>() {
                        forms[operand] = if forms[operand] == 0 || forms[operand] == 5 {
                            5
                        } else {
                            0
                        };
                    }
                }
            }
        }
        for (prefix, form) in [
            ("indirect_tuple_reg", 4),
            ("unary_plus_indirect_reg", 2),
            ("unary_minus_indirect_reg", 3),
            ("indirect_reg", 1),
        ] {
            if let Some(rest) = predicate.strip_prefix(prefix) {
                let Some((operand, _)) = rest.split_once('.') else {
                    continue;
                };
                if let Ok(operand @ 0..=1) = operand.parse::<usize>() {
                    if forms[operand] == 0 || forms[operand] == form {
                        forms[operand] = form;
                    } else {
                        forms[operand] = 0;
                    }
                }
            }
        }
    }
    forms[0] | forms[1] << 4
}

fn necessary_scalar_root(predicate: &str) -> Option<usize> {
    let operand = predicate
        .strip_prefix("target:expr")
        .or_else(|| predicate.strip_prefix("expr"))?;
    if operand.is_empty() || !operand.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    operand.parse::<usize>().ok().filter(|operand| *operand < 2)
}

fn necessary_named_root(predicate: &str) -> Option<usize> {
    let rest = predicate.strip_prefix("register_or_named_range")?;
    let (operand, rest) = rest.split_once(".classes")?;
    let (classes, rest) = rest.split_once(".prefix")?;
    let (prefix, rest) = rest.split_once(".min")?;
    let (minimum, maximum) = rest.split_once(".max")?;
    if operand.is_empty()
        || !operand.bytes().all(|byte| byte.is_ascii_digit())
        || !classes.split('+').all(|class| class.parse::<u16>().is_ok())
        || prefix.is_empty()
        || !prefix
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
        || minimum.parse::<u32>().ok()? > maximum.parse::<u32>().ok()?
    {
        return None;
    }
    operand.parse::<usize>().ok().filter(|operand| *operand < 2)
}

// A partial structural proof for two exact expression-path roots. The full
// path stays unsupported; these facts only let native selection disprove it.
fn necessary_path_form(predicate: &str) -> Option<(usize, u8)> {
    let spec = predicate.strip_prefix("xp1:")?;
    let parts = spec.split('/').collect::<Vec<_>>();
    if !(4..=8).contains(&parts.len()) || parts[1..3] != ["i", "t0"] {
        return None;
    }
    let operand = parts[0].parse::<usize>().ok()?;
    if operand > 1 || !parts[0].bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    let (terminal, containers) = parts[3..].split_last()?;
    let identifier = |value: &str| {
        !value.is_empty()
            && value
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
    };
    let valid_terminal = terminal
        .strip_prefix('r')
        .is_some_and(|value| value.parse::<u16>().is_ok())
        || terminal.strip_prefix('m').is_some_and(identifier)
        || terminal.strip_prefix('n').is_some_and(identifier)
        || *terminal == "s"
        || terminal
            .strip_prefix('q')
            .and_then(|value| value.split_once(".c"))
            .is_some_and(|(qualifier, class)| {
                identifier(qualifier) && class.parse::<u16>().is_ok()
            });
    if !valid_terminal
        || !containers.iter().all(|step| {
            matches!(*step, "i" | "b" | "l" | "r")
                || step
                    .strip_prefix('t')
                    .is_some_and(|value| value.parse::<u8>().is_ok())
        })
    {
        return None;
    }
    if parts.len() == 4 && terminal.strip_prefix('m').is_some_and(identifier) {
        Some((operand, 8))
    } else if containers.first() == Some(&"b") {
        Some((operand, 9))
    } else {
        None
    }
}

fn tuple_arity_three<'a>(inputs: impl Iterator<Item = &'a Projection>) -> BTreeSet<u8> {
    inputs
        .filter_map(|input| match input {
            Projection::TupleArityThree { operand } => Some(*operand),
            _ => None,
        })
        .collect()
}

fn write_bound_projection(
    out: &mut Vec<u8>,
    projection: &Projection,
    programs: &Programs<'_>,
    arity_three: &BTreeSet<u8>,
) -> Result<bool, String> {
    let start = out.len();
    if !write_projection(out, projection, programs)? {
        return Ok(false);
    }
    if arity_three.contains(&out[start + 1]) {
        let item = match out[start] {
            5 => {
                out[start] = 11;
                Some(1)
            }
            6 => {
                out[start] = 12;
                Some(0)
            }
            _ => None,
        };
        if let Some(item) = item {
            set_word(out, start + 10, (3 << 8) | item);
        }
    }
    Ok(true)
}

// Each descriptor carries only stage kind, program and bounded projection slice.
// Projection-only match stages never invoke an emitting program.
fn write_sequence(
    out: &mut Vec<u8>,
    stages: &[SemanticStage],
    programs: &Programs<'_>,
) -> Result<Option<u32>, String> {
    if stages.is_empty() || stages.len() > 8 {
        return Ok(None);
    }
    let start = out.len();
    let offset = long(start)?;
    out.resize(start + stages.len() * 12, 0);
    let arity_three = tuple_arity_three(stages.iter().flat_map(|stage| &stage.inputs));
    let mut encoded = false;
    for (index, stage) in stages.iter().enumerate() {
        let descriptor = start + index * 12;
        let program = stage
            .program
            .and_then(|id| programs.semantics.get(&id).copied())
            .unwrap_or(MISSING);
        let supported = if stage.program.is_some() {
            encoded = true;
            programs
                .rows
                .get(usize::from(program))
                .is_some_and(|row| row.kind == 2 && matches!(row.version, 2 | 6))
        } else {
            !encoded
        };
        if !supported || stage.inputs.is_empty() || stage.inputs.len() > 16 {
            out.truncate(start);
            return Ok(None);
        }
        out[descriptor] = u8::from(stage.program.is_some());
        set_word(out, descriptor + 2, program);
        set_word(out, descriptor + 4, word(stage.inputs.len())?);
        let inputs_offset = long(out.len())?;
        set_long(out, descriptor + 8, inputs_offset);
        for input in &stage.inputs {
            if !write_bound_projection(out, input, programs, &arity_three)? {
                out.truncate(start);
                return Ok(None);
            }
        }
    }
    if !encoded {
        out.truncate(start);
        return Ok(None);
    }
    Ok(Some(offset))
}

fn write_projection(
    out: &mut Vec<u8>,
    projection: &Projection,
    programs: &Programs<'_>,
) -> Result<bool, String> {
    let (projection, value_program) = match projection {
        Projection::ValueProgram { program, source }
        | Projection::RequiredValueProgram { program, source } => {
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
        Projection::IndirectRegister { operand, class } => (8, *operand, *class, 0),
        Projection::UpdatedIndirectRegister {
            operand,
            class,
            token,
        } => (
            match *token {
                18 => 9,
                19 => 10,
                _ => return Ok(false),
            },
            *operand,
            *class,
            0,
        ),
        Projection::Member { operand, qualifier } => (2, *operand, *qualifier, 0),
        Projection::TupleRegister { operand, class } => (5, *operand, *class, 0),
        Projection::TupleValue { operand } => (6, *operand, 0, 0),
        Projection::TupleArity { operand } => (14, *operand, 2, 0),
        Projection::TupleArityThree { operand } => (14, *operand, 3, 0),
        Projection::TupleQualifiedRegister {
            operand,
            class,
            qualifier,
        } => {
            let Some(qualifier) = programs.qualifiers.get(qualifier) else {
                return Ok(false);
            };
            (13, *operand, *class, i32::from(*qualifier))
        }
        Projection::NamedRegister { operand, name } => (4, *operand, *name, 0),
        Projection::Constant(value) => {
            let Ok(value) = i32::try_from(*value) else {
                return Ok(false);
            };
            (3, 0, 0, value)
        }
        Projection::ValueProgram { .. } | Projection::RequiredValueProgram { .. } => {
            return Ok(false)
        }
    };
    out.extend_from_slice(&[kind, operand]);
    push_word(out, field);
    out.extend_from_slice(&literal.to_be_bytes());
    push_word(out, value_program);
    push_word(out, if kind == 13 { 0x0302 } else { 0 });
    Ok(true)
}

fn collect_qualified_classes(projection: &Projection, classes: &mut BTreeSet<(u16, u16)>) {
    match projection {
        Projection::TupleQualifiedRegister {
            class, qualifier, ..
        } => {
            classes.insert((*class, *qualifier));
        }
        Projection::ValueProgram { source, .. }
        | Projection::RequiredValueProgram { source, .. } => {
            collect_qualified_classes(source, classes)
        }
        _ => {}
    }
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
        | Projection::Member { qualifier, .. }
        | Projection::TupleQualifiedRegister { qualifier, .. } => {
            bind(dictionary, name(names, *qualifier)?.into(), *qualifier, 0)
        }
        Projection::ValueProgram { source, .. }
        | Projection::RequiredValueProgram { source, .. } => bind_member(source, names, dictionary),
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

#[cfg(test)]
mod sequence_wire_tests {
    use super::*;

    #[test]
    fn scalar_and_named_roots_preserve_unknown_or_malformed_metadata() {
        assert_eq!(
            required_operand_forms(
                "semv.sequence.v1:match:_@target:expr0;encode:x@expr0;fixup:y@expr0"
            ),
            6
        );
        assert_eq!(required_operand_forms("semv.reject.v1:bad@register_or_named_range0.classes5.prefixb.min0.max7,reg1.class0"), 7);
        assert_eq!(required_operand_forms("semv.inputs.v1:future@expr1"), 0x60);
        for predicate in [
            "target:expr",
            "target:expr2",
            "target:expr0.more",
            "expr0future",
            "register_or_named_range0.classes.prefixb.min0.max7",
            "register_or_named_range0.classes5.prefix.min0.max7",
            "register_or_named_range0.classes5.prefixb.min8.max7",
            "register_or_named_range0.classes5.prefixb.min0.max7.future",
        ] {
            assert_eq!(
                required_operand_forms(&format!("semv.reject.v1:bad@{predicate}")),
                0,
                "{predicate}"
            );
        }
    }

    #[test]
    fn necessary_path_forms_remain_partial_and_bounded() {
        for (path, expected) in [
            ("xp1:0/i/t0/mW", Some((0, 8))),
            ("xp1:1/i/t0/b/t2/qL.c0", Some((1, 9))),
            ("xp1:0/i/t0/b/r1", Some((0, 9))),
        ] {
            assert_eq!(necessary_path_form(path), expected);
        }
        for path in [
            "xp1:0/i/t0/m",
            "xp1:0/i/t0/b",
            "xp1:0/i/t0/b/future",
            "xp1:0/i/t0/b/t2/qL.c",
            "xp1:2/i/t0/mW",
            "xp1:0/i/t1/mW",
            "xp1:0/i/t0/r1",
            "xp1:0/i/t0/b/x/r1",
            "xp1:0/i/t0/b/b/b/b/b/b/r1",
        ] {
            assert_eq!(necessary_path_form(path), None, "{path}");
        }
        assert_eq!(
            required_operand_forms(
                "semv.sequence.v1:match:_@xp1:0/i/t1/r1,xp1:0/i/t0/mW,reg1.class0;encode:x@expr0"
            ),
            8
        );
    }

    #[test]
    fn unsupported_member_sequence_preserves_only_exact_necessary_wrapper() {
        assert_eq!(
            required_operand_forms(
                "semv.sequence.v1:match:_@expr0,member_shape1.W;encode:x@expr0;fixup:y@expr1"
            ),
            0x56
        );
        assert_eq!(
            required_operand_forms("semv.sequence.v1:match:_@member_shape0.L;encode:x@expr0"),
            5
        );
        for predicate in [
            "member_shape1.",
            "member_shape1.W.extra",
            "member_shape.W",
            "member_shape2.W",
            "member_shape1W",
            "member_shape_1.W",
            "member1.W",
        ] {
            assert_eq!(
                required_operand_forms(&format!(
                    "semv.sequence.v1:match:_@{predicate};encode:x@expr0"
                )),
                0,
                "{predicate}"
            );
        }
    }

    #[test]
    fn bounded_sequence_wire_preserves_predicates_and_exact_tuple_projections() {
        let mut programs = Programs::default();
        programs.add(2, 6, &[]).unwrap();
        programs.semantics.insert(7, 0);
        programs.qualifiers.insert(9, 2);
        let stages = [
            SemanticStage {
                program: None,
                inputs: vec![
                    Projection::TupleArityThree { operand: 0 },
                    Projection::TupleQualifiedRegister {
                        operand: 0,
                        class: 4,
                        qualifier: 9,
                    },
                ],
            },
            SemanticStage {
                program: Some(7),
                inputs: vec![
                    Projection::TupleRegister {
                        operand: 0,
                        class: 1,
                    },
                    Projection::TupleValue { operand: 0 },
                ],
            },
        ];
        let mut wire = vec![0; 80];
        assert_eq!(
            write_sequence(&mut wire, &stages, &programs).unwrap(),
            Some(80)
        );
        assert_eq!(&wire[80..84], &[0, 0, 255, 255]);
        assert_eq!(&wire[92..96], &[1, 0, 0, 0]);
        assert_eq!(&wire[88..92], &104u32.to_be_bytes());
        assert_eq!(&wire[100..104], &128u32.to_be_bytes());
        assert_eq!(&wire[104..108], &[14, 0, 0, 3]);
        assert_eq!(&wire[116..120], &[13, 0, 0, 4]);
        assert_eq!(&wire[120..124], &2i32.to_be_bytes());
        assert_eq!(&wire[126..128], &[3, 2]);
        assert_eq!(wire[128], 11);
        assert_eq!(&wire[138..140], &[3, 1]);
        assert_eq!(wire[140], 12);
        assert_eq!(&wire[150..152], &[3, 0]);
        let original = wire.clone();
        programs.rows[0].version = 4;
        assert_eq!(write_sequence(&mut wire, &stages, &programs).unwrap(), None);
        assert_eq!(wire, original, "non-encoding stages must not serialize");
        programs.rows[0].version = 6;
        for unsupported in [
            vec![stages[0].clone()],
            vec![stages[1].clone(), stages[0].clone()],
            vec![SemanticStage {
                program: Some(7),
                inputs: Vec::new(),
            }],
        ] {
            assert_eq!(
                write_sequence(&mut wire, &unsupported, &programs).unwrap(),
                None
            );
            assert_eq!(wire, original, "unsupported sequence must roll back");
        }
        programs.semantics.clear();
        let original = wire.clone();
        assert_eq!(write_sequence(&mut wire, &stages, &programs).unwrap(), None);
        assert_eq!(
            wire, original,
            "unsupported stage must leave no partial wire body"
        );
    }
}
