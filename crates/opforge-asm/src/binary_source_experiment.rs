//! Provisional native binary-source package wire format. No source or CPU semantics
//! are inferred here: selection metadata and executable bodies come from the package.

use std::collections::{BTreeMap, BTreeSet};

use package::{
    decode_encoding_program, inline_head_policy_program, macro_descriptor_program,
    macro_fragment_program, macro_spelling_program, packed_data_program,
    packed_declaration_program, packed_file_program, packed_macro_call_program,
    packed_metadata_program, validate_fixup_program, EncodingStep,
};
use types::hierarchy::ResolvedHierarchy;
use vm::binary_source_package::{
    BinarySourcePackage, CandidateRecipe, NumericCandidate, Projection, ScalarPlan, SemanticStage,
};
use vm::runtime_model_core::RuntimeModelCore;

const MISSING: u16 = u16::MAX;
const HEADER: usize = 200;
const ROW: usize = 32;
const SCALAR_ADDRESS_IDENTITY: u16 = 1;

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct DictionaryRoleFlags(u8);

impl DictionaryRoleFlags {
    // Identity is contextual for statement heads and configured directive operands.
    const CONTEXTUAL: Self = Self(0);
    // Package-owned registers/named operands retain identity in ordinary operands.
    const REGISTER_OR_NAMED: Self = Self(1);
    // Member markers retain identity only after a non-head dot.
    const MEMBER: Self = Self(2);
    // Exact STVM spellings retain their identity only in state directive operands.
    const STATE_ARGUMENT: Self = Self(4);
}

#[derive(Clone, Copy, Debug)]
struct DictionaryBinding {
    id: u16,
    qualifier: u8,
    roles: DictionaryRoleFlags,
}

/// Prepare a self-contained BS24 block for one resolved package hierarchy.
/// Offsets and lengths are big-endian and relative to the block start.
/// Unsupported candidate recipes remain explicit rows, never silent omissions.
pub fn prepare_package(
    core: &RuntimeModelCore,
    resolved: &ResolvedHierarchy,
) -> Result<Vec<u8>, String> {
    let target = format!("{}--{}", resolved.cpu_id, resolved.dialect_id);
    if resolved.cpu_id.is_empty()
        || resolved.dialect_id.is_empty()
        || target.len() > 26
        || !target
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-'))
    {
        return Err(format!(
            "runtime package target key {target:?} must use nonempty CPU and dialect identifiers containing only ASCII letters, digits, '_' or '-' and fit within 26 bytes"
        ));
    }
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
            DictionaryRoleFlags::CONTEXTUAL,
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
                DictionaryRoleFlags::CONTEXTUAL,
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
        bind(
            &mut dictionary,
            name(&names, row.name)?.into(),
            row.name,
            0,
            DictionaryRoleFlags::REGISTER_OR_NAMED,
        )?;
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
                DictionaryRoleFlags::REGISTER_OR_NAMED,
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
    for member in &package.member_bindings {
        bind(
            &mut dictionary,
            name(&names, member.field)?.into(),
            member.field,
            0,
            DictionaryRoleFlags::MEMBER,
        )?;
    }
    let mut directive_ids = Vec::new();
    for directive in [
        "cpu", "org", "byte", "word", "long", "end", "align", "res", "for", "endfor",
    ] {
        let id = intern(&mut names, directive)?;
        bind(
            &mut dictionary,
            directive.into(),
            id,
            0,
            DictionaryRoleFlags::CONTEXTUAL,
        )?;
        directive_ids.push(id);
    }
    let mut declaration_heads = [0; 3];
    for (head, spelling) in declaration_heads.iter_mut().zip(["const", "var", "set"]) {
        *head = intern(&mut names, spelling)?;
        bind(
            &mut dictionary,
            spelling.into(),
            *head,
            0,
            DictionaryRoleFlags::CONTEXTUAL,
        )?;
    }
    let emit_id = intern(&mut names, "emit")?;
    bind(
        &mut dictionary,
        "emit".into(),
        emit_id,
        0,
        DictionaryRoleFlags::CONTEXTUAL,
    )?;
    let file_id = intern(&mut names, "incbin")?;
    bind(
        &mut dictionary,
        "incbin".into(),
        file_id,
        0,
        DictionaryRoleFlags::CONTEXTUAL,
    )?;
    let mut metadata_heads = [0; 6];
    for (slot, spelling) in metadata_heads.iter_mut().zip([
        "meta.output.name",
        "meta.output.hex",
        "meta.output.bin",
        "meta.output.fill",
        "meta.name",
        "meta.version",
    ]) {
        *slot = intern(&mut names, spelling)?;
        bind(
            &mut dictionary,
            spelling.into(),
            *slot,
            0,
            DictionaryRoleFlags::CONTEXTUAL,
        )?;
    }
    let cpu_id = intern(&mut names, &resolved.cpu_id)?;
    bind(
        &mut dictionary,
        resolved.cpu_id.to_ascii_lowercase(),
        cpu_id,
        0,
        DictionaryRoleFlags::CONTEXTUAL,
    )?;
    for (spelling, _, _) in core.supported_cpus() {
        if core
            .canonical_cpu_id_for_input(&spelling)
            .is_some_and(|id| id.eq_ignore_ascii_case(&resolved.cpu_id))
        {
            bind(
                &mut dictionary,
                spelling.to_ascii_lowercase(),
                cpu_id,
                0,
                DictionaryRoleFlags::CONTEXTUAL,
            )?;
        }
    }
    // State operands use a separate lexical context so CPU aliases cannot widen
    // the exact package-owned state argument vocabulary.
    let mut state_argument_dictionary = BTreeMap::new();
    for directive in &package.state.directives {
        bind(
            &mut dictionary,
            name(&names, directive.head)?.to_string(),
            directive.head,
            0,
            DictionaryRoleFlags::CONTEXTUAL,
        )?;
        for argument in &directive.arguments {
            if argument.kind != 0 {
                return Err("binary-source state argument kind unsupported".into());
            }
            let id = word(argument.matched as usize)?;
            bind(
                &mut state_argument_dictionary,
                name(&names, id)?.to_string(),
                id,
                0,
                DictionaryRoleFlags::STATE_ARGUMENT,
            )?;
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
            state_guard: 0,
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
    out[..4].copy_from_slice(b"BS24");
    // Structural policies come from canonical projections, never CPU identities.
    let retain_indirect = package
        .candidates
        .iter()
        .any(|candidate| match &candidate.recipe {
            CandidateRecipe::SemanticInputs { inputs, .. }
            | CandidateRecipe::SemanticBranch { inputs, .. } => {
                inputs.iter().any(preserves_indirect)
            }
            CandidateRecipe::SemanticSequence { stages } => stages
                .iter()
                .any(|stage| stage.inputs.iter().any(preserves_indirect)),
            _ => false,
        });
    set_word(&mut out, 130, u16::from(retain_indirect));
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
    let target_offset = out.len();
    out.extend_from_slice(target.as_bytes());
    align(&mut out);
    let data_offset = out.len();
    let word_bytes = word(properties.word_size_bytes as usize)?;
    if word_bytes == 0 {
        return Err("binary-source word size must be positive".into());
    }
    let data_plan = packed_data_program(
        emit_id,
        directive_ids[2],
        directive_ids[3],
        directive_ids[4],
        word_bytes,
    );
    out.extend_from_slice(&data_plan);
    align(&mut out);
    let member_bindings_offset = out.len();
    for member in &package.member_bindings {
        push_word(&mut out, member.mnemonic);
        out.extend_from_slice(&[qualifier(member.qualifier)?, member.operand]);
        push_word(&mut out, member.field);
        push_word(&mut out, 0);
    }
    let head_policy_offset = out.len();
    let head_policy = inline_head_policy_program();
    out.extend_from_slice(&head_policy);
    align(&mut out);
    let declaration_plan_offset = out.len();
    let declaration_plan = packed_declaration_program(declaration_heads);
    out.extend_from_slice(&declaration_plan);
    align(&mut out);
    align(&mut out);
    let state_offset = if package.state.defaults.is_empty() {
        0
    } else {
        out.len()
    };
    let state_plan = if package.state.defaults.is_empty() {
        Vec::new()
    } else {
        write_state_plan(&package.state)?
    };
    out.extend_from_slice(&state_plan);
    let runtime_bytes = long(out.len())?;
    let dictionary_offset = out.len();
    for (spelling, binding) in dictionary.iter().chain(state_argument_dictionary.iter()) {
        push_word(&mut out, word(spelling.len())?);
        push_word(&mut out, binding.id);
        out.extend_from_slice(&[binding.qualifier, binding.roles.0]);
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
    let macro_call_offset = out.len();
    let macro_call = macro_descriptor_program(false);
    out.extend_from_slice(&macro_call);
    let macro_call_length = macro_call.len();
    align(&mut out);
    let macro_header_offset = out.len();
    let macro_header = macro_descriptor_program(true);
    out.extend_from_slice(&macro_header);
    let macro_header_length = macro_header.len();
    align(&mut out);
    let macro_packed_offset = out.len();
    let macro_packed = packed_macro_call_program();
    out.extend_from_slice(&macro_packed);
    let macro_packed_length = macro_packed.len();
    align(&mut out);
    let macro_spelling_offset = out.len();
    let macro_spelling = macro_spelling_program();
    out.extend_from_slice(&macro_spelling);
    let macro_spelling_length = macro_spelling.len();
    align(&mut out);
    let macro_fragment_offset = out.len();
    let macro_fragment = macro_fragment_program();
    out.extend_from_slice(&macro_fragment);
    let macro_fragment_length = macro_fragment.len();
    align(&mut out);
    let file_offset = out.len();
    let file_plan = packed_file_program(file_id);
    out.extend_from_slice(&file_plan);
    align(&mut out);
    let metadata_offset = out.len();
    let metadata_plan = packed_metadata_program(metadata_heads);
    out.extend_from_slice(&metadata_plan);
    align(&mut out);
    let total = long(out.len())?;
    for (offset, value) in [
        (4, total),
        (192, long(state_offset)?),
        (196, long(state_plan.len())?),
        (160, long(member_bindings_offset)?),
        (164, long(package.member_bindings.len())?),
        (180, long(declaration_plan_offset)?),
        (184, long(declaration_plan.len())?),
        (168, long(head_policy_offset)?),
        (172, long(head_policy.len())?),
        (8, long(dictionary_offset)?),
        (
            12,
            long(dictionary.len() + state_argument_dictionary.len())?,
        ),
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
        (80, long(macro_call_offset)?),
        (84, long(macro_call_length)?),
        (88, long(macro_header_offset)?),
        (92, long(macro_header_length)?),
        (100, long(macro_packed_offset)?),
        (104, long(macro_packed_length)?),
        (108, long(macro_spelling_offset)?),
        (112, long(macro_spelling_length)?),
        (116, long(macro_fragment_offset)?),
        (120, long(macro_fragment_length)?),
        (124, long(target_offset)?),
        (132, long(file_offset)?),
        (136, long(file_plan.len())?),
        (144, long(data_offset)?),
        (148, long(data_plan.len())?),
        (152, long(metadata_offset)?),
        (156, long(metadata_plan.len())?),
    ] {
        set_long(&mut out, offset, value);
    }
    for (index, id) in directive_ids.iter().enumerate() {
        let offset = match index {
            0..=5 => 48 + index * 2,
            6..=7 => 76 + (index - 6) * 2,
            8 => 66,
            9 => 98,
            _ => unreachable!(),
        };
        set_word(&mut out, offset, *id);
    }
    set_word(&mut out, 140, emit_id);
    set_word(&mut out, 142, word_bytes);
    set_word(&mut out, 60, cpu_id);
    set_word(&mut out, 62, total_names);
    set_word(&mut out, 64, u16::from(properties.data_little_endian));
    set_word(&mut out, 96, 2);
    set_word(&mut out, 188, package::PARSER_VM_MACRO_VERSION);
    set_word(
        &mut out,
        176,
        package::PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT,
    );
    set_word(&mut out, 128, word(target.len())?);
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
    let sequence_needs_prefix = matches!(&candidate.recipe, CandidateRecipe::SemanticSequence { stages }
        if !stages.iter().any(|stage| stage.program.is_some_and(|id| programs.semantics.get(&id).is_some_and(|index| semantic_emits_opcode(programs, *index)))));
    // Native emits the table prefix before the combined sequence payload.
    // Canonical TABLE must therefore emit that payload exactly once at the end.
    if recipe == 9
        && sequence_needs_prefix
        && !programs
            .rows
            .get(usize::from(table))
            .is_some_and(|row| sequence_prefix_table(row.bytes))
    {
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
    if (program == MISSING && recipe != 9) || shape == 255 {
        recipe = 6;
    }
    // Tuple arity is a match predicate, not a scalar input to the SEMV
    // encoder. Each projected leaf validates its package-specified arity and
    // item index before execution; absent arity remains bounded to two/three.
    let execution_inputs = inputs
        .iter()
        .filter(|input| {
            !matches!(
                input,
                Projection::TupleArity { .. } | Projection::TupleArityThree { .. }
            )
        })
        .collect::<Vec<_>>();
    if !filtered_tuple_arities_covered(inputs) {
        recipe = 6;
    }
    let sequence_offset = if let CandidateRecipe::SemanticSequence { stages } = &candidate.recipe {
        let offset = write_sequence(out, stages, programs)?;
        if offset.is_none() {
            recipe = 6;
        }
        offset
    } else {
        None
    };
    let arities = tuple_arities(inputs.iter());
    let projection_start = out.len();
    if recipe != 6 {
        for (index, projection) in execution_inputs.iter().enumerate() {
            let descriptor = out.len();
            if !write_bound_projection(out, projection, programs, &arities)? {
                out.truncate(projection_start);
                recipe = 6;
                break;
            }
            // The canonical branch envelope fixes scalar input one as its
            // target. Bind optional affine address identity alongside that scalar;
            // the branch VM owns the successful positional proof.
            if recipe == 5 && index == 1 {
                if !matches!(projection, Projection::Expression(0)) {
                    out.truncate(projection_start);
                    recipe = 6;
                    break;
                }
                set_word(out, descriptor + 10, SCALAR_ADDRESS_IDENTITY);
            }
        }
    }
    set_word(out, row + 30, candidate.state_guard);
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
        if recipe == 6 {
            0
        } else {
            sequence_offset.unwrap_or(if execution_inputs.is_empty() {
                0
            } else {
                long(projection_start)?
            })
        },
    );
    out[row + 16] = candidate.width_rank;
    out[row + 17] = u8::from(candidate.unstable_widen);
    out[row + 18] = candidate.member_excluded;
    let (required_forms, tuple_classes) = match &candidate.recipe {
        CandidateRecipe::Unsupported { plan } => {
            let plan = name(names, *plan)?;
            (required_operand_forms(plan), required_tuple_classes(plan))
        }
        CandidateRecipe::SemanticSequence { stages } if recipe == 6 => {
            necessary_sequence_match(stages)
        }
        _ => (0, 0),
    };
    out[row + 19] = required_forms;
    set_word(out, row + 20, candidate.mode);
    set_word(out, row + 22, tuple_classes);
    set_word(
        out,
        row + 28,
        if recipe == 7 || (recipe == 9 && sequence_needs_prefix) {
            table
        } else {
            MISSING
        },
    );
    Ok(())
}

fn sequence_prefix_table(mut bytes: &[u8]) -> bool {
    while let [vm::bytecode::OP_EMIT_U8, _, rest @ ..] = bytes {
        bytes = rest;
    }
    bytes == [vm::bytecode::OP_EMIT_OPERAND, 0, vm::bytecode::OP_END]
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
    let Some(predicates) = match_predicates(plan) else {
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
    exact_tuple_forms(
        forms,
        predicates.split(',').filter_map(canonical_tuple_arity),
    )
}

// Exact arity strengthens only an unknown or arbitrary-tuple wrapper. Conflicting
// arity conjuncts retain the barrier; unrelated wrapper facts are not replaced.
// Nibbles 10/11 require a complete two/three-item tuple, respectively.
fn exact_tuple_forms(forms: [u8; 2], arities: impl Iterator<Item = (u8, u8)>) -> u8 {
    let mut required = [0u8; 2];
    for (operand, arity) in arities {
        let Some(existing) = required.get_mut(usize::from(operand)) else {
            continue;
        };
        if *existing == 0 {
            *existing = arity;
        } else if *existing != arity {
            *existing = u8::MAX;
        }
    }
    let mut forms = forms;
    for (form, arity) in forms.iter_mut().zip(required) {
        if matches!(*form, 0 | 4) && matches!(arity, 2 | 3) {
            *form = arity + 8;
        }
    }
    forms[0] | forms[1] << 4
}

fn canonical_tuple_arity(predicate: &str) -> Option<(u8, u8)> {
    let rest = predicate.strip_prefix("indirect_tuple_arity")?;
    let (operand, arity) = rest.split_once(".value")?;
    if operand.is_empty() || !operand.bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    let operand = operand.parse::<u8>().ok()?;
    let arity = match arity {
        "2" => 2,
        "3" => 3,
        _ => return None,
    };
    Some((operand, arity))
}

// Facts come only from canonical match conjuncts. Unknown stages and predicates
// provide no proof; known conjuncts may still disprove an unsupported candidate.
fn match_predicates(plan: &str) -> Option<&str> {
    if let Some(body) = plan.strip_prefix("semv.sequence.v1:match:_@") {
        return body.split_once(';').map(|(predicates, _)| predicates);
    }
    let body = plan
        .strip_prefix("semv.reject.v1:")
        .or_else(|| plan.strip_prefix("semv.inputs.v1:"))?;
    let (_, predicates) = body.split_once('@')?;
    Some(predicates.split('|').next().unwrap_or(predicates))
}

// Two bytes in the row encode necessary tuple-base classes, operand 0 then 1.
// Zero means unknown; classes beyond the one-byte representation retain the barrier.
fn required_tuple_classes(plan: &str) -> u16 {
    let Some(predicates) = match_predicates(plan) else {
        return 0;
    };
    let mut classes = [0u8; 2];
    for predicate in predicates.split(',') {
        let Some((operand, class)) = predicate
            .strip_prefix("indirect_tuple_reg")
            .and_then(|rest| rest.split_once(".item1.class"))
        else {
            continue;
        };
        if operand.is_empty()
            || class.is_empty()
            || !operand.bytes().all(|byte| byte.is_ascii_digit())
            || !class.bytes().all(|byte| byte.is_ascii_digit())
        {
            continue;
        }
        let (Ok(operand @ 0..=1), Ok(class)) = (operand.parse::<usize>(), class.parse::<u8>())
        else {
            continue;
        };
        // Item 1 may be an index in a register-first pair. Only a required
        // scalar item 0 proves this class belongs to a scalar-first tuple base.
        let scalar = format!("indirect_tuple_value{operand}.item0");
        let scalar_first = plan.match_indices(&scalar).any(|(offset, _)| {
            plan.as_bytes()
                .get(offset + scalar.len())
                .is_none_or(|byte| matches!(byte, b',' | b';' | b'|'))
        });
        if scalar_first {
            if let Some(encoded) = class.checked_add(1) {
                classes[operand] = encoded;
            }
        }
    }
    u16::from_be_bytes(classes)
}

// A sequence can be downgraded after a later executable stage fails native
// transport. Its leading match-only stages still provide sound, typed facts
// about the operand wrapper and base-register class. A required scalar projection
// in any stage proves the tuple starts with scalar data; it does not prove that
// stage is executable. Unknown projections retain the unsupported barrier.
fn necessary_sequence_match(stages: &[SemanticStage]) -> (u8, u16) {
    let mut forms = [0u8; 2];
    let mut classes = [0u8; 2];
    for stage in stages
        .iter()
        .take_while(|stage| stage.program.is_none() && !stage.fixup)
    {
        for input in &stage.inputs {
            if let Projection::TupleRegister {
                operand,
                item: 1,
                class,
            } = input
            {
                let index = usize::from(*operand);
                let scalar_first = stages
                    .iter()
                    .flat_map(|stage| &stage.inputs)
                    .any(|input| tuple_scalar_first(input, *operand));
                if index < 2 && scalar_first {
                    forms[index] = 4;
                    classes[index] = class
                        .checked_add(1)
                        .and_then(|value| u8::try_from(value).ok())
                        .unwrap_or(0);
                }
            }
        }
    }
    let arities = tuple_arities(
        stages
            .iter()
            .take_while(|stage| stage.program.is_none() && !stage.fixup)
            .flat_map(|stage| &stage.inputs),
    );
    (
        exact_tuple_forms(forms, arities.into_iter()),
        u16::from_be_bytes(classes),
    )
}

fn tuple_scalar_first(projection: &Projection, operand: u8) -> bool {
    match projection {
        Projection::TupleValue {
            operand: other,
            item: 0,
        } => *other == operand,
        Projection::ValueProgram { source, .. }
        | Projection::RequiredValueProgram { source, .. } => tuple_scalar_first(source, operand),
        _ => false,
    }
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

// Flat semantic inputs omit arity predicates from the encoder ABI. Every such
// predicate must therefore survive in an executed leaf's structural metadata.
fn filtered_tuple_arities_covered(inputs: &[Projection]) -> bool {
    fn covers(input: &Projection, operand: u8, arity: u8) -> bool {
        match input {
            Projection::TupleRegister {
                operand: other,
                item,
                ..
            }
            | Projection::TupleValue {
                operand: other,
                item,
            }
            | Projection::TupleIdentityScale {
                operand: other,
                item,
            }
            | Projection::TupleQualifiedRegister {
                operand: other,
                item,
                ..
            } => *other == operand && *item < arity,
            Projection::TupleNamedRegister { operand: other, .. } => {
                *other == operand && arity == 2
            }
            Projection::ValueProgram { source, .. }
            | Projection::RequiredValueProgram { source, .. } => covers(source, operand, arity),
            _ => false,
        }
    }
    tuple_arities(inputs.iter())
        .into_iter()
        .all(|(operand, arity)| {
            matches!(arity, 2 | 3) && inputs.iter().any(|input| covers(input, operand, arity))
        })
}

fn tuple_arities<'a>(inputs: impl Iterator<Item = &'a Projection>) -> BTreeMap<u8, u8> {
    let mut arities = BTreeMap::new();
    for input in inputs {
        let (operand, arity) = match input {
            Projection::TupleArity { operand } => (*operand, 2),
            Projection::TupleArityThree { operand } => (*operand, 3),
            _ => continue,
        };
        arities
            .entry(operand)
            .and_modify(|existing| {
                if *existing != arity {
                    *existing = u8::MAX;
                }
            })
            .or_insert(arity);
    }
    arities
}

fn write_bound_projection(
    out: &mut Vec<u8>,
    projection: &Projection,
    programs: &Programs<'_>,
    arities: &BTreeMap<u8, u8>,
) -> Result<bool, String> {
    let source = match projection {
        Projection::ValueProgram { source, .. }
        | Projection::RequiredValueProgram { source, .. } => source.as_ref(),
        other => other,
    };
    let tuple = match source {
        Projection::TupleRegister { operand, item, .. }
        | Projection::TupleValue { operand, item }
        | Projection::TupleIdentityScale { operand, item }
        | Projection::TupleQualifiedRegister { operand, item, .. } => Some((*operand, *item)),
        _ => None,
    };
    let bounds = if let Some((operand, item)) = tuple {
        // An absent predicate allows either supported tuple length. A conflicting
        // predicate is kept distinct from that bounded, dynamic arity.
        let arity = arities.get(&operand).copied().unwrap_or(0);
        if !matches!(arity, 0 | 2 | 3) || item > 2 || (arity != 0 && item >= arity) {
            return Ok(false);
        }
        Some((arity, item))
    } else {
        None
    };
    let start = out.len();
    if !write_projection(out, projection, programs)? {
        return Ok(false);
    }
    if let Some((arity, item)) = bounds {
        set_word(out, start + 10, (u16::from(arity) << 8) | u16::from(item));
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
    let arities = tuple_arities(
        stages
            .iter()
            .filter(|stage| stage.program.is_none() && !stage.fixup)
            .flat_map(|stage| &stage.inputs),
    );
    let mut encoded = false;
    for (index, stage) in stages.iter().enumerate() {
        let descriptor = start + index * 12;
        let program = stage
            .program
            .and_then(|id| programs.semantics.get(&id).copied())
            .unwrap_or(MISSING);
        let supported = if stage.program.is_some() {
            let valid = programs.rows.get(usize::from(program)).is_some_and(|row| {
                row.kind == 2
                    && if stage.fixup {
                        matches!(row.version, 4 | 7)
                            && validate_fixup_program(row.version, row.bytes).is_ok()
                    } else {
                        matches!(row.version, 2 | 6)
                    }
            });
            if !stage.fixup {
                encoded = true;
            }
            valid && (!stage.fixup || encoded)
        } else {
            !encoded && !stage.fixup
        };
        // Rust has no TargetMember match predicate; only fixup stages transport it.
        // Keep those canonical rows as barriers until both executors support matching.
        if !supported
            || stage.inputs.is_empty()
            || stage.inputs.len() > 16
            || (stage.program.is_none()
                && stage
                    .inputs
                    .iter()
                    .any(|input| matches!(input, Projection::TargetMember { .. })))
        {
            out.truncate(start);
            return Ok(None);
        }
        out[descriptor] = if stage.fixup {
            2
        } else {
            u8::from(stage.program.is_some())
        };
        set_word(out, descriptor + 2, program);
        set_word(out, descriptor + 4, word(stage.inputs.len())?);
        let inputs_offset = long(out.len())?;
        set_long(out, descriptor + 8, inputs_offset);
        for input in &stage.inputs {
            if !write_bound_projection(out, input, programs, &arities)? {
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
    if let Projection::CallArgumentRegister {
        operand,
        argument,
        class,
    } = projection
    {
        if *operand > 1 || *argument > 1 || *class == MISSING {
            return Ok(false);
        }
        out.extend_from_slice(&[24, *operand]);
        push_word(out, *class);
        out.extend_from_slice(&0i32.to_be_bytes());
        push_word(out, value_program);
        push_word(out, u16::from(*argument));
        return Ok(true);
    }
    if let Projection::RegisterMask {
        operand,
        first_class,
        first_shift,
        second_class,
        second_shift,
        reverse,
    } = projection
    {
        if value_program != MISSING
            || *operand > 1
            || *first_class == MISSING
            || *first_class == *second_class
            || *first_shift > 15
            || *second_shift > 15
            || (*second_class == MISSING && *second_shift != 0)
        {
            return Ok(false);
        }
        out.extend_from_slice(&[18, *operand]);
        push_word(out, *first_class);
        push_word(out, *second_class);
        out.extend_from_slice(&[*first_shift, *second_shift]);
        push_word(out, MISSING);
        push_word(out, u16::from(*reverse));
        return Ok(true);
    }
    let (kind, operand, field, literal) = match projection {
        Projection::Expression(operand) => (0, *operand, 0, 0),
        Projection::IndirectValue { operand } => (20, *operand, 0, 0),
        Projection::TupleNamedRegister { operand, name } => (21, *operand, *name, 0),
        Projection::ScalarExpression(operand) => (22, *operand, 0, 0),
        Projection::TargetExpression(operand) => (15, *operand, 0, 0),
        Projection::AtomicTargetExpression(operand) => (17, *operand, 0, 0),
        Projection::TargetMember { operand, qualifier } => (16, *operand, *qualifier, 0),
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
        Projection::MemberShape { operand, qualifier } => (19, *operand, *qualifier, 0),
        // Canonical tuple-register sources accept an optional identity product.
        // Its expected value is transported in the high word; the low word
        // remains the qualified-register dictionary id (zero when absent).
        Projection::TupleRegister { operand, class, .. } => (11, *operand, *class, 1 << 16),
        Projection::TupleValue { operand, .. } => (12, *operand, 0, 0),
        Projection::TupleIdentityScale { operand, .. } => (23, *operand, 1, 0),
        Projection::TupleArity { operand } => (14, *operand, 2, 0),
        Projection::TupleArityThree { operand } => (14, *operand, 3, 0),
        Projection::TupleQualifiedRegister {
            operand,
            class,
            qualifier,
            ..
        } => {
            let Some(qualifier) = programs.qualifiers.get(qualifier) else {
                return Ok(false);
            };
            (13, *operand, *class, (1 << 16) | i32::from(*qualifier))
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
        Projection::RegisterMask { .. } | Projection::CallArgumentRegister { .. } => unreachable!(),
    };
    out.extend_from_slice(&[kind, operand]);
    push_word(out, field);
    out.extend_from_slice(&literal.to_be_bytes());
    push_word(out, value_program);
    push_word(out, 0);
    Ok(true)
}

fn preserves_indirect(projection: &Projection) -> bool {
    match projection {
        Projection::IndirectValue { .. }
        | Projection::TupleNamedRegister { .. }
        | Projection::ScalarExpression(_) => true,
        Projection::ValueProgram { source, .. }
        | Projection::RequiredValueProgram { source, .. } => preserves_indirect(source),
        _ => false,
    }
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
    dictionary: &mut BTreeMap<String, DictionaryBinding>,
) -> Result<(), String> {
    match projection {
        Projection::NamedRegister { name, .. } | Projection::TupleNamedRegister { name, .. } => {
            bind(
                dictionary,
                self::name(names, *name)?.into(),
                *name,
                0,
                DictionaryRoleFlags::REGISTER_OR_NAMED,
            )
        }
        Projection::Member { qualifier, .. }
        | Projection::MemberShape { qualifier, .. }
        | Projection::TargetMember { qualifier, .. }
        | Projection::TupleQualifiedRegister { qualifier, .. } => bind(
            dictionary,
            name(names, *qualifier)?.into(),
            *qualifier,
            0,
            DictionaryRoleFlags::MEMBER,
        ),
        Projection::ValueProgram { source, .. }
        | Projection::RequiredValueProgram { source, .. } => bind_member(source, names, dictionary),
        _ => Ok(()),
    }
}
fn bind(
    dictionary: &mut BTreeMap<String, DictionaryBinding>,
    spelling: String,
    id: u16,
    qualifier: u8,
    roles: DictionaryRoleFlags,
) -> Result<(), String> {
    let spelling = spelling.to_ascii_lowercase();
    if let Some(previous) = dictionary.get_mut(&spelling) {
        if (previous.id, previous.qualifier) != (id, qualifier) {
            return Err(format!("conflicting binary lexical binding for {spelling}"));
        }
        previous.roles.0 |= roles.0;
    } else {
        dictionary.insert(
            spelling,
            DictionaryBinding {
                id,
                qualifier,
                roles,
            },
        );
    }
    Ok(())
}
fn name(names: &[String], id: u16) -> Result<&str, String> {
    names
        .get(usize::from(id))
        .map(String::as_str)
        .ok_or_else(|| "binary package name index out of bounds".into())
}

#[cfg(test)]
mod dictionary_role_contract_tests {
    use super::*;

    #[test]
    fn sequence_prefix_requires_one_trailing_payload_slot() {
        use vm::bytecode::{OP_EMIT_OPERAND as INPUT, OP_EMIT_U8 as BYTE, OP_END as END};
        assert!(sequence_prefix_table(&[BYTE, 0x90, INPUT, 0, END]));
        assert!(sequence_prefix_table(&[INPUT, 0, END]));
        for program in [
            vec![BYTE, 0x90, END],
            vec![INPUT, 1, END],
            vec![INPUT, 0, INPUT, 0, END],
            vec![INPUT, 0, BYTE, 0x90, END],
            vec![BYTE],
        ] {
            assert!(!sequence_prefix_table(&program), "{program:?}");
        }
    }

    #[test]
    fn member_shape_and_target_share_the_package_field_dictionary_role() {
        let names = vec!["Width".to_string()];
        let mut dictionary = BTreeMap::new();
        for projection in [
            Projection::MemberShape {
                operand: 1,
                qualifier: 0,
            },
            Projection::TargetMember {
                operand: 1,
                qualifier: 0,
            },
        ] {
            bind_member(&projection, &names, &mut dictionary).unwrap();
        }
        assert_eq!(dictionary.len(), 1);
        assert_eq!(dictionary["width"].id, 0);
        assert_eq!(dictionary["width"].roles, DictionaryRoleFlags::MEMBER);
    }

    #[test]
    fn dictionary_roles_merge_only_the_same_canonical_identity() {
        let mut dictionary = BTreeMap::new();
        for roles in [
            DictionaryRoleFlags::REGISTER_OR_NAMED,
            DictionaryRoleFlags::MEMBER,
            DictionaryRoleFlags::CONTEXTUAL,
        ] {
            bind(&mut dictionary, "Shared".into(), 7, 0, roles).unwrap();
        }
        assert_eq!(dictionary["shared"].roles.0, 3);
        for (id, qualifier) in [(8, 0), (7, 1)] {
            assert!(bind(
                &mut dictionary,
                "SHARED".into(),
                id,
                qualifier,
                DictionaryRoleFlags::CONTEXTUAL,
            )
            .is_err());
        }
        assert_eq!(dictionary["shared"].id, 7);
        assert_eq!(dictionary["shared"].qualifier, 0);
        assert_eq!(dictionary["shared"].roles.0, 3);
    }
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
    fn fixup_stage_writes_target_projection_and_rejects_invalid_program() {
        use package::{
            compile_fixup_program, EncodingEndian, FixupBase, FixupEncodingStep, FixupRange,
            FixupTransform, PortableRelocationKind, UnresolvedValuePolicy,
        };

        let bytes = compile_fixup_program(&[FixupEncodingStep {
            input: 0,
            width: 4,
            endian: EncodingEndian::Big,
            base: FixupBase::Value,
            range: FixupRange::BitPattern,
            unresolved: UnresolvedValuePolicy::Placeholder(0),
            relocation: PortableRelocationKind::Absolute,
            transform: FixupTransform::Identity,
        }])
        .unwrap();
        let mut programs = Programs::default();
        programs.add(2, 6, &[]).unwrap();
        programs.add(2, 4, &bytes).unwrap();
        programs.semantics.insert(1, 0);
        programs.semantics.insert(2, 1);
        let stages = [
            SemanticStage {
                program: Some(1),
                fixup: false,
                inputs: vec![Projection::Constant(0x41f9)],
            },
            SemanticStage {
                program: Some(2),
                fixup: true,
                inputs: vec![Projection::TargetExpression(0)],
            },
        ];
        let mut wire = vec![0; 32];
        assert_eq!(
            write_sequence(&mut wire, &stages, &programs).unwrap(),
            Some(32)
        );
        assert_eq!(wire[44], 2);
        assert_eq!(&wire[68..80], &[15, 0, 0, 0, 0, 0, 0, 0, 255, 255, 0, 0]);
        let member_stages = [
            stages[0].clone(),
            SemanticStage {
                program: Some(2),
                fixup: true,
                inputs: vec![Projection::TargetMember {
                    operand: 0,
                    qualifier: 3,
                }],
            },
        ];
        assert_eq!(
            write_sequence(&mut wire, &member_stages, &programs).unwrap(),
            Some(80)
        );
        assert_eq!(&wire[116..128], &[16, 0, 0, 3, 0, 0, 0, 0, 255, 255, 0, 0]);
        let original = wire.clone();
        programs.rows[1].version = 6;
        assert_eq!(write_sequence(&mut wire, &stages, &programs).unwrap(), None);
        assert_eq!(wire, original);
        assert_eq!(
            write_sequence(&mut wire, &member_stages, &programs).unwrap(),
            None
        );
        assert_eq!(wire, original);
        programs.rows[1].version = 4;
        programs.rows[1].bytes = &[];
        assert_eq!(
            write_sequence(&mut wire, &member_stages, &programs).unwrap(),
            None
        );
        assert_eq!(wire, original);
    }

    #[test]
    fn member_shape_wire_preserves_package_field_without_value_program() {
        let mut programs = Programs::default();
        programs.add(2, 6, &[]).unwrap();
        programs.semantics.insert(7, 0);
        let stages = [
            SemanticStage {
                program: None,
                fixup: false,
                inputs: vec![Projection::MemberShape {
                    operand: 1,
                    qualifier: 42,
                }],
            },
            SemanticStage {
                program: Some(7),
                fixup: false,
                inputs: vec![Projection::Constant(1)],
            },
        ];
        let mut wire = Vec::new();
        assert_eq!(
            write_sequence(&mut wire, &stages, &programs).unwrap(),
            Some(0)
        );
        assert_eq!(wire[0], 0);
        assert_eq!(&wire[24..36], &[19, 1, 0, 42, 0, 0, 0, 0, 255, 255, 0, 0]);
    }

    #[test]
    fn member_target_match_keeps_an_unsupported_barrier_without_partial_wire() {
        let mut programs = Programs::default();
        programs.add(2, 6, &[]).unwrap();
        programs.semantics.insert(7, 0);
        let stages = [
            SemanticStage {
                program: None,
                fixup: false,
                inputs: vec![Projection::TargetMember {
                    operand: 1,
                    qualifier: 42,
                }],
            },
            SemanticStage {
                program: Some(7),
                fixup: false,
                inputs: vec![Projection::Constant(1)],
            },
        ];
        let mut wire = vec![0xa5; 16];
        let original = wire.clone();
        assert_eq!(write_sequence(&mut wire, &stages, &programs).unwrap(), None);
        assert_eq!(wire, original);
    }

    #[test]
    fn tuple_class_proof_uses_only_bounded_canonical_match_conjuncts() {
        for (predicate, expected) in [
            ("indirect_tuple_reg0.item1.class8", 0x0900),
            ("indirect_tuple_reg1.item1.class0", 1),
            (
                "indirect_tuple_reg0.item1.class1,indirect_tuple_reg1.item1.class2",
                0x0203,
            ),
            ("future,indirect_tuple_reg0.item1.class1", 0x0200),
        ] {
            assert_eq!(
                required_tuple_classes(&format!(
                    "semv.sequence.v1:match:_@{predicate};encode:x@indirect_tuple_value0.item0,indirect_tuple_value1.item0"
                )),
                expected
            );
        }
        assert_eq!(required_tuple_classes("semv.inputs.v1:x@indirect_tuple_reg0.item0.class1,indirect_tuple_reg0.item1.class0,indirect_tuple_arity0.value2"), 0);
        for predicate in [
            "indirect_tuple_reg0.item2.class8",
            "indirect_tuple_reg2.item1.class8",
            "indirect_tuple_reg0.item1.class255",
            "indirect_tuple_reg0.item1.class256",
            "indirect_tuple_reg0.item1.class-1",
            "indirect_tuple_reg0.item1.class8.future",
            "indirect_tuple_reg+0.item1.class8",
            "indirect_tuple_reg0.item1.class+8",
            "indirect_tuple_reg.item1.class8",
            "indirect_tuple_reg0.item1.class",
        ] {
            assert_eq!(
                required_tuple_classes(&format!("semv.reject.v1:bad@{predicate}")),
                0
            );
        }
        assert_eq!(
            required_tuple_classes(
                "semv.sequence.v2:match:_@indirect_tuple_reg0.item1.class8;encode:x@expr0"
            ),
            0
        );
        assert_eq!(
            required_tuple_classes("semv.sequence.v1:encode:x@indirect_tuple_reg0.item1.class8"),
            0
        );
    }

    #[test]
    fn downgraded_sequence_keeps_only_leading_typed_match_facts() {
        let matched = SemanticStage {
            program: None,
            fixup: false,
            inputs: vec![
                Projection::TupleRegister {
                    operand: 0,
                    item: 1,
                    class: 8,
                },
                Projection::TupleArity { operand: 0 },
                Projection::TupleValue {
                    operand: 0,
                    item: 0,
                },
            ],
        };
        let later = SemanticStage {
            program: Some(1),
            fixup: true,
            inputs: vec![Projection::TargetMember {
                operand: 1,
                qualifier: 2,
            }],
        };
        assert_eq!(
            necessary_sequence_match(&[matched.clone(), later]),
            (10, 0x0900)
        );
        let mut register_pair = matched.clone();
        register_pair
            .inputs
            .retain(|input| !matches!(input, Projection::TupleValue { .. }));
        assert_eq!(necessary_sequence_match(&[register_pair.clone()]), (10, 0));
        let scalar_fixup = SemanticStage {
            program: Some(1),
            fixup: true,
            inputs: vec![Projection::TupleValue {
                operand: 0,
                item: 0,
            }],
        };
        assert_eq!(
            necessary_sequence_match(&[register_pair, scalar_fixup]),
            (10, 0x0900)
        );
        let mut encode_only = matched.clone();
        encode_only.program = Some(1);
        assert_eq!(necessary_sequence_match(&[encode_only]), (0, 0));
        let mut outside = matched.clone();
        outside.inputs[0] = Projection::TupleRegister {
            operand: 2,
            item: 1,
            class: 8,
        };
        assert_eq!(necessary_sequence_match(&[outside]), (10, 0));
        let mut untransportable_class = matched;
        untransportable_class.inputs[0] = Projection::TupleRegister {
            operand: 0,
            item: 1,
            class: 255,
        };
        assert_eq!(necessary_sequence_match(&[untransportable_class]), (10, 0));
    }

    #[test]
    fn exact_tuple_arity_proof_uses_only_canonical_match_conjuncts() {
        for (predicate, expected) in [
            ("indirect_tuple_arity0.value2", 10),
            ("indirect_tuple_arity1.value3", 0xb0),
            ("indirect_tuple_reg0.item1.class8,indirect_tuple_arity0.value3", 11),
            ("indirect_tuple_arity0.value2,indirect_tuple_arity1.value3", 0xba),
            ("indirect_tuple_arity0.value2,indirect_tuple_arity0.value2", 10),
            ("indirect_tuple_arity0.value2,indirect_tuple_arity0.value3", 0),
            ("indirect_tuple_reg0.item1.class8,indirect_tuple_arity0.value2,indirect_tuple_arity0.value3", 4),
            ("expr0,indirect_tuple_arity0.value2", 6),
        ] {
            assert_eq!(required_operand_forms(
                &format!("semv.sequence.v1:match:_@{predicate};encode:x@literal:0")
            ), expected, "{predicate}");
        }
        for predicate in [
            "indirect_tuple_arity0.value4",
            "indirect_tuple_arity0.valueX",
            "indirect_tuple_arity0.value+2",
            "indirect_tuple_arity+0.value2",
            "indirect_tuple_arity0.value2.future",
            "indirect_tuple_arity2.value2",
        ] {
            assert_eq!(
                required_operand_forms(&format!("semv.reject.v1:bad@{predicate}")),
                0,
                "{predicate}"
            );
        }
        for plan in [
            "semv.sequence.v1:encode:x@indirect_tuple_arity0.value2",
            "semv.sequence.v1:match:_@indirect_tuple_reg0.item1.class8;encode:x@indirect_tuple_arity0.value3",
            "semv.sequence.v1:match:_@indirect_tuple_reg0.item1.class8;encode:x@literal:0;fixup:y@indirect_tuple_arity0.value3",
        ] {
            assert_eq!(required_operand_forms(plan), if plan.contains("match:") { 4 } else { 0 }, "{plan}");
        }
    }

    #[test]
    fn downgraded_exact_tuple_arity_ignores_later_stages_and_conflicts() {
        let matched = SemanticStage {
            program: None,
            fixup: false,
            inputs: vec![Projection::TupleArityThree { operand: 0 }],
        };
        let later = SemanticStage {
            program: Some(1),
            fixup: false,
            inputs: vec![Projection::TupleArity { operand: 0 }],
        };
        assert_eq!(
            necessary_sequence_match(&[matched.clone(), later.clone()]),
            (11, 0)
        );
        assert_eq!(
            necessary_sequence_match(&[later.clone(), matched.clone()]),
            (0, 0)
        );
        let mut fixup = later.clone();
        fixup.fixup = true;
        assert_eq!(necessary_sequence_match(&[fixup]), (0, 0));
        let conflicting = SemanticStage {
            program: None,
            fixup: false,
            inputs: vec![Projection::TupleArity { operand: 0 }],
        };
        assert_eq!(
            necessary_sequence_match(&[matched.clone(), conflicting]),
            (0, 0)
        );
        assert_eq!(
            necessary_sequence_match(&[matched.clone(), matched]),
            (11, 0)
        );
    }

    #[test]
    fn unsupported_candidate_wire_carries_exact_match_arity() {
        let names = vec![
            "instruction".to_string(),
            "direct_register".to_string(),
            "semantic".to_string(),
            "semv.sequence.v1:match:_@indirect_tuple_arity0.value3;encode:future@expr0".to_string(),
        ];
        let candidate = NumericCandidate {
            state_guard: 0,
            mnemonic: 0,
            qualifier: None,
            shape: 1,
            mode: 2,
            owner_rank: 0,
            priority: 0,
            width_rank: 0,
            unstable_widen: false,
            member_excluded: 0,
            known_name_excluded: Vec::new(),
            recipe: CandidateRecipe::Unsupported { plan: 3 },
        };
        let mut wire = vec![0; ROW];
        write_candidate(&mut wire, 0, &candidate, &names, &Programs::default()).unwrap();
        assert_eq!(wire[5], 6);
        assert_eq!(wire[19], 11);
        let mut downgraded = candidate;
        downgraded.recipe = CandidateRecipe::SemanticSequence {
            stages: vec![
                SemanticStage {
                    program: None,
                    fixup: false,
                    inputs: vec![
                        Projection::TupleRegister {
                            operand: 0,
                            item: 1,
                            class: 8,
                        },
                        Projection::TupleArityThree { operand: 0 },
                    ],
                },
                SemanticStage {
                    program: Some(99),
                    fixup: false,
                    inputs: vec![Projection::Constant(0)],
                },
                SemanticStage {
                    program: Some(99),
                    fixup: true,
                    inputs: vec![Projection::TupleValue {
                        operand: 0,
                        item: 0,
                    }],
                },
            ],
        };
        let mut wire = vec![0; ROW];
        write_candidate(&mut wire, 0, &downgraded, &names, &Programs::default()).unwrap();
        assert_eq!(wire[5], 6);
        assert_eq!(wire[19], 11);
        assert_eq!(&wire[22..24], &[9, 0]);
    }

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
                    "semv.sequence.v1:match:_@{predicate};encode:x@indirect_tuple_value0.item0,indirect_tuple_value1.item0"
                )),
                0,
                "{predicate}"
            );
        }
    }

    #[test]
    fn filtered_tuple_arity_requires_an_executed_covering_leaf() {
        assert!(!filtered_tuple_arities_covered(&[
            Projection::TupleArity { operand: 0 },
            Projection::Constant(1),
        ]));
        assert!(!filtered_tuple_arities_covered(&[
            Projection::TupleArity { operand: 0 },
            Projection::TupleRegister {
                operand: 1,
                item: 0,
                class: 1
            },
        ]));
        assert!(filtered_tuple_arities_covered(&[
            Projection::TupleArity { operand: 0 },
            Projection::TupleRegister {
                operand: 0,
                item: 0,
                class: 1
            },
            Projection::TupleRegister {
                operand: 0,
                item: 1,
                class: 0
            },
        ]));
        assert!(filtered_tuple_arities_covered(&[
            Projection::TupleArity { operand: 0 },
            Projection::RequiredValueProgram {
                program: 7,
                source: Box::new(Projection::TupleValue {
                    operand: 0,
                    item: 0
                })
            },
        ]));
        assert!(filtered_tuple_arities_covered(&[
            Projection::TupleArity { operand: 0 },
            Projection::TupleNamedRegister {
                operand: 0,
                name: 7
            },
        ]));
        assert!(!filtered_tuple_arities_covered(&[
            Projection::TupleArityThree { operand: 0 },
            Projection::TupleNamedRegister {
                operand: 0,
                name: 7
            },
        ]));
        assert!(!filtered_tuple_arities_covered(&[
            Projection::TupleArity { operand: 0 },
            Projection::TupleArityThree { operand: 0 },
            Projection::TupleRegister {
                operand: 0,
                item: 0,
                class: 1
            },
        ]));
    }

    #[test]
    fn compact_call_and_single_class_mask_wire_preserve_numeric_fields() {
        let programs = Programs::default();
        let mut wire = Vec::new();
        assert!(write_projection(
            &mut wire,
            &Projection::CallArgumentRegister {
                operand: 1,
                argument: 1,
                class: 2,
            },
            &programs
        )
        .unwrap());
        assert_eq!(wire, [24, 1, 0, 2, 0, 0, 0, 0, 255, 255, 0, 1]);
        wire.clear();
        assert!(write_projection(
            &mut wire,
            &Projection::RegisterMask {
                operand: 0,
                first_class: 2,
                first_shift: 0,
                second_class: u16::MAX,
                second_shift: 0,
                reverse: false,
            },
            &programs
        )
        .unwrap());
        assert_eq!(wire, [18, 0, 0, 2, 255, 255, 0, 0, 255, 255, 0, 0]);
        wire.clear();
        assert!(!write_projection(
            &mut wire,
            &Projection::RegisterMask {
                operand: 0,
                first_class: 2,
                first_shift: 0,
                second_class: u16::MAX,
                second_shift: 1,
                reverse: false,
            },
            &programs
        )
        .unwrap());
        assert!(wire.is_empty());
    }

    #[test]
    fn tuple_wire_binds_register_first_pairs_and_rejects_invalid_bounds() {
        let mut programs = Programs::default();
        programs.qualifiers.insert(9, 2);
        let arities = tuple_arities([&Projection::TupleArity { operand: 0 }].into_iter());
        let mut wire = Vec::new();
        for projection in [
            Projection::TupleRegister {
                operand: 0,
                item: 0,
                class: 1,
            },
            Projection::TupleQualifiedRegister {
                operand: 0,
                item: 1,
                class: 0,
                qualifier: 9,
            },
        ] {
            assert!(write_bound_projection(&mut wire, &projection, &programs, &arities).unwrap());
        }
        assert_eq!(wire[0], 11);
        assert_eq!(&wire[4..8], &65536i32.to_be_bytes());
        assert_eq!(&wire[10..12], &[2, 0]);
        assert_eq!(wire[12], 13);
        assert_eq!(&wire[16..20], &65538i32.to_be_bytes());
        assert_eq!(&wire[22..24], &[2, 1]);
        let original = wire.clone();
        for item in [2, 3, 255] {
            let projection = Projection::TupleRegister {
                operand: 0,
                item,
                class: 1,
            };
            assert!(!write_bound_projection(&mut wire, &projection, &programs, &arities).unwrap());
            assert_eq!(wire, original);
        }
        let conflicting = tuple_arities(
            [
                &Projection::TupleArity { operand: 0 },
                &Projection::TupleArityThree { operand: 0 },
            ]
            .into_iter(),
        );
        assert!(!write_bound_projection(
            &mut wire,
            &Projection::TupleValue {
                operand: 0,
                item: 0
            },
            &programs,
            &conflicting
        )
        .unwrap());
        assert_eq!(wire, original);
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
                fixup: false,
                inputs: vec![
                    Projection::TupleArityThree { operand: 0 },
                    Projection::TupleQualifiedRegister {
                        operand: 0,
                        item: 2,
                        class: 4,
                        qualifier: 9,
                    },
                ],
            },
            SemanticStage {
                program: Some(7),
                fixup: false,
                inputs: vec![
                    Projection::TupleRegister {
                        operand: 0,
                        item: 1,
                        class: 1,
                    },
                    Projection::TupleValue {
                        operand: 0,
                        item: 0,
                    },
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
        assert_eq!(&wire[120..124], &65538i32.to_be_bytes());
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
                fixup: false,
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

    #[test]
    fn identity_scale_wire_preserves_predicate_identity_and_tuple_bounds() {
        let mut programs = Programs::default();
        programs.add(2, 6, &[]).unwrap();
        programs.semantics.insert(7, 0);
        programs.qualifiers.insert(9, 2);
        let stages = [
            SemanticStage {
                program: None,
                fixup: false,
                inputs: vec![
                    Projection::TupleQualifiedRegister {
                        operand: 0,
                        item: 1,
                        class: 4,
                        qualifier: 9,
                    },
                    Projection::TupleIdentityScale {
                        operand: 0,
                        item: 1,
                    },
                    Projection::TupleArity { operand: 0 },
                ],
            },
            SemanticStage {
                program: Some(7),
                fixup: false,
                inputs: vec![Projection::TupleQualifiedRegister {
                    operand: 0,
                    item: 1,
                    class: 4,
                    qualifier: 9,
                }],
            },
        ];
        let mut wire = vec![0; 80];
        assert_eq!(
            write_sequence(&mut wire, &stages, &programs).unwrap(),
            Some(80)
        );
        assert_eq!(&wire[84..86], &3u16.to_be_bytes());
        assert_eq!(&wire[104..108], &[13, 0, 0, 4]);
        assert_eq!(&wire[108..112], &[0, 1, 0, 2]);
        assert_eq!(&wire[114..116], &[2, 1]);
        assert_eq!(&wire[116..128], &[23, 0, 0, 1, 0, 0, 0, 0, 255, 255, 2, 1]);
        assert_eq!(&wire[128..132], &[14, 0, 0, 2]);
        assert_eq!(&wire[140..144], &[13, 0, 0, 4]);
        assert_eq!(&wire[144..148], &[0, 1, 0, 2]);
        assert_eq!(&wire[150..152], &[2, 1]);
        assert!(filtered_tuple_arities_covered(&stages[0].inputs));
        let original = wire.clone();
        for item in [2, 3, 255] {
            let mut invalid = stages.clone();
            invalid[0].inputs[1] = Projection::TupleIdentityScale { operand: 0, item };
            assert_eq!(
                write_sequence(&mut wire, &invalid, &programs).unwrap(),
                None
            );
            assert_eq!(wire, original, "invalid identity item must roll back");
        }
        let conflicting = tuple_arities(
            [
                &Projection::TupleArity { operand: 0 },
                &Projection::TupleArityThree { operand: 0 },
            ]
            .into_iter(),
        );
        assert!(!write_bound_projection(
            &mut wire,
            &Projection::TupleIdentityScale {
                operand: 0,
                item: 1
            },
            &programs,
            &conflicting,
        )
        .unwrap());
        assert_eq!(wire, original);
    }
}

#[cfg(test)]
#[path = "tests/compact_package_inventory.rs"]
mod compact_package_inventory;

fn write_state_plan(plan: &vm::binary_source_package::NumericStatePlan) -> Result<Vec<u8>, String> {
    let mut out = vec![0; 20];
    set_word(&mut out, 0, word(plan.defaults.len())?);
    set_word(&mut out, 2, word(plan.directives.len())?);
    set_word(&mut out, 4, word(plan.guards.len())?);
    set_long(&mut out, 8, 20);
    for value in &plan.defaults {
        out.extend_from_slice(&value.to_be_bytes());
    }
    let directives = out.len();
    set_long(&mut out, 12, long(directives)?);
    reserve(&mut out, plan.directives.len(), 12)?;
    let guards = out.len();
    set_long(&mut out, 16, long(guards)?);
    reserve(&mut out, plan.guards.len(), 8)?;
    for (index, directive) in plan.directives.iter().enumerate() {
        let row = directives + index * 12;
        set_word(&mut out, row, directive.head);
        set_word(&mut out, row + 2, directive.key);
        set_word(&mut out, row + 4, word(directive.arguments.len())?);
        let offset = long(out.len())?;
        set_long(&mut out, row + 8, offset);
        for argument in &directive.arguments {
            push_word(&mut out, argument.kind);
            push_word(&mut out, u16::from(argument.allowed));
            out.extend_from_slice(&argument.matched.to_be_bytes());
            out.extend_from_slice(&argument.value.to_be_bytes());
        }
    }
    for (index, guard) in plan.guards.iter().enumerate() {
        let row = guards + index * 8;
        set_word(&mut out, row, word(guard.clauses.len())?);
        let offset = long(out.len())?;
        set_long(&mut out, row + 4, offset);
        let clauses = out.len();
        reserve(&mut out, guard.clauses.len(), 12)?;
        for (index, clause) in guard.clauses.iter().enumerate() {
            let row = clauses + index * 12;
            set_word(&mut out, row, clause.key);
            set_word(&mut out, row + 2, word(clause.values.len())?);
            set_word(&mut out, row + 4, u16::from(clause.reject));
            let offset = long(out.len())?;
            set_long(&mut out, row + 8, offset);
            for value in &clause.values {
                out.extend_from_slice(&value.to_be_bytes());
            }
        }
    }
    Ok(out)
}

#[cfg(test)]
mod state_wire_tests {
    use super::*;
    use vm::binary_source_package::{
        NumericStateArgument, NumericStateClause, NumericStateDirective, NumericStateGuard,
        NumericStatePlan,
    };
    #[test]
    fn numeric_state_wire_offsets_are_plan_relative() {
        let mut plan = NumericStatePlan::default();
        plan.defaults = vec![4, 0];
        plan.directives.push(NumericStateDirective {
            head: 17,
            key: 1,
            arguments: vec![NumericStateArgument {
                kind: 0,
                allowed: true,
                matched: 18,
                value: 1,
            }],
        });
        plan.guards.push(NumericStateGuard {
            clauses: vec![NumericStateClause {
                reject: true,
                key: 0,
                values: vec![1, 2],
            }],
        });
        let wire = write_state_plan(&plan).unwrap();
        let word = |offset| u16::from_be_bytes(wire[offset..offset + 2].try_into().unwrap());
        let long = |offset| u32::from_be_bytes(wire[offset..offset + 4].try_into().unwrap());
        assert_eq!((word(0), word(2), word(4), word(6)), (2, 1, 1, 0));
        assert_eq!((long(8), long(12), long(16)), (20, 28, 40));
        assert_eq!((long(20), long(24)), (4, 0));
        assert_eq!(
            (word(28), word(30), word(32), word(34), long(36)),
            (17, 1, 1, 0, 48)
        );
        assert_eq!((word(40), word(42), long(44)), (1, 0, 60));
        assert_eq!((word(48), word(50), long(52), long(56)), (0, 1, 18, 1));
        assert_eq!(
            (word(60), word(62), word(64), word(66), long(68)),
            (0, 2, 1, 0, 72)
        );
        assert_eq!((long(72), long(76)), (1, 2));
        assert_eq!(wire.len(), 80);
    }
}
