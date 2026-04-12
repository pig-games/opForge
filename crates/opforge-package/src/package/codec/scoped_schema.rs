use std::collections::VecDeque;

use super::*;

pub(super) fn encode_scoped_schema_chunk<T: ScopedSchemaEntry>(
    entries: &[T],
) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut out = Vec::new();
    write_u32(&mut out, u32_count(entries.len(), T::COUNT_LABEL)?);
    for entry in entries {
        encode_scoped_owner(&mut out, T::CHUNK, entry.owner())?;
        let field_values = entry.field_values();
        for (spec, value) in T::FIELD_SPECS.iter().zip(field_values.into_iter()) {
            encode_field(&mut out, T::CHUNK, spec, value)?;
        }
    }
    Ok(out)
}

pub(super) fn decode_scoped_schema_chunk<T: ScopedSchemaEntry>(
    bytes: &[u8],
) -> Result<Vec<T>, OpcpuCodecError> {
    let mut cur = Decoder::new(bytes, T::CHUNK);
    let count = read_bounded_count(&mut cur, 1, T::ENTRY_KIND)?;
    let mut entries = Vec::with_capacity(count);
    for _ in 0..count {
        let owner = decode_scoped_owner(&mut cur, T::CHUNK)?;
        let mut fields = Vec::with_capacity(T::FIELD_SPECS.len());
        for spec in T::FIELD_SPECS {
            fields.push(decode_field(&mut cur, T::CHUNK, spec)?);
        }
        let entry = T::from_decoded(owner, DecodedFields::new(fields))?;
        T::validate_decoded(&entry)?;
        entries.push(entry);
    }
    cur.finish()?;
    Ok(entries)
}

pub(super) trait ScopedSchemaEntry: Sized {
    const CHUNK: &'static str;
    const ENTRY_KIND: &'static str;
    const COUNT_LABEL: &'static str;
    const FIELD_SPECS: &'static [FieldSpec];

    fn owner(&self) -> &ScopedOwner;
    fn field_values(&self) -> Vec<FieldValue<'_>>;
    fn from_decoded(owner: ScopedOwner, fields: DecodedFields) -> Result<Self, OpcpuCodecError>;

    fn validate_decoded(_entry: &Self) -> Result<(), OpcpuCodecError> {
        Ok(())
    }
}

#[derive(Clone, Copy)]
pub(super) enum FieldSpec {
    String,
    Bytes {
        len_label: &'static str,
        value_label: &'static str,
    },
    U32List {
        count_label: &'static str,
        entry_label: &'static str,
    },
    U16,
    U32,
    Bool {
        label: &'static str,
    },
    U8,
}

pub(super) enum FieldValue<'a> {
    String(&'a str),
    Bytes(&'a [u8]),
    U32List(&'a [u32]),
    U16(u16),
    U32(u32),
    Bool(bool),
    U8(u8),
}

enum DecodedField {
    String(String),
    Bytes(Vec<u8>),
    U32List(Vec<u32>),
    U16(u16),
    U32(u32),
    Bool(bool),
    U8(u8),
}

pub(super) struct DecodedFields {
    fields: VecDeque<DecodedField>,
}

impl DecodedFields {
    fn new(fields: Vec<DecodedField>) -> Self {
        Self {
            fields: VecDeque::from(fields),
        }
    }

    pub(super) fn next_string(&mut self, chunk: &'static str) -> Result<String, OpcpuCodecError> {
        match self.fields.pop_front() {
            Some(DecodedField::String(value)) => Ok(value),
            _ => Err(internal_schema_error(chunk, "string")),
        }
    }

    pub(super) fn next_bytes(&mut self, chunk: &'static str) -> Result<Vec<u8>, OpcpuCodecError> {
        match self.fields.pop_front() {
            Some(DecodedField::Bytes(value)) => Ok(value),
            _ => Err(internal_schema_error(chunk, "bytes")),
        }
    }

    pub(super) fn next_u32_list(
        &mut self,
        chunk: &'static str,
    ) -> Result<Vec<u32>, OpcpuCodecError> {
        match self.fields.pop_front() {
            Some(DecodedField::U32List(value)) => Ok(value),
            _ => Err(internal_schema_error(chunk, "u32 list")),
        }
    }

    pub(super) fn next_u16(&mut self, chunk: &'static str) -> Result<u16, OpcpuCodecError> {
        match self.fields.pop_front() {
            Some(DecodedField::U16(value)) => Ok(value),
            _ => Err(internal_schema_error(chunk, "u16")),
        }
    }

    pub(super) fn next_u32(&mut self, chunk: &'static str) -> Result<u32, OpcpuCodecError> {
        match self.fields.pop_front() {
            Some(DecodedField::U32(value)) => Ok(value),
            _ => Err(internal_schema_error(chunk, "u32")),
        }
    }

    pub(super) fn next_bool(&mut self, chunk: &'static str) -> Result<bool, OpcpuCodecError> {
        match self.fields.pop_front() {
            Some(DecodedField::Bool(value)) => Ok(value),
            _ => Err(internal_schema_error(chunk, "bool")),
        }
    }

    pub(super) fn next_u8(&mut self, chunk: &'static str) -> Result<u8, OpcpuCodecError> {
        match self.fields.pop_front() {
            Some(DecodedField::U8(value)) => Ok(value),
            _ => Err(internal_schema_error(chunk, "u8")),
        }
    }
}

impl ScopedSchemaEntry for ScopedRegisterDescriptor {
    const CHUNK: &'static str = "REGS";
    const ENTRY_KIND: &'static str = "register entry";
    const COUNT_LABEL: &'static str = "REGS count";
    const FIELD_SPECS: &'static [FieldSpec] = &[FieldSpec::String];

    fn owner(&self) -> &ScopedOwner {
        &self.owner
    }

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![FieldValue::String(&self.id)]
    }

    fn from_decoded(
        owner: ScopedOwner,
        mut fields: DecodedFields,
    ) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            owner,
            id: fields.next_string(Self::CHUNK)?,
        })
    }
}

impl ScopedSchemaEntry for ScopedFormDescriptor {
    const CHUNK: &'static str = "FORM";
    const ENTRY_KIND: &'static str = "form entry";
    const COUNT_LABEL: &'static str = "FORM count";
    const FIELD_SPECS: &'static [FieldSpec] = &[FieldSpec::String];

    fn owner(&self) -> &ScopedOwner {
        &self.owner
    }

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![FieldValue::String(&self.mnemonic)]
    }

    fn from_decoded(
        owner: ScopedOwner,
        mut fields: DecodedFields,
    ) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            owner,
            mnemonic: fields.next_string(Self::CHUNK)?,
        })
    }
}

impl ScopedSchemaEntry for VmProgramDescriptor {
    const CHUNK: &'static str = "TABL";
    const ENTRY_KIND: &'static str = "table entry";
    const COUNT_LABEL: &'static str = "TABL count";
    const FIELD_SPECS: &'static [FieldSpec] = &[
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::Bytes {
            len_label: "TABL program byte length",
            value_label: "program bytes",
        },
    ];

    fn owner(&self) -> &ScopedOwner {
        &self.owner
    }

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![
            FieldValue::String(&self.mnemonic),
            FieldValue::String(&self.mode_key),
            FieldValue::Bytes(&self.program),
        ]
    }

    fn from_decoded(
        owner: ScopedOwner,
        mut fields: DecodedFields,
    ) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            owner,
            mnemonic: fields.next_string(Self::CHUNK)?,
            mode_key: fields.next_string(Self::CHUNK)?,
            program: fields.next_bytes(Self::CHUNK)?,
        })
    }
}

impl ScopedSchemaEntry for ModeSelectorDescriptor {
    const CHUNK: &'static str = "MSEL";
    const ENTRY_KIND: &'static str = "mode selector entry";
    const COUNT_LABEL: &'static str = "MSEL count";
    const FIELD_SPECS: &'static [FieldSpec] = &[
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::U16,
        FieldSpec::Bool {
            label: "unstable_widen",
        },
        FieldSpec::U8,
    ];

    fn owner(&self) -> &ScopedOwner {
        &self.owner
    }

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![
            FieldValue::String(&self.mnemonic),
            FieldValue::String(&self.shape_key),
            FieldValue::String(&self.mode_key),
            FieldValue::String(&self.operand_plan),
            FieldValue::U16(self.priority),
            FieldValue::Bool(self.unstable_widen),
            FieldValue::U8(self.width_rank),
        ]
    }

    fn from_decoded(
        owner: ScopedOwner,
        mut fields: DecodedFields,
    ) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            owner,
            mnemonic: fields.next_string(Self::CHUNK)?,
            shape_key: fields.next_string(Self::CHUNK)?,
            mode_key: fields.next_string(Self::CHUNK)?,
            operand_plan: fields.next_string(Self::CHUNK)?,
            priority: fields.next_u16(Self::CHUNK)?,
            unstable_widen: fields.next_bool(Self::CHUNK)?,
            width_rank: fields.next_u8(Self::CHUNK)?,
        })
    }
}

impl ScopedSchemaEntry for TokenizerVmProgramDescriptor {
    const CHUNK: &'static str = "TKVM";
    const ENTRY_KIND: &'static str = "tokenizer VM entry";
    const COUNT_LABEL: &'static str = "TKVM count";
    const FIELD_SPECS: &'static [FieldSpec] = &[
        FieldSpec::U16,
        FieldSpec::U16,
        FieldSpec::U32List {
            count_label: "TKVM state_entry_offsets count",
            entry_label: "state-entry offset",
        },
        FieldSpec::U32,
        FieldSpec::U32,
        FieldSpec::U32,
        FieldSpec::U32,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::Bytes {
            len_label: "TKVM program byte length",
            value_label: "tokenizer vm program",
        },
    ];

    fn owner(&self) -> &ScopedOwner {
        &self.owner
    }

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![
            FieldValue::U16(self.opcode_version),
            FieldValue::U16(self.start_state),
            FieldValue::U32List(&self.state_entry_offsets),
            FieldValue::U32(self.limits.max_steps_per_line),
            FieldValue::U32(self.limits.max_tokens_per_line),
            FieldValue::U32(self.limits.max_lexeme_bytes),
            FieldValue::U32(self.limits.max_errors_per_line),
            FieldValue::String(&self.diagnostics.invalid_char),
            FieldValue::String(&self.diagnostics.unterminated_string),
            FieldValue::String(&self.diagnostics.step_limit_exceeded),
            FieldValue::String(&self.diagnostics.token_limit_exceeded),
            FieldValue::String(&self.diagnostics.lexeme_limit_exceeded),
            FieldValue::String(&self.diagnostics.error_limit_exceeded),
            FieldValue::Bytes(&self.program),
        ]
    }

    fn from_decoded(
        owner: ScopedOwner,
        mut fields: DecodedFields,
    ) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            owner,
            opcode_version: fields.next_u16(Self::CHUNK)?,
            start_state: fields.next_u16(Self::CHUNK)?,
            state_entry_offsets: fields.next_u32_list(Self::CHUNK)?,
            limits: TokenizerVmLimits {
                max_steps_per_line: fields.next_u32(Self::CHUNK)?,
                max_tokens_per_line: fields.next_u32(Self::CHUNK)?,
                max_lexeme_bytes: fields.next_u32(Self::CHUNK)?,
                max_errors_per_line: fields.next_u32(Self::CHUNK)?,
            },
            diagnostics: TokenizerVmDiagnosticMap {
                invalid_char: fields.next_string(Self::CHUNK)?,
                unterminated_string: fields.next_string(Self::CHUNK)?,
                step_limit_exceeded: fields.next_string(Self::CHUNK)?,
                token_limit_exceeded: fields.next_string(Self::CHUNK)?,
                lexeme_limit_exceeded: fields.next_string(Self::CHUNK)?,
                error_limit_exceeded: fields.next_string(Self::CHUNK)?,
            },
            program: fields.next_bytes(Self::CHUNK)?,
        })
    }
}

impl ScopedSchemaEntry for ParserContractDescriptor {
    const CHUNK: &'static str = "PARS";
    const ENTRY_KIND: &'static str = "parser contract entry";
    const COUNT_LABEL: &'static str = "PARS count";
    const FIELD_SPECS: &'static [FieldSpec] = &[
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::U16,
        FieldSpec::U32,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
    ];

    fn owner(&self) -> &ScopedOwner {
        &self.owner
    }

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![
            FieldValue::String(&self.grammar_id),
            FieldValue::String(&self.ast_schema_id),
            FieldValue::U16(self.opcode_version),
            FieldValue::U32(self.max_ast_nodes_per_line),
            FieldValue::String(&self.diagnostics.unexpected_token),
            FieldValue::String(&self.diagnostics.expected_expression),
            FieldValue::String(&self.diagnostics.expected_operand),
            FieldValue::String(&self.diagnostics.invalid_statement),
        ]
    }

    fn from_decoded(
        owner: ScopedOwner,
        mut fields: DecodedFields,
    ) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            owner,
            grammar_id: fields.next_string(Self::CHUNK)?,
            ast_schema_id: fields.next_string(Self::CHUNK)?,
            opcode_version: fields.next_u16(Self::CHUNK)?,
            max_ast_nodes_per_line: fields.next_u32(Self::CHUNK)?,
            diagnostics: ParserDiagnosticMap {
                unexpected_token: fields.next_string(Self::CHUNK)?,
                expected_expression: fields.next_string(Self::CHUNK)?,
                expected_operand: fields.next_string(Self::CHUNK)?,
                invalid_statement: fields.next_string(Self::CHUNK)?,
            },
        })
    }
}

impl ScopedSchemaEntry for ParserVmProgramDescriptor {
    const CHUNK: &'static str = "PRVM";
    const ENTRY_KIND: &'static str = "parser VM entry";
    const COUNT_LABEL: &'static str = "PRVM count";
    const FIELD_SPECS: &'static [FieldSpec] = &[
        FieldSpec::U16,
        FieldSpec::Bytes {
            len_label: "PRVM program byte length",
            value_label: "parser vm program",
        },
    ];

    fn owner(&self) -> &ScopedOwner {
        &self.owner
    }

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![
            FieldValue::U16(self.opcode_version),
            FieldValue::Bytes(&self.program),
        ]
    }

    fn from_decoded(
        owner: ScopedOwner,
        mut fields: DecodedFields,
    ) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            owner,
            opcode_version: fields.next_u16(Self::CHUNK)?,
            program: fields.next_bytes(Self::CHUNK)?,
        })
    }
}

impl ScopedSchemaEntry for ExprContractDescriptor {
    const CHUNK: &'static str = "EXPR";
    const ENTRY_KIND: &'static str = "expression contract entry";
    const COUNT_LABEL: &'static str = "EXPR count";
    const FIELD_SPECS: &'static [FieldSpec] = &[
        FieldSpec::U16,
        FieldSpec::U32,
        FieldSpec::U32,
        FieldSpec::U32,
        FieldSpec::U32,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::String,
    ];

    fn owner(&self) -> &ScopedOwner {
        &self.owner
    }

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![
            FieldValue::U16(self.opcode_version),
            FieldValue::U32(self.max_program_bytes),
            FieldValue::U32(self.max_stack_depth),
            FieldValue::U32(self.max_symbol_refs),
            FieldValue::U32(self.max_eval_steps),
            FieldValue::String(&self.diagnostics.invalid_opcode),
            FieldValue::String(&self.diagnostics.stack_underflow),
            FieldValue::String(&self.diagnostics.stack_depth_exceeded),
            FieldValue::String(&self.diagnostics.unknown_symbol),
            FieldValue::String(&self.diagnostics.eval_failure),
            FieldValue::String(&self.diagnostics.unsupported_feature),
            FieldValue::String(&self.diagnostics.budget_exceeded),
            FieldValue::String(&self.diagnostics.invalid_program),
        ]
    }

    fn from_decoded(
        owner: ScopedOwner,
        mut fields: DecodedFields,
    ) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            owner,
            opcode_version: fields.next_u16(Self::CHUNK)?,
            max_program_bytes: fields.next_u32(Self::CHUNK)?,
            max_stack_depth: fields.next_u32(Self::CHUNK)?,
            max_symbol_refs: fields.next_u32(Self::CHUNK)?,
            max_eval_steps: fields.next_u32(Self::CHUNK)?,
            diagnostics: ExprDiagnosticMap {
                invalid_opcode: fields.next_string(Self::CHUNK)?,
                stack_underflow: fields.next_string(Self::CHUNK)?,
                stack_depth_exceeded: fields.next_string(Self::CHUNK)?,
                unknown_symbol: fields.next_string(Self::CHUNK)?,
                eval_failure: fields.next_string(Self::CHUNK)?,
                unsupported_feature: fields.next_string(Self::CHUNK)?,
                budget_exceeded: fields.next_string(Self::CHUNK)?,
                invalid_program: fields.next_string(Self::CHUNK)?,
            },
        })
    }

    fn validate_decoded(entry: &Self) -> Result<(), OpcpuCodecError> {
        validate_expr_contract_descriptor(entry)
    }
}

impl ScopedSchemaEntry for ExprParserContractDescriptor {
    const CHUNK: &'static str = "EXPP";
    const ENTRY_KIND: &'static str = "expression parser contract entry";
    const COUNT_LABEL: &'static str = "EXPP count";
    const FIELD_SPECS: &'static [FieldSpec] = &[FieldSpec::U16, FieldSpec::String];

    fn owner(&self) -> &ScopedOwner {
        &self.owner
    }

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![
            FieldValue::U16(self.opcode_version),
            FieldValue::String(&self.diagnostics.invalid_expression_program),
        ]
    }

    fn from_decoded(
        owner: ScopedOwner,
        mut fields: DecodedFields,
    ) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            owner,
            opcode_version: fields.next_u16(Self::CHUNK)?,
            diagnostics: ExprParserDiagnosticMap {
                invalid_expression_program: fields.next_string(Self::CHUNK)?,
            },
        })
    }

    fn validate_decoded(entry: &Self) -> Result<(), OpcpuCodecError> {
        validate_expr_parser_contract_descriptor(entry)
    }
}

fn encode_field(
    out: &mut Vec<u8>,
    chunk: &'static str,
    spec: &FieldSpec,
    value: FieldValue<'_>,
) -> Result<(), OpcpuCodecError> {
    match (spec, value) {
        (FieldSpec::String, FieldValue::String(value)) => write_string(out, chunk, value),
        (
            FieldSpec::Bytes {
                len_label,
                value_label: _,
            },
            FieldValue::Bytes(value),
        ) => {
            write_u32(out, u32_count(value.len(), len_label)?);
            out.extend_from_slice(value);
            Ok(())
        }
        (
            FieldSpec::U32List {
                count_label,
                entry_label: _,
            },
            FieldValue::U32List(values),
        ) => {
            write_u32(out, u32_count(values.len(), count_label)?);
            for value in values {
                write_u32(out, *value);
            }
            Ok(())
        }
        (FieldSpec::U16, FieldValue::U16(value)) => {
            write_u16(out, value);
            Ok(())
        }
        (FieldSpec::U32, FieldValue::U32(value)) => {
            write_u32(out, value);
            Ok(())
        }
        (FieldSpec::Bool { .. }, FieldValue::Bool(value)) => {
            out.push(u8::from(value));
            Ok(())
        }
        (FieldSpec::U8, FieldValue::U8(value)) => {
            out.push(value);
            Ok(())
        }
        _ => Err(internal_schema_error(chunk, "encode field mismatch")),
    }
}

fn decode_field(
    cur: &mut Decoder<'_>,
    chunk: &'static str,
    spec: &FieldSpec,
) -> Result<DecodedField, OpcpuCodecError> {
    match spec {
        FieldSpec::String => Ok(DecodedField::String(cur.read_string()?)),
        FieldSpec::Bytes { value_label, .. } => {
            let byte_count = cur.read_u32()? as usize;
            Ok(DecodedField::Bytes(
                cur.read_exact(byte_count, value_label)?.to_vec(),
            ))
        }
        FieldSpec::U32List { entry_label, .. } => {
            let count = read_bounded_count(cur, 4, entry_label)?;
            let mut values = Vec::with_capacity(count);
            for _ in 0..count {
                values.push(cur.read_u32()?);
            }
            Ok(DecodedField::U32List(values))
        }
        FieldSpec::U16 => Ok(DecodedField::U16(cur.read_u16()?)),
        FieldSpec::U32 => Ok(DecodedField::U32(cur.read_u32()?)),
        FieldSpec::Bool { label } => match cur.read_u8()? {
            0 => Ok(DecodedField::Bool(false)),
            1 => Ok(DecodedField::Bool(true)),
            other => Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: chunk.to_string(),
                detail: format!("invalid bool flag for {}: {}", label, other),
            }),
        },
        FieldSpec::U8 => Ok(DecodedField::U8(cur.read_u8()?)),
    }
}

fn internal_schema_error(chunk: &'static str, detail: &'static str) -> OpcpuCodecError {
    OpcpuCodecError::InvalidChunkFormat {
        chunk: chunk.to_string(),
        detail: format!("internal scoped schema error: {}", detail),
    }
}
