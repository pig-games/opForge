use std::collections::VecDeque;

use super::*;

pub(super) fn encode_simple_schema_chunk<T: SimpleSchemaEntry>(
    entries: &[T],
) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut out = Vec::new();
    write_u32(&mut out, u32_count(entries.len(), T::COUNT_LABEL)?);
    for entry in entries {
        encode_schema_fields(&mut out, T::CHUNK, T::FIELD_SPECS, entry.field_values())?;
    }
    Ok(out)
}

pub(super) fn decode_simple_schema_chunk<T: SimpleSchemaEntry>(
    bytes: &[u8],
) -> Result<Vec<T>, OpcpuCodecError> {
    let mut cur = Decoder::new(bytes, T::CHUNK);
    let count = read_bounded_count(&mut cur, T::MIN_RECORD_BYTES, T::ENTRY_KIND)?;
    let mut entries = Vec::with_capacity(count);
    for _ in 0..count {
        let entry = T::from_decoded(decode_schema_fields(&mut cur, T::CHUNK, T::FIELD_SPECS)?)?;
        T::validate_decoded(&entry)?;
        entries.push(entry);
    }
    cur.finish()?;
    Ok(entries)
}

pub(super) fn encode_simple_schema_record<T: SimpleSchemaRecord>(
    entry: &T,
) -> Result<Vec<u8>, OpcpuCodecError> {
    let mut out = Vec::new();
    encode_schema_fields(&mut out, T::CHUNK, T::FIELD_SPECS, entry.field_values())?;
    Ok(out)
}

pub(super) fn decode_simple_schema_record<T: SimpleSchemaRecord>(
    bytes: &[u8],
) -> Result<T, OpcpuCodecError> {
    let mut cur = Decoder::new(bytes, T::CHUNK);
    let entry = T::from_decoded(decode_schema_fields(&mut cur, T::CHUNK, T::FIELD_SPECS)?)?;
    T::validate_decoded(&entry)?;
    cur.finish()?;
    Ok(entry)
}

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

pub(super) trait SimpleSchemaEntry: Sized {
    const CHUNK: &'static str;
    const ENTRY_KIND: &'static str;
    const COUNT_LABEL: &'static str;
    const MIN_RECORD_BYTES: usize;
    const FIELD_SPECS: &'static [FieldSpec];

    fn field_values(&self) -> Vec<FieldValue<'_>>;
    fn from_decoded(fields: DecodedFields) -> Result<Self, OpcpuCodecError>;

    fn validate_decoded(_entry: &Self) -> Result<(), OpcpuCodecError> {
        Ok(())
    }
}

pub(super) trait SimpleSchemaRecord: Sized {
    const CHUNK: &'static str;
    const FIELD_SPECS: &'static [FieldSpec];

    fn field_values(&self) -> Vec<FieldValue<'_>>;
    fn from_decoded(fields: DecodedFields) -> Result<Self, OpcpuCodecError>;

    fn validate_decoded(_entry: &Self) -> Result<(), OpcpuCodecError> {
        Ok(())
    }
}

#[derive(Clone, Copy)]
pub(super) enum FieldSpec {
    String,
    OptionalString {
        label: &'static str,
    },
    TokenPolicyLexicalTail,
    Bytes {
        len_label: &'static str,
        value_label: &'static str,
    },
    OptionalStringList {
        label: &'static str,
        count_label: &'static str,
        entry_label: &'static str,
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
    OptionalString(Option<&'a str>),
    TokenPolicyLexicalTail(TokenPolicyLexicalTailRef<'a>),
    Bytes(&'a [u8]),
    OptionalStringList(Option<&'a [String]>),
    U32List(&'a [u32]),
    U16(u16),
    U32(u32),
    Bool(bool),
    U8(u8),
}

enum DecodedField {
    String(String),
    OptionalString(Option<String>),
    TokenPolicyLexicalDefaults(TokenPolicyLexicalDefaults),
    Bytes(Vec<u8>),
    OptionalStringList(Option<Vec<String>>),
    U32List(Vec<u32>),
    U16(u16),
    U32(u32),
    Bool(bool),
    U8(u8),
}

pub(super) struct DecodedFields {
    fields: VecDeque<DecodedField>,
}

pub(super) struct TokenPolicyLexicalTailRef<'a> {
    comment_prefix: &'a str,
    quote_chars: &'a str,
    escape_char: Option<char>,
    number_prefix_chars: &'a str,
    number_suffix_binary: &'a str,
    number_suffix_octal: &'a str,
    number_suffix_decimal: &'a str,
    number_suffix_hex: &'a str,
    operator_chars: &'a str,
    multi_char_operators: &'a [String],
}

impl<'a> TokenPolicyLexicalTailRef<'a> {
    fn from_policy(policy: &'a TokenPolicyDescriptor) -> Self {
        Self {
            comment_prefix: &policy.comment_prefix,
            quote_chars: &policy.quote_chars,
            escape_char: policy.escape_char,
            number_prefix_chars: &policy.number_prefix_chars,
            number_suffix_binary: &policy.number_suffix_binary,
            number_suffix_octal: &policy.number_suffix_octal,
            number_suffix_decimal: &policy.number_suffix_decimal,
            number_suffix_hex: &policy.number_suffix_hex,
            operator_chars: &policy.operator_chars,
            multi_char_operators: &policy.multi_char_operators,
        }
    }
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

    pub(super) fn next_optional_string(
        &mut self,
        chunk: &'static str,
    ) -> Result<Option<String>, OpcpuCodecError> {
        match self.fields.pop_front() {
            Some(DecodedField::OptionalString(value)) => Ok(value),
            _ => Err(internal_schema_error(chunk, "optional string")),
        }
    }

    pub(super) fn next_bytes(&mut self, chunk: &'static str) -> Result<Vec<u8>, OpcpuCodecError> {
        match self.fields.pop_front() {
            Some(DecodedField::Bytes(value)) => Ok(value),
            _ => Err(internal_schema_error(chunk, "bytes")),
        }
    }

    pub(super) fn next_optional_string_list(
        &mut self,
        chunk: &'static str,
    ) -> Result<Option<Vec<String>>, OpcpuCodecError> {
        match self.fields.pop_front() {
            Some(DecodedField::OptionalStringList(value)) => Ok(value),
            _ => Err(internal_schema_error(chunk, "optional string list")),
        }
    }

    pub(super) fn next_token_policy_lexical_defaults(
        &mut self,
        chunk: &'static str,
    ) -> Result<TokenPolicyLexicalDefaults, OpcpuCodecError> {
        match self.fields.pop_front() {
            Some(DecodedField::TokenPolicyLexicalDefaults(value)) => Ok(value),
            _ => Err(internal_schema_error(
                chunk,
                "token policy lexical defaults",
            )),
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

impl SimpleSchemaEntry for FamilyDescriptor {
    const CHUNK: &'static str = "FAMS";
    const ENTRY_KIND: &'static str = "family entry";
    const COUNT_LABEL: &'static str = "FAMS count";
    const MIN_RECORD_BYTES: usize = 8;
    const FIELD_SPECS: &'static [FieldSpec] = &[FieldSpec::String, FieldSpec::String];

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![
            FieldValue::String(&self.id),
            FieldValue::String(&self.canonical_dialect),
        ]
    }

    fn from_decoded(mut fields: DecodedFields) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            id: fields.next_string(Self::CHUNK)?,
            canonical_dialect: fields.next_string(Self::CHUNK)?,
        })
    }
}

impl SimpleSchemaRecord for PackageMetaDescriptor {
    const CHUNK: &'static str = "META";
    const FIELD_SPECS: &'static [FieldSpec] =
        &[FieldSpec::String, FieldSpec::String, FieldSpec::U32];

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![
            FieldValue::String(&self.package_id),
            FieldValue::String(&self.package_version),
            FieldValue::U32(self.capability_flags),
        ]
    }

    fn from_decoded(mut fields: DecodedFields) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            package_id: fields.next_string(Self::CHUNK)?,
            package_version: fields.next_string(Self::CHUNK)?,
            capability_flags: fields.next_u32(Self::CHUNK)?,
        })
    }
}

impl SimpleSchemaEntry for String {
    const CHUNK: &'static str = "STRS";
    const ENTRY_KIND: &'static str = "string entry";
    const COUNT_LABEL: &'static str = "STRS count";
    const MIN_RECORD_BYTES: usize = 4;
    const FIELD_SPECS: &'static [FieldSpec] = &[FieldSpec::String];

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![FieldValue::String(self)]
    }

    fn from_decoded(mut fields: DecodedFields) -> Result<Self, OpcpuCodecError> {
        fields.next_string(Self::CHUNK)
    }
}

impl SimpleSchemaEntry for DiagnosticDescriptor {
    const CHUNK: &'static str = "DIAG";
    const ENTRY_KIND: &'static str = "diagnostic entry";
    const COUNT_LABEL: &'static str = "DIAG count";
    const MIN_RECORD_BYTES: usize = 8;
    const FIELD_SPECS: &'static [FieldSpec] = &[FieldSpec::String, FieldSpec::String];

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![
            FieldValue::String(&self.code),
            FieldValue::String(&self.message_template),
        ]
    }

    fn from_decoded(mut fields: DecodedFields) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            code: fields.next_string(Self::CHUNK)?,
            message_template: fields.next_string(Self::CHUNK)?,
        })
    }
}

impl SimpleSchemaEntry for CpuDescriptor {
    const CHUNK: &'static str = "CPUS";
    const ENTRY_KIND: &'static str = "cpu entry";
    const COUNT_LABEL: &'static str = "CPUS count";
    const MIN_RECORD_BYTES: usize = 1;
    const FIELD_SPECS: &'static [FieldSpec] = &[
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::OptionalString {
            label: "default_dialect",
        },
    ];

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![
            FieldValue::String(&self.id),
            FieldValue::String(&self.family_id),
            FieldValue::OptionalString(self.default_dialect.as_deref()),
        ]
    }

    fn from_decoded(mut fields: DecodedFields) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            id: fields.next_string(Self::CHUNK)?,
            family_id: fields.next_string(Self::CHUNK)?,
            default_dialect: fields.next_optional_string(Self::CHUNK)?,
        })
    }
}

impl SimpleSchemaEntry for DialectDescriptor {
    const CHUNK: &'static str = "DIAL";
    const ENTRY_KIND: &'static str = "dialect entry";
    const COUNT_LABEL: &'static str = "DIAL count";
    const MIN_RECORD_BYTES: usize = 1;
    const FIELD_SPECS: &'static [FieldSpec] = &[
        FieldSpec::String,
        FieldSpec::String,
        FieldSpec::OptionalStringList {
            label: "cpu_allow_list",
            count_label: "DIAL allow-list count",
            entry_label: "dialect allow-list entry",
        },
    ];

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![
            FieldValue::String(&self.id),
            FieldValue::String(&self.family_id),
            FieldValue::OptionalStringList(self.cpu_allow_list.as_deref()),
        ]
    }

    fn from_decoded(mut fields: DecodedFields) -> Result<Self, OpcpuCodecError> {
        Ok(Self {
            id: fields.next_string(Self::CHUNK)?,
            family_id: fields.next_string(Self::CHUNK)?,
            cpu_allow_list: fields.next_optional_string_list(Self::CHUNK)?,
        })
    }
}

impl ScopedSchemaEntry for TokenPolicyDescriptor {
    const CHUNK: &'static str = "TOKS";
    const ENTRY_KIND: &'static str = "token policy entry";
    const COUNT_LABEL: &'static str = "TOKS count";
    const FIELD_SPECS: &'static [FieldSpec] = &[
        FieldSpec::U8,
        FieldSpec::U32,
        FieldSpec::U32,
        FieldSpec::String,
        FieldSpec::TokenPolicyLexicalTail,
    ];

    fn owner(&self) -> &ScopedOwner {
        &self.owner
    }

    fn field_values(&self) -> Vec<FieldValue<'_>> {
        vec![
            FieldValue::U8(self.case_rule as u8),
            FieldValue::U32(self.identifier_start_class),
            FieldValue::U32(self.identifier_continue_class),
            FieldValue::String(&self.punctuation_chars),
            FieldValue::TokenPolicyLexicalTail(TokenPolicyLexicalTailRef::from_policy(self)),
        ]
    }

    fn from_decoded(
        owner: ScopedOwner,
        mut fields: DecodedFields,
    ) -> Result<Self, OpcpuCodecError> {
        let case_rule = TokenCaseRule::from_u8(fields.next_u8(Self::CHUNK)?, Self::CHUNK)?;
        let identifier_start_class = fields.next_u32(Self::CHUNK)?;
        let identifier_continue_class = fields.next_u32(Self::CHUNK)?;
        let punctuation_chars = fields.next_string(Self::CHUNK)?;
        let lexical_defaults = fields.next_token_policy_lexical_defaults(Self::CHUNK)?;

        Ok(Self {
            owner,
            case_rule,
            identifier_start_class,
            identifier_continue_class,
            punctuation_chars,
            comment_prefix: lexical_defaults.comment_prefix,
            quote_chars: lexical_defaults.quote_chars,
            escape_char: lexical_defaults.escape_char,
            number_prefix_chars: lexical_defaults.number_prefix_chars,
            number_suffix_binary: lexical_defaults.number_suffix_binary,
            number_suffix_octal: lexical_defaults.number_suffix_octal,
            number_suffix_decimal: lexical_defaults.number_suffix_decimal,
            number_suffix_hex: lexical_defaults.number_suffix_hex,
            operator_chars: lexical_defaults.operator_chars,
            multi_char_operators: lexical_defaults.multi_char_operators,
        })
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

fn encode_schema_fields(
    out: &mut Vec<u8>,
    chunk: &'static str,
    specs: &'static [FieldSpec],
    values: Vec<FieldValue<'_>>,
) -> Result<(), OpcpuCodecError> {
    if specs.len() != values.len() {
        return Err(internal_schema_error(chunk, "field count mismatch"));
    }

    for (spec, value) in specs.iter().zip(values.into_iter()) {
        encode_field(out, chunk, spec, value)?;
    }
    Ok(())
}

fn decode_schema_fields(
    cur: &mut Decoder<'_>,
    chunk: &'static str,
    specs: &'static [FieldSpec],
) -> Result<DecodedFields, OpcpuCodecError> {
    let mut fields = Vec::with_capacity(specs.len());
    for spec in specs {
        fields.push(decode_field(cur, chunk, spec)?);
    }
    Ok(DecodedFields::new(fields))
}

fn encode_field(
    out: &mut Vec<u8>,
    chunk: &'static str,
    spec: &FieldSpec,
    value: FieldValue<'_>,
) -> Result<(), OpcpuCodecError> {
    match (spec, value) {
        (FieldSpec::String, FieldValue::String(value)) => write_string(out, chunk, value),
        (FieldSpec::OptionalString { .. }, FieldValue::OptionalString(value)) => match value {
            Some(value) => {
                out.push(1);
                write_string(out, chunk, value)
            }
            None => {
                out.push(0);
                Ok(())
            }
        },
        (FieldSpec::TokenPolicyLexicalTail, FieldValue::TokenPolicyLexicalTail(value)) => {
            out.push(TOKS_EXT_MARKER);
            write_string(out, chunk, value.comment_prefix)?;
            write_string(out, chunk, value.quote_chars)?;
            match value.escape_char {
                Some(ch) if ch.is_ascii() => {
                    out.push(1);
                    out.push(ch as u8);
                }
                Some(ch) => {
                    return Err(OpcpuCodecError::InvalidChunkFormat {
                        chunk: chunk.to_string(),
                        detail: format!("escape_char must be ASCII: {:?}", ch),
                    });
                }
                None => out.push(0),
            }
            write_string(out, chunk, value.number_prefix_chars)?;
            write_string(out, chunk, value.number_suffix_binary)?;
            write_string(out, chunk, value.number_suffix_octal)?;
            write_string(out, chunk, value.number_suffix_decimal)?;
            write_string(out, chunk, value.number_suffix_hex)?;
            write_string(out, chunk, value.operator_chars)?;
            write_u32(
                out,
                u32_count(
                    value.multi_char_operators.len(),
                    "TOKS multi-char operator count",
                )?,
            );
            for operator in value.multi_char_operators {
                write_string(out, chunk, operator)?;
            }
            Ok(())
        }
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
            FieldSpec::OptionalStringList {
                count_label,
                entry_label: _,
                label: _,
            },
            FieldValue::OptionalStringList(value),
        ) => match value {
            Some(values) => {
                out.push(1);
                write_u32(out, u32_count(values.len(), count_label)?);
                for value in values {
                    write_string(out, chunk, value)?;
                }
                Ok(())
            }
            None => {
                out.push(0);
                Ok(())
            }
        },
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
        FieldSpec::OptionalString { label } => match cur.read_u8()? {
            0 => Ok(DecodedField::OptionalString(None)),
            1 => Ok(DecodedField::OptionalString(Some(cur.read_string()?))),
            other => Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: chunk.to_string(),
                detail: format!("invalid bool flag for {}: {}", label, other),
            }),
        },
        FieldSpec::TokenPolicyLexicalTail => {
            let mut defaults = default_token_policy_lexical_defaults();
            if cur.has_remaining() {
                let marker = cur.peek_u8()?;
                if marker == TOKS_EXT_MARKER {
                    let _ = cur.read_u8()?;
                    defaults.comment_prefix = cur.read_string()?;
                    defaults.quote_chars = cur.read_string()?;
                    defaults.escape_char = match cur.read_u8()? {
                        0 => None,
                        1 => Some(cur.read_u8()? as char),
                        other => {
                            return Err(OpcpuCodecError::InvalidChunkFormat {
                                chunk: chunk.to_string(),
                                detail: format!("invalid bool flag for escape_char: {}", other),
                            });
                        }
                    };
                    defaults.number_prefix_chars = cur.read_string()?;
                    defaults.number_suffix_binary = cur.read_string()?;
                    defaults.number_suffix_octal = cur.read_string()?;
                    defaults.number_suffix_decimal = cur.read_string()?;
                    defaults.number_suffix_hex = cur.read_string()?;
                    defaults.operator_chars = cur.read_string()?;
                    let operator_count = read_bounded_count(cur, 1, "multi-char operator")?;
                    let mut operators = Vec::with_capacity(operator_count);
                    for _ in 0..operator_count {
                        operators.push(cur.read_string()?);
                    }
                    defaults.multi_char_operators = operators;
                }
            }
            Ok(DecodedField::TokenPolicyLexicalDefaults(defaults))
        }
        FieldSpec::Bytes { value_label, .. } => {
            let byte_count = cur.read_u32()? as usize;
            Ok(DecodedField::Bytes(
                cur.read_exact(byte_count, value_label)?.to_vec(),
            ))
        }
        FieldSpec::OptionalStringList {
            label, entry_label, ..
        } => match cur.read_u8()? {
            0 => Ok(DecodedField::OptionalStringList(None)),
            1 => {
                let count = read_bounded_count(cur, 1, entry_label)?;
                let mut values = Vec::with_capacity(count);
                for _ in 0..count {
                    values.push(cur.read_string()?);
                }
                Ok(DecodedField::OptionalStringList(Some(values)))
            }
            other => Err(OpcpuCodecError::InvalidChunkFormat {
                chunk: chunk.to_string(),
                detail: format!("invalid bool flag for {}: {}", label, other),
            }),
        },
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
