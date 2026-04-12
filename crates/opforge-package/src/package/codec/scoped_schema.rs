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
        entries.push(T::from_decoded(owner, DecodedFields::new(fields))?);
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
}

#[derive(Clone, Copy)]
pub(super) enum FieldSpec {
    String,
    Bytes {
        len_label: &'static str,
        value_label: &'static str,
    },
    U16 {
        label: &'static str,
    },
    Bool {
        label: &'static str,
    },
    U8,
}

pub(super) enum FieldValue<'a> {
    String(&'a str),
    Bytes(&'a [u8]),
    U16(u16),
    Bool(bool),
    U8(u8),
}

enum DecodedField {
    String(String),
    Bytes(Vec<u8>),
    U16(u16),
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

    pub(super) fn next_u16(&mut self, chunk: &'static str) -> Result<u16, OpcpuCodecError> {
        match self.fields.pop_front() {
            Some(DecodedField::U16(value)) => Ok(value),
            _ => Err(internal_schema_error(chunk, "u16")),
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
        FieldSpec::U16 { label: "priority" },
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
        (FieldSpec::U16 { .. }, FieldValue::U16(value)) => {
            write_u16(out, value);
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
        FieldSpec::U16 { label } => {
            let bytes = cur.read_exact(2, label)?;
            Ok(DecodedField::U16(u16::from_le_bytes([bytes[0], bytes[1]])))
        }
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
