// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Shared compound value model.

use std::collections::HashMap;

/// Captured readonly values for each canonical module ID.
pub type ModuleParameters = HashMap<String, HashMap<String, AsmValue>>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ScalarProvenance {
    Absolute,
    Section(String),
    Unsupported,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AsmValue {
    Scalar(i64),
    RelocatableScalar {
        value: i64,
        provenance: ScalarProvenance,
    },
    Values(Vec<AsmValue>),
    Record {
        scalar: Box<AsmValue>,
        fields: Vec<(String, AsmValue)>,
    },
    Range {
        start: i64,
        end: i64,
        step: i64,
    },
    List(Vec<i64>),
    Struct(StructDef),
    StructInstance(StructInstance),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<StructField>,
    pub size: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub offset: u32,
    pub size: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructInstance {
    pub type_name: String,
    pub fields: HashMap<String, i64>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AsmValueError {
    ZeroStep,
    EndOverflow,
    DirectionMismatch { start: i64, end: i64, step: i64 },
}

pub enum AsmValueIter<'a> {
    Values(std::slice::Iter<'a, AsmValue>),
    List(std::iter::Copied<std::slice::Iter<'a, i64>>),
    Range {
        current: i64,
        end: i64,
        step: i64,
        done: bool,
    },
}

impl<'a> Iterator for AsmValueIter<'a> {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            AsmValueIter::List(iter) => iter.next(),
            AsmValueIter::Values(iter) => iter.next().and_then(AsmValue::as_scalar),
            AsmValueIter::Range {
                current,
                end,
                step,
                done,
            } => {
                if *done {
                    return None;
                }
                let in_bounds = if *step > 0 {
                    *current < *end
                } else {
                    *current > *end
                };
                if !in_bounds {
                    *done = true;
                    return None;
                }
                let value = *current;
                *current = current.saturating_add(*step);
                Some(value)
            }
        }
    }
}

impl AsmValue {
    pub fn scalar(value: i64) -> Self {
        Self::Scalar(value)
    }

    pub fn try_range(
        start: i64,
        end: i64,
        inclusive: bool,
        step: Option<i64>,
    ) -> Result<Self, AsmValueError> {
        let step = step.unwrap_or(if start <= end { 1 } else { -1 });
        if step == 0 {
            return Err(AsmValueError::ZeroStep);
        }
        let normalized_end = if inclusive {
            end.checked_add(step.signum())
                .ok_or(AsmValueError::EndOverflow)?
        } else {
            end
        };
        if (step > 0 && start > normalized_end) || (step < 0 && start < normalized_end) {
            return Err(AsmValueError::DirectionMismatch { start, end, step });
        }
        Ok(Self::Range {
            start,
            end: normalized_end,
            step,
        })
    }

    pub fn as_scalar(&self) -> Option<i64> {
        match self {
            AsmValue::Scalar(value) | AsmValue::RelocatableScalar { value, .. } => Some(*value),
            AsmValue::Record { scalar, .. } => scalar.as_scalar(),
            _ => None,
        }
    }

    pub fn len(&self) -> Option<usize> {
        match self {
            AsmValue::Scalar(_)
            | AsmValue::RelocatableScalar { .. }
            | AsmValue::Record { .. }
            | AsmValue::Struct(_)
            | AsmValue::StructInstance(_) => None,
            AsmValue::Values(items) => Some(items.len()),
            AsmValue::List(items) => Some(items.len()),
            AsmValue::Range { start, end, step } => Some(range_len(*start, *end, *step)),
        }
    }

    pub fn is_empty(&self) -> Option<bool> {
        self.len().map(|len| len == 0)
    }

    pub fn iter(&self) -> Option<AsmValueIter<'_>> {
        match self {
            AsmValue::Scalar(_)
            | AsmValue::RelocatableScalar { .. }
            | AsmValue::Record { .. }
            | AsmValue::Struct(_)
            | AsmValue::StructInstance(_) => None,
            AsmValue::Values(items) if items.iter().all(|item| item.as_scalar().is_some()) => {
                Some(AsmValueIter::Values(items.iter()))
            }
            AsmValue::Values(_) => None,
            AsmValue::List(items) => Some(AsmValueIter::List(items.iter().copied())),
            AsmValue::Range { start, end, step } => Some(AsmValueIter::Range {
                current: *start,
                end: *end,
                step: *step,
                done: false,
            }),
        }
    }

    pub fn get(&self, index: usize) -> Option<i64> {
        match self {
            AsmValue::Scalar(_)
            | AsmValue::RelocatableScalar { .. }
            | AsmValue::Record { .. }
            | AsmValue::Struct(_)
            | AsmValue::StructInstance(_) => None,
            AsmValue::Values(items) => items.get(index).and_then(AsmValue::as_scalar),
            AsmValue::List(items) => items.get(index).copied(),
            AsmValue::Range { start, end, step } => {
                let len = range_len(*start, *end, *step);
                if index >= len {
                    return None;
                }
                let index_i64 = i64::try_from(index).ok()?;
                start.checked_add(step.checked_mul(index_i64)?)
            }
        }
    }

    /// Returns the complete indexed value, retaining record members and provenance.
    pub fn get_value(&self, index: usize) -> Option<AsmValue> {
        match self {
            Self::Values(items) => items.get(index).cloned(),
            _ => self.get(index).map(Self::Scalar),
        }
    }

    pub fn scalar_provenance(&self) -> Option<ScalarProvenance> {
        match self {
            Self::Scalar(_) => Some(ScalarProvenance::Absolute),
            Self::RelocatableScalar { provenance, .. } => Some(provenance.clone()),
            Self::Record { scalar, .. } => scalar.scalar_provenance(),
            _ => None,
        }
    }

    pub fn field_value_typed(&self, name: &str) -> Option<&AsmValue> {
        match self {
            Self::Record { fields, .. } => fields
                .iter()
                .find(|(field, _)| field.eq_ignore_ascii_case(name))
                .map(|(_, value)| value),
            _ => None,
        }
    }

    pub fn to_list(&self) -> Option<Vec<i64>> {
        match self {
            Self::Values(items) => items.iter().map(Self::as_scalar).collect(),
            _ => self.iter().map(|iter| iter.collect()),
        }
    }

    pub fn field_offset(&self, name: &str) -> Option<u32> {
        match self {
            AsmValue::Struct(def) => def
                .fields
                .iter()
                .find(|field| field.name == name)
                .map(|field| field.offset),
            _ => None,
        }
    }

    pub fn field_value(&self, name: &str) -> Option<i64> {
        match self {
            Self::Record { .. } => self.field_value_typed(name).and_then(Self::as_scalar),
            AsmValue::StructInstance(instance) => {
                instance.fields.get(name).copied().or_else(|| {
                    instance
                        .fields
                        .iter()
                        .find(|(field_name, _)| field_name.eq_ignore_ascii_case(name))
                        .map(|(_, value)| *value)
                })
            }
            _ => None,
        }
    }
}

fn range_len(start: i64, end: i64, step: i64) -> usize {
    if step == 0 {
        return 0;
    }
    if (step > 0 && start >= end) || (step < 0 && start <= end) {
        return 0;
    }
    let step_abs = i128::from(step).abs();
    if step_abs == 0 {
        return 0;
    }
    let distance = if step > 0 {
        i128::from(end) - i128::from(start)
    } else {
        i128::from(start) - i128::from(end)
    };
    let count = ((distance - 1) / step_abs) + 1;
    usize::try_from(count).unwrap_or(usize::MAX)
}

#[cfg(test)]
mod tests {
    use super::{AsmValue, AsmValueError, StructDef, StructField, StructInstance};
    use std::collections::HashMap;

    #[test]
    fn range_construction_normalizes_inclusive_end() {
        let value = AsmValue::try_range(0, 3, true, None).expect("range should build");
        assert_eq!(
            value,
            AsmValue::Range {
                start: 0,
                end: 4,
                step: 1
            }
        );
        assert_eq!(value.to_list(), Some(vec![0, 1, 2, 3]));
    }

    #[test]
    fn descending_range_uses_negative_default_step() {
        let value = AsmValue::try_range(3, 0, true, None).expect("descending range should build");
        assert_eq!(
            value,
            AsmValue::Range {
                start: 3,
                end: -1,
                step: -1
            }
        );
        assert_eq!(value.to_list(), Some(vec![3, 2, 1, 0]));
    }

    #[test]
    fn invalid_range_direction_is_rejected() {
        assert_eq!(
            AsmValue::try_range(0, 3, false, Some(-1)),
            Err(AsmValueError::DirectionMismatch {
                start: 0,
                end: 3,
                step: -1,
            })
        );
    }

    #[test]
    fn range_get_observes_bounds() {
        let value = AsmValue::try_range(10, 14, false, None).expect("range should build");
        assert_eq!(value.get(0), Some(10));
        assert_eq!(value.get(3), Some(13));
        assert_eq!(value.get(4), None);
    }

    #[test]
    fn minimum_negative_step_len_and_get_do_not_overflow() {
        let value = AsmValue::try_range(i64::MAX, i64::MIN, false, Some(i64::MIN))
            .expect("range with minimum negative step should build");

        assert_eq!(value.len(), Some(2));
        assert_eq!(value.get(0), Some(i64::MAX));
        assert_eq!(value.get(1), Some(-1));
        assert_eq!(value.get(2), None);
    }

    #[test]
    fn inclusive_range_endpoint_overflow_is_reported() {
        assert_eq!(
            AsmValue::try_range(0, i64::MAX, true, None),
            Err(AsmValueError::EndOverflow)
        );
        assert_eq!(
            AsmValue::try_range(0, i64::MIN, true, None),
            Err(AsmValueError::EndOverflow)
        );
    }

    #[test]
    fn struct_offsets_are_exposed() {
        let value = AsmValue::Struct(StructDef {
            name: "Vec2".to_string(),
            fields: vec![
                StructField {
                    name: "x".to_string(),
                    offset: 0,
                    size: 1,
                },
                StructField {
                    name: "y".to_string(),
                    offset: 1,
                    size: 1,
                },
            ],
            size: 2,
        });

        assert_eq!(value.field_offset("x"), Some(0));
        assert_eq!(value.field_offset("y"), Some(1));
        assert_eq!(value.field_offset("z"), None);
    }

    #[test]
    fn struct_instance_values_are_case_tolerant() {
        let value = AsmValue::StructInstance(StructInstance {
            type_name: "Flags".to_string(),
            fields: HashMap::from([("READY".to_string(), 1), ("busy".to_string(), 0)]),
        });

        assert_eq!(value.field_value("READY"), Some(1));
        assert_eq!(value.field_value("ready"), Some(1));
        assert_eq!(value.field_value("busy"), Some(0));
        assert_eq!(value.field_value("BUSY"), Some(0));
        assert_eq!(value.field_value("missing"), None);
    }
    #[test]
    fn typed_records_preserve_fields_and_address_provenance() {
        use super::ScalarProvenance;
        let record = AsmValue::Record {
            scalar: Box::new(AsmValue::RelocatableScalar {
                value: 42,
                provenance: ScalarProvenance::Section("code".into()),
            }),
            fields: vec![("next".into(), AsmValue::Scalar(7))],
        };
        let values = AsmValue::Values(vec![record.clone()]);
        assert_eq!(values.get_value(0), Some(record.clone()));
        assert_eq!(values.get_value(1), None);
        assert_eq!(values.get(0), Some(42));
        assert_eq!(values.to_list(), Some(vec![42]));
        assert_eq!(
            record.scalar_provenance(),
            Some(ScalarProvenance::Section("code".into()))
        );
        assert_eq!(record.field_value("NEXT"), Some(7));
        assert_eq!(record.field_value_typed("missing"), None);
        assert_eq!(
            AsmValue::Values(vec![AsmValue::List(vec![1])]).to_list(),
            None
        );
    }
}
