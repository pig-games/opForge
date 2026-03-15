// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Shared compound value model.

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AsmValue {
    Scalar(i64),
    Range { start: i64, end: i64, step: i64 },
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
            AsmValue::Scalar(value) => Some(*value),
            _ => None,
        }
    }

    pub fn len(&self) -> Option<usize> {
        match self {
            AsmValue::Scalar(_) | AsmValue::Struct(_) | AsmValue::StructInstance(_) => None,
            AsmValue::List(items) => Some(items.len()),
            AsmValue::Range { start, end, step } => Some(range_len(*start, *end, *step)),
        }
    }

    pub fn is_empty(&self) -> Option<bool> {
        self.len().map(|len| len == 0)
    }

    pub fn iter(&self) -> Option<AsmValueIter<'_>> {
        match self {
            AsmValue::Scalar(_) | AsmValue::Struct(_) | AsmValue::StructInstance(_) => None,
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
            AsmValue::Scalar(_) | AsmValue::Struct(_) | AsmValue::StructInstance(_) => None,
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

    pub fn to_list(&self) -> Option<Vec<i64>> {
        self.iter().map(|iter| iter.collect())
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
    let step_abs = i128::from(step.abs());
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
}
