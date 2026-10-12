// SPDX-License-Identifier: GPL-3.0-or-later
//! Neutral current shared PRVM invocation records. Offsets address owning arenas.
use crate::packed_statement::Capture;
#[derive(Debug, Clone, Copy)]
pub struct ScopeRow {
    pub keyword_offset: u32,
    pub keyword_bytes: u32,
    pub plan_id: u32,
    pub template_id: u32,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Route {
    pub label_token: u32,
    pub head_token: u32,
    pub prefix_bytes: u32,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Selection {
    pub scope_index: u32,
    pub plan_id: u32,
    pub template_id: u32,
    pub captures: Vec<Capture>,
    /// Captures address these derived views when true, ordinary views otherwise.
    pub used_fallback: bool,
}

pub const SCOPE_BYTES: usize = 16;
pub const ROUTE_BYTES: usize = 12;
pub const SELECTION_BYTES: usize = 20;
impl ScopeRow {
    pub fn encode(self) -> [u8; SCOPE_BYTES] {
        let mut out = [0; SCOPE_BYTES];
        for (index, value) in [
            self.keyword_offset,
            self.keyword_bytes,
            self.plan_id,
            self.template_id,
        ]
        .into_iter()
        .enumerate()
        {
            out[index * 4..index * 4 + 4].copy_from_slice(&value.to_be_bytes());
        }
        out
    }
}
impl Route {
    pub fn encode(self) -> [u8; ROUTE_BYTES] {
        let mut out = [0; ROUTE_BYTES];
        for (index, value) in [self.label_token, self.head_token, self.prefix_bytes]
            .into_iter()
            .enumerate()
        {
            out[index * 4..index * 4 + 4].copy_from_slice(&value.to_be_bytes());
        }
        out
    }
}
impl Selection {
    pub fn encode_header(&self) -> [u8; SELECTION_BYTES] {
        let mut out = [0; SELECTION_BYTES];
        for (index, value) in [
            self.scope_index,
            self.plan_id,
            self.template_id,
            u32::from(self.used_fallback),
            self.captures.len() as u32,
        ]
        .into_iter()
        .enumerate()
        {
            out[index * 4..index * 4 + 4].copy_from_slice(&value.to_be_bytes());
        }
        out
    }
}
