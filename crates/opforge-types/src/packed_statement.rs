// SPDX-License-Identifier: GPL-3.0-or-later
//! Current shared PRVM statement signature wire layout. All integers are big endian.
//! Offsets address the complete immutable plan, including its header and rows.
pub const COMPILE_ENTRY: u16 = 11;
pub const MATCH_ENTRY: u16 = 12;
pub const HEAD_CLASSIFY: u32 = 0;
pub const HEAD_COMPLETE: u32 = 1;
pub const HEADER_BYTES: usize = 16;
pub const ROW_BYTES: usize = 24;
pub const EXTENT: usize = 0;
pub const ROW_COUNT: usize = 4;
pub const LITERAL_SCORE: usize = 8;
pub const ATOM_SCORE: usize = 12;
pub const KIND: usize = 0;
pub const CAPTURE_TYPE: usize = 1;
pub const PAYLOAD_OFFSET: usize = 4;
pub const PAYLOAD_BYTES: usize = 8;
pub const PAIRED_ROW: usize = 12;
pub const COL_START: usize = 16;
pub const COL_END: usize = 20;
pub const LITERAL: u8 = 1;
pub const CAPTURE: u8 = 2;
pub const BOUNDARY_OPEN: u8 = 3;
pub const BOUNDARY_CLOSE: u8 = 4;
pub const NONE: u32 = u32::MAX;
pub const BYTE: u8 = 1;
pub const WORD: u8 = 2;
pub const LONG: u8 = 3;
pub const CHAR: u8 = 4;
pub const STR: u8 = 5;
pub const TOKEN_BYTES: usize = 32;
pub const TOKEN_KIND: usize = 0;
pub const TOKEN_NUMERIC_STATUS: usize = 1;
pub const TOKEN_LITERAL_OFFSET: usize = 4;
pub const TOKEN_LITERAL_BYTES: usize = 8;
pub const TOKEN_DECODED_BYTES: usize = 12;
pub const TOKEN_COL_START: usize = 16;
pub const TOKEN_COL_END: usize = 20;
pub const TOKEN_NUMERIC_VALUE: usize = 24;
pub const CAPTURE_BYTES: usize = 12;
pub const CAPTURE_NAME_OFFSET: usize = 0;
pub const CAPTURE_NAME_BYTES: usize = 4;
pub const CAPTURE_TOKEN_INDEX: usize = 8;

/// A capture identifies immutable name bytes in the plan and one input token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Capture {
    pub name_offset: u32,
    pub name_bytes: u32,
    pub token_index: u32,
}
impl Capture {
    pub fn encode(self) -> [u8; CAPTURE_BYTES] {
        let mut bytes = [0; CAPTURE_BYTES];
        for (offset, value) in [
            (CAPTURE_NAME_OFFSET, self.name_offset),
            (CAPTURE_NAME_BYTES, self.name_bytes),
            (CAPTURE_TOKEN_INDEX, self.token_index),
        ] {
            bytes[offset..offset + 4].copy_from_slice(&value.to_be_bytes());
        }
        bytes
    }
}

/// Prepared lexical facet. Literal offsets address the invocation's byte arena;
/// numeric status is 0 for absent/invalid and 1 for a valid signed value.
/// Numeric interpretation happens once during preparation, never per candidate.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TokenView {
    pub kind: u8,
    pub numeric_status: u8,
    pub literal_offset: u32,
    pub literal_bytes: u32,
    pub decoded_bytes: u32,
    pub col_start: u32,
    pub col_end: u32,
    pub numeric_value: i64,
}
impl TokenView {
    pub fn encode(self) -> [u8; TOKEN_BYTES] {
        let mut bytes = [0; TOKEN_BYTES];
        bytes[TOKEN_KIND] = self.kind;
        bytes[TOKEN_NUMERIC_STATUS] = self.numeric_status;
        for (offset, value) in [
            (TOKEN_LITERAL_OFFSET, self.literal_offset),
            (TOKEN_LITERAL_BYTES, self.literal_bytes),
            (TOKEN_DECODED_BYTES, self.decoded_bytes),
            (TOKEN_COL_START, self.col_start),
            (TOKEN_COL_END, self.col_end),
        ] {
            bytes[offset..offset + 4].copy_from_slice(&value.to_be_bytes());
        }
        bytes[TOKEN_NUMERIC_VALUE..TOKEN_NUMERIC_VALUE + 8]
            .copy_from_slice(&self.numeric_value.to_be_bytes());
        bytes
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Head {
    /// 1 definition, 2 end marker. Selection does not validate the signature.
    pub action: u8,
    pub keyword_token: u32,
    pub signature_token: u32,
}
