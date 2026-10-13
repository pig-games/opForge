// SPDX-License-Identifier: GPL-3.0-or-later
//! Current private compact-source envelope shared by packed PRVM grammars.
//! Widths describe bytes; multibyte numeric fields use big-endian order.
pub const RECORD_LENGTH: usize = 0;
pub const RECORD_FLAGS: usize = 1;
pub const RECORD_BYTES: usize = 4;
pub const MAX_RECORD_BYTES: usize = 256;
pub const FLAG_INDENT: u8 = 1;
pub const FLAG_BLOCK_OPEN: u8 = 2;
pub const FLAG_BLOCK_CLOSE: u8 = 4;
pub const FLAG_OMIT: u8 = 8;
pub const FLAG_LAYOUT: u8 = 16;
pub const FLAG_PLAN: u8 = 32;
pub const FLAG_MUTABLE_SNAPSHOT: u8 = 64;
pub const FLAG_ALLOWED: u8 = FLAG_INDENT
    | FLAG_BLOCK_OPEN
    | FLAG_BLOCK_CLOSE
    | FLAG_OMIT
    | FLAG_LAYOUT
    | FLAG_PLAN
    | FLAG_MUTABLE_SNAPSHOT;
pub const FLAG_INACTIVE: u8 = FLAG_OMIT | FLAG_LAYOUT;
pub const NAME_KIND: usize = 0;
pub const NAME_ID: usize = 1;
pub const NAME_QUALIFIER: usize = 3;
pub const NAME_BYTES: usize = 4;
pub const DOT_KIND: usize = 0;
pub const DOT_NAME: usize = 1;
pub const DOT_BYTES: usize = DOT_NAME + NAME_BYTES;
pub const LABEL_COLON: usize = NAME_BYTES;
pub const LABEL_BYTES: usize = NAME_BYTES + 1;
pub const STRING_LENGTH: usize = 1;
pub const STRING_HEADER_BYTES: usize = 2;
pub const TOKEN_IDENTIFIER: u8 = 0;
pub const TOKEN_REGISTER: u8 = 1;
pub const TOKEN_NUMBER: u8 = 2;
pub const TOKEN_STRING: u8 = 3;
pub const TOKEN_COMMA: u8 = 4;
pub const TOKEN_COLON: u8 = 5;
pub const TOKEN_EQUAL: u8 = 6;
pub const TOKEN_DOT: u8 = 7;
pub const TOKEN_OPERATOR: u8 = 9;
pub const TOKEN_OPERATOR_FIRST: u8 = 16;
pub const TOKEN_OPERATOR_LAST: u8 = 39;
pub const TOKEN_OPEN_BRACKET: u8 = 10;
pub const TOKEN_CLOSE_BRACKET: u8 = 11;
pub const TOKEN_OPEN_BRACE: u8 = 12;
pub const TOKEN_CLOSE_BRACE: u8 = 13;
pub const TOKEN_OPEN_PAREN: u8 = 14;
pub const TOKEN_CLOSE_PAREN: u8 = 15;
pub const TOKEN_WIDE_NUMBER: u8 = 64;
pub const TOKEN_COMPILED_EXPRESSION: u8 = 129;
pub const TOKEN_TYPED_EXPRESSION: u8 = 131;
/// Prepared binary asset payload: u8 byte length; contents are not decoded text.
pub const TOKEN_BINARY_BYTES: u8 = 0x87;
pub const BINARY_BYTES_KIND: usize = 0;
pub const BINARY_BYTES_LENGTH: usize = 1;
pub const BINARY_BYTES_HEADER_BYTES: usize = 2;
pub const NUMBER_BYTES: usize = 5;
pub const WIDE_NUMBER_BYTES: usize = 9;
pub fn canonical_name(token: &[u8]) -> bool {
    token.len() == NAME_BYTES
        && token[NAME_KIND] <= TOKEN_REGISTER
        && token[NAME_QUALIFIER] == 0
        && token[NAME_ID..NAME_QUALIFIER] != [0, 0]
}
