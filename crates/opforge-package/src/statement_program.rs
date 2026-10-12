// SPDX-License-Identifier: GPL-3.0-or-later
//! Shared PRVM statement grammar and typed-token matching policy.
use types::packed_statement::{BYTE, CHAR, LONG, STR, WORD};

pub const COMPILE: u8 = 0xa6;
pub const MATCH: u8 = 0xa7;
pub const HEAD: u8 = 0xa8;
pub const PUBLISH: u8 = 0x83;
pub const END: u8 = 0;
pub const TYPE_ROW_BYTES: usize = 24;
pub const TYPE_COUNT: usize = 5;
pub const HEAD_SPELLINGS: [&str; 2] = ["statement", "endstatement"];
/// Canonical literal rendering of shared tokenizer kinds. Source spelling stays
/// separate: notably a single source '=' token renders as the literal '=='.
pub const TOKEN_RENDER_SPELLINGS: [(u8, &str); 37] = [
    (4, ","),
    (5, ":"),
    (6, "$"),
    (7, "."),
    (8, "#"),
    (9, "?"),
    (10, "["),
    (11, "]"),
    (12, "{"),
    (13, "}"),
    (14, "("),
    (15, ")"),
    (16, ".."),
    (17, "..="),
    (18, "+"),
    (19, "-"),
    (20, "*"),
    (21, "**"),
    (22, "/"),
    (23, "%"),
    (24, "<<"),
    (25, ">>"),
    (26, "~"),
    (27, "!"),
    (28, "&"),
    (29, "|"),
    (30, "^"),
    (31, "&&"),
    (32, "||"),
    (33, "^^"),
    (34, "=="),
    (35, "!="),
    (36, ">="),
    (37, ">"),
    (38, "<="),
    (39, "<"),
    (40, "@"),
];

/// Policy 1 accepts a normalized label on the end marker. Complete-envelope
/// mode rejects definition labels and requires an identifier/register keyword;
/// classification mode only identifies the declaration role.
pub fn head_program(heads: [u16; 2]) -> Vec<u8> {
    let mut out = vec![HEAD, 1];
    for id in heads {
        out.extend(id.to_be_bytes());
    }
    out.extend([PUBLISH, END]);
    out
}

/// Compilation policy: quoted literal, dot literal, boundary open/close pairs,
/// capture delimiter, then capture-name/type vocabulary. Token kind numbers are
/// the shared packed-source contract; capture type names are package data.
pub fn compile_program() -> Vec<u8> {
    let mut out = vec![COMPILE, 3, 7, 10, 12, 13, 11, 5, TYPE_COUNT as u8];
    for (id, name) in [
        (BYTE, "byte"),
        (WORD, "word"),
        (LONG, "long"),
        (CHAR, "char"),
        (STR, "str"),
    ] {
        out.extend([id, name.len() as u8]);
        out.extend(name.as_bytes());
    }
    out.push(TOKEN_RENDER_SPELLINGS.len() as u8);
    for (kind, text) in TOKEN_RENDER_SPELLINGS {
        out.extend([kind, text.len() as u8]);
        out.extend(text.as_bytes());
    }
    out.extend([PUBLISH, END]);
    out
}

/// Matcher rows: type:u8, identifier policy:u8 (0 reject, 1 any, 2 one byte),
/// string minimum:u8, string maximum:u8 (0 rejects strings), numeric minimum
/// and maximum:i64. String maximum 255 means unbounded. Remaining four bytes
/// are reserved and must be zero.
/// Policy 1 preserves deferred symbolic captures and literal adjacency.
pub fn match_program() -> Vec<u8> {
    let mut out = vec![MATCH, 1, TYPE_COUNT as u8];
    for (id, ident, smin, smax, min, max) in [
        (BYTE, 1, 1, 1, -128i64, 255i64),
        (WORD, 1, 1, 2, -32768, 65535),
        (LONG, 1, 0, 0, -2147483648, 4294967295),
        (CHAR, 2, 1, 1, 1, 0),
        (STR, 0, 0, 255, 1, 0),
    ] {
        out.extend([id, ident, smin, smax]);
        out.extend(min.to_be_bytes());
        out.extend(max.to_be_bytes());
        out.extend([0; 4]);
    }
    out.extend([PUBLISH, END]);
    out
}
