// SPDX-License-Identifier: GPL-3.0-or-later
//! Experimental bounded token/spelling input. Raw spelling is owned by atoms,
//! never reconstructed into a physical source line by consumers.
//!
//! Big-endian header (16 bytes): total u32, atom count u16, token count u16,
//! logical spelling bytes u16, zero u16, payload offset u32. Each 12-byte atom
//! stores kind u16, token index u16, logical start u16, byte count u16 and
//! absolute payload offset u32. Gap kind/index are 0xffff; token kinds are the
//! TKVM codes 0..=40. Gaps contain only space/tab. Rows cover logical spelling
//! exactly and their payloads are contiguous in row order. There is no version
//! dispatch: this is a single experimental format, not a persisted public ABI.
use crate::macro_descriptor_vm::DescriptorError;

pub const HEADER_BYTES: usize = 16;
pub const ATOM_BYTES: usize = 12;
pub const MAX_SPELLING: usize = 1024;
pub const MAX_TOKENS: usize = 64;
pub const MAX_ATOMS: usize = MAX_TOKENS * 2 + 1;
pub const GAP: u16 = u16::MAX;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: u16,
    pub start: u16,
    pub end: u16,
}
#[derive(Clone, Copy, Debug)]
pub struct Atom<'a> {
    pub kind: u16,
    pub token_index: u16,
    pub start: u16,
    pub bytes: &'a [u8],
}
impl Atom<'_> {
    pub fn end(&self) -> usize {
        self.start as usize + self.bytes.len()
    }
}
#[derive(Debug)]
pub struct View<'a> {
    atoms: Vec<Atom<'a>>,
    tokens: Vec<Token>,
    len: usize,
}
fn error(offset: usize) -> DescriptorError {
    DescriptorError {
        status: 5,
        offset: offset as u32,
        message: "Invalid binary spelling view",
    }
}
fn word(b: &[u8], p: usize) -> usize {
    u16::from_be_bytes([b[p], b[p + 1]]) as usize
}
fn long(b: &[u8], p: usize) -> usize {
    u32::from_be_bytes(b[p..p + 4].try_into().unwrap()) as usize
}
impl<'a> View<'a> {
    /// Validate the complete canonical block before exposing any atoms. Payload
    /// extents are contiguous and unaliased; token indices are dense and ordered.
    pub fn parse(block: &'a [u8]) -> Result<Self, DescriptorError> {
        if block.len() < HEADER_BYTES || long(block, 0) != block.len() {
            return Err(error(0));
        }
        let count = word(block, 4);
        let token_count = word(block, 6);
        let len = word(block, 8);
        let payload = HEADER_BYTES + count * ATOM_BYTES;
        if count > MAX_ATOMS
            || token_count > MAX_TOKENS
            || len > MAX_SPELLING
            || word(block, 10) != 0
            || long(block, 12) != payload
            || payload.checked_add(len) != Some(block.len())
        {
            return Err(error(0));
        }
        let mut atoms = Vec::with_capacity(count);
        let mut tokens = Vec::with_capacity(token_count);
        let mut pos = 0;
        for i in 0..count {
            let p = HEADER_BYTES + i * ATOM_BYTES;
            let kind = word(block, p) as u16;
            let index = word(block, p + 2) as u16;
            let start = word(block, p + 4);
            let n = word(block, p + 6);
            if start != pos || n == 0 || long(block, p + 8) != payload + pos || pos + n > len {
                return Err(error(p));
            }
            let bytes = &block[payload + pos..payload + pos + n];
            // Each atom ends on a UTF-8 boundary, including trivia atoms.
            if std::str::from_utf8(bytes).is_err() {
                return Err(error(p));
            }
            if kind == GAP {
                if index != GAP
                    || atoms.last().is_some_and(|a: &Atom<'_>| a.kind == GAP)
                    || !bytes.iter().all(|b| matches!(b, b' ' | b'\t'))
                {
                    return Err(error(p));
                }
            } else {
                if kind > 40 || index as usize != tokens.len() || tokens.len() >= token_count {
                    return Err(error(p));
                }
                tokens.push(Token {
                    kind,
                    start: start as u16,
                    end: (start + n) as u16,
                });
            }
            atoms.push(Atom {
                kind,
                token_index: index,
                start: start as u16,
                bytes,
            });
            pos += n;
        }
        if pos != len || tokens.len() != token_count {
            return Err(error(0));
        }
        Ok(Self { atoms, tokens, len })
    }
    pub fn len(&self) -> usize {
        self.len
    }
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
    pub fn atoms(&self) -> &[Atom<'a>] {
        &self.atoms
    }
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }
    pub fn byte(&self, pos: usize) -> Option<u8> {
        let i = self.atoms.partition_point(|a| a.end() <= pos);
        let a = self.atoms.get(i)?;
        a.bytes.get(pos.checked_sub(a.start as usize)?).copied()
    }
    pub fn token_at(&self, pos: usize) -> Option<&Token> {
        let i = self.tokens.partition_point(|t| (t.end as usize) <= pos);
        self.tokens.get(i).filter(|t| t.start as usize == pos)
    }
    /// UTF-8 whitespace length at a logical coordinate, without joining atoms.
    pub fn whitespace(&self, pos: usize) -> usize {
        let i = self.atoms.partition_point(|a| a.end() <= pos);
        let Some(a) = self.atoms.get(i) else {
            return 0;
        };
        let Some(bytes) = a.bytes.get(pos.saturating_sub(a.start as usize)..) else {
            return 0;
        };
        std::str::from_utf8(bytes)
            .ok()
            .and_then(|s| s.chars().next())
            .filter(|c| c.is_whitespace())
            .map_or(0, char::len_utf8)
    }
    pub fn trim(&self, mut a: usize, b: usize) -> (usize, usize) {
        while a < b {
            let n = self.whitespace(a);
            if n == 0 {
                break;
            }
            a += n;
        }
        let mut last = a;
        let mut p = a;
        while p < b {
            let n = self.whitespace(p);
            if n == 0 {
                p += 1;
                last = p;
            } else {
                p += n;
            }
        }
        (a, last)
    }
}

/// Capture once from the tokenizer's original byte spans. The caller chooses
/// the comment-free extent; this producer performs no grammar or tokenization.
pub fn capture(source: &str, tokens: &[Token]) -> Result<Vec<u8>, DescriptorError> {
    if source.len() > MAX_SPELLING || tokens.len() > MAX_TOKENS {
        return Err(error(0));
    }
    let mut rows = Vec::new();
    let mut pos = 0;
    for (index, token) in tokens.iter().enumerate() {
        let a = token.start as usize;
        let b = token.end as usize;
        if token.kind > 40
            || a < pos
            || a >= b
            || b > source.len()
            || !source.is_char_boundary(a)
            || !source.is_char_boundary(b)
        {
            return Err(error(a));
        }
        if a > pos {
            rows.push((GAP, GAP, pos, a));
        }
        rows.push((token.kind, index as u16, a, b));
        pos = b;
    }
    if pos < source.len() {
        rows.push((GAP, GAP, pos, source.len()));
    }
    let payload = HEADER_BYTES + rows.len() * ATOM_BYTES;
    let mut out = vec![0; payload + source.len()];
    let total = out.len() as u32;
    out[..4].copy_from_slice(&total.to_be_bytes());
    out[4..6].copy_from_slice(&(rows.len() as u16).to_be_bytes());
    out[6..8].copy_from_slice(&(tokens.len() as u16).to_be_bytes());
    out[8..10].copy_from_slice(&(source.len() as u16).to_be_bytes());
    out[12..16].copy_from_slice(&(payload as u32).to_be_bytes());
    for (i, (kind, index, a, b)) in rows.into_iter().enumerate() {
        let p = HEADER_BYTES + i * ATOM_BYTES;
        out[p..p + 2].copy_from_slice(&kind.to_be_bytes());
        out[p + 2..p + 4].copy_from_slice(&index.to_be_bytes());
        out[p + 4..p + 6].copy_from_slice(&(a as u16).to_be_bytes());
        out[p + 6..p + 8].copy_from_slice(&((b - a) as u16).to_be_bytes());
        out[p + 8..p + 12].copy_from_slice(&((payload + a) as u32).to_be_bytes());
        out[payload + a..payload + b].copy_from_slice(&source.as_bytes()[a..b]);
    }
    View::parse(&out)?;
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn binary_view_retains_gaps_raw_quotes_and_numeric_spelling() {
        let source = "  $0f\t\"\\x401\" ";
        let block = capture(
            source,
            &[
                Token {
                    kind: 2,
                    start: 2,
                    end: 5,
                },
                Token {
                    kind: 3,
                    start: 6,
                    end: 13,
                },
            ],
        )
        .unwrap();
        let v = View::parse(&block).unwrap();
        assert_eq!(v.tokens().len(), 2);
        assert_eq!(v.atoms()[1].bytes, b"$0f");
        assert_eq!(v.atoms()[2].bytes, b"\t");
        assert_eq!(v.atoms()[3].bytes, b"\"\\x401\"");
        for (i, b) in source.bytes().enumerate() {
            assert_eq!(v.byte(i), Some(b));
        }
        assert_eq!(v.byte(source.len()), None);
        assert_eq!(v.trim(0, v.len()), (2, 13));
        assert!(View::parse(&capture("", &[]).unwrap()).unwrap().is_empty());
    }
    #[test]
    fn binary_view_rejects_corrupted_integrity_and_ownership() {
        let block = capture(
            " a ",
            &[Token {
                kind: 0,
                start: 1,
                end: 2,
            }],
        )
        .unwrap();
        for (offset, value) in [
            (0, 1),
            (4, 255),
            (6, 255),
            (8, 255),
            (10, 1),
            (12, 1),
            (16, 0),
            (18, 0),
            (20, 1),
            (22, 0),
            (24, 0),
            (30, 1),
        ] {
            let mut bad = block.clone();
            bad[offset] = value;
            if bad != block {
                assert!(View::parse(&bad).is_err(), "offset {offset}");
            }
        }
        let mut bad = block;
        bad.push(0);
        assert!(View::parse(&bad).is_err());
        assert!(capture(
            "a b",
            &[Token {
                kind: 0,
                start: 0,
                end: 1
            }]
        )
        .is_err());
        assert!(capture(
            "é",
            &[Token {
                kind: 0,
                start: 0,
                end: 1
            }]
        )
        .is_err());
        assert!(capture(&" ".repeat(MAX_SPELLING + 1), &[]).is_err());
    }
}
