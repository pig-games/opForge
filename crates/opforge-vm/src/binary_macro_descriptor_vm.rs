// SPDX-License-Identifier: GPL-3.0-or-later
//! Experimental descriptor execution directly over validated token/spelling atoms.
use crate::binary_spelling_vm::View;
use crate::macro_descriptor_vm::{Descriptor, DescriptorError, NONE};
use opcore::text_utils::{is_ident_char, is_ident_start};
use package::package::{PARSER_VM_MACRO_ENTRY, PARSER_VM_MACRO_VERSION};

fn failure(status: u32, offset: usize, message: &'static str) -> DescriptorError {
    DescriptorError {
        status,
        offset: offset as u32,
        message,
    }
}
struct Machine<'a> {
    view: View<'a>,
    budget: usize,
    cap: usize,
    records: Vec<Descriptor>,
    list: Option<(usize, usize)>,
    header: bool,
}
impl Machine<'_> {
    fn tick(&mut self, n: usize) -> Result<(), DescriptorError> {
        self.budget = self
            .budget
            .checked_sub(n)
            .ok_or(failure(12, 0, "Step budget exceeded"))?;
        Ok(())
    }
    fn record(&mut self, kind: u16, a: usize, b: usize) -> Result<Descriptor, DescriptorError> {
        self.tick(1)?;
        let tokens = self.view.tokens();
        let start = tokens.partition_point(|t| t.end as usize <= a);
        let end = tokens.partition_point(|t| (t.start as usize) < b);
        if a != b
            && (start >= end
                || (tokens[start].start as usize) < a
                || tokens[end - 1].end as usize > b)
        {
            return Err(failure(5, a, "Span does not align with initial tokens"));
        }
        Ok(Descriptor {
            kind,
            flags: 0,
            token_start: start as u32,
            token_end: end as u32,
            source_start: a as u32,
            source_end: b as u32,
            aux: [NONE; 3],
        })
    }
    fn push(&mut self, r: Descriptor) -> Result<(), DescriptorError> {
        if self.records.len() >= self.cap {
            return Err(failure(
                7,
                r.source_start as usize,
                "Descriptor capacity exceeded",
            ));
        }
        self.records.push(r);
        Ok(())
    }
    fn ws(&self, mut p: usize) -> usize {
        while matches!(self.view.byte(p), Some(b' ' | b'\t')) {
            p += 1;
        }
        p
    }
    fn ident(&self, p: usize) -> Option<usize> {
        if !self.view.byte(p).is_some_and(is_ident_start) {
            return None;
        }
        let mut end = p + 1;
        while self.view.byte(end).is_some_and(is_ident_char) {
            end += 1;
        }
        Some(end)
    }
    fn word_eq(&self, a: usize, b: usize, word: &[u8]) -> bool {
        b - a == word.len()
            && word.iter().enumerate().all(|(i, c)| {
                self.view
                    .byte(a + i)
                    .is_some_and(|v| v.eq_ignore_ascii_case(c))
            })
    }
    fn envelope(&mut self, mode: u8, flags: u8) -> Result<(), DescriptorError> {
        if self.list.is_some()
            || !(1..=2).contains(&mode)
            || flags & !15 != 0
            || mode == 2 && flags & 4 != 0
        {
            return Err(failure(6, 0, "Invalid envelope policy"));
        }
        self.tick(self.view.len())?;
        self.header = mode == 2;
        let mut p = self.ws(0);
        let mut label = None;
        if flags & 1 != 0 {
            if let Some(e) = self.ident(p) {
                label = Some((p, e));
                p = e;
                if self.view.token_at(p).is_some_and(|t| t.kind == 5) {
                    p += 1;
                }
                p = self.ws(p);
            }
        }
        if self.view.token_at(p).is_none_or(|t| t.kind != 7) {
            return Err(failure(4, p, "Expected dot statement"));
        }
        p += 1;
        if self.header {
            p = self.ws(p);
        }
        let head = p;
        let e = self
            .ident(p)
            .ok_or(failure(4, p, "Expected statement name"))?;
        p = self.ws(e);
        let mut role = 1;
        let name = if self.header {
            role = if self.word_eq(head, e, b"MACRO") {
                2
            } else if self.word_eq(head, e, b"SEGMENT") {
                3
            } else {
                return Err(failure(4, head, "Expected macro or segment header"));
            };
            if let Some(l) = label {
                l
            } else {
                let e =
                    self.ident(p)
                        .ok_or(failure(4, p, "Macro name is required after directive"))?;
                let name = (p, e);
                p = self.ws(e);
                name
            }
        } else {
            (head, e)
        };
        let mut a = p;
        let mut z = self.view.len();
        if flags & 2 != 0
            && self.view.token_at(p).is_some_and(|t| t.kind == 14)
            && (!self.header || label.is_none())
        {
            let mut depth = 1;
            let mut close = None;
            for t in self.view.tokens().iter().filter(|t| t.start as usize > p) {
                match t.kind {
                    14 => depth += 1,
                    15 => {
                        depth -= 1;
                        if depth == 0 {
                            close = Some(t.start as usize);
                            break;
                        }
                    }
                    _ => (),
                }
            }
            let c = close.ok_or(failure(4, p, "Unterminated argument list"))?;
            let (tail, end) = self.view.trim(c + 1, self.view.len());
            if tail != end {
                return Err(failure(4, c + 1, "Unexpected tokens after macro list"));
            }
            a = p + 1;
            z = c;
        } else if !self.header
            && flags & 4 != 0
            && self.view.token_at(p).is_some_and(|t| t.kind == 4)
        {
            a = self.ws(p + 1);
            if a == self.view.len() {
                return Err(failure(4, p, "Empty macro argument list"));
            }
        }
        if self.header {
            (a, z) = self.view.trim(a, z);
        }
        let mut line = self.record(8, name.0, name.1)?;
        line.flags = role;
        line.source_start = a as u32;
        line.source_end = z as u32;
        line.aux = [
            1,
            0,
            label.map_or(NONE, |(s, _)| {
                self.view.tokens().partition_point(|t| t.end as usize <= s) as u32
            }),
        ];
        self.push(line)?;
        self.list = Some((a, z));
        Ok(())
    }
    fn split(&mut self, policy: u8, sep: u8) -> Result<(), DescriptorError> {
        if policy != 1 || sep != b',' || self.records.len() != 1 {
            return Err(failure(6, 0, "Invalid split policy or order"));
        }
        let (a, z) = self.list.ok_or(failure(6, 0, "Envelope required"))?;
        self.tick(z - a)?;
        let (s, e) = self.view.trim(a, z);
        if s == e {
            return Ok(());
        }
        let mut depths = [0usize; 3];
        let mut boundaries = Vec::new();
        for token in self
            .view
            .tokens()
            .iter()
            .filter(|t| t.start as usize >= a && (t.start as usize) < z)
        {
            let slot = match token.kind {
                14 | 15 => Some(0),
                10 | 11 => Some(1),
                12 | 13 => Some(2),
                _ => None,
            };
            if let Some(i) = slot {
                if matches!(token.kind, 10 | 12 | 14) {
                    depths[i] += 1;
                } else {
                    depths[i] = depths[i].saturating_sub(1);
                }
            } else if token.kind == 4 && depths == [0; 3] {
                boundaries.push(token.start as usize);
            }
        }
        boundaries.push(z);
        let mut start = a;
        for end in boundaries {
            let (s, e) = self.view.trim(start, end);
            if s == e {
                return Err(failure(4, s, "Macro argument or parameter cannot be empty"));
            }
            let row = self.record(9, s, e)?;
            self.push(row)?;
            start = end + 1;
        }
        self.records[0].aux[1] = (self.records.len() - 1) as u32;
        Ok(())
    }
    fn formals(&mut self, policy: u8) -> Result<(), DescriptorError> {
        if policy != 1 || !self.header {
            return Err(failure(6, 0, "Invalid formal policy"));
        }
        let count = self.records[0].aux[1] as usize;
        for idx in 1..=count {
            let a = self.records[idx].source_start as usize;
            let z = self.records[idx].source_end as usize;
            self.tick(z - a)?;
            // Preserve the current grammar's first raw '=' ownership, including
            // '=' inside a quoted token, rather than silently changing policy.
            let eq = (a..z).find(|p| self.view.byte(*p) == Some(b'='));
            let (s, e) = self.view.trim(a, eq.unwrap_or(z));
            let mut words = Vec::new();
            let mut pos = s;
            while pos < e {
                let start = pos;
                while pos < e && self.view.whitespace(pos) == 0 {
                    pos += 1;
                }
                words.push((start, pos));
                while pos < e && self.view.whitespace(pos) > 0 {
                    pos += self.view.whitespace(pos);
                }
            }
            if words.is_empty() || words.len() > 2 {
                return Err(failure(4, s, "Invalid macro parameter format"));
            }
            for &(start, end) in &words {
                if self.ident(start) != Some(end) {
                    return Err(failure(4, start, "Invalid macro parameter identity"));
                }
            }
            let (s, e) = *words.last().unwrap();
            let mut formal = self.record(10, s, e)?;
            if words.len() == 2 {
                formal.aux[0] = self.record(10, words[0].0, words[0].1)?.token_start;
            }
            if let Some(eq) = eq {
                let (d, e) = self.view.trim(eq + 1, z);
                formal.aux[1] = self.records.len() as u32;
                let row = self.record(11, d, e)?;
                self.push(row)?;
            }
            self.records[idx] = formal;
        }
        Ok(())
    }
}

/// Execute the selected shared descriptor grammar without a source parser.
/// Comments are absent from the captured extent; flags still select all other
/// envelope policies. Result coordinates name logical atom spelling positions.
pub fn execute(
    entry: u16,
    version: u16,
    program: &[u8],
    block: &[u8],
    capacity: usize,
    steps: usize,
) -> Result<Vec<Descriptor>, DescriptorError> {
    if entry != PARSER_VM_MACRO_ENTRY || version != PARSER_VM_MACRO_VERSION {
        return Err(failure(4, 0, "Invalid macro entry or version"));
    }
    let mut m = Machine {
        view: View::parse(block)?,
        budget: steps,
        cap: capacity.min(64),
        records: Vec::new(),
        list: None,
        header: false,
    };
    let mut pc = 0;
    let mut published = false;
    let mut split = false;
    let mut formal = false;
    while pc < program.len() {
        m.tick(1)?;
        let op = program[pc];
        pc += 1;
        let n = match op {
            0x80 | 0x81 => 2,
            0x82 => 1,
            _ => 0,
        };
        if pc + n > program.len() {
            return Err(failure(6, pc, "Truncated macro program"));
        }
        if published && op != 0 {
            return Err(failure(6, pc, "Operation after publication"));
        }
        match op {
            0x80 => m.envelope(program[pc], program[pc + 1])?,
            0x81 => {
                if split {
                    return Err(failure(6, pc, "Invalid split policy or order"));
                }
                m.split(program[pc], program[pc + 1])?;
                split = true;
            }
            0x82 => {
                if !split || formal {
                    return Err(failure(6, pc, "Invalid formal order"));
                }
                m.formals(program[pc])?;
                formal = true;
            }
            0x83 => {
                if !split || m.header && !formal || published {
                    return Err(failure(6, pc, "Invalid publish order"));
                }
                published = true;
            }
            0 => {
                if !published || pc != program.len() {
                    return Err(failure(6, pc, "Invalid program termination"));
                }
                return Ok(m.records);
            }
            _ => return Err(failure(6, pc - 1, "Invalid macro opcode")),
        }
        pc += n;
    }
    Err(failure(6, pc, "Missing macro end"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary_spelling_vm::{capture, Token};
    use crate::macro_descriptor_vm::{self, TokenSpan};
    use opcore::tokenizer::{TokenKind, Tokenizer};
    use package::package::macro_descriptor_program;

    fn tokens(source: &str) -> Vec<Token> {
        let mut lexer = Tokenizer::new(source, 1);
        let mut tokens = Vec::new();
        loop {
            let t = lexer.next_token().unwrap();
            let kind = match t.kind {
                TokenKind::End => break,
                TokenKind::Identifier(_) => 0,
                TokenKind::Register(_) => 1,
                TokenKind::Number(_) => 2,
                TokenKind::String(_) => 3,
                TokenKind::Comma => 4,
                TokenKind::Colon => 5,
                TokenKind::Dollar => 6,
                TokenKind::Dot => 7,
                TokenKind::Hash => 8,
                TokenKind::Question => 9,
                TokenKind::OpenBracket => 10,
                TokenKind::CloseBracket => 11,
                TokenKind::OpenBrace => 12,
                TokenKind::CloseBrace => 13,
                TokenKind::OpenParen => 14,
                TokenKind::CloseParen => 15,
                TokenKind::Operator(_) => 18,
                TokenKind::At => 40,
            };
            tokens.push(Token {
                kind,
                start: (t.span.col_start - 1) as u16,
                end: (t.span.col_end - 1) as u16,
            });
        }
        tokens
    }
    fn compare(source: &str, header: bool) {
        let ts = tokens(source);
        let spans = ts
            .iter()
            .map(|t| TokenSpan {
                start: t.start as u32,
                end: t.end as u32,
            })
            .collect::<Vec<_>>();
        let program = macro_descriptor_program(header);
        let reference = macro_descriptor_vm::execute(2, 2, &program, source, &spans, 64, 65536);
        let block = capture(source, &ts).unwrap();
        let actual = execute(2, 2, &program, &block, 64, 65536);
        match (reference, actual) {
            (Ok(a), Ok(b)) => assert_eq!(a, b, "{source}"),
            (Err(a), Err(b)) => assert_eq!((a.status, a.offset), (b.status, b.offset), "{source}"),
            (a, b) => panic!("{source}: {a:?} != {b:?}"),
        }
    }
    #[test]
    fn binary_descriptors_match_eager_tokenized_calls_and_headers() {
        for s in [
            ".m  a , [1,2]  ",
            ".m( a , \"x,y\" )",
            "  lab: .m , a, {b,c}",
            ".m ), b",
            ".m ",
            ".m,",
            ".m a,,b",
            ".m(a) extra",
            ".m(a",
            ".m 1\t+\t2",
            ".m \"x\\\"(,y\",9",
            ".m .{param},@1",
        ] {
            compare(s, false);
        }
        for s in [
            ".macro m(byte p = [1, 2], q= )",
            "m .macro(p)",
            "  m: .segment p=7",
            ".macro m p=\"x=y\", q=9",
            ".macro m p=, q=4",
            ".macro m byte a, word b",
            ".macro m a b c",
            ".macro m =5",
        ] {
            compare(s, true);
        }
    }
    #[test]
    fn binary_descriptor_failure_is_not_partial_publication() {
        let s = ".m a,b";
        let block = capture(s, &tokens(s)).unwrap();
        let p = macro_descriptor_program(false);
        assert_eq!(execute(2, 2, &p, &block, 1, 65536).unwrap_err().status, 7);
        assert_eq!(execute(2, 2, &p, &block, 64, 1).unwrap_err().status, 12);
        let mut bad = p;
        bad.push(0);
        assert_eq!(
            execute(2, 2, &bad, &block, 64, 65536).unwrap_err().status,
            6
        );
    }
}
