// SPDX-License-Identifier: GPL-3.0-or-later
//! Package-selected initial macro descriptors. Source traversal belongs to this
//! service; consumers copy selected spans and bind token identities.
use opcore::text_utils::{is_ident_char, is_ident_start};
use package::package::{PARSER_VM_MACRO_ENTRY, PARSER_VM_MACRO_VERSION};

pub const NONE: u32 = u32::MAX;
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TokenSpan {
    pub start: u32,
    pub end: u32,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Descriptor {
    pub kind: u16,
    pub flags: u16,
    pub token_start: u32,
    pub token_end: u32,
    pub source_start: u32,
    pub source_end: u32,
    pub aux: [u32; 3],
}
impl Descriptor {
    pub fn encode(&self) -> [u8; 32] {
        let mut out = [0; 32];
        out[..2].copy_from_slice(&self.kind.to_be_bytes());
        out[2..4].copy_from_slice(&self.flags.to_be_bytes());
        for (i, v) in [
            self.token_start,
            self.token_end,
            self.source_start,
            self.source_end,
            self.aux[0],
            self.aux[1],
            self.aux[2],
        ]
        .iter()
        .enumerate()
        {
            out[4 + i * 4..8 + i * 4].copy_from_slice(&v.to_be_bytes());
        }
        out
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DescriptorError {
    pub status: u32,
    pub offset: u32,
    pub message: &'static str,
}
impl DescriptorError {
    pub fn status(&self) -> u32 {
        self.status
    }
}
fn err(offset: usize, message: &'static str) -> DescriptorError {
    failure(4, offset, message)
}
fn failure(status: u32, offset: usize, message: &'static str) -> DescriptorError {
    DescriptorError {
        status,
        offset: offset as u32,
        message,
    }
}
struct Machine<'a> {
    source: &'a str,
    tokens: &'a [TokenSpan],
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
    fn record(
        &mut self,
        kind: u16,
        start: usize,
        end: usize,
    ) -> Result<Descriptor, DescriptorError> {
        self.tick(1)?;
        let a = self.tokens.partition_point(|t| t.end <= start as u32);
        let b = self.tokens.partition_point(|t| t.start < end as u32);
        if start != end
            && (a >= b
                || self.tokens[a].start < start as u32
                || self.tokens[b - 1].end > end as u32)
        {
            return Err(failure(5, start, "Span does not align with initial tokens"));
        }
        Ok(Descriptor {
            kind,
            flags: 0,
            token_start: a as u32,
            token_end: b as u32,
            source_start: start as u32,
            source_end: end as u32,
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
    fn trim(&self, a: usize, b: usize) -> (usize, usize) {
        let s = &self.source[a..b];
        let l = s.len() - s.trim_start().len();
        let n = s.trim().len();
        (a + l, a + l + n)
    }
    fn ws(&self, mut p: usize) -> usize {
        while self
            .source
            .as_bytes()
            .get(p)
            .is_some_and(|c| *c == b' ' || *c == b'\t')
        {
            p += 1;
        }
        p
    }
    fn ident(&self, p: usize) -> Option<usize> {
        let b = self.source.as_bytes();
        if !b.get(p).is_some_and(|c| is_ident_start(*c)) {
            return None;
        }
        let mut e = p + 1;
        while b.get(e).is_some_and(|c| is_ident_char(*c)) {
            e += 1;
        }
        Some(e)
    }
    fn envelope(&mut self, mode: u8, flags: u8) -> Result<(), DescriptorError> {
        if self.list.is_some()
            || !(1..=2).contains(&mode)
            || flags & !15 != 0
            || mode == 2 && flags & 4 != 0
        {
            return Err(failure(6, 0, "Invalid envelope policy"));
        }
        self.tick(self.source.len())?;
        if flags & 8 != 0 {
            let (code, _) = opcore::text_utils::split_comment(self.source);
            self.source = code;
        }
        self.header = mode == 2;
        let b = self.source.as_bytes();
        let mut p = self.ws(0);
        let mut label = None;
        if flags & 1 != 0 {
            if let Some(e) = self.ident(p) {
                label = Some((p, e));
                p = e;
                if b.get(p) == Some(&b':') {
                    p += 1;
                }
                p = self.ws(p);
            }
        }
        if b.get(p) != Some(&b'.') {
            return Err(err(p, "Expected dot statement"));
        }
        p += 1;
        if self.header {
            p = self.ws(p);
        }
        let head = p;
        let e = self.ident(p).ok_or(err(p, "Expected statement name"))?;
        p = self.ws(e);
        let mut role = 1;
        let name;
        if self.header {
            role = match self.source[head..e].to_ascii_uppercase().as_str() {
                "MACRO" => 2,
                "SEGMENT" => 3,
                _ => return Err(err(head, "Expected macro or segment header")),
            };
            if let Some(l) = label {
                name = l;
            } else {
                let e = self
                    .ident(p)
                    .ok_or(err(p, "Macro name is required after directive"))?;
                name = (p, e);
                p = self.ws(e);
            }
        } else {
            name = (head, e);
        }
        let mut a = p;
        let mut z = self.source.len();
        // Name-first headers retain their parentheses in the parameter text.
        if flags & 2 != 0 && b.get(p) == Some(&b'(') && (!self.header || label.is_none()) {
            let mut depth = 1usize;
            let mut quote = 0;
            let mut i = p + 1;
            let mut close = None;
            while i < b.len() {
                let c = b[i];
                if quote != 0 && c == b'\\' && i + 1 < b.len() {
                    i += 2;
                    continue;
                }
                if c == b'\'' || c == b'"' {
                    if quote == c {
                        quote = 0;
                    } else if quote == 0 {
                        quote = c;
                    }
                } else if quote == 0 {
                    if c == b'(' {
                        depth += 1;
                    } else if c == b')' {
                        depth -= 1;
                        if depth == 0 {
                            close = Some(i);
                            break;
                        }
                    }
                }
                i += 1;
            }
            let c = close.ok_or(err(p, "Unterminated argument list"))?;
            if !self.source[c + 1..].trim().is_empty() {
                return Err(err(c + 1, "Unexpected tokens after macro list"));
            }
            a = p + 1;
            z = c;
        } else if !self.header && flags & 4 != 0 && b.get(p) == Some(&b',') {
            a = self.ws(p + 1);
            if self.source[a..].is_empty() {
                return Err(err(p, "Empty macro argument list"));
            }
        }
        if self.header {
            (a, z) = self.trim(a, z);
        }
        let mut line = self.record(8, name.0, name.1)?;
        line.flags = role;
        line.source_start = a as u32;
        line.source_end = z as u32;
        line.aux = [
            1,
            0,
            label.map_or(NONE, |(s, _)| {
                self.tokens.partition_point(|t| t.end <= s as u32) as u32
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
        if self.source[a..z].trim().is_empty() {
            return Ok(());
        }
        let b = self.source.as_bytes();
        let mut depths = [0usize; 3];
        let mut quote = 0;
        let mut start = a;
        let mut i = a;
        while i <= z {
            let c = b.get(i).copied().unwrap_or(0);
            if i == z || c == sep && quote == 0 && depths == [0; 3] {
                let (s, e) = self.trim(start, i);
                if s == e {
                    return Err(err(s, "Macro argument or parameter cannot be empty"));
                }
                let r = self.record(9, s, e)?;
                self.push(r)?;
                start = i + 1;
            } else if quote != 0 && c == b'\\' && i + 1 < z {
                i += 1;
            } else if c == b'\'' || c == b'"' {
                if quote == c {
                    quote = 0;
                } else if quote == 0 {
                    quote = c;
                }
            } else if quote == 0 {
                match c {
                    b'(' => depths[0] += 1,
                    b'[' => depths[1] += 1,
                    b'{' => depths[2] += 1,
                    b')' => depths[0] = depths[0].saturating_sub(1),
                    b']' => depths[1] = depths[1].saturating_sub(1),
                    b'}' => depths[2] = depths[2].saturating_sub(1),
                    _ => {}
                }
            }
            i += 1;
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
            let eq = self.source[a..z].find('=').map(|i| a + i);
            let (s, e) = self.trim(a, eq.unwrap_or(z));
            let words = self.source[s..e].split_whitespace().collect::<Vec<_>>();
            if words.is_empty() || words.len() > 2 {
                return Err(err(s, "Invalid macro parameter format"));
            }
            let mut spans = Vec::new();
            let mut pos = s;
            for w in words {
                pos += self.source[pos..e].find(w).unwrap();
                if self.ident(pos) != Some(pos + w.len()) {
                    return Err(err(pos, "Invalid macro parameter identity"));
                }
                spans.push((pos, pos + w.len()));
                pos += w.len();
            }
            let n = *spans.last().unwrap();
            let mut formal = self.record(10, n.0, n.1)?;
            if spans.len() == 2 {
                formal.aux[0] = self.record(10, spans[0].0, spans[0].1)?.token_start;
            }
            if let Some(eq) = eq {
                let (d, e) = self.trim(eq + 1, z);
                formal.aux[1] = self.records.len() as u32;
                let r = self.record(11, d, e)?;
                self.push(r)?;
            }
            self.records[idx] = formal;
        }
        Ok(())
    }
}
/// Failures return no published records. Token spans use original source bytes,
/// including string quotes; decoded string buffers must not be passed here.
pub fn execute(
    entry: u16,
    version: u16,
    program: &[u8],
    source: &str,
    tokens: &[TokenSpan],
    record_capacity: usize,
    steps: usize,
) -> Result<Vec<Descriptor>, DescriptorError> {
    if entry != PARSER_VM_MACRO_ENTRY || version != PARSER_VM_MACRO_VERSION {
        return Err(err(0, "Invalid macro entry or version"));
    }
    if source.len() > u32::MAX as usize || tokens.len() > u32::MAX as usize {
        return Err(err(0, "Input extent overflow"));
    }
    let mut prev = 0;
    for t in tokens {
        if t.start < prev
            || t.start >= t.end
            || t.end as usize > source.len()
            || !source.is_char_boundary(t.start as usize)
            || !source.is_char_boundary(t.end as usize)
        {
            return Err(failure(5, t.start as usize, "Invalid token span"));
        }
        prev = t.end;
    }
    let mut m = Machine {
        source,
        tokens,
        budget: steps,
        cap: record_capacity.min(64),
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
    use package::package::macro_descriptor_program;
    fn spans(s: &str) -> Vec<TokenSpan> {
        s.char_indices()
            .filter(|(_, c)| !c.is_whitespace())
            .map(|(i, c)| TokenSpan {
                start: i as u32,
                end: (i + c.len_utf8()) as u32,
            })
            .collect()
    }
    fn run(s: &str, header: bool) -> Result<Vec<Descriptor>, DescriptorError> {
        execute(
            2,
            2,
            &macro_descriptor_program(header),
            s,
            &spans(s),
            64,
            10000,
        )
    }
    #[test]
    fn macro_descriptor_calls_match_live_core_substitution() {
        use opcore::macro_processor::MacroProcessor;
        for source in [
            ".m  a , [1,2]  ",
            ".m( a , \"x,y\" )",
            "  lab: .m , a, {b,c}",
            ".m ), b",
            ".m ",
            ".m a, b  ; comment",
        ] {
            let records = run(source, false).unwrap();
            let args = records
                .iter()
                .filter(|r| r.kind == 9)
                .map(|r| source[r.source_start as usize..r.source_end as usize].to_string())
                .collect::<Vec<_>>();
            let full = &source[records[0].source_start as usize..records[0].source_end as usize];
            let mut lines = vec![
                "m .macro p,q".into(),
                "RESULT @1|@2|.@".into(),
                ".endmacro".into(),
                source.into(),
            ];
            let expanded = MacroProcessor::new().expand(&lines).unwrap();
            let expected = format!(
                "RESULT {}|{}|{}",
                args.first().map_or("", String::as_str),
                args.get(1).map_or("", String::as_str),
                full
            );
            assert!(
                expanded.iter().any(|l| l.trim() == expected.trim()),
                "{source}: {expanded:?}"
            );
            lines.clear();
        }
    }
    #[test]
    fn macro_descriptor_headers_defaults_and_types() {
        let source = ".macro m(byte p = [1, 2], q= )";
        let r = run(source, true).unwrap();
        assert_eq!(r[0].flags, 2);
        assert_eq!(r[0].aux[1], 2);
        assert_eq!(r[1].kind, 10);
        assert_ne!(r[1].aux[0], NONE);
        let d = &r[r[1].aux[1] as usize];
        assert_eq!(
            &source[d.source_start as usize..d.source_end as usize],
            "[1, 2]"
        );
        assert_eq!(
            r[r[2].aux[1] as usize].source_start,
            r[r[2].aux[1] as usize].source_end
        );
        assert!(run("m .macro(p)", true).is_err());
        assert!(run("  m: .segment p=7", true).is_ok());
    }
    #[test]
    fn macro_descriptor_formals_match_live_core_defaults() {
        use opcore::macro_processor::MacroProcessor;
        for source in [
            ".macro m(byte p = [1, 2], q=7)",
            "  m: .macro p=\"x=y\", q=9",
            ".macro m p=, q=4",
        ] {
            let records = run(source, true).unwrap();
            let defaults = (1..=records[0].aux[1] as usize)
                .map(|i| {
                    let d = &records[records[i].aux[1] as usize];
                    &source[d.source_start as usize..d.source_end as usize]
                })
                .collect::<Vec<_>>();
            let lines = vec![
                source.into(),
                "RESULT @1|@2".into(),
                ".endmacro".into(),
                ".m".into(),
            ];
            let expanded = MacroProcessor::new().expand(&lines).unwrap();
            assert!(expanded
                .iter()
                .any(|s| s.trim() == format!("RESULT {}|{}", defaults[0], defaults[1])));
        }
    }
    #[test]
    fn macro_descriptor_failures_and_policy() {
        for source in [".m,", ".m a,,b", ".m(a) extra", ".m(a"] {
            assert!(run(source, false).is_err(), "{source}");
        }
        let source = ".m a,b";
        let tokens = spans(source);
        let p = macro_descriptor_program(false);
        assert!(execute(1, 2, &p, source, &tokens, 64, 1000).is_err());
        assert!(execute(2, 1, &p, source, &tokens, 64, 1000).is_err());
        assert!(execute(2, 2, &p, source, &tokens, 2, 1000).is_err());
        assert!(execute(2, 2, &p, source, &tokens, 64, 1).is_err());
        let mut changed = p;
        changed[2] = 0x80;
        assert!(execute(2, 2, &changed, source, &tokens, 64, 1000).is_err());
        let r = run(source, false).unwrap();
        assert_eq!(r[0].encode().len(), 32);
    }
    #[test]
    fn macro_descriptor_rejects_malformed_program_sequences() {
        let source = ".m a";
        let tokens = spans(source);
        for program in [
            vec![0x80],
            vec![0x80, 1],
            vec![0x80, 1, 15, 0x81],
            vec![0x80, 1, 15, 0x81, 1],
            vec![0xff],
            vec![0x80, 1, 15, 0x80, 1, 15],
            vec![0x80, 1, 15, 0x81, 1, b',', 0x81, 1, b','],
            vec![0x80, 1, 15, 0x83, 0],
            vec![0x80, 1, 15, 0x81, 1, b',', 0],
            vec![0x80, 1, 15, 0x81, 1, b',', 0x83],
            vec![0x80, 1, 15, 0x81, 1, b',', 0x83, 0, 0],
            vec![0x80, 1, 15, 0x81, 1, b',', 0x83, 0xff],
            vec![0x80, 1, 15, 0x81, 1, b',', 0x83, 0x83, 0],
        ] {
            let failure = execute(2, 2, &program, source, &tokens, 64, 1000).unwrap_err();
            assert_eq!(failure.status, 6, "{program:?}: {failure:?}");
        }
        let failure = execute(2, 2, &[0x80, 1], source, &tokens, 64, 1000).unwrap_err();
        assert_eq!(failure.offset, 1);
        let failure = execute(2, 2, &[0xff], source, &tokens, 64, 1000).unwrap_err();
        assert_eq!(failure.offset, 0);
        for tail in [vec![0xff], vec![0x83], vec![0x80, 1, 15]] {
            let mut program = macro_descriptor_program(false);
            program.pop();
            program.extend(tail);
            let failure = execute(2, 2, &program, source, &tokens, 64, 1000).unwrap_err();
            assert_eq!(
                (failure.status, failure.offset, failure.message),
                (6, 8, "Operation after publication")
            );
        }
    }

    #[test]
    fn macro_descriptor_rejects_invalid_lexical_spans() {
        let source = ".m a";
        let program = macro_descriptor_program(false);
        for (tokens, offset) in [
            (vec![TokenSpan { start: 1, end: 1 }], 1),
            (vec![TokenSpan { start: 0, end: 5 }], 0),
            (
                vec![
                    TokenSpan { start: 1, end: 2 },
                    TokenSpan { start: 0, end: 1 },
                ],
                0,
            ),
        ] {
            let failure = execute(2, 2, &program, source, &tokens, 64, 1000).unwrap_err();
            assert_eq!((failure.status, failure.offset), (5, offset));
        }
        let mut tokens = spans(source);
        tokens.pop();
        let failure = execute(2, 2, &program, source, &tokens, 64, 1000).unwrap_err();
        assert_eq!((failure.status, failure.offset), (5, 3));
        let failure = execute(
            2,
            2,
            &program,
            ".m é",
            &[TokenSpan { start: 4, end: 5 }],
            64,
            1000,
        )
        .unwrap_err();
        assert_eq!((failure.status, failure.offset), (5, 4));
    }
}
