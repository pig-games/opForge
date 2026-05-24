// SPDX-License-Identifier: GPL-3.0-or-later

use crate::line::CachedRuntimeParseResult;
use std::cell::RefCell;

#[derive(Debug)]
pub(crate) struct PreparedSource {
    lines: Vec<PreparedLine>,
}

impl PreparedSource {
    pub(crate) fn from_lines(lines: &[String]) -> Self {
        let lines = lines
            .iter()
            .enumerate()
            .map(|(idx, source)| {
                PreparedLine::new(
                    u32::try_from(idx)
                        .unwrap_or(u32::MAX.saturating_sub(1))
                        .saturating_add(1),
                    source,
                )
            })
            .collect();
        Self { lines }
    }

    pub(crate) fn matches_lines(&self, lines: &[String]) -> bool {
        self.lines.len() == lines.len()
            && self
                .lines
                .iter()
                .zip(lines)
                .all(|(prepared, source)| prepared.source_hash == source_hash(source))
    }

    pub(crate) fn get(&self, index: usize) -> Option<&PreparedLine> {
        self.lines.get(index)
    }
}

#[derive(Debug)]
pub(crate) struct PreparedLine {
    line_num: u32,
    source_hash: u64,
    runtime_parse: RefCell<Option<CachedRuntimeParseResult>>,
}

impl PreparedLine {
    fn new(line_num: u32, source: &str) -> Self {
        Self {
            line_num,
            source_hash: source_hash(source),
            runtime_parse: RefCell::new(None),
        }
    }

    pub(crate) fn line_num(&self) -> u32 {
        self.line_num
    }

    pub(crate) fn cached_runtime_parse(&self) -> Option<CachedRuntimeParseResult> {
        self.runtime_parse.borrow().clone()
    }

    pub(crate) fn store_runtime_parse(&self, parsed: &CachedRuntimeParseResult) {
        *self.runtime_parse.borrow_mut() = Some(parsed.clone());
    }
}

fn source_hash(source: &str) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in source.as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}
