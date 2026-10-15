// SPDX-License-Identifier: GPL-3.0-or-later
//! Experimental recipes over original atom spelling, with no source reconstruction.
use crate::binary_spelling_vm::View;
use crate::macro_descriptor_vm::{Descriptor, DescriptorError};
use std::ops::Range;

/// Execute the selected fragment grammar over a complete canonical atom view.
/// Descriptor coordinates and budget charges match the ordinary fragment VM.
pub fn execute(
    entry: u16,
    version: u16,
    program: &[u8],
    block: &[u8],
    capacity: usize,
    steps: usize,
) -> Result<Vec<Descriptor>, DescriptorError> {
    let view = View::parse(block)?;
    execute_view(
        entry,
        version,
        program,
        &view,
        0..view.len(),
        capacity,
        steps,
    )
}

/// Scan one captured descriptor's logical spelling range. Results, named spans
/// and failure offsets are relative to `range.start`, as for an eager substring.
pub fn execute_range(
    entry: u16,
    version: u16,
    program: &[u8],
    block: &[u8],
    range: Range<usize>,
    capacity: usize,
    steps: usize,
) -> Result<Vec<Descriptor>, DescriptorError> {
    let view = View::parse(block)?;
    execute_view(entry, version, program, &view, range, capacity, steps)
}

fn execute_view(
    entry: u16,
    version: u16,
    program: &[u8],
    view: &View<'_>,
    range: Range<usize>,
    capacity: usize,
    steps: usize,
) -> Result<Vec<Descriptor>, DescriptorError> {
    if range.start > range.end || range.end > view.len() {
        return Err(DescriptorError {
            status: 4,
            offset: 0,
            message: "Invalid fragment range",
        });
    }
    crate::macro_fragment_vm::execute_bytes(
        entry,
        version,
        program,
        range.len(),
        |i| {
            (i < range.len())
                .then(|| view.byte(range.start + i))
                .flatten()
        },
        capacity,
        steps,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary_spelling_vm::{capture, Token};
    use crate::macro_fragment_vm;
    use package::package::macro_fragment_program;

    fn atoms(source: &str) -> Vec<u8> {
        // Deliberately split references and names between atoms.
        let tokens = source
            .char_indices()
            .map(|(a, c)| Token {
                kind: 0,
                start: a as u16,
                end: (a + c.len_utf8()) as u16,
            })
            .collect::<Vec<_>>();
        capture(source, &tokens).unwrap()
    }
    fn compare(source: &str, p: &[u8]) {
        let block = atoms(source);
        for capacity in [0, 1, 2, 64] {
            for budget in 0..80 {
                let eager = macro_fragment_vm::execute(4, 2, p, source, capacity, budget);
                let direct = execute(4, 2, p, &block, capacity, budget);
                match (eager, direct) {
                    (Ok(a), Ok(b)) => assert_eq!(a, b),
                    (Err(a), Err(b)) => assert_eq!((a.status, a.offset), (b.status, b.offset)),
                    pair => panic!("Differential failure {source:?} capacity={capacity} budget={budget}: {pair:?}"),
                }
            }
        }
    }
    #[test]
    fn eager_differential() {
        let p = macro_fragment_program();
        for s in [
            "",
            "abc",
            "é'@1'\\.2.@.name.{foo_2}@10",
            ".{bad .{} @0 é",
            "@1abc",
            "x.@\".name\"\\'@2'",
            "@1.2.@",
        ] {
            compare(s, &p);
        }
        compare(
            "@1%3!%![name9]",
            &[0x86, 1, b'%', b'!', b'3', b'5', b'[', b']', 0x83, 0],
        );
        for i in 0..p.len() {
            compare("@1", &p[..i]);
        }
    }
    #[test]
    fn complete_bound_and_record_cap() {
        let p = macro_fragment_program();
        for source in ["x".repeat(1024), "@1".repeat(65)] {
            let block = capture(
                &source,
                &[Token {
                    kind: 0,
                    start: 0,
                    end: source.len() as u16,
                }],
            )
            .unwrap();
            let eager = macro_fragment_vm::execute(4, 2, &p, &source, 100, 65536);
            let direct = execute(4, 2, &p, &block, 100, 65536);
            match (eager, direct) {
                (Ok(a), Ok(b)) => assert_eq!(a, b),
                (Err(a), Err(b)) => assert_eq!((a.status, a.offset), (b.status, b.offset)),
                pair => panic!("Bound differential failure: {pair:?}"),
            }
        }
    }
    #[test]
    fn ranges_and_validation() {
        let p = macro_fragment_program();
        let block = atoms("prefix @10.name tail");
        assert_eq!(
            execute_range(4, 2, &p, &block, 7..15, 64, 65536).unwrap(),
            macro_fragment_vm::execute(4, 2, &p, "@10.name", 64, 65536).unwrap()
        );
        assert_eq!(
            execute_range(
                4,
                2,
                &p,
                &block,
                std::ops::Range { start: 8, end: 7 },
                64,
                65536
            )
            .unwrap_err()
            .status,
            4
        );
        assert!(execute(4, 2, &p, &block[..block.len() - 1], 64, 65536).is_err());
    }
}
