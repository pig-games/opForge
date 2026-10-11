//! Reachable mapped extents participate in concrete placement before emission.
use super::*;

fn source(prefix: &str, align: u32, end: u32, unused: bool) -> String {
    let dead = if unused {
        "unused .block\n.byte 1,2,3,4,5,6,7,8\n.bend\n"
    } else {
        ""
    };
    format!(".module high\n.cpu 6502\n.pub\n.section upper,kind=code,logical\nentry .block\n.byte $44\n.bend\n{dead}.endsection\n.endmodule\n.module low\n.cpu 6502\n.pub\n.section lower,kind=code,logical\nentry .block\n.byte $11\n.bend\n.endsection\n.endmodule\n.module main\n.cpu 6502\n.use high map {{ upper -> high_code }}\n.use low map {{ lower -> low_code }}\n.region ram,$1000,${end:x}\n.section low_code,kind=code\n{prefix}.endsection\n.section header,kind=data\nentry: .word high.entry,low.entry\n.endsection\n.section high_code,kind=code,align={align}\n.endsection\n.place low_code in ram\n.place header in ram\n.place high_code in ram\n.endmodule\n")
}

#[test]
fn mapped_placement_reserves_growth_and_replays_labels() {
    for (cpu, expected) in [
        ("6502", [0x11, 5, 0x10, 0, 0x10, 0x44]),
        ("68020", [0x11, 0x10, 5, 0x10, 0, 0x44]),
    ] {
        let input = source("", 1, 0x10ff, false).replace(".cpu 6502", &format!(".cpu {cpu}"));
        let assembler = run_passes(&input.lines().collect::<Vec<_>>());
        let mut entries = assembler.image().entries().unwrap();
        entries.sort_by_key(|(address, _)| *address);
        assert_eq!(entries, (0x1000..).zip(expected).collect::<Vec<_>>());
        for (name, value) in [
            ("low.entry", 0x1000),
            ("main.entry", 0x1001),
            ("high.entry", 0x1005),
        ] {
            assert_eq!(assembler.symbols().lookup(name), Some(value));
        }
    }
}

#[test]
fn mapped_placement_prunes_before_reserving_capacity() {
    let input = source("", 1, 0x1005, true);
    let assembler = run_passes(&input.lines().collect::<Vec<_>>());
    assert_eq!(assembler.sections()["high_code"].bytes, [0x44]);
    assert_eq!(assembler.symbols().lookup("high.unused"), None);
    assert_eq!(assembler.image().entries().unwrap().len(), 6);
}

#[test]
fn mapped_placement_preserves_concrete_prefix_and_alignment() {
    let input = source(".byte $aa,$bb\n", 8, 0x10ff, false);
    let assembler = run_passes(&input.lines().collect::<Vec<_>>());
    assert_eq!(assembler.sections()["low_code"].bytes, [0xaa, 0xbb, 0x11]);
    assert_eq!(assembler.symbols().lookup("low.entry"), Some(0x1002));
    assert_eq!(assembler.symbols().lookup("main.entry"), Some(0x1003));
    assert_eq!(assembler.symbols().lookup("high.entry"), Some(0x1008));
    assert_eq!(assembler.sections()["header"].bytes, [8, 0x10, 2, 0x10]);
}

#[test]
fn mapped_placement_rejects_region_capacity_exceeded() {
    let input = source("", 1, 0x1004, false);
    let assembler = run_pass1(&input.lines().collect::<Vec<_>>());
    assert!(
        assembler
            .diagnostics
            .iter()
            .any(|diag| diag.error.message().contains("overflows region")),
        "expected capacity diagnostic: {:?}",
        assembler.diagnostics
    );
}

#[test]
fn mapped_placement_replays_shifted_concrete_labels_with_stable_logical_origin() {
    let input = source("", 1, 0x10ff, false)
        .replace(".use high map { upper -> high_code }\n", "")
        .replace(".word high.entry,low.entry", ".word low.entry,low.entry");
    let assembler = run_passes(&input.lines().collect::<Vec<_>>());
    assert_eq!(assembler.symbols().lookup("low.entry"), Some(0x1000));
    assert_eq!(assembler.symbols().lookup("main.entry"), Some(0x1001));
    assert_eq!(assembler.sections()["header"].bytes, [0, 0x10, 0, 0x10]);
}
