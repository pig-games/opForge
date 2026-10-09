// SPDX-License-Identifier: GPL-3.0-or-later

use crate::{engine::Assembler, listing::ListingWriter};
use families::{
    register_intel8080_family_stack, register_mos6502_family_stack,
    register_motorola68000_family_stack,
};
use registry::{cpu::CpuType, registry::ModuleRegistry};

fn assembler(cpu: &'static str) -> Assembler {
    let mut registry = ModuleRegistry::new();
    register_mos6502_family_stack(&mut registry);
    register_intel8080_family_stack(&mut registry);
    register_motorola68000_family_stack(&mut registry);
    Assembler::with_cpu_and_registry(CpuType::new(cpu), registry)
}

#[test]
fn forward_byte_expression_keeps_its_width_during_layout() {
    let lines = [
        ".org $100",
        "start:",
        ".byte end-start, end-start+1",
        ".word end",
        "end:",
        ".byte end-start",
    ]
    .map(str::to_owned);
    for (cpu, word) in [("m6502", [4, 1]), ("8085", [4, 1]), ("m68000", [1, 4])] {
        let mut assembler = assembler(cpu);
        let layout = assembler.pass1(&lines);
        assert_eq!(layout.errors, 0, "{cpu}: {:?}", assembler.diagnostics);
        assert_eq!(
            layout.warnings, 0,
            "unresolved values must not cause truncation warnings"
        );
        let mut listing = Vec::new();
        let result = assembler
            .pass2(&lines, &mut ListingWriter::new(&mut listing, false))
            .unwrap();
        assert_eq!(result.errors, 0, "{cpu}: {:?}", assembler.diagnostics);
        assert_eq!(result.warnings, 0);
        let expected = [4, 5, word[0], word[1], 4]
            .into_iter()
            .enumerate()
            .map(|(offset, byte)| (0x100 + offset as u32, byte))
            .collect::<Vec<_>>();
        assert_eq!(assembler.image().entries().unwrap(), expected, "{cpu}");
    }
}

#[test]
fn resolved_out_of_range_byte_still_warns() {
    let mut assembler = assembler("m6502");
    let lines = [".byte 256".to_owned()];
    assert_eq!(assembler.pass1(&lines).errors, 0);
    let mut listing = Vec::new();
    let result = assembler
        .pass2(&lines, &mut ListingWriter::new(&mut listing, false))
        .unwrap();
    assert_eq!(result.warnings, 1);
}

#[test]
fn byte_truncation_emits_every_operand_and_preserves_following_labels() {
    let lines = [
        ".org $100",
        ".byte 1, 256, 300, -1, 2, $100000000, $100000100",
        "after:",
        ".word $12345678",
        ".long $123456789",
        ".byte after-$100",
    ]
    .map(str::to_owned);
    for (cpu, wide) in [
        ("m6502", [0x78, 0x56, 0x89, 0x67, 0x45, 0x23]),
        ("8085", [0x78, 0x56, 0x89, 0x67, 0x45, 0x23]),
        ("m68000", [0x56, 0x78, 0x23, 0x45, 0x67, 0x89]),
    ] {
        let mut assembler = assembler(cpu);
        assert_eq!(assembler.pass1(&lines).errors, 0, "{cpu}");
        assert_eq!(assembler.symbols().lookup("after"), Some(0x107), "{cpu}");
        let mut listing = Vec::new();
        let result = assembler
            .pass2(&lines, &mut ListingWriter::new(&mut listing, false))
            .unwrap();
        assert_eq!(result.errors, 0, "{cpu}: {:?}", assembler.diagnostics);
        assert_eq!(result.warnings, 4, "{cpu}");
        let warnings = assembler
            .diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.severity == crate::error::Severity::Warning)
            .collect::<Vec<_>>();
        assert_eq!(warnings.len(), 4, "{cpu}");
        assert!(warnings.iter().all(|warning| warning.line == 2));
        assert!(warnings
            .windows(2)
            .all(|pair| pair[0].column < pair[1].column));
        let expected = [1, 0, 44, 255, 2, 0, 0]
            .into_iter()
            .chain(wide)
            .chain([7])
            .enumerate()
            .map(|(offset, byte)| (0x100 + offset as u32, byte))
            .collect::<Vec<_>>();
        assert_eq!(assembler.image().entries().unwrap(), expected, "{cpu}");
        let listing = String::from_utf8(listing).unwrap();
        assert_eq!(
            listing.matches("Value truncated to byte").count(),
            4,
            "{cpu}"
        );
    }
}

#[test]
fn byte_warning_survives_an_error_in_a_later_operand() {
    let lines = [".byte 300, missing".to_owned()];
    let mut assembler = assembler("m6502");
    assembler.pass1(&lines);
    let mut listing = Vec::new();
    let result = assembler
        .pass2(&lines, &mut ListingWriter::new(&mut listing, false))
        .unwrap();
    assert_eq!(result.errors, 1);
    assert_eq!(result.warnings, 1);
    assert_eq!(assembler.image().entries().unwrap(), vec![(0, 44)]);
}
