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
