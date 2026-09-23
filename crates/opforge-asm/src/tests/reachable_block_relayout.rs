use super::*;

#[test]
fn reachable_block_keeps_fallthrough_after_internal_label() {
    let assembler = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "entry .block",
        "    .byte $11",
        "inside:",
        "    .byte $22",
        "    .bend",
        "unused .block",
        "    .byte $33",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module main",
        ".cpu 68000",
        ".section app_code, kind=code",
        ".endsection",
        ".use dep (entry) as d map { code -> app_code }",
        ".endmodule",
    ]);

    assert_eq!(assembler.sections()["app_code"].bytes, [0x11, 0x22]);
}

#[test]
fn reachable_block_keeps_dependency_referenced_after_internal_label() {
    let assembler = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "entry .block",
        "    .byte $11",
        "inside:",
        "    .word helper",
        "    .bend",
        "helper .block",
        "    .byte $22",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module main",
        ".cpu 68000",
        ".section app_code, kind=code",
        ".endsection",
        ".use dep (entry) as d map { code -> app_code }",
        ".endmodule",
    ]);

    assert_eq!(
        assembler.sections()["app_code"].bytes,
        [0x11, 0x00, 0x03, 0x22]
    );
}

#[test]
fn reachable_block_reencodes_address_after_pruning_and_placement() {
    let assembler = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "unused .block",
        "    .byte $aa, $bb",
        "    .bend",
        "entry .block",
        "    .word entry",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module main",
        ".cpu 68000",
        ".region rom, $1000, $10ff",
        ".section app_code, kind=code",
        ".endsection",
        ".place app_code in rom",
        ".use dep (entry) as d map { code -> app_code }",
        ".output \"build/app.bin\", format=bin, sections=app_code",
        ".endmodule",
    ]);

    let output = assembler.root_metadata.linker_outputs.first().unwrap();
    let payload = build_linker_output_payload(output, assembler.sections()).unwrap();
    assert_eq!(payload, [0x10, 0x00]);
}

#[test]
fn reachable_block_rebases_after_68020_layout_stabilization() {
    let assembler = run_passes(&[
        ".module dep",
        ".cpu 68020",
        ".pub",
        ".section code, kind=code, logical",
        "unused .block",
        "    .byte $aa, $bb",
        "    .bend",
        "entry .block",
        "    .word entry",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module main",
        ".cpu 68020",
        ".region rom, $1000, $10ff",
        ".section app_code, kind=code",
        ".endsection",
        ".place app_code in rom",
        ".use dep (entry) as d map { code -> app_code }",
        ".endmodule",
    ]);

    assert_eq!(assembler.sections()["app_code"].bytes, [0x10, 0x00]);
}

#[test]
fn qualified_reference_to_internal_label_retains_its_whole_block() {
    let assembler = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "entry .block",
        "    .byte $11",
        "inside:",
        "    .byte $22",
        "    .bend",
        "unused .block",
        "    .byte $33",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module main",
        ".cpu 68000",
        ".region rom, $1000, $10ff",
        ".section app_code, kind=code",
        "    .word d.entry.inside",
        ".endsection",
        ".place app_code in rom",
        ".use dep as d map { code -> app_code }",
        ".endmodule",
    ]);

    assert_eq!(
        assembler.sections()["app_code"].bytes,
        [0x10, 0x03, 0x11, 0x22]
    );
}

#[test]
fn qualified_reference_to_block_entry_retains_its_whole_block() {
    let assembler = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "entry .block",
        "    .byte $11",
        "inside:",
        "    .byte $22",
        "    .bend",
        "unused .block",
        "    .byte $33",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module main",
        ".cpu 68000",
        ".region rom, $1000, $10ff",
        ".section app_code, kind=code",
        "    .word d.entry",
        ".endsection",
        ".place app_code in rom",
        ".use dep as d map { code -> app_code }",
        ".endmodule",
    ]);

    assert_eq!(
        assembler.sections()["app_code"].bytes,
        [0x10, 0x02, 0x11, 0x22]
    );
}

#[test]
fn qualified_reference_after_internal_label_retains_target_block() {
    let assembler = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "entry .block",
        "    .byte $11",
        "inside:",
        "    .word dep.helper",
        "    .bend",
        "helper .block",
        "    .byte $22",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module main",
        ".cpu 68000",
        ".section app_code, kind=code",
        ".endsection",
        ".use dep (entry) as d map { code -> app_code }",
        ".endmodule",
    ]);

    assert_eq!(
        assembler.sections()["app_code"].bytes,
        [0x11, 0x00, 0x03, 0x22]
    );
}

#[test]
fn qualified_reference_inside_block_pulls_mapped_dependency_module() {
    let assembler = run_passes(&[
        ".module util",
        ".pub",
        ".section tables, kind=data, logical",
        "helper: .long 1",
        ".endsection",
        ".endmodule",
        ".module dep",
        ".use util as u map { tables -> app_tables }",
        ".section app_tables, kind=data",
        ".endsection",
        ".pub",
        ".section code, kind=code, logical",
        "entry .block",
        "    .long u.helper",
        "    .bend",
        "unused .block",
        "    .byte $ff",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module main",
        ".section app_code, kind=code",
        ".endsection",
        ".use dep (entry) as d map { code -> app_code }",
        ".endmodule",
    ]);

    assert_eq!(assembler.sections()["app_tables"].bytes, [1, 0, 0, 0]);
    assert_eq!(assembler.sections()["app_code"].bytes, [0, 0, 0, 0]);
}

#[test]
fn reachable_block_pruning_keeps_unowned_bytes() {
    let assembler = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "    .byte $fe",
        "unused .block",
        "    .byte $33",
        "    .bend",
        "entry .block",
        "    .byte $11",
        "    .bend",
        "    .byte $ef",
        ".endsection",
        ".endmodule",
        ".module main",
        ".cpu 68000",
        ".section app_code, kind=code",
        ".endsection",
        ".use dep (entry) as d map { code -> app_code }",
        ".endmodule",
    ]);

    assert_eq!(assembler.sections()["app_code"].bytes, [0xfe, 0x11, 0xef]);
}

#[test]
fn unowned_reference_in_imported_section_retains_its_target_block() {
    let assembler = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "    .word helper",
        "entry .block",
        "    .byte $11",
        "    .bend",
        "helper .block",
        "    .byte $22",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module main",
        ".cpu 68000",
        ".section app_code, kind=code",
        ".endsection",
        ".use dep (entry) as d map { code -> app_code }",
        ".endmodule",
    ]);

    assert_eq!(
        assembler.sections()["app_code"].bytes,
        [0x00, 0x03, 0x11, 0x22]
    );
}

#[test]
fn unowned_reference_in_unmapped_section_does_not_pull_dependency() {
    let assembler = run_passes(&[
        ".module util",
        ".pub",
        ".section tables, kind=data, logical",
        "helper: .word $1234",
        ".endsection",
        ".endmodule",
        ".module dep",
        ".use util as u",
        ".pub",
        ".section code, kind=code, logical",
        "entry .block",
        "    .byte $11",
        "    .bend",
        ".endsection",
        ".section misc, kind=data, logical",
        "    .word u.helper",
        ".endsection",
        ".endmodule",
        ".module main",
        ".section app_code, kind=code",
        ".endsection",
        ".use dep (entry) as d map { code -> app_code }",
        ".endmodule",
    ]);

    assert_eq!(assembler.sections()["app_code"].bytes, [0x11]);
    assert!(!assembler
        .symbols
        .reachable_units_from_selected_roots()
        .iter()
        .any(|unit| unit.full_name == "util.helper"));
}

#[test]
fn mapped_block_must_fit_placed_region_after_relayout() {
    let lines: Vec<_> = [
        ".module dep",
        ".pub",
        ".section code, kind=code, logical",
        "entry .block",
        "    .byte $11, $22",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module main",
        ".region rom, $1000, $1000",
        ".section app_code, kind=code",
        ".endsection",
        ".place app_code in rom",
        ".use dep (entry) as d map { code -> app_code }",
        ".endmodule",
    ]
    .into_iter()
    .map(str::to_string)
    .collect();
    let mut assembler = Assembler::new();
    assert_eq!(assembler.pass1(&lines).errors, 0);
    let mut listing_output = Vec::new();
    let mut listing = ListingWriter::new(&mut listing_output, false);
    let pass2 = assembler.pass2(&lines, &mut listing).expect("pass2");

    assert_eq!(pass2.errors, 1);
    assert!(assembler.diagnostics.iter().any(|diagnostic| diagnostic
        .error
        .message()
        .contains("Mapped section exceeds its placed region")));
}

#[test]
fn reachable_block_branches_and_addresses_match_final_layout_reference() {
    let imported = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "unused .block",
        "    .byte $33, $44",
        "    .bend",
        "entry .block",
        "    bra helper",
        "    .word helper",
        "    .bend",
        "helper .block",
        "    rts",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module main",
        ".cpu 68000",
        ".region rom, $1000, $10ff",
        ".section app_code, kind=code",
        "    .byte $aa, $bb",
        ".endsection",
        ".place app_code in rom",
        ".use dep (entry) as d map { code -> app_code }",
        ".endmodule",
    ]);
    let direct = run_passes(&[
        ".module main",
        ".cpu 68000",
        ".region rom, $1000, $10ff",
        ".section app_code, kind=code",
        "    .byte $aa, $bb",
        "entry .block",
        "    bra helper",
        "    .word helper",
        "    .bend",
        "helper .block",
        "    rts",
        "    .bend",
        ".endsection",
        ".place app_code in rom",
        ".endmodule",
    ]);

    assert_eq!(
        imported.sections()["app_code"].bytes,
        direct.sections()["app_code"].bytes
    );
}
