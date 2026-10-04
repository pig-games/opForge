use super::*;

#[test]
fn reachable_imported_block_keeps_helper_referenced_by_unowned_code() {
    let assembled = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code",
        "entry .block",
        "    bsr compileExpression",
        "    rts",
        "    .bend",
        "compileExpression .block",
        "    bsr compileHighLow",
        "    rts",
        "    .bend",
        "compileHighLow",
        "    bsr skipWhitespace",
        "    rts",
        "skipWhitespace .block",
        "    rts",
        "    .bend",
        "unused .block",
        "    nop",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module main",
        ".cpu 68000",
        ".use dep",
        ".byte $42",
        ".endmodule",
    ]);
    assert!(!assembled.sections()["code"].bytes.is_empty());
    assert!(assembled.symbols.entry("dep.skipWhitespace").is_some());
    assert!(assembled.symbols.entry("dep.unused").is_none());
}

#[test]
fn hunk_output_entry_block_is_reachable() {
    let assembled = run_passes(&[
        ".module program",
        ".cpu 68000",
        ".use entry",
        ".output \"program\", format=hunk, sections=entry",
        ".endmodule",
        ".module entry",
        ".cpu 68000",
        ".section entry, kind=code",
        "start .block",
        "    rts",
        "    .bend",
        "unused .block",
        "    nop",
        "    .bend",
        ".endsection",
        ".endmodule",
    ]);
    assert_eq!(assembled.sections()["entry"].bytes, [0x4e, 0x75]);
    assert!(assembled.symbols.entry("entry.start").is_some());
    assert!(assembled.symbols.entry("entry.unused").is_none());
}

#[test]
fn selective_import_without_code_reference_emits_no_blocks() {
    let assembler = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "entry .block",
        "    .byte $11",
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

    assert!(assembler.sections()["app_code"].bytes.is_empty());
    assert!(assembler
        .symbols
        .reachable_units_from_root_references()
        .is_empty());
}

#[test]
fn unused_selected_import_does_not_require_section_mapping() {
    let assembler = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "entry .block",
        "    .byte $11",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module main",
        ".cpu 68000",
        ".use dep (entry) as d",
        ".endmodule",
    ]);

    assert!(assembler
        .symbols
        .reachable_units_from_root_references()
        .is_empty());
}

#[test]
fn reference_inside_discarded_block_does_not_retain_its_target() {
    let assembler = run_passes(&[
        ".module dep",
        ".cpu 68000",
        ".pub",
        ".section code, kind=code, logical",
        "live .block",
        "    .byte $11",
        "    .bend",
        "dead .block",
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
        ".use dep (live) as d map { code -> app_code }",
        ".section refs, kind=data",
        "    .word d.live",
        ".endsection",
        ".endmodule",
    ]);

    assert_eq!(assembler.sections()["app_code"].bytes, [0x11]);
    let reachable: Vec<_> = assembler
        .symbols
        .reachable_units_from_root_references()
        .into_iter()
        .map(|unit| unit.full_name)
        .collect();
    assert!(reachable.contains(&"dep.live".to_string()));
    assert!(!reachable.contains(&"dep.dead".to_string()));
    assert!(!reachable.contains(&"dep.helper".to_string()));
}

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
        ".section refs, kind=data",
        "    .word d.entry",
        ".endsection",
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
        ".section refs, kind=data",
        "    .word d.entry",
        ".endsection",
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
        ".section refs, kind=data",
        "    .word d.entry",
        ".endsection",
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
        ".section refs, kind=data",
        "    .word d.entry",
        ".endsection",
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
        ".section refs, kind=data",
        "    .word d.entry",
        ".endsection",
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
        ".section refs, kind=data",
        "    .word d.entry",
        ".endsection",
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
        ".section refs, kind=data",
        "    .word d.entry",
        ".endsection",
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
        ".section refs, kind=data",
        "    .word d.entry",
        ".endsection",
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
        ".section refs, kind=data",
        "    .word d.entry",
        ".endsection",
        ".endmodule",
    ]);

    assert_eq!(assembler.sections()["app_code"].bytes, [0x11]);
    assert!(!assembler
        .symbols
        .reachable_units_from_root_references()
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
        ".section refs, kind=data",
        "    .word d.entry",
        ".endsection",
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
        ".section refs, kind=data",
        "    .word d.entry",
        ".endsection",
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

#[test]
fn rooted_imported_entry_retains_short_branch_targets() {
    let lines = [
        ".module program",
        ".cpu 68000",
        ".use entry",
        ".output \"program\", format=hunk, sections=entry, code",
        ".endmodule",
        ".module entry",
        ".cpu 68000",
        ".use helper",
        ".section entry, kind=code",
        ".pub",
        "start .block",
        "    bsr.w helper.run",
        "    rts",
        "    .bend",
        ".endsection",
        ".endmodule",
        ".module helper",
        ".cpu 68000",
        ".section code, kind=code",
        ".pub",
        "run .block",
        "    tst.b d0",
        "    bne.s skip",
        "    nop",
        "skip",
        "    rts",
        "    .bend",
        "unused .block",
        "    nop",
        "    .bend",
        ".endsection",
        ".endmodule",
    ];
    let assembled = run_passes(&lines);
    assert!(!assembled.sections()["entry"].bytes.is_empty());
    assert!(!assembled.sections()["code"].bytes.is_empty());
    assert!(assembled.symbols.entry("helper.unused").is_none());
}

#[test]
fn absolute_long_reference_reaches_imported_block_and_its_helper() {
    let assembled = run_passes(&[
        ".module main",
        ".cpu 68020",
        ".use dep",
        ".section code, kind=code",
        "    jsr dep.entry.l",
        ".endsection",
        ".endmodule",
        ".module dep",
        ".cpu 68020",
        ".section code, kind=code",
        ".pub",
        "entry .block",
        "    bsr.w helper",
        "    rts",
        "    .bend",
        "helper .block",
        "    rts",
        "    .bend",
        "unused .block",
        "    nop",
        "    .bend",
        ".endsection",
        ".endmodule",
    ]);
    assert!(assembled.symbols.entry("dep.entry").is_some());
    assert!(assembled.symbols.entry("dep.helper").is_some());
    assert!(assembled.symbols.entry("dep.unused").is_none());
}

#[test]
fn mapped_worker_after_root_header_emits_final_address_and_bytes() {
    let assembler = run_passes(&[
        ".module proof.main",
        ".cpu 6502",
        ".use presenter.worker as worker map { code -> worker_code }",
        ".region image, $0900, $090f",
        ".section header, kind=code",
        "header",
        "    .word worker.entry",
        ".endsection",
        ".section worker_code, kind=code",
        ".endsection",
        ".place header in image",
        ".place worker_code in image",
        ".endmodule",
        ".module presenter.worker",
        ".cpu 6502",
        ".pub",
        ".section code, kind=code, logical",
        "entry .block",
        "    rts",
        "    .bend",
        ".endsection",
        ".endmodule",
    ]);

    assert_eq!(assembler.sections()["header"].bytes, [0x02, 0x09]);
    assert_eq!(assembler.sections()["worker_code"].bytes, [0x60]);
    assert_eq!(
        assembler.image().entries().expect("emitted image"),
        [(0x0900, 0x02), (0x0901, 0x09), (0x0902, 0x60)]
    );
    assert_eq!(
        assembler.symbols().lookup("proof.main.header"),
        Some(0x0900)
    );
    assert_eq!(
        assembler.symbols().lookup("presenter.worker.entry"),
        Some(0x0902)
    );
}

#[test]
fn mapped_worker_after_root_header_prunes_and_rebases_internal_labels() {
    let assembler = run_passes(&[
        ".module proof.main",
        ".cpu 6502",
        ".use presenter.worker as worker map { code -> worker_code }",
        ".region image, $0900, $090f",
        ".section header, kind=data",
        "header: .word worker.entry, worker.entry.inside",
        ".endsection",
        ".section worker_code, kind=code",
        ".endsection",
        ".place header in image",
        ".place worker_code in image",
        ".endmodule",
        ".module presenter.worker",
        ".cpu 6502",
        ".pub",
        ".section code, kind=code, logical",
        "unused .block",
        "    .byte $aa, $bb",
        "    .bend",
        "entry .block",
        "    nop",
        "inside:",
        "    .word inside",
        "    rts",
        "    .bend",
        ".endsection",
        ".endmodule",
    ]);

    assert_eq!(
        assembler.sections()["header"].bytes,
        [0x04, 0x09, 0x05, 0x09]
    );
    assert_eq!(
        assembler.sections()["worker_code"].bytes,
        [0xea, 0x05, 0x09, 0x60]
    );
    assert_eq!(
        assembler.image().entries().expect("emitted image"),
        [
            (0x0900, 0x04),
            (0x0901, 0x09),
            (0x0902, 0x05),
            (0x0903, 0x09),
            (0x0904, 0xea),
            (0x0905, 0x05),
            (0x0906, 0x09),
            (0x0907, 0x60),
        ]
    );
    assert_eq!(
        assembler.symbols().lookup("proof.main.header"),
        Some(0x0900)
    );
    assert_eq!(
        assembler.symbols().lookup("presenter.worker.entry"),
        Some(0x0904)
    );
    assert_eq!(
        assembler.symbols().lookup("presenter.worker.entry.inside"),
        Some(0x0905)
    );
    assert!(assembler
        .symbols()
        .entry("presenter.worker.unused")
        .is_none());
}

#[test]
fn mapped_worker_split_cli_emits_final_address_and_labels() {
    let dir = create_temp_dir("mapped-worker-split-cli");
    let input = dir.join("main.asm");
    let binary = dir.join("image.bin");
    let labels = dir.join("image.lbl");
    write_file(
        &input,
        r#".module proof.main
    .cpu 6502
    .use presenter.worker as worker map { code -> worker_code }
    .region image, $0900, $090f
    .section header, kind=code
header
    .word worker.entry
    .endsection
    .section worker_code, kind=code
    .endsection
    .place header in image
    .place worker_code in image
.endmodule
"#,
    );
    write_file(
        &dir.join("worker.asm"),
        r#".module presenter.worker
    .cpu 6502
    .pub
    .section code, kind=code, logical
entry .block
    rts
    .bend
    .endsection
.endmodule
"#,
    );
    let cli = Cli::parse_from([
        "opForge",
        "-i",
        input.to_string_lossy().as_ref(),
        "-b",
        binary.to_string_lossy().as_ref(),
        "--labels",
        labels.to_string_lossy().as_ref(),
    ]);
    let config = validate_cli(&cli).expect("validate mapped worker CLI");
    run_with_validated_cli_with_context(&cli, &config).expect("assemble split mapped worker");
    let payload = fs::read(&binary).expect("read complete mapped worker image");
    let exported_labels = fs::read_to_string(&labels).expect("read mapped worker labels");
    fs::remove_dir_all(&dir).expect("remove mapped worker CLI directory");

    assert_eq!(payload, [0x02, 0x09, 0x60]);
    assert!(
        exported_labels
            .lines()
            .any(|line| line == "proof.main.header = $0900"),
        "wrong header label: {exported_labels}"
    );
    assert!(
        exported_labels
            .lines()
            .any(|line| line == "presenter.worker.entry = $0902"),
        "wrong worker entry label: {exported_labels}"
    );
}
