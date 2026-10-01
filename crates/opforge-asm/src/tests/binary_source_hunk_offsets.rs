//! Section bases cancel only for same-section address differences.
use super::*;

fn source(placed: bool) -> String {
    let placement = if placed {
        ".region ram,$2000,$2fff\n.place data in ram\n"
    } else {
        ""
    };
    format!(
        ".module offsets\n.cpu m68020\n.section code,kind=code\n.byte End-Start\n.word End-Start\n.long End-Start\n.long (End+2)-(Start+1),End-End\n.endsection\n.section data,kind=data\nStart .byte $11,$22,$33,$44\nEnd .byte $55\n.endsection\n.section bss,kind=bss\n.res byte,12\n.endsection\n{placement}.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule\n"
    )
}

#[test]
fn compact_hunk_same_section_offsets_rust_oracle() {
    for placed in [false, true] {
        let oracle = hunk_sections::rust_hunk_source(&source(placed));
        let segments = hunk::segments(&oracle).expect("valid same-section offsets Hunk");
        assert_eq!(
            segments[0].payload,
            [4, 0, 4, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 0, 0]
        );
        assert!(segments[0].relocations.is_empty());
    }
}

#[test]
#[ignore = "requires configured FS-UAE; exact Hunk comparison for forward same-section offsets"]
fn compact_hunk_same_section_offsets_fs_uae() {
    hunk_sections::native_hunk_source(&source(false));
}

// Start is already defined at a nonzero offset; End is forward and has no
// following emission. Pass one must defer both numeric range and base proof.
const FORWARD_OFFSET: &str = ".module catalog_offsets\n.cpu m68020\n.section code,kind=code\n.byte 0\n.endsection\n.section data,kind=data\n.byte 0,0,0,0\nStart\n.byte End-Start\n.word End-Start\n.long End-Start\n.long (End+2)-(Start+1),End-End\nEnd\n.endsection\n.section bss,kind=bss\n.res byte,12\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n";

#[test]
fn compact_hunk_forward_offset_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(FORWARD_OFFSET);
    let segments = hunk::segments(&oracle).expect("valid forward offset Hunk");
    assert_eq!(segments.len(), 3);
    assert_eq!(
        segments[1].payload,
        [0, 0, 0, 0, 15, 0, 15, 0, 0, 0, 15, 0, 0, 0, 16, 0, 0, 0, 0, 0]
    );
    assert!(segments[1].relocations.is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; forward offset with one known section base"]
fn compact_hunk_forward_offset_fs_uae() {
    hunk_sections::native_hunk_source(FORWARD_OFFSET);
}

fn catalog_source() -> String {
    let module =
        include_str!("../../../../native/motorola68000/amigaos/experimental/binary_catalog.asm");
    let catalog =
        include_str!("../../../../native/motorola68000/amigaos/experimental/package_catalog.i");
    format!(".module main\n.cpu m68020\n.use experimental.amigaos.binary_catalog as lookup\n.section code,kind=code\n jsr lookup.find\n.endsection\n.section data,kind=data\n{catalog}\n.endsection\n.section bss,kind=bss\n.res byte,12\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule\n{module}\n")
}

#[test]
fn compact_hunk_catalog_rust_oracle() {
    assert!(hunk_sections::rust_hunk_source(&catalog_source()).len() > 1000);
}

#[test]
#[ignore = "requires configured FS-UAE; actual catalog module/table self-assembly"]
fn compact_hunk_catalog_fs_uae() {
    hunk_sections::native_hunk_source(&catalog_source());
}

const FORWARD_IMMEDIATE: &str = ".module forward_immediate\n.cpu m68020\n.section code,kind=code\nStart\n move.l #End-Start,d3\nEnd\n.endsection\n.section data,kind=data\n.byte 0\n.endsection\n.section bss,kind=bss\n.res byte,12\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule\n";

#[test]
fn compact_hunk_forward_immediate_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(FORWARD_IMMEDIATE);
    let segments = hunk::segments(&oracle).unwrap();
    assert_eq!(&segments[0].payload[..6], [0x26, 0x3c, 0, 0, 0, 6]);
    assert!(segments[0].relocations.is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; forward label difference in an instruction"]
fn compact_hunk_forward_immediate_fs_uae() {
    hunk_sections::native_hunk_source(FORWARD_IMMEDIATE);
}

fn cross_section_source() -> String {
    source(false)
        .replace(".byte End-Start", ".byte End-CodeStart")
        .replace(
            ".section code,kind=code\n",
            ".section code,kind=code\nCodeStart\n",
        )
}

#[test]
fn compact_hunk_cross_section_offsets_rust_rejects() {
    let source = cross_section_source();
    let assembler = run_passes(&source.lines().collect::<Vec<_>>());
    let output = &assembler.root_metadata.linker_outputs[0];
    let error = build_linker_output_payload(output, assembler.sections())
        .expect_err("cross-section address differences must reject");
    assert!(error.message().contains("does not support this symbolic"));
}

fn cross_section_immediate() -> String {
    FORWARD_IMMEDIATE
        .replace("End-Start", "Data-Start")
        .replace(".byte 0", "Data .byte 0")
}

#[test]
fn compact_hunk_cross_section_immediate_rust_rejects() {
    let source = cross_section_immediate();
    let assembler = run_passes(&source.lines().collect::<Vec<_>>());
    let output = &assembler.root_metadata.linker_outputs[0];
    assert!(build_linker_output_payload(output, assembler.sections()).is_err());
}

#[test]
#[ignore = "requires configured FS-UAE; cross-section offsets must fail closed"]
fn compact_hunk_cross_section_offsets_fs_uae() {
    native_rejects(&cross_section_source());
    native_rejects(&cross_section_immediate());
}

fn native_rejects(source: &str) {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        None,
    )
    .expect("fresh native rejection of cross-section offset");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert!(runs[0].exit_code.is_some_and(|code| code != 0));
    assert!(
        runs[0]
            .stdout
            .contains("binary source: unsupported or invalid input"),
        "fresh native diagnostic required: {}",
        runs[0].stdout
    );
}
