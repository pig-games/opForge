//! Shared structure layouts lower to numeric constants, without output storage.
use super::*;

const LAYOUTS: &str = r#".module layouts
.cpu m68020
.pub
Span .struct
Start .long ?
End .long ?
File .long ?
.endstruct
IncludeFrame .struct
Handle .long ?
Buffer .long ?
Cursor .long ?
End .long ?
Line .long ?
Origin .long ?
Path .byte ?
.endstruct
.struct Odd
Tag .db ?
Value .dw ?
Pointer .long ?
.endstruct
.endmodule
.module app
.cpu m68020
.if 0
Ignored .struct
invalid .byte 99
.endif
.use layouts as shape
.byte shape.Span.Start,shape.Span.End,shape.Span.File,shape.Span
.byte shape.IncludeFrame.Path,shape.IncludeFrame
.byte shape.Odd.Tag,shape.Odd.Value,shape.Odd.Pointer,shape.Odd
frameBytes = shape.IncludeFrame.Path+256
.long frameBytes
.endmodule
"#;

const RESERVED: &str = r#".cpu m6502
count = 5
Buffer .struct
Tag .byte ?
Bytes .res count*3
Tail .word ?
.endstruct
.byte Buffer.Tag,Buffer.Bytes,Buffer.Tail,Buffer
.end
"#;

const MACRO: &str = r#".cpu m6502
emit .macro
Local .struct
End .long ?
Tag .byte ?
.endstruct
.byte Local.End,Local.Tag,Local
.endmacro
.emit
.emit
.end
"#;

fn package_layout_source() -> String {
    let package =
        include_str!("../../../../native/motorola68000/amigaos/experimental/binary_package.asm");
    format!("{package}\n.module app\n.cpu m68020\n.use experimental.amigaos.binary_package as pkg\n.long pkg.PARAMETER_BYTES,pkg.Context.High,pkg.Context.Package,pkg.Parameter.Low,pkg.Parameter.High\n.endmodule\n")
}

const LONG_IMPORTED_FIELD: &str = r#".module experimental.amigaos.binary_ordered_records
.cpu m68020
.pub
Frame .struct
OrderCount .long ?
SourceCount .long ?
SourceSpans .long ?
SourceSpanCount .long ?
.endstruct
.endmodule
.module app
.cpu m68020
.use experimental.amigaos.binary_ordered_records as ordered
.byte ordered.Frame.SourceSpanCount
.endmodule
"#;

#[test]
fn compact_struct_long_imported_field_rust_oracle() {
    assert_eq!(oracle(LONG_IMPORTED_FIELD, "68020").unwrap(), [12]);
}

#[test]
#[ignore = "requires configured FS-UAE; import binding beyond old 63-byte path cap"]
fn compact_struct_long_imported_field_fs_uae() {
    native(LONG_IMPORTED_FIELD, "m68020");
}

#[test]
fn compact_package_layout_rust_oracle() {
    assert_eq!(
        oracle(&package_layout_source(), "68020").unwrap(),
        [0, 0, 0, 12, 0, 0, 0, 16, 0, 0, 0, 20, 0, 0, 0, 4, 0, 0, 0, 8]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; real self-host package layouts and derived stride"]
fn compact_package_layout_fs_uae() {
    native(&package_layout_source(), "m68020");
}

#[test]
#[ignore = "requires configured FS-UAE; local qualified fields in module constants"]
fn compact_struct_local_field_constant_fs_uae() {
    native(".module layouts\n.cpu m68020\nParameter .struct\nId .word ?\nReserved .word ?\nLow .long ?\nHigh .long ?\n.endstruct\nstride=Parameter.High+4\n.long stride,Parameter.High\n.endmodule\n", "m68020");
}

fn scoped_field_source(shadow: bool) -> String {
    let local = if shadow {
        "Cell .struct\nPad .long ?\nHigh .word ?\n.endstruct\n"
    } else {
        ""
    };
    format!(".module app\n.cpu m68020\nCell .struct\nPad .byte ?\nHigh .word ?\n.endstruct\nroutine .block\n{local}.namespace inner\n.long Cell.High\n.endnamespace\n.bend\n.long routine,Cell.High\n.endmodule\n")
}

#[test]
fn compact_struct_scoped_field_rust_oracles() {
    for shadow in [false, true] {
        assert_eq!(
            oracle(&scoped_field_source(shadow), "68020").unwrap(),
            [0, 0, 0, if shadow { 4 } else { 1 }, 0, 0, 0, 0, 0, 0, 0, 1]
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; ancestor field lookup and local struct shadowing"]
fn compact_struct_scoped_field_fs_uae() {
    for shadow in [false, true] {
        native(&scoped_field_source(shadow), "m68020");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; field owner must be available at use"]
fn compact_struct_forward_field_rejection_fs_uae() {
    let source = scoped_field_source(true).replace(
        "routine .block\nCell .struct\nPad .long ?\nHigh .word ?\n.endstruct\n.namespace inner\n.long Cell.High\n.endnamespace\n",
        "routine .block\n.namespace inner\n.long Cell.High\n.endnamespace\nCell .struct\nPad .long ?\nHigh .word ?\n.endstruct\n",
    );
    assert!(oracle(&source, "68020").is_err());
    native_rejection_for_cpu(&source, "m68020");
}

fn oracle(source: &str, cpu: &str) -> Result<Vec<u8>, String> {
    let dir = create_temp_dir("compact-struct-layout-oracle");
    let input = dir.join("input.asm");
    let output = dir.join("output.bin");
    fs::write(&input, source).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        cpu.to_string(),
        "--bin".to_string(),
        output.to_string_lossy().into_owned(),
    ]);
    let config = validate_cli(&cli).unwrap();
    let result = run_with_validated_cli_with_context(&cli, &config)
        .map(|_| fs::read(output).unwrap())
        .map_err(|error| format!("{error:?}"));
    fs::remove_dir_all(dir).unwrap();
    result
}

#[test]
fn compact_struct_layout_rust_oracles() {
    assert_eq!(
        oracle(LAYOUTS, "68020").unwrap(),
        [0, 4, 8, 12, 24, 25, 0, 1, 3, 7, 0, 0, 1, 24]
    );
    assert_eq!(oracle(RESERVED, "6502").unwrap(), [0, 1, 16, 18]);
    assert_eq!(oracle(MACRO, "6502").unwrap(), [0, 4, 5, 0, 4, 5]);
    for body in [
        "A .struct\nB .struct\n.endstruct\n.endstruct\n",
        "A .struct\nf .byte ?\nf .word ?\n.endstruct\n",
        "A .struct\nf .byte ?\n",
        "A .struct\nf .byte 1\n.endstruct\n",
        "A .struct\nf .res long,1\n.endstruct\n",
    ] {
        assert!(oracle(&format!(".cpu m6502\n{body}"), "6502").is_err());
    }
}

fn native(source: &str, cpu: &str) {
    let expected = oracle(source, cpu).unwrap();
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        Some(&expected),
    )
    .expect("fresh native struct layout comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    let image = &runs[0].captured_artifacts[&PathBuf::from("Work/build/opforge_compact")];
    eprintln!("COMPACT_STRUCT_LAYOUT cpu={cpu} source_bytes={} output_bytes={} seconds={:?} image_bytes={} linked_reserved_bytes={}", source.len(), expected.len(), runs[0].start_to_done_host_seconds, image.len(), hunk::allocation(image).unwrap().total());
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let record = &runs[0].captured_artifacts[&PathBuf::from("Work/memory.bin")];
        let words = record
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(words[0], 0x4d454d38);
        assert_eq!(words[1], 0, "all owned storage must be released");
        assert_eq!(words[3], words[4], "allocation accounting must balance");
        eprintln!(
            "COMPACT_STRUCT_LAYOUT_INSTRUMENTED peak_owned_bytes={} profiling_errors={}",
            words[2], words[29]
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; complete imported layouts and field constants"]
fn compact_struct_layout_fs_uae() {
    native(LAYOUTS, "m68020");
}

#[test]
#[ignore = "requires configured FS-UAE; shared reservation field size"]
fn compact_struct_reserved_layout_fs_uae() {
    native(RESERVED, "m6502");
}

#[test]
#[ignore = "requires configured FS-UAE; qualified macro-local layout readiness boundary"]
fn compact_struct_macro_layout_readiness_fs_uae() {
    // Rust supports invocation-local qualified paths. Native currently retains
    // their definition-time qualification and must reject, not emit wrong bytes.
    assert_eq!(oracle(MACRO, "6502").unwrap(), [0, 4, 5, 0, 4, 5]);
    native_rejection(MACRO);
}

#[test]
#[ignore = "requires configured FS-UAE; explicit invalid structure rejection"]
fn compact_struct_unclosed_fs_uae() {
    let source = ".cpu m6502\nUnfinished .struct\nf .byte ?\n";
    assert!(oracle(source, "6502").is_err());
    let diagnostic = native_rejection(source);
    assert!(diagnostic.contains("[file 00000001, line 00000004]"));
    let path = diagnostic
        .lines()
        .find_map(|line| line.strip_prefix("source: "));
    assert!(path.is_some_and(|path| path.ends_with("/input.asm")));
}

fn native_rejection(source: &str) -> String {
    native_rejection_for_cpu(source, "m6502")
}

fn native_rejection_for_cpu(source: &str, cpu: &str) -> String {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        None,
    )
    .expect("fresh native structure rejection");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
    assert!(runs[0].stdout.contains("unsupported or invalid input"));
    runs[0].stdout.clone()
}
