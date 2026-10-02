// SPDX-License-Identifier: GPL-3.0-or-later
//! Fresh CLI record artifacts; flat data/adjacent sections only, not sparse .org parity.
use super::*;
use crate::native_package_build::{build_native_packages, EmbedSelection};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use types::image::ImageStore;

struct RecordCase {
    name: &'static str,
    cpu: &'static str,
    source: String,
    flag: &'static str,
    filename: Option<&'static str>,
    go: Option<&'static str>,
    path: &'static str,
    data_prefix: &'static str,
    source_bin: bool,
}

// Keep the fixture matrix in rows so option/address coverage is easy to compare.
#[rustfmt::skip]
fn cases() -> Vec<RecordCase> {
    // Exercise record coalescing across several emissions without relying on
    // the frontend's independent per-line token capacity.
    let bytes = (0..33).collect::<Vec<_>>().chunks(8)
        .map(|chunk| format!(".byte {}", chunk.iter()
            .map(|n| format!("${n:02x}")).collect::<Vec<_>>().join(",")))
        .collect::<Vec<_>>().join("\n");
    let mut result = Vec::new();
    for (name, cpu, origin, flag, filename, go, path, prefix) in [
        ("hex-16-split", "6502", "1234", "-x", Some("records"), None, "records.hex", ":20123400"),
        ("hex-boundary", "68020", "ffff", "--hex", Some("image.hex"), None, "image.hex", ":01FFFF00"),
        ("hex-32", "68020", "01000000", "--hex", None, None, "entry.hex", ":020000040100F9"),
        ("hex-go-16", "6502", "1234", "--hex", None, Some("1234"), "entry.hex", ":20123400"),
        ("hex-go-32", "68020", "1234", "-x", Some("launch"), Some("89abcdef"), "launch.hex", ":20123400"),
        ("srec-16-split", "6502", "1234", "-s", Some("records"), None, "records.srec", "S1231234"),
        ("srec-24-boundary", "68020", "ffff", "--srec", None, None, "entry.srec", "S22400FFFF"),
        ("srec-32", "68020", "01000000", "--srec", Some("image.srec"), None, "image.srec", "S32501000000"),
        ("srec-go-16", "6502", "1234", "--srec", None, Some("abcd"), "entry.srec", "S1231234"),
        ("srec-go-32", "68020", "1234", "-s", Some("launch"), Some("89ABCDEF"), "launch.srec", "S32500001234"),
    ] {
        result.push(RecordCase {
            name,
            cpu,
            source: format!(
                ".module main\n.cpu {cpu}\n.org ${origin}\n{bytes}\n.endmodule\n"
            ),
            flag,
            filename,
            go,
            path,
            data_prefix: prefix,
            source_bin: false,
        });
    }
    for (name, flag, path, prefix) in [
        ("hex-empty", "--hex", "entry.hex", ":00000001FF"),
        ("srec-empty", "--srec", "entry.srec", "S9030000FC"),
    ] {
        result.push(RecordCase {
            name,
            cpu: "68020",
            source: ".module main\n.cpu 68020\n.endmodule\n".into(),
            flag,
            filename: None,
            go: None,
            path,
            data_prefix: prefix,
            source_bin: false,
        });
    }
    result.push(RecordCase {
        name: "hex-adjacent-sections-source-bin", cpu: "68020", flag: "--hex", filename: None, go: None,
        path: "entry.hex", data_prefix: ":041000001122334442", source_bin: true,
        source: ".module main\n.cpu 68020\n.region first,$1000,$1001\n.region second,$1002,$10ff\n.section code,kind=code\n.byte $11,$22\n.endsection\n.section data,kind=data\n.byte $33,$44\n.endsection\n.place code in first\n.place data in second\n.output \"source-image\",format=bin,sections=code,data\n.endmodule\n".into(),
    });
    result
}

fn scratch() -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "opforge-compact-cli-records-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir(&dir).unwrap();
    dir
}

fn telemetry_defines() -> Vec<&'static str> {
    if std::env::var_os("OPFORGE_CLI_RECORD_TELEMETRY").is_some() {
        vec!["OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY"]
    } else {
        vec![]
    }
}

fn rust_oracle(base: &Path, case: &RecordCase) -> Vec<Vec<u8>> {
    let dir = base.join(case.name);
    fs::create_dir(&dir).unwrap();
    let input = dir.join("entry.asm");
    fs::write(&input, &case.source).unwrap();
    let mut argv = vec![
        "opForge".into(),
        input.to_string_lossy().into_owned(),
        "--cpu".into(),
        case.cpu.into(),
        case.flag.into(),
    ];
    if let Some(name) = case.filename {
        argv.push(dir.join(name).to_string_lossy().into_owned());
    }
    if let Some(go) = case.go {
        argv.extend(["--go".into(), go.into()]);
    }
    let cli = Cli::parse_from(argv);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("assemble fresh Rust record oracle");
    let bytes = fs::read(dir.join(case.path)).unwrap();
    let text = std::str::from_utf8(&bytes).unwrap();
    assert!(
        text.lines().any(|line| line.starts_with(case.data_prefix)),
        "{}: meaningful record/address coverage: {text}",
        case.name
    );
    assert!(text.ends_with('\n'));
    if let Some(go) = case.go {
        let start = u32::from_str_radix(go, 16).unwrap();
        let prefix = if case.path.ends_with("hex") {
            if start <= 0xffff {
                format!(":040000030000{start:04X}")
            } else {
                format!(":04000005{start:08X}")
            }
        } else if start <= 0xffff {
            format!("S903{start:04X}")
        } else {
            format!("S705{start:08X}")
        };
        assert!(
            text.lines().any(|line| line.starts_with(&prefix)),
            "{}: start address",
            case.name
        );
    }
    let mut outputs = vec![bytes];
    if case.source_bin {
        let bin = fs::read(dir.join("source-image")).unwrap();
        assert_eq!(bin, [0x11, 0x22, 0x33, 0x44]);
        outputs.push(bin);
    }
    let mut inventory: Vec<_> = fs::read_dir(&dir)
        .unwrap()
        .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
        .collect();
    inventory.sort();
    let mut expected = vec!["entry.asm", case.path];
    if case.source_bin {
        expected.push("source-image");
    }
    expected.sort();
    assert_eq!(
        inventory, expected,
        "{}: exact Rust artifact inventory",
        case.name
    );
    outputs
}

#[test]
fn compact_cli_records_live_rust_oracles() {
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    for case in cases() {
        rust_oracle(&dir, &case);
    }
    sparse_renderer_fixture();
    for go in ["123", "123456789", "0x1234", "xyzq"] {
        let cli = Cli::parse_from(["opForge", "entry.asm", "--hex", "--go", go]);
        assert!(validate_cli(&cli).is_err(), "reject invalid go {go}");
    }
}

#[test]
#[ignore = "fresh real-native compact CLI record proof; requires configured FS-UAE"]
fn compact_cli_hex_and_srec_records() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    let registry = engine::build_default_asm_registry();
    let build = build_native_packages(
        &registry,
        &dir.join("native"),
        &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::Targets(vec!["68020".into(), "6502".into()]),
    )
    .unwrap();
    let defines = telemetry_defines();
    let image = super::compact_cli_input::assemble_cli_with_defines(&root, &build, &defines);
    let cases = cases();
    let negatives = [
        ("reject-go-short", "--hex --go 123"),
        ("reject-go-long", "--srec --go 123456789"),
        ("reject-go-prefix", "--hex --go 0x1234"),
        ("reject-go-nonhex", "--srec --go xyzq"),
        ("reject-two-record-outputs", "--hex --srec"),
    ];
    let output_failures = [
        ("reject-hunk-record-conversion",
         ".module main\n.cpu 68020\n.section code,kind=code\n.byte $12\n.endsection\n.output \"source-hunk\",format=hunk,sections=code\n.endmodule\n",
         "--hex"),
        ("record-write-failure",
         ".module main\n.cpu 68020\n.byte $12\n.endmodule\n",
         "--hex Work:entry.asm/image.hex"),
    ];
    let selected = std::env::var("OPFORGE_CLI_RECORD_CASES").ok();
    if let Some(names) = &selected {
        for name in names.split(',') {
            assert!(
                cases.iter().any(|case| case.name == name)
                    || negatives.iter().any(|case| case.0 == name)
                    || output_failures.iter().any(|case| case.0 == name),
                "record selector must match exact case: {name}"
            );
        }
    }
    let wanted = |name: &str| {
        selected
            .as_ref()
            .is_none_or(|names| names.split(',').any(|item| item == name))
    };
    let mut failures = Vec::new();
    let mut attempted = 0;
    let mut run =
        |name: &str, source: &[u8], command: &str, proof: OpforgeNativeCliProof<'_>, exit| {
            attempted += 1;
            let files = [OpforgeNativeCliGuestFile {
                relative_path: "entry.asm",
                bytes: source,
            }];
            let case = OpforgeNativeCliParityCase {
                name,
                // Guest executable architecture; assembly target comes from the CLI/source.
                cpu_override: "68020",
                extra_assembly_defines: &defines,
                source_override: Some(source),
                command_template: Some(command),
                package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
                extra_guest_files: &files,
                proof: proof,
            };
            match run_prebuilt_compact_cli_case_from_env(&root, &case, &image) {
                Ok(FsUaeSmokeOutcome::Completed { runs })
                    if runs.len() == 1
                        && runs[0].protocol_completed
                        && runs[0].exit_code == Some(exit) =>
                {
                    eprintln!("{name}: fresh native completion, exit {exit}")
                }
                Ok(FsUaeSmokeOutcome::Completed { runs }) => failures.push(format!(
                    "{name}: invalid completion/exit; {} runs",
                    runs.len()
                )),
                Ok(FsUaeSmokeOutcome::Skipped(reason)) => {
                    failures.push(format!("{name}: skipped: {reason}"))
                }
                Err(error) => failures.push(format!("{name}: {error}")),
            }
        };
    for case in cases.iter().filter(|case| wanted(case.name)) {
        let oracle = std::panic::catch_unwind(|| rust_oracle(&dir, case));
        let Ok(oracle) = oracle else {
            // Oracle failures do not stop later native cases from obtaining fresh evidence.
            eprintln!("{}: live Rust oracle failed", case.name);
            continue;
        };
        let mut command = format!("--cpu {} Work:entry.asm {}", case.cpu, case.flag);
        if let Some(name) = case.filename {
            command.push_str(&format!(" Work:{name}"));
        }
        if let Some(go) = case.go {
            command.push_str(&format!(" -g {go}"));
        }
        let path = format!("Work/{}", case.path);
        let mut artifacts = vec![OpforgeNativeCliExpectedArtifact {
            relative_path: &path,
            rust_oracle: &oracle[0],
        }];
        if case.source_bin {
            artifacts.push(OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/source-image",
                rust_oracle: &oracle[1],
            });
        }
        run(
            case.name,
            case.source.as_bytes(),
            &command,
            OpforgeNativeCliProof::ExactArtifacts(&artifacts),
            0,
        );
    }
    for (name, flags) in negatives.iter().filter(|(name, _)| wanted(name)) {
        run(
            name,
            b".module main\n.cpu 68020\n.byte $12\n.endmodule\n",
            &format!("--cpu 68020 Work:entry.asm {flags}"),
            OpforgeNativeCliProof::ExpectedFailureContaining(
                "compact CLI: invalid or unsupported arguments",
            ),
            20,
        );
    }
    for (name, source, flags) in output_failures.iter().filter(|(name, _, _)| wanted(name)) {
        run(
            name,
            source.as_bytes(),
            &format!("--cpu 68020 Work:entry.asm {flags}"),
            OpforgeNativeCliProof::ExpectedFailureContaining("compact CLI: output failed"),
            20,
        );
    }
    drop(run);
    // Every selected positive must have produced a live oracle and a native attempt.
    let expected = cases.iter().filter(|case| wanted(case.name)).count()
        + negatives.iter().filter(|(name, _)| wanted(name)).count()
        + output_failures
            .iter()
            .filter(|(name, _, _)| wanted(name))
            .count();
    assert_eq!(
        attempted, expected,
        "some selected live Rust oracles failed"
    );
    assert!(attempted > 0);
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

// Deliberately separate from source-language parity: the renderer consumes these
// sparse address/data views directly, including unused bytes beyond the last span.
fn sparse_renderer_fixture() -> (String, Vec<Vec<u8>>) {
    let data: Vec<u8> = (0..2432).map(|n| ((n * 37 + 11) & 255) as u8).collect();
    let spans = [
        (0x1000, 0, 17),
        (0x1011, 96, 16),
        (0x2000, 128, 2100),
        (0xffff, 2300, 2),
        (0x0100_0000, 2350, 33),
    ];
    let mut image = ImageStore::new();
    for (address, offset, bytes) in spans {
        image.store_slice(address, &data[offset..offset + bytes]);
    }
    let mut hex = Vec::new();
    let mut srec = Vec::new();
    image.write_hex_file(&mut hex, Some("12345678")).unwrap();
    image.write_srec_file(&mut srec, Some("12345678")).unwrap();
    assert!(
        hex.len() > 4096 && srec.len() > 4096,
        "both transports require multiple bounded chunks"
    );
    let mut source = SPARSE_RENDERER_WRAPPER.to_string();
    source = source.replace(
        "OPEN_LIBRARY = -552",
        &format!(
            "HexOutputBytes = {}\nSrecOutputBytes = {}\nOPEN_LIBRARY = -552",
            hex.len(),
            srec.len()
        ),
    );
    for (address, offset, bytes) in spans {
        source.push_str(&format!("\t.long ${address:08x},{offset},{bytes}\n"));
    }
    source.push_str("SpansEnd\nPayload\n");
    for chunk in data.chunks(32) {
        source.push_str(&format!(
            "\t.byte {}\n",
            chunk
                .iter()
                .map(|value| format!("${value:02x}"))
                .collect::<Vec<_>>()
                .join(",")
        ));
    }
    source.push_str("PayloadEnd\n\t.endsection\n\t.section bss,kind=bss\n\t.align 4\nRender\t.res byte,rec.FRAME_BYTES\nTransport\t.res byte,io.FRAME_BYTES\nInvalidSpan\t.res byte,rec.SPAN_BYTES\nChunks\t.res long,1\nBuffer\t.res byte,4096\n\t.endsection\n\t.output \"renderer\",format=hunk,sections=entry,code,data,bss\n\t.endmodule\n");
    (source, vec![hex, srec])
}

const SPARSE_RENDERER_WRAPPER: &str = r#"
    .module main
    .cpu 68020
    .use experimental.amigaos.binary_record_output as rec
    .use experimental.amigaos.binary_output_io as io
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
    .section entry,kind=code
    .pub
; Shell entry: D0=exit status; preserves D2-D7/A2-A6.
start .block
    movem.l d2-d7/a2-a6,-(sp)
    moveq #20,d7
    movea.l 4.w,a6
    lea DosName,a1
    moveq #36,d0
    jsr OPEN_LIBRARY(a6)
    tst.l d0
    beq.w done
    movea.l d0,a6
    lea Render,a0
    move.l #Payload,rec.Frame.Data(a0)
    move.l #PayloadEnd-Payload,d0
    move.l d0,rec.Frame.DataBytes(a0)
    move.l #Spans,rec.Frame.Spans(a0)
    move.l #SpansEnd-Spans,d0
    move.l d0,rec.Frame.SpanBytes(a0)
    move.l #Buffer,rec.Frame.Buffer(a0)
    move.l #4096,rec.Frame.Capacity(a0)
    move.w #1,rec.Frame.StartSet(a0)
    move.l #$12345678,rec.Frame.Start(a0)
    lea Transport,a1
    move.l #generate,io.Frame.Generator(a1)
    move.l a0,io.Frame.Context(a1)
    move.w #rec.HEX,rec.Frame.Format(a0)
    move.l #HexName,io.Frame.Path(a1)
    jsr writeImage
    bne.w close
    lea Render,a0
    lea Transport,a1
    move.w #rec.SREC,rec.Frame.Format(a0)
    move.l #SrecName,io.Frame.Path(a1)
    jsr writeImage
    bne.w close
    jsr rejectInvalid
    bne.w close
    moveq #0,d7
close
    movea.l a6,a1
    movea.l 4.w,a6
    jsr CLOSE_LIBRARY(a6)
done
    move.l d7,d0
    movem.l (sp)+,d2-d7/a2-a6
    rts
    .bend ; start
    .endsection
    .section code,kind=code
    .priv
; A0=Render,A1=Transport,A6=DOS. D0/CCR=0 success, 1 failure.
writeImage .block
    jsr rec.begin
    bne.w bad
    clr.l Chunks
    lea Transport,a0
    jsr io.write
    bne.w bad
    cmpi.l #2,Chunks
    blo.w bad
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
    tst.l rec.RecordCount
    beq.w bad
    move.l #HexOutputBytes,d0
    lea Render,a0
    cmpi.w #rec.HEX,rec.Frame.Format(a0)
    beq.w measured
    move.l #SrecOutputBytes,d0
measured
    cmp.l rec.OutputBytes,d0
    bne.w bad
.endif
.endif
    moveq #0,d0
    rts
bad
    moveq #1,d0
    rts
    .bend ; writeImage
; Generator ABI matches rec.next, recording successful chunks only.
generate .block
    jsr rec.next
    tst.l d0
    bne.w done
    addq.l #1,Chunks
done
    tst.l d0
    rts
    .bend ; generate
; After successful output, verify begin disables invalid inputs.
rejectInvalid .block
    lea Render,a0
    move.l #127,rec.Frame.Capacity(a0)
    bsr.w rejected
    bne.w bad
    move.l #4096,rec.Frame.Capacity(a0)
    move.l #InvalidSpan,rec.Frame.Spans(a0)
    move.l #rec.SPAN_BYTES,rec.Frame.SpanBytes(a0)
    lea InvalidSpan,a1
    clr.l rec.Span.Bytes(a1)
    bsr.w rejected
    bne.w bad
    move.l #1,rec.Span.Bytes(a1)
    move.l #PayloadEnd-Payload,d0
    move.l d0,rec.Span.Offset(a1)
    bsr.w rejected
    bne.w bad
    move.l #Spans,rec.Frame.Spans(a0)
    move.l #SpansEnd-Spans,d0
    move.l d0,rec.Frame.SpanBytes(a0)
    lea Spans,a1
    move.l #$1000,rec.SPAN_BYTES+rec.Span.Address(a1)
    bsr.w rejected
    bne.w bad
    moveq #0,d0
    rts
bad
    moveq #1,d0
    rts
    .bend ; rejectInvalid
; A0=invalid Render. Require both rejection and invalid iterator state.
rejected .block
    jsr rec.begin
    cmpi.l #rec.STATUS_BAD,d0
    bne.w bad
    jsr rec.next
    cmpi.l #rec.INVALID,d0
    bne.w bad
    moveq #0,d0
    rts
bad
    moveq #1,d0
    rts
    .bend ; rejected
    .endsection
    .section data,kind=data
DosName .byte "dos.library",0
HexName .byte "Work:sparse.hex",0
SrecName .byte "Work:sparse.srec",0
    .align 4
Spans
"#;

#[test]
#[ignore = "fresh real-native sparse renderer/streaming transport proof; requires configured FS-UAE"]
fn compact_record_sparse_renderer_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    let (source, oracle) = sparse_renderer_fixture();
    let input = dir.join("renderer.asm");
    fs::write(&input, &source).unwrap();
    let mut argv = vec!["opForge".into(), input.to_string_lossy().into_owned()];
    let mut roots: Vec<_> = fs::read_dir(root.join("native"))
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.is_dir())
        .collect();
    roots.sort();
    for path in roots {
        argv.extend(["-M".into(), path.to_string_lossy().into_owned()]);
    }
    argv.extend([
        "-I".into(),
        root.join("native/motorola68000/amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ]);
    let defines = telemetry_defines();
    for define in &defines {
        argv.extend(["--define".into(), (*define).into()]);
    }
    let cli = Cli::parse_from(argv);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "assemble sparse renderer: {}; diagnostics: {:?}",
            error.summary(),
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("assemble sparse renderer: {error}")
        }
        cli_core::CliRunError::WarningsAsErrors { .. } => {
            panic!("assemble sparse renderer: warnings treated as errors")
        }
    });
    let executable = fs::read(dir.join("renderer")).unwrap();
    let artifacts = [
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/sparse.hex",
            rust_oracle: &oracle[0],
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/sparse.srec",
            rust_oracle: &oracle[1],
        },
    ];
    let case = OpforgeNativeCliParityCase {
        name: "sparse-renderer-streaming",
        cpu_override: "68020",
        extra_assembly_defines: &defines,
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifacts(&artifacts),
    };
    match run_prebuilt_compact_cli_case_from_env(&root, &case, &executable) {
        Ok(FsUaeSmokeOutcome::Completed { runs }) => assert!(
            runs.len() == 1 && runs[0].protocol_completed && runs[0].exit_code == Some(0),
            "sparse renderer requires fresh completion and explicit zero exit"
        ),
        Ok(FsUaeSmokeOutcome::Skipped(reason)) => panic!("sparse renderer skipped: {reason}"),
        Err(error) => panic!("sparse renderer: {error}"),
    }
}
