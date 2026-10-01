//! Built-in shared .emit parity; macros named emit retain their precedence.
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

fn oracle(cpu: &str, source: &str, hunk: bool) -> Result<Vec<u8>, String> {
    let dir = create_temp_dir("compact-emit-oracle");
    let result = (|| {
        let input = dir.join("main.asm");
        fs::write(&input, source).unwrap();
        fs::create_dir_all(dir.join("build")).unwrap();
        let mut args = vec![
            "opForge".to_string(),
            input.to_string_lossy().into_owned(),
            "--cpu".into(),
            cpu.into(),
        ];
        if !hunk {
            args.extend([
                "--bin".into(),
                dir.join("output.bin").to_string_lossy().into_owned(),
            ]);
        }
        let cli = Cli::parse_from(args);
        let mut config = validate_cli(&cli).map_err(|e| e.to_string())?;
        config.out_dir = Some(dir.clone());
        run_with_validated_cli_with_context(&cli, &config).map_err(|e| format!("{e:?}"))?;
        fs::read(dir.join(if hunk {
            "build/sections.hunk"
        } else {
            "output.bin"
        }))
        .map_err(|e| e.to_string())
    })();
    fs::remove_dir_all(dir).unwrap();
    result
}
const DATA: &str = ".org 0\nSize=3\nStart .emit byte,0,255\nColon: .emit word,$1234\n.emit long,$12345678,-1\n.emit 3,$abcdef\n.emit Size,End-Start\n.emit (2+3),$12345678\n.emit 8,$12345678\nEnd .byte 42\n.end\n";
const MACRO_SIMPLE: &str =
    ".org 0\npack .macro value\n.emit byte,.value\n.endmacro\n.pack 17\n.end\n";
const COLLISION: &str = ".org 0\nemit .macro value\n.byte .value\n.endmacro\n.emit 99\n.end\n";
const MACRO: &str = ".org 0\npack .macro value\n.emit byte,.value\n.endmacro\n.for 0\n.pack 17\n.endfor\n.for 2\n.pack 17\n.endfor\nemit .macro value\n.byte .value\n.endmacro\n.emit 99\n.end\n";
const HUNK: &str = ".module emit_probe\n.cpu m68020\n.section code,kind=code\n.emit long,Payload+3,End-Payload\n.endsection\n.section data,kind=data\nPayload .emit byte,17,34\nEnd\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,data\n.endmodule\n";
const FAILURES: &[&str] = &[
    ".emit byte,256",
    ".emit word,65536",
    ".emit 3,$1000000",
    ".emit byte,-1",
    ".emit 0,1",
    ".emit -1,1",
    ".emit byte",
    ".emit byte,",
    ".emit byte,,1",
    ".emit byte,1,",
    ".emit byte,\"abc\"",
    ".emit Later,1\nLater=2",
    "Size=Later\n.emit Size,1\nLater=2",
    ".module bss_emit\n.section bss,kind=bss\n.emit byte,1\n.endsection\n.endmodule",
];
#[test]
fn binary_emit_rust_oracles() {
    let be = oracle("m68020", DATA, false).unwrap();
    let le = oracle("m6502", DATA, false).unwrap();
    assert_eq!(be.len(), 32);
    assert_eq!(
        &be[..15],
        &[0, 255, 0x12, 0x34, 0x12, 0x34, 0x56, 0x78, 255, 255, 255, 255, 0xab, 0xcd, 0xef]
    );
    assert_eq!(
        &le[..15],
        &[0, 255, 0x34, 0x12, 0x78, 0x56, 0x34, 0x12, 255, 255, 255, 255, 0xef, 0xcd, 0xab]
    );
    assert_eq!(oracle("m68020", MACRO_SIMPLE, false).unwrap(), [17]);
    assert_eq!(oracle("m68020", COLLISION, false).unwrap(), [99]);
    assert_eq!(oracle("m68020", MACRO, false).unwrap(), [17, 17, 99]);
    let image = oracle("m68020", HUNK, true).unwrap();
    let segments = hunk::segments(&image).unwrap();
    assert_eq!(segments[0].payload, [0, 0, 0, 3, 0, 0, 0, 2]);
    assert_eq!(segments[0].relocations, [(0, 1)]);
    assert_eq!(segments[1].payload, [17, 34, 0, 0]);
    for source in FAILURES {
        assert!(oracle("m68020", source, false).is_err(), "{source}");
    }
}
fn native(cpu: &str, source: &str, hunk: bool, valid: bool) {
    let expected = oracle(cpu, source, hunk);
    assert_eq!(expected.is_ok(), valid, "{source}: {expected:?}");
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let outcome = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        &[("main.asm", source.as_bytes())],
        &[],
        &[],
        expected.as_deref().ok(),
        false,
    )
    .expect("fresh .emit native completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native run required")
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.protocol_completed);
    assert_eq!(
        run.exit_code,
        Some(if valid { 0 } else { 20 }),
        "{source}: {} {}",
        run.stdout,
        run.stderr
    );
    assert_eq!(
        run.success, valid,
        "{source}: {} {}",
        run.stdout, run.stderr
    );
    if !valid {
        assert!(
            run.stdout.contains("unsupported or invalid input"),
            "missing native rejection diagnostic: {} {}",
            run.stdout,
            run.stderr
        );
    }
    if let Ok(expected) = expected {
        assert_eq!(run.verified_output.as_deref(), Some(expected.as_slice()));
    }
    let image = &run.captured_artifacts[&PathBuf::from("Work/build/opforge_compact")];
    eprintln!("EMIT cpu={cpu} valid={valid} source_bytes={} seconds={:?} image_bytes={} linked_reserved_bytes={}", source.len(),run.start_to_done_host_seconds,image.len(),hunk::allocation(image).unwrap().total());
}
#[test]
#[ignore = "requires configured FS-UAE; shared built-in data and macro precedence"]
fn binary_emit_fs_uae() {
    for cpu in ["m68020", "m6502"] {
        native(cpu, DATA, false, true);
    }
    native("m68020", MACRO, false, true);
    native(
        "m68020",
        ".byte 0,0\nSize\n.emit Size,7\n.emit $,7\n.end\n",
        false,
        true,
    );
    native("m68020", HUNK, true, true);
}
#[test]
#[ignore = "requires configured FS-UAE; overflow, grammar, unresolved width and BSS rejection"]
fn binary_emit_errors_fs_uae() {
    for source in FAILURES {
        native("m68020", source, false, false);
    }
}

#[test]
fn binary_emit_layout_unit_rust_oracle() {
    for source in [
        ".byte 0,0\nSize\n.emit Size,7\n.end\n",
        ".byte 0,0\n.emit $,7\n.end\n",
    ] {
        assert_eq!(oracle("m68020", source, false).unwrap(), [0, 0, 0, 7]);
    }
}

fn hunk_failure(data: &str) -> String {
    HUNK.replace(".emit long,Payload+3,End-Payload", data)
}
#[test]
fn binary_emit_hunk_rejection_oracles() {
    for data in [
        ".emit byte,Payload",
        ".emit 8,Payload",
        ".emit long,Payload*2",
        "Unit=4\n.emit Unit,1",
    ] {
        assert!(
            oracle("m68020", &hunk_failure(data), true).is_err(),
            "{data}"
        );
    }
}
#[test]
#[ignore = "requires configured FS-UAE; complete data relocations and explicit unsupported unit/arithmetic rejection"]
fn binary_emit_hunk_fs_uae() {
    native("m68020", HUNK, true, true);
    for data in [
        ".emit byte,Payload",
        ".emit 8,Payload",
        ".emit long,Payload*2",
        "Unit=4\n.emit Unit,1",
    ] {
        native("m68020", &hunk_failure(data), true, false);
    }
}
