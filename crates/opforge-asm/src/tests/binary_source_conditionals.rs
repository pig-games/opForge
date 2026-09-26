//! Conditional selection is resolved before packed assembly.
use super::*;

const CONTROLS: &str = r#".cpu m6502
assigned = 0
.ifdef assigned
 .byte 99
.else
 .byte 1
.endif
.ifndef absent
 .ifdef another_absent
  .byte 99
 .else
  .byte 2
 .endif
.endif
.ifdef absent
 .byte 99
.elseif assigned
 .byte 99
.else
 .byte 3
.endif
.if 0
 .byte 99
.elseif 2+3
 .byte 4
.elseif 1
 .byte 99
.else
 .byte 99
.endif
.if 0
 .if 1
  .byte 99
 .else
  .byte 99
 .endif
.else
 .byte 5
.endif
emit .macro value
.ifdef absent
 .byte 99
.else
 .byte .value
.endif
.endmacro
.emit 6
.end
"#;

const TELEMETRY: &str =
    include_str!("../../../../native/motorola68000/amigaos/debug/memory_telemetry.i");
const INCLUDE_ENTRY: &str = ".cpu m68020\n.include \"memory_telemetry.i\"\n.MEMORY_PHASE 0\n.MEMORY_STAGE 2\n.MEMORY_LAYOUT 1,2,3\n.TOKEN_WORK 1,2\n.byte $5a\n.end\n";

fn file_oracle(source: &str, cpu: &str) -> Vec<u8> {
    let dir = create_temp_dir("compact-conditional-telemetry-oracle");
    let input = dir.join("input.asm");
    let output = dir.join("output.bin");
    fs::write(&input, source).unwrap();
    fs::write(dir.join("memory_telemetry.i"), TELEMETRY).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        cpu.to_string(),
        "--bin".to_string(),
        output.to_string_lossy().into_owned(),
    ]);
    let config = validate_cli(&cli).unwrap();
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    let oracle = fs::read(output).unwrap();
    fs::remove_dir_all(dir).unwrap();
    oracle
}

fn telemetry_oracle() -> Vec<u8> {
    let oracle = file_oracle(INCLUDE_ENTRY, "68020");
    assert_eq!(oracle, [0x5a]);
    oracle
}

#[test]
fn compact_conditional_family_rust_oracles() {
    assert_eq!(file_oracle(CONTROLS, "6502"), [1, 2, 3, 4, 5, 6]);
    telemetry_oracle();
}

#[test]
#[ignore = "requires configured FS-UAE; conditional-family packed selection"]
fn compact_conditional_family_fs_uae() {
    let oracle = file_oracle(CONTROLS, "6502");
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        CONTROLS.as_bytes(),
        Some(&oracle),
    )
    .expect("fresh native conditional-family comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    let image = &runs[0].captured_artifacts[&PathBuf::from("Work/build/opforge_compact")];
    eprintln!("COMPACT_CONDITIONAL_FAMILY seconds={:?} image_bytes={} linked_reserved_bytes={} package_bytes={}",
        runs[0].start_to_done_host_seconds, image.len(), hunk::allocation(image).unwrap().total(), package.len());
}

#[test]
#[ignore = "requires configured FS-UAE; unchanged include still exceeds template registry"]
fn compact_telemetry_include_readiness_fs_uae() {
    telemetry_oracle();
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        &[
            ("input.asm", INCLUDE_ENTRY.as_bytes()),
            ("debug/memory_telemetry.i", TELEMETRY.as_bytes()),
        ],
        &[],
        &["debug"],
        None,
        false,
    )
    .expect("fresh native unchanged telemetry include rejection");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
    assert!(runs[0]
        .stdout
        .contains("source: Work:sources/debug/memory_telemetry.i"));
    assert!(
        runs[0].stdout.contains("line 00000072"),
        "{:#?}",
        runs[0].stdout
    );
}
