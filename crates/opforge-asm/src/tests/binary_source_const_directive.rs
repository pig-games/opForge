//! Shared scalar declarations reuse immutable assignment execution.
use super::*;
use crate::fs_uae_smoke::{
    compact_cli_input::assemble_cli, run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome,
    OpforgeNativeCliGuestFile, OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase,
    OpforgeNativeCliProof,
};
use crate::native_package_build::{build_native_packages, EmbedSelection};
use serde_json::json;

struct Cleanup(PathBuf);
impl Drop for Cleanup {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn source(cpu: &str, style: &str) -> String {
    let declaration = |name: &str, value: &str| match style {
        "equals" => format!("{name} = {value}\n"),
        "bare" => format!("{name} .const {value}\n"),
        "colon" => format!("{name}: .const {value}\n"),
        _ => unreachable!(),
    };
    let mut body = String::new();
    for (name, value) in [
        ("one", "later+1"),
        ("negative", "-17"),
        ("quotient", "negative/7"),
        ("location", "$"),
    ] {
        body.push_str(&declaration(name, value));
    }
    body.push_str("later = 7\nstart\n.byte one,negative+20\nend\n");
    body.push_str(&declaration("distance", "end-start"));
    body.push_str(".word distance,location\n.long quotient\n");
    for (flag, value, chosen) in [("off", "0", "pickedOff"), ("on", "1", "pickedOn")] {
        body.push_str(&declaration(flag, value));
        body.push_str(&format!(".if {flag}\n"));
        body.push_str(&declaration(chosen, "11"));
        body.push_str(".else\n");
        body.push_str(&declaration(chosen, "22"));
        body.push_str(".endif\n");
    }
    body.push_str(".byte pickedOff,pickedOn\nproduce .macro value\n");
    body.push_str(&declaration("captured", ".value+1"));
    body.push_str(".byte captured\n.endmacro\nfirst .produce 2\nsecond .produce 5\n");
    body.push_str(if cpu == "6502" {
        " lda #one\n"
    } else {
        " moveq #one,d0\n"
    });
    format!(".module probe\n.cpu {cpu}\n.org $1000\n{body}.endmodule\n")
}

fn oracle(dir: &Path, source: &str) -> Result<Vec<u8>, String> {
    fs::create_dir_all(dir).unwrap();
    let input = dir.join("main.asm");
    let output = dir.join("output.bin");
    fs::write(&input, source).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--bin".into(),
        output.to_string_lossy().into_owned(),
    ]);
    let config = validate_cli(&cli).map_err(|error| format!("{error:?}"))?;
    run_with_validated_cli_with_context(&cli, &config).map_err(|error| format!("{error:?}"))?;
    Ok(fs::read(output).unwrap())
}

fn invalid_sources() -> Vec<(&'static str, String)> {
    [
        ("duplicate", "value = 1\nvalue .const 2\n.byte value\n"),
        (
            "cycle",
            "left .const right+1\nright .const left+1\n.byte left\n",
        ),
        ("unlabelled", ".const 7\n.byte 1\n"),
        ("missing-value", "value .const\n.byte 1\n"),
    ]
    .map(|(name, body)| {
        (
            name,
            format!(".module probe\n.cpu m68020\n{body}.endmodule\n"),
        )
    })
    .into()
}

#[test]
fn compact_const_rust_oracles() {
    let dir = create_temp_dir("const-directive-oracles");
    let _cleanup = Cleanup(dir.clone());
    for cpu in ["6502", "m68020"] {
        let expected = if cpu == "6502" {
            vec![8, 3, 2, 0, 0, 16, 254, 255, 255, 255, 22, 11, 3, 6, 0xa9, 8]
        } else {
            vec![8, 3, 0, 2, 16, 0, 255, 255, 255, 254, 22, 11, 3, 6, 0x70, 8]
        };
        for style in ["equals", "bare", "colon"] {
            assert_eq!(
                oracle(&dir, &source(cpu, style)).unwrap(),
                expected,
                "{cpu}/{style}"
            );
        }
    }
    for (name, source) in invalid_sources() {
        assert!(oracle(&dir, &source).is_err(), "{name} must be rejected");
    }
}

#[test]
#[ignore = "requires fresh FS-UAE scalar .const parity and rejection controls"]
fn compact_const_fs_uae() {
    let root = workspace_root();
    let dir = std::env::temp_dir().join(format!(
        "opforge-const-{}-{}",
        process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir(&dir).unwrap();
    let _cleanup = Cleanup(dir.clone());
    let build = build_native_packages(
        &engine::build_default_asm_registry(),
        &dir.join("build"),
        &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::Targets(vec!["m68020".into()]),
    )
    .unwrap();
    let image = assemble_cli(&root, &build);
    let packages = ["m6502--transparent.bin", "m68020--motorola68k.bin"].map(|name| {
        (
            name,
            fs::read(build.output_dir.join("packages").join(name)).unwrap(),
        )
    });
    let report = std::env::var_os("OPFORGE_CONST_REPORT")
        .map(PathBuf::from)
        .expect("absolute OPFORGE_CONST_REPORT required");
    assert!(report.is_absolute());
    let mut cases = Vec::new();
    for cpu in ["6502", "m68020"] {
        for style in ["equals", "bare", "colon"] {
            cases.push((format!("{cpu}/{style}"), cpu, source(cpu, style), true));
        }
    }
    cases.extend(
        invalid_sources()
            .into_iter()
            .map(|(name, source)| (name.into(), "m68020", source, false)),
    );
    let mut rows = Vec::new();
    for (index, (name, cpu, source, valid)) in cases.iter().enumerate() {
        let expected = oracle(&dir.join(index.to_string()), source);
        assert_eq!(expected.is_ok(), *valid, "Rust oracle {name}: {expected:?}");
        let expected = expected.unwrap_or_default();
        let mut guest = vec![("main.asm".to_string(), source.as_bytes().to_vec())];
        guest.extend(
            packages
                .iter()
                .map(|(name, bytes)| (format!("packages/{name}"), bytes.clone())),
        );
        let files = guest
            .iter()
            .map(|(name, bytes)| OpforgeNativeCliGuestFile {
                relative_path: name,
                bytes,
            })
            .collect::<Vec<_>>();
        let command = format!("--cpu {cpu} Work:main.asm -P Work:packages --bin Work:output.bin");
        let case = OpforgeNativeCliParityCase {
            name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(source.as_bytes()),
            command_template: Some(&command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &files,
            proof: if *valid {
                OpforgeNativeCliProof::ExactArtifact {
                    relative_path: "Work/output.bin",
                    rust_oracle: &expected,
                }
            } else {
                OpforgeNativeCliProof::ExpectedFailureContaining(
                    "binary source: unsupported or invalid input",
                )
            },
        };
        let mut row = json!({"name": name, "expected_success": valid, "success": false});
        match run_prebuilt_compact_cli_case_from_env(&root, &case, &image) {
            Ok(FsUaeSmokeOutcome::Completed { runs }) => {
                assert_eq!(runs.len(), 1);
                let run = &runs[0];
                row["success"] = json!(
                    run.protocol_completed
                        && if *valid {
                            run.success && run.exit_code == Some(0)
                        } else {
                            run.exit_code.is_some_and(|code| code != 0)
                        }
                );
                row["native_seconds"] = json!(run.start_to_done_host_seconds);
                row["exit_code"] = json!(run.exit_code);
            }
            Ok(FsUaeSmokeOutcome::Skipped(reason)) | Err(reason) => row["error"] = json!(reason),
        }
        eprintln!("CONST_PARITY {row}");
        rows.push(row);
        fs::write(&report, serde_json::to_vec_pretty(&rows).unwrap()).unwrap();
    }
    assert!(
        rows.iter().all(|row| row["success"] == true),
        "inspect {}",
        report.display()
    );
}
