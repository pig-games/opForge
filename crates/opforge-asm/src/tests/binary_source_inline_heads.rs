//! Shared head binding keeps labels and value operands out of package identities.
use super::*;
use crate::fs_uae_smoke::{
    compact_cli_input::assemble_cli,
    native_package_loading_performance::{linked_static_bytes, workload},
    opforge_self_host_package_digest, run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome,
    OpforgeNativeCliGuestFile, OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase,
    OpforgeNativeCliProof,
};
use crate::native_package_build::{build_native_packages, EmbedSelection};
use serde_json::{json, Value};
use vm::runtime_model_core::RuntimeModelCore;

struct Cleanup(PathBuf);
impl Drop for Cleanup {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn source(cpu: &str, style: &str) -> String {
    let label = |name: &str, instruction: &str| match style {
        "split" => format!("{name}\n {instruction}\n"),
        "bare" => format!("{name} {instruction}\n"),
        "colon" => format!("{name}: {instruction}\n"),
        _ => unreachable!(),
    };
    let body = if cpu == "6502" {
        format!(
            "{} sta destination\n bne end\ndestination .byte $42\n",
            label("end", "lda #nop")
        )
    } else {
        format!(
            "{} bra.w destination\n{}.byte $42\n",
            label("end", "moveq #nop,d0"),
            label("destination", "rts")
        )
    };
    format!(".module probe\n.cpu {cpu}\n.org $1000\nnop = 7\n{body}.endmodule\n")
}

fn oracle(dir: &Path, source: &str) -> Vec<u8> {
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
    let config = validate_cli(&cli).unwrap();
    run_with_validated_cli_with_context(&cli, &config)
        .unwrap_or_else(|error| panic!("inline-head Rust oracle: {error:?}"));
    fs::read(output).unwrap()
}

#[test]
fn compact_inline_heads_rust_oracles() {
    let dir = create_temp_dir("inline-head-oracles");
    for cpu in ["6502", "m68020"] {
        let expected = oracle(&dir, &source(cpu, "split"));
        assert!(!expected.is_empty());
        for style in ["bare", "colon"] {
            assert_eq!(oracle(&dir, &source(cpu, style)), expected, "{cpu}/{style}");
        }
    }
    fs::remove_dir_all(dir).unwrap();
}

fn register_label_source(colon: bool) -> String {
    let separator = if colon { ":" } else { "" };
    format!(".module probe\n.cpu m68020\nd0{separator} moveq #7,d1\n.byte $42\n.endmodule\n")
}

#[test]
fn compact_inline_register_label_rust_oracles() {
    let dir = create_temp_dir("inline-register-label-oracles");
    for colon in [false, true] {
        assert_eq!(oracle(&dir, &register_label_source(colon)), [0x72, 7, 0x42]);
    }
    fs::remove_dir_all(dir).unwrap();
}

#[test]
#[ignore = "requires FS-UAE; register-spelling labels before ordinary instruction heads"]
fn compact_inline_register_label_fs_uae() {
    let dir = create_temp_dir("inline-register-label-native");
    let _cleanup = Cleanup(dir.clone());
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = crate::binary_source_experiment::prepare_package(&core, &resolved).unwrap();
    for colon in [false, true] {
        let source = register_label_source(colon);
        let expected = oracle(&dir, &source);
        let result = crate::fs_uae_smoke::run_binary_source_harness_from_env(
            &workspace_root(),
            &package,
            &[("input.asm", source.as_bytes())],
            &expected,
        )
        .expect("fresh native register-label comparison");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
    }
}

fn save_report(path: &Path, image: &[u8], packages: &[(&str, Vec<u8>)], rows: &[Value]) {
    fs::write(
        path,
        serde_json::to_vec_pretty(&json!({
            "image_digest": opforge_self_host_package_digest(image),
            "image_bytes": image.len(),
            "linked_reserved_bytes": linked_static_bytes(image),
            "package_digests": packages.iter().map(|(name, bytes)|
                (*name, opforge_self_host_package_digest(bytes))).collect::<BTreeMap<_, _>>(),
            "package_sizes": packages.iter().map(|(name, bytes)| (*name, bytes.len())).collect::<BTreeMap<_, _>>(),
            "cases": rows,
        }))
        .unwrap(),
    )
    .unwrap();
}

#[test]
#[ignore = "fresh shared inline-head proof and same-input release timings; requires FS-UAE"]
fn compact_inline_heads_fs_uae() {
    let root = workspace_root();
    let dir = std::env::temp_dir().join(format!(
        "opforge-inline-heads-{}-{}",
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
    let report = std::env::var_os("OPFORGE_INLINE_HEAD_REPORT")
        .map(PathBuf::from)
        .expect("absolute OPFORGE_INLINE_HEAD_REPORT is required");
    assert!(report.is_absolute());
    let mut rows = Vec::new();
    // Persist the build identity before execution so baseline capture is explicit.
    save_report(&report, &image, &packages, &rows);
    let mut cases = Vec::new();
    for cpu in ["6502", "m68020"] {
        for style in ["split", "bare", "colon"] {
            cases.push((format!("{cpu}/{style}"), cpu, source(cpu, style)));
        }
    }
    for repeat in 0..2 {
        cases.push((format!("mixed-release/{repeat}"), "m68020", workload()));
    }
    // Allow a focused repeat of measurements without relaunching unrelated
    // native controls. This selects test cases, never production behavior.
    if let Ok(selection) = std::env::var("OPFORGE_INLINE_HEAD_CASES") {
        let names: Vec<_> = selection.split(',').map(str::trim).collect();
        for name in &names {
            assert!(
                cases.iter().any(|(case, _, _)| case == name),
                "unknown inline-head case: {name}"
            );
        }
        cases.retain(|(name, _, _)| names.contains(&name.as_str()));
    }
    for (index, (name, cpu, source)) in cases.iter().enumerate() {
        let expected = oracle(&dir.join(index.to_string()), source);
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
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/output.bin",
                rust_oracle: &expected,
            },
        };
        let result = run_prebuilt_compact_cli_case_from_env(&root, &case, &image);
        let mut row = json!({"name": name, "source_bytes": source.len(),
            "source_digest": opforge_self_host_package_digest(source.as_bytes()),
            "oracle_digest": opforge_self_host_package_digest(&expected),
            "output_bytes": expected.len(), "command": command, "success": false});
        match result {
            Ok(FsUaeSmokeOutcome::Completed { runs }) => {
                assert_eq!(runs.len(), 1);
                let run = &runs[0];
                row["success"] =
                    json!(run.success && run.protocol_completed && run.exit_code == Some(0));
                row["native_seconds"] = json!(run.start_to_done_host_seconds);
                row["exit_code"] = json!(run.exit_code);
            }
            Ok(FsUaeSmokeOutcome::Skipped(reason)) | Err(reason) => row["error"] = json!(reason),
        }
        eprintln!("INLINE_HEAD {row}");
        rows.push(row);
        save_report(&report, &image, &packages, &rows);
    }
    assert!(
        rows.iter().all(|row| row["success"] == true),
        "inspect {}",
        report.display()
    );
}
