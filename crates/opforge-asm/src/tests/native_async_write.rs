//! Packet protocol proof uses mock DOS and real Exec; the final write uses real DOS.
//! This component does not establish full native CLI parity or performance.
use super::native_iterable_loops::scratch;
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

fn payload() -> Vec<u8> {
    (0..65_536u32)
        .map(|index| index.wrapping_mul(37).wrapping_add(11) as u8)
        .collect()
}

fn build_probe(root: &Path, dir: &Path, instrumented: bool) -> Vec<u8> {
    fs::write(dir.join("module_async_payload.bin"), payload()).unwrap();
    let input = dir.join("module_async_write_probe.asm");
    fs::copy(
        root.join(
            "native/motorola68000/amigaos/test-harnesses/experimental/module_async_write_probe.asm",
        ),
        &input,
    )
    .unwrap();
    let native = root.join("native/motorola68000/amigaos");
    let mut args = vec![
        "opforge".to_owned(),
        input.to_string_lossy().into_owned(),
        "-M".to_owned(),
        native.to_string_lossy().into_owned(),
        "-I".to_owned(),
        native.join("debug").to_string_lossy().into_owned(),
    ];
    if instrumented {
        for define in ["OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY"] {
            args.extend(["--define".to_owned(), define.to_owned()]);
        }
    }
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    fs::read(dir.join("module-async-write-probe.hunk")).unwrap()
}

#[test]
fn native_async_write_host_assembles() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    for instrumented in [false, true] {
        assert!(!build_probe(&root, &dir.0, instrumented).is_empty());
    }
}

#[test]
#[ignore = "requires FS-UAE; mock DOS protocol and real DOS 64KiB write component"]
fn native_async_write_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let expected = payload();
    for instrumented in [false, true] {
        let image = build_probe(&root, &dir.0, instrumented);
        let case = OpforgeNativeCliParityCase {
            name: "native-async-write-component",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(b""),
            command_template: Some(""),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/output.bin",
                rust_oracle: &expected,
            },
        };
        let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
            .expect("fresh native asynchronous write component");
        let FsUaeSmokeOutcome::Completed { runs } = outcome else {
            panic!("real native execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].protocol_completed && runs[0].success);
        assert_eq!(runs[0].exit_code, Some(0));
        assert_eq!(
            runs[0].verified_output.as_deref(),
            Some(expected.as_slice())
        );
        assert!(!runs[0].artifact_dir.exists(), "ephemeral run cleanup");
    }
}
