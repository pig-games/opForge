//! Native saved-image ownership and relocation boundary, not CLI activation.
//! Payload and linked bytes come from live Rust assembly of the same instructions.
use super::native_iterable_loops::{oracle, scratch};
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

const PAYLOAD: &str = ".module payload\n.cpu 68020\n moveq #7,d0\n.long 0\n rts\n moveq #9,d1\n rts\n.byte $99\n.endmodule\n";
const LINKED: &str = ".module linked\n.cpu 68020\n moveq #7,d0\n.long $1000+4\n rts\n moveq #7,d0\n.long $2000+4\n rts\n moveq #9,d1\n rts\n.endmodule\n";

fn prepare(dir: &Path) -> Vec<u8> {
    let payload = oracle(dir, PAYLOAD).expect("live fixed-fragment payload");
    assert_eq!(payload.len(), 13);
    fs::write(dir.join("module_image_payload.bin"), payload).unwrap();
    fs::write(
        dir.join("module_image_payload.i"),
        "PAYLOAD_BYTES=13\nFIRST_BYTES=8\nSECOND_BYTES=4\n",
    )
    .unwrap();
    let expected = oracle(dir, LINKED).expect("live relocated-fragment output");
    assert_eq!(expected.len(), 20);
    let mut expected = expected;
    let registry = oracle(dir, ".module linked\n.cpu 68020\n moveq #7,d0\n.long $1004\n rts\n moveq #7,d0\n.long $3004\n rts\n moveq #7,d0\n.long $1004\n rts\n.endmodule\n").expect("live alternating registry output");
    expected.extend(registry);
    assert_eq!(expected.len(), 44);
    expected
}

fn build_probe(root: &Path, dir: &Path, disk: bool) -> Vec<u8> {
    fs::write(
        dir.join("module_registry_mode.i"),
        format!("DISK_REGISTRY={}\n", u8::from(disk)),
    )
    .unwrap();
    let input = dir.join("module_image_probe.asm");
    fs::copy(
        root.join(
            "native/motorola68000/amigaos/test-harnesses/experimental/module_image_probe.asm",
        ),
        &input,
    )
    .unwrap();
    fs::copy(
        root.join(
            "native/motorola68000/amigaos/test-harnesses/experimental/module_data_delta_checks.i",
        ),
        dir.join("module_data_delta_checks.i"),
    )
    .unwrap();
    fs::copy(
        root.join(
            "native/motorola68000/amigaos/test-harnesses/experimental/module_runtime_reloc_checks.i",
        ),
        dir.join("module_runtime_reloc_checks.i"),
    )
    .unwrap();
    let native = root.join("native/motorola68000/amigaos");
    let cli = Cli::parse_from([
        "opforge".to_owned(),
        input.to_string_lossy().into_owned(),
        "-M".to_owned(),
        native.to_string_lossy().into_owned(),
        "-I".to_owned(),
        native.join("debug").to_string_lossy().into_owned(),
        // Existing registry counters prove metadata lookup does not read payload.
        "--define".to_owned(),
        "OPFORGE_DEBUG_CONTRACTS".to_owned(),
        "--define".to_owned(),
        "OPFORGE_MEMORY_TELEMETRY".to_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    fs::read(dir.join("module-image-probe.hunk")).unwrap()
}

#[test]
fn native_module_images_host_assembles() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    prepare(&dir.0);
    for disk in [false, true] {
        let image = build_probe(&root, &dir.0, disk);
        assert!(!image.is_empty());
    }
}

#[test]
#[ignore = "requires FS-UAE and OPFORGE_FS_UAE_GUEST_STACK_BYTES=65536; native image ownership and relocation component"]
fn native_module_images_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let expected = prepare(&dir.0);
    for disk in [false, true] {
        let image = build_probe(&root, &dir.0, disk);
        let case = OpforgeNativeCliParityCase {
            name: "native-module-image-component",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(PAYLOAD.as_bytes()),
            command_template: Some(""),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/output.bin",
                rust_oracle: &expected,
            },
        };
        let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
            .expect("fresh native module-image component");
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
        let objects = runs[0]
            .captured_artifacts
            .iter()
            .filter(|(path, _)| path.extension().is_some_and(|suffix| suffix == "ofm"))
            .collect::<Vec<_>>();
        assert!(runs[0]
            .captured_artifacts
            .keys()
            .all(|path| path.extension().is_none_or(|suffix| suffix != "tmp")));
        if disk {
            // The runtime fixture preserves provider/consumer objects; the
            // registry rollback removes B and retains the original A object.
            assert_eq!(objects.len(), 3);
            let names = objects
                .iter()
                .map(|(path, _)| path.file_name().unwrap().to_string_lossy().into_owned())
                .collect::<std::collections::BTreeSet<_>>();
            assert_eq!(names, std::collections::BTreeSet::from([
                "binary_frontend.exper-0000.ofm".to_owned(),
                "consumer-0001.ofm".to_owned(),
                "provider-0000.ofm".to_owned(),
            ]));
            for (path, bytes) in &objects {
                assert_eq!(&bytes[..4], b"OFMI");
                let declared_bytes = u32::from_be_bytes(bytes[4..8].try_into().unwrap());
                assert_eq!(declared_bytes as usize, bytes.len());
                if path.file_name().unwrap() == "binary_frontend.exper-0000.ofm" {
                    let payload = fs::read(dir.0.join("module_image_payload.bin")).unwrap();
                    assert!(bytes.ends_with(&payload), "complete unchanged Rust payload");
                } else {
                    let interface = u32::from_be_bytes(bytes[28..32].try_into().unwrap()) as usize;
                    assert_eq!(&bytes[interface..interface + 4], b"OFMA");
                }
            }
        } else {
            assert!(objects.is_empty());
        }
        eprintln!(
            "MODULE_IMAGE_COMPONENT {}",
            serde_json::json!({
                "disk_registry": disk,
                "persisted_objects": objects.len(),
                "scope": "supplied-byte and stored-interface relocation component; CLI lowering not covered",
                "native_image_digest": runs[0].native_image_digest,
                "fresh_protocol": runs[0].protocol_completed,
                "exit_code": runs[0].exit_code,
                "exact_rust_bytes": expected.len(),
                "full_self_host": false,
                "full_build_memory_saving": null,
                "full_build_runtime_saving": null,
            })
        );
    }
}
