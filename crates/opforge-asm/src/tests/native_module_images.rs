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
    expected
}

fn build_probe(root: &Path, dir: &Path) -> Vec<u8> {
    let input = dir.join("module_image_probe.asm");
    fs::copy(
        root.join(
            "native/motorola68000/amigaos/test-harnesses/experimental/module_image_probe.asm",
        ),
        &input,
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
    let image = build_probe(&root, &dir.0);
    assert!(!image.is_empty());
}

#[test]
#[ignore = "requires FS-UAE; native first-request image ownership and relocation component"]
fn native_module_images_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let expected = prepare(&dir.0);
    let image = build_probe(&root, &dir.0);
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
    eprintln!(
        "MODULE_IMAGE_COMPONENT {}",
        serde_json::json!({
            "scope": "supplied-byte component; CLI module lowering not integrated",
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
