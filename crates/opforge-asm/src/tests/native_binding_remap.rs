//! Direct native binding remap proofs for opaque strings and plan trailers.
use super::native_iterable_loops::scratch;
use super::*;

struct Probe {
    name: &'static str,
    stem: &'static str,
    output_stem: &'static str,
    artifact: &'static str,
    source_root_env: &'static str,
    defines: &'static [&'static str],
}

const STRING: Probe = Probe {
    name: "typed-string-remap-boundary",
    stem: "remap_typed_string",
    output_stem: "remap-typed-string",
    artifact: "Work/remap-typed-string.bin",
    source_root_env: "OPFORGE_REMAP_STRING_SOURCE_ROOT",
    defines: &[],
};
const PLAN: Probe = Probe {
    name: "plan-trailer-remap-boundary",
    stem: "remap_plan_trailers",
    output_stem: "remap-plan-trailers",
    artifact: "Work/remap-plan-trailers.bin",
    source_root_env: "OPFORGE_REMAP_PLAN_SOURCE_ROOT",
    defines: &[
        "OPFORGE_DEBUG_CONTRACTS",
        "OPFORGE_MEMORY_TELEMETRY",
        "OPFORGE_PREPARATION_PROGRESS",
    ],
};

const BLOCK: Probe = Probe {
    name: "block-index-current-flags",
    stem: "block_index_flags",
    output_stem: "block-index-flags",
    artifact: "Work/block-index-flags.bin",
    source_root_env: "OPFORGE_BLOCK_INDEX_SOURCE_ROOT",
    defines: PLAN.defines,
};

const REFERENCES: Probe = Probe {
    name: "block-reference-opaque-payload",
    stem: "block_reference_payload",
    output_stem: "block-reference-payload",
    artifact: "Work/block-reference-payload.bin",
    source_root_env: "OPFORGE_BLOCK_REFERENCE_SOURCE_ROOT",
    defines: &[],
};

const BINARY_BYTES: Probe = Probe {
    name: "opaque-binary-byte-boundaries",
    stem: "bytes",
    output_stem: "binary-bytes",
    artifact: "Work/binary-bytes.bin",
    source_root_env: "OPFORGE_BINARY_BYTES_SOURCE_ROOT",
    defines: &[],
};

fn component_image(dir: &Path, probe: &Probe) -> (Vec<u8>, String) {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let source = fs::read_to_string(root.join(format!(
        "native/motorola68000/amigaos/tests/assembler/{}.asm",
        probe.stem
    )))
    .unwrap();
    let entry = dir.join("entry.asm");
    fs::write(&entry, &source).unwrap();
    let source_root = std::env::var_os(probe.source_root_env)
        .map(PathBuf::from)
        .unwrap_or_else(|| root.join("native/motorola68000"));
    let mut args: Vec<String> = vec![
        "opForge".into(),
        entry.to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        source_root.to_string_lossy().into_owned(),
        "-I".into(),
        source_root.join("amigaos").to_string_lossy().into_owned(),
        "-I".into(),
        source_root
            .join("amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ];
    for define in probe.defines {
        args.extend(["-D".into(), (*define).into()]);
    }
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.into());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "{} harness: {error}; diagnostics: {:?}",
            probe.name,
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("{} harness: {error}", probe.name)
        }
        _ => panic!("{} harness warnings", probe.name),
    });
    (
        fs::read(dir.join(format!("{}.hunk", probe.output_stem))).unwrap(),
        source,
    )
}

fn host_assembles(probe: &Probe) {
    let dir = scratch();
    assert!(!component_image(&dir.0, probe).0.is_empty());
}

fn native_proof(probe: &Probe) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let (image, source) = component_image(&dir.0, probe);
    let expected = [0x5a];
    let case = OpforgeNativeCliParityCase {
        name: probe.name,
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: probe.artifact,
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .unwrap_or_else(|error| panic!("fresh {} proof: {error}", probe.name));
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
    if probe.name == BLOCK.name {
        let context: Vec<_> = runs[0]
            .stdout
            .lines()
            .filter(|line| line.starts_with("progress p=00000157 "))
            .collect();
        assert_eq!(context.len(), 1, "one failure-only index context");
        assert!(context[0].contains("f=00000012 l=00000000 r=00000000"));
        eprintln!("{}", runs[0].stdout);
    }
}

#[test]
fn native_typed_string_remap_host_assembles() {
    host_assembles(&STRING);
}

#[test]
#[ignore = "requires configured FS-UAE; actual remap rejection and ABI"]
fn native_typed_string_remap_fs_uae() {
    native_proof(&STRING);
}

#[test]
fn native_plan_trailer_remap_host_assembles() {
    host_assembles(&PLAN);
}

#[test]
#[ignore = "requires configured FS-UAE; actual remap rejection and ABI"]
fn native_plan_trailer_remap_fs_uae() {
    native_proof(&PLAN);
}

#[test]
fn native_block_index_flags_host_assembles() {
    host_assembles(&BLOCK);
}

#[test]
#[ignore = "requires configured FS-UAE; actual block topology and flag validation"]
fn native_block_index_flags_fs_uae() {
    native_proof(&BLOCK);
}

#[test]
fn native_block_reference_payload_host_assembles() {
    host_assembles(&REFERENCES);
}

#[test]
#[ignore = "requires configured FS-UAE; real symbol reachability excludes opaque payloads"]
fn native_block_reference_payload_fs_uae() {
    native_proof(&REFERENCES);
}

#[test]
fn native_binary_bytes_host_assembles() {
    host_assembles(&BINARY_BYTES);
}

#[test]
#[ignore = "requires configured FS-UAE; opaque binary bytes and malformed length controls"]
fn native_binary_bytes_fs_uae() {
    native_proof(&BINARY_BYTES);
}
