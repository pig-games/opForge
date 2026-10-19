//! Original source identities survive actual package-owned instruction fixups.
use super::native_iterable_loops::{oracle, scratch};
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use vm::binary_source_package::BinarySourcePackage;
use vm::runtime_model_core::RuntimeModelCore;

const SOURCE: &str = r#" .module probe
 .cpu 68020
 .org $1000
first:
 .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
second:
 .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 move.l first,first
 move.l first,second
 move.l #first+4,d0
 move.l #7,d0
 .endmodule
"#;

fn prepare(dir: &Path) -> Vec<u8> {
    let core = RuntimeModelCore::from_registry(&engine::build_default_asm_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let package =
        crate::binary_source_experiment::prepare_package_with_names(&core, &resolved).unwrap();
    let name = |text: &str| {
        package
            .names
            .iter()
            .position(|entry| entry.eq_ignore_ascii_case(text))
            .unwrap() as u16
    };
    let first = package.names.len() as u16;
    let second = first + 1;
    let qualifier = numeric
        .qualifiers
        .iter()
        .position(|entry| entry.eq_ignore_ascii_case("l"))
        .unwrap()
        + 1;
    let symbol = |id: u16| vec![0x81, 4, 0x12, id as u8, (id >> 8) as u8, 0];
    let register = |id: u16| vec![0, (id >> 8) as u8, id as u8, 0];
    let mut repeated = symbol(first);
    repeated.push(4);
    repeated.extend(symbol(first));
    let mut distinct = symbol(first);
    distinct.push(4);
    distinct.extend(symbol(second));
    let mut addend = vec![
        8,
        0x81,
        7,
        0x12,
        first as u8,
        (first >> 8) as u8,
        0x13,
        4,
        0x31,
        0,
    ];
    addend.push(4);
    addend.extend(register(name("d0")));
    let mut absolute = vec![8, 0x81, 3, 0x13, 7, 0, 4];
    absolute.extend(register(name("d0")));
    let mut fixture = format!(
        "NAME_COUNT={}\nSYMBOL_COUNT={}\nFIRST_ID={}\nSECOND_ID={}\nMOVE_ID={}\nLONG_QUALIFIER={}\n",
        first, second + 1, first, second, name("move"), qualifier
    );
    for (index, operands) in [repeated, distinct, addend, absolute].iter().enumerate() {
        fixture.push_str(&format!(
            "Operands{index} .byte {}\nOperands{index}End\n",
            operands
                .iter()
                .map(u8::to_string)
                .collect::<Vec<_>>()
                .join(",")
        ));
    }
    fs::write(dir.join("instruction_fixup_identity_fixture.i"), fixture).unwrap();
    fs::write(
        dir.join("instruction_fixup_identity_package.bin"),
        package.bytes,
    )
    .unwrap();

    let bytes = oracle(dir, SOURCE).expect("live Rust MOVE instruction oracle");
    assert_eq!(bytes.len(), 64);
    let mut expected = Vec::new();
    let mut cursor = 32;
    for (length, fields) in [
        (10, vec![(2, first, 0x1000), (6, first, 0x1000)]),
        (10, vec![(2, first, 0x1000), (6, second, 0x1010)]),
        (6, vec![(2, first, 0x1004)]),
        (6, vec![]),
    ] {
        expected.extend_from_slice(&(length as u32).to_be_bytes());
        expected.extend_from_slice(&(fields.len() as u32).to_be_bytes());
        expected.extend_from_slice(&bytes[cursor..cursor + length]);
        cursor += length;
        for (offset, identity, value) in fields {
            for word in [offset, 4, 1, value, identity as u32] {
                expected.extend_from_slice(&word.to_be_bytes());
            }
        }
    }
    expected
}

fn build_probe(root: &Path, dir: &Path) -> Vec<u8> {
    let input = dir.join("instruction_fixup_identity_probe.asm");
    fs::copy(root.join("native/motorola68000/amigaos/test-harnesses/experimental/instruction_fixup_identity_probe.asm"), &input).unwrap();
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
    if let Err(error) = run_with_validated_cli_with_context(&cli, &config) {
        match error {
            cli_core::CliRunError::Assembler { error, .. } => {
                panic!("native identity probe: {}", error.summary());
            }
            error => panic!("native identity probe: {error:?}"),
        }
    }
    fs::read(dir.join("instruction-fixup-identity.hunk")).unwrap()
}

#[test]
fn native_instruction_fixup_identity_host_assembles() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    assert!(!prepare(&dir.0).is_empty());
    assert!(!build_probe(&root, &dir.0).is_empty());
}

#[test]
#[ignore = "requires FS-UAE; actual package encoding and original per-field identities"]
fn native_instruction_fixup_identity_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let expected = prepare(&dir.0);
    let image = build_probe(&root, &dir.0);
    let case = OpforgeNativeCliParityCase {
        name: "instruction-fixup-source-identities",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(SOURCE.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/output.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh package encoding identity execution");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed && runs[0].success);
    assert_eq!(runs[0].exit_code, Some(0));
    assert_eq!(
        runs[0].verified_output.as_deref(),
        Some(expected.as_slice())
    );
}
