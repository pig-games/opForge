// SPDX-License-Identifier: GPL-3.0-or-later
//! Numeric initialized interval overlay controls against an independent byte image.
use std::path::{Path, PathBuf};
fn fixture() -> (Vec<u8>, Vec<u8>) {
    use std::collections::BTreeMap;
    let directory = super::native_iterable_loops::scratch();
    assert_eq!(
        super::native_iterable_loops::oracle(
            &directory.0,
            ".cpu 6502\n.org $1000\n.byte 1,2,3\n.org $1001\n.byte 9\n.end\n"
        )
        .unwrap(),
        [1, 9, 3]
    );

    // Last-write offsets are the numeric counterpart of Rust's addressed image.
    // These component controls include u32 envelopes beyond any fixture CPU.
    let cases = [
        (0x1000u32, 0u32, 4u32, 0u32),
        (0x1004, 4, 2, 0),
        (0x1002, 6, 2, 0),
        (0x1001, 8, 4, 0),
        (0x0ff0, 12, 2, 0),
        (0x0ff2, 14, 2, 0),
        (0x0ff1, 16, 0, 0),
        (0x0ff1, 16, 4, 0),
        (0x0ff5, 20, 1, 0),
        (0x1000, 21, 6, 0),
        (u32::MAX, 27, 1, 0),
        (u32::MAX, 28, 1, 0),
        (u32::MAX, 29, 2, 0),
        (0x2000, u32::MAX, 1, 0),
        (0x2000, 30, 1, 0),
        (0x0fff, 31, 1, 0),
        (0x0fff, 20, 1, 0),
        (0x0ff0, 12, 1, 0),
        (0x2001, 32, 1, 1),
    ];
    let mut input = Vec::new();
    let mut expected = Vec::new();
    let mut image = BTreeMap::new();
    input.extend((cases.len() as u32).to_be_bytes());
    for (address, offset, count, fault) in cases {
        for n in [address, offset, count, fault] {
            input.extend(n.to_be_bytes());
        }
        let valid = fault == 0
            && (count == 0
                || (address.checked_add(count - 1).is_some()
                    && offset.checked_add(count).is_some()));
        if valid {
            for i in 0..count {
                image.insert(address + i, offset + i);
            }
        }
        let mut rows: Vec<(u32, u32, u32)> = Vec::new();
        for (&address, &offset) in &image {
            if let Some((start, payload, count)) = rows.last_mut() {
                if start.checked_add(*count) == Some(address)
                    && payload.checked_add(*count) == Some(offset)
                {
                    *count += 1;
                    continue;
                }
            }
            rows.push((address, offset, 1));
        }
        expected.extend((u32::from(!valid)).to_be_bytes());
        expected.extend(((rows.len() * 12) as u32).to_be_bytes());
        for (a, o, n) in rows {
            for v in [a, o, n] {
                expected.extend(v.to_be_bytes());
            }
        }
    }
    (input, expected)
}

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}
struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}
fn assemble(input: &[u8]) -> (Scratch, Vec<u8>, String) {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let repo = root();
    let path = std::env::temp_dir().join(format!(
        "opforge-output-spans-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&path).unwrap();
    let scratch = Scratch(path);
    std::fs::write(scratch.0.join("output-spans-fixture.bin"), input).unwrap();
    let source = std::fs::read_to_string(
        repo.join("native/motorola68000/amigaos/tests/assembler/output_spans_harness.asm"),
    )
    .unwrap();
    let entry = scratch.0.join("entry.asm");
    std::fs::write(&entry, &source).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_owned(),
        entry.to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        repo.join("native/motorola68000")
            .to_string_lossy()
            .into_owned(),
        "-I".into(),
        repo.join("native/motorola68000/amigaos")
            .to_string_lossy()
            .into_owned(),
        "-I".into(),
        repo.join("native/motorola68000/amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(scratch.0.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "native output spans: {error}; diagnostics: {:?}",
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => panic!("native output spans: {error}"),
        _ => panic!("native output spans warnings"),
    });
    let image = std::fs::read(scratch.0.join("output-spans.hunk")).unwrap();
    (scratch, image, source)
}
#[test]
fn native_output_spans_oracle_and_fixture_export() {
    let (input, expected) = fixture();
    assert!(!input.is_empty() && expected.len() > 32);
    let directory = root().join("target/native-output-spans");
    std::fs::create_dir_all(&directory).unwrap();
    std::fs::write(directory.join("output-spans-fixture.bin"), input).unwrap();
    std::fs::write(directory.join("output-spans-expected.bin"), expected).unwrap();
    eprintln!("OUTPUT_SPANS cases=19 last-write-wins=true u32-envelope=true");
}
#[test]
fn native_output_spans_host_assembles() {
    let (input, _) = fixture();
    assert!(!assemble(&input).1.is_empty());
}
#[test]
#[ignore = "requires configured FS-UAE; fresh numeric span overlay proof"]
fn native_output_spans_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let (input, expected) = fixture();
    let (_scratch, image, source) = assemble(&input);
    let case = OpforgeNativeCliParityCase {
        name: "output-spans-numeric-overlay",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/output-spans-results.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root(), &case, &image)
        .unwrap_or_else(|error| panic!("fresh numeric span overlay proof: {error}"));
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
    eprintln!(
        "OUTPUT_SPANS inputs={} exact=true numeric-overlay-only=true seconds={:?}",
        u32::from_be_bytes(input[..4].try_into().unwrap()),
        runs[0].start_to_done_host_seconds
    );
}
