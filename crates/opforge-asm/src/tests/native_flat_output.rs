// SPDX-License-Identifier: GPL-3.0-or-later
//! Pure flat-output controls with explicitly specified byte streams.
use std::path::{Path, PathBuf};
fn fixture() -> (Vec<u8>, Vec<u8>) {
    type Row = (u32, u32, u32);
    struct Case {
        fault: u32,
        capacity: u32,
        rows: Vec<Row>,
        span_bytes: Option<u32>,
        load: u32,
        extent: u32,
        high: u32,
        output: Option<&'static [u8]>,
        init_only: bool,
        range_set: u32,
        range: (u32, u32),
        fill: u32,
    }
    let positive = |capacity, rows, load, extent, high, output, init_only| Case {
        fault: 0,
        capacity,
        rows,
        span_bytes: None,
        load,
        extent,
        high,
        output: Some(output),
        init_only,
        range_set: 0,
        range: (0, 0),
        fill: 0,
    };
    let negative = |fault, capacity, rows, span_bytes| Case {
        fault,
        capacity,
        rows,
        span_bytes,
        load: 0,
        extent: 0,
        high: 0,
        output: None,
        init_only: false,
        range_set: 0,
        range: (0, 0),
        fill: 0,
    };
    let sparse = vec![(0x1000, 0, 2), (0x1004, 4, 2)];
    let mut cases = vec![
        positive(3, sparse.clone(), 0x1000, 6, 0, &[1, 2, 0, 0, 5, 6], false),
        positive(1, sparse, 0x1000, 6, 0, &[1, 2, 0, 0, 5, 6], false),
        positive(
            3,
            vec![(0x1000, 0, 2), (0x1002, 4, 2)],
            0x1000,
            4,
            0,
            &[1, 2, 5, 6],
            false,
        ),
        positive(3, vec![], 0, 0, 0, &[], false),
        positive(
            1,
            vec![(u32::MAX - 1, 0, 2)],
            u32::MAX - 1,
            2,
            0,
            &[1, 2],
            false,
        ),
        positive(4, vec![(0, 0, 1), (u32::MAX, 1, 1)], 0, 0, 1, &[], true),
        negative(0, 4, vec![(0x1000, 0, 1)], Some(11)),
        negative(0, 4, vec![(0x1000, 0, 0)], None),
        negative(0, 4, vec![(0x1002, 0, 1), (0x1000, 1, 1)], None),
        negative(0, 4, vec![(0x1000, 0, 2), (0x1001, 2, 1)], None),
        negative(0, 4, vec![(0x1000, 7, 2)], None),
        negative(1, 4, vec![(0x1000, 0, 1)], None),
        negative(2, 4, vec![(0x1000, 0, 1)], None),
        negative(3, 4, vec![(0x1000, 0, 1)], None),
        negative(4, 4, vec![(0x1000, 0, 1)], None),
        negative(5, 4, vec![(0x1000, 0, 1)], None),
        negative(6, 4, vec![(0x1000, 0, 1)], None),
        negative(0, 0, vec![(0x1000, 0, 1)], None),
        negative(0, 4, vec![(u32::MAX, 0, 2)], None),
    ];
    cases[3].fault = 7; // Empty image also accepts NULL data/span views.
    let ranged = |mut case: Case, start, end, fill| {
        case.range_set = 1;
        case.range = (start, end);
        case.fill = fill;
        case
    };
    cases.extend([
        ranged(
            positive(3, vec![(0x1000, 0, 8)], 0x1002, 4, 0, &[3, 4, 5, 6], false),
            0x1002,
            0x1005,
            0xaa,
        ),
        ranged(
            positive(
                2,
                vec![(0x1000, 0, 2), (0x1004, 4, 2)],
                0x1001,
                4,
                0,
                &[2, 0xaa, 0xaa, 5],
                false,
            ),
            0x1001,
            0x1004,
            0xaa,
        ),
        ranged(
            positive(
                2,
                vec![(0x1000, 0, 2)],
                0x0ffe,
                3,
                0,
                &[0xaa, 0xaa, 1],
                false,
            ),
            0x0ffe,
            0x1000,
            0xaa,
        ),
        ranged(
            positive(2, vec![(0x1000, 0, 2)], 0x1006, 3, 0, &[0xaa; 3], false),
            0x1006,
            0x1008,
            0xaa,
        ),
        ranged(
            positive(
                3,
                vec![(0x1000, 0, 2), (0x1004, 4, 2)],
                0x0ffe,
                10,
                0,
                &[0xaa, 0xaa, 1, 2, 0xaa, 0xaa, 5, 6, 0xaa, 0xaa],
                false,
            ),
            0x0ffe,
            0x1007,
            0xaa,
        ),
        ranged(
            positive(1, vec![], 0x1000, 3, 0, &[0xff; 3], false),
            0x1000,
            0x1002,
            0xff,
        ),
        ranged(positive(1, vec![], 0, 0, 1, &[], true), 0, u32::MAX, 0),
        ranged(negative(0, 3, vec![], None), 0x1001, 0x1000, 0),
        ranged(negative(0, 3, vec![], None), 0x1000, 0x1001, 256),
    ]);
    let mut invalid_range_flag = negative(0, 3, vec![], None);
    invalid_range_flag.range_set = 2;
    cases.push(invalid_range_flag);
    let count = cases.len() as u32;
    let mut input = count.to_be_bytes().to_vec();
    let mut expected = count.to_be_bytes().to_vec();
    let word = |output: &mut Vec<u8>, value: u32| output.extend(value.to_be_bytes());
    word(&mut expected, 2); // next before init: INVALID, A0/D1 preserved.
    word(&mut expected, 0x11223344);
    expected.extend([0xa5; 64]);
    for case in cases {
        let mut rows = Vec::new();
        for (address, offset, bytes) in case.rows {
            for value in [address, offset, bytes] {
                word(&mut rows, value);
            }
        }
        let span_bytes = case.span_bytes.unwrap_or(rows.len() as u32);
        for value in [
            case.fault,
            case.capacity,
            span_bytes,
            8,
            u32::from(case.init_only),
            u32::from(case.output.is_none()),
            case.range_set,
            case.range.0,
            case.range.1,
            case.fill,
        ] {
            word(&mut input, value);
        }
        input.extend(1u8..=8);
        input.extend(&rows[..span_bytes as usize]);
        while !input.len().is_multiple_of(4) {
            input.push(0);
        }
        for value in [
            u32::from(case.output.is_none()),
            case.load,
            case.extent,
            case.high,
        ] {
            word(&mut expected, value);
        }
        if let Some(output) = case.output {
            if !case.init_only {
                for chunk in output.chunks(case.capacity as usize) {
                    word(&mut expected, 0);
                    word(&mut expected, chunk.len() as u32);
                    expected.extend(chunk);
                }
                word(&mut expected, 1);
                word(&mut expected, 0);
            }
        } else {
            word(&mut expected, 0x11223344); // Failure publishes no frame state.
            expected.extend([0xa5; 64]);
            expected.extend(1u8..=8); // Alias rejection preserves immutable inputs.
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
        "opforge-flat-output-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&path).unwrap();
    let scratch = Scratch(path);
    std::fs::write(scratch.0.join("flat-output-fixture.bin"), input).unwrap();
    let source = std::fs::read_to_string(
        repo.join("native/motorola68000/amigaos/tests/assembler/flat_output_harness.asm"),
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
            "native flat output: {error}; diagnostics: {:?}",
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => panic!("native flat output: {error}"),
        _ => panic!("native flat output warnings"),
    });
    let image = std::fs::read(scratch.0.join("flat-output.hunk")).unwrap();
    (scratch, image, source)
}
#[test]
fn native_flat_output_oracle_and_fixture_export() {
    let (input, expected) = fixture();
    assert!(!input.is_empty() && expected.len() > 32);
    let directory = root().join("target/native-flat-output");
    std::fs::create_dir_all(&directory).unwrap();
    std::fs::write(directory.join("flat-output-fixture.bin"), input).unwrap();
    std::fs::write(directory.join("flat-output-expected.bin"), expected).unwrap();
    eprintln!("FLAT_OUTPUT cases=29 caller-buffer=true u32-envelope=true explicit-ranges=true");
}
#[test]
fn native_flat_output_host_assembles() {
    let (input, _) = fixture();
    assert!(!assemble(&input).1.is_empty());
}
#[test]
#[ignore = "requires configured FS-UAE; fresh flat output generator proof"]
fn native_flat_output_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let (input, expected) = fixture();
    let (_scratch, image, source) = assemble(&input);
    let case = OpforgeNativeCliParityCase {
        name: "flat-output-generator",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/flat-output-results.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root(), &case, &image)
        .unwrap_or_else(|error| panic!("fresh flat output generator proof: {error}"));
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
    eprintln!(
        "FLAT_OUTPUT inputs={} exact=true generator-only=true seconds={:?}",
        u32::from_be_bytes(input[..4].try_into().unwrap()),
        runs[0].start_to_done_host_seconds
    );
}
