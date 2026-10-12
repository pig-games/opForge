//! Captured ordinary macro lines retain CCR syntax and grow their plan arena.
use super::native_iterable_loops::{build_image, oracle, scratch};
use super::native_source_cases::{self, Case};
use super::*;

const CCR_LINE: &str = "\tmove.w ccr,-(sp)\n";

fn source(lines: usize, invoked: bool) -> String {
    let mut source = String::from(".module main\n.cpu 68020\nSAVE .macro\n");
    for _ in 0..lines {
        source.push_str(CCR_LINE);
    }
    source.push_str(".endmacro\n");
    if invoked {
        source.push_str(".SAVE\n");
    }
    source.push_str(".byte $5a\n.endmodule\n");
    source
}

fn cases() -> Vec<Case> {
    [
        ("ccr-unused", 1, false),
        ("ccr-invoked", 1, true),
        // Captured headers, rows and spelling alone exceed 1 MiB at 24,000
        // lines; 12,000 supplies a smaller control with the same syntax.
        ("plans-4000-lines", 4_000, false),
        ("plans-12000-lines", 12_000, false),
        ("plans-24000-lines", 24_000, false),
    ]
    .into_iter()
    .map(|(name, lines, invoked)| Case {
        name: name.into(),
        cpu: "68020".into(),
        files: vec![("entry.asm", source(lines, invoked))],
        expected: Some(if invoked {
            vec![0x42, 0xe7, 0x5a]
        } else {
            vec![0x5a]
        }),
    })
    .collect()
}

#[test]
fn native_macro_plan_limits_live_rust() {
    let dir = scratch();
    for case in cases() {
        let actual = oracle(&dir.0, &case.files[0].1)
            .unwrap_or_else(|error| panic!("{}: {error}", case.name));
        assert_eq!(Some(actual), case.expected, "{}", case.name);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; captured CCR syntax and plan arena growth"]
fn native_macro_plan_limits_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_MACRO_PLAN_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter = std::env::var("OPFORGE_MACRO_PLAN_CASE").unwrap_or_default();
    let cases = cases()
        .into_iter()
        .filter(|case| filter.is_empty() || filter.split(',').any(|name| name.trim() == case.name))
        .collect::<Vec<_>>();
    assert!(!cases.is_empty(), "no macro plan cases selected");
    native_source_cases::run_source_language_batch(&root, &image, &cases, 1);
}

fn overflow_image(dir: &Path) -> (Vec<u8>, String) {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let source =
        fs::read_to_string(root.join(
            "native/motorola68000/amigaos/test-harnesses/experimental/macro_plan_overflow.asm",
        ))
        .unwrap();
    let entry = dir.join("entry.asm");
    fs::write(&entry, &source).unwrap();
    let cli = Cli::parse_from([
        "opForge".into(),
        entry.to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        root.join("native/motorola68000")
            .to_string_lossy()
            .into_owned(),
        "-I".into(),
        root.join("native/motorola68000/amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.into());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "macro plan overflow harness: {error}; diagnostics: {:?}",
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("macro plan overflow harness: {error}")
        }
        _ => panic!("macro plan overflow harness warnings"),
    });
    (
        fs::read(dir.join("macro-plan-overflow.hunk")).unwrap(),
        source,
    )
}

#[test]
fn native_macro_plan_overflow_host_assembles() {
    let dir = scratch();
    let image = overflow_image(&dir.0).0;
    assert!(!image.is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; actual macro plan overflow rejection and ABI"]
fn native_macro_plan_overflow_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let (image, source) = overflow_image(&dir.0);
    let expected = [0x5a];
    let case = OpforgeNativeCliParityCase {
        name: "macro-plan-preallocation-overflow",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/macro-plan-overflow.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .unwrap_or_else(|error| panic!("fresh macro plan overflow proof: {error}"));
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
}
