//! Fresh Rust/native proof for package-selected whole-file binary inclusion.
use super::*;
use crate::fs_uae_smoke::FsUaeSmokeOutcome;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use vm::runtime_model_core::RuntimeModelCore;

#[path = "binary_source_hunk.rs"]
mod hunk;

struct Scratch(PathBuf);
impl Scratch {
    fn new() -> Self {
        let path = create_temp_dir("binary-source-incbin");
        Self(path)
    }
}
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn oracle(
    cpu: &str,
    files: &[(&str, &[u8])],
    roots: &[&str],
    output: &str,
    binary: bool,
) -> Result<Vec<u8>, String> {
    let scratch = Scratch::new();
    for (name, bytes) in files {
        let path = scratch.0.join(name);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, bytes).unwrap();
    }
    fs::create_dir_all(scratch.0.join("build")).unwrap();
    let mut args = vec![
        "opForge".to_string(),
        scratch.0.join(files[0].0).to_string_lossy().into_owned(),
        "--cpu".into(),
        cpu.into(),
    ];
    if binary {
        args.extend([
            "--bin".into(),
            scratch.0.join(output).to_string_lossy().into_owned(),
        ]);
    }
    for root in roots {
        args.extend([
            "-I".into(),
            scratch.0.join(root).to_string_lossy().into_owned(),
        ]);
    }
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).map_err(|error| error.to_string())?;
    config.out_dir = Some(scratch.0.clone());
    run_with_validated_cli_with_context(&cli, &config).map_err(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => error.summary().to_string(),
        cli_core::CliRunError::Workflow { error, .. } => error.to_string(),
        cli_core::CliRunError::WarningsAsErrors { .. } => "warnings treated as errors".into(),
    })?;
    fs::read(scratch.0.join(output)).map_err(|error| error.to_string())
}

fn patterned() -> Vec<u8> {
    (0..4099)
        .map(|n| ((n * 73 + n / 256) & 255) as u8)
        .collect()
}
fn large_source(cpu: &str) -> String {
    format!(".cpu {cpu}\n.org 0\n.word End-Start\nStart .incbin \"payload.bin\"\nColon: .incbin \"tail.bin\"\nEmpty .incbin \"empty.bin\"\nEnd: .byte $ee\n.word Colon-Start,Empty-Colon\n.end\n")
}
fn large_files<'a>(source: &'a str, payload: &'a [u8]) -> Vec<(&'a str, &'a [u8])> {
    vec![
        ("project/main.asm", source.as_bytes()),
        ("project/payload.bin", payload),
        ("project/tail.bin", &[0, 255, 42]),
        ("project/empty.bin", &[]),
    ]
}
fn expected_large(cpu: &str, payload: &[u8]) -> Vec<u8> {
    let word = |value: u16| {
        if cpu == "m6502" {
            value.to_le_bytes()
        } else {
            value.to_be_bytes()
        }
    };
    let mut bytes = word((payload.len() + 3) as u16).to_vec();
    bytes.extend_from_slice(payload);
    bytes.extend([0, 255, 42, 0xee]);
    bytes.extend(word(payload.len() as u16));
    bytes.extend(word(3));
    bytes
}
const NESTED: &[(&str, &[u8])] = &[
    (
        "project/main.asm",
        b".org 0\n.include \"parts/part.i\"\n.byte $ee\n.end\n",
    ),
    ("project/parts/part.i", b"Nested: .incbin \"data.bin\"\n"),
    ("project/parts/data.bin", &[0, 128, 255]),
];
const ROOT_SEARCH: &[(&str, &[u8])] = &[
    ("project/main.asm", b".org 0\n.incbin \"data.bin\"\n.end\n"),
    ("assets/data.bin", &[17, 34, 51]),
];
const INACTIVE: &str = ".org 0\n.if 0\n.incbin \"absent-if.bin\"\n.endif\nUnused .macro\n.incbin \"absent-macro.bin\"\n.endmacro\n.byte $2a\n.end\n";
const MACRO_ASSET: &[(&str, &[u8])] = &[
    ("project/main.asm", b".org 0\n.include \"parts/macros.i\"\n.outer\n.inner\n.end\n"),
    ("project/parts/macros.i", b"inner .macro\n.incbin \"data.bin\"\n.incbin \"data.bin\"\n.byte 17\n.endmacro\nouter .macro\n.inner\n.if 0\n.incbin \"absent.bin\"\n.endif\n.endmacro\nunused .macro\n.incbin \"unused.bin\"\n.endmacro\n"),
    ("project/parts/data.bin", &[0, 128, 255]),
    ("project/data.bin", &[42]),
];
const MACRO_ARGUMENT: &[(&str, &[u8])] = &[
    (
        "main.asm",
        b".org 0\nemit .macro filename\n.incbin .filename\n.endmacro\n.emit \"data.bin\"\n.end\n",
    ),
    ("data.bin", &[0, 128, 255]),
];
const HUNK: &str = ".module binary_catalog_probe\n.cpu m68020\n.section code,kind=code\n rts\n.endsection\n.section data,kind=data\n.long Payload-Base,End-Payload\nBase\n.byte 0,0,0,0\nPayload: .incbin \"payload.bin\"\nEnd\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,data\n.endmodule\n";

#[test]
fn binary_incbin_rust_oracle() {
    let payload = patterned();
    assert_eq!(
        payload
            .iter()
            .copied()
            .collect::<std::collections::BTreeSet<_>>()
            .len(),
        256
    );
    for cpu in ["m6502", "m68020"] {
        let source = large_source(cpu);
        assert_eq!(
            oracle(
                cpu,
                &large_files(&source, &payload),
                &[],
                "output.bin",
                true
            )
            .unwrap(),
            expected_large(cpu, &payload)
        );
    }
}
#[test]
fn binary_incbin_paths_rust_oracle() {
    assert_eq!(
        oracle("m6502", NESTED, &[], "output.bin", true).unwrap(),
        [0, 128, 255, 238]
    );
    assert_eq!(
        oracle("m6502", ROOT_SEARCH, &["assets"], "output.bin", true).unwrap(),
        [17, 34, 51]
    );
    assert!(oracle("m6502", ROOT_SEARCH, &[], "output.bin", true).is_err());
}
#[test]
fn binary_incbin_inactive_rust_oracle() {
    assert_eq!(
        oracle(
            "m6502",
            &[("main.asm", INACTIVE.as_bytes())],
            &[],
            "output.bin",
            true
        )
        .unwrap(),
        [42]
    );
}
#[test]
fn binary_incbin_macro_origin_rust_oracle() {
    assert_eq!(
        oracle("m6502", MACRO_ARGUMENT, &[], "output.bin", true).unwrap(),
        [0, 128, 255]
    );
    assert_eq!(
        oracle("m6502", MACRO_ASSET, &[], "output.bin", true).unwrap(),
        [0, 128, 255, 0, 128, 255, 17, 0, 128, 255, 0, 128, 255, 17]
    );
}
#[test]
fn binary_incbin_hunk_rust_oracle() {
    let payload = patterned();
    let bytes = oracle(
        "m68020",
        &[("main.asm", HUNK.as_bytes()), ("payload.bin", &payload)],
        &[],
        "build/sections.hunk",
        false,
    )
    .unwrap();
    let segments = hunk::segments(&bytes).unwrap();
    assert_eq!(segments.len(), 2);
    assert_eq!(
        &segments[1].payload[..12],
        &[0, 0, 0, 4, 0, 0, 16, 3, 0, 0, 0, 0]
    );
    assert_eq!(&segments[1].payload[12..12 + payload.len()], payload);
    assert!(segments[1].relocations.is_empty());
}

fn native(name: &str, cpu: &str, files: &[(&str, &[u8])], roots: &[&str], expected: Option<&[u8]>) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
    let package = crate::binary_source_experiment::prepare_package(&core, &resolved).unwrap();
    let outcome = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &root,
        &package,
        files,
        &[],
        roots,
        expected,
        false,
    )
    .expect("fresh incbin native completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native run required")
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.protocol_completed, "{name}: missing fresh completion");
    assert_eq!(
        run.success,
        expected.is_some(),
        "{name}: {} {}",
        run.stdout,
        run.stderr
    );
    assert_eq!(
        run.exit_code,
        Some(if expected.is_some() { 0 } else { 20 }),
        "{name}"
    );
    if let Some(expected) = expected {
        assert_eq!(run.verified_output.as_deref(), Some(expected), "{name}");
    } else {
        assert!(
            run.stdout
                .contains("binary source: unsupported or invalid input"),
            "{name}: missing rejection diagnostic: {} {}",
            run.stdout,
            run.stderr
        );
    }
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let record = &run.captured_artifacts[&PathBuf::from("Work/memory.bin")];
        assert_eq!(record.len(), 2280, "current MEMD extent");
        let words: Vec<_> = record
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect();
        assert_eq!(words[0], 0x4d454d44);
        assert_eq!(words[1], 0, "terminal live allocations");
        assert_eq!(words[11], 0, "terminal cleanup allocations");
        assert_eq!(words[3], words[4], "owned allocation/free balance");
        assert_eq!(words[29], 0, "profiling errors");
        eprintln!("INCBIN_MEMORY case={name} peak_owned_bytes={} allocated_bytes={} freed_bytes={} live_bytes={}", words[2], words[3], words[4], words[1]);
    } else {
        assert!(!run
            .captured_artifacts
            .contains_key(&PathBuf::from("Work/memory.bin")));
    }
    let image = run
        .captured_artifacts
        .get(&PathBuf::from("Work/build/opforge_compact"))
        .expect("fresh native image");
    eprintln!("INCBIN case={name} cpu={cpu} source_bytes={} staged_bytes={} output_bytes={:?} start_to_done_host_seconds={:?} image_bytes={} linked_reserved_bytes={} fresh_completion=true", files[0].1.len(),files.iter().map(|(_,bytes)|bytes.len()).sum::<usize>(),expected.map(<[u8]>::len),run.start_to_done_host_seconds,image.len(),hunk::allocation(image).unwrap().total());
}

#[test]
#[ignore = "requires configured FS-UAE; fresh exact incbin output and explicit rejection"]
fn binary_incbin_fs_uae() {
    let payload = patterned();
    for cpu in ["m6502", "m68020"] {
        let source = large_source(cpu);
        let files = large_files(&source, &payload);
        let expected = oracle(cpu, &files, &[], "output.bin", true).unwrap();
        native(
            "patterned-labels-empty-endian",
            cpu,
            &files,
            &[],
            Some(&expected),
        );
    }
    for (name, files, roots) in [
        ("nested-relative", NESTED, &[][..]),
        ("configured-root", ROOT_SEARCH, &["assets"][..]),
        ("macro-definition-relative", MACRO_ASSET, &[][..]),
    ] {
        let expected = oracle("m6502", files, roots, "output.bin", true).unwrap();
        native(name, "m6502", files, roots, Some(&expected));
    }
    let files = [("main.asm", INACTIVE.as_bytes())];
    let expected = oracle("m6502", &files, &[], "output.bin", true).unwrap();
    native(
        "inactive-missing-files",
        "m6502",
        &files,
        &[],
        Some(&expected),
    );
    let files = [
        ("main.asm", HUNK.as_bytes()),
        ("payload.bin", payload.as_slice()),
    ];
    let expected = oracle("m68020", &files, &[], "build/sections.hunk", false).unwrap();
    native(
        "hunk-catalog-offsets",
        "m68020",
        &files,
        &[],
        Some(&expected),
    );
    native(
        "missing-file",
        "m6502",
        &[("main.asm", b".incbin \"missing.bin\"\n")],
        &[],
        None,
    );
    native(
        "forbidden-traversal",
        "m6502",
        &[
            ("project/main.asm", b".incbin \"../secret.bin\"\n"),
            ("secret.bin", &[42]),
        ],
        &[],
        None,
    );
    native("root-required", "m6502", ROOT_SEARCH, &[], None);
}

#[test]
#[ignore = "requires configured FS-UAE and OPFORGE_COMPARE_MEMORY=1; binary asset allocation lifecycle"]
fn binary_incbin_memory_fs_uae() {
    assert_eq!(std::env::var("OPFORGE_COMPARE_MEMORY").as_deref(), Ok("1"));
    let payload = patterned();
    let source = large_source("m68020");
    let expected = oracle(
        "m68020",
        &large_files(&source, &payload),
        &[],
        "output.bin",
        true,
    )
    .unwrap();
    native(
        "patterned-asset-memory",
        "m68020",
        &large_files(&source, &payload),
        &[],
        Some(&expected),
    );
}

#[test]
#[ignore = "requires configured FS-UAE; binary filename passed as macro argument"]
fn binary_incbin_argument_fs_uae() {
    let expected = oracle("m6502", MACRO_ARGUMENT, &[], "output.bin", true).unwrap();
    native(
        "macro-filename-argument",
        "m6502",
        MACRO_ARGUMENT,
        &[],
        Some(&expected),
    );
}
