//! Typed lists execute from packed programs; Rust supplies each live oracle.
use super::*;
use crate::binary_source_experiment::prepare_package;
use crate::native_package_build::{build_native_packages, EmbedSelection};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use vm::runtime_model_core::RuntimeModelCore;

struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
fn scratch() -> Scratch {
    let path = std::env::temp_dir().join(format!(
        "opforge-lists-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&path).unwrap();
    Scratch(path)
}
fn source(cpu: &str) -> String {
    let wide = (0..25).map(|n| n.to_string()).collect::<Vec<_>>().join(",");
    format!(
        r#".module main
.cpu {cpu}
values = {{2,-3,65535,(4+5)}}
empty = {{}}
alias = values
.byte .len(values),.len(alias),.len(empty),.len({{9,8,7}})
.word values[0],-values[1],values[2],values[1+2],alias[2],({{5,6}})[1]
changing .var {{1,2}}
snapshot = changing
changing .set {{7,8,9}}
.byte snapshot[0],.len(snapshot),changing[0],.len(changing)
changing .set 11
.byte changing
len = 17
.byte len
wide = {{{wide}}}
.byte .len(wide),wide[24],.len({{values[0],alias[1]}})+1
nop
forward = {{tail}}
forwardAlias = forward
.byte .len(forward),.len(forwardAlias)
.word forwardAlias[0]
tail .byte 77
.long values[1],{{65537}}[0]
.endmodule
"#
    )
}
fn oracle(dir: &Path, source: &str) -> Result<Vec<u8>, String> {
    let input = dir.join("entry.asm");
    let output = dir.join("out.bin");
    fs::write(&input, source).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_owned(),
        input.to_string_lossy().into_owned(),
        "--bin".to_owned(),
        output.to_string_lossy().into_owned(),
    ]);
    let config = validate_cli(&cli).map_err(|error| format!("{error:?}"))?;
    run_with_validated_cli_with_context(&cli, &config).map_err(|error| format!("{error:?}"))?;
    fs::read(output).map_err(|error| error.to_string())
}
const BAD: &[&str] = &[
    "values={1,{2}}\n.byte .len(values)\n",
    "values={1,2}\n.byte values[-1]\n",
    "values={}\n.byte values[0]\n",
    ".byte .len(7)\n",
    ".byte .len()\n",
    ".byte .unknown({1,2})\n",
    "values={1,2}\n.byte values+1\n",
];
#[test]
fn native_lists_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        let bytes = oracle(&dir.0, &source(cpu)).unwrap();
        assert_eq!(&bytes[..4], &[4, 4, 0, 3]);
        assert_eq!(&bytes[16..25], &[1, 2, 7, 3, 11, 17, 25, 24, 3]);
    }
    for body in BAD {
        assert!(
            oracle(&dir.0, &format!(".cpu 68020\n{body}")).is_err(),
            "Rust accepted {body}"
        );
    }
}
#[test]
fn native_lists_host_assembles() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let build = build_native_packages(
        &engine::build_default_asm_registry(),
        &dir.0.join("native"),
        &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::Targets(vec!["68020".into(), "6502".into()]),
    )
    .unwrap();
    assert!(!super::compact_cli_input::assemble_cli(&root, &build).is_empty());
}
#[test]
#[ignore = "fresh packed list CLI positives and failures; requires configured FS-UAE"]
fn native_lists_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = if let Some(path) = std::env::var_os("OPFORGE_LIST_IMAGE") {
        fs::read(path).unwrap()
    } else {
        let build = build_native_packages(
            &engine::build_default_asm_registry(),
            &dir.0.join("native"),
            &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
            &EmbedSelection::Targets(vec!["68020".into(), "6502".into()]),
        )
        .unwrap();
        super::compact_cli_input::assemble_cli(&root, &build)
    };
    if let Some(path) = std::env::var_os("OPFORGE_LIST_SAVE_IMAGE") {
        fs::write(path, &image).unwrap();
    }
    if let Some(path) = std::env::var_os("OPFORGE_LIST_SOURCE") {
        run_case(
            &root,
            &dir.0,
            &image,
            &fs::read_to_string(path).unwrap(),
            false,
        );
        return;
    }
    let mut failures = 0;
    let mut check = |source: &str, failure| {
        if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            run_case(&root, &dir.0, &image, source, failure);
        }))
        .is_err()
        {
            failures += 1;
        }
    };
    if std::env::var("OPFORGE_LIST_CASES").as_deref() != Ok("negative") {
        for cpu in ["68020", "6502"] {
            check(&source(cpu), false);
        }
    }
    for body in BAD {
        check(&format!(".cpu 68020\n{body}"), true);
    }
    assert_eq!(failures, 0, "list cases failed; all cases were attempted");
}

// One real CLI case for bounded localization; an optional source file changes
// both guest input and the live Rust oracle, never production behavior.
#[test]
#[ignore = "single list CLI localization/control; requires configured FS-UAE"]
fn native_lists_probe_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let source = std::env::var_os("OPFORGE_LIST_SOURCE")
        .map(|path| fs::read_to_string(path).unwrap())
        .unwrap_or_else(|| ".cpu 68020\nvalues={2,3}\n.byte .len(values),values[0]\n".into());
    let expected = oracle(&dir.0, &source).unwrap();
    if let Some(path) = std::env::var_os("OPFORGE_LIST_IMAGE") {
        run_case(&root, &dir.0, &fs::read(path).unwrap(), &source, false);
        return;
    }
    let core = RuntimeModelCore::from_registry(&engine::build_default_asm_registry()).unwrap();
    let resolved = core.resolve_pipeline("68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let outcome = run_compact_cli_from_env(&root, &package, source.as_bytes(), Some(&expected))
        .expect("fresh list CLI probe");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let memory = &runs[0].captured_artifacts[&PathBuf::from("Work/memory.bin")];
        assert_eq!(memory.len(), 2280, "current MEMD layout");
        let words = memory
            .chunks_exact(4)
            .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(words[0], 0x4d454d44);
        assert_eq!(words[1], 0, "all owned blocks released");
        assert_eq!(words[3], words[4], "allocated capacity was freed");
        assert_eq!(words[29], 0, "no profiling errors");
        eprintln!("LIST_MEMORY peak_owned_bytes={}", words[2]);
    }
    eprintln!(
        "LIST_PROBE seconds={:?}",
        runs[0].start_to_done_host_seconds
    );
}
fn run_case(root: &Path, dir: &Path, image: &[u8], source: &str, failure: bool) {
    let expected = oracle(dir, source);
    assert_eq!(expected.is_err(), failure);
    let files = [OpforgeNativeCliGuestFile {
        relative_path: "entry.asm",
        bytes: source.as_bytes(),
    }];
    let case = OpforgeNativeCliParityCase {
        name: "packed-list-values",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some("Work:entry.asm --bin Work:out.bin"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: match &expected {
            Ok(bytes) => OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/out.bin",
                rust_oracle: bytes,
            },
            Err(_) => OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        },
    };
    let result = run_prebuilt_compact_cli_case_from_env(root, &case, image)
        .expect("fresh list CLI completion");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].success, !failure);
    assert_eq!(runs[0].exit_code, Some(if failure { 20 } else { 0 }));
    eprintln!(
        "LIST_CASE failure={failure} source_bytes={} seconds={:?} image_bytes={}",
        source.len(),
        runs[0].start_to_done_host_seconds,
        image.len()
    );
}
