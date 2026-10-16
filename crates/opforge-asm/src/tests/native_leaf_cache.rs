//! Independent-process cold/warm prepared-leaf reuse with a live Rust oracle.
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use serde_json::json;

const MAIN: &str = ".module app\n.cpu 68020\n.use definitions as defs\n.byte defs.VALUE,defs.Frame.Field,defs.Frame,defs.SIZE\n.endmodule\n";
const DEFINITIONS: &str = ".module definitions\n.cpu 68020\n.pub\nVALUE = 7\nFrame .struct\nTag .byte ?\nField .word ?\n.endstruct\nSIZE = Frame.Field+2\n.endmodule\n";
const INCLUDE_MAIN: &str = ".module app\n.cpu 68020\n.include \"definitions.asm\"\n.endmodule\n";
const INCLUDE_BODY: &str = ".byte $5a\n";
const CONTROL_BODY: &str = ".module app\n.cpu 68020\n.if 1\n.byte $5a\n.endif\n.endmodule\n";

#[derive(Clone, Copy)]
enum CacheExpectation {
    Disabled,
    Populate,
    Reuse,
    RejectStale,
    Ineligible,
}

impl CacheExpectation {
    fn enabled(self) -> bool {
        !matches!(self, Self::Disabled)
    }
}

pub(super) fn cache_work(stdout: &str) -> Option<(u32, u32, u32)> {
    stdout
        .lines()
        .find(|line| line.starts_with("progress p=00000154 "))
        .map(|line| {
            let values = line
                .split_whitespace()
                .filter_map(|part| part.split_once('='))
                .map(|(key, value)| (key, u32::from_str_radix(value, 16).unwrap()))
                .collect::<std::collections::BTreeMap<_, _>>();
            (values["f"], values["l"], values["r"])
        })
}

fn oracle(main: &str, definitions: &str) -> Vec<u8> {
    let nonce = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let directory =
        std::env::temp_dir().join(format!("prepared-leaf-{}-{nonce}", std::process::id()));
    fs::create_dir(&directory).unwrap();
    fs::write(directory.join("main.asm"), main).unwrap();
    fs::write(directory.join("definitions.asm"), definitions).unwrap();
    let output = directory.join("output.bin");
    let cli = Cli::parse_from([
        "opforge".to_owned(),
        directory.join("main.asm").to_string_lossy().into_owned(),
        "--bin".to_owned(),
        output.to_string_lossy().into_owned(),
    ]);
    let config = validate_cli(&cli).unwrap();
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    let bytes = fs::read(output).unwrap();
    fs::remove_dir_all(directory).unwrap();
    bytes
}

#[test]
fn prepared_leaf_declaration_rust_oracle() {
    assert_eq!(oracle(MAIN, DEFINITIONS), [7, 1, 3, 3]);
    assert_eq!(
        oracle(MAIN, &DEFINITIONS.replace("VALUE = 7", "VALUE = 9")),
        [9, 1, 3, 3]
    );
}

fn run(
    image: &[u8],
    output: &Path,
    name: &str,
    main: &str,
    definitions: &str,
    cache: &[(String, Vec<u8>)],
    expectation: CacheExpectation,
) -> Vec<(String, Vec<u8>)> {
    let directory = output.join(name);
    fs::create_dir(&directory).expect("fresh result directory");
    let expected = oracle(main, definitions);
    let mut owned = vec![
        ("main.asm".to_owned(), main.as_bytes().to_vec()),
        (
            "definitions.asm".to_owned(),
            definitions.as_bytes().to_vec(),
        ),
        ("cache/keep".to_owned(), vec![]),
    ];
    owned.extend_from_slice(cache);
    let files = owned
        .iter()
        .map(|(name, bytes)| OpforgeNativeCliGuestFile {
            relative_path: name,
            bytes,
        })
        .collect::<Vec<_>>();
    let command = if expectation.enabled() {
        "Work:main.asm --bin Work:output.bin --leaf-cache Work:cache"
    } else {
        "Work:main.asm --bin Work:output.bin"
    };
    let case = OpforgeNativeCliParityCase {
        name: "prepared-leaf-cache-comparison",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(b""),
        command_template: Some(command),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/output.bin",
            rust_oracle: &expected,
        },
    };
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let outcome =
        run_prebuilt_compact_cli_case_from_env(&workspace, &case, image).unwrap_or_else(|error| {
            fs::write(directory.join("failure.txt"), &error).unwrap();
            panic!("{name}: {error}")
        });
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("actual native execution required")
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    fs::write(directory.join("assembly.stdout"), &run.stdout).unwrap();
    fs::write(directory.join("assembly.stderr"), &run.stderr).unwrap();
    let stats = cache_work(&run.stdout);
    let mut entries = Vec::new();
    for (path, bytes) in &run.captured_artifacts {
        let relative = path.strip_prefix("Work").unwrap();
        assert!(relative
            .components()
            .all(|part| matches!(part, std::path::Component::Normal(_))));
        let destination = directory.join("guest").join(relative);
        fs::create_dir_all(destination.parent().unwrap()).unwrap();
        fs::write(destination, bytes).unwrap();
        if relative.starts_with("cache") && relative.extension().is_some_and(|s| s == "ofl") {
            entries.push((relative.to_string_lossy().into_owned(), bytes.clone()));
        }
    }
    let result = json!({
        "name": name, "cache_enabled": expectation.enabled(), "cache_inputs": cache.len(),
        "cache_outputs": entries.len(), "guest_protocol_completed": run.protocol_completed,
        "assembler_exit_code": run.exit_code,
        "exact_rust_match": run.verified_output.as_deref() == Some(expected.as_slice()),
        "start_to_done_host_seconds": run.start_to_done_host_seconds,
        "native_image_digest": run.native_image_digest,
        "cache_work": stats.map(|(requests,hits,writes)|json!({"requests":requests,"hits":hits,"writes":writes})),
    });
    fs::write(
        directory.join("result.json"),
        serde_json::to_vec_pretty(&result).unwrap(),
    )
    .unwrap();
    eprintln!("LEAF_CACHE_RESULT {result}");
    assert!(run.protocol_completed && run.success && run.exit_code == Some(0));
    assert_eq!(run.verified_output.as_deref(), Some(expected.as_slice()));
    let (requests, hits, writes) = stats.expect("instrumented image proves actual cache use");
    match expectation {
        CacheExpectation::Disabled => assert_eq!((requests, hits, writes), (0, 0, 0)),
        CacheExpectation::Populate => {
            assert_eq!(hits, 0, "a cold run must not hit a prior entry");
            assert!(
                writes > 0,
                "ordinary preparation must publish an eligible leaf"
            );
        }
        CacheExpectation::Reuse => {
            assert!(hits > 0, "a warm output alone does not prove cache reuse");
        }
        CacheExpectation::RejectStale => {
            assert_eq!(hits, 0, "stale or invalid entries must not be hits");
            assert!(
                writes > 0,
                "ordinary preparation should publish a fresh leaf"
            );
        }
        CacheExpectation::Ineligible => {
            assert_eq!(hits, 0, "ineligible source bodies must not hit the cache");
            assert_eq!(
                writes, 0,
                "ineligible source bodies must not publish leaves"
            );
        }
    }
    entries
}

#[test]
#[ignore = "requires instrumented OPFORGE_LEAF_IMAGE, fresh OPFORGE_LEAF_RESULTS and FS-UAE"]
fn native_prepared_leaf_cache_fs_uae() {
    let image = fs::read(std::env::var_os("OPFORGE_LEAF_IMAGE").unwrap()).unwrap();
    let output = PathBuf::from(std::env::var_os("OPFORGE_LEAF_RESULTS").unwrap());
    fs::create_dir(&output).expect("fresh retained result directory");
    run(
        &image,
        &output,
        "reference",
        MAIN,
        DEFINITIONS,
        &[],
        CacheExpectation::Disabled,
    );
    let entries = run(
        &image,
        &output,
        "cold",
        MAIN,
        DEFINITIONS,
        &[],
        CacheExpectation::Populate,
    );
    assert!(
        !entries.is_empty(),
        "cold run must publish a real cache entry"
    );
    run(
        &image,
        &output,
        "warm",
        MAIN,
        DEFINITIONS,
        &entries,
        CacheExpectation::Reuse,
    );
    let changed = DEFINITIONS.replace("VALUE = 7", "VALUE = 9");
    run(
        &image,
        &output,
        "changed-source",
        MAIN,
        &changed,
        &entries,
        CacheExpectation::RejectStale,
    );
    let context = MAIN.replace(".use definitions", "earlier = 123\n.use definitions");
    run(
        &image,
        &output,
        "changed-context",
        &context,
        DEFINITIONS,
        &entries,
        CacheExpectation::RejectStale,
    );
    let mut corrupt = entries.clone();
    for (_, bytes) in &mut corrupt {
        *bytes.last_mut().unwrap() ^= 1;
    }
    run(
        &image,
        &output,
        "corrupt",
        MAIN,
        DEFINITIONS,
        &corrupt,
        CacheExpectation::RejectStale,
    );
    let mut truncated = entries.clone();
    for (_, bytes) in &mut truncated {
        bytes.truncate(bytes.len() / 2);
    }
    run(
        &image,
        &output,
        "truncated",
        MAIN,
        DEFINITIONS,
        &truncated,
        CacheExpectation::RejectStale,
    );
    run(
        &image,
        &output,
        "include-body-ineligible",
        INCLUDE_MAIN,
        INCLUDE_BODY,
        &entries,
        CacheExpectation::Ineligible,
    );
    run(
        &image,
        &output,
        "control-body-ineligible",
        CONTROL_BODY,
        DEFINITIONS,
        &entries,
        CacheExpectation::Ineligible,
    );
}
