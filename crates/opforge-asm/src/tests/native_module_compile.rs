//! Actual native source compilation into an owned reusable module image.
//! This component comparison makes no claim about current CLI reuse or speed.
use super::native_iterable_loops::scratch;
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use vm::runtime_model_core::RuntimeModelCore;

const SOURCE: &str = r#".module leaf
.cpu 68020
.priv
Config=7
emitConfigured .macro
 moveq #Config,d0
.endmacro
.section first,kind=code
first .block
 .emitConfigured
 bra.w done
 .byte $11,$22
done
 rts
.bend
.endsection
.section second,kind=code
second .block
 moveq #9,d1
 rts
.bend
.endsection
.section third,kind=code
third .block
 .byte $99
.bend
.endsection
.output "leaf.hunk",format=hunk,sections=first,second,third
.endmodule
"#;
struct Fixture {
    source: String,
    source_digest: String,
    package_digest: String,
    selected: Vec<u8>,
    output_bytes: usize,
    scale: usize,
}

fn env_count(name: &str, default: usize) -> usize {
    std::env::var(name)
        .map(|value| value.parse().expect("numeric module compile control"))
        .unwrap_or(default)
}

fn concise_failure(context: &str, failure: cli_core::CliRunError) -> ! {
    match failure {
        cli_core::CliRunError::Assembler { error, .. } => {
            let details = error
                .diagnostics()
                .iter()
                .take(6)
                .map(|diagnostic| {
                    format!(
                        "{}:{}: {}",
                        diagnostic.file.as_deref().unwrap_or("<unknown>"),
                        diagnostic.line,
                        diagnostic.error.message()
                    )
                })
                .collect::<Vec<_>>()
                .join("\n");
            panic!("{context}: {}\n{details}", error.summary());
        }
        cli_core::CliRunError::Workflow { error, .. } => panic!("{context}: {error}"),
        cli_core::CliRunError::WarningsAsErrors { .. } => {
            panic!("{context}: warnings treated as errors")
        }
    }
}

fn section_oracle(dir: &Path, source: &str, selected: &str, terminal: u8) -> Vec<u8> {
    // Keep every body and declaration unchanged. Select this leaf as the Hunk
    // entry so ordinary Rust reachability retains the same requested block.
    let source = source.replace(
        "sections=first,second,third",
        &format!("sections={selected}"),
    );
    let input = dir.join("oracle.asm");
    fs::write(&input, source).unwrap();
    let cli = Cli::parse_from(["opforge".to_owned(), input.to_string_lossy().into_owned()]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config)
        .unwrap_or_else(|error| concise_failure("module compile source oracle", error));
    let bytes = fs::read(dir.join("leaf.hunk")).unwrap();
    let segments = hunk::segments(&bytes).expect("live section Hunk oracle");
    assert_eq!(segments.len(), 1);
    assert!(segments[0].relocations.is_empty());
    let mut payload = segments[0].payload.to_vec();
    let mut padding = 0;
    // Fixture-specific: each leaf ends in RTS ($75) or the explicit $99 byte.
    // Only Hunk's final word padding may be zero; this is not general trimming.
    while payload.last() == Some(&0) {
        payload.pop();
        padding += 1;
    }
    assert!(padding <= 3);
    assert_eq!(payload.last(), Some(&terminal));
    payload
}

fn prepare(dir: &Path) -> Fixture {
    let scale = env_count("OPFORGE_MODULE_COMPILE_SCALE", 0);
    assert!(matches!(scale, 0 | 64), "supported scales are 0 and 64");
    let mut source = SOURCE.to_owned();
    if scale != 0 {
        let mut work = String::new();
        for index in 0..scale {
            work.push_str(&format!(" moveq #7,d2\n add.l d2,d0\n move.l 4(a0),d3\n cmpi.l #7,d3\n bne.w step{index}\n nop\nstep{index}\n"));
        }
        source = source.replace(" .emitConfigured\n", &format!(" .emitConfigured\n{work}"));
        source = source.replace(" moveq #9,d1\n", &format!(" moveq #9,d1\n{work}"));
    }
    let core = RuntimeModelCore::from_registry(&engine::build_default_asm_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = crate::binary_source_experiment::prepare_package(&core, &resolved).unwrap();
    let package_digest = opforge_self_host_package_digest(&package);
    let source_digest = format!(
        "fnv1a64:{:016x}",
        fnv1a64_update(0xcbf2_9ce4_8422_2325, source.as_bytes())
    );
    fs::write(dir.join("module_compile_package.bin"), package).unwrap();
    fs::write(dir.join("module_compile_source.asm"), &source).unwrap();
    fs::write(
        dir.join("module_compile_bad_absolute.asm"),
        source
            .replace("Config=7", "Config=7\nAlias=second")
            .replace(" bra.w done", " .long Alias"),
    )
    .unwrap();
    fs::write(
        dir.join("module_compile_bad_relative.asm"),
        source.replace(" bra.w done", " bra.w second"),
    )
    .unwrap();
    // Derive each bound from a live assembly of that section with the identical
    // constant and private macro context, rather than instruction-size arithmetic.
    let first = section_oracle(dir, &source, "first", 0x75);
    let second = section_oracle(dir, &source, "second", 0x75);
    let third = section_oracle(dir, &source, "third", 0x99);
    assert_eq!(third, [0x99]);
    let selected = [
        first.as_slice(),
        second.as_slice(),
        first.as_slice(),
        second.as_slice(),
    ]
    .concat();
    fs::write(
        dir.join("module_compile_fixture.i"),
        format!(
            "FIRST_BYTES={}\nSECOND_BYTES={}\nRESULT_BYTES={}\n",
            first.len(),
            second.len(),
            selected.len()
        ),
    )
    .unwrap();
    Fixture {
        source,
        source_digest,
        package_digest,
        selected,
        output_bytes: first.len() + second.len() + third.len(),
        scale,
    }
}

fn build_probe(root: &Path, dir: &Path, rejections: bool) -> Vec<u8> {
    let input = dir.join("module_compile_probe.asm");
    fs::copy(
        root.join(
            "native/motorola68000/amigaos/test-harnesses/experimental/module_compile_probe.asm",
        ),
        &input,
    )
    .unwrap();
    let native = root.join("native/motorola68000/amigaos");
    let mut args = vec![
        "opforge".to_owned(),
        input.to_string_lossy().into_owned(),
        "-M".to_owned(),
        native.to_string_lossy().into_owned(),
        "-I".to_owned(),
        native.join("debug").to_string_lossy().into_owned(),
    ];
    if rejections {
        args.extend([
            "--define".to_owned(),
            "OPFORGE_MODULE_COMPILE_REJECTIONS".to_owned(),
        ]);
    }
    if std::env::var("OPFORGE_MODULE_COMPILE_MEMORY").as_deref() == Ok("1") {
        // Failure-path accounting is a separate correctness run. It must not
        // contribute to the unchanged positive-input resource comparison.
        for define in ["OPFORGE_DEBUG_CONTRACTS", "OPFORGE_MEMORY_TELEMETRY"] {
            args.extend(["--define".to_owned(), define.to_owned()]);
        }
    }
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config)
        .unwrap_or_else(|error| concise_failure("module compile native harness", error));
    fs::read(dir.join("module-compile-probe.hunk")).unwrap()
}

#[test]
fn native_module_compile_host_assembles() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    prepare(&dir.0);
    assert!(!build_probe(&root, &dir.0, false).is_empty());
}

#[test]
#[ignore = "requires FS-UAE; actual native source compiler and saved-image reuse"]
fn native_module_compile_fs_uae() {
    native_run(false);
}

#[test]
#[ignore = "requires FS-UAE; native module compiler cross-block rejection cleanup"]
fn native_module_compile_rejections_fs_uae() {
    native_run(true);
}

fn native_run(rejections: bool) {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let fixture = prepare(&dir.0);
    let expected = &fixture.selected;
    let image = build_probe(&root, &dir.0, rejections);
    let case = OpforgeNativeCliParityCase {
        name: "native-module-source-component",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(fixture.source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/output.bin",
            rust_oracle: expected,
        },
    };
    let repetitions = env_count("OPFORGE_MODULE_COMPILE_REPETITIONS", 1);
    assert!((1..=3).contains(&repetitions));
    for trial in 0..repetitions {
        let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
            .expect("fresh native source component");
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
        let metrics = runs[0]
            .captured_artifacts
            .get(&PathBuf::from("Work/metrics.bin"))
            .expect("fresh native compiler metrics");
        assert_eq!(metrics.len(), 28);
        let words = metrics
            .chunks_exact(4)
            .map(|chunk| u32::from_be_bytes(chunk.try_into().unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(words[0], 0x4d434d31);
        assert_eq!(words[1], 1, "one actual native compile");
        assert_eq!(words[2], 0, "reserved metric word");
        assert_eq!(words[3], fixture.output_bytes as u32);
        assert!(words[4] > words[3], "saved image includes its directory");
        assert_eq!(words[5], 3);
        assert_eq!(words[6], 0, "retired image owner");
        if std::env::var("OPFORGE_MODULE_COMPILE_MEMORY").as_deref() == Ok("1") {
            let bytes = runs[0]
                .captured_artifacts
                .get(&PathBuf::from("Work/memory.bin"))
                .expect("fresh MEMD record");
            let mut memory = super::native_package_loading_performance::memory(bytes);
            let retained = u32::from_be_bytes(bytes[20..24].try_into().unwrap()) as u64;
            let peak = memory["peak_owned_bytes"].as_u64().unwrap();
            assert!(peak >= retained);
            let linked = super::native_package_loading_performance::linked_static_bytes(&image);
            memory["retained_after_publication_bytes"] = retained.into();
            memory["linked_static_reserved_bytes"] = linked.into();
            memory["stack_bytes"] = 65536u64.into();
            memory["process_peak_estimate_bytes"] = (linked + peak + 65536).into();
            memory["scope"] = "linked static + tracked heap peak + configured stack; OS and untracked allocations excluded".into();
            eprintln!("MODULE_COMPILE_MEMORY {memory}");
        }
        eprintln!(
            "MODULE_COMPILE_COMPONENT {}",
            serde_json::json!({
                "scope": "actual native source compiler and first-reference image reuse; not CLI integration",
                "native_image_digest": runs[0].native_image_digest,
                "source_digest": fixture.source_digest,
                "package_digest": fixture.package_digest,
                "source_bytes": fixture.source.len(),
                "fresh_protocol": runs[0].protocol_completed,
                "exact_rust_bytes": expected.len(), "scale": fixture.scale, "trial": trial + 1, "rejections_checked": rejections,
                "native_compiles": words[1], "selected_requests": 5, "stored_blocks": words[5],
                "prepared_bytes": null, "output_bytes": words[3], "image_bytes": words[4],
                "native_component_seconds": runs[0].start_to_done_host_seconds,
                "full_build_memory_saving": null, "full_build_runtime_saving": null,
            })
        );
    }
}
