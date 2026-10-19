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
Hidden=Later+10
Later=1
.pub
Answer=Hidden+31
Frame .struct
Tag .byte ?
Value .long ?
.endstruct
Choices={2,7,9}
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
    let mut payload = section_payload(dir, source, selected);
    let mut padding = 0;
    // These fixture code leaves end in RTS/$99; strip only final Hunk padding.
    while payload.last() == Some(&0) {
        payload.pop();
        padding += 1;
    }
    assert!(padding <= 3);
    assert_eq!(payload.last(), Some(&terminal));
    payload
}

fn section_payload(dir: &Path, source: &str, selected: &str) -> Vec<u8> {
    // Keep declarations and bodies unchanged; Rust selects this Hunk section.
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
    segments[0].payload.to_vec()
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
    let nominal = source.replace(
        "Choices={2,7,9}",
        "Choices={2,7,9}\n.priv\nSecret .struct\nCode .byte ?\n.endstruct\n.pub\nSample=Secret{Code:4}",
    );
    section_oracle(dir, &nominal, "third", 0x99);
    fs::write(dir.join("module_compile_bad_nominal.asm"), nominal).unwrap();
    let template = source.replace(".priv\nConfig=7", ".pub\nConfig=7");
    section_oracle(dir, &template, "third", 0x99);
    fs::write(dir.join("module_compile_bad_template.asm"), template).unwrap();
    // Derive each bound from a live assembly of that section with the identical
    // constant and private macro context, rather than instruction-size arithmetic.
    let first = section_oracle(dir, &source, "first", 0x75);
    let second = section_oracle(dir, &source, "second", 0x75);
    let third = section_oracle(dir, &source, "third", 0x99);
    assert_eq!(third, [0x99]);
    let consumer_source = source.replace(".output \"leaf.hunk\"", ".section consumer,kind=code\n .byte Answer,Frame.Value,Choices[1]\n.endsection\n.output \"leaf.hunk\"");
    let consumer = section_payload(dir, &consumer_source, "consumer");
    assert_eq!(consumer.len(), 4, "three consumer bytes plus Hunk padding");
    assert_eq!(consumer[3], 0, "Hunk padding only");
    let selected = [
        first.as_slice(),
        second.as_slice(),
        first.as_slice(),
        second.as_slice(),
        &consumer[..3],
    ]
    .concat();
    fs::write(
        dir.join("module_compile_fixture.i"),
        format!(
            "FIRST_BYTES={}\nSECOND_BYTES={}\nCODE_RESULT_BYTES={}\nRESULT_BYTES={}\n",
            first.len(),
            second.len(),
            selected.len() - 3,
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
        "--dependencies".to_owned(),
        dir.join("probe.d").to_string_lossy().into_owned(),
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
        for define in [
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_MEMORY_TELEMETRY",
            "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
        ] {
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

fn export_component(root: &Path, scratch: &Path, fixture: &Fixture, image: &[u8]) {
    let Some(output) = std::env::var_os("OPFORGE_MODULE_COMPILE_EXPORT_DIR") else {
        return;
    };
    let output = PathBuf::from(output);
    assert!(output.is_absolute(), "export destination must be absolute");
    assert!(
        !output.starts_with(root.canonicalize().unwrap().join("target")),
        "export outside target"
    );
    fs::create_dir(&output).expect("new export directory must not already exist");
    fs::write(output.join("opforge"), image).unwrap();
    fs::write(output.join("oracle.bin"), &fixture.selected).unwrap();
    let mut files = Vec::new();
    for name in [
        "module_compile_source.asm",
        "module_compile_package.bin",
        "module_compile_fixture.i",
    ] {
        let bytes = fs::read(scratch.join(name)).unwrap();
        fs::write(output.join(name), &bytes).unwrap();
        files.push(
            serde_json::json!({"path": name, "digest": opforge_self_host_package_digest(&bytes)}),
        );
    }
    let dependencies = fs::read_to_string(scratch.join("probe.d")).unwrap();
    let (_, dependencies) = dependencies
        .split_once(": ")
        .expect("dependency prerequisites");
    let canonical_scratch = scratch.canonicalize().unwrap();
    let mut paths = dependencies
        .split_whitespace()
        .map(|path| Path::new(path).canonicalize().unwrap())
        .filter(|path| !path.starts_with(&canonical_scratch))
        .collect::<BTreeSet<_>>();
    paths.insert(
        root.join("crates/opforge-asm/src/tests/native_module_compile.rs")
            .canonicalize()
            .unwrap(),
    );
    paths.insert(
        root.join(
            "native/motorola68000/amigaos/test-harnesses/experimental/module_compile_probe.asm",
        )
        .canonicalize()
        .unwrap(),
    );
    let snapshot = paths
        .into_iter()
        .map(|path| {
            let bytes = fs::read(&path).unwrap();
            serde_json::json!({"path": path, "digest": opforge_self_host_package_digest(&bytes)})
        })
        .collect::<Vec<_>>();
    let memory = std::env::var("OPFORGE_MODULE_COMPILE_MEMORY").as_deref() == Ok("1");
    let manifest = serde_json::json!({
        "schema": 1, "kind": "native-module-compile-export", "output_paths": "relative",
        "image": {"path": "opforge", "digest": opforge_self_host_package_digest(image)},
        "oracle": {"path": "oracle.bin", "digest": opforge_self_host_package_digest(&fixture.selected)},
        "files": files, "source_snapshot": snapshot, "source_digest": fixture.source_digest,
        "package_digest": fixture.package_digest, "expected_output_bytes": fixture.selected.len(),
        "scale": fixture.scale, "instrumented": memory,
        "expected_metrics": {"magic": "MCM1", "bytes": 28, "compiles": 1,
            "semantic_bytes_minimum": 1, "raw_code_bytes": fixture.output_bytes,
            "stored_blocks": 3, "retired_pointer": 0},
        "scope": "native module compiler component and moved owned semantic consumer; not CLI self-host",
        "native_validation": "not_run", "command": "opforge",
        "outputs": ["output.bin", "metrics.bin"], "memory_output": memory.then_some("memory.bin"),
    });
    fs::write(
        output.join("manifest.json"),
        serde_json::to_vec_pretty(&manifest).unwrap(),
    )
    .unwrap();
    eprintln!("MODULE_COMPILE_EXPORT {}", output.display());
}

#[test]
fn native_module_compile_host_assembles() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let fixture = prepare(&dir.0);
    let image = build_probe(&root, &dir.0, false);
    assert!(!image.is_empty());
    export_component(&root, &dir.0, &fixture, &image);
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
        assert!(words[2] > 0, "owned semantic interface bytes");
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
                "semantic_bytes": words[2], "owned_consumer_bytes": 3,
                "native_component_seconds": runs[0].start_to_done_host_seconds,
                "full_build_memory_saving": null, "full_build_runtime_saving": null,
            })
        );
    }
}
