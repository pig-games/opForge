//! Complete live output proof for the bounded fragment experiment.
use super::native_iterable_loops::scratch;
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

pub(super) fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

pub(super) fn source(guard: u8, placement: u32) -> String {
    format!(
        r#".module main
.cpu 68020
.use dep
.region ram, ${placement:x}, ${end:x}
.section code,kind=code
.long dep.helper
.long dep.entry.inside
.long dep.payload
.endsection
.section data,kind=data
.byte $77
.endsection
.place code in ram
.place data in ram
.output "build/fragment.bin",format=bin,sections=code,data
.output "build/fragment.hunk",format=hunk,sections=code,data
.endmodule
.module dep
.cpu 68020
.pub
.section code,kind=code
entry .block
.byte $11
inside:
.long helper
.if forward
.byte $31
.else
.byte $32
.endif
.for item in {{2,4}}
.byte item
.endfor
.byte $22
.bend
helper .block
.byte $44
.bend
unused .block
.byte $99
.bend
forward = {guard}
.endsection
.section data,kind=data
payload .byte $55
.endsection
.endmodule
.module empty
.cpu 68020
.endmodule
"#,
        end = placement + 0xff
    )
}

fn oracle(source: &str) -> (Vec<u8>, Vec<u8>) {
    let dir = scratch();
    fs::create_dir_all(dir.0.join("build")).unwrap();
    let input = dir.0.join("entry.asm");
    let output = dir.0.join("build/fragment.bin");
    fs::write(&input, source).unwrap();
    let cli = Cli::parse_from(["opforge".into(), input.to_string_lossy().into_owned()]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.0.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    (
        fs::read(output).unwrap(),
        fs::read(dir.0.join("build/fragment.hunk")).unwrap(),
    )
}

#[test]
fn native_fragment_live_rust_controls() {
    for guard in [0, 1] {
        for placement in [0x1000, 0x2000] {
            let (_, hunk) = oracle(&source(guard, placement));
            let segments = hunk::segments(&hunk).unwrap();
            assert_eq!(segments.len(), 2);
            assert!(segments[0].payload.contains(&0x11));
            assert!(segments[0].payload.contains(&0x22));
            assert!(segments[0].payload.contains(&0x44));
            assert!(!segments[0].payload.contains(&0x99));
            assert!(segments[0]
                .payload
                .contains(&(if guard == 1 { 0x31 } else { 0x32 })));
            assert_eq!(segments[0].relocations.len(), 4);
        }
    }
}

fn expected_artifacts<'a>(
    oracle: &'a (Vec<u8>, Vec<u8>),
) -> [OpforgeNativeCliExpectedArtifact<'a>; 2] {
    [
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/fragment.bin",
            rust_oracle: &oracle.0,
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/fragment.hunk",
            rust_oracle: &oracle.1,
        },
    ]
}

fn guest_files(source: &str) -> [OpforgeNativeCliGuestFile<'_>; 2] {
    [
        OpforgeNativeCliGuestFile {
            relative_path: "entry.asm",
            bytes: source.as_bytes(),
        },
        OpforgeNativeCliGuestFile {
            relative_path: "build/.keep",
            bytes: b"",
        },
    ]
}

fn case<'a>(
    name: &'a str,
    source: &'a str,
    artifacts: &'a [OpforgeNativeCliExpectedArtifact<'a>],
    files: &'a [OpforgeNativeCliGuestFile<'a>],
) -> OpforgeNativeCliParityCase<'a> {
    OpforgeNativeCliParityCase {
        name,
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some("Work:entry.asm"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: files,
        proof: OpforgeNativeCliProof::ExactArtifacts(artifacts),
    }
}

#[test]
#[ignore = "requires fresh FS-UAE and prebuilt baseline/candidate/probe images"]
fn native_fragment_prebuilt_fs_uae() {
    let root = root();
    let inputs: Vec<_> = [0, 1]
        .into_iter()
        .flat_map(|guard| {
            [0x1000, 0x2000].into_iter().map(move |placement| {
                (
                    format!("fragment-{guard}-{placement:x}"),
                    source(guard, placement),
                )
            })
        })
        .collect();
    let oracles: Vec<_> = inputs.iter().map(|(_, source)| oracle(source)).collect();
    let artifacts: Vec<_> = oracles.iter().map(expected_artifacts).collect();
    let files: Vec<_> = inputs
        .iter()
        .map(|(_, source)| guest_files(source))
        .collect();
    let cases: Vec<_> = inputs
        .iter()
        .zip(&artifacts)
        .zip(&files)
        .map(|(((name, source), artifacts), files)| case(name, source, artifacts, files))
        .collect();
    for variable in [
        "OPFORGE_FRAGMENT_REFERENCE_IMAGE",
        "OPFORGE_FRAGMENT_CANDIDATE_IMAGE",
        "OPFORGE_FRAGMENT_PROBE_IMAGE",
    ] {
        if std::env::var("OPFORGE_FRAGMENT_CHECK_PROBE_ONLY").as_deref() == Ok("1")
            && variable != "OPFORGE_FRAGMENT_PROBE_IMAGE"
        {
            continue;
        }
        let image =
            fs::read(std::env::var_os(variable).unwrap_or_else(|| panic!("set {variable}")))
                .unwrap();
        let results = run_prebuilt_compact_cli_breadth_from_env(&root, &cases, &image, 4).unwrap();
        assert_eq!(results.len(), 4);
        for (result, case) in results.iter().zip(&cases) {
            let run = result.run.as_ref().expect("fresh guest run");
            eprintln!("FRAGMENT_CASE image={variable} name={} exit={:?} success={} error={:?} stdout={} stderr={}",case.name,run.exit_code,run.success,result.error,run.stdout,run.stderr);
            if let Some(directory) = std::env::var_os("OPFORGE_FRAGMENT_NATIVE_EVIDENCE_DIR") {
                let directory = PathBuf::from(directory).join(variable).join(case.name);
                assert!(!directory.starts_with(root.join("target")));
                fs::create_dir_all(&directory).unwrap();
                fs::write(directory.join("stdout.txt"), &run.stdout).unwrap();
                if let Some(memory) = run.captured_artifacts.get(Path::new("Work/memory.bin")) {
                    fs::write(directory.join("memory.bin"), memory).unwrap();
                }
            }
            assert!(
                run.protocol_completed && run.success && run.exit_code == Some(0),
                "{variable} {}: fresh guest failed",
                case.name
            );
            assert!(
                result.error.is_none(),
                "{variable} {}: {:?}",
                case.name,
                result.error
            );
        }
    }
}

fn timing_source() -> String {
    let source = source(1, 0x1000).replace("$10ff", "$10fff");
    let macro_body =
        "step .macro value\n\tmoveq #.value,d0\n\taddq.l #1,d0\n.byte .value,.value\n.endmacro\n";
    let source = source.replace(
        ".module dep\n.cpu 68020\n",
        &format!(".module dep\n.cpu 68020\n{macro_body}"),
    );
    let calls: String = (0..1200)
        .map(|index| format!(".step {}\n", index % 64))
        .collect();
    source.replace(".endfor\n.byte $22", &format!(".endfor\n{calls}.byte $22"))
}

#[test]
#[ignore = "single fresh exact-artifact control timing; requires prebuilt image and FS-UAE"]
fn native_fragment_control_timing_fs_uae() {
    let image_path = std::env::var_os("OPFORGE_FRAGMENT_TIMING_IMAGE")
        .expect("set OPFORGE_FRAGMENT_TIMING_IMAGE");
    let image = fs::read(&image_path).unwrap();
    let source = timing_source();
    let oracle = oracle(&source);
    let artifacts = expected_artifacts(&oracle);
    let files = guest_files(&source);
    let case = case("fragment-control-timing", &source, &artifacts, &files);
    let _guard = fs_uae_native_cli_smoke_lock().lock().unwrap();
    let outcome = run_prebuilt_compact_cli_case_from_env(&root(), &case, &image)
        .expect("fresh fragment timed control");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("timed control requires native completion")
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.protocol_completed && run.success && run.exit_code == Some(0));
    let instrumented = std::env::var("OPFORGE_ITERABLE_MEMORY").as_deref() == Ok("1");
    let memory = if instrumented {
        let bytes = run
            .captured_artifacts
            .get(Path::new("Work/memory.bin"))
            .expect("captured current MEMD")
            .as_slice();
        Some(super::native_package_loading_performance::memory(bytes))
    } else {
        None
    };
    let receipt = serde_json::json!({"image": image_path.to_string_lossy(), "image_bytes": image.len(), "memory_profile":std::env::var("OPFORGE_FS_UAE_MEMORY_PROFILE").unwrap_or_else(|_| "existing".into()), "linked_static_bytes": super::native_package_loading_performance::linked_static_bytes(&image), "input_digest": opforge_self_host_package_digest(source.as_bytes()), "input_bytes": source.len(), "macro_calls":1200, "output_bytes":oracle.0.len(), "guest_start_to_done_host_seconds":run.start_to_done_host_seconds.expect("START/DONE timing"), "instrumented":instrumented, "measurement":if instrumented {"memory; instrumented time is diagnostic"} else {"release control"}, "memory":memory});
    let save = PathBuf::from(
        std::env::var_os("OPFORGE_FRAGMENT_EVIDENCE_DIR")
            .expect("set OPFORGE_FRAGMENT_EVIDENCE_DIR"),
    );
    fs::create_dir_all(&save).unwrap();
    let image_digest = opforge_self_host_package_digest(&image);
    let stem = format!("fragment-control-{image_digest}");
    fs::write(
        save.join(format!("{stem}.json")),
        serde_json::to_vec_pretty(&receipt).unwrap(),
    )
    .unwrap();
    fs::write(save.join(format!("{stem}.asm")), source).unwrap();
    fs::write(save.join(format!("{stem}.stdout.txt")), &run.stdout).unwrap();
    if instrumented {
        fs::write(
            save.join(format!("{stem}.memd")),
            run.captured_artifacts
                .get(Path::new("Work/memory.bin"))
                .expect("captured current MEMD")
                .as_slice(),
        )
        .unwrap();
    }
    eprintln!("FRAGMENT_CONTROL {receipt}");
}
