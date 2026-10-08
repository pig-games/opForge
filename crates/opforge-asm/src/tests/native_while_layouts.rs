//! Raw Hunk/placed parity and explicit mapped-layout capability boundaries for while.
//! A filtered-layout rejection is not Rust/native language or diagnostic parity.
use super::native_iterable_loops::{build_image, oracle, scratch};
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

fn raw_hunk(cpu: &str) -> String {
    format!(".module entry\n.cpu {cpu}\n.section code,kind=code\n.while $<2\n.byte 1\n.endwhile\n.while 0\n.byte missing\n.endwhile\n.endsection\n.section data,kind=data\n.byte 7\n.endsection\n.output \"out.hunk\",format=hunk,sections=code,data\n.endmodule\n")
}

pub(super) fn hunk_oracle(dir: &Path, source: &str) -> Vec<u8> {
    let input = dir.join("entry.asm");
    let output = dir.join("out.hunk");
    fs::write(
        &input,
        source.replace("\"out.hunk\"", &format!("\"{}\"", output.display())),
    )
    .unwrap();
    let cli = Cli::parse_from(["opForge".to_string(), input.to_string_lossy().into_owned()]);
    let config = validate_cli(&cli).unwrap();
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    fs::read(output).unwrap()
}

fn body(has_while: bool) -> &'static str {
    if has_while {
        ".while 0\n.byte missing\n.endwhile\n.byte 7\n"
    } else {
        ".byte 7\n"
    }
}

fn layout(cpu: &str, mode: u8, has_while: bool) -> String {
    let code = if mode == 3 {
        if has_while {
            "start .byte 7\n.while $-start<2\n.byte 7\n.endwhile\n"
        } else {
            "start .byte 7,7,7\n"
        }
    } else {
        body(has_while)
    };
    if mode == 3 {
        return format!(".module entry\n.cpu {cpu}\n.region rom_a,$1000,$1002\n.region rom_b,$1003,$10ff\n.section app_a\n{code}.endsection\n.section app_b\n.byte 8\n.endsection\n.place app_a in rom_a\n.place app_b in rom_b\n.endmodule\n.end\n");
    }
    assert!(matches!(mode, 2 | 4));
    let second_dep = if mode == 4 {
        format!(".module dep_b\n.cpu {cpu}\n.pub\n.section code_b,logical\nentry .block\n.byte 10\n.bend\n.endsection\n.endmodule\n")
    } else {
        String::new()
    };
    let second_map = if mode == 4 {
        ".use dep_b (entry) as right map { code_b -> app_b }\n"
    } else {
        ""
    };
    let second_section = if mode == 4 {
        ".section app_b\n.byte 8\n.word right.entry\n.endsection\n.place app_b in rom_b\n"
    } else {
        ""
    };
    let regions = if mode == 4 {
        ".region rom_a,$1000,$1003\n.region rom_b,$1004,$10ff\n"
    } else {
        ".region rom_a,$1000,$10ff\n"
    };
    format!(".module dep_a\n.cpu {cpu}\n.pub\n.section code_a,logical\nentry .block\n.byte 9\n.bend\n.endsection\n.endmodule\n{second_dep}.module entry\n.cpu {cpu}\n{regions}.use dep_a (entry) as left map {{ code_a -> app_a }}\n{second_map}.section app_a\n{code}.word left.entry\n.endsection\n.place app_a in rom_a\n{second_section}.endmodule\n")
}

#[test]
fn native_while_layouts_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        let image = hunk_oracle(&dir.0, &raw_hunk(cpu));
        let parts = hunk::segments(&image).unwrap();
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0].kind, 0x3e9);
        assert_eq!(parts[0].payload, [1, 1, 1, 0]);
        assert_eq!(parts[1].kind, 0x3ea);
        assert_eq!(parts[1].payload, [7, 0, 0, 0]);
        for mode in [2, 3, 4] {
            let accepted = oracle(&dir.0, &layout(cpu, mode, false)).unwrap();
            let with_while = oracle(&dir.0, &layout(cpu, mode, true)).unwrap();
            assert_eq!(
                with_while, accepted,
                "{cpu}/mode{mode}: loop and control output"
            );
        }
    }
}

pub(super) fn native_case(
    root: &Path,
    image: &[u8],
    name: &str,
    source: &str,
    command: &str,
    expected: &[u8],
    rejected: bool,
) {
    let files = [OpforgeNativeCliGuestFile {
        relative_path: "entry.asm",
        bytes: source.as_bytes(),
    }];
    let output = if command.contains("--hunk") {
        "Work/out.hunk"
    } else {
        "Work/out.bin"
    };
    let case = OpforgeNativeCliParityCase {
        name,
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(command),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: if rejected {
            OpforgeNativeCliProof::ExpectedFailureWithDiagnostic
        } else {
            OpforgeNativeCliProof::ExactArtifact {
                relative_path: output,
                rust_oracle: expected,
            }
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(root, &case, image).unwrap();
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("fresh real-native completion required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].success, !rejected);
    assert_eq!(runs[0].exit_code, Some(if rejected { 20 } else { 0 }));
    eprintln!(
        "WHILE_LAYOUT name={name} rejected={rejected} source_bytes={} output_bytes={} seconds={:?} image_bytes={}",
        source.len(), expected.len(), runs[0].start_to_done_host_seconds, image.len()
    );
}

#[test]
#[ignore = "requires FS-UAE; exact whole-Hunk while and filtered-layout capability controls"]
fn native_while_layouts_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_WHILE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let cpus = std::env::var("OPFORGE_WHILE_LAYOUT_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let selection = std::env::var("OPFORGE_WHILE_LAYOUT_CASES")
        .unwrap_or_else(|_| "raw-hunk,mode2,mode3,mode4".into());
    let selected = selection.split(',').map(str::trim).collect::<Vec<_>>();
    for case in &selected {
        assert!(
            matches!(*case, "raw-hunk" | "mode2" | "mode3" | "mode4"),
            "unknown while layout selector {case}"
        );
    }
    let mut attempted = 0;
    let mut failures = Vec::new();
    let mut check = |cpu: &str, name: &str, source: String, hunk: bool, rejected: bool| {
        attempted += 1;
        if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            // Oracle failures are included: they cannot silently drop a case.
            let expected = if hunk {
                hunk_oracle(&dir.0, &source)
            } else {
                oracle(&dir.0, &source).unwrap()
            };
            native_case(
                &root,
                &image,
                name,
                &source,
                if hunk {
                    "Work:entry.asm --hunk Work:out.hunk"
                } else {
                    "Work:entry.asm --bin Work:out.bin"
                },
                &expected,
                rejected,
            );
        }))
        .is_err()
        {
            failures.push(format!("{cpu}/{name}"));
        }
    };
    for cpu in cpus.split(',').map(str::trim) {
        assert!(matches!(cpu, "68020" | "6502"), "unknown fixture CPU {cpu}");
        if selected.contains(&"raw-hunk") {
            check(cpu, "while-raw-hunk", raw_hunk(cpu), true, false);
        }
        for mode in [2, 3, 4] {
            if !selected.contains(&format!("mode{mode}").as_str()) {
                continue;
            }
            for has_while in [false, true] {
                let rejected = has_while && mode != 3;
                // Rust accepts both; mapped native rejection is an explicit gap.
                let name = format!(
                    "while-mode{mode}-{}",
                    if rejected {
                        "reject"
                    } else if has_while {
                        "positive"
                    } else {
                        "control"
                    }
                );
                check(cpu, &name, layout(cpu, mode, has_while), false, rejected);
            }
        }
    }
    assert!(attempted > 0, "while layout selection must attempt a case");
    assert!(
        failures.is_empty(),
        "while layout cases failed: {failures:?}; all selected cases were attempted"
    );
}
