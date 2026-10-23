//! Shared language controls and fresh exact-artifact proof against the current native CLI.
use super::native_iterable_loops::scratch;
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

pub(super) struct Control {
    pub name: String,
    pub source: String,
    pub includes: Vec<(&'static str, &'static str)>,
}

pub(super) fn controls() -> Vec<Control> {
    let mut result = Vec::new();
    for guard in [0, 1] {
        result.push(Control {
            name: format!("blocks-guard-{guard}"),
            source: super::native_fragment_controls::source(guard, 0x1000),
            includes: vec![],
        });
    }
    let base = super::native_fragment_controls::source(1, 0x1000);
    for (name, definitions, invocation) in [
        (
            "zero-output-macro",
            "EMPTY .macro\n.if 0\n.byte $6a\n.endif\n.endmacro\n",
            ".EMPTY\n",
        ),
        (
            "emitting-macro",
            "EMIT .macro value\n.byte .value\n.endmacro\n",
            ".EMIT $6a\n",
        ),
        (
            "nested-macro",
            "EMIT .macro value\n.byte .value\n.endmacro\nOUTER .macro value\n.EMIT .value\n.endmacro\n",
            ".OUTER $6a\n",
        ),
    ] {
        for referenced in [false, true] {
            // Definitions remain identical and shared in both controls. Only
            // the root reference changes whether the entire costed unit lives.
            let source = base
                .replace(".module dep\n.cpu 68020\n", &format!(".module dep\n.cpu 68020\n{definitions}"))
                .replace("entry .block", &format!("costed .block\n{invocation}.byte $6b\n.bend\nentry .block"));
            let source = if referenced {
                source.replace(".long dep.payload", ".long dep.payload\n.long dep.costed")
            } else {
                source
            };
            result.push(Control {
                name: format!("{name}-{}", if referenced { "selected" } else { "unreferenced" }),
                source,
                includes: vec![],
            });
        }
    }
    for referenced in [false, true] {
        let body = "costed .block\n\tmoveq #2,d0\n\tadd.l d0,d1\n.byte $31\ninside:\n.byte $32\n.long inside\n.bend\n";
        let source = base.replace("entry .block", &format!("{body}entry .block"));
        let source = if referenced {
            source.replace(".long dep.payload", ".long dep.payload\n.long dep.costed")
        } else {
            source
        };
        result.push(Control {
            name: format!(
                "whole-work-{}",
                if referenced {
                    "selected"
                } else {
                    "unreferenced"
                }
            ),
            source,
            includes: vec![],
        });
    }
    for (name, source, includes) in [
        ("preceding-nested", base.replace(".long dep.helper", ".long dep.helper\n.long dep.preceding").replace("entry .block\n.byte $11", "preceding:\nentry .block\n.byte $11\nnested .block\n.byte $18\n.bend"), vec![]),
        ("generated-name", base.replace(".byte $11", "MAKE .macro suffix\ngenerated@1:\n.byte $11\n.long generated@1\n.endmacro\n.MAKE A"), vec![]),
        ("generated-block", base.replace(".byte $11", "MAKE .macro suffix\ngenerated@1 .block\n.byte $11\n.bend\n.long generated@1\n.endmacro\n.MAKE A"), vec![]),
        ("loop-shadow-schema", base.replace(".for item in {2,4}", "Info .struct\nvalue .byte ?\n.endstruct\nsaved .const Info {value:7}\n.byte saved.value\nitem={9,11}\n.for item in {2,4}").replace(".endfor\n.byte $22", ".endfor\n.byte item[1]\n.byte $22"), vec![]),
        ("active-include-resume", base.replace(".byte $11", ".byte $11\n.if 1\n.include \"active.inc\"\n.endif\n.byte $19"), vec![("active.inc", ".byte $17\n.if 0\n.include \"missing.inc\"\n.endif\n")]),
        ("transitive-forward-member", base.replace(".long dep.payload", ".long dep.payload\n.byte dep.saved.value").replace(".long helper", ".long leaf.helper\n.byte saved.value").replace(".module dep\n.cpu 68020", ".module dep\n.cpu 68020\n.use leaf").replace("forward = 1", "Info .struct\nvalue .byte ?\n.endstruct\nsaved .const Info {value:later}\nlater .const 7\nforward = 1") + ".module leaf\n.cpu 68020\n.pub\n.section code,kind=code\nhelper .block\n.byte $48\n.bend\n.endsection\n.endmodule\n", vec![]),
    ] {
        result.push(Control { name: name.into(), source, includes });
    }
    // Separate discovered modules expose `.end` operands in the shared
    // template header to the acquisition index; they are not boundaries.
    result.push(Control {
        name: "imported-template-boundary".into(),
        source: r#".module main
.cpu 68020
.use depA as a
.use depB as b
.region ram,$1000,$ffff
.section code,kind=code
.long a.entry,b.entry
.endsection
.section data,kind=data
.byte $77
.endsection
.place code in ram
.place data in ram
.output "build/fragment.bin",format=bin,sections=code,data
.output "build/fragment.hunk",format=hunk,sections=code,data
.endmodule
"#.into(),
        includes: vec![
            ("shared_templates.i", "SHARED .macro start,end\n.byte .start,.end\n.endmacro\n"),
            ("depA.asm", ".module depA\n.cpu 68020\n.include \"shared_templates.i\"\n.pub\n.section code,kind=code\nentry .block\n.byte $5a\n.bend\n.endsection\n.endmodule\n"),
            ("depB.asm", ".module depB\n.cpu 68020\n.include \"shared_templates.i\"\n.pub\n.section code,kind=code\nentry .block\n.byte $5b\n.bend\n.endsection\n.endmodule\n"),
        ],
    });
    result.push(Control {
        name: "typed-configuration".into(),
        source: base
            .replace(
                ".use dep",
                "Info .struct\nvalue .byte ?\n.endstruct\n.use dep with (VALUE=Info{value:7})",
            )
            .replace(".byte $11", ".byte $11,VALUE.value"),
        includes: vec![],
    });
    let definitions = "ONCE .macro value=7\n.byte .value\n.endmacro\nOUTER .macro value\n.ONCE .value\n.endmacro\nSTASH .segment value\n.byte .value\n.endsegment\nEMPTY .macro\n.endmacro\nUNUSED .macro value=9\n.byte .value\n.endmacro\nUNUSED_SEG .segment\n.byte 10\n.endsegment\n.statement LOAD byte:x\n.byte .x\n.endstatement\n.statement DORMANT byte:x\n.byte .x\n.endstatement\nWRAP .macro value\n LOAD .value\n.endmacro\n";
    result.push(Control {
        name: "template-inventory-lifecycle".into(),
        source: base
            .replace(".module dep\n.cpu 68020\n", &format!(".module dep\n.cpu 68020\n{definitions}"))
            .replace("entry .block\n.byte $11", "entry .block\n.ONCE\n.OUTER 4\n.STASH 6\n.EMPTY\n LOAD 5\n.WRAP 8\n.if 0\n.UNUSED\n.endif\n.byte $11"),
        includes: vec![],
    });
    result.push(Control {
        name: "template-inventory-include-floor".into(),
        source: base
            .replace(".module dep\n.cpu 68020\n", ".module dep\n.cpu 68020\nLOAD .macro file\n.include .file\n.LEAF 8\n.endmacro\nLEAF .macro value\n.byte .value\n.endmacro\n")
            .replace("entry .block\n.byte $11", "entry .block\n.LOAD \"consumer.inc\"\n.byte $11"),
        includes: vec![("consumer.inc", ".LEAF 7\n")],
    });
    result
}

pub(super) fn oracle(control: &Control) -> (Vec<u8>, Vec<u8>) {
    let dir = scratch();
    fs::create_dir_all(dir.0.join("build")).unwrap();
    let input = dir.0.join("entry.asm");
    fs::write(&input, &control.source).unwrap();
    for (path, contents) in &control.includes {
        fs::write(dir.0.join(path), contents).unwrap();
    }
    let cli = Cli::parse_from(["opforge".into(), input.to_string_lossy().into_owned()]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.0.clone());
    run_with_validated_cli_with_context(&cli, &config)
        .unwrap_or_else(|error| panic!("{}: {error:?}", control.name));
    (
        fs::read(dir.0.join("build/fragment.bin")).unwrap(),
        fs::read(dir.0.join("build/fragment.hunk")).unwrap(),
    )
}

#[test]
fn native_source_controls_live_rust_controls() {
    for control in controls() {
        let (binary, hunk) = oracle(&control);
        assert!(!binary.is_empty(), "{}", control.name);
        assert_eq!(hunk::segments(&hunk).unwrap().len(), 2, "{}", control.name);
    }
}

#[test]
#[ignore = "requires a prebuilt current CLI image and fresh FS-UAE"]
fn native_source_controls_prebuilt_fs_uae() {
    let mut controls = controls();
    if let Ok(filter) = std::env::var("OPFORGE_SOURCE_CONTROLS_CASE_FILTER") {
        controls.retain(|control| control.name == filter);
        assert!(
            !controls.is_empty(),
            "unknown source control case filter {filter}"
        );
    }
    run_controls(controls, true);
}

#[test]
#[ignore = "requires a prebuilt current CLI image and fresh FS-UAE; single-case transport control"]
fn native_source_template_boundary_single_fs_uae() {
    let control = controls()
        .into_iter()
        .find(|control| control.name == "imported-template-boundary")
        .expect("template boundary control");
    run_controls(vec![control], false);
}

fn run_controls(controls: Vec<Control>, breadth: bool) {
    let root = super::native_fragment_controls::root();
    let oracles: Vec<_> = controls.iter().map(oracle).collect();
    let core = vm::runtime_model_core::RuntimeModelCore::from_registry(
        &engine::build_default_asm_registry(),
    )
    .unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = crate::native_runtime_package::prepare_package(&core, &resolved).unwrap();
    let artifacts: Vec<_> = oracles
        .iter()
        .map(|oracle| {
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
        })
        .collect();
    let files: Vec<Vec<_>> = controls
        .iter()
        .map(|control| {
            let mut files = vec![
                OpforgeNativeCliGuestFile {
                    relative_path: "entry.asm",
                    bytes: control.source.as_bytes(),
                },
                OpforgeNativeCliGuestFile {
                    relative_path: "build/.keep",
                    bytes: b"",
                },
                OpforgeNativeCliGuestFile {
                    relative_path: "packages/m68020--motorola68k.bin",
                    bytes: &package,
                },
            ];
            files.extend(control.includes.iter().map(|(path, contents)| {
                OpforgeNativeCliGuestFile {
                    relative_path: path,
                    bytes: contents.as_bytes(),
                }
            }));
            files
        })
        .collect();
    let cases: Vec<_> = controls
        .iter()
        .zip(&artifacts)
        .zip(&files)
        .map(|((control, artifacts), files)| OpforgeNativeCliParityCase {
            name: &control.name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(control.source.as_bytes()),
            command_template: Some("Work:entry.asm -P Work:packages"),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: files,
            proof: OpforgeNativeCliProof::ExactArtifacts(artifacts),
        })
        .collect();
    let image = fs::read(
        std::env::var_os("OPFORGE_SOURCE_CONTROLS_IMAGE")
            .expect("set OPFORGE_SOURCE_CONTROLS_IMAGE to the current native CLI image"),
    )
    .expect("current native CLI image");
    for case in &cases {
        eprintln!(
            "SOURCE_CONTROL name={} identity={} image={} package={}",
            case.name,
            opforge_native_cli_case_identity(
                case,
                None,
                NativeCliParityExecutable::CompactCli,
                Some(&image)
            ),
            opforge_self_host_package_digest(&image),
            opforge_self_host_package_digest(&package)
        );
    }
    let results = if breadth {
        run_prebuilt_compact_cli_breadth_from_env(&root, &cases, &image, 4).unwrap()
    } else {
        let _guard = fs_uae_native_cli_smoke_lock().lock().unwrap();
        cases
            .iter()
            .map(|case| {
                let outcome = run_prebuilt_compact_cli_case_from_env(&root, case, &image).unwrap();
                let FsUaeSmokeOutcome::Completed { mut runs } = outcome else {
                    panic!("{}: native execution required", case.name);
                };
                assert_eq!(runs.len(), 1);
                BreadthCaseResult {
                    run: runs.pop(),
                    error: None,
                }
            })
            .collect()
    };
    assert_eq!(results.len(), cases.len());
    let mut failures = Vec::new();
    for (result, case) in results.iter().zip(&cases) {
        let Some(run) = result.run.as_ref() else {
            failures.push(format!(
                "{}: missing fresh guest run: {:?}",
                case.name, result.error
            ));
            continue;
        };
        assert_eq!(
            run.native_image_digest.as_deref(),
            Some(opforge_self_host_package_digest(&image).as_str()),
            "staged executable identity"
        );
        eprintln!(
            "SOURCE_CONTROL_RESULT name={} completed={} exit={:?} exact={}",
            case.name,
            run.protocol_completed,
            run.exit_code,
            run.success && result.error.is_none()
        );
        if !run.protocol_completed
            || !run.success
            || run.exit_code != Some(0)
            || result.error.is_some()
        {
            failures.push(format!("{}: {:?}\n{}", case.name, result.error, run.stdout));
        }
    }
    assert!(
        failures.is_empty(),
        "{} source controls failed:\n{}",
        failures.len(),
        failures.join("\n")
    );
}
