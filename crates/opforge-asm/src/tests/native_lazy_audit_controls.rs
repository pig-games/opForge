//! Live source controls and fresh exact-artifact proof for the opt-in L1 observer.
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
        name: "imported-audit-template-boundary".into(),
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
            ("lazy_audit.i", include_str!("../../../../native/motorola68000/amigaos/debug/lazy_audit.i")),
            ("lazy_work.i", include_str!("../../../../native/motorola68000/amigaos/debug/lazy_work.i")),
            ("depA.asm", ".module depA\n.cpu 68020\n.include \"lazy_audit.i\"\n.pub\n.section code,kind=code\nentry .block\n.byte $5a\n.bend\n.endsection\n.endmodule\n"),
            ("depB.asm", ".module depB\n.cpu 68020\n.include \"lazy_audit.i\"\n.pub\n.section code,kind=code\nentry .block\n.byte $5b\n.bend\n.endsection\n.endmodule\n"),
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
fn native_lazy_audit_live_rust_controls() {
    for control in controls() {
        let (binary, hunk) = oracle(&control);
        assert!(!binary.is_empty(), "{}", control.name);
        assert_eq!(hunk::segments(&hunk).unwrap().len(), 2, "{}", control.name);
    }
}

#[test]
#[ignore = "requires prebuilt reference/audit images and fresh FS-UAE"]
fn native_lazy_audit_prebuilt_fs_uae() {
    let root = super::native_fragment_controls::root();
    let mut controls = controls();
    if let Ok(filter) = std::env::var("OPFORGE_LAZY_CASE_FILTER") {
        controls.retain(|control| control.name == filter);
        assert!(
            !controls.is_empty(),
            "unknown lazy audit case filter {filter}"
        );
    }
    let oracles: Vec<_> = controls.iter().map(oracle).collect();
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
            command_template: Some("Work:entry.asm"),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: files,
            proof: OpforgeNativeCliProof::ExactArtifacts(artifacts),
        })
        .collect();
    let mut variables = vec!["OPFORGE_LAZY_REFERENCE_IMAGE", "OPFORGE_LAZY_AUDIT_IMAGE"];
    if std::env::var_os("OPFORGE_LAZY_PROBE_IMAGE").is_some() {
        variables.push("OPFORGE_LAZY_PROBE_IMAGE");
    }
    if std::env::var_os("OPFORGE_LAZY_WORK_IMAGE").is_some() {
        variables.push("OPFORGE_LAZY_WORK_IMAGE");
    }
    if let Ok(selected) = std::env::var("OPFORGE_LAZY_CHECK_ROLES") {
        let roles: Vec<_> = selected.split(',').collect();
        assert!(
            roles.iter().all(|role| variables.contains(role)),
            "select configured audit image roles"
        );
        variables.retain(|role| roles.contains(role));
        assert!(
            !variables.is_empty(),
            "select at least one audit image role"
        );
    }
    for variable in variables {
        let mut costed_work = std::collections::BTreeMap::new();
        let mut macro_work = std::collections::BTreeMap::new();
        let image =
            fs::read(std::env::var_os(variable).unwrap_or_else(|| panic!("set {variable}")))
                .unwrap();
        let results = run_prebuilt_compact_cli_breadth_from_env(&root, &cases, &image, 4).unwrap();
        assert_eq!(results.len(), cases.len());
        for (result, case) in results.iter().zip(&cases) {
            let run = result.run.as_ref().expect("fresh guest run");
            assert_eq!(
                run.native_image_digest.as_deref(),
                Some(opforge_self_host_package_digest(&image).as_str()),
                "staged executable identity"
            );
            assert!(
                run.protocol_completed && run.success && run.exit_code == Some(0),
                "{variable} {}: {}",
                case.name,
                run.stdout
            );
            assert!(result.error.is_none(), "{}: {:?}", case.name, result.error);
            if let Some(directory) = std::env::var_os("OPFORGE_LAZY_EVIDENCE_DIR") {
                let directory = PathBuf::from(directory).join(variable).join(case.name);
                assert!(!directory.starts_with(root.join("target")));
                fs::create_dir_all(&directory).unwrap();
                fs::write(directory.join("stdout.txt"), &run.stdout).unwrap();
                for (path, bytes) in &run.captured_artifacts {
                    if let Some(name) = path.file_name() {
                        fs::write(directory.join(name), bytes).unwrap();
                    }
                }
            }
            if variable != "OPFORGE_LAZY_REFERENCE_IMAGE"
                && std::env::var("OPFORGE_LAZY_CHECK_REPORT").as_deref() == Ok("1")
            {
                eprintln!(
                    "LAZY_AUDIT_REPORT image={variable} case={}\n{}",
                    case.name, run.stdout
                );
                let rows = super::native_lazy_audit_report::assert_report(&run.stdout);
                if variable == "OPFORGE_LAZY_WORK_IMAGE" {
                    assert!(rows.contains_key(&2000), "work image emits attribution");
                    assert!(
                        rows[&2000][2] <= 16,
                        "focused controls expose every work row"
                    );
                    if case.name == "whole-work-unreferenced" {
                        assert!(
                            rows[&2030].iter().any(|value| *value > 0),
                            "unused whole block incurred preparation"
                        );
                    }
                    if case.name == "whole-work-selected" {
                        assert!(
                            rows[&2020].iter().any(|value| *value > 0),
                            "selected whole block incurred preparation"
                        );
                    }
                    if case.name.starts_with("whole-work-") {
                        // The fixture's first discovered block is `costed` in
                        // dep; the root contains only its external reference.
                        assert_eq!(rows[&2060][0..2], [1, 1]);
                        assert_eq!(rows[&2060][2], 0, "proven ordinary ownership");
                        let vector: Vec<_> = (2061..=2066).flat_map(|phase| rows[&phase]).collect();
                        assert!(vector[0] > 0 && vector[1] > 0 && vector[13] > 0);
                        assert_eq!(
                            vector[7],
                            vector[9] + vector[10],
                            "ordinary costed body copies its tokens and lexemes once"
                        );
                        costed_work.insert(case.name, vector);
                    }
                    for family in ["zero-output-macro", "emitting-macro", "nested-macro"] {
                        if case.name.starts_with(&format!("{family}-")) {
                            // The first structural publication is costed. Its
                            // macro expansion inherits that active source owner.
                            assert_eq!(
                                rows[&2060],
                                [1, 1, 0],
                                "{}: macro caller ownership",
                                case.name
                            );
                            let vector: Vec<_> =
                                (2061..=2066).flat_map(|phase| rows[&phase]).collect();
                            assert!(
                                vector[3] > 0,
                                "{}: processing is charged to caller",
                                case.name
                            );
                            assert!(
                                rows[&2011][2] > 0,
                                "macro definitions retain shared templates"
                            );
                            assert_eq!(vector[5], 0, "shared macro definition precedes caller");
                            if family == "zero-output-macro" {
                                // The consumed EMPTY invocation still binds its
                                // plan and processes conditional records. A
                                // publication-only sample loses this work.
                                assert_eq!(vector[2], 1, "consumed macro plan binding");
                                assert_eq!(vector[3], 8, "consumed macro record processing");
                                assert_eq!(vector[4], 44, "consumed macro bound plan payload");
                            }
                            let selected = case.name.ends_with("-selected");
                            assert_eq!(rows[&1121][1], u32::from(selected), "costed selection");
                            macro_work.insert((family, selected), vector);
                        }
                    }
                    if case.name == "generated-block" || case.name == "active-include-resume" {
                        assert_ne!(rows[&2002][2], 0, "ambiguous provenance remains unknown");
                    }
                    if case.name == "generated-block" {
                        assert!(
                            (0..rows[&2001][2]).any(|index| rows[&(2060 + index * 7)][2] & 1 != 0),
                            "generated publication retains its explicit GENERATED reason"
                        );
                    }
                }
                if case.name == "generated-block" {
                    assert!(
                        rows[&710][1] > 0,
                        "generated block relationships are explicitly deferred"
                    );
                }
                if rows.contains_key(&3000) && case.name == "template-inventory-lifecycle" {
                    let definitions: Vec<Vec<_>> = (0..rows[&3002][0])
                        .map(|index| {
                            (3100 + 8 * index..3108 + 8 * index)
                                .flat_map(|phase| rows[&phase])
                                .collect()
                        })
                        .collect();
                    let epoch = definitions
                        .iter()
                        .map(|definition| definition[0])
                        .max()
                        .unwrap();
                    let current: Vec<_> = definitions
                        .iter()
                        .filter(|definition| definition[0] == epoch)
                        .collect();
                    assert_eq!(
                        current.len(),
                        9,
                        "registered definitions, including never invoked"
                    );
                    assert_eq!(
                        current.iter().map(|definition| definition[14]).sum::<u32>(),
                        8,
                        "accepted invocations include empty body"
                    );
                    for (field, expected) in [(15, 2), (16, 1), (22, 1)] {
                        assert_eq!(
                            current
                                .iter()
                                .map(|definition| definition[field])
                                .sum::<u32>(),
                            expected,
                            "nested/default consumers"
                        );
                    }
                    let unused: Vec<_> = current
                        .iter()
                        .filter(|definition| definition[14] == 0)
                        .collect();
                    assert_eq!(unused.len(), 3, "inactive call is not a consumer");
                    assert!(
                        unused.iter().any(|definition| definition[20] > 0),
                        "never-invoked body still incurs binding work"
                    );
                }
                if rows.contains_key(&3000) && case.name == "template-inventory-include-floor" {
                    let definitions: Vec<Vec<_>> = (0..rows[&3002][0])
                        .map(|index| {
                            (3100 + 8 * index..3108 + 8 * index)
                                .flat_map(|phase| rows[&phase])
                                .collect()
                        })
                        .collect();
                    let epoch = definitions
                        .iter()
                        .map(|definition| definition[0])
                        .max()
                        .unwrap();
                    let current: Vec<_> = definitions
                        .iter()
                        .filter(|definition| definition[0] == epoch)
                        .collect();
                    assert_eq!(current.len(), 2);
                    assert_eq!(
                        current.iter().map(|definition| definition[14]).sum::<u32>(),
                        3
                    );
                    assert_eq!(
                        current.iter().map(|definition| definition[15]).sum::<u32>(),
                        1,
                        "physical include is direct; resumed LOAD owns its nested call"
                    );
                }
            }
        }
        if let (Some(selected), Some(unreferenced)) = (
            costed_work.get("whole-work-selected"),
            costed_work.get("whole-work-unreferenced"),
        ) {
            assert_eq!(
                selected, unreferenced,
                "same whole body incurs identical preparation before selection"
            );
        }
        for family in ["zero-output-macro", "emitting-macro", "nested-macro"] {
            if let (Some(selected), Some(unreferenced)) = (
                macro_work.get(&(family, true)),
                macro_work.get(&(family, false)),
            ) {
                assert_eq!(
                    selected, unreferenced,
                    "{family}: identical body preparation before selection"
                );
            }
        }
    }
}
