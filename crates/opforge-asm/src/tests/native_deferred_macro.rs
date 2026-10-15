//! First-use descriptor remapping must preserve eager declaration/name binding.
use super::native_iterable_loops::{oracle, scratch};
use super::*;

struct Control {
    name: &'static str,
    cpu: &'static str,
    source: String,
    files: Vec<(&'static str, &'static str)>,
    expected: Option<&'static [u8]>,
}

fn controls() -> Vec<Control> {
    let mut controls = Vec::new();
    for (name, calls, expected) in [
        ("unused", "", &[7][..]),
        ("repeated", ".ONE\n.ONE\n", &[1, 1, 7][..]),
    ] {
        controls.push(Control {
            name,
            cpu: "68020",
            source: format!(".module main\n.cpu 68020\nONE .macro\n.byte 1\n.endmacro\n{calls}.byte 7\n.endmodule\n"),
            files: vec![],
            expected: Some(expected),
        });
    }
    controls.push(Control {
        name: "declaration-values-and-shadowing",
        cpu: "68020",
        source: ".module main\n.cpu 68020\nvalue .var 7\nEMIT .macro argument=3\n.byte value,.argument\n.endmacro\nvalue := 9\n.namespace caller\nvalue .const 18\n.EMIT\n.EMIT 5\n.endnamespace\n.EMIT 6\n.endmodule\n".into(),
        files: vec![],
        expected: None,
    });
    controls.push(Control {
        name: "included-definition-after-slice-and-child-include",
        cpu: "68020",
        source: ".module main\n.cpu 68020\n.include \"defs/declare.inc\"\n.include \"other.inc\"\n.EMIT\n.LOAD\n.EMIT 5\n.endmodule\n".into(),
        files: vec![
            ("defs/declare.inc", "EMIT .macro value=3\n.byte .value\n.endmacro\nLOAD .macro\n.include \"child.inc\"\n.EMIT 8\n.endmacro\n"),
            ("defs/child.inc", ".EMIT 6\n"),
            ("other.inc", ".byte 2\n"),
        ],
        expected: Some(&[2, 3, 6, 8, 5]),
    });
    controls.push(Control {
        name: "quoted-substitution-preserves-argument-trivia",
        cpu: "68020",
        source: ".module main\n.cpu 68020\nEMIT .macro value\n.byte \"@1\",0 ; retain argument spelling\n.endmacro\n.EMIT 1+2\n.EMIT 1 + 2\n.EMIT 1\t+\t2\n.endmodule\n".into(),
        files: vec![],
        expected: Some(b"1+2\x001 + 2\x001\t+\t2\0"),
    });
    controls.push(Control {
        name: "unused-selected-nested-call",
        cpu: "68020",
        source: concat!(
            ".module main\n.cpu 68020\n",
            "INNER .macro value=3\n.byte .value\n.endmacro\n",
            "OUTER .macro\n.INNER\n.INNER 5\n.endmacro\n",
            ".byte 7\n.endmodule\n",
        )
        .into(),
        files: vec![],
        expected: Some(&[7]),
    });
    controls.push(Control {
        name: "selected-nested-call-first-use-and-reuse",
        cpu: "68020",
        source: concat!(
            ".module main\n.cpu 68020\n",
            "INNER .macro value=3\n.byte .value\n.endmacro\n",
            "OUTER .macro\n.INNER\n.INNER 5\n.endmacro\n",
            ".OUTER\n.OUTER\n.endmodule\n",
        )
        .into(),
        files: vec![],
        expected: Some(&[3, 5, 3, 5]),
    });
    controls.push(Control {
        name: "nested-body-caller-context-defaults-and-trivia",
        cpu: "68020",
        source: concat!(
            ".module main\n.cpu 68020\nvalue .const 7\n",
            "TEXT .macro argument=3\n.byte value,.argument\n.byte \"@1\",0\n.endmacro\n",
            "OUTER .macro argument=5\n.TEXT .argument\n.TEXT 1\t+\t2\n.endmacro\n",
            ".namespace caller\nvalue .const 18\n.OUTER\n.OUTER 6\n.endnamespace\n.endmodule\n",
        )
        .into(),
        files: vec![],
        // Ordinary value names resolve in the expansion namespace. Keep the
        // caller shadow in the oracle rather than assuming early value binding.
        expected: Some(b"\x12\x055\0\x12\x031\t+\t2\0\x12\x066\0\x12\x031\t+\t2\0"),
    });
    for cpu in ["68020", "6502"] {
        controls.push(Control {
            name: "existing-macro-language-matrix",
            cpu,
            source: super::native_macro_parity::combined_positive_source(cpu),
            files: vec![],
            expected: None,
        });
    }
    controls
}

// Rust supports changing the expansion CPU. The compact native capsule remains
// one selected pipeline: binary_assembly.cpu rejects any different CpuName.
// Keep this scope gap separate from positive spelling-representation controls.
fn cross_cpu_control() -> Control {
    Control {
        name: "declaration-cpu-and-expansion-cpu",
        cpu: "68020",
        source:
            ".module main\n.cpu 68020\nEMIT .macro\n nop\n.endmacro\n.cpu 6502\n.EMIT\n.endmodule\n"
                .into(),
        files: vec![],
        expected: Some(&[0xea]),
    }
}

fn live_oracle(control: &Control) -> Vec<u8> {
    let dir = scratch();
    for (path, contents) in &control.files {
        let path = dir.0.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }
    let bytes =
        oracle(&dir.0, &control.source).unwrap_or_else(|error| panic!("{}: {error}", control.name));
    if let Some(expected) = control.expected {
        assert_eq!(bytes, expected, "{}", control.name);
    }
    bytes
}

#[test]
fn native_deferred_macro_rust_controls() {
    for control in controls() {
        assert!(!live_oracle(&control).is_empty());
    }
}

#[test]
fn native_deferred_macro_cross_cpu_rust_oracle() {
    assert_eq!(live_oracle(&cross_cpu_control()), [0xea]);
}

#[test]
#[ignore = "requires prebuilt eager/deferred images and fresh FS-UAE"]
fn native_deferred_macro_prebuilt_fs_uae() {
    run_prebuilt_controls(controls());
}

fn run_prebuilt_controls(mut controls: Vec<Control>) {
    if let Ok(filter) = std::env::var("OPFORGE_DEFERRED_CASE") {
        controls.retain(|control| control.name == filter);
        assert!(!controls.is_empty(), "unknown control {filter}");
    }
    let oracles: Vec<_> = controls.iter().map(live_oracle).collect();
    let artifacts: Vec<_> = oracles
        .iter()
        .map(|bytes| {
            [OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/out.bin",
                rust_oracle: bytes,
            }]
        })
        .collect();
    let files: Vec<Vec<_>> = controls
        .iter()
        .map(|control| {
            std::iter::once(("entry.asm", control.source.as_str()))
                .chain(control.files.iter().map(|(path, text)| (*path, *text)))
                .map(|(path, bytes)| OpforgeNativeCliGuestFile {
                    relative_path: path,
                    bytes: bytes.as_bytes(),
                })
                .collect()
        })
        .collect();
    let cases: Vec<_> = controls
        .iter()
        .enumerate()
        .map(|(index, control)| OpforgeNativeCliParityCase {
            name: control.name,
            // The executable runs on 68020; the source selects its target package.
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(control.source.as_bytes()),
            command_template: Some("Work:entry.asm --bin Work:out.bin"),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &files[index],
            proof: OpforgeNativeCliProof::ExactArtifacts(&artifacts[index]),
        })
        .collect();
    let root = super::native_fragment_controls::root();
    let mut attempted = 0;
    for variable in [
        "OPFORGE_DEFERRED_REFERENCE_IMAGE",
        "OPFORGE_DEFERRED_IMAGE",
        "OPFORGE_DEFERRED_PROBE_IMAGE",
    ] {
        let Some(path) = std::env::var_os(variable) else {
            continue;
        };
        attempted += 1;
        let image = fs::read(path).unwrap();
        let results = run_prebuilt_compact_cli_breadth_from_env(&root, &cases, &image, 4).unwrap();
        assert_eq!(results.len(), cases.len());
        let mut unused_created = None;
        let mut repeated_created = None;
        let mut unused_lines_created = None;
        let mut repeated_lines_created = None;
        for (control, result) in controls.iter().zip(results) {
            let run = result.run.unwrap_or_else(|| {
                panic!(
                    "{variable} {} cpu={}: {:?}",
                    control.name, control.cpu, result.error
                )
            });
            assert!(
                run.protocol_completed && run.success && run.exit_code == Some(0),
                "{variable} {}: {}",
                control.name,
                run.stdout
            );
            assert!(
                result.error.is_none(),
                "{}: {:?}",
                control.name,
                result.error
            );
            assert_eq!(
                run.native_image_digest.as_deref(),
                Some(opforge_self_host_package_digest(&image).as_str())
            );
            if let Some(directory) = std::env::var_os("OPFORGE_DEFERRED_EVIDENCE_DIR") {
                let directory = PathBuf::from(directory)
                    .join(variable)
                    .join(format!("{}-{}", control.name, control.cpu));
                assert!(!directory.starts_with(root.join("target")));
                fs::create_dir_all(&directory).unwrap();
                fs::write(directory.join("stdout.txt"), &run.stdout).unwrap();
                fs::write(directory.join("source.asm"), &control.source).unwrap();
                for (path, bytes) in &run.captured_artifacts {
                    if let Some(name) = path.file_name() {
                        fs::write(directory.join(name), bytes).unwrap();
                    }
                }
            }
            if variable == "OPFORGE_DEFERRED_IMAGE"
                && std::env::var("OPFORGE_DEFERRED_CHECK_ACQUISITION_STATS").as_deref() == Ok("1")
            {
                let rows = super::native_lazy_audit_report::rows(&run.stdout);
                let [created, bound, reserved] = rows[&450002];
                let [plans_created, plans_bound, _] = rows[&450000];
                assert_eq!(reserved, 0);
                assert!(bound <= created && created <= plans_created && bound <= plans_bound);
                match control.name {
                    "unused-selected-nested-call" => {
                        assert_eq!([created, bound, reserved], [2, 0, 0])
                    }
                    "selected-nested-call-first-use-and-reuse" => {
                        assert_eq!([created, bound, reserved], [2, 2, 0])
                    }
                    _ => {}
                }
            }
            if variable == "OPFORGE_DEFERRED_IMAGE"
                && std::env::var("OPFORGE_DEFERRED_CHECK_LINE_STATS").as_deref() == Ok("1")
            {
                let rows = super::native_lazy_audit_report::rows(&run.stdout);
                let [created, bound, reserved] = rows[&450003];
                let [plans_created, plans_bound, _] = rows[&450000];
                assert_eq!(reserved, 0);
                assert!(bound <= created && created <= plans_created && bound <= plans_bound);
                // Both controls have the same declaration; invocation must not
                // recreate its recipes, and only the scalar body line binds.
                match control.name {
                    "unused" => {
                        assert!(created > 0);
                        assert_eq!(bound, 0);
                        unused_lines_created = Some(created);
                    }
                    "repeated" => {
                        assert_eq!(bound, 1);
                        repeated_lines_created = Some(created);
                    }
                    _ => {}
                }
            }
            if variable == "OPFORGE_DEFERRED_IMAGE"
                && std::env::var("OPFORGE_DEFERRED_CHECK_STATS").as_deref() == Ok("1")
            {
                let rows = super::native_lazy_audit_report::rows(&run.stdout);
                let [created, bound, _] = rows[&450000];
                assert!(bound <= created);
                if control.name == "unused" {
                    assert!(created > 0);
                    assert_eq!(bound, 0);
                    unused_created = Some(created);
                }
                if control.name == "repeated" {
                    assert_eq!(bound, 1);
                    repeated_created = Some(created);
                }
            }
        }
        if let (Some(unused), Some(repeated)) = (unused_lines_created, repeated_lines_created) {
            assert_eq!(
                unused, repeated,
                "invocations do not recapture whole-line recipes"
            );
        }
        if let (Some(unused), Some(repeated)) = (unused_created, repeated_created) {
            assert_eq!(
                unused, repeated,
                "invocations do not recapture declaration recipes"
            );
        }
    }
    assert!(attempted > 0, "set a prebuilt eager/deferred image");
}

// Rust permits an unused malformed nested call, but rejects it on first use.
// Existing eager native acquisition rejects both states. Preserve that native
// behavior in the candidate, while recording unused acceptance as a known
// Rust/native non-equivalence rather than claiming positive artifact parity.
// Each native failure still requires fresh completion and a diagnostic.
const INVALID_UNUSED_NESTED_CALL: &str = concat!(
    ".module main\n.cpu 68020\n",
    "INNER .macro left,right\n.byte .left,.right\n.endmacro\n",
    "UNUSED .macro\n.INNER(1,2\n.endmacro\n.byte 7\n.endmodule\n",
);

#[test]
fn native_deferred_macro_invalid_nested_call_rust_oracle() {
    let dir = scratch();
    assert_eq!(oracle(&dir.0, INVALID_UNUSED_NESTED_CALL).unwrap(), [7]);
    let selected =
        INVALID_UNUSED_NESTED_CALL.replace(".byte 7\n.endmodule", ".byte 7\n.UNUSED\n.endmodule");
    assert!(oracle(&dir.0, &selected).is_err());
}

#[test]
#[ignore = "requires prebuilt macro experiment images and fresh FS-UAE"]
fn native_deferred_macro_invalid_nested_call_prebuilt_fs_uae() {
    let selected =
        INVALID_UNUSED_NESTED_CALL.replace(".byte 7\n.endmodule", ".byte 7\n.UNUSED\n.endmodule");
    let root = super::native_fragment_controls::root();
    let cases: Vec<_> = [
        (
            "unused-invalid-selected-nested-call",
            INVALID_UNUSED_NESTED_CALL,
        ),
        ("invoked-invalid-selected-nested-call", selected.as_str()),
    ]
    .into_iter()
    .map(|(name, source)| OpforgeNativeCliParityCase {
        name,
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some("Work:entry.asm --bin Work:out.bin"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
    })
    .collect();
    let mut attempted = 0;
    for variable in ["OPFORGE_DEFERRED_REFERENCE_IMAGE", "OPFORGE_DEFERRED_IMAGE"] {
        let Some(path) = std::env::var_os(variable) else {
            continue;
        };
        attempted += 1;
        let image = fs::read(path).unwrap();
        let results = run_prebuilt_compact_cli_breadth_from_env(&root, &cases, &image, 4).unwrap();
        assert_eq!(results.len(), cases.len());
        for (case, result) in cases.iter().zip(results) {
            assert!(
                result.error.is_none(),
                "{variable} {}: {:?}",
                case.name,
                result.error
            );
            let run = result
                .run
                .as_ref()
                .expect("fresh native failure completion");
            assert!(
                run.protocol_completed && run.exit_code.is_some_and(|exit| exit != 0),
                "{variable} {}: {}",
                case.name,
                run.stdout
            );
        }
    }
    assert!(attempted > 0, "set a prebuilt eager/deferred image");
}
