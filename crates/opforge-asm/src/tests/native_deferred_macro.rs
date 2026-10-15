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
#[ignore = "requires prebuilt eager/deferred images and fresh FS-UAE"]
fn native_deferred_macro_prebuilt_fs_uae() {
    let mut controls = controls();
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
                && std::env::var("OPFORGE_DEFERRED_CHECK_STATS").as_deref() == Ok("1")
            {
                let rows = super::native_lazy_audit_report::rows(&run.stdout);
                let [created, bound, _] = rows[&3300];
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
        if let (Some(unused), Some(repeated)) = (unused_created, repeated_created) {
            assert_eq!(
                unused, repeated,
                "invocations do not recapture declaration recipes"
            );
        }
    }
    assert!(attempted > 0, "set a prebuilt eager/deferred image");
}
