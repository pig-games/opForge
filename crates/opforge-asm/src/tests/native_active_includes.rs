//! Active source acquisition uses live Rust file-tree oracles and fresh native proof.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;

const POSITIVE_CASE_NAMES: &[&str] = &[
    "active-expanded-includes",
    "nested-repeated-empty-expansion-includes",
    "inactive-malformed-loop-include",
    "source-and-binary-actions",
    "template-loop-acquisition",
    "physical-loop-hidden-acquisition",
    "scoped-loop-acquisition",
    "scoped-while-acquisition",
    "inactive-composed-block-reference",
    "ordinary-template-forward-count",
];
const POSITIVE_CPUS: &[&str] = &["68020", "6502"];

fn control_source() -> String {
    let mut source = String::from(
        ".module provider\n.cpu 68020\n.pub\nEMIT .macro value=7\n.byte .value\n.endmacro\n.endmodule\n.module app\n.cpu 68020\n.use provider (EMIT)\n",
    );
    for value in 0..1200 {
        source.push_str(&format!(".EMIT {}\n", value % 256));
    }
    source.push_str(".endmodule\n");
    source
}

#[test]
fn native_active_include_control_rust_oracle() {
    let source = control_source();
    let dir = scratch();
    let bytes = oracle(&dir.0, &source).expect("control Rust oracle");
    let expected = (0..1200)
        .map(|value| (value % 256) as u8)
        .collect::<Vec<_>>();
    assert_eq!(bytes, expected);
    if let Some(path) = std::env::var_os("OPFORGE_INCLUDE_CONTROL_SOURCE") {
        fs::write(path, source.as_bytes()).expect("write control source");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; active-include performance control"]
fn native_active_include_control_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TEMPLATE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    run_case(
        &root,
        &dir.0,
        &image,
        "active-include-control",
        "68020",
        &control_source(),
        false,
    );
}

pub(super) fn selected_names(variable: &str, available: &[&'static str]) -> Vec<&'static str> {
    let Some(value) = std::env::var_os(variable) else {
        return available.to_vec();
    };
    let value = value.to_string_lossy();
    let requested = value.split(',').map(str::trim).collect::<Vec<_>>();
    assert!(
        !requested.is_empty() && requested.iter().all(|name| !name.is_empty()),
        "{variable} must select at least one comma-separated value"
    );
    requested
        .into_iter()
        .map(|name| {
            available
                .iter()
                .copied()
                .find(|supported| *supported == name)
                .unwrap_or_else(|| {
                    panic!("unsupported {variable} value {name:?}; supported: {available:?}")
                })
        })
        .collect()
}

fn tree(cpu: &str) -> Vec<(&'static str, String)> {
    vec![
        ("entry.asm", format!(".module app\n.cpu {cpu}\n.use provider (LOAD as imported_load, INLINE as imported_inline)\n.if 0\n.include \"missing-branch.inc\"\n.endif\n.for 0\n.include \"missing-loop.inc\"\n.endfor\n.while 0\n.include \"missing-while.inc\"\n.endwhile\n.if 1\n.include \"active.inc\"\n.else\n.include \"missing-else.inc\"\n.endif\n.byte acquired\n.EMIT\nLOAD .macro file\n.include .file\n.endmacro\nINLINE .segment file\n.include .file\n.endsegment\n.LOAD \"macro.inc\"\n.INLINE \"segment.inc\"\n.for item in {{0,1}}\n.if item\n.include \"loop.inc\"\n.else\n.byte 0\n.if 0\n.include \"missing-iterator.inc\"\n.endif\n.endif\n.endfor\n.imported_load \"sibling.inc\"\n.imported_inline \"sibling.inc\"\n.endmodule\n")),
        ("active.inc", "acquired=11\nEMIT .macro\n.byte 12\n.endmacro\n.byte 10\n".into()),
        ("macro.inc", ".byte 13\n".into()),
        ("segment.inc", ".byte 14\n".into()),
        ("loop.inc", ".byte 15\n".into()),
        ("provider.asm", format!(".module provider\n.cpu {cpu}\n.pub\n.include \"templates/definitions.inc\"\n.endmodule\n")),
        ("templates/definitions.inc", "LOAD .macro file\n.include .file\n.endmacro\nINLINE .segment file\n.include .file\n.endsegment\n".into()),
        ("templates/sibling.inc", ".byte 16\n".into()),
        // A caller-relative resolution would produce different bytes.
        ("sibling.inc", ".byte 99\n".into()),
    ]
}

fn edge_tree(cpu: &str) -> Vec<(&'static str, String)> {
    vec![
        ("entry.asm", format!(".module app\n.cpu {cpu}\n.if 1\n.include \"edge/root.inc\"\n.endif\n.endmodule\n")),
        ("edge/root.inc", ".if 0\n.module unused\n.use absent\n.endif\nUNUSED .macro\n.include \"missing-unused.inc\"\n.endmacro\n.byte 20\nEMPTY .macro\n.endmacro\nOUTER .macro\n.EMPTY\n.include \"nested.inc\"\n.byte 21\n.endmacro\n.OUTER\n.include \"nested.inc\"\n.byte 23\n".into()),
        ("edge/nested.inc", ".if 1\n.include \"leaf.inc\"\n.endif\n".into()),
        ("edge/leaf.inc", ".byte 22\n".into()),
    ]
}

fn malformed_inactive_loop_include_tree(cpu: &str) -> Vec<(&'static str, String)> {
    vec![(
        "entry.asm",
        format!(
            ".module app\n.cpu {cpu}\n.for 1\n.if 0\n.include 123\n.endif\n.byte 24\n.endfor\n.endmodule\n"
        ),
    )]
}

fn pc_bounded_while_include_tree(cpu: &str) -> Vec<(&'static str, String)> {
    vec![
        (
            "entry.asm",
            format!(
                ".module app\n.cpu {cpu}\n.for 1\n.if 0\n.include 123\n.endif\n.byte 24\n.endfor\n.while $ < 2\n.include \"while.inc\"\n.endwhile\n.endmodule\n"
            ),
        ),
        ("while.inc", ".byte 25\n".into()),
    ]
}

type SourceTree = Vec<(&'static str, String)>;
type PositiveCase = (&'static str, SourceTree, Vec<u8>);

fn positive_cases(cpu: &str) -> Vec<PositiveCase> {
    vec![
        (
            "active-expanded-includes",
            tree(cpu),
            vec![10, 11, 12, 13, 14, 0, 15, 16, 16],
        ),
        (
            "nested-repeated-empty-expansion-includes",
            edge_tree(cpu),
            vec![20, 22, 21, 22, 23],
        ),
        (
            "inactive-malformed-loop-include",
            malformed_inactive_loop_include_tree(cpu),
            vec![24],
        ),
        (
            "source-and-binary-actions",
            vec![
                ("entry.asm", format!(".module app\n.cpu {cpu}\n.if 1\n.include \"parts/part.inc\"\n.endif\n.endmodule\n")),
                ("parts/part.inc", "LOAD .macro file\n.incbin .file\n.endmacro\n.LOAD \"assets.bin\"\n.byte 27\n".into()),
                ("parts/assets.bin", "ab".into()),
                // Detect caller-relative binary acquisition as well.
                ("assets.bin", "cd".into()),
            ],
            vec![b'a', b'b', 27],
        ),
        (
            "template-loop-acquisition",
            vec![
                ("entry.asm", format!(".module app\n.cpu {cpu}\n.include \"templates/loops.inc\"\n.REPEAT 2,\"data.inc\"\n.REPEAT 0,\"missing.inc\"\n.SELECT \"data.inc\"\n.endmodule\n")),
                ("templates/loops.inc", "REPEAT .macro times,file\n.for .times\n.include .file\n.endfor\n.endmacro\nSELECT .segment file\n.for item in {0,1}\n.if item\n.include .file\n.else\n.byte 0\n.endif\n.endfor\n.endsegment\n".into()),
                ("templates/data.inc", ".byte 31\n".into()),
                ("data.inc", ".byte 99\n".into()),
            ],
            vec![31, 31, 0, 31],
        ),
        (
            "physical-loop-hidden-acquisition",
            vec![
                ("entry.asm", format!(".module app\n.cpu {cpu}\nLOAD .macro file\n.include .file\n.endmacro\n.for item in {{0,1,2}}\n.if item\n.LOAD \"leaf.inc\"\n.else\n.byte 0\n.endif\n.endfor\n.for 0\n.LOAD \"missing.inc\"\n.endfor\n.endmodule\n")),
                ("leaf.inc", ".byte item+34\n".into()),
            ],
            vec![0, 35, 36],
        ),
        (
            "scoped-loop-acquisition",
            vec![
                ("entry.asm", format!(".module app\n.cpu {cpu}\n.bfor index in {{1,2}}\n.include \"scoped.inc\"\n.endfor\n.bfor 0\n.include \"missing.inc\"\n.endfor\n.endmodule\n")),
                ("scoped.inc", "value=index+40\n.byte value\n".into()),
            ],
            vec![41, 42],
        ),
        (
            "ordinary-template-forward-count",
            vec![("entry.asm", format!(".module app\n.cpu {cpu}\nEMIT .macro value\n.byte .value\n.endmacro\n.for later\n.EMIT 43\n.endfor\nlater=2\n.endmodule\n"))],
            vec![43, 43],
        ),
        (
            "inactive-composed-block-reference",
            vec![
                ("entry.asm", format!(".module app\n.cpu {cpu}\n.use provider\nSCAN .macro suffix\n.for 1\n.if 0\n.byte provider.routine.saved@1.value\n.endif\n.include \"leaf.inc\"\n.endfor\n.endmacro\n.SCAN 1\n.endmodule\n")),
                ("provider.asm", format!(".module provider\n.cpu {cpu}\n.pub\nroutine .block\nInfo .struct\nvalue .byte ?\n.endstruct\nsaved1 .const Info {{value:66}}\n.byte 99\n.bend\n.endmodule\n")),
                ("leaf.inc", ".byte 60\n".into()),
            ],
            vec![60],
        ),
        (
            "scoped-while-acquisition",
            vec![
                ("entry.asm", format!(".module app\n.cpu {cpu}\nn .var 0\n.bwhile n<2\n.include \"while.inc\"\napp.n .set app.n+1\n.endwhile\n.bwhile 0\n.include \"missing.inc\"\n.endwhile\n.byte n\n.endmodule\n")),
                ("while.inc", "value .const n+44\n.byte value\n".into()),
            ],
            vec![44, 45, 2],
        ),
    ]
}

#[test]
fn native_active_includes_pc_bounded_while_rust_oracle() {
    // Rust-only coverage: native execution-stream traversal remains pending.
    for cpu in selected_names("OPFORGE_INCLUDE_CPUS", POSITIVE_CPUS) {
        let dir = scratch();
        assert_eq!(
            live_oracle(&dir.0, &pc_bounded_while_include_tree(cpu)),
            [24, 25, 25],
            "pc-bounded while include cpu={cpu}"
        );
    }
}

fn oracle_result(dir: &Path, files: &[(&str, String)]) -> Result<Vec<u8>, String> {
    for (path, text) in files {
        let path = dir.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, text).unwrap();
    }
    oracle(dir, &files[0].1)
}

pub(super) fn live_oracle(dir: &Path, files: &[(&str, String)]) -> Vec<u8> {
    oracle_result(dir, files).expect("live active-include Rust oracle")
}

fn failure_trees() -> Vec<(&'static str, Vec<(&'static str, String)>)> {
    let entry = |body: &str| format!(".module app\n.cpu 68020\n{body}.endmodule\n");
    vec![
        (
            "active-missing-file",
            vec![(
                "entry.asm",
                entry(".if 1\n.include \"missing-active.inc\"\n.endif\n"),
            )],
        ),
        (
            "active-include-cycle",
            vec![
                (
                    "entry.asm",
                    entry(".if 1\n.include \"cycle.inc\"\n.endif\n"),
                ),
                ("cycle.inc", ".include \"cycle.inc\"\n".into()),
            ],
        ),
        (
            "forward-include-guard",
            vec![
                (
                    "entry.asm",
                    entry(".if later\n.include \"forward.inc\"\n.endif\nlater=1\n"),
                ),
                ("forward.inc", ".byte 7\n".into()),
            ],
        ),
        (
            "acquired-module-change",
            vec![
                (
                    "entry.asm",
                    entry(".if 1\n.include \"module.inc\"\n.endif\n"),
                ),
                ("module.inc", ".module other\n".into()),
            ],
        ),
        (
            "acquired-use-change",
            vec![
                ("entry.asm", entry(".if 1\n.include \"use.inc\"\n.endif\n")),
                ("use.inc", ".use absent\n".into()),
            ],
        ),
        (
            "expanded-module-change",
            vec![(
                "entry.asm",
                entry("CHANGE .macro\n.module other\n.endmacro\n.CHANGE\n"),
            )],
        ),
        (
            "expanded-use-change",
            vec![(
                "entry.asm",
                entry("CHANGE .macro\n.use absent\n.endmacro\n.CHANGE\n"),
            )],
        ),
    ]
}

#[test]
fn native_active_includes_failure_rust_oracles() {
    let cases = failure_trees();
    let available = cases.iter().map(|(name, _)| *name).collect::<Vec<_>>();
    let selected = selected_names("OPFORGE_INCLUDE_FAILURE_CASES", &available);
    let mut attempted = 0;
    for (name, files) in cases {
        if !selected.contains(&name) {
            continue;
        }
        attempted += 1;
        let dir = scratch();
        assert!(
            oracle_result(&dir.0, &files).is_err(),
            "Rust accepted {name}"
        );
    }
    assert!(
        attempted > 0,
        "no active-include failure cases were attempted"
    );
}

#[test]
fn native_active_includes_rust_oracles() {
    let selected_cases = selected_names("OPFORGE_INCLUDE_CASES", POSITIVE_CASE_NAMES);
    let selected_cpus = selected_names("OPFORGE_INCLUDE_CPUS", POSITIVE_CPUS);
    let mut attempted = 0;
    for cpu in selected_cpus {
        for (name, files, expected) in positive_cases(cpu) {
            if !selected_cases.contains(&name) {
                continue;
            }
            attempted += 1;
            let dir = scratch();
            assert_eq!(live_oracle(&dir.0, &files), expected, "{name} cpu={cpu}");
        }
    }
    assert!(
        attempted > 0,
        "no active-include positive cases were attempted"
    );
}

pub(super) fn run_binary_files(
    root: &Path,
    image: &[u8],
    name: &str,
    cpu: &str,
    files: &[(&str, String)],
    expected_bytes: &[u8],
) {
    let oracle_dir = scratch();
    let rust_oracle = live_oracle(&oracle_dir.0, files);
    assert_eq!(rust_oracle, expected_bytes, "{name} cpu={cpu}");
    let guest_files = files
        .iter()
        .map(|(path, text)| OpforgeNativeCliGuestFile {
            relative_path: path,
            bytes: text.as_bytes(),
        })
        .collect::<Vec<_>>();
    let case = OpforgeNativeCliParityCase {
        name,
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(files[0].1.as_bytes()),
        command_template: Some("Work:entry.asm --bin Work:out.bin"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &guest_files,
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/out.bin",
            rust_oracle: &rust_oracle,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(root, &case, image)
        .expect("fresh active-include CLI completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(
        runs[0].protocol_completed,
        "{cpu}: missing fresh completion"
    );
    assert!(runs[0].success, "{cpu}: {}", runs[0].stdout);
    assert_eq!(runs[0].exit_code, Some(0));
    eprintln!(
        "ACTIVE_INCLUDE_CASE name={name} cpu={cpu} seconds={:?} image_bytes={}",
        runs[0].start_to_done_host_seconds,
        image.len()
    );
}

#[test]
#[ignore = "requires configured FS-UAE; active expanded source acquisition"]
fn native_active_includes_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TEMPLATE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let selected_cases = selected_names("OPFORGE_INCLUDE_CASES", POSITIVE_CASE_NAMES);
    let selected_cpus = selected_names("OPFORGE_INCLUDE_CPUS", POSITIVE_CPUS);
    let mut attempted = 0;
    let mut failures = Vec::new();
    for cpu in selected_cpus {
        for (name, files, expected_bytes) in positive_cases(cpu) {
            if !selected_cases.contains(&name) {
                continue;
            }
            attempted += 1;
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_binary_files(&root, &image, name, cpu, &files, &expected_bytes);
            }));
            if result.is_err() {
                failures.push(format!("{name} cpu={cpu}"));
            }
        }
    }
    assert!(
        attempted > 0,
        "no active-include native cases were attempted"
    );
    assert!(
        failures.is_empty(),
        "active include native proof failures: {failures:?}"
    );
}

pub(super) fn run_rejected_files(root: &Path, image: &[u8], name: &str, files: &[(&str, String)]) {
    let oracle_dir = scratch();
    assert!(
        oracle_result(&oracle_dir.0, files).is_err(),
        "Rust accepted {name}"
    );
    let guest_files = files
        .iter()
        .map(|(path, text)| OpforgeNativeCliGuestFile {
            relative_path: path,
            bytes: text.as_bytes(),
        })
        .collect::<Vec<_>>();
    let case = OpforgeNativeCliParityCase {
        name,
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(files[0].1.as_bytes()),
        command_template: Some("Work:entry.asm --bin Work:out.bin"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &guest_files,
        proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(root, &case, image)
        .expect("fresh active-include rejection completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(
        runs[0].protocol_completed,
        "{name}: missing fresh completion"
    );
    assert!(!runs[0].success, "{name}: native unexpectedly succeeded");
    assert_eq!(runs[0].exit_code, Some(20));
    eprintln!(
        "ACTIVE_INCLUDE_FAILURE name={name} seconds={:?}",
        runs[0].start_to_done_host_seconds
    );
}

#[test]
#[ignore = "requires configured FS-UAE; active source acquisition rejection"]
fn native_active_includes_failures_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TEMPLATE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let cases = failure_trees();
    let available = cases.iter().map(|(name, _)| *name).collect::<Vec<_>>();
    let selected = selected_names("OPFORGE_INCLUDE_FAILURE_CASES", &available);
    let mut attempted = 0;
    let mut failures = Vec::new();
    for (name, files) in cases {
        if !selected.contains(&name) {
            continue;
        }
        attempted += 1;
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            run_rejected_files(&root, &image, name, &files);
        }));
        if result.is_err() {
            failures.push(name);
        }
    }
    assert!(
        attempted > 0,
        "no active-include native failure cases were attempted"
    );
    assert!(
        failures.is_empty(),
        "active include rejection failures: {failures:?}"
    );
}
