//! Iterable loops use each case's live Rust oracle.
use super::*;
use crate::native_package_build::{build_native_packages, EmbedSelection};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

pub(super) struct Scratch(pub(super) PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
pub(super) fn scratch() -> Scratch {
    static NEXT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let serial = NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let path = std::env::temp_dir().join(format!(
        "opforge-iterable-{}-{}-{serial}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&path).unwrap();
    Scratch(path)
}

const POSITIVE: &[(&str, &str, &[u8])] = &[
    ("contextual-name", "in=23\n.for in in {2,4}\n.byte in\n.endfor\n.byte in\n", &[2,4,23]),
    ("register-name", ".for a in {1,2}\n.byte a\n.endfor\n", &[1,2]),
    ("lexical-parent", "items={9,11}\nroutine .block\n.for items in {2,4}\n.byte items\n.endfor\n.byte items[1]\n.bend\n.word routine\n", &[2,4,11,0,0]),
    ("lists", "items={2,4,6}\n.for item in items\n.byte item+1\n.endfor\n.for item in {7,9}\n.byte item*2\n.endfor\n", &[3,5,7,14,18]),
    ("ranges", "span=2..=8:3\n.for item in span\n.byte item\n.endfor\ndescending=7..1:-2\n.for item in descending\n.byte item\n.endfor\n.for item in 7..1:-2\n.byte item\n.endfor\n.for item in 1..4\n.byte item\n.endfor\n", &[2,5,8,7,5,3,7,5,3,1,2,3]),
    ("binding-restoration", "item=19\n.for item in {2,4}\n.byte item\n.for item in {7,8}\n.byte item\n.endfor\n.byte item\n.endfor\n.byte item\n", &[2,7,8,2,4,7,8,4,19]),
    ("outer-rhs", "items={2,4}\n.for items in items\n.byte items\n.for items in {items+1}\n.byte items\n.endfor\n.byte items\n.endfor\n.byte items[0]\n", &[2,3,2,4,5,4,2]),
    ("compound-shadow", "items={8,9}\n.for items in {2,4}\n.byte items+1\n.endfor\n.byte items[0]\n", &[3,5,8]),
    ("named-snapshot", "items .var {2,4}\nsaved=items\nitems .set {9,10,11}\n.for item in saved\n.byte item+1\n.endfor\n.for item in items\n.byte item\n.endfor\n", &[3,5,9,10,11]),
    ("nested-count-empty", "item=23\n.for item in {}\n.byte missing\n.endfor\n.for item in 4..4\n.byte missing\n.endfor\n.for item in {2,3}\n.for item\n.byte item+10\n.endfor\n.endfor\n.byte item\n", &[12,12,13,13,13,23]),
    ("iterator-branches", ".for item in {0,1,2}\n.if item==0\n.byte 10\n.elseif item==1\n.byte 11\n.else\n.byte 12\n.endif\n.endfor\n", &[10, 11, 12]),
    ("nested-iterator-conditions", "outer=9\n.for outer in {1,2}\n.for inner in {2,1}\n.if outer==inner\n.byte 20+outer\n.else\n.byte 30+inner\n.endif\n.endfor\n.endfor\n.byte outer\n", &[32, 21, 22, 31, 9]),
    ("inactive-conditional-loops", ".for item in {1,2}\n.if item==0\n.for 1/0\n.byte missing\n.endfor\n.else\n.for 2\n.if item==1\n.byte 3\n.else\n.byte 4\n.endif\n.endfor\n.endif\n.endfor\n", &[3, 3, 4, 4]),
    ("conditional-short-circuit", ".for item in {1,2}\n.if item\n.byte item\n.elseif 1/0\n.byte missing\n.else\n.byte 99\n.endif\n.if item==0\n.byte 1/0\n.endif\n.endfor\n", &[1, 2]),
    ("empty-conditional-loop", ".for item in {}\n.if missing\n.byte 99\n.else\n.byte 1/0\n.endif\n.endfor\n.byte 7\n", &[7]),
    ("macro-iterator-condition", "pick .macro value\n.if .value==2\n.byte .value+10\n.else\n.byte .value+20\n.endif\n.endmacro\n.for item in {2,4}\n.pick item\n.endfor\n", &[12, 24]),
    ("conditional-parent-restoration", ".if 1\n.for item in {1,2}\n.if item==1\n.if 0\n.byte 99\n.else\n.byte 5\n.endif\n.else\n.byte 6\n.endif\n.endfor\n.byte 7\n.else\n.byte 99\n.endif\n", &[5, 6, 7]),
    ("conditional-wide-truth", ".for item in {0,1}\n.if item+((1<<16)<<16)\n.byte 8\n.else\n.byte 99\n.endif\n.endfor\n", &[8,8]),
    // Test iterator transport with legal byte values, independently of the
    // existing Rust/native narrow-output policy differences.
    ("inline-wide-iterator", "wideUnit=((1<<16)<<16)\n.for item in {wideUnit}\n.byte item&255\n.endfor\n", &[0]),
    ("named-wide-iterator", "wideUnit=((1<<16)<<16)\nitems={wideUnit}\n.for item in items\n.byte item&255\n.endfor\n", &[0]),
    ("inline-negative-iterator", ".for item in {-1,2}\n.byte item&255\n.endfor\n", &[255,2]),
    ("named-negative-iterator", "items={-1,2}\n.for item in items\n.byte item&255\n.endfor\n", &[255,2]),
    ("conditional-preprocessor", ".for item in {1,2}\n.if item==1\n.ifdef absent\n.byte missing\n.else\n.byte 7\n.endif\n.else\n.ifndef absent\n.byte 8\n.endif\n.endif\n.endfor\n", &[7,8]),
];
const BAD: &[(&str, &str)] = &[
    (
        "shadowed-list-index",
        "items={8,9}\n.for items in {2}\n.byte items[0]\n.endfor\n",
    ),
    (
        "shadowed-list-length",
        "items={8,9}\n.for items in {2}\n.byte .len(items)\n.endfor\n",
    ),
    (
        "iteration-limit",
        ".for item in 0..=65536\n.byte item\n.endfor\n",
    ),
    (
        "wide-range-iteration-limit",
        ".for item in -4294967296..4294967296\n.byte 0\n.endfor\n",
    ),
    (
        "indented-mutable-body",
        "acc .var 0\n.for item in {1,2}\n acc .set item\n.byte item\n.endfor\n",
    ),
    (
        "conditional-mutable-body",
        "acc .var 0\n.for item in {1,2}\n.if item\n acc := item\n.endif\n.byte item\n.endfor\n",
    ),
    (
        "body-mutation",
        "acc .var 0\n.for item in {1,2}\nacc := acc+item\n.byte item\n.endfor\n",
    ),
    ("scalar-iterator", ".for item in 3\n.byte item\n.endfor\n"),
    (
        "unavailable-iterator",
        ".for item in missing\n.byte item\n.endfor\n",
    ),
    ("zero-step", ".for item in 1..4:0\n.byte item\n.endfor\n"),
    (
        "wrong-direction",
        ".for item in 1..4:-1\n.byte item\n.endfor\n",
    ),
    (
        "body-label",
        ".for item in {1,2}\ninside:\n.byte item\n.endfor\n",
    ),
    (
        "binding-does-not-leak",
        ".for item in {1,2}\n.byte item\n.endfor\n.byte item\n",
    ),
    (
        "conditional-double-else",
        ".for item in {1}\n.if item\n.byte 1\n.else\n.byte 2\n.else\n.byte 3\n.endif\n.endfor\n",
    ),
    (
        "conditional-unmatched-end",
        ".for item in {1}\n.endif\n.endfor\n",
    ),
    (
        "conditional-missing-end",
        ".for item in {1}\n.if item\n.byte 1\n.endfor\n",
    ),
    (
        "conditional-active-error",
        ".for item in {1}\n.if item\n.byte 1/0\n.endif\n.endfor\n",
    ),
    (
        "conditional-unavailable",
        ".for item in {1}\n.if missing\n.byte 1\n.endif\n.endfor\n",
    ),
];
fn imported_shadow_source(cpu: &str, qualified: bool) -> String {
    // Bare iterators have distinct lexical identities; qualified references
    // still retain the imported block and its relocated address.
    let reference = if qualified { ".word dep.helper\n" } else { "" };
    format!(
        ".module dep\n.cpu {cpu}\n.pub\nhelper .block\n.byte $a5\n.bend\n.endmodule\n.module main\n.cpu {cpu}\n.use dep (helper)\n.for helper in {{2,4}}\n.byte helper\n{reference}.endfor\n.endmodule\n"
    )
}

fn register_context_source(cpu: &str) -> (String, Vec<u8>) {
    let (name, instruction, expected) = match cpu {
        "68020" => ("d0", "moveq #1,d0", vec![2, 0x70, 1, 4, 0x70, 1, 2]),
        "6502" => ("a", "asl a", vec![2, 0x0a, 4, 0x0a, 2]),
        _ => panic!("unsupported fixture CPU"),
    };
    (
        source(cpu, &format!("{name}={{2,4}}\n.for {name} in {name}\n.byte {name}\n {instruction}\n.endfor\n.byte {name}[0]\n")),
        expected,
    )
}

fn growth_source(cpu: &str) -> String {
    let mut body = String::from("item=7\n");
    for value in 0..192 {
        body.push_str(&format!(".for item in {{{value}}}\n.byte item\n.endfor\n"));
    }
    body.push_str(".byte item\n");
    source(cpu, &body)
}

#[test]
fn native_iterable_loops_imported_shadow_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        let (source, expected) = register_context_source(cpu);
        assert_eq!(oracle(&dir.0, &source).unwrap(), expected);
        let mut expected_growth = (0..192).map(|value| value as u8).collect::<Vec<_>>();
        expected_growth.push(7);
        assert_eq!(
            oracle(&dir.0, &growth_source(cpu)).unwrap(),
            expected_growth
        );
        assert_eq!(
            oracle(&dir.0, &imported_shadow_source(cpu, false)).unwrap(),
            [2, 4],
            "{cpu}/imported-shadow"
        );
        assert_eq!(
            oracle(&dir.0, &imported_shadow_source(cpu, true)).unwrap(),
            [0xa5, 2, 0, 0, 4, 0, 0],
            "{cpu}/imported-shadow-qualified"
        );
    }
}

fn combined_positive_source(cpu: &str) -> String {
    let mut body = String::new();
    for (name, case_body, _) in POSITIVE {
        body.push_str(&format!(
            ".namespace {}\n{case_body}.endnamespace\n",
            name.replace('-', "_")
        ));
    }
    source(cpu, &body)
}

fn source(cpu: &str, body: &str) -> String {
    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n")
}
#[test]
fn native_iterable_loops_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        for (name, body, expected) in POSITIVE {
            assert_eq!(
                oracle(&dir.0, &source(cpu, body))
                    .unwrap_or_else(|error| panic!("Rust oracle {cpu}/{name}: {error}")),
                *expected,
                "{cpu}/{name}"
            );
        }
        for (name, body) in BAD {
            assert!(
                oracle(&dir.0, &source(cpu, body)).is_err(),
                "Rust accepted {cpu}/{name}"
            );
        }
    }
}

pub(super) fn oracle(dir: &Path, source: &str) -> Result<Vec<u8>, String> {
    let input = dir.join("entry.asm");
    let output = dir.join("out.bin");
    fs::write(&input, source).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_owned(),
        input.to_string_lossy().into_owned(),
        "--bin".to_owned(),
        output.to_string_lossy().into_owned(),
    ]);
    let config = validate_cli(&cli).map_err(|error| format!("{error:?}"))?;
    run_with_validated_cli_with_context(&cli, &config).map_err(|error| format!("{error:?}"))?;
    fs::read(output).map_err(|error| error.to_string())
}

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}
pub(super) fn build_image(root: &Path, dir: &Path) -> Vec<u8> {
    let build = build_native_packages(
        &engine::build_default_asm_registry(),
        &dir.join("native"),
        &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::Targets(vec!["68020".into(), "6502".into()]),
    )
    .unwrap();
    if std::env::var("OPFORGE_ITERABLE_MEMORY").as_deref() == Ok("1") {
        super::compact_cli_input::assemble_cli_with_defines(
            root,
            &build,
            &[
                "OPFORGE_DEBUG_CONTRACTS",
                "OPFORGE_MEMORY_TELEMETRY",
                "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
                "OPFORGE_PREPARATION_PROGRESS",
            ],
        )
    } else {
        super::compact_cli_input::assemble_cli(root, &build)
    }
}

#[test]
fn native_iterable_loops_host_assembles() {
    let dir = scratch();
    // An explicit source snapshot lets comparative runs regenerate identical
    // current packages for the old native code without disturbing this checkout.
    let source_root = std::env::var_os("OPFORGE_ITERABLE_BUILD_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(root);
    let image = build_image(&source_root, &dir.0);
    assert!(!image.is_empty());
    if let Some(path) = std::env::var_os("OPFORGE_ITERABLE_SAVE_IMAGE") {
        fs::write(path, image).unwrap();
    }
}

#[test]
#[ignore = "requires FS-UAE; lexical iterator/import identity comparison"]
fn native_iterable_loops_fs_uae() {
    let root = root();
    let dir = scratch();
    // These knobs affect only this test's image build and localization input.
    let image = std::env::var_os("OPFORGE_ITERABLE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    if let Some(path) = std::env::var_os("OPFORGE_ITERABLE_SAVE_IMAGE") {
        fs::write(path, &image).unwrap();
    }
    if let Some(path) = std::env::var_os("OPFORGE_ITERABLE_SOURCE") {
        run_case(
            &root,
            &dir.0,
            &image,
            "iterable-localization",
            "68020",
            &fs::read_to_string(path).unwrap(),
            false,
        );
        return;
    }
    let mut failures = Vec::new();
    let mut check = |name: &str, cpu: &str, source: &str, failure| {
        if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            run_case(&root, &dir.0, &image, name, cpu, source, failure);
        }))
        .is_err()
        {
            failures.push(format!("{cpu}/{name}"));
        }
    };
    let selection = std::env::var("OPFORGE_ITERABLE_CASES").unwrap_or_else(|_| "all".into());
    let selected = |name: &str, group: &str| {
        selection == "all"
            || selection
                .split(',')
                .any(|item| item == name || item == group)
    };
    let cpu_selection =
        std::env::var("OPFORGE_ITERABLE_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let cpus: Vec<_> = cpu_selection.split(',').map(str::trim).collect();
    assert!(
        cpus.iter().all(|cpu| matches!(*cpu, "68020" | "6502")),
        "iterable CPU selector must contain 68020 or 6502"
    );
    let mut attempted = 0;
    for cpu in cpus {
        if selected("identity-growth", "positive") {
            attempted += 1;
            check("identity-growth", cpu, &growth_source(cpu), false);
        }
        if selected("register-context", "positive") {
            attempted += 1;
            let (source, _) = register_context_source(cpu);
            check("register-context", cpu, &source, false);
        }
        for (name, qualified) in [
            ("imported-shadow", false),
            ("imported-shadow-qualified", true),
        ] {
            if selected(name, "positive") {
                attempted += 1;
                check(name, cpu, &imported_shadow_source(cpu, qualified), false);
            }
        }
        if selection.split(',').any(|item| item == "combined-positive") {
            attempted += 1;
            check(
                "combined-positive",
                cpu,
                &combined_positive_source(cpu),
                false,
            );
        }

        for (name, body, _) in POSITIVE {
            if selected(name, "positive") {
                attempted += 1;
                check(name, cpu, &source(cpu, body), false);
            }
        }
        for (name, body) in BAD {
            if selected(name, "negative") {
                attempted += 1;
                check(name, cpu, &source(cpu, body), true);
            }
        }
    }
    assert!(attempted > 0, "iterable selector must match a case");
    assert!(
        failures.is_empty(),
        "iterable cases failed: {failures:?}; all cases were attempted"
    );
}

pub(super) fn run_case(
    root: &Path,
    dir: &Path,
    image: &[u8],
    name: &str,
    cpu: &str,
    source: &str,
    failure: bool,
) {
    let expected = oracle(dir, source);
    assert_eq!(
        expected.is_err(),
        failure,
        "Rust oracle {name}: {expected:?}"
    );
    let files = [OpforgeNativeCliGuestFile {
        relative_path: "entry.asm",
        bytes: source.as_bytes(),
    }];
    let case = OpforgeNativeCliParityCase {
        name,
        // The emulator runs the 68020 guest platform; the source selects the
        // assembly target independently (including the embedded 6502 package).
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some("Work:entry.asm --bin Work:out.bin"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: match &expected {
            Ok(bytes) => OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/out.bin",
                rust_oracle: bytes,
            },
            Err(_) => OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(root, &case, image)
        .expect("fresh iterable CLI completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].success, !failure);
    assert_eq!(runs[0].exit_code, Some(if failure { 20 } else { 0 }));
    if std::env::var("OPFORGE_ITERABLE_MEMORY").as_deref() == Ok("1") {
        let bytes = runs[0]
            .captured_artifacts
            .get(&PathBuf::from("Work/memory.bin"))
            .expect("fresh instrumented memory record");
        // Keep gated functional work counters beside the same fresh MEMD proof.
        for line in runs[0]
            .stdout
            .lines()
            .filter(|line| line.starts_with("progress p="))
        {
            eprintln!("ITERABLE_PROGRESS {line}");
        }
        // Reuse the maintained MEMD decoder and its cleanup/profile checks.
        eprintln!(
            "ITERABLE_MEMORY {}",
            super::native_package_loading_performance::memory(bytes)
        );
    }
    eprintln!(
        "ITERABLE_CASE name={name} cpu={cpu} failure={failure} source_bytes={} seconds={:?} image_bytes={}",
        source.len(), runs[0].start_to_done_host_seconds, image.len()
    );
}
