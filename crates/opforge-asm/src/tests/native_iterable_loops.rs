//! Iterable loops use each case's live Rust oracle.
use super::*;
use crate::native_package_build::{build_native_packages, EmbedSelection};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
fn scratch() -> Scratch {
    let path = std::env::temp_dir().join(format!(
        "opforge-iterable-{}-{}",
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
    ("named-snapshot", "items .var {2,4}\nsaved=items\nitems .set {9,10,11}\n.for item in saved\n.byte item+1\n.endfor\n.for item in items\n.byte item\n.endfor\n", &[3,5,9,10,11]),
    ("nested-count-empty", "item=23\n.for item in {}\n.byte missing\n.endfor\n.for item in 4..4\n.byte missing\n.endfor\n.for item in {2,3}\n.for item\n.byte item+10\n.endfor\n.endfor\n.byte item\n", &[12,12,13,13,13,23]),
];
const BAD: &[(&str, &str)] = &[
    (
        "iteration-limit",
        ".for item in 0..=65536\n.byte item\n.endfor\n",
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
    (
        "inline-negative-iterator",
        ".for item in {-1,2}\n.byte item\n.endfor\n",
    ),
    (
        "named-negative-iterator",
        "items={-1,2}\n.for item in items\n.byte item\n.endfor\n",
    ),
    (
        "inline-wide-iterator",
        "wideUnit=((1<<16)<<16)\n.for item in {wideUnit}\n.byte item\n.endfor\n",
    ),
    (
        "named-wide-iterator",
        "wideUnit=((1<<16)<<16)\nitems={wideUnit}\n.for item in items\n.byte item\n.endfor\n",
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
];
fn imported_shadow_source(cpu: &str, qualified: bool) -> String {
    // These live-oracle probes remain red on native at this checkpoint.
    // Iterator identities still collapse into imported targets during binding.
    let reference = if qualified { ".word dep.helper\n" } else { "" };
    format!(
        ".module dep\n.cpu {cpu}\n.pub\nhelper .block\n.byte $a5\n.bend\n.endmodule\n.module main\n.cpu {cpu}\n.use dep (helper)\n.for helper in {{2,4}}\n.byte helper\n{reference}.endfor\n.endmodule\n"
    )
}

#[test]
fn native_iterable_loops_imported_shadow_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        assert_eq!(
            oracle(&dir.0, &imported_shadow_source(cpu, false)).unwrap(),
            // Current Rust reachability retains the selected block even though
            // every bare use here executes through the iterator binding.
            [0xa5, 2, 4],
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
fn build_image(root: &Path, dir: &Path) -> Vec<u8> {
    let build = build_native_packages(
        &engine::build_default_asm_registry(),
        &dir.join("native"),
        &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::Targets(vec!["68020".into(), "6502".into()]),
    )
    .unwrap();
    super::compact_cli_input::assemble_cli(root, &build)
}

#[test]
fn native_iterable_loops_host_assembles() {
    let dir = scratch();
    assert!(!build_image(&root(), &dir.0).is_empty());
}

#[test]
#[ignore = "requires FS-UAE; imported iterator-shadowing probes have known parity failures"]
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

fn run_case(
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
    eprintln!(
        "ITERABLE_CASE name={name} cpu={cpu} failure={failure} source_bytes={} seconds={:?} image_bytes={}",
        source.len(), runs[0].start_to_done_host_seconds, image.len()
    );
}
