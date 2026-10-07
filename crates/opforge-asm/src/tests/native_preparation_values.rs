//! Preparation values and configured imports use each case's live Rust oracle.
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
        "opforge-preparation-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&path).unwrap();
    Scratch(path)
}

pub(super) fn source(cpu: &str) -> String {
    format!(
        r#".module main
.cpu {cpu}
values = {{2,4,6}}
span = 3..=9:3
alias = values
.if .len(alias)==3 && alias[1]==4 && .len(span)==3 && span[2]==9
.byte 11
.else
.byte 99
.endif
changing .var {{7,8}}
snapshot = changing
changing .set 10..=14:2
.if .len(snapshot)==2 && snapshot[1]==8 && .len(changing)==3 && changing[2]==14
.byte 12
.else
.byte 99
.endif
rangeSnapshot = changing
changing .set {{21,22,23,24}}
.if .len(rangeSnapshot)==3 && rangeSnapshot[1]==12 && .len(changing)==4 && changing[3]==24
.byte 13
.else
.byte 99
.endif
changing .set 17
.if changing==17 && snapshot[0]==7 && rangeSnapshot[2]==14
.byte 14
.else
.byte 99
.endif
hidden .block
values = {{31,32}}
.byte .len(values),values[1]
.bend
.if .len(values)==3 && values[1]==4
.byte 15
.else
.byte 99
.endif
.endmodule
"#
    )
}

fn dependency_source(cpu: &str) -> String {
    let modules = format!(
        r#".module main
.cpu {cpu}
changing .var {{2,4,6}}
values = changing
changing .set 3..=9:3
span = changing
changing .set 17
.use middle as dep with (SCALAR=changing-10, ITEMS=values, RANGE=span, INLINE={{11,12}})
.use middle as again with (SCALAR=7, ITEMS={{2,4,6}}, RANGE=3..=9:3, INLINE={{11,12}})
.byte dep.result
.endmodule
.module middle
.cpu {cpu}
snapshot = ITEMS
rangeSnapshot = RANGE
.if INLINE[1]==12 && .len(INLINE)==2 && SCALAR==7 && .len(ITEMS)==3 && ITEMS[1]==4 && .len(RANGE)==3 && RANGE[2]==9
.use leaf as child with (SCALAR=SCALAR+1, ITEMS=snapshot, RANGE=rangeSnapshot)
.else
.use unavailable
.endif
.pub
result = child.result
.endmodule
.module leaf
.cpu {cpu}
.if SCALAR==8 && .len(ITEMS)==3 && ITEMS[2]==6 && .len(RANGE)==3 && RANGE[0]==3
.pub
result = 42
.else
.pub
result = 99
.endif
.endmodule
"#
    );
    modules
        .split_inclusive(".endmodule\n")
        .collect::<Vec<_>>()
        .into_iter()
        .rev()
        .collect()
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

fn wide_parameter_source(cpu: &str) -> String {
    // Small literal tokens construct full-width values without relying on the
    // separately unsupported wide-literal packing. Copy both scalar halves.
    format!(
        r#".module dep
.cpu {cpu}
.if .len(ITEMS)==2 && ITEMS[0]/7==-2 && ITEMS[1]==WIDE && .len(SPAN)==3 && SPAN[2]==WIDE+4
.pub
result=42
.else
.pub
result=99
.endif
.endmodule
.module main
.cpu {cpu}
wideUnit=((1<<16)<<16)
values={{-17,wideUnit}}
window = wideUnit ..= (wideUnit+4) : 2
.use dep as d with (ITEMS=values, SPAN=window, WIDE=wideUnit)
.byte d.result
.endmodule
"#
    )
}

const BAD: &[(&str, &str)] = &[
    (
        "prepare-zero-step",
        "span=0..4:0\n.if .len(span)\n.byte 1\n.endif\n",
    ),
    (
        "prepare-bad-index",
        "values={1,2}\n.if values[2]\n.byte 1\n.endif\n",
    ),
    ("use-unavailable", ".use dep with (ITEMS=missing)\n"),
    (
        "use-forward-unavailable",
        ".use dep with (ITEMS=values)\nvalues={1,2}\n",
    ),
    ("use-zero-step", ".use dep with (ITEMS=0..4:0)\n"),
    (
        "use-compound-scalar",
        "values={1,2}\n.use dep with (ITEMS=values+1)\n",
    ),
    (
        "use-address-in-unselected-branch",
        ".use dep with (ITEMS=1?7:$)\n",
    ),
    ("use-nested-list", ".use dep with (ITEMS={{1},2})\n"),
    (
        "use-compound-conflict",
        ".use dep as a with (ITEMS={1,2})\n.use dep as b with (ITEMS={1,3})\n",
    ),
    (
        "use-lexical-isolation",
        "hidden .block\nvalues={1,2}\n.bend\n.use dep with (ITEMS=values)\n",
    ),
];

fn negative_source(body: &str) -> String {
    format!(".module main\n.cpu 68020\n{body}.endmodule\n.module dep\n.cpu 68020\n.pub\nresult=1\n.endmodule\n")
}

#[test]
fn native_preparation_values_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        assert_eq!(
            oracle(&dir.0, &source(cpu)).unwrap(),
            [11, 12, 13, 14, 2, 32, 15],
            "CPU {cpu}"
        );
    }
    for (name, body) in BAD {
        assert!(
            oracle(&dir.0, &negative_source(body)).is_err(),
            "Rust accepted {name}"
        );
    }
}

#[test]
fn native_preparation_values_dependency_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        assert_eq!(
            oracle(&dir.0, &dependency_source(cpu)).unwrap(),
            [42],
            "dependency CPU {cpu}"
        );
        assert_eq!(oracle(&dir.0, &wide_parameter_source(cpu)).unwrap(), [42]);
    }
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
fn native_preparation_values_host_assembles() {
    let dir = scratch();
    assert!(!build_image(&root(), &dir.0).is_empty());
}

#[test]
#[ignore = "fresh packed preparation CLI positives and failures; requires configured FS-UAE"]
fn native_preparation_values_fs_uae() {
    let root = root();
    let dir = scratch();
    // These knobs affect only this test's image build and localization input.
    let image = std::env::var_os("OPFORGE_PREPARATION_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    if let Some(path) = std::env::var_os("OPFORGE_PREPARATION_SAVE_IMAGE") {
        fs::write(path, &image).unwrap();
    }
    if let Some(path) = std::env::var_os("OPFORGE_PREPARATION_SOURCE") {
        run_case(
            &root,
            &dir.0,
            &image,
            "preparation-localization",
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
    let selection = std::env::var("OPFORGE_PREPARATION_CASES").unwrap_or_else(|_| "all".into());
    let selected = |name: &str, group: &str| {
        selection == "all"
            || selection
                .split(',')
                .any(|item| item == name || item == group)
    };
    let mut attempted = 0;
    for cpu in ["68020", "6502"] {
        for (name, source) in [
            ("preparation-values", source(cpu)),
            ("configured-compound-chain", dependency_source(cpu)),
            ("wide-compound-parameters", wide_parameter_source(cpu)),
        ] {
            if selected(name, "positive") {
                attempted += 1;
                check(name, cpu, &source, false);
            }
        }
    }
    for (name, body) in BAD {
        if selected(name, "negative") {
            attempted += 1;
            check(name, "68020", &negative_source(body), true);
        }
    }
    assert!(attempted > 0, "preparation selector must match a case");
    assert!(
        failures.is_empty(),
        "preparation cases failed: {failures:?}; all cases were attempted"
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
        .expect("fresh preparation CLI completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].success, !failure);
    assert_eq!(runs[0].exit_code, Some(if failure { 20 } else { 0 }));
    eprintln!(
        "PREPARATION_CASE name={name} cpu={cpu} failure={failure} source_bytes={} seconds={:?} image_bytes={}",
        source.len(), runs[0].start_to_done_host_seconds, image.len()
    );
}
