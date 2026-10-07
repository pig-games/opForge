//! Compact statement-time ranges use each case's live Rust binary oracle.
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
        "opforge-ranges-{}-{}",
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
    // Build signed64 endpoints with small literals and successive shifts: the
    // current scalar language masks each shift count to five bits.
    format!(
        r#".module main
.cpu {cpu}
ascending = 1..4
inclusive = 1..=4
descending = 4..1
descendingInclusive = 4..=1
stepped = 0..=7:3
descendingStep = 7..=0:-3
empty = 3..3
emptyNegative = 3..3:-1
singleton = 3..=3
alias = stepped
.byte .len(ascending),.len(inclusive),.len(descending),.len(descendingInclusive)
.byte ascending[0],ascending[2],inclusive[3],descending[0],descending[2],descendingInclusive[3]
.byte .len(stepped),stepped[0],stepped[1],stepped[2]
.byte .len(descendingStep),descendingStep[0],descendingStep[1],descendingStep[2]
.byte .len(empty),.len(emptyNegative),.len(singleton),singleton[0],.len(alias),alias[2]
.byte .len(0..7:3),(0..7:3)[2],.len(7..0:-3),(7..0:-3)[2]
changing .var 1..=3
snapshot = changing
changing .set 8..=12:2
.byte .len(snapshot),snapshot[0],snapshot[2],.len(changing),changing[0],changing[2]
rangeSnapshot = changing
changing .set {{21,22}}
.byte .len(rangeSnapshot),rangeSnapshot[2],.len(changing),changing[0],changing[1]
changing .set 17
.byte changing,.len(snapshot),snapshot[2],.len(rangeSnapshot),rangeSnapshot[0]
minimum = (((-1 << 31) << 31) << 1)
maximum = ~minimum
wideUnit = ((1 << 16) << 16)
nearMin = minimum .. (minimum+4)
nearMax = maximum .. (maximum-4)
minimumStep = maximum .. minimum : minimum
hugeAscending = minimum .. maximum
hugeDescending = maximum .. minimum
widePositive = wideUnit ..= (wideUnit+4) : 2
wideNegative = (-wideUnit) ..= (-wideUnit-4) : -2
.byte .len(nearMin),nearMin[0]==minimum,nearMin[3]==(minimum+3)
.byte .len(nearMax),nearMax[0]==maximum,nearMax[3]==(maximum-3)
.byte .len(minimumStep),minimumStep[0]==maximum,minimumStep[1]==-1
.byte .len(hugeAscending)==maximum,.len(hugeDescending)==maximum
.byte .len(hugeAscending)>wideUnit,.len(hugeDescending)>wideUnit
.byte hugeAscending[wideUnit]==(minimum+wideUnit)
.byte hugeDescending[wideUnit]==(maximum-wideUnit)
.byte .len(widePositive),widePositive[2]==(wideUnit+4)
.byte .len(wideNegative),wideNegative[2]==(-wideUnit-4)
.long minimumStep[1]
nop
forward = tail .. (tail+4) : 2
forwardAlias = forward
.byte .len(forward),.len(forwardAlias)
.word forwardAlias[0],forwardAlias[1]
tail .byte 77
.endmodule
"#
    )
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

const BAD: &[(&str, &str)] = &[
    (
        "zero-step-declaration",
        "values=0..4:0\n.byte .len(values)\n",
    ),
    ("zero-step-inline", ".byte .len(0..4:0)\n"),
    (
        "ascending-direction",
        "values=0..4:-1\n.byte .len(values)\n",
    ),
    (
        "descending-direction",
        "values=4..0:1\n.byte .len(values)\n",
    ),
    (
        "inclusive-maximum-overflow",
        "values=0 ..= ~(((-1 << 31) << 31) << 1)\n.byte .len(values)\n",
    ),
    (
        "inclusive-minimum-overflow",
        "values=0 ..= (((-1 << 31) << 31) << 1)\n.byte .len(values)\n",
    ),
    ("negative-index", "values=1..4\n.byte values[-1]\n"),
    ("exclusive-bound-index", "values=1..4\n.byte values[3]\n"),
    ("inclusive-bound-index", "values=1..=4\n.byte values[4]\n"),
    ("stepped-bound-index", "values=0..=7:3\n.byte values[3]\n"),
    ("empty-index", "values=3..3\n.byte values[0]\n"),
    // Rust checks the signed64 product before adding start: the mathematical
    // final value fits, but step * index overflows and indexing must fail.
    (
        "index-product-overflow",
        "minimum=(((-1 << 31) << 31) << 1)\nmaximum=~minimum\nvalues=minimum .. maximum : maximum\n.byte values[2]\n",
    ),
];

#[test]
fn native_ranges_rust_oracles() {
    let dir = scratch();
    // The prefix pins range semantics independently of native and CPU byte order.
    let expected = [
        3, 4, 3, 4, 1, 3, 4, 4, 2, 1, 3, 0, 3, 6, 3, 7, 4, 1, 0, 0, 1, 3, 3, 6, 3, 6, 3, 1, 3, 1,
        3, 3, 8, 12, 3, 12, 2, 21, 22, 17, 3, 3, 3, 8, 4, 1, 1, 4, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1,
        3, 1, 3, 1, 255, 255, 255, 255,
    ];
    for cpu in ["68020", "6502"] {
        let bytes = oracle(&dir.0, &source(cpu)).unwrap();
        assert_eq!(&bytes[..expected.len()], &expected, "CPU {cpu}");
        assert_eq!(bytes.last(), Some(&77), "forward range CPU {cpu}");
    }
    for (name, body) in BAD {
        assert!(
            oracle(&dir.0, &format!(".cpu 68020\n{body}")).is_err(),
            "Rust accepted {name}: {body}"
        );
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
fn native_ranges_host_assembles() {
    let dir = scratch();
    assert!(!build_image(&root(), &dir.0).is_empty());
}

#[test]
#[ignore = "fresh packed range CLI positives and failures; requires configured FS-UAE"]
fn native_ranges_fs_uae() {
    let root = root();
    let dir = scratch();
    // These knobs affect only this test's image build and localization input.
    let image = std::env::var_os("OPFORGE_RANGE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    if let Some(path) = std::env::var_os("OPFORGE_RANGE_SAVE_IMAGE") {
        fs::write(path, &image).unwrap();
    }
    if let Some(path) = std::env::var_os("OPFORGE_RANGE_SOURCE") {
        run_case(
            &root,
            &dir.0,
            &image,
            "range-localization",
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
    let selection = std::env::var("OPFORGE_RANGE_CASES").unwrap_or_else(|_| "all".into());
    let selected = |name: &str, group: &str| {
        selection == "all"
            || selection
                .split(',')
                .any(|item| item == name || item == group)
    };
    if selected("positive", "positive") {
        for cpu in ["68020", "6502"] {
            check("packed-range-values", cpu, &source(cpu), false);
        }
    }
    for (name, body) in BAD {
        if selected(name, "negative") {
            check(name, "68020", &format!(".cpu 68020\n{body}"), true);
        }
    }
    assert!(
        failures.is_empty(),
        "range cases failed: {failures:?}; all cases were attempted"
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
        .expect("fresh range CLI completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].success, !failure);
    assert_eq!(runs[0].exit_code, Some(if failure { 20 } else { 0 }));
    eprintln!(
        "RANGE_CASE name={name} cpu={cpu} failure={failure} source_bytes={} seconds={:?} image_bytes={}",
        source.len(), runs[0].start_to_done_host_seconds, image.len()
    );
}
