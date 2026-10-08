//! Anonymous counted scoped repetition; indexed/labeled and iterable scopes
//! remain separate capabilities. Native positives always use live Rust output.
use super::native_iterable_loops::{build_image, oracle, run_case, Scratch};
use super::*;

fn scratch() -> Scratch {
    static NEXT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let serial = NEXT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let path = std::env::temp_dir().join(format!(
        "opforge-scoped-{}-{}-{serial}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir(&path).unwrap();
    Scratch(path)
}

const POSITIVE: &[(&str, &str, &[u8])] = &[
    ("dormant-local-configuration", "outer=9\n.bfor 0\nouter=missing\n.endfor\n.byte outer\n", &[9]),
    ("scoped-constant-with-while", ".while 0\n.endwhile\n.bfor 2\nvalue=3\n.byte value\n.endfor\n", &[3, 3]),
    ("local-label", ".bfor 2\nlocal .byte 7\n.byte $-local\n.endfor\n", &[7, 1, 7, 1]),
    ("local-constant", "value=9\n.bfor 2\nvalue=3\n.byte value\n.endfor\n.byte value\n", &[3, 3, 9]),
    ("forward-local-chain", ".bfor 2\n.byte first\nfirst=second+1\nsecond=third+1\nthird=4\n.endfor\n", &[6, 6]),
    ("local-mutable", "value .var 9\n.bfor 2\nvalue .var 2\n.byte value\nvalue .set value+1\n.byte value\n.endfor\n.byte value\n", &[2, 3, 2, 3, 9]),
    ("mutable-constant-snapshot", ".bfor 2\nvalue .var 2\nsnapshot=value\nvalue .set 8\n.byte snapshot,value\n.endfor\n", &[2, 8, 2, 8]),
    ("nested-local-restoration", ".bfor 2\nvalue=3\n.byte value\n.bfor 2\nvalue=5\n.byte value\n.endfor\n.byte value\n.endfor\n", &[3, 5, 5, 3, 3, 5, 5, 3]),
    ("macro-local", "emit .macro\nlocal=6\n.byte local\n.endmacro\n.bfor 2\n.emit\n.endfor\n", &[6, 6]),
    ("empty-scope", ".bfor 0\nlocal=missing\n.byte local\n.endfor\n.byte 7\n", &[7]),
    ("forward-mutable-snapshot", ".bfor 2\nn .var 1\nsaved .const n+later\nn .set 9\nlater .const 2\n.byte saved,n\n.endfor\n", &[3,9,3,9]),
    ("count-before-local-shadow", "count=2\n.bfor count\ncount=3\n.byte count\n.endfor\n.byte count\n", &[3,3,2]),
    ("count-before-local-shadow-with-while", ".while 0\n.endwhile\ncount=2\n.bfor count\ncount=3\n.byte count\n.endfor\n.byte count\n", &[3,3,2]),
    ("inner-constant-outer-mutable", ".bfor 2\nouter .var 3\n.bfor 2\nsaved .const outer+later\nlater .const 2\n.byte saved\n.endfor\n.byte outer\n.endfor\n", &[5,5,3,5,5,3]),
    ("forward-local-label", ".bfor 2\n.byte tail-head\nhead .byte 1\ntail\n.endfor\n", &[1,1,1,1]),
    ("available-local-conditional", ".bfor 2\nlocal .var 1\n.if local\n.byte 5\n.endif\n.endfor\n", &[5,5]),
    ("local-conditional-mutations", ".bfor 2\nn .var 1\n.if n\n.byte 2\n.endif\nn .set 0\n.if n\n.byte 9\n.else\n.byte 3\n.endif\n.endfor\n", &[2,3,2,3]),
    ("compound-local-snapshot", ".bfor 2\nitems .var {2,3}\nsaved=items\nitems .set {9,10}\n.byte saved[1],items[0]\n.endfor\n", &[3,9,3,9]),
    ("wide-local-values", ".bfor 2\nwide .var $ffffffff+4\n.byte wide&255,(wide>>16)>>16\nwide .set wide+1\n.byte wide&255,(wide>>16)>>16\n.endfor\n", &[3,1,4,1,3,1,4,1]),
    ("signed-local-snapshot", ".bfor 2\nn .var -17\nsaved .const n\nn .set 7\n.byte -(saved/7),-(saved%7),n\n.endfor\n", &[2,3,7,2,3,7]),
    ("forward-local-mutable-initializer", ".bfor 2\nn .var later\nlater .var 2\n.byte n\n.endfor\n", &[2, 2]),
];
const BAD: &[(&str, &str)] = &[
    (
        "active-local-missing-constant",
        ".bfor 2\nlocal=missing\n.byte local\n.endfor\n",
    ),
    (
        "active-local-missing-value",
        ".bfor 2\n.byte missing\n.endfor\n",
    ),
    ("unmatched-close", ".endfor\n"),
    ("unclosed-scope", ".bfor 2\nlocal .byte 1\n"),
    (
        "local-cycle",
        ".bfor 2\nfirst=second\nsecond=first\n.byte first\n.endfor\n",
    ),
    ("local-escape", ".bfor 2\nlocal=3\n.endfor\n.byte local\n"),
];
// These sources are accepted by Rust; native completion must explicitly reject
// capabilities outside the anonymous counted checkpoint, not claim Rust parity.
const GAPS: &[(&str, &str, &[u8])] = &[
    (
        "wide-literal-input",
        ".bfor 2\nwide .var $100000003\n.byte wide&255,(wide>>16)>>16\nwide .set wide+1\n.byte wide&255,(wide>>16)>>16\n.endfor\n",
        &[3, 1, 4, 1, 3, 1, 4, 1],
    ),
    (
        "inactive-local-constant",
        ".bfor 2\n.if 0\nhidden=missing\n.byte hidden\n.else\n.byte 7\n.endif\n.endfor\n",
        &[7, 7],
    ),
    (
        "retained-forward-local-conditional",
        ".bfor 2\n.if local\n.byte 5\n.endif\nlocal .var 1\n.endfor\n",
        &[5, 5],
    ),
    (
        "labeled-indexed-scope",
        "items .bfor 2\nentry .byte 1\n.endfor\n.byte items[1]\n",
        &[1, 1, 1],
    ),
    (
        "scoped-iterable",
        ".bfor item in {2,3}\n.byte item\n.endfor\n",
        &[2, 3],
    ),
    (
        "scoped-while",
        ".org 0\n.bwhile $<1\nentry .byte 4\n.endwhile\n",
        &[4, 4],
    ),
];
fn source(cpu: &str, body: &str) -> String {
    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n")
}
fn combined_positive() -> (String, Vec<u8>) {
    let mut body = String::new();
    let mut expected = Vec::new();
    for (index, (_, scenario, bytes)) in POSITIVE.iter().enumerate() {
        body.push_str(&format!(".namespace scoped_case_{index}\n"));
        body.push_str(scenario);
        body.push_str(".endnamespace\n");
        expected.extend_from_slice(bytes);
    }
    (body, expected)
}
#[test]
fn native_scoped_loops_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        for (name, body, expected) in POSITIVE.iter() {
            assert_eq!(
                oracle(&dir.0, &source(cpu, body)).unwrap(),
                *expected,
                "{cpu}/{name}"
            );
        }
        for (name, body, expected) in GAPS {
            assert_eq!(
                oracle(&dir.0, &source(cpu, body)).unwrap(),
                *expected,
                "Rust gap oracle {cpu}/{name}"
            );
        }
        for (name, body) in BAD {
            assert!(
                oracle(&dir.0, &source(cpu, body)).is_err(),
                "Rust accepted {cpu}/{name}"
            );
        }
        let (body, expected) = combined_positive();
        assert_eq!(oracle(&dir.0, &source(cpu, &body)).unwrap(), expected);
    }
}
// Isolate the current Rust module/layout discrepancy; it is not a native
// parity assertion or package-neutral output contract for this checkpoint.
#[test]
fn native_scoped_loops_rust_self_initializer_boundary() {
    let dir = scratch();
    let body = "n .var 1\n.bfor 2\n.byte n\nn .set n+1\n.endfor\n.byte n\n";
    let error = oracle(&dir.0, &source("68020", body)).unwrap_err();
    assert!(
        error.contains("asm202")
            && error.contains("layout did not stabilize after bounded sizing retries"),
        "{error}"
    );
    assert_eq!(oracle(&dir.0, &source("6502", body)).unwrap(), [2, 2, 1]);
}

#[test]
#[ignore = "requires configured FS-UAE; anonymous counted scoped repetition"]
fn native_scoped_loops_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_SCOPED_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter = std::env::var("OPFORGE_SCOPED_FILTER").unwrap_or_default();
    let cpus = std::env::var("OPFORGE_SCOPED_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let mut failures = Vec::new();
    let mut attempted = 0;
    let (combined, _) = combined_positive();
    for cpu in cpus.split(',').map(str::trim) {
        assert!(
            matches!(cpu, "68020" | "6502"),
            "unknown scoped fixture CPU {cpu}"
        );
        for (name, body, bad) in std::iter::once(("combined-positive", combined.as_str(), false))
            .chain(POSITIVE.iter().map(|(n, b, _)| (*n, *b, false)))
            .chain(BAD.iter().map(|(n, b)| (*n, *b, true)))
        {
            if filter.is_empty()
                && !bad
                && !matches!(
                    name,
                    "combined-positive" | "nested-local-restoration" | "forward-local-label"
                )
            {
                continue;
            }
            if !filter.is_empty() && !filter.split(',').any(|part| name.contains(part)) {
                continue;
            }
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_case(&root, &dir.0, &image, name, cpu, &source(cpu, body), bad);
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
    }
    for cpu in cpus.split(',').map(str::trim) {
        for (name, body, expected) in GAPS {
            if !filter.is_empty() && !filter.split(',').any(|part| name.contains(part)) {
                continue;
            }
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let source = source(cpu, body);
                assert_eq!(
                    oracle(&dir.0, &source).unwrap(),
                    *expected,
                    "Rust gap oracle {cpu}/{name}"
                );
                run_gap(&root, &image, cpu, name, &source);
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
    }
    assert!(attempted > 0, "scoped filter selected no cases: {filter}");
    assert!(failures.is_empty(), "scoped cases failed: {failures:?}");
}

fn run_gap(root: &Path, image: &[u8], cpu: &str, name: &str, source: &str) {
    let started = std::time::Instant::now();
    let proof_name = format!("{cpu}/{name}");
    let files = [OpforgeNativeCliGuestFile {
        relative_path: "entry.asm",
        bytes: source.as_bytes(),
    }];
    let case = OpforgeNativeCliParityCase {
        name: &proof_name,
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some("Work:entry.asm --bin Work:out.bin"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(root, &case, image)
        .expect("fresh scoped capability completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert!(!runs[0].success);
    assert_eq!(runs[0].exit_code, Some(20));
    eprintln!(
        "SCOPED_GAP {cpu}/{name} rejected fresh {:.6}s",
        started.elapsed().as_secs_f64()
    );
}
