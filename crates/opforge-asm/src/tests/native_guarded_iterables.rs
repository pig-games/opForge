//! Source-order immutable iterable owners selected by forward guards.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;

const POSITIVE: &[(&str, &str, &[u8])] = &[
    ("list-grow", ".if enabled\nitems={2,4}\n.else\nitems={6}\n.endif\nenabled=1\n.for item in items\n.byte item\n.endfor\n", &[2,4]),
    ("list-shrink", ".if enabled\nitems={7}\n.else\nitems={2,4,6}\n.endif\nenabled=1\n.for item in items\n.byte item\n.endfor\n", &[7]),
    ("list-to-empty", ".if enabled\nitems={}\n.else\nitems={2,4}\n.endif\nenabled=1\n.for item in items\n.byte item\n.endfor\n.byte 9\n", &[9]),
    ("list-from-empty", ".if enabled\nitems={2,4}\n.else\nitems={}\n.endif\nenabled=1\n.for item in items\n.byte item\n.endfor\n", &[2,4]),
    ("false-arm-range", ".if enabled\nitems=1..2\n.else\nitems=4..=8:2\n.endif\nenabled=0\n.for item in items\n.byte item\n.endfor\n", &[4,6,8]),
    ("descending-range", ".if enabled\nitems=7..1:-2\n.else\nitems=1..2\n.endif\nenabled=1\n.for item in items\n.byte item\n.endfor\n", &[7,5,3]),
    ("alias-after-branch", ".if enabled\nitems={2,4}\n.else\nitems={6}\n.endif\nsaved=items\nenabled=1\n.for item in saved\n.byte item\n.endfor\n", &[2,4]),
    ("nested-guards", ".if enabled\n.if choice\nitems={2,4}\n.else\nitems={6}\n.endif\n.else\nitems={8}\n.endif\nenabled=1\nchoice=1\n.for item in items\n.byte item\n.endfor\n", &[2,4]),
    ("inactive-missing", ".if enabled\nitems={missing}\n.else\nitems={2,4}\n.endif\nenabled=0\n.for item in items\n.byte item\n.endfor\n", &[2,4]),
    ("same-length-values", ".if enabled\nitems={2,4}\n.else\nitems={6,8}\n.endif\nenabled=1\n.for item in items\n.byte item\n.endfor\n.byte .len(items),items[1]\n", &[2,4,2,4]),
    ("scoped-alias", ".if enabled\nitems={2,4}\n.else\nitems={6}\n.endif\nsaved=items\nenabled=1\n.bfor item in saved\n.byte item\n.endfor\n", &[2,4]),
    ("nested-grow", ".if enabled\nitems={2,4}\n.else\nitems={6}\n.endif\nenabled=1\n.for item in items\n.for 2\n.byte item\n.endfor\n.endfor\n", &[2,2,4,4]),
    ("nested-shrink", ".if enabled\nitems={7}\n.else\nitems={2,4,6}\n.endif\nenabled=1\n.for item in items\n.for 2\n.byte item\n.endfor\n.endfor\n", &[7,7]),
    ("nested-iterable-grow", ".if enabled\nitems={2,4}\n.else\nitems={6}\n.endif\nenabled=1\n.for item in items\n.for sub in {1,2}\n.byte item+sub\n.endfor\n.endfor\n", &[3,4,5,6]),
    ("nested-iterable-shrink", ".if enabled\nitems={7}\n.else\nitems={2,4,6}\n.endif\nenabled=1\n.for item in items\n.for sub in {1,2}\n.byte item+sub\n.endfor\n.endfor\n", &[8,9]),
];

const BAD: &[(&str, &str)] = &[
    ("selected-missing", ".if enabled\nitems={missing}\n.else\nitems={2}\n.endif\nenabled=1\n.for item in items\n.byte item\n.endfor\n"),
    ("selected-duplicate", ".if enabled\nitems={2,4}\nitems={6}\n.endif\nenabled=1\n.for item in items\n.byte item\n.endfor\n"),
    ("invalid-range", ".if enabled\nitems=1..4:0\n.else\nitems={2}\n.endif\nenabled=1\n.for item in items\n.byte item\n.endfor\n"),
];

fn source(cpu: &str, name: &str, body: &str) -> String {
    format!(".module {name}\n.cpu {cpu}\n{body}.endmodule\n")
}

fn combined(cpu: &str) -> (String, Vec<u8>) {
    let mut input = String::new();
    let mut expected = Vec::new();
    for (index, (_, body, bytes)) in POSITIVE.iter().enumerate() {
        input.push_str(&source(cpu, &format!("case{index}"), body));
        expected.extend_from_slice(bytes);
    }
    (input, expected)
}

#[test]
fn native_guarded_iterables_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        for (name, body, expected) in POSITIVE {
            assert_eq!(
                oracle(&dir.0, &source(cpu, "main", body)).unwrap(),
                *expected,
                "{cpu}/{name}"
            );
        }
        let (input, expected) = combined(cpu);
        assert_eq!(oracle(&dir.0, &input).unwrap(), expected, "{cpu}/combined");
        for (name, body) in BAD {
            assert!(
                oracle(&dir.0, &source(cpu, "main", body)).is_err(),
                "{cpu}/{name}"
            );
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; immutable iterable owner guard replay"]
fn native_guarded_iterables_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_GUARDED_ITERABLE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter =
        std::env::var("OPFORGE_GUARDED_ITERABLE_FILTER").unwrap_or_else(|_| "combined".into());
    let cpus =
        std::env::var("OPFORGE_GUARDED_ITERABLE_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let mut failures = Vec::new();
    let mut attempted = 0;
    for cpu in cpus.split(',').map(str::trim) {
        assert!(matches!(cpu, "68020" | "6502"));
        let cases = std::iter::once(("combined", combined(cpu).0, false))
            .chain(
                POSITIVE
                    .iter()
                    .map(|(name, body, _)| (*name, source(cpu, "main", body), false)),
            )
            .chain(
                BAD.iter()
                    .map(|(name, body)| (*name, source(cpu, "main", body), true)),
            );
        for (name, input, rejected) in cases {
            if !filter
                .split(',')
                .any(|item| item.trim() == name || item.trim() == "all")
            {
                continue;
            }
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_case(&root, &dir.0, &image, name, cpu, &input, rejected);
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
    }
    assert!(attempted > 0, "no guarded iterable cases selected");
    assert!(
        failures.is_empty(),
        "native guarded iterable gaps: {}",
        failures.join(", ")
    );
}
