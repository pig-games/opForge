//! Conditional repetition preserves the live Rust execution and PC observations.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;

const POSITIVE: &[(&str, &str)] = &[
    ("previous-statement-pc", ".org 0\n.while $<3\n.byte 1\n.endwhile\n"),
    ("org-boundary", ".org 10\n.while $<10\n.byte 3\n.endwhile\n"),
    ("false-body", ".while 0\n.byte missing\n.endwhile\n.byte 7\n"),
    ("mixed-loop-matching", ".org 0\n.while $<2\n.for 2\n.byte 4\n.endfor\n.endwhile\n"),
    ("nested-while", ".org 0\n.while $<2\n.while $<2\n.byte 6\n.endwhile\n.endwhile\n"),
    ("iterator-dependent-while", ".org 0\n.for item in {1,2}\n.while $<item\n.byte item\n.endwhile\n.endfor\n"),
    ("conditional-pc", ".org 0\n.while $<3\n.if $==1\n.byte 2\n.else\n.byte 3\n.endif\n.endwhile\n"),
    ("macro-body", "emit .macro value\n.byte .value\n.endmacro\n.org 0\n.while $<2\n.emit 5\n.endwhile\n"),
    ("inactive-while", ".for item in {1,2}\n.if item==0\n.while 1/0\n.byte missing\n.endwhile\n.else\n.byte item\n.endif\n.endfor\n"),
    ("empty-mixed-loop", ".for item in {}\n.while 1/0\n.byte missing\n.endwhile\n.endfor\n.while 0\n.for missing\n.byte missing\n.endfor\n.endwhile\n.byte 9\n"),
    ("known-constant-guard", ".org 0\nlimit=2\n.while $<limit\n.byte 5\n.endwhile\n"),
    ("compound-guard", ".org 0\nitems={1,2}\n.while $<.len(items)\n.byte 8\n.endwhile\n"),
    ("compound-alias-guard", ".org 0\nitems={1,2}\nalias=items\n.while $<.len(alias)\n.byte 8\n.endwhile\n"),
    ("forward-short-circuit", ".while 0&&later\n.byte 1\n.endwhile\nlater=3\n.byte 7\n"),
];

const BAD: &[(&str, &str)] = &[
    ("unmatched-close", ".endwhile\n"),
    ("missing-close", ".while 0\n.byte 1\n"),
    ("mixed-close", ".while 0\n.endfor\n"),
    ("missing-condition", ".while\n.endwhile\n"),
    (
        "star-is-not-pc-expression",
        ".while *<3\n.byte 2\n.endwhile\n",
    ),
    (
        "unavailable-condition",
        ".while missing\n.byte 1\n.endwhile\n",
    ),
    ("body-label", ".while 0+1\ninside\n.byte 1\n.endwhile\n"),
    (
        "body-assignment",
        "item .var 0\n.while item<2\nitem .set item+1\n.endwhile\n",
    ),
    ("iteration-limit", ".while 1\n.endwhile\n"),
    (
        "pass-stability",
        ".org 0\n.while $<limit\n.byte 1\n.endwhile\ntarget=2\nlimit=target\n",
    ),
    (
        "transitive-pass-stability",
        ".org 0\na=b\n.while $<a\n.byte 1\n.endwhile\nb=2\n",
    ),
];

fn source(cpu: &str, body: &str) -> String {
    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n")
}

// Give each case an independent address window, avoiding one emulator boot per
// positive case. Only these fixtures' PC expressions are translated to the local
// coordinate; each original case above still has its own live Rust oracle.
fn combined_source(cpu: &str) -> String {
    let mut body = String::new();
    // The isolated origin-boundary case already proves an empty result. Native
    // currently cannot introduce a new discontinuous origin after emitting data.
    for (index, (name, case_body)) in POSITIVE
        .iter()
        .filter(|(name, _)| *name != "org-boundary")
        .enumerate()
    {
        let origin = index * 32;
        let local = case_body
            .replace(".org 0\n", &format!(".org {origin}\n"))
            .replace(".org 10\n", &format!(".org {}\n", origin + 10))
            .replace('$', &format!("($-{origin})"));
        body.push_str(&format!(
            ".namespace {}\n.align 32\n.org {origin}\n{local}.byte {}\n.endnamespace\n",
            name.replace('-', "_"),
            128 + index
        ));
    }
    source(cpu, &body)
}

#[test]
fn native_while_loops_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        assert!(
            oracle(&dir.0, &combined_source(cpu)).is_ok(),
            "combined {cpu}"
        );
        for (name, body) in POSITIVE {
            let bytes = oracle(&dir.0, &source(cpu, body))
                .unwrap_or_else(|error| panic!("Rust oracle {cpu}/{name}: {error}"));
            // Pin the reference's statement-start PC observation explicitly;
            // matching two implementations alone could conceal a joint change.
            match *name {
                "previous-statement-pc" => assert_eq!(bytes, [1, 1, 1, 1]),
                "org-boundary" => assert!(bytes.is_empty()),
                "false-body" => assert_eq!(bytes, [7]),
                "inactive-while" => assert_eq!(bytes, [1, 2]),
                "empty-mixed-loop" => assert_eq!(bytes, [9]),
                _ => {}
            }
            eprintln!("WHILE_ORACLE name={name} cpu={cpu} bytes={bytes:?}");
        }
        for (name, body) in BAD {
            assert!(
                oracle(&dir.0, &source(cpu, body)).is_err(),
                "Rust accepted {cpu}/{name}"
            );
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; binary conditional repetition comparisons"]
fn native_while_loops_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_WHILE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    if let Some(path) = std::env::var_os("OPFORGE_WHILE_SAVE_IMAGE") {
        fs::write(path, &image).unwrap();
    }
    let selection = std::env::var("OPFORGE_WHILE_CASES").unwrap_or_else(|_| "all".into());
    let cpus = std::env::var("OPFORGE_WHILE_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let selected = |name: &str, group: &str| {
        selection == "all"
            || selection
                .split(',')
                .any(|item| item == name || item == group)
    };
    let mut failures = Vec::new();
    let mut attempted = 0;
    for cpu in cpus.split(',').map(str::trim) {
        assert!(matches!(cpu, "68020" | "6502"), "unknown fixture CPU {cpu}");
        if selection.split(',').any(|name| name == "combined-positive") {
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_case(
                    &root,
                    &dir.0,
                    &image,
                    "while-combined-positive",
                    cpu,
                    &combined_source(cpu),
                    false,
                );
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/combined-positive"));
            }
        }
        for (cases, failure, group) in [(POSITIVE, false, "positive"), (BAD, true, "negative")] {
            for (name, body) in cases {
                if selected(name, group) {
                    attempted += 1;
                    if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        run_case(
                            &root,
                            &dir.0,
                            &image,
                            name,
                            cpu,
                            &source(cpu, body),
                            failure,
                        );
                    }))
                    .is_err()
                    {
                        failures.push(format!("{cpu}/{name}"));
                    }
                }
            }
        }
    }
    assert!(attempted > 0, "while selector must match a case");
    assert!(
        failures.is_empty(),
        "while cases failed: {failures:?}; all selected cases were attempted"
    );
}
