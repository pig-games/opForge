//! Labeled counted/iterable repetition and indexed iteration members.
//! Native artifact expectations are assembled from the same source in each run.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::native_while_layouts::{hunk_oracle, native_case};
use super::*;

const POSITIVE: &[(&str, &str, &[u8])] = &[
    ("counted-address-members", "start\nitems .bfor 2\n.byte 7\nentry .byte 9\nvalue .const 13\n.endfor\n.byte items[0]-start,items[1]-start\n.byte items[1].entry-start,items[0].value,items[1].value\n", &[7,9,7,9,0,2,3,13,13]),
    ("iterable-members", "start\nitems .bfor item in {2,5}\n.byte item\nentry .byte item+1\nvalue .const item+10\n.endfor\n.byte items[1]-start,items[1].entry-start\n.byte items[0].value,items[1].value\n", &[2,3,5,6,2,3,12,15]),
    ("range-and-single", "start\nitems .bfor item in 1..=3:2\nentry .byte item\n.endfor\none .bfor item in {0}\nvalue .const item+6\nentry .byte value\n.endfor\n.byte items[1]-start,one[0]-start,one[0].entry-start,one[0].value\n", &[1,3,6,1,2,2,6]),
    ("nested-labeled", "start\nouter .bfor item in {2,4}\nentry .byte item\ninner .bfor child in {7,8}\nentry .byte child\nvalue .const item+child\n.endfor\n.byte inner[1]-entry,inner[1].entry-entry,inner[1].value\n.endfor\n.byte outer[1]-start,outer[1].entry-start\n", &[2,7,8,2,2,10,4,7,8,2,2,12,6,6]),
    ("qualified-original-binding", "original=19\nitems .bfor original in {2,4}\nvalue .const original\n.byte original,main.original\n.endfor\n.byte original,items[1].value\n", &[2,19,4,19,19,4]),
    ("record-alias", "start\nitems .bfor item in {2,4}\nentry .byte item\nvalue .const item+10\n.endfor\nsaved .const items[0]\n.byte saved-start,(saved).entry-start,(saved).value,items[1].value\n", &[2,4,0,0,12,14]),
    ("nested-member-chain", "outer .bfor item in {2,4}\ninner .bfor child in {7,8}\nvalue .const item+child\n.byte value\n.endfor\n.endfor\n.byte outer[0].inner[1].value,outer[1].inner[0].value\n", &[9,10,11,12,10,11]),
    ("conditional-members", "items .bfor item in {0,1}\n.if item==0\nvalue .const 10\n.else\nvalue .const 20\n.endif\n.byte value\n.endfor\n.byte items[0].value,items[1].value\n", &[10,20,10,20]),
    ("empty-labeled", "empty .bfor 0\n.byte missing\n.endfor\nnone .bfor item in {}\n.byte missing\n.endfor\n.byte 7\n", &[7]),
];

const BAD: &[(&str, &str)] = &[
    (
        "address-index-out-of-bounds",
        "items .bfor 1\n.byte 7\n.endfor\n.byte items[1]\n",
    ),
    (
        "member-index-out-of-bounds",
        "items .bfor 1\nentry .byte 7\n.endfor\n.byte items[1].entry\n",
    ),
    (
        "negative-member-index",
        "items .bfor 1\nentry .byte 7\n.endfor\n.byte items[-1].entry\n",
    ),
    (
        "missing-member",
        "items .bfor 1\nentry .byte 7\n.endfor\n.byte items[0].missing\n",
    ),
    (
        "empty-index",
        "items .bfor 0\nentry .byte 7\n.endfor\n.byte items[0]\n",
    ),
    (
        "member-escape",
        "items .bfor 1\nvalue=3\n.endfor\n.byte value\n",
    ),
    (
        "scalar-iterable",
        "items .bfor item in 3\n.byte item\n.endfor\n",
    ),
];

// Unchanged anonymous scalar workload for isolated before/after measurements.
// Selecting this test fixture never changes production behavior.
const SCALAR_CONTROL: &str = "start\n.bfor item in 0..1024\nentry\n.if item&1\nvalue .const item+3\n.else\nvalue .const item+5\n.endif\n.byte value&255\n.word entry-start\n moveq #item&127,d0\n nop\n.endfor\n";

fn source(cpu: &str, body: &str) -> String {
    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n")
}

fn combined_positive() -> (String, Vec<u8>) {
    let mut body = String::new();
    let mut expected = Vec::new();
    for (index, (_, scenario, bytes)) in POSITIVE.iter().enumerate() {
        let scenario = scenario.replace(
            "main.original",
            &format!("main.named_case_{index}.original"),
        );
        body.push_str(&format!(
            ".namespace named_case_{index}\n{scenario}.endnamespace\n"
        ));
        expected.extend_from_slice(bytes);
    }
    (body, expected)
}

fn raw_hunk(cpu: &str) -> String {
    format!(".module main\n.cpu {cpu}\n.section code,kind=code\nitems .bfor item in {{2,4}}\n.byte item\nentry .byte item+1\nvalue .const item+10\n.endfor\n.long items[1].entry\n.byte items[1].value\n.endsection\n.section data,kind=data\n.byte 9\n.endsection\n.output \"out.hunk\",format=hunk,sections=code,data\n.endmodule\n")
}

fn alias_hunk(cpu: &str) -> String {
    raw_hunk(cpu).replace(
        ".long items[1].entry\n",
        ".long items[1].entry\nsaved .const items[1]\n.long saved+4\n.long (saved).entry+1\n",
    )
}

#[test]
fn native_named_scoped_loops_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        for (name, body, expected) in POSITIVE {
            assert_eq!(
                oracle(&dir.0, &source(cpu, body))
                    .unwrap_or_else(|error| panic!("{cpu}/{name}: {error}")),
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
        let (body, expected) = combined_positive();
        assert_eq!(
            oracle(&dir.0, &source(cpu, &body)).unwrap(),
            expected,
            "{cpu}/combined-positive"
        );
    }
}

#[test]
fn native_named_scoped_loops_hunk_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        let image = hunk_oracle(&dir.0, &raw_hunk(cpu));
        let segments = hunk::segments(&image).unwrap();
        assert_eq!(segments.len(), 2);
        assert_eq!(segments[0].kind, 0x3e9);
        let address = if cpu == "6502" {
            [3, 0, 0, 0]
        } else {
            [0, 0, 0, 3]
        };
        let expected: Vec<_> = [2, 3, 4, 5]
            .into_iter()
            .chain(address)
            .chain([14, 0, 0, 0])
            .collect();
        assert_eq!(segments[0].payload, expected);
        assert_eq!(segments[0].relocations, [(4, 0)]);
        assert_eq!(segments[1].payload, [9, 0, 0, 0]);
        assert!(segments[1].relocations.is_empty());

        let image = hunk_oracle(&dir.0, &alias_hunk(cpu));
        let segments = hunk::segments(&image).unwrap();
        let word = |value: u32| {
            if cpu == "6502" {
                value.to_le_bytes()
            } else {
                value.to_be_bytes()
            }
        };
        let expected: Vec<_> = [2, 3, 4, 5]
            .into_iter()
            .chain(word(3))
            .chain(word(6))
            .chain(word(4))
            .chain([14, 0, 0, 0])
            .collect();
        assert_eq!(segments[0].payload, expected);
        assert_eq!(segments[0].relocations, [(4, 0), (8, 0), (12, 0)]);
        assert_eq!(segments[1].payload, [9, 0, 0, 0]);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; labeled scoped repetition and indexed members"]
fn native_named_scoped_loops_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_NAMED_SCOPED_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter = std::env::var("OPFORGE_NAMED_SCOPED_FILTER").unwrap_or_default();
    let cpus = std::env::var("OPFORGE_NAMED_SCOPED_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let selected =
        |name: &str| filter.is_empty() || filter.split(',').any(|part| name.contains(part.trim()));
    let (combined, _) = combined_positive();
    let mut failures = Vec::new();
    let mut attempted = 0;
    for cpu in cpus.split(',').map(str::trim) {
        assert!(
            matches!(cpu, "68020" | "6502"),
            "unknown named scoped fixture CPU {cpu}"
        );
        for (name, body, bad) in std::iter::once(("combined-positive", combined.as_str(), false))
            .chain(
                POSITIVE
                    .iter()
                    .filter(|_| !filter.is_empty())
                    .map(|(n, b, _)| (*n, *b, false)),
            )
            .chain(BAD.iter().map(|(n, b)| (*n, *b, true)))
        {
            if !selected(name) {
                continue;
            }
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_case(
                    &root,
                    &dir.0,
                    &image,
                    &format!("named-scoped-{cpu}/{name}"),
                    cpu,
                    &source(cpu, body),
                    bad,
                );
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
        for (name, source) in [
            ("raw-hunk", raw_hunk(cpu)),
            ("raw-alias-hunk", alias_hunk(cpu)),
        ] {
            if !selected(name) {
                continue;
            }
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let expected = hunk_oracle(&dir.0, &source);
                native_case(
                    &root,
                    &image,
                    &format!("named-scoped-{cpu}/{name}"),
                    &source,
                    "Work:entry.asm --hunk Work:out.hunk",
                    &expected,
                    false,
                );
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
        if cpu == "68020" && !filter.is_empty() && selected("scalar-control") {
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_case(
                    &root,
                    &dir.0,
                    &image,
                    "named-scoped-scalar-control",
                    cpu,
                    &source(cpu, SCALAR_CONTROL),
                    false,
                );
            }))
            .is_err()
            {
                failures.push("68020/scalar-control".into());
            }
        }
    }
    assert!(
        attempted > 0,
        "named scoped filter selected no cases: {filter}"
    );
    assert!(
        failures.is_empty(),
        "named scoped cases failed: {failures:?}; all selected cases were attempted"
    );
}

// A contiguous dotted name remains a qualified-symbol token in the compact
// frontend. Grouping the base reaches shared postfix parsing; the plain spelling
// is retained separately until qualified/member binding is consolidated.
fn dotted_alias_source(cpu: &str) -> String {
    source(cpu, "items .bfor item in {2,4}\nvalue .const item+10\n.byte item\n.endfor\nsaved .const items[0]\n.byte saved.value\n")
}

#[test]
fn native_named_record_dotted_alias_rust_oracle() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        assert_eq!(
            oracle(&dir.0, &dotted_alias_source(cpu)).unwrap(),
            [2, 4, 12]
        );
    }
}

#[test]
#[ignore = "known compact frontend gap: contiguous dotted record aliases bind as qualified symbols"]
fn native_named_record_dotted_alias_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = build_image(&root, &dir.0);
    for cpu in ["68020", "6502"] {
        run_case(
            &root,
            &dir.0,
            &image,
            "dotted-record-alias",
            cpu,
            &dotted_alias_source(cpu),
            false,
        );
    }
}
