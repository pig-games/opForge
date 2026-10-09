//! Scoped condition loops retain fresh locals and completed iteration records.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::native_while_layouts::{hunk_oracle, native_case};
use super::*;

const POSITIVE: &[(&str, &str, &[u8])] = &[
    ("outer-mutation", "n .var 0\n.bwhile n<3\nentry .byte n\nmain.n .set main.n+1\n.endwhile\n.byte n\n", &[0,1,2,3]),
    ("guard-shadow-restoration", "n .var 0\ncount .var 0\n.bwhile n<1&&count<2\nn .var 9\n.byte n\nmain.count .set main.count+1\n.endwhile\n.byte n,count\n", &[9,9,0,2]),
    ("named-members", "start\nn .var 0\nitems: .bwhile n<3\nentry .byte n+10\nvalue .const n+10\nmain.n .set main.n+1\n.endwhile\n.byte items[1]-start,items[2].entry-start,items[0].value,items[2].value\n", &[10,11,12,1,2,10,12]),
    ("shadow-restoration", "value .var 9\nn .var 0\nitems .bwhile n<2\nvalue .var n+2\n.byte value\nvalue .set value+1\n.byte value\nmain.n .set main.n+1\n.endwhile\n.byte value,n,items[0].value,items[1].value\n", &[2,3,3,4,9,2,3,4]),
    ("conditional-alias", "n .var 0\nitems .bwhile n<2\n.if n==0\nvalue .const 10\n.else\nvalue .const 20\n.endif\n.byte value\nmain.n .set main.n+1\n.endwhile\nsaved .const items[1]\n.byte items[0].value,saved.value\n", &[10,20,10,20]),
    ("nested-while", "n .var 0\nm .var 0\nouter .bwhile n<2\nmain.m .set 0\ninner .bwhile m<2\nvalue .const n*10+m\n.byte value\nmain.m .set main.m+1\n.endwhile\nmain.n .set main.n+1\n.endwhile\n.byte outer[0].inner[1].value,outer[1].inner[0].value\n", &[0,1,10,11,1,10]),
    ("while-in-for", "n .var 0\nouter: .bfor item in {2,4}\nmain.n .set 0\ninner .bwhile n<2\nvalue .const item+n\n.byte value\nmain.n .set main.n+1\n.endwhile\n.endfor\n.byte outer[0].inner[1].value,outer[1].inner[0].value\n", &[2,3,4,5,3,4]),
    ("for-in-while", "n .var 0\nouter .bwhile n<2\ninner .bfor item in {2,4}\nvalue .const item+n\n.byte value\n.endfor\nmain.n .set main.n+1\n.endwhile\n.byte outer[0].inner[1].value,outer[1].inner[0].value\n", &[2,4,3,5,4,3]),
    ("zero-and-skipped", "empty .bwhile 0\n.byte missing\n.bwhile 1/0\n.byte missing\n.endwhile\n.endwhile\n.if 0\n.bwhile 1/0\n.byte missing\n.endwhile\n.endif\n.byte 7\n", &[7]),
    ("forward-local", "n .var 0\n.bwhile n<2\n.byte value\nvalue .const n+10\nmain.n .set main.n+1\n.endwhile\n", &[10,11]),
    ("macro-record-member", "items .bfor 1\ninner .bfor 1\nvalue .const 7\n.byte 0\n.endfor\nchild .const inner[0]\nvalue .const 7\n.endfor\nsaved .const items[0]\nsend .macro amount\n.byte .amount,(saved).value,((saved).child).value\n.endmacro\nproject .macro member\n.byte (saved).member .value\n.endmacro\nn .var 0\n.bwhile n<2\n.send n+5\n.project .child\nmain.n .set main.n+1\n.endwhile\n", &[0,5,7,7,7,6,7,7,7]),
    ("macro-body", "send .macro value\n.byte .value\n.endmacro\nn .var 0\n.bwhile n<2\n.send n+5\nmain.n .set main.n+1\n.endwhile\n", &[5,6]),
];

const BAD: &[(&str, &str)] = &[
    ("empty-index", "items .bwhile 0\n.byte 1\n.endwhile\n.byte items[0]\n"),
    ("member-escape", "n .var 0\n.bwhile n<1\nvalue .const 3\nmain.n .set main.n+1\n.endwhile\n.byte value\n"),
    ("missing-member", "n .var 0\nitems .bwhile n<1\nentry .byte 7\nmain.n .set main.n+1\n.endwhile\n.byte items[0].missing\n"),
    ("outer-mutable-not-member", "n .var 0\nitems .bwhile n<1\nentry .byte 7\nmain.n .set main.n+1\n.endwhile\n.byte items[0].n\n"),
    ("unresolved-guard", ".bwhile missing\n.byte 1\n.endwhile\n"),
    ("formal-member-collision", "items .bfor 1\nvalue .const 7\n.byte 0\n.endfor\nsaved .const items[0]\nsend .macro value\n.byte (saved).value\n.endmacro\n.send 5\n"),
    ("wrong-close", ".bwhile 0\n.byte 1\n.endfor\n"),
];

fn source(cpu: &str, body: &str) -> String {
    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n")
}

fn combined() -> (String, Vec<u8>) {
    let mut body = String::new();
    let mut expected = Vec::new();
    for (index, (_, case, bytes)) in POSITIVE.iter().enumerate() {
        let case = case.replace("main.", &format!("main.case_{index}."));
        body.push_str(&format!(".namespace case_{index}\n{case}.endnamespace\n"));
        expected.extend_from_slice(bytes);
    }
    (body, expected)
}

// `$` observes the previous ordinary statement's start, not the emission cursor.
// Compound update operators remain a separate packed-declaration gap.
const NAMED_FORMAL_CONTROL: &str =
    "send .macro value\n.byte .value\n.endmacro\nn .var 0\n.send n+5\n";
const COMPOUND_UPDATE: &str = "n .var 0\n.bwhile n<3\nentry .byte n\nn += 1\n.endwhile\n.byte n\n";
const PC_GUARD: &str = ".org 0\n.bwhile $<1\nentry .byte 4\n.endwhile\n";
const GROWTH: &str = "n .var 0\n.bwhile n<192\nentry .byte n\nmain.n .set main.n+1\n.endwhile\nn .set 0\nitems .bwhile n<64\nentry .byte n\nvalue .const n+1\nmain.n .set main.n+1\n.endwhile\n.byte items[0].value,items[63].value\n";

fn hunk_source(cpu: &str) -> String {
    source(cpu, "n .var 0\n.section data,kind=data\nitems .bwhile n<2\nentry .byte n\nvalue .const n+10\nmain.n .set main.n+1\n.endwhile\nsaved .const items[1]\n.endsection\n.section code,kind=code\n.long items[1],saved.entry,items[0].value,saved.value\n.endsection\n.output \"out.hunk\",format=hunk,sections=code,data\n")
}

#[test]
fn native_scoped_while_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        for (name, body, expected) in POSITIVE {
            assert_eq!(
                oracle(&dir.0, &source(cpu, body)).unwrap_or_else(|e| panic!("{cpu}/{name}: {e}")),
                *expected,
                "{cpu}/{name}"
            );
        }
        let (body, expected) = combined();
        assert_eq!(oracle(&dir.0, &source(cpu, &body)).unwrap(), expected);
        assert_eq!(
            oracle(&dir.0, &source(cpu, NAMED_FORMAL_CONTROL)).unwrap(),
            [5]
        );
        assert_eq!(
            oracle(&dir.0, &source(cpu, COMPOUND_UPDATE)).unwrap(),
            [0, 1, 2, 3]
        );
        assert_eq!(oracle(&dir.0, &source(cpu, PC_GUARD)).unwrap(), [4, 4]);
        let expected: Vec<_> = (0..192).chain(0..64).chain([1, 64]).collect();
        assert_eq!(oracle(&dir.0, &source(cpu, GROWTH)).unwrap(), expected);
        for (name, body) in BAD {
            assert!(
                oracle(&dir.0, &source(cpu, body)).is_err(),
                "Rust accepted {cpu}/{name}"
            );
        }
        let image = hunk_oracle(&dir.0, &hunk_source(cpu));
        let segments = hunk::segments(&image).unwrap();
        assert_eq!(segments.len(), 2);
        let word = |value: u32| {
            if cpu == "6502" {
                value.to_le_bytes()
            } else {
                value.to_be_bytes()
            }
        };
        assert_eq!(
            segments[0].payload,
            [word(1), word(1), word(10), word(11)].concat()
        );
        assert_eq!(segments[0].relocations, [(0, 1), (4, 1)]);
        assert_eq!(segments[1].payload, [0, 1, 0, 0]);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; scoped condition loops and retained records"]
fn native_scoped_while_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_SCOPED_WHILE_IMAGE")
        .map(|p| fs::read(p).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let cpus = std::env::var("OPFORGE_SCOPED_WHILE_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let filter = std::env::var("OPFORGE_SCOPED_WHILE_FILTER").unwrap_or_default();
    let selected =
        |name: &str| filter.is_empty() || filter.split(',').any(|part| name.contains(part.trim()));
    let (body, _) = combined();
    let mut attempted = 0;
    let mut failures = Vec::new();
    for cpu in cpus.split(',').map(str::trim) {
        assert!(matches!(cpu, "68020" | "6502"));
        for (name, body, bad) in std::iter::once(("combined-positive", body.as_str(), false))
            .chain(std::iter::once(("pc-guard", PC_GUARD, false)))
            .chain(std::iter::once(("growth", GROWTH, false)))
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
                    &format!("scoped-while-{cpu}/{name}"),
                    cpu,
                    &source(cpu, body),
                    bad,
                )
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
        if !filter.is_empty() && selected("named-formal-control") {
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_case(
                    &root,
                    &dir.0,
                    &image,
                    &format!("scoped-while-{cpu}/named-formal-control"),
                    cpu,
                    &source(cpu, NAMED_FORMAL_CONTROL),
                    false,
                )
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/named-formal-control"));
            }
        }
        if selected("raw-hunk") {
            attempted += 1;
            let source = hunk_source(cpu);
            let expected = hunk_oracle(&dir.0, &source);
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                native_case(
                    &root,
                    &image,
                    &format!("scoped-while-{cpu}/raw-hunk"),
                    &source,
                    "Work:entry.asm --hunk Work:out.hunk",
                    &expected,
                    false,
                )
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/raw-hunk"));
            }
        }
    }
    assert!(attempted > 0, "filter selected no cases");
    assert!(failures.is_empty(), "scoped while failures: {failures:?}");
}
