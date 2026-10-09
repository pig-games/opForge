//! Constructed lists preserve scalar projections, record members and addresses.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::native_while_layouts::{hunk_oracle, native_case};
use super::*;

const POSITIVE: &[(&str, &str, &[u8])] = &[
    ("record-elements", "start\ngroups .bfor item in {2,4}\nentry .byte item\nvalue .const item+10\n.endfor\ncopies={groups[1],groups[0]}\n.byte .len(copies),copies[0].value,copies[1].value\n.byte copies[0]-start,copies[1].entry-start\n", &[2,4,2,14,12,1,0]),
    ("record-snapshots", "groups .bfor item in {2,4}\nvalue .const item+10\n.byte item\n.endfor\nitems .var {groups[0],groups[1]}\nsaved=items\nitems .set {groups[1]}\n.byte .len(saved),saved[1].value,items[0].value\nitems .set {7,8}\n.byte items[0],.len(items)\n", &[2,4,2,14,14,7,2]),
    ("record-iteration", "start\ngroups .bfor item in {2,4}\n.byte item\n.endfor\nitems={groups[1],groups[0]}\n.for value in items\n.byte value-start\n.endfor\n.bfor value in items\n.byte value-start\n.endfor\n", &[2,4,1,0,1,0]),
];
const BAD: &[(&str, &str)] = &[
    ("nested-declaration", "items={1,{2}}\n.byte .len(items)\n"),
    (
        "nested-update",
        "items .var {1}\nitems .set {{2}}\n.byte .len(items)\n",
    ),
    (
        "range-element-declaration",
        "items={1..3}\n.byte .len(items)\n",
    ),
    ("nested-temporary", ".byte .len({{1,2},{3}})\n"),
];

fn source(cpu: &str, body: &str) -> String {
    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n")
}
fn combined() -> (String, Vec<u8>) {
    let mut body = String::new();
    let mut expected = Vec::new();
    for (index, (_, part, bytes)) in POSITIVE.iter().enumerate() {
        body.push_str(&format!(".namespace case{index}\n{part}.endnamespace\n"));
        expected.extend_from_slice(bytes);
    }
    (body, expected)
}
fn hunk_source(cpu: &str, unsafe_value: bool) -> String {
    let item = if unsafe_value {
        "payload*2"
    } else {
        "payload+1"
    };
    source(cpu, &format!(".section data,kind=data\npayload .long 0\n.endsection\naddresses .var {{{item},payload+2,7}}\nsnapshot=addresses\naddresses .set {{payload+3}}\n.section code,kind=code\n.long snapshot[0],snapshot[1],snapshot[2],addresses[0],snapshot[1]-snapshot[0]\n.for value in snapshot\n.long value\n.endfor\naddresses .set {{8,9}}\nplain={{4,5}}\nspan=1..4\n.long addresses[1],plain[0],.len(span)\n.endsection\n.output \"out.hunk\",format=hunk,sections=code,data\n"))
}

#[test]
fn native_typed_list_values_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        for (name, body, expected) in POSITIVE {
            assert_eq!(
                oracle(&dir.0, &source(cpu, body)).unwrap(),
                *expected,
                "{cpu}/{name}"
            );
        }
        let (body, expected) = combined();
        assert_eq!(oracle(&dir.0, &source(cpu, &body)).unwrap(), expected);
        for (name, body) in BAD {
            assert!(
                oracle(&dir.0, &source(cpu, body)).is_err(),
                "Rust accepted {cpu}/{name}"
            );
        }
        let bytes = hunk_oracle(&dir.0, &hunk_source(cpu, false));
        let sections = hunk::segments(&bytes).unwrap();
        let expected = [1u32, 2, 7, 3, 1, 1, 2, 7, 9, 4, 3]
            .into_iter()
            .flat_map(|value| {
                if cpu == "6502" {
                    value.to_le_bytes()
                } else {
                    value.to_be_bytes()
                }
            })
            .collect::<Vec<_>>();
        assert_eq!(sections[0].payload, expected);
        assert_eq!(sections[0].relocations, [(0, 1), (4, 1), (12, 1)]);
        assert_eq!(sections[1].payload, [0; 4]);
        assert!(sections[1].relocations.is_empty());
        // The bin oracle also builds source-directed Hunk metadata; unsafe
        // address multiplication must fail there rather than lose its origin.
        assert!(oracle(&dir.0, &hunk_source(cpu, true)).is_err());
    }
}

#[test]
#[ignore = "requires FS-UAE; constructed record/address lists and temporary shapes"]
fn native_typed_list_values_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TYPED_LIST_IMAGE")
        .map(|p| fs::read(p).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter = std::env::var("OPFORGE_TYPED_LIST_FILTER").unwrap_or_default();
    let selected =
        |name: &str| filter.is_empty() || filter.split(',').any(|p| name.contains(p.trim()));
    let cpus = std::env::var("OPFORGE_TYPED_LIST_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let (body, _) = combined();
    let mut failures = Vec::new();
    let mut attempted = 0;
    for cpu in cpus.split(',').map(str::trim) {
        assert!(matches!(cpu, "68020" | "6502"));
        for (name, body, rejected) in std::iter::once(("combined", body.as_str(), false))
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
                    name,
                    cpu,
                    &source(cpu, body),
                    rejected,
                )
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
        for unsafe_value in [false, true] {
            let name = if unsafe_value {
                "unsafe-hunk"
            } else {
                "hunk-addresses"
            };
            if !selected(name) {
                continue;
            }
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                let text = hunk_source(cpu, unsafe_value);
                let expected = if unsafe_value {
                    assert!(oracle(&dir.0, &text).is_err());
                    Vec::new()
                } else {
                    hunk_oracle(&dir.0, &text)
                };
                native_case(
                    &root,
                    &image,
                    name,
                    &text,
                    "Work:entry.asm --hunk Work:out.hunk",
                    &expected,
                    unsafe_value,
                );
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
    }
    assert!(attempted > 0, "no typed list cases selected");
    assert!(
        failures.is_empty(),
        "typed list failures: {failures:?}; all selected cases attempted"
    );
}
