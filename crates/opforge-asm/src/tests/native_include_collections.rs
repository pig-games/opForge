//! File acquisition must preserve typed iteration collections, addresses and members.
use super::native_active_includes::{live_oracle, selected_names};
use super::native_iterable_loops::{build_image, scratch};
use super::native_source_cases::{self, Case as SourceCase};
use super::*;

struct Case {
    name: &'static str,
    source: &'static str,
    support: &'static [(&'static str, &'static str)],
    expected: &'static [u8],
}

const CASES: &[Case] = &[
    Case {
        name: "explicit-colon-collection",
        source: r#".module main
.cpu @CPU@
empty: .bfor 0
.include "missing.inc"
.endfor
items: .bfor 2
.include "item.inc"
.endfor
.byte .len(empty),.len(items),items[0].value,items[1].value
.endmodule
"#,
        support: &[("item.inc", ".byte 7\nvalue .const 9\n")],
        expected: &[7, 7, 0, 2, 9, 9],
    },
    Case {
        name: "counted-and-iterable-members",
        source: r#".module main
.cpu @CPU@
start
counted .bfor 2
.include "counted.inc"
.endfor
items .bfor item in {3,5,8}
.include "items.inc"
.endfor
.byte .len(counted),.len(items)
.byte counted[1]-start,items[2]-start
.byte counted[0].entry-start,counted[1].entry-start
.byte counted[1].value,items[0].value,items[2].value
.endmodule
"#,
        support: &[
            ("counted.inc", "entry .byte 7\nvalue .const 17\n"),
            ("items.inc", "entry .byte item\nvalue .const item+10\n"),
        ],
        expected: &[7, 7, 3, 5, 8, 2, 3, 1, 4, 0, 1, 17, 13, 18],
    },
    Case {
        name: "empty-collections",
        source: r#".module main
.cpu @CPU@
empty .bfor 0
.include "missing-for.inc"
.endfor
none .bwhile 0
.include "missing-while.inc"
.endwhile
.byte .len(empty),.len(none),7
.endmodule
"#,
        support: &[],
        expected: &[0, 0, 7],
    },
    Case {
        name: "while-member-snapshots",
        source: r#".module main
.cpu @CPU@
start
n .var 0
items .bwhile n<3
.include "while.inc"
main.n .set main.n+1
.endwhile
.byte .len(items),n
.byte items[1]-start,items[2].entry-start
.byte items[0].value,items[1].value,items[2].value
.endmodule
"#,
        support: &[("while.inc", "entry .byte n\nvalue .const n+10\n")],
        expected: &[0, 1, 2, 3, 3, 1, 2, 10, 11, 12],
    },
    Case {
        name: "nested-member-collections",
        source: r#".module main
.cpu @CPU@
start
outer .bfor item in {2,4}
.include "outer.inc"
.endfor
.byte .len(outer),.len(outer[0].inner)
.byte outer[0].inner[1]-start,outer[1].inner[0]-start
.byte outer[0].inner[1].entry-start,outer[1].inner[0].entry-start
.byte outer[0].inner[1].value,outer[1].inner[0].value
.endmodule
"#,
        support: &[
            (
                "outer.inc",
                "inner .bfor child in {7,8}\n.include \"inner.inc\"\n.endfor\n",
            ),
            ("inner.inc", "entry .byte child\nvalue .const item+child\n"),
        ],
        expected: &[7, 8, 7, 8, 2, 2, 1, 2, 1, 2, 10, 11],
    },
    Case {
        name: "expanded-definition-relative-collections",
        source: r#".module main
.cpu @CPU@
.include "templates/definitions.inc"
.LOAD "parts/item.inc"
.INLINE "parts/item.inc"
.endmodule
"#,
        support: &[
            (
                "templates/definitions.inc",
                r#"LOAD .macro file
start
items .bfor item in {2,4}
.include .file
.endfor
.byte .len(items),items[1]-start,items[1].entry-start,items[1].value
.endmacro
INLINE .segment file
start
items .bfor item in {5,6}
.include .file
.endfor
.byte .len(items),items[1]-start,items[1].entry-start,items[1].value
.endsegment
"#,
            ),
            (
                "templates/parts/item.inc",
                "entry .byte item\nvalue .const item+10\n",
            ),
            ("parts/item.inc", ".byte 99\n"),
        ],
        expected: &[2, 4, 2, 1, 1, 14, 5, 6, 2, 1, 1, 16],
    },
];

fn selected_cases() -> Vec<&'static Case> {
    let names = CASES.iter().map(|case| case.name).collect::<Vec<_>>();
    let selected = selected_names("OPFORGE_INCLUDE_COLLECTION_CASES", &names);
    CASES
        .iter()
        .filter(|case| selected.contains(&case.name))
        .collect()
}

fn selected_cpus() -> Vec<&'static str> {
    selected_names("OPFORGE_INCLUDE_CPUS", &["68020", "6502"])
}

fn files(case: &Case, cpu: &str) -> Vec<(&'static str, String)> {
    std::iter::once(("entry.asm", case.source.replace("@CPU@", cpu)))
        .chain(
            case.support
                .iter()
                .map(|(path, source)| (*path, (*source).into())),
        )
        .collect()
}

#[test]
fn native_include_collections_rust_oracles() {
    for cpu in selected_cpus() {
        for case in selected_cases() {
            let dir = scratch();
            assert_eq!(
                live_oracle(&dir.0, &files(case, cpu)),
                case.expected,
                "{} cpu={cpu}",
                case.name
            );
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; file acquisition and named collection snapshots"]
fn native_include_collections_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TEMPLATE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let cases = selected_cpus()
        .into_iter()
        .flat_map(|cpu| {
            selected_cases().into_iter().map(move |case| SourceCase {
                name: format!("{}-{cpu}", case.name),
                cpu: cpu.into(),
                files: files(case, cpu),
                expected: Some(case.expected.to_vec()),
            })
        })
        .collect::<Vec<_>>();
    native_source_cases::run(&root, &image, &cases);
}
