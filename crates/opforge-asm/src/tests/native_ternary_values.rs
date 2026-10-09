//! Compact ternary compilation and shape-preserving eager selection.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::native_while_layouts::{hunk_oracle, native_case};
use super::*;

const POSITIVE: &[(&str, &str, &[u8])] = &[
    (
        "scalar",
        r#".for flag in {0,1}
.byte flag ? 17 : 34
.byte flag ? 0 ? 4 : 5 : 1 ? 6 : 7
.byte ((flag<<16)<<16) ? 8 : 9
.byte (flag ? -17 : 4294967296) & 255
.endfor
"#,
        &[34, 6, 9, 0, 17, 5, 8, 239],
    ),
    (
        "lists",
        r#"selected .var 0 ? {2,4} : {9}
saved=selected
selected .set 1 ? {7,8,9} : {3}
.byte .len(saved),saved[0],.len(selected),selected[2]
"#,
        &[1, 9, 3, 9],
    ),
    (
        "mixed",
        r#".bfor flag in {0,1}
choice=flag ? {3,4} : 9
.if flag
.byte .len(choice),choice[1]
.else
.byte choice
.endif
.endfor
"#,
        &[9, 2, 4],
    ),
    (
        "ranges",
        r#"choice=0 ? (1..=3) : (9..3:-2)
.byte .len(choice),choice[1]
.for value in choice
.byte value
.endfor
"#,
        &[3, 7, 9, 7, 5],
    ),
    (
        "records",
        r#"start
groups .bfor item in {2,4}
value .const item+10
.byte item
.endfor
selected .var 0 ? groups[0] : groups[1]
.byte selected.value,selected-start
selected .set 1 ? groups[0] : groups[1]
.byte selected.value,selected-start
.byte (1 ? groups[0] : groups[1]).value
.byte groups[1] ? 77 : 88
"#,
        &[2, 4, 14, 1, 12, 0, 12, 77],
    ),
];
const BAD: &[(&str, &str)] = &[
    ("missing-false", ".byte 1 ? 2 :\n"),
    ("missing-colon", ".byte 1 ? 2\n"),
    ("list-condition", ".byte {1} ? 2 : 3\n"),
    ("eager-error", ".byte 1 ? 2 : 1/0\n"),
    ("eager-symbol", ".byte 1 ? 2 : missing\n"),
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
    source(
        cpu,
        &format!(
            r#".section data,kind=data
payload .long 0
other .long 0
.endsection
.section code,kind=code
codebase
.for flag in {{0,1}}
.long flag ? {item} : other+2
.endfor
.long 1 ? payload+3 : 7
.long 0 ? payload : 5
.long 0 ? payload*2 : other+2
.long 0 ? payload : codebase+4
.endsection
.output "out.hunk",format=hunk,sections=code,data
"#
        ),
    )
}
#[test]
fn native_ternary_values_rust_oracles() {
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
        let expected = [6u32, 1, 3, 5, 6, 4]
            .into_iter()
            .flat_map(|v| {
                if cpu == "6502" {
                    v.to_le_bytes()
                } else {
                    v.to_be_bytes()
                }
            })
            .collect::<Vec<_>>();
        assert_eq!(sections[0].payload, expected);
        assert_eq!(
            sections[0].relocations,
            [(20, 0), (0, 1), (4, 1), (8, 1), (16, 1)]
        );
        assert_eq!(sections[1].payload, [0; 8]);
        assert!(oracle(&dir.0, &hunk_source(cpu, true)).is_err());
    }
}
#[test]
#[ignore = "requires FS-UAE; scalar/compound ternary selection and Hunk provenance"]
fn native_ternary_values_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TERNARY_IMAGE")
        .map(|p| fs::read(p).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter = std::env::var("OPFORGE_TERNARY_FILTER").unwrap_or_default();
    let selected =
        |name: &str| filter.is_empty() || filter.split(',').any(|p| name.contains(p.trim()));
    let cpus = std::env::var("OPFORGE_TERNARY_CPUS").unwrap_or_else(|_| "68020,6502".into());
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
                "hunk-selection"
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
    assert!(attempted > 0, "no ternary cases selected");
    assert!(
        failures.is_empty(),
        "ternary failures: {failures:?}; all selected cases attempted"
    );
}
