//! Shared struct construction, schema validation and immutable value snapshots.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;

const POINT: &str = "Point .struct\nx .byte ?\ny .byte ?\n.endstruct\n";
const POSITIVE: &[(&str, &str, &[u8])] = &[
    ("definition-size", ".byte Point\n.if Point\n.byte 3\n.endif\n", &[2,3]),
    ("snapshot", "p .var main.Point{x:24,y:50}\nsaved=p\np .set main.Point{x:41,y:61}\n.byte saved.x,saved.y,p.x,p.y\n", &[24,50,41,61]),
    ("case-order", "p=main.Point{Y:7,X:-17}\n.byte p.x&255,p.Y\n.byte (main.Point{x:3,y:4}).y\n", &[239,7,4]),
    ("type-alias", "Alias=main.Point\np=Alias{x:2,y:5}\n.byte Alias.y,p.x,p.y\n", &[1,2,5]),
    ("ternary", ".bfor flag in {0,1}\np=flag ? main.Point{x:2,y:4} : main.Point{x:7,y:9}\n.byte p.x,p.y\n.endfor\n", &[7,9,2,4]),
    ("macro", "emit .macro point\n.byte (.point).x,(.point).y\n.endmacro\n.emit main.Point{x:5,y:8}\n", &[5,8]),
    ("geometry", "Wide .struct\nfirst .byte ?\nsecond .long ?\nlast .word ?\n.endstruct\np=Wide{last:3,first:1,second:2}\n.byte Wide.first,Wide.second,Wide.last,p.first,p.second,p.last\n", &[0,1,5,1,2,3]),
    ("register-fields", "Registers .struct\na .byte ?\nd0 .byte ?\nsp .byte ?\n.endstruct\np=Registers{sp:3*4,d0:9-7,a:2**3}\n.byte p.a,p.d0,p.sp\n", &[8,2,12]),
    ("empty", "Empty .struct\n.endstruct\np=Empty{}\nsaved=p\n.byte 1\n", &[1]),
];
const BAD: &[(&str, &str)] = &[
    ("unknown-type", "p=Absent{x:1,y:2}\n"),
    ("wrong-type", "NotType=7\np=NotType{x:1,y:2}\n"),
    ("unknown-field", "p=main.Point{x:1,z:2}\n"),
    ("qualified-field", "p=main.Point{x:1,other.y:2}\n"),
    ("duplicate-case", "p=main.Point{x:1,X:2}\n"),
    ("missing-field", "p=main.Point{x:1}\n"),
    ("list-field", "p=main.Point{x:{1},y:2}\n"),
    ("literal-field", "p=main.Point{x:main.Point{x:1,y:2},y:3}\n"),
    ("type-arithmetic", ".byte Point+1\n"),
    ("type-alias-scalar", "Alias=Point\n.byte Alias\n"),
    ("type-ternary-scalar", ".byte (1?Point:Point)\n"),
    ("literal-scalar", "p=main.Point{x:1,y:2}\n.byte p\n"),
    ("literal-length", "p=main.Point{x:1,y:2}\n.byte .len(p)\n"),
    ("missing-member", "p=main.Point{x:1,y:2}\n.byte p.z\n"),
    ("trailing-comma", "p=main.Point{x:1,y:2,}\n"),
    ("missing-colon", "p=main.Point{x 1,y:2}\n"),
];
fn source(cpu: &str, body: &str) -> String {
    format!(".module main\n.cpu {cpu}\n{POINT}{body}.endmodule\n")
}
fn imported_field_source(cpu: &str, referenced: bool) -> String {
    let reference = if referenced {
        ".byte library.entry\n"
    } else {
        ""
    };
    format!(".module library\n.cpu {cpu}\n.pub\nentry .block\n.byte 99\n.bend\n.endmodule\n.module main\n.cpu {cpu}\n.use library\nShape .struct\nentry .byte ?\n.endstruct\np=Shape{{entry:7}}\n{reference}.byte p.entry\n.endmodule\n.end\n")
}
fn combined() -> (String, Vec<u8>) {
    let mut body = String::new();
    let mut bytes = Vec::new();
    for (index, (_, part, expected)) in POSITIVE.iter().enumerate() {
        body.push_str(&format!(".namespace case{index}\n{part}.endnamespace\n"));
        bytes.extend_from_slice(expected);
    }
    (body, bytes)
}
#[test]
fn native_struct_literal_values_rust_oracles() {
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
        assert_eq!(
            oracle(&dir.0, &imported_field_source(cpu, false)).unwrap(),
            [7]
        );
        assert_eq!(
            oracle(&dir.0, &imported_field_source(cpu, true)).unwrap(),
            [99, 0, 7]
        );
    }
}
#[test]
#[ignore = "requires FS-UAE; typed struct construction through shared ExprVM"]
fn native_struct_literal_values_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_STRUCT_IMAGE")
        .map(|p| fs::read(p).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter = std::env::var("OPFORGE_STRUCT_FILTER").unwrap_or_default();
    let selected =
        |name: &str| filter.is_empty() || filter.split(',').any(|p| name.contains(p.trim()));
    let cpus = std::env::var("OPFORGE_STRUCT_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let (body, _) = combined();
    let mut failures = Vec::new();
    let mut attempted = 0;
    for cpu in cpus.split(',').map(str::trim) {
        assert!(matches!(cpu, "68020" | "6502"));
        for (name, referenced) in [
            ("field-key-unused-block", false),
            ("field-key-referenced-block", true),
        ] {
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
                    &imported_field_source(cpu, referenced),
                    false,
                )
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
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
    }
    assert!(attempted > 0, "no struct cases selected");
    assert!(
        failures.is_empty(),
        "struct failures: {failures:?}; all selected cases attempted"
    );
}
