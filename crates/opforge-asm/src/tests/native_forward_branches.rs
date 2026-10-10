//! Forward branches select mutable effects and immutable count dependencies.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;

const POSITIVE: &[(&str, &str, &[u8])] = &[
    ("scalar-true-snapshot", "n .var 2\nsaved=n\n.if later\nn .set 9\n.byte n\n.else\nn .set 3\n.byte n\n.endif\nlater=1\n.byte saved,n\n", &[9,2,9]),
    ("scalar-false-snapshot", "n .var 2\nsaved=n\n.if later\nn .set 9\n.byte n\n.else\nn .set 3\n.byte n\n.endif\nlater=0\n.byte saved,n\n", &[3,2,3]),
    ("nested-elseif-shortcircuit", "n .var 1\n.if choice==0\nn .set 2\n.elseif choice==1\n.if enabled\nn+=4\n.elseif missing\nn .set missing\n.else\nn .set 7\n.endif\n.else\nn .set 8\n.endif\nchoice=1\nenabled=1\n.byte n\n", &[5]),
    ("list-replacement-snapshot", "items .var {2,3}\nsaved=items\n.if later\nitems .set {8,9}\n.else\nitems .set {10,11}\n.endif\nlater=1\n.byte saved[0],items[1]\n", &[2,9]),
    ("range-replacement-snapshot", "span .var 1..5:2\nsaved=span\n.if later\nspan .set 4..8:2\n.else\nspan .set 10..14:2\n.endif\nlater=0\n.byte saved[0],span[1]\n", &[1,12]),
    ("struct-replacement-snapshot", "Point .struct\nx .byte ?\ny .byte ?\n.endstruct\np .var Point{x:2,y:4}\nsaved=p\n.if later\np .set Point{x:8,y:9}\n.endif\nlater=1\n.byte saved.x,p.y\n", &[2,9]),
    ("branch-declarations", ".if later\nn .var 3\n.else\nn .var 4\n.endif\nlater=1\n.byte n\n", &[3]),
    ("inactive-invalid-values", "n .var 2\n.if later\nn .set missing\n.byte missing\n.else\nn .set 3\n.endif\nlater=0\n.byte n\n", &[3]),
    ("inactive-readonly-replacement", "fixed=1\n.if later\nfixed .set missing\n.else\n.byte 7\n.endif\nlater=0\n.byte fixed\n", &[7,1]),
    ("readonly-struct-control", "Point .struct\nx .byte ?\n.endstruct\np .var Point{x:2}\nsaved=p\n.byte saved.x,p.x\n", &[2,2]),
    ("inactive-readonly-struct", "Point .struct\nx .byte ?\n.endstruct\np .var Point{x:2}\nsaved=p\n.if later\nsaved .set Point{x:8}\n.endif\nlater=0\n.byte saved.x,p.x\n", &[2,2]),
    ("inactive-readonly-struct-two-fields", "Point .struct\nx .byte ?\ny .byte ?\n.endstruct\np .var Point{x:2,y:4}\nsaved=p\n.if later\nsaved .set Point{x:8,y:9}\n.endif\nlater=0\n.byte saved.x,p.x\n", &[2,2]),
    ("expanded-macro-update", "bump .macro\nmain.n+=3\n.endmacro\nn .var 2\n.if later\n.bump\n.endif\nlater=1\n.byte n\n", &[5]),
    ("forward-elseif-true", "n .var 2\nsaved=n\n.if 0\nn .set missing\n.elseif later\nn .set 9\n.byte n\n.else\nn .set 3\n.byte n\n.endif\nlater=1\n.byte saved,n\n", &[9,2,9]),
    ("forward-elseif-false", "n .var 2\n.if 0\nn .set missing\n.elseif later\nn .set 9\n.else\nn+=3\n.endif\nlater=0\n.byte n\n", &[5]),
    ("forward-elseif-chain", "n .var 1\n.if 0\nn .set missing\n.elseif 0\nn .set missing\n.elseif first\nn .set 4\n.elseif second\nn .set 7\n.else\nn .set 9\n.endif\nfirst=0\nsecond=1\n.byte n\n", &[7]),
    ("forward-elseif-no-else", "n .var 2\n.if 0\nn .set missing\n.elseif later\nn .set 9\n.endif\nlater=0\n.byte n\n", &[2]),
    ("forward-elseif-declarations", ".if 0\nn .var missing\n.elseif later\nn .var 3\n.else\nn .var 4\n.endif\nlater=1\n.byte n\n", &[3]),
    ("forward-elseif-compound-snapshot", "items .var {2,3}\nsaved=items\n.if 0\nitems .set missing\n.elseif later\nitems .set {8,9}\n.else\nitems .set {10,11}\n.endif\nlater=1\n.byte saved[0],items[1]\n", &[2,9]),
    ("forward-elseif-parent-restoration", "n .var 1\n.if 1\n.if 0\nn .set missing\n.elseif later\nn+=4\n.else\nn .set 8\n.endif\n.byte n\n.else\n.byte missing\n.endif\nlater=1\n.byte n\n", &[5,5]),
    ("forward-elseif-inactive-parent", ".if 0\n.if 0\n.byte missing\n.elseif missing\n.byte missing\n.else\n.byte missing\n.endif\n.else\n.byte 7\n.endif\n", &[7]),
    ("forward-elseif-short-circuit", ".if 1\n.byte 7\n.elseif missing\n.byte missing\n.else\n.byte missing\n.endif\n", &[7]),
    ("forward-elseif-preprocessor", ".if 0\n.byte missing\n.elseif later\n.ifdef absent\n.byte missing\n.else\n.byte 7\n.endif\n.else\n.byte 8\n.endif\nlater=1\n", &[7]),
    ("forward-elseif-sequential", ".if 0\n.byte missing\n.elseif first\n.byte 3\n.endif\n.if 0\n.byte missing\n.elseif second\n.byte 4\n.else\n.byte 5\n.endif\nfirst=1\nsecond=0\n.byte 6\n", &[3,5,6]),
    ("forward-opening-iterable-loop", ".if later\n.bfor item in {1,2}\n.byte item\n.endfor\n.else\n.byte 8\n.endif\nlater=1\n.byte 9\n", &[1,2,9]),
    ("forward-elseif-scoped-loop", ".if 0\n.byte missing\n.elseif later\n.bfor item in {1,2}\n.if item==1\n.byte 7\n.else\n.byte 8\n.endif\n.endfor\n.else\n.byte missing\n.endif\nlater=1\n.byte 9\n", &[7,8,9]),
    ("forward-opening-counted-loop", ".if later\n.for 2\n.byte 7\n.endfor\n.else\n.byte 8\n.endif\nlater=1\n.byte 9\n", &[7,7,9]),
    ("forward-else-loop-removal", ".if later\n.byte 7\n.else\n.bfor item in {1,2}\n.byte item\n.endfor\n.endif\nlater=1\n.byte 9\n", &[7,9]),
    ("forward-nested-loop-constant", ".if later\n.if enabled\n.bfor item in {1,2}\nvalue .const item+3\n.byte value\n.endfor\n.else\n.byte 8\n.endif\n.endif\nlater=1\nenabled=1\n.byte 9\n", &[4,5,9]),
    ("forward-count-guard-local-constant", ".for count\n.byte 1\n.endfor\ncount=2\n.if enabled\n.bfor item in {1,2}\nvalue .const item+3\n.byte value\n.endfor\n.endif\nenabled=1\n.byte 9\n", &[1,1,4,5,9]),
    ("forward-guard-local-count", ".if enabled\n.for count\n.byte 7\n.endfor\ncount=2\n.endif\nenabled=1\n.byte 9\n", &[7,7,9]),
    ("forward-guard-local-nested-counts", ".if enabled\n.for outer\n.for inner\n.byte 3\n.endfor\n.endfor\nouter=2\ninner=2\n.endif\nenabled=1\n.byte 9\n", &[3,3,3,3,9]),
    ("forward-guard-local-dag-count", ".if enabled\n.for count\n.byte 7\n.endfor\ncount=base+1\nbase=1\n.endif\nenabled=1\n.byte 9\n", &[7,7,9]),
    ("guarded-constant-alternative-true", ".if enabled\nvalue=3\n.else\nvalue=4\n.endif\nenabled=1\n.byte value\n", &[3]),
    ("guarded-constant-alternative-false", ".if enabled\nvalue=missing+1\n.else\nvalue=4\n.endif\nenabled=0\n.byte value\n", &[4]),
    ("guarded-constant-inactive-replacement", "fixed=1\n.if enabled\nfixed=missing+1\n.endif\nenabled=0\n.byte fixed\n", &[1]),
    ("guarded-count-alternative-true", ".if enabled\n.for count\n.byte 7\n.endfor\ncount=2\n.else\n.for count\n.byte 8\n.endfor\ncount=3\n.endif\nenabled=1\n.byte 9\n", &[7,7,9]),
    ("guarded-count-alternative-false", ".if enabled\n.for count\n.byte missing\n.endfor\ncount=missing+1\n.else\n.for count\n.byte 8\n.endfor\ncount=2\n.endif\nenabled=0\n.byte 9\n", &[8,8,9]),
    ("guarded-elseif-constant", ".if 0\nvalue=missing\n.elseif enabled\nvalue=7\n.else\nvalue=missing\n.endif\nenabled=1\n.byte value\n", &[7]),
];

const BAD: &[(&str, &str)] = &[
    ("required-reference-after-branch", ".if later\n.byte missing\n.else\n.byte 1\n.endif\nlater=0\n.byte missing\n"),
    ("active-invalid-value", "n .var 2\n.if later\nn .set missing\n.endif\nlater=1\n.byte n\n"),
    ("unresolved-guard", "n .var 2\n.if missing\nn .set 3\n.else\nn .set 4\n.endif\n.byte n\n"),
    ("active-readonly-replacement", "fixed=1\n.if later\nfixed .set 3\n.endif\nlater=1\n.byte fixed\n"),
    ("active-readonly-struct", "Point .struct\nx .byte ?\n.endstruct\np .var Point{x:2}\nsaved=p\n.if later\nsaved .set Point{x:8}\n.endif\nlater=1\n.byte saved.x\n"),
    ("active-readonly-assignment", "fixed=1\n.if later\nfixed:=3\n.endif\nlater=1\n.byte fixed\n"),
    ("active-readonly-const-directive", "fixed .const 1\n.if later\nfixed .var 3\n.endif\nlater=1\n.byte fixed\n"),
    ("unclosed-branch", "n .var 2\n.if later\nn .set 3\nlater=1\n.byte n\n"),
    ("forward-elseif-unresolved", ".if 0\n.byte 1\n.elseif missing\n.byte 2\n.else\n.byte 3\n.endif\n"),
    ("forward-elseif-readonly", "fixed=1\n.if 0\n.byte 1\n.elseif later\nfixed .set 3\n.endif\nlater=1\n.byte fixed\n"),
    ("forward-elseif-double-else", ".if 0\n.byte 1\n.elseif later\n.byte 2\n.else\n.byte 3\n.else\n.byte 4\n.endif\nlater=1\n"),
    ("forward-elseif-after-else", ".if 0\n.byte 1\n.elseif later\n.byte 2\n.else\n.byte 3\n.elseif 1\n.byte 4\n.endif\nlater=1\n"),
    ("guarded-constant-selected-replacement", "fixed=1\n.if enabled\nfixed=2\n.endif\nenabled=1\n.byte fixed\n"),
    ("guarded-constant-selected-duplicate", ".if enabled\nvalue=2\nvalue=3\n.endif\nenabled=1\n.byte value\n"),
    ("guarded-constant-selected-cycle", ".if enabled\nleft=right+1\nright=left+1\n.for left\n.byte 7\n.endfor\n.endif\nenabled=1\n"),
    ("guarded-count-selected-missing", ".if enabled\ncount=missing+1\n.for count\n.byte 7\n.endfor\n.endif\nenabled=1\n"),
];

fn source(cpu: &str, name: &str, body: &str) -> String {
    let body = body.replace("main.", &format!("{name}."));
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
fn native_forward_branches_rust_oracles() {
    let dir = scratch();
    let mut failures = Vec::new();
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
            let actual = oracle(&dir.0, &source(cpu, "main", body));
            if actual.is_ok() {
                failures.push(format!("{cpu}/{name}: {actual:?}"));
            }
        }
    }
    assert!(
        failures.is_empty(),
        "forward branch oracle gaps: {}",
        failures.join(", ")
    );
}

#[test]
#[ignore = "requires configured FS-UAE; selected branch effects and constant/count replay"]
fn native_forward_branches_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_FORWARD_BRANCH_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter = std::env::var("OPFORGE_FORWARD_BRANCH_FILTER").unwrap_or_default();
    let selected =
        |name: &str| filter.is_empty() || filter.split(',').any(|item| name == item.trim());
    let cpus = std::env::var("OPFORGE_FORWARD_BRANCH_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let mut failures = Vec::new();
    let mut attempted = 0;
    for cpu in cpus.split(',').map(str::trim) {
        assert!(matches!(cpu, "68020" | "6502"));
        let cases = std::iter::once(("combined", combined(cpu).0, false))
            .chain(
                POSITIVE
                    .iter()
                    .filter(|_| !filter.is_empty())
                    .map(|(name, body, _)| (*name, source(cpu, "main", body), false)),
            )
            .chain(
                BAD.iter()
                    .map(|(name, body)| (*name, source(cpu, "main", body), true)),
            );
        for (name, input, rejected) in cases {
            if !selected(name) {
                continue;
            }
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_case(&root, &dir.0, &image, name, cpu, &input, rejected)
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
    }
    assert!(attempted > 0, "no forward branch cases selected");
    assert!(
        failures.is_empty(),
        "native forward branch gaps: {}",
        failures.join(", ")
    );
}
