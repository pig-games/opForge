//! Signed scalar iterable parity, with explicit bytes and a live Rust oracle.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;

// Emit every bit through legal byte expressions; direct data output can test
// output bounds rather than the complete iterator value.
// Shift counts follow the shared expression contract's five-bit mask, so each
// high-word projection chains shifts below 32 rather than using >>32 or above.
macro_rules! bytes64 {
    ($value:literal) => {
        concat!(
            ".byte (",
            $value,
            ")&255,((",
            $value,
            ")>>8)&255\n",
            ".byte ((",
            $value,
            ")>>16)&255,((",
            $value,
            ")>>24)&255\n",
            ".byte (((",
            $value,
            ")>>16)>>16)&255,((((",
            $value,
            ")>>16)>>16)>>8)&255\n",
            ".byte ((((",
            $value,
            ")>>16)>>16)>>16)&255,(((((",
            $value,
            ")>>16)>>16)>>16)>>8)&255\n"
        )
    };
}

const CASES: &[(&str, &str, &[u8])] = &[
    (
        "signed-limits",
        concat!(".for item in {-1,4294967296,9223372036854775807,-9223372036854775807-1}\n", bytes64!("item"), ".endfor\n"),
        &[
            255,255,255,255,255,255,255,255,
            0,0,0,0,1,0,0,0,
            255,255,255,255,255,255,255,127,
            0,0,0,0,0,0,0,128,
        ],
    ),
    (
        "negative-ranges",
        ".for item in -5..=-1:2\n.byte item&255\n.endfor\n.bfor item in -1..-6:-2\n.byte item&255\n.endfor\n.for item in -2..=2\n.byte item&255\n.endfor\n.bfor item in 2..-3:-1\n.byte item&255\n.endfor\n",
        &[251,253,255,255,253,251,254,255,0,1,2,2,1,0,255,254],
    ),
    (
        "wide-ranges",
        concat!(".for item in 4294967295..=4294967297\n", bytes64!("item"), ".endfor\n.bfor item in -4294967295..=-4294967297:-1\n", bytes64!("item"), ".endfor\n"),
        &[
            255,255,255,255,0,0,0,0,
            0,0,0,0,1,0,0,0,
            1,0,0,0,1,0,0,0,
            1,0,0,0,255,255,255,255,
            0,0,0,0,255,255,255,255,
            255,255,255,255,254,255,255,255,
        ],
    ),
    (
        "wide-step",
        concat!(".for item in -4294967296..=4294967296:4294967296\n", bytes64!("item"), ".endfor\n.bfor item in 4294967296..=-4294967296:-4294967296\n", bytes64!("item"), ".endfor\n"),
        &[
            0,0,0,0,255,255,255,255,
            0,0,0,0,0,0,0,0,
            0,0,0,0,1,0,0,0,
            0,0,0,0,1,0,0,0,
            0,0,0,0,0,0,0,0,
            0,0,0,0,255,255,255,255,
        ],
    ),
    (
        "binding-restoration",
        "item=-4294967297\n.for item in {4294967298,-3}\n.byte item&255,((item>>16)>>16)&255\n.for item in {-4294967292}\n.byte item&255,((item>>16)>>16)&255\n.endfor\n.byte item&255,((item>>16)>>16)&255\n.endfor\n.byte item&255,((item>>16)>>16)&255\n",
        &[2,1,4,255,2,1,253,255,4,255,253,255,255,254],
    ),
    (
        "named-snapshots",
        concat!("items .var {-4294967297,4294967298}\nsaved=items\nitems .set {9223372036854775807,-9223372036854775807-1}\n.for item in saved\n", bytes64!("item"), ".endfor\n.bfor item in items\n", bytes64!("item"), ".endfor\n"),
        &[
            255,255,255,255,254,255,255,255,
            2,0,0,0,1,0,0,0,
            255,255,255,255,255,255,255,127,
            0,0,0,0,0,0,0,128,
        ],
    ),
    (
        "labeled-value-members",
        concat!("items .bfor item in {-4294967297,4294967298}\nvalue .const item\n.byte value&255,((value>>16)>>16)&255\n.endfor\n", bytes64!("items[0].value"), bytes64!("items[1].value"), ".byte items[0].value==-4294967297,items[1].value==4294967298\n"),
        &[255,254,2,1,255,255,255,255,254,255,255,255,2,0,0,0,1,0,0,0,1,1],
    ),
    (
        "conditional-and-macro",
        "emit .macro value\n.byte (.value)&255,(((.value)>>16)>>16)&255\n.endmacro\n.for item in {0,-1,4294967296,-9223372036854775807-1}\n.if item\n.byte 1\n.else\n.byte 0\n.endif\n.emit item\n.endfor\n.bfor item in {-1,4294967296}\n.if item==-1\n.byte 7\n.elseif item==4294967296\n.byte 8\n.else\n.byte 99\n.endif\n.endfor\n",
        &[0,0,0,1,255,255,1,0,1,1,0,0,7,8],
    ),
    (
        "scoped-immutable-snapshot",
        "item=4294967299\n.bfor item in {-4294967297,4294967298}\nlocal .var item\nsaved .const local\nlocal .set 9\n.byte saved&255,((saved>>16)>>16)&255,local\n.bfor item in {-9223372036854775807-1}\n.byte item==-9223372036854775807-1\n.endfor\n.byte item&255,((item>>16)>>16)&255\n.endfor\n.byte item&255,((item>>16)>>16)&255\n",
        &[255,254,9,1,255,254,2,1,9,1,2,1,3,1],
    ),
];

fn source(cpu: &str, cases: &[(&str, &str, &[u8])]) -> String {
    let mut result = format!(".module main\n.cpu {cpu}\n");
    for (name, body, _) in cases {
        result.push_str(&format!(
            ".namespace {}\n{body}.endnamespace\n",
            name.replace('-', "_")
        ));
    }
    result.push_str(".endmodule\n");
    result
}

#[test]
fn native_wide_iterables_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        for case in CASES {
            assert_eq!(
                oracle(&dir.0, &source(cpu, std::slice::from_ref(case)))
                    .unwrap_or_else(|error| panic!("Rust oracle {cpu}/{}: {error}", case.0)),
                case.2,
                "{cpu}/{}",
                case.0
            );
        }
        let expected: Vec<_> = CASES
            .iter()
            .flat_map(|case| case.2.iter().copied())
            .collect();
        assert_eq!(
            oracle(&dir.0, &source(cpu, CASES)).unwrap(),
            expected,
            "{cpu}/combined"
        );
    }
}

#[test]
#[ignore = "requires FS-UAE; signed64 scalar iterable comparison"]
fn native_wide_iterables_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    // Test-only selectors allow a frozen guest image and a reduced diagnostic
    // matrix; the selected source always produces its own live Rust oracle.
    let image = std::env::var_os("OPFORGE_ITERABLE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let selection = std::env::var("OPFORGE_ITERABLE_CASES").unwrap_or_else(|_| "all".into());
    let cases: Vec<_> = CASES
        .iter()
        .copied()
        .filter(|case| {
            selection.split(',').map(str::trim).any(|name| {
                matches!(name, "all" | "positive" | "combined-positive" | "wide") || name == case.0
            })
        })
        .collect();
    assert!(
        !cases.is_empty(),
        "wide iterable selector must match a case"
    );
    let cpu_selection =
        std::env::var("OPFORGE_ITERABLE_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let cpus: Vec<_> = cpu_selection.split(',').map(str::trim).collect();
    assert!(
        cpus.iter().all(|cpu| matches!(*cpu, "68020" | "6502")),
        "wide iterable CPU selector must contain 68020 or 6502"
    );
    let mut failures = Vec::new();
    for cpu in cpus {
        let source = source(cpu, &cases);
        let expected: Vec<_> = cases
            .iter()
            .flat_map(|case| case.2.iter().copied())
            .collect();
        assert_eq!(
            oracle(&dir.0, &source).unwrap(),
            expected,
            "{cpu}/explicit bytes"
        );
        if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            run_case(&root, &dir.0, &image, "wide-iterables", cpu, &source, false);
        }))
        .is_err()
        {
            failures.push(cpu);
        }
    }
    assert!(
        failures.is_empty(),
        "wide iterable cases failed: {failures:?}; all CPUs were attempted"
    );
}
