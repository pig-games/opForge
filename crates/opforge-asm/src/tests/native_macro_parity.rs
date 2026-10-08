//! Bounded macro parity matrix. Every native positive consumes a live Rust oracle.
//! Scoped .bfor/.bwhile composition remains future coverage, not a parity claim.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;

struct Case {
    name: &'static str,
    body: &'static str,
    expected: &'static [u8],
}

const POSITIVE: &[Case] = &[
    Case {
        name: "argument-forms-defaults",
        body: "PAIR .macro a,b=2\n.byte .a,.b\n.endmacro\n.macro SINGLE(value=5)\n.byte .value\n.endmacro\n.PAIR 1\n.PAIR(3,4)\n.PAIR 6\n.SINGLE\n.SINGLE(7)\n",
        expected: &[1, 2, 3, 4, 6, 2, 5, 7],
    },
    Case {
        name: "all-positional-substitutions",
        body: "NINE .macro a,b,c,d,e,f,g,h,i\n.byte .1,.2,.3,.4,.5,.6,.7,.8,.9\n.byte @1,@2,@3,@4,@5,@6,@7,@8,@9\n.endmacro\n.NINE 1,2,3,4,5,6,7,8,9\n",
        expected: &[1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    },
    Case {
        name: "positional-sigil-boundaries",
        // Positional sigils consume one digit: @0 is literal, @10 is @1 + "0".
        body: "BOUNDARY .macro first\n.byte \"@0/@10/.10\"\n.byte @10,.10\n.endmacro\n.BOUNDARY 7\n",
        expected: b"@0/70/70\x46\x46",
    },
    Case {
        name: "namespace-shadow-and-fallback",
        body: "CHOOSE .macro\n.byte 1\n.endmacro\nGLOBAL_ONLY .macro\n.byte 4\n.endmacro\n.namespace outer\nCHOOSE .macro\n.byte 2\n.endmacro\nANCESTOR_ONLY .macro\n.byte 5\n.endmacro\n.namespace inner\nCHOOSE .macro\n.byte 3\n.endmacro\n.CHOOSE\n.ANCESTOR_ONLY\n.GLOBAL_ONLY\n.endnamespace\n.CHOOSE\n.namespace sibling\n.CHOOSE\n.ANCESTOR_ONLY\n.GLOBAL_ONLY\n.endnamespace\n.endnamespace\n.CHOOSE\n",
        expected: &[3, 5, 4, 2, 2, 5, 4, 1],
    },
    Case {
        name: "multiple-default-overrides",
        body: "DEFAULTS .macro first=6,second=8\n.byte .first,.{second}\n.endmacro\n.DEFAULTS\n.DEFAULTS 3\n.DEFAULTS(4,9)\n",
        expected: &[6, 8, 3, 8, 4, 9],
    },
    Case {
        name: "nested-fragments-and-text",
        body: "INNER .macro left,right\n.byte .left,.right\n.endmacro\nTEXT .macro n\n.byte \"<.n/@1/.{n}>\"\n.endmacro\nOUTER .macro a,b=2\n.INNER .a,.{b}\n.INNER @1,.2\n.TEXT .a\n.endmacro\nFORWARD .macro x,y\n.INNER .@\n.endmacro\n.OUTER 3,4\n.OUTER 5\n.FORWARD(6,7)\n",
        expected: b"\x03\x04\x03\x04<3/3/3>\x05\x02\x05\x02<5/5/5>\x06\x07",
    },
    Case {
        name: "invocation-hygiene-and-segment",
        body: "LOCAL .macro value\nlocal .const .value\n.byte local\n.endmacro\nINLINE .segment value\n.byte .value\n.endsegment\nfirst .LOCAL 9\nsecond .LOCAL 11\n.word first.local,second.local\n.INLINE 7\n",
        // Word order is selected separately for each package below.
        expected: &[9, 11],
    },
    Case {
        name: "zero-parameter-segment-iterator-shadow",
        body: "item .const 9\nVALUES .segment\n.for item in {2,4,6}\n.byte item\n.endfor\n.byte item\n.ends\n.VALUES\n.byte item\n",
        expected: &[2, 4, 6, 9, 9],
    },
    Case {
        name: "segment-nested-iterator-input-and-restore",
        body: "NESTED .segment\n.for item in {2,4}\n.for item in {item,item+1}\n.byte item\n.endfor\n.byte item\n.endfor\n.ends\n.NESTED\n",
        expected: &[2, 3, 2, 4, 5, 4],
    },
    Case {
        name: "captured-loop-contextual-in",
        body: "in .const 9\nCOLUMN_ZERO .segment\n.for in in {2,4}\n.byte in\n.endfor\n.byte in\n.ends\nINDENTED .segment\n    .for item in {3,5}\n    .byte item\n    .endfor\n.byte in\n.ends\n.COLUMN_ZERO\n.INDENTED\n.byte in\n",
        expected: &[2, 4, 9, 3, 5, 9, 9],
    },
    Case {
        name: "zero-parameter-segment-counted-loop",
        body: "COUNTED .segment\n.for 2\n.byte 7\n.endfor\n.ends\n.COUNTED\n",
        expected: &[7, 7],
    },
    Case {
        name: "macro-loop-condition-composition",
        body: "EMIT .macro value\n.if .value==2\n.byte .value+10\n.elseif .value==4\n.byte .value+20\n.else\n.byte .value+30\n.endif\n.endmacro\nREPEAT .macro count,value\n.for .count\n.EMIT .value\n.endfor\n.endmacro\nFORWARD_LOOP .segment\n.for item in {2,4,6}\n.REPEAT 2,item\n.endfor\n.ends\n.FORWARD_LOOP\n.if 0\n.EMIT missing\n.endif\n",
        expected: &[12, 12, 24, 24, 36, 36],
    },
];

// Existing register/value role and name-collision matrices remain in
// binary_source_macro_calls.rs and binary_source_macro_name_collisions.rs.
// This matrix adds consumed substitution/visibility/loop combinations.
const BAD: &[(&str, &str)] = &[
    (
        "macro-local-declaration-in-unscoped-loop",
        "EMIT .macro\nitem .const 9\n.byte item\n.endmacro\n.for item in {2,4}\n.EMIT\n.byte item\n.endfor\n",
    ),
    (
        "missing-required-argument",
        "PAIR .macro a,b\n.byte .a,.b\n.endmacro\n.PAIR 1\n",
    ),
    (
        "zero-positional-sigil-as-value",
        "ZERO .macro value\n.byte @0\n.endmacro\n.ZERO 7\n",
    ),
    (
        "named-actual-is-not-parameter-binding",
        // Actuals are positional text; `value=7` is not a named binding syntax.
        "NAMED .macro value=2\n.byte .value\n.endmacro\n.NAMED(value=7)\n",
    ),
    ("unclosed-definition", "BROKEN .macro value\n.byte .value\n"),
    ("unmatched-endmacro", ".endmacro\n.byte 1\n"),
    (
        "malformed-call",
        "PAIR .macro a,b\n.byte .a,.b\n.endmacro\n.PAIR(1,2\n",
    ),
    ("direct-recursion", "SELF .macro\n.SELF\n.endmacro\n.SELF\n"),
    (
        "mutual-recursion",
        "LEFT .macro\n.RIGHT\n.endmacro\nRIGHT .macro\n.LEFT\n.endmacro\n.LEFT\n",
    ),
];

fn source(cpu: &str, body: &str) -> String {
    format!(".module entry\n.cpu {cpu}\n{body}.endmodule\n")
}

fn combined_positive_source(cpu: &str) -> String {
    let mut body = String::new();
    for case in POSITIVE {
        let namespace = case.name.replace('-', "_");
        body.push_str(&format!(
            ".namespace {namespace}\n{}.endnamespace\n",
            case.body
        ));
    }
    source(cpu, &body)
}

fn visibility_source(cpu: &str, private: bool) -> String {
    let visibility = if private { ".priv" } else { ".pub" };
    format!(
        ".module library\n.cpu {cpu}\n{visibility}\nEMIT .macro value\n.byte .value\n.endmacro\n.endmodule\n.module entry\n.cpu {cpu}\n.use library as lib\n.lib.EMIT 13\n.endmodule\n"
    )
}

fn expected(case: &Case, cpu: &str) -> Vec<u8> {
    let mut bytes = case.expected.to_vec();
    if case.name == "invocation-hygiene-and-segment" {
        bytes.extend_from_slice(if cpu == "68020" {
            &[0, 9, 0, 11, 7]
        } else {
            &[9, 0, 11, 0, 7]
        });
    }
    bytes
}

fn combined_positive_expected(cpu: &str) -> Vec<u8> {
    POSITIVE
        .iter()
        .flat_map(|case| expected(case, cpu))
        .collect()
}

#[test]
fn native_macro_parity_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        for case in POSITIVE {
            assert_eq!(
                oracle(&dir.0, &source(cpu, case.body)).unwrap(),
                expected(case, cpu),
                "{cpu}/{}",
                case.name
            );
        }
        assert_eq!(
            oracle(&dir.0, &combined_positive_source(cpu)).unwrap(),
            combined_positive_expected(cpu),
            "{cpu}/combined-positive"
        );
        assert_eq!(
            oracle(&dir.0, &visibility_source(cpu, false)).unwrap(),
            [13]
        );
        assert!(
            oracle(&dir.0, &visibility_source(cpu, true)).is_err(),
            "private macro accepted: {cpu}"
        );
        for (name, body) in BAD {
            assert!(
                oracle(&dir.0, &source(cpu, body)).is_err(),
                "Rust accepted {cpu}/{name}"
            );
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; complete macro matrix with fresh live Rust oracles"]
fn native_macro_parity_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_MACRO_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter = std::env::var("OPFORGE_MACRO_FILTER").unwrap_or_default();
    let cpus = std::env::var("OPFORGE_MACRO_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let mut failures = Vec::new();
    let mut attempted = 0;
    let mut check = |name: &str, cpu: &str, source: String, bad: bool| {
        if !filter.is_empty() && !filter.split(',').any(|item| name.contains(item)) {
            return;
        }
        attempted += 1;
        // Attempt every selected case; preserve each runner's strict assertions.
        if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            run_case(&root, &dir.0, &image, name, cpu, &source, bad);
        }))
        .is_err()
        {
            failures.push(format!("{cpu}/{name}"));
        }
    };
    for cpu in cpus.split(',').map(str::trim) {
        assert!(
            matches!(cpu, "68020" | "6502"),
            "unknown macro fixture CPU {cpu}"
        );
        for case in POSITIVE {
            check(case.name, cpu, source(cpu, case.body), false);
        }
        check(
            "combined-positive",
            cpu,
            combined_positive_source(cpu),
            false,
        );
        check(
            "public-qualified-macro",
            cpu,
            visibility_source(cpu, false),
            false,
        );
        check(
            "private-qualified-macro",
            cpu,
            visibility_source(cpu, true),
            true,
        );
        for (name, body) in BAD {
            check(name, cpu, source(cpu, body), true);
        }
    }
    assert!(attempted > 0, "macro filter selected no cases: {filter}");
    assert!(failures.is_empty(), "macro cases failed: {failures:?}");
}
