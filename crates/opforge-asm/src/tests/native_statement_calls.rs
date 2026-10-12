//! Connected compact CLI statement invocation; every source has a live Rust oracle.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;
const CASES: &[(&str, &str, &[u8])] = &[
    ("statement-packed-macro-no-definitions", "PLAIN .macro\n.byte 3\n.endmacro\n .PLAIN\n", &[3]),
    ("statement-byte", ".statement LOAD byte:x\n.byte .x\n.endstatement\n LOAD 7\n", &[7]),
    ("statement-word-overload", ".statement LOAD byte:x\n.byte 1\n.endstatement\n.statement LOAD word:x\n.byte 2\n.endstatement\n LOAD 256\n", &[2]),
    ("statement-caller-label", ".statement LOAD byte:x\n.byte .x\n.endstatement\nentry LOAD 7\n.word entry\n", &[7,0,0]),
    ("statement-empty-string", ".statement LOAD str:x\n.byte 9\n.endstatement\n LOAD \"\"\n", &[9]),
    ("statement-duplicate-capture", ".statement LOAD byte:x byte:x\n.byte .x\n.byte @1,@2\n.endstatement\n LOAD 7 8\n", &[8,7,8]),
    ("statement-text-substitution", ".statement LOAD str:x\n.byte \".x:@1\"\n.endstatement\n LOAD \"Hi\"\n", b"Hi:Hi"),
    ("statement-decoded-expression", ".statement LOAD str:x\n.byte .x\n.endstatement\n LOAD \"1+2\"\n", &[3]),
    ("statement-numeric-fallback", ".statement LOAD char:x byte:y\n.byte .y\n.endstatement\n LOAD d0\n", &[0]),
    ("statement-numeric-suffix", ".statement LOAD byte:x\n.byte .x\n.endstatement\n LOAD7\n", &[7]),
    ("statement-macro-coexist", ".statement LOAD byte:x\n.byte 1\n.endstatement\nLOAD .macro x\n.byte 2\n.endmacro\n LOAD 7\n.LOAD 8\n", &[1,2]),
];
// Definitions may be captured before their callee exists; replay must retain
// lexical facets and resolve through the same caller-visible statement vectors.
const EXPANDED_CASES: &[(&str, &str, &[u8])] = &[
    ("statement-expanded-macro-forward-formal", "WRAP .macro x\n LOAD .x\n.endmacro\n.statement LOAD byte:x\n.byte .x\n.endstatement\n .WRAP 7\n", &[7]),
    ("statement-expanded-segment-forward-formal", "WRAP .segment x\n LOAD .x\n.endsegment\n.statement LOAD byte:x\n.byte .x\n.endstatement\n .WRAP 8\n", &[8]),
    ("statement-expanded-statement-forward-formal", ".statement OUT byte:x\n LOAD .x\n.endstatement\n.statement LOAD byte:x\n.byte .x\n.endstatement\n OUT 9\n", &[9]),
    ("statement-expanded-keyword-adjacency", "WRAP .macro\n LOAD7\n LOAD 8\n.endmacro\n.statement LOAD byte:x\n.byte .x\n.endstatement\n .WRAP\n", &[7,8]),
    ("statement-expanded-numeric-literal-spelling", "WRAP .macro x\n LOAD .x\n.endmacro\n.statement LOAD \"0B8H\"\n.byte 9\n.endstatement\n .WRAP 0B8H\n", &[9]),
    ("statement-expanded-decoded-text-formal", "WRAP .macro x\n LOAD .x\n.endmacro\n.statement LOAD str:x\n.byte \".x\"\n.endstatement\n .WRAP \"Hi\"\n", b"Hi"),
    ("statement-expanded-segment-empty-label", "WRAP .segment\n EMPTY\n.byte 9\n.endsegment\n.statement EMPTY\n.endstatement\nentry .WRAP\n.word entry\n", &[9,0,0]),
    ("statement-expanded-named-macro-body-label", "WRAP .macro\ninside LOAD 7\n.endmacro\n.statement LOAD byte:x\n.byte .x\n.endstatement\nentry .WRAP\n.word entry.inside\n", &[7,0,0]),
    ("statement-expanded-inactive-unmatched-call", "WRAP .macro\n.if 0\n LOAD \"not a number\"\n.endif\n.byte 5\n.endmacro\n.statement LOAD byte:x\n.byte .x\n.endstatement\n .WRAP\n", &[5]),
    ("statement-expanded-imported-body-caller-scope", ".use library(OUT)\n.statement INNER byte:x\n.byte .x+1\n.endstatement\n OUT 7\n", &[8]),
];
// Complete same-source graphs exercise source acquisition and .use processing,
// beyond the direct statement exposure metadata controls. None means rejection.
const GRAPH_CASES: &[(&str, &str, Option<&[u8]>)] = &[
    (
        "statement-import-qualified-alias",
        ".use foo.bar as q\n q.LOAD 1\n",
        Some(&[7]),
    ),
    (
        "statement-import-selected-bare",
        ".use foo.bar(LOAD)\n LOAD 1\n",
        Some(&[7]),
    ),
    (
        "statement-import-selected-alias",
        ".use foo.bar(LOAD as RENAMED)\n RENAMED 1\n",
        Some(&[7]),
    ),
    (
        "statement-import-wildcard-and-full-qualifier",
        ".use foo.bar\n foo.bar.LOAD 1\n.use foo.bar(*)\n LOAD 2\n",
        Some(&[7, 7]),
    ),
    (
        "statement-import-no-implicit-leaf-alias",
        ".use foo.bar\n bar.LOAD 1\n",
        None,
    ),
    (
        "statement-import-private-rejected",
        ".use foo.bar(HIDDEN)\n HIDDEN 1\n",
        None,
    ),
    (
        "statement-import-replacement-and-local-overload",
        ".use foo.bar(*)\n.use other(*)\n.priv\n.statement LOAD str:x\n.byte 8\n.endstatement\n LOAD 1\n LOAD \"xx\"\n",
        Some(&[9, 8]),
    ),
    (
        "statement-sibling-block-ambiguity",
        "a .block\n.statement T byte:x\n.byte 1\n.endstatement\n.bend\nb .block\n.statement T byte:y\n.byte 2\n.endstatement\n T 1\n.bend\n",
        None,
    ),
];
fn source(body: &str) -> String {
    format!(".module app\n.cpu 68020\n{body}.endmodule\n")
}
fn graph_source(body: &str) -> String {
    format!(
        "{}\
.module foo.bar\n.pub\n.statement LOAD byte:x\n.byte 7\n.endstatement\n\
.priv\n.statement HIDDEN byte:x\n.byte 6\n.endstatement\n.endmodule\n\
.module other\n.pub\n.statement LOAD byte:x\n.byte 9\n.endstatement\n.endmodule\n",
        source(body)
    )
}
fn expanded_source(body: &str) -> String {
    format!(
        "{}\n.module library\n.pub\n.statement OUT byte:x\n INNER .x\n.endstatement\n.endmodule\n",
        source(body)
    )
}
#[test]
fn native_statement_calls_live_oracles() {
    let dir = scratch();
    for (name, body, bytes) in CASES {
        assert_eq!(oracle(&dir.0, &source(body)).unwrap(), *bytes, "{name}");
    }
    for (name, body, bytes) in EXPANDED_CASES {
        assert_eq!(
            oracle(&dir.0, &expanded_source(body)).unwrap(),
            *bytes,
            "{name}"
        );
    }
    for (name, body, bytes) in GRAPH_CASES {
        let result = oracle(&dir.0, &graph_source(body));
        match bytes {
            Some(bytes) => assert_eq!(result.unwrap(), *bytes, "{name}"),
            None => assert!(
                result.is_err(),
                "{name}: expected rejection, got {result:?}"
            ),
        }
    }
}
#[test]
#[ignore = "requires configured FS-UAE; actual compact CLI statement calls"]
fn native_statement_calls_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TEMPLATE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    if let Some(path) = std::env::var_os("OPFORGE_STATEMENT_SAVE_IMAGE") {
        fs::write(path, &image).unwrap();
    }
    let selected = std::env::var("OPFORGE_STATEMENT_CASE").ok();
    let cases = CASES
        .iter()
        .map(|(name, body, _)| (*name, source(body), false))
        .chain(
            GRAPH_CASES
                .iter()
                .map(|(name, body, bytes)| (*name, graph_source(body), bytes.is_none())),
        )
        .chain(
            EXPANDED_CASES
                .iter()
                .map(|(name, body, _)| (*name, expanded_source(body), false)),
        );
    let mut ran = 0;
    for (name, source, failure) in cases {
        if selected.as_deref().is_some_and(|selected| {
            selected != name
                && !(selected == "graph" && GRAPH_CASES.iter().any(|case| case.0 == name))
                && !(selected == "expanded" && EXPANDED_CASES.iter().any(|case| case.0 == name))
        }) {
            continue;
        }
        run_case(&root, &dir.0, &image, name, "68020", &source, failure);
        ran += 1;
    }
    assert!(ran > 0, "statement selection did not execute any cases");
}
