//! Definition acquisition/storage through the actual compact CLI.
//! These controls deliberately do not claim connected statement invocation.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;
const CASES: &[(&str, &str)] = &[
 ("unused-typed-body", ".statement UNUSED byte:x word:y\n.byte .x\n.word .y\n.endstatement\n.byte 7\n"),
 ("unused-include-body", ".statement UNUSED str:file\n.include .file\n.endstatement\n.byte 7\n"),
 ("unused-missing-include", ".statement UNUSED\n.include \"missing.inc\"\n.endstatement\n.byte 7\n"),
 ("same-name-overloads", ".statement UNUSED byte:x\n.byte .x\n.endstatement\n.statement UNUSED word:x\n.word .x\n.endstatement\n.byte 7\n"),
 ("inactive-definition", ".if 0\n.statement UNUSED byte:x\n.include \"missing.inc\"\n.endstatement\n.endif\n.byte 7\n"),
 ("empty-definition", ".statement UNUSED\n; no body\n.endstatement\n.byte 7\n"),
];
fn source(body: &str) -> String {
    format!(".module app\n.cpu 68020\n{body}.endmodule\n")
}
#[test]
fn native_statement_storage_live_oracles() {
    let dir = scratch();
    for (_, body) in CASES {
        assert_eq!(oracle(&dir.0, &source(body)).unwrap(), [7]);
    }
}
#[test]
#[ignore = "requires configured FS-UAE; dormant statement definition storage only"]
fn native_statement_storage_fs_uae() {
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
    for (name, body) in CASES {
        run_case(&root, &dir.0, &image, name, "68020", &source(body), false);
    }
}
