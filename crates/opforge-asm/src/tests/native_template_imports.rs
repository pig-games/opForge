//! Imported templates resolve exposed names to public canonical definitions.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;

fn source(cpu: &str, imports: &str, body: &str, public: bool) -> String {
    let visibility = if public { ".pub" } else { ".priv" };
    format!(
        ".module provider\n.cpu {cpu}\n{visibility}\nforeign .macro value=7\n.byte .value\n.endmacro\ninline .segment value=9\n.byte .value\n.ends\n.endmodule\n.module app\n.cpu {cpu}\n{imports}{body}.endmodule\n"
    )
}

fn positive(cpu: &str) -> String {
    source(
        cpu,
        ".use provider (foreign as renamed, inline as renamed_segment)\n.use provider (foreign) as selected\n",
        // Aliases have distinct leaves, remain visible through lexical parents,
        // and do not capture a local reuse of the original spelling.
        "foreign .macro\n.byte 42\n.endmacro\n.namespace inner\n.byte renamed\n.ReNaMeD 3\n.renamed\n.renamed_segment 4\n.renamed_segment\n.selected.foreign 5\n.endnamespace\nWRAP .macro value\n.renamed .value\n.endmacro\n.WRAP 6\n.foreign\n.byte renamed\n",
        true,
    )
    // The canonical name owns both facets. Their import proxies must remain
    // separate even though both resolve the same exposed alias spelling.
    .replacen(".pub\n", ".pub\nforeign = 11\n", 1)
}

fn negatives(cpu: &str) -> Vec<(&'static str, String)> {
    vec![
        (
            "hidden-original-macro",
            source(
                cpu,
                ".use provider (foreign as renamed)\n",
                ".foreign\n",
                true,
            ),
        ),
        (
            "hidden-original-segment",
            source(
                cpu,
                ".use provider (inline as renamed)\n",
                ".inline\n",
                true,
            ),
        ),
        (
            "private-template-alias",
            source(
                cpu,
                ".use provider (foreign as renamed)\n",
                ".renamed\n",
                false,
            ),
        ),
        (
            "unselected-qualified-template",
            source(
                cpu,
                ".use provider (foreign) as selected\n",
                ".selected.inline\n",
                true,
            ),
        ),
    ]
}

#[test]
fn native_template_imports_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        assert_eq!(
            oracle(&dir.0, &positive(cpu)).unwrap(),
            [11, 3, 7, 4, 9, 5, 6, 42, 11]
        );
        for (name, source) in negatives(cpu) {
            assert!(
                oracle(&dir.0, &source).is_err(),
                "Rust accepted {cpu}/{name}"
            );
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; selected template aliases and visibility"]
fn native_template_imports_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TEMPLATE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter = std::env::var("OPFORGE_TEMPLATE_CASES").unwrap_or_else(|_| "all".into());
    let mut failures = Vec::new();
    let mut attempted = 0;
    for cpu in ["68020", "6502"] {
        let cases = std::iter::once(("lexical-template-aliases", positive(cpu), false)).chain(
            negatives(cpu)
                .into_iter()
                .map(|(name, source)| (name, source, true)),
        );
        for (name, source, bad) in cases {
            if filter != "all" && !filter.split(',').any(|item| item == name) {
                continue;
            }
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_case(&root, &dir.0, &image, name, cpu, &source, bad);
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
    }
    assert!(attempted > 0, "template selector matched no case");
    assert!(failures.is_empty(), "template imports failed: {failures:?}");
}
