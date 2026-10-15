//! Raw include reuse must repeat semantics and retain independent reader cursors.
use super::native_active_includes::{live_oracle, run_binary_files, selected_names};
use super::native_iterable_loops::scratch;
use super::*;

type Tree = Vec<(String, String)>;

fn cases() -> Vec<(&'static str, Tree, Vec<u8>)> {
    let repeated = vec![
        ("entry.asm".into(), ".module app\n.cpu 68020\nn .var 1\n.include \"effects.inc\"\n.include \"effects.inc\"\n.include \"empty.inc\"\n.include \"empty.inc\"\n.byte n\n.endmodule\n".into()),
        ("effects.inc".into(), ".byte n\r\napp.n .set app.n+1\r\n.include \"nested/leaf.inc\"".into()),
        ("nested/leaf.inc".into(), ".byte n".into()),
        ("empty.inc".into(), String::new()),
    ];
    let contexts = vec![
        ("entry.asm".into(), ".module app\n.cpu 68020\n.namespace first\nFEATURE=1\n.include \"choice.inc\"\n.endnamespace\n.namespace second\nFEATURE=0\n.include \"choice.inc\"\n.endnamespace\n.endmodule\n".into()),
        ("choice.inc".into(), ".if FEATURE\n.byte 7\n.else\n.byte 9\n.endif\n".into()),
    ];
    let mut main =
        String::from(".module app\n.cpu 68020\n.include \"large.inc\"\n.include \"large.inc\"\n");
    let mut bounded = Vec::new();
    for index in 0..65 {
        let path = format!("part{index}.inc");
        main.push_str(&format!(".include \"{path}\"\n"));
        bounded.push((path, format!(".byte {index}\n")));
    }
    main.push_str(".include \"part0.inc\"\n.include \"part64.inc\"\n.endmodule\n");
    bounded.insert(0, ("entry.asm".into(), main));
    bounded.push((
        "large.inc".into(),
        format!(
            "{}.byte 99\n",
            format!(";{}\n", "x".repeat(254)).repeat(130)
        ),
    ));
    let mut expected = vec![99, 99];
    expected.extend(0..65);
    expected.extend([0, 64]);
    // Four exactly eligible payloads fill the byte budget before the row limit.
    let mut budget = Vec::new();
    let mut main = String::from(".module app\n.cpu 68020\n");
    for index in 0..5 {
        let name = format!("budget{index}.inc");
        main.push_str(&format!(".include \"{name}\"\n"));
        let mut text = format!(".byte {index}\n");
        while text.len() < 32768 {
            let remaining = 32768 - text.len();
            let count = remaining.min(256);
            text.push_str(&format!(";{}\n", "x".repeat(count - 2)));
        }
        budget.push((name, text));
    }
    main.push_str(".include \"budget0.inc\"\n.include \"budget4.inc\"\n.endmodule\n");
    budget.insert(0, ("entry.asm".into(), main));
    vec![
        (
            "repeated-nested-empty-and-partial-lines",
            repeated,
            vec![1, 2, 2, 3, 3],
        ),
        ("same-text-distinct-lexical-values", contexts, vec![7, 9]),
        ("bounded-cache-streaming-fallback", bounded, expected),
        ("payload-budget-fallback", budget, vec![0, 1, 2, 3, 4, 0, 4]),
    ]
}

#[test]
fn include_text_reuse_live_rust_oracles() {
    for (name, tree, expected) in cases() {
        let files = tree
            .iter()
            .map(|(path, text)| (path.as_str(), text.clone()))
            .collect::<Vec<_>>();
        let dir = scratch();
        assert_eq!(live_oracle(&dir.0, &files), expected, "{name}");
    }
}

#[test]
#[ignore = "requires prebuilt OPFORGE_INCLUDE_REUSE_IMAGE and FS-UAE"]
fn include_text_reuse_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let image =
        fs::read(std::env::var_os("OPFORGE_INCLUDE_REUSE_IMAGE").expect("prebuilt image")).unwrap();
    let cases = cases();
    let names = cases.iter().map(|(name, _, _)| *name).collect::<Vec<_>>();
    let selected = selected_names("OPFORGE_INCLUDE_REUSE_CASES", &names);
    let mut failures = Vec::new();
    for (name, tree, expected) in cases {
        if !selected.contains(&name) {
            continue;
        }
        let files = tree
            .iter()
            .map(|(path, text)| (path.as_str(), text.clone()))
            .collect::<Vec<_>>();
        if std::panic::catch_unwind(|| {
            run_binary_files(&root, &image, name, "68020", &files, &expected)
        })
        .is_err()
        {
            failures.push(name);
        }
    }
    assert!(failures.is_empty(), "include reuse failures: {failures:?}");
}
