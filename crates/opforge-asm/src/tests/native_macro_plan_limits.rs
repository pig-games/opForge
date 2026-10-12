//! Captured ordinary macro lines retain CCR syntax and grow their plan arena.
use super::native_iterable_loops::{build_image, oracle, scratch};
use super::native_source_cases::{self, Case};
use super::*;

const CCR_LINE: &str = "\tmove.w ccr,-(sp)\n";

fn source(lines: usize, invoked: bool) -> String {
    let mut source = String::from(".module main\n.cpu 68020\nSAVE .macro\n");
    for _ in 0..lines {
        source.push_str(CCR_LINE);
    }
    source.push_str(".endmacro\n");
    if invoked {
        source.push_str(".SAVE\n");
    }
    source.push_str(".byte $5a\n.endmodule\n");
    source
}

fn cases() -> Vec<Case> {
    [
        ("ccr-unused", 1, false),
        ("ccr-invoked", 1, true),
        // Captured headers, rows and spelling alone exceed 1 MiB at 24,000
        // lines; 12,000 supplies a smaller control with the same syntax.
        ("plans-under-one-mib", 12_000, false),
        ("plans-over-one-mib", 24_000, false),
    ]
    .into_iter()
    .map(|(name, lines, invoked)| Case {
        name: name.into(),
        cpu: "68020".into(),
        files: vec![("entry.asm", source(lines, invoked))],
        expected: Some(if invoked {
            vec![0x42, 0xe7, 0x5a]
        } else {
            vec![0x5a]
        }),
    })
    .collect()
}

#[test]
fn native_macro_plan_limits_live_rust() {
    let dir = scratch();
    for case in cases() {
        let actual = oracle(&dir.0, &case.files[0].1)
            .unwrap_or_else(|error| panic!("{}: {error}", case.name));
        assert_eq!(Some(actual), case.expected, "{}", case.name);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; captured CCR syntax and plan arena growth"]
fn native_macro_plan_limits_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_MACRO_PLAN_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter = std::env::var("OPFORGE_MACRO_PLAN_CASE").unwrap_or_default();
    let cases = cases()
        .into_iter()
        .filter(|case| filter.is_empty() || filter.split(',').any(|name| name.trim() == case.name))
        .collect::<Vec<_>>();
    assert!(!cases.is_empty(), "no macro plan cases selected");
    native_source_cases::run_source_language_batch(&root, &image, &cases, 1);
}
