//! Package member operands remain distinct from shared typed member expressions.
//! Disabled instrumentation calls discriminate the neighboring self-host failure.
use super::native_active_includes::live_oracle;
use super::native_iterable_loops::{build_image, oracle, scratch};
use super::native_source_cases::{self, Case};

fn cases() -> Vec<Case> {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let telemetry =
        std::fs::read_to_string(root.join("native/motorola68000/amigaos/debug/memory_telemetry.i"))
            .unwrap();
    let mut cases: Vec<Case> = [
        ("register", ".MEMORY_FREE d0"),
        ("immediate", ".MEMORY_FREE #0"),
        ("phase", ".MEMORY_PHASE #0"),
        ("two-operands", ".MEMORY_WORK #0, d0"),
        ("register-movea", ".MEMORY_FREE d0\n\tmovea.l 4.w, a6"),
        ("movea", "movea.l 4.w, a6"),
    ]
    .into_iter()
    .map(|(name, call)| Case {
        name: format!("disabled-memory-macro-{name}"),
        cpu: "68020".into(),
        files: vec![
            (
                "entry.asm",
                format!(
                    ".module memory\n.cpu 68020\n.include \"memory_telemetry.i\"\n.pub\n.region image, $1000, $100f, align=1\n.section code, kind=code\nrelease .block\n\t{call}\n\trts\n.bend\n.word release\n.endsection\n.place code in image\n.output \"out.bin\", format=bin, sections=code\n.endmodule\n"
                ),
            ),
            ("memory_telemetry.i", telemetry.clone()),
        ],
        expected: Some(if name.contains("movea") { vec![0x2c, 0x78, 0, 4, 0x4e, 0x75, 0x10, 0] } else { vec![0x4e, 0x75, 0x10, 0] }),
    })
    .collect();
    for (name, body, code) in [
        ("negative-width", "movea.l -4.w,a6", None),
        ("positive-width", "movea.l +4.w,a6", None),
        (
            "upper-width",
            "movea.l 4.W,a6",
            Some(vec![0x2c, 0x78, 0, 4]),
        ),
        (
            "long-width",
            "movea.l 4.l,a6",
            Some(vec![0x2c, 0x79, 0, 0, 0, 4]),
        ),
        (
            "parenthesized-width",
            "movea.l (4).w,a6",
            Some(vec![0x2c, 0x78, 0, 4]),
        ),
        (
            "record-displacement",
            "move.l Frame.Value(a4),d0",
            Some(vec![0x20, 0x2c, 0, 4]),
        ),
        ("record-package-spelling", ".word Frame.w", Some(vec![0, 2])),
        ("unknown-width", "movea.l 4.unknown,a6", None),
        ("truncated-width", "movea.l 4.,a6", None),
    ] {
        cases.push(Case {
            name: format!("contextual-member-{name}"),
            cpu: "68020".into(),
            files: vec![("entry.asm", format!(
                ".module app\n.cpu 68020\nFrame .struct\nPadding .word ?\nw .word ?\nValue .long ?\n.endstruct\n.region image,$1000,$101f,align=1\n.section code,kind=code\n\t{body}\n.endsection\n.place code in image\n.output \"out.bin\",format=bin,sections=code\n.endmodule\n"
            ))],
            expected: code,
        });
    }
    cases
}

#[test]
fn disabled_memory_macro_rust_oracles() {
    for case in cases() {
        let dir = scratch();
        if let Some(expected) = case.expected {
            assert_eq!(live_oracle(&dir.0, &case.files), expected, "{}", case.name);
        } else {
            assert!(oracle(&dir.0, &case.files[0].1).is_err(), "{}", case.name);
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; package members and disabled instrumentation"]
fn disabled_memory_macro_fs_uae() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = build_image(&root, &dir.0);
    native_source_cases::run_source_language_batch(&root, &image, &cases(), 8);
}

#[test]
fn absolute_member_cli_host_assembles() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    assert!(!build_image(&root, &dir.0).is_empty());
}
