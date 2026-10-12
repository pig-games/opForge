//! Contextual package heads retain ordinary shared directive meaning.
use super::native_active_includes::live_oracle;
use super::native_iterable_loops::{build_image, oracle, scratch};
use super::native_source_cases::{self, Case};

fn cases() -> Vec<Case> {
    let mut cases = Vec::new();
    for cpu in ["68020", "6502"] {
        for metadata in [false, true] {
            let prefix = if metadata {
                ".meta\n.output\n.name \"contextual-head\"\n.endoutput\n.endmeta\n"
            } else {
                ""
            };
            cases.push(Case {
                name: format!("package-output-head-{cpu}-metadata-{metadata}"),
                cpu: cpu.into(),
                files: vec![(
                    "entry.asm",
                    format!(".module app\n.cpu {cpu}\n{prefix}.region image, $1000, $100f, align=1\n.section code, kind=code\n.byte $42\n.endsection\n.place code in image\n.output \"selected.bin\", format=bin, sections=code\n.endmodule\n"),
                )],
                expected: Some(vec![0x42]),
            });
        }
        for body in [
            ".endoutput\n",
            ".section code, kind=code\n.byte 1\n.endsection\n.output \"bad.bin\", format=bin, sections=missing\n",
        ] {
            cases.push(Case {
                name: format!("package-output-head-rejection-{cpu}-{}", cases.len()),
                cpu: cpu.into(),
                files: vec![(
                    "entry.asm",
                    format!(".module app\n.cpu {cpu}\n{body}.endmodule\n"),
                )],
                expected: None,
            });
        }
    }
    cases
}

#[test]
fn package_output_head_rust_oracles() {
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
#[ignore = "requires configured FS-UAE; contextual metadata and ordinary output heads"]
fn package_output_head_fs_uae() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = build_image(&root, &dir.0);
    native_source_cases::run_source_language_batch(&root, &image, &cases(), 8);
}
