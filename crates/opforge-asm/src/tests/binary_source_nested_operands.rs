//! Canonical numeric expression paths traverse packed nested operand structure.
use super::*;

const EXAMPLE: &str =
    include_str!("../../../../examples/motorola68000/68020_full_extension_addressing.asm");

fn variations() -> String {
    let mut body = String::from(
        ".cpu m68020\n.org $1000\nFrame .struct\nPadding .long ?\nValue .long ?\n.endstruct\n",
    );
    for scale in [1, 2, 4, 8] {
        body.push_str(&format!(
            " move.w (4.W,a0,d1.l*{scale}),d0\n move.w ([a3],d2.w*{scale},8.L),d3\n"
        ));
    }
    body.push_str(" move.l Frame.Value(a4),d0\n");
    source(&body)
}

const REJECTIONS: &[&str] = &[
    "move.w (4.W,a0,d1.l*0),d0",
    "move.w (4.W,a0,d1.l*3),d0",
    "move.w (4.W,a0,d1.l*16),d0",
    "move.w (4.W,a0,d1.l*4294967297),d0",
    "move.w (4.W,d0,d1.l*4),d0",
    "move.w (4.W,a0,d1.b*4),d0",
    "move.w ([a3],d2.w*3,8.L),d3",
];

#[test]
fn compact_nested_operand_rust_oracles() {
    assert!(!oracle(&[("main.asm", EXAMPLE)]).unwrap().is_empty());
    assert!(!oracle(&[("main.asm", &variations())]).unwrap().is_empty());
    for body in REJECTIONS {
        let text = source(&format!(".cpu m68020\n {body}\n"));
        assert!(
            oracle(&[("main.asm", &text)]).is_err(),
            "Rust accepted {body}"
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; complete full-extension example"]
fn compact_nested_operand_example_fs_uae() {
    let expected = oracle(&[("main.asm", EXAMPLE)]).unwrap();
    compact_cli_cpu(
        &[("main.asm", EXAMPLE)],
        &[],
        &[],
        Some(&expected),
        false,
        "m68020",
    );
}

#[test]
#[ignore = "requires configured FS-UAE; nested scales and scalar member expressions"]
fn compact_nested_operand_variations_fs_uae() {
    let text = variations();
    let expected = oracle(&[("main.asm", &text)]).unwrap();
    compact_cli_cpu(
        &[("main.asm", &text)],
        &[],
        &[],
        Some(&expected),
        false,
        "m68020",
    );
}

#[test]
#[ignore = "requires configured FS-UAE; nested shape, qualifier and scale rejection"]
fn compact_nested_operand_rejections_fs_uae() {
    for body in REJECTIONS {
        let text = source(&format!(".cpu m68020\n {body}\n"));
        assert!(
            oracle(&[("main.asm", &text)]).is_err(),
            "Rust accepted {body}"
        );
        compact_cli_cpu(&[("main.asm", &text)], &[], &[], None, false, "m68020");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; corrupted numeric path cannot execute"]
fn compact_nested_operand_bad_path_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let mut package = prepare_package(&core, &resolved).unwrap();
    let rows = u32::from_be_bytes(package[16..20].try_into().unwrap()) as usize;
    assert!(rows > 200 && (rows - 200).is_multiple_of(4));
    for step in package[200..rows].chunks_exact_mut(4) {
        step[0] = 255;
    }
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        EXAMPLE.as_bytes(),
        None,
    )
    .expect("fresh rejection of corrupted package paths");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
    assert!(!runs[0].success);
}
