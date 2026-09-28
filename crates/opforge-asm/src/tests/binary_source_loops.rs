//! Counted loops are prepared and replayed solely as numeric source records.
use super::*;

const COUNTED: &str = ".cpu m6502\n.org 0\nCOUNT = 2\n.for 0\n.byte 99\n.endfor\n.for 1\n.byte 1\n.endfor\n.for COUNT\n.for 2\n.byte 2\n.endfor\n.endfor\n.end\n";
const MOTOROLA: &str = ".cpu m68020\n.org 0\n.for 4\n rol.l #8,d1\n.byte 7\n.endfor\n.end\n";

fn oracle(source: &str, cpu: &str) -> Vec<u8> {
    let dir = create_temp_dir("compact-counted-loop-oracle");
    let input = dir.join("input.asm");
    let output = dir.join("output.bin");
    fs::write(&input, source).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        cpu.to_string(),
        "--bin".to_string(),
        output.to_string_lossy().into_owned(),
    ]);
    run_with_cli_with_context(&cli).unwrap();
    let bytes = fs::read(output).unwrap();
    fs::remove_dir_all(dir).unwrap();
    bytes
}

#[test]
fn counted_loop_rust_oracle() {
    assert_eq!(oracle(COUNTED, "m6502"), [1, 2, 2, 2, 2]);
    assert_eq!(oracle(MOTOROLA, "m68020").len(), 12);
}

#[test]
#[ignore = "requires configured FS-UAE; counted packed-loop replay"]
fn counted_loop_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for (source, cpu) in [(COUNTED, "m6502"), (MOTOROLA, "m68020")] {
        let expected = oracle(source, cpu);
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let package = prepare_package(&core, &resolved).unwrap();
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            Some(&expected),
        )
        .expect("fresh native counted-loop comparison");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; invalid unscoped loop body"]
fn counted_loop_rejects_label_fs_uae() {
    let source = b".cpu m6502\n.org 0\n.for 1\ninside:\n.byte 1\n.endfor\n.end\n";
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result =
        crate::fs_uae_smoke::run_compact_cli_from_env(&workspace_root(), &package, source, None)
            .expect("fresh native invalid-loop diagnostic");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
}
