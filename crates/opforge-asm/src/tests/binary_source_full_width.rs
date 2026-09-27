//! Full-width scalar meaning must survive folding, symbols and module parameters.
use super::*;

const LITERALS: &str = ".cpu m68020\n.long $7fffffff,$80000000,$fffffffe,$ffffffff\n.long 2147483648,4_294_967_294,0x80000000,0XFF_FF_FF_FF\n.long %11111111111111111111111111111110\n.long +$ffffffff,-$ffffffff,~$fffffffe\n.long ($7fffffff+1)-$7fffffff,(50000*50000)*0\n.long -(-$7fffffff-1)+(-$7fffffff-1)\n.end\n";
const SYMBOLS: &str = ".module app\n.cpu m68020\nmask=$fffffffe\nwide=$ffffffff+1\nnegative=-$ffffffff\nforward=late/2\n.if wide\n.long mask/2,mask>>1\n.else\n.long 99\n.endif\nlate=$fffffffe\n.long mask,wide,negative,forward\n.long (wide+5)/wide,negative/3,~mask\n.if 0\n.long 99\n.elseif negative-1\n.long 7\n.else\n.long 88\n.endif\nemit .macro value\n.long .value/2\n.endmacro\n.emit mask\n\tandi.l #mask,d1\n.endmodule\n.end\n";
const MAIN: &str = ".module app\n.cpu m68020\n.use dep with (Mask=$fffffffe,Wide=$ffffffff+1,Negative=-$ffffffff)\n.long dep.item\n.endmodule\n.end\n";
const DEP: &str = ".module dep\n.cpu m68020\n.pub\nitem .block\n.if Wide\n.long Mask/2,Negative/3,Wide+5\n.else\n.long 99\n.endif\n.bend\n.endmodule\n.end\n";

fn oracle(files: &[(&str, &str)], roots: &[&str]) -> Vec<u8> {
    graph::oracle_with_roots(files, roots).unwrap()
}

fn longs(values: &[u32]) -> Vec<u8> {
    values
        .iter()
        .flat_map(|value| value.to_be_bytes())
        .collect()
}

#[test]
fn compact_full_width_rust_oracles() {
    assert_eq!(
        oracle(&[("input.asm", LITERALS)], &[]),
        longs(&[
            0x7fffffff, 0x80000000, 0xfffffffe, 0xffffffff, 0x80000000, 0xfffffffe, 0x80000000,
            0xffffffff, 0xfffffffe, 0xffffffff, 1, 1, 1, 0, 0,
        ])
    );
    let mut expected = longs(&[
        0x7fffffff, 0x7fffffff, 0xfffffffe, 0, 1, 0x7fffffff, 1, 0xaaaaaaab, 1, 7, 0x7fffffff,
    ]);
    expected.extend([0x02, 0x81, 0xff, 0xff, 0xff, 0xfe]);
    assert_eq!(oracle(&[("input.asm", SYMBOLS)], &[]), expected);
    assert_eq!(
        oracle(
            &[("main.asm", MAIN), ("library/dep.asm", DEP)],
            &["library"]
        ),
        longs(&[0x7fffffff, 0xaaaaaaab, 5, 0])
    );
}

fn native(files: &[(&str, &str)], roots: &[&str]) {
    let expected = oracle(files, roots);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let inputs: Vec<_> = files
        .iter()
        .map(|(path, source)| (*path, source.as_bytes()))
        .collect();
    let outcome = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        &inputs,
        roots,
        &[],
        Some(&expected),
        false,
    )
    .expect("fresh full-width scalar comparison");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.success && run.protocol_completed);
    assert_eq!(run.exit_code, Some(0));
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let words: Vec<_> = run.captured_artifacts[&PathBuf::from("Work/memory.bin")]
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect();
        assert_eq!(words[0], 0x4d454d36);
        assert_eq!(words[1], 0);
        assert_eq!(words[3], words[4]);
        assert_eq!(words[11], 0);
        assert_eq!(words[29], 0);
        eprintln!("COMPACT_FULL_WIDTH peak_owned_bytes={}", words[2]);
    }
    eprintln!(
        "COMPACT_FULL_WIDTH output_bytes={} seconds={:?}",
        expected.len(),
        run.start_to_done_host_seconds
    );
}

#[test]
#[ignore = "requires configured FS-UAE; positive u32 and computed i64 literals"]
fn compact_full_width_literals_fs_uae() {
    native(&[("input.asm", LITERALS)], &[]);
}

#[test]
#[ignore = "requires configured FS-UAE; scalar storage, forward constants, macro and mask encoding"]
fn compact_full_width_symbols_fs_uae() {
    native(&[("input.asm", SYMBOLS)], &[]);
}

#[test]
#[ignore = "requires configured FS-UAE; full-width incoming module parameters and conditional truth"]
fn compact_full_width_parameters_fs_uae() {
    native(
        &[("main.asm", MAIN), ("library/dep.asm", DEP)],
        &["library"],
    );
}

#[test]
#[ignore = "requires configured FS-UAE; source numeric payload remains bounded to u32"]
fn compact_full_width_scanner_bound_fs_uae() {
    for value in [
        "4294967296",
        "$100000000",
        "%100000000000000000000000000000000",
    ] {
        let source = format!(".cpu m68020\n.long {value}\n.end\n");
        assert!(!oracle(&[("input.asm", &source)], &[]).is_empty());
        assert_native_rejection(&source, "m68020");
    }
}
