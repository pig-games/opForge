//! Package-controlled memory-to-memory transfers used by the scanner.
use super::*;
use vm::binary_source_package::BinarySourcePackage;

const TRANSFERS: &str = " move.w 8(a2),(a1)\n move.l 12(a2),16(a1)\n move.b -3(a4),(a0)+\n move.w (a3),6(a5)\n move.l 4(a6),-(a2)\n move.b 0(a4,d2.l),(a0)+\n move.w -5(a1,d4.w),-(a5)\n";

fn flat_source() -> String {
    format!(".cpu m68020\n.org 0\n{TRANSFERS}.end\n")
}

#[test]
fn compact_memory_move_package_rows() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let wire = prepare_package(&core, &resolved).unwrap();
    let rows = u32::from_be_bytes(wire[16..20].try_into().unwrap()) as usize;
    let move_id = numeric
        .names
        .iter()
        .position(|name| name == "move")
        .unwrap() as u16;
    let count = u32::from_be_bytes(wire[20..24].try_into().unwrap()) as usize;
    let mut barriers = 0;
    let mut sequences = 0;
    for row in wire[rows..rows + count * 32].chunks_exact(32) {
        if u16::from_be_bytes(row[..2].try_into().unwrap()) != move_id
            || row[2] != 1
            || row[3] != 10
        {
            continue;
        }
        // Unsupported PC-tuple recipes retain their barrier and the necessary
        // package class (8 + 1); the usable semantic recipes need no such proof.
        if row[5] == 6 && row[22] == 9 {
            assert_eq!(row[23], 0);
            barriers += 1;
        }
        if row[5] == 9 {
            assert_eq!(&row[22..24], &[0, 0]);
            sequences += 1;
        }
    }
    assert_eq!(barriers, 2);
    assert!(sequences > 0);
}

#[test]
fn compact_memory_move_rust_oracle() {
    let source = flat_source();
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let bytes: Vec<_> = entries.into_iter().map(|(_, byte)| byte).collect();
    assert_eq!(&bytes[..4], &[0x32, 0xaa, 0x00, 0x08]);
    assert_eq!(&bytes[22..26], &[0x10, 0xf4, 0x28, 0x00]);
    assert_eq!(bytes.len(), 30);
}

#[test]
#[ignore = "requires configured FS-UAE; displacement-to-indirect package recipe"]
fn compact_memory_move_displacement_fs_uae() {
    assert_binary_source(
        ".cpu m68020\n.org 0\n move.w 8(a2),(a1)\n.end\n".into(),
        "m68020".into(),
    );
}

#[test]
#[ignore = "requires configured FS-UAE; scanner memory transfer forms"]
fn compact_memory_move_matrix_fs_uae() {
    assert_binary_source(flat_source(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; matching unsupported tuple class retains barrier"]
fn compact_memory_move_pc_barrier_fs_uae() {
    let source = ".cpu m68020\n move.w 8(pc),target\ntarget .word 0\n.end\n";
    let (_, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        None,
    )
    .expect("fresh unsupported PC-tuple rejection");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
    assert!(runs[0].stdout.contains("unsupported or invalid input"));
}

#[test]
#[ignore = "requires configured FS-UAE; memory transfers inside compact CLI Hunk sections"]
fn compact_memory_move_hunk_fs_uae() {
    let source = format!(".module scanner_probe\n.cpu m68020\n.section code, kind=code\n{TRANSFERS} RTS\n.endsection\n.section data, kind=data\n.byte 7\n.endsection\n.section bss, kind=bss\n.res long,3\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n");
    super::hunk_sections::native_hunk_source(&source);
}
