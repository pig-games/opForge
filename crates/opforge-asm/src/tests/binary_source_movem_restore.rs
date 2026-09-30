//! MOVEM stack restores used by the prepared expression VM.
use super::*;
use vm::binary_source_package::BinarySourcePackage;

const RESTORE: &str =
    ".cpu m68020\n.org 0\nentry\n movem.l 8(sp),d2-d3\n movem.l (sp),d2-d3\n.end\n";
const CONTROLS: &str =
    ".cpu m68020\n.org 0\nentry\n movem.l d0-d3,-(sp)\n movem.l (sp)+,d0-d1\n.end\n";
const MIXED_MASK: &str = ".cpu m68020\n.org 0\nentry\n movem.l 8(sp),d2/a3\n.end\n";
const BOUNDED_MASK: &str = ".cpu m68020\n.org 0\nentry\n movem.l 8(sp),d2-d3/a0\n.end\n";
const MEMBER_TARGET: &str = ".cpu m68020\n.org 0\nentry\n movem.l d0-d1,(entry).l\n.end\n";

fn hunk_source() -> String {
    ".module probe\n.cpu m68020\n.section code,kind=code\nentry\n movem.l d0-d3,-(sp)\n movem.l 8(sp),d2-d3\n movem.l (sp),d2-d3\n movem.l (sp)+,d0-d1\n rts\n.endsection\n.section data,kind=data\n.byte $aa,$bb,$cc\n.endsection\n.section bss,kind=bss\n.res long,3\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule\n".into()
}

fn rust_bytes(source: &str) -> Vec<u8> {
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn compact_movem_displaced_and_indirect_restore_rust_oracle() {
    assert_eq!(
        rust_bytes(RESTORE),
        [0x4c, 0xef, 0x00, 0x0c, 0x00, 0x08, 0x4c, 0xd7, 0x00, 0x0c]
    );
}

#[test]
fn compact_movem_stack_controls_rust_oracle() {
    assert_eq!(
        rust_bytes(CONTROLS),
        [0x48, 0xe7, 0xf0, 0x00, 0x4c, 0xdf, 0x00, 0x03]
    );
}

#[test]
fn compact_movem_mixed_register_mask_rust_oracle() {
    assert_eq!(rust_bytes(MIXED_MASK), [0x4c, 0xef, 0x08, 0x04, 0, 8]);
    assert_eq!(rust_bytes(BOUNDED_MASK), [0x4c, 0xef, 0x01, 0x0c, 0, 8]);
}

#[test]
fn compact_movem_member_target_rust_oracle() {
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&MEMBER_TARGET.lines().collect::<Vec<_>>(), true)
            .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert!(!entries.is_empty());
}

#[test]
fn compact_movem_restore_uses_executable_mask_projection_rows() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let move_id = numeric
        .names
        .iter()
        .position(|name| name == "movem")
        .unwrap() as u16;
    let qualifier = numeric
        .qualifiers
        .iter()
        .position(|name| name == "l")
        .unwrap() as u8
        + 1;
    let wire = prepare_package(&core, &resolved).unwrap();
    let offset = u32::from_be_bytes(wire[16..20].try_into().unwrap()) as usize;
    let count = u32::from_be_bytes(wire[20..24].try_into().unwrap()) as usize;
    for priority in [25, 33] {
        assert!(
            (0..count).any(|index| {
                let row = offset + index * 32;
                u16::from_be_bytes(wire[row..row + 2].try_into().unwrap()) == move_id
                    && wire[row + 2] == qualifier
                    && wire[row + 3] == 10
                    && u16::from_be_bytes(wire[row + 6..row + 8].try_into().unwrap()) == priority
                    && wire[row + 5] == 9
            }),
            "missing executable MOVEM.L direct_direct row priority {priority}"
        );
    }
    assert!((0..count).all(|index| wire[offset + index * 32 + 5] != 8));
    assert!(wire
        .windows(12)
        .any(|row| { row == [18, 1, 0, 0, 0, 1, 0, 8, 0xff, 0xff, 0, 0] }));
    assert!(wire
        .windows(12)
        .any(|row| { row == [18, 0, 0, 0, 0, 1, 0, 8, 0xff, 0xff, 0, 1] }));
}

#[test]
#[ignore = "requires configured FS-UAE; MOVEM displaced and indirect stack restores"]
fn compact_movem_displaced_and_indirect_restore_fs_uae() {
    assert_binary_source(RESTORE.into(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; established MOVEM stack forms"]
fn compact_movem_stack_controls_fs_uae() {
    assert_binary_source(CONTROLS.into(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; package-mapped mixed-class MOVEM mask"]
fn compact_movem_mixed_register_mask_fs_uae() {
    assert_binary_source(MIXED_MASK.into(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; matching unsupported MOVEM member target"]
fn compact_movem_member_target_barrier_fs_uae() {
    assert_native_files_rejection(
        &[("input.asm", MEMBER_TARGET)],
        "m68020",
        Some("[file 00000001, line 00000004]"),
    );
}

#[test]
#[ignore = "requires configured FS-UAE; overflowing package register ordinal must reject"]
fn compact_movem_overflowed_register_ordinal_fs_uae() {
    assert_eq!(rust_bytes(BOUNDED_MASK), [0x4c, 0xef, 0x01, 0x0c, 0, 8]);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let a0 = numeric.names.iter().position(|name| name == "a0").unwrap() as u16;
    let mut wire = prepare_package(&core, &resolved).unwrap();
    let rows = u32::from_be_bytes(wire[24..28].try_into().unwrap()) as usize;
    let count = u32::from_be_bytes(wire[28..32].try_into().unwrap()) as usize;
    let row = (0..count)
        .map(|index| rows + index * 6)
        .find(|&row| u16::from_be_bytes(wire[row..row + 2].try_into().unwrap()) == a0)
        .unwrap();
    wire[row + 4..row + 6].copy_from_slice(&u16::MAX.to_be_bytes());
    let result = crate::fs_uae_smoke::run_binary_source_rejection_from_env(
        &workspace_root(),
        &wire,
        &[("input.asm", BOUNDED_MASK.as_bytes())],
        Some("[file 00000001, line 00000004]"),
    )
    .expect("fresh native rejection for overflowing package mask index");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
}

#[test]
fn compact_movem_restore_hunk_rust_oracle() {
    let oracle = super::hunk_sections::rust_hunk_source(&hunk_source());
    assert!(oracle
        .windows(6)
        .any(|bytes| bytes == [0x4c, 0xef, 0, 0x0c, 0, 8]));
}

#[test]
#[ignore = "requires configured FS-UAE; mixed MOVEM stack forms in Hunk sections"]
fn compact_movem_restore_hunk_fs_uae() {
    super::hunk_sections::native_hunk_source(&hunk_source());
}
