//! Package-controlled memory and register transfers used by the scanner.
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
        // Supported PC-tuple recipes now carry their bounded match and
        // positional fixup in executable sequence rows.
        if row[5] == 6 && row[22] == 9 {
            assert_eq!(row[23], 0);
            barriers += 1;
        }
        if row[5] == 9 {
            assert_eq!(&row[22..24], &[0, 0]);
            sequences += 1;
        }
    }
    assert_eq!(barriers, 0);
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
#[ignore = "requires configured FS-UAE; self-host register-to-stack frontier"]
fn compact_register_transfer_frontier_fs_uae() {
    assert_binary_source(
        ".cpu m68020\n move.l d1,-(sp)\n.end\n".into(),
        "m68020".into(),
    );
}

#[test]
#[ignore = "requires configured FS-UAE; scanner memory transfer forms"]
fn compact_memory_move_matrix_fs_uae() {
    assert_binary_source(flat_source(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; PC-relative memory move uses package fixup"]
fn compact_memory_move_pc_tuple_fs_uae() {
    let source = ".cpu m68020\n move.w 8(pc),target\ntarget .word 0\n.end\n";
    assert_binary_source(source.into(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; memory transfers inside compact CLI Hunk sections"]
fn compact_memory_move_hunk_fs_uae() {
    let source = format!(".module scanner_probe\n.cpu m68020\n.section code, kind=code\n{TRANSFERS} RTS\n.endsection\n.section data, kind=data\n.byte 7\n.endsection\n.section bss, kind=bss\n.res long,3\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n");
    super::hunk_sections::native_hunk_source(&source);
}

// A single name can be either a register or a one-element list. Both directions
// and all data widths must reach package recipes without losing the mask route.
const REGISTER_TRANSFERS: &str = " move.b d1,-(sp)\n move.w d2,-(a3)\n move.l d1,-(sp)\n move.b (sp)+,d1\n move.w (a3)+,d2\n move.l (sp)+,d1\n move.l d3,(a2)\n move.l (a2),d3\n move.l d4,d5\n movem.l d2,-(sp)\n movem.l (sp)+,d2\n movem.w d0/a7,-(a7)\n movem.w (a7)+,d0/a7\n movem.l d2/d2,-(sp)\n movem.l (sp)+,d2/d2\n movea.l (sp)+,a1\n movea.w (sp)+,a1\n add.w d1,-(a2)\n sub.l (a2)+,d0\n";

fn register_source() -> String {
    format!(".cpu m68020\n.org 0\n{REGISTER_TRANSFERS}.end\n")
}

#[test]
fn compact_register_transfer_rust_oracle() {
    let source = register_source();
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let bytes: Vec<_> = entries.into_iter().map(|(_, byte)| byte).collect();
    assert_eq!(
        &bytes[..12],
        &[0x1f, 0x01, 0x37, 0x02, 0x2f, 0x01, 0x12, 0x1f, 0x34, 0x1b, 0x22, 0x1f]
    );
    assert_eq!(bytes.len(), 50);
}

#[test]
#[ignore = "requires configured FS-UAE; ordinary and one-element list ambiguity"]
fn compact_register_transfer_matrix_fs_uae() {
    assert_binary_source(register_source(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; ambiguous register transfers in CLI Hunk sections"]
fn compact_register_transfer_hunk_fs_uae() {
    let source = format!(".module register_probe\n.cpu m68020\n.section code,kind=code\n{REGISTER_TRANSFERS} RTS\n.endsection\n.section data,kind=data\n.byte 7\n.endsection\n.section bss,kind=bss\n.res long,3\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n");
    super::hunk_sections::native_hunk_source(&source);
}

const INVALID_REGISTER_TRANSFERS: &[(&str, &str)] = &[
    (
        ".cpu m68020\n move.l (d0)+,d1\n.end\n",
        "[file 00000001, line 00000002]",
    ),
    (
        ".cpu m68020\n move.l d1,-(d0)\n.end\n",
        "[file 00000001, line 00000002]",
    ),
    (
        ".cpu m68020\n move.l d1,-(sp)\n nop d0\n.end\n",
        "[file 00000001, line 00000003]",
    ),
];

#[test]
fn compact_register_transfer_rejection_oracles() {
    for (source, _) in INVALID_REGISTER_TRANSFERS {
        let rejected = match assemble_source_entries_with_runtime_mode(
            &source.lines().collect::<Vec<_>>(),
            true,
        ) {
            Ok((_, diagnostics)) => !diagnostics.is_empty(),
            Err(_) => true,
        };
        assert!(rejected, "Rust must reject {source}");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; class rejection and shape reset after ambiguity"]
fn compact_register_transfer_rejections_fs_uae() {
    for (source, diagnostic) in INVALID_REGISTER_TRANSFERS {
        assert_native_files_rejection(&[("input.asm", source)], "m68020", Some(diagnostic));
    }
}
