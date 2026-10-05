//! Qualified absolute-memory operands from the tokenizer runtime.
use super::*;

fn source(body: &str) -> String {
    format!(".module probe\n.cpu m68020\n.use state\nLimit=8\n.section code,kind=code\n{body} rts\n.endsection\n.section data,kind=data\n.byte 7\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n.module state\n.cpu m68020\n.pub\n.section bss,kind=bss\nStart .res word,1\nCount .res long,1\nPointer .res long,1\n.res word,1\n.endsection\n.endmodule\n")
}

#[test]
fn compact_absolute_state_frontier_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&source(" move.w state.Start,d0\n"));
    assert!(oracle.windows(2).any(|bytes| bytes == [0x30, 0x39]));
}

#[test]
#[ignore = "requires configured FS-UAE; qualified absolute state load"]
fn compact_absolute_state_frontier_fs_uae() {
    hunk_sections::native_hunk_source(&source(" move.w state.Start,d0\n"));
}

// These forms occur together in the tokenizer VM's state accesses. Include
// all widths and both operand directions; preserve numeric, non-relocating use.
const STATE_ACCESSES: &str = " move.b state.Start,d2\n move.w state.Start,d0\n move.l state.Pointer,d1\n cmp.l state.Count,d0\n clr.w state.Start\n move.w d0,state.Start\n move.w #1,state.Start\n movea.l state.Pointer,a1\n move.w 8,d0\n movea.l 8,a1\n move.w 3+5,d0\n move.w Limit,d3\n move.w Limit+1,d3\n";

#[test]
fn compact_absolute_state_matrix_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&source(STATE_ACCESSES));
    assert!(oracle.windows(2).any(|bytes| bytes == [0x30, 0x39]));
    assert!(oracle.windows(2).any(|bytes| bytes == [0xb0, 0xb9]));
    assert!(oracle.windows(2).any(|bytes| bytes == [0x30, 0x38]));
}

#[test]
#[ignore = "requires configured FS-UAE; runtime state load/compare/store forms"]
fn compact_absolute_state_matrix_fs_uae() {
    hunk_sections::native_hunk_source(&source(STATE_ACCESSES));
}

fn bss_to_struct_source(body: &str) -> String {
    format!(
        ".module probe\n.cpu m68020\n.use app\n.section code,kind=code\nentry:\n{body} rts\n.align 4\n.endsection\n.section data,kind=data\npayload: .byte $aa,$bb,$cc\n.endsection\n.section bss,kind=bss\nmoduleCount: .res long,1\nincludeCount: .res long,1\nreserved: .res long,1\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule\n.module app\n.cpu m68020\n.pub\nFrame .struct\nPad .long ?\nModuleCount .long ?\nIncludeCount .long ?\n.endstruct\n.endmodule\n"
    )
}

const BSS_TO_STRUCT: &str =
    " move.l moduleCount,app.Frame.ModuleCount(a0)\n move.l includeCount,app.Frame.IncludeCount(a0)\n";
const BSS_TO_LITERAL_OFFSET: &str = " move.l moduleCount,4(a0)\n";
const IMMEDIATE_BSS_TO_STRUCT: &str = " move.l #moduleCount,app.Frame.ModuleCount(a0)\n";

#[test]
fn compact_bss_to_struct_rust_hunk_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&bss_to_struct_source(BSS_TO_STRUCT));
    assert_eq!(hunk::allocation(&oracle).unwrap().bss, 12);
    assert!(
        oracle
            .windows(2)
            .filter(|bytes| *bytes == [0x21, 0x79])
            .count()
            >= 2
    );
}

#[test]
fn compact_bss_to_struct_controls_rust_hunk_oracle() {
    for (body, opcode) in [
        (BSS_TO_LITERAL_OFFSET, [0x21, 0x79]),
        (IMMEDIATE_BSS_TO_STRUCT, [0x21, 0x7c]),
    ] {
        let oracle = hunk_sections::rust_hunk_source(&bss_to_struct_source(body));
        assert!(oracle.windows(2).any(|bytes| bytes == opcode));
    }
}

#[test]
fn compact_bss_to_struct_has_executable_package_sequence() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric =
        vm::binary_source_package::BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let move_id = numeric
        .names
        .iter()
        .position(|name| name == "move")
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
    let member_row = (0..count)
        .map(|index| offset + index * crate::binary_source_experiment::ROW)
        .find(|&row| {
            u16::from_be_bytes(wire[row..row + 2].try_into().unwrap()) == move_id
                && wire[row + 2] == qualifier
                && wire[row + 3] == 10
                && u16::from_be_bytes(wire[row + 6..row + 8].try_into().unwrap()) == 75
        })
        .unwrap();
    assert_eq!(wire[member_row + 5], 6); // TargetMember match remains unsupported in the native package.
    assert_eq!(wire[member_row + 19] & 0x0f, 10); // source needs exactly two tuple items
    assert_eq!(
        u16::from_be_bytes(wire[member_row + 22..member_row + 24].try_into().unwrap()),
        0x0900
    ); // PC base register class + 1
    assert!((0..count).any(|index| {
        let row = offset + index * crate::binary_source_experiment::ROW;
        u16::from_be_bytes(wire[row..row + 2].try_into().unwrap()) == move_id
            && wire[row + 2] == qualifier
            && wire[row + 3] == 10 // direct_direct
            && u16::from_be_bytes(wire[row + 6..row + 8].try_into().unwrap()) == 160
            && wire[row + 5] == 9 // semantic sequence
    }));
}

#[test]
#[ignore = "requires configured FS-UAE; absolute BSS source to imported struct displacement"]
fn compact_bss_to_struct_fs_uae() {
    hunk_sections::native_hunk_source(&bss_to_struct_source(BSS_TO_STRUCT));
}

#[test]
#[ignore = "requires configured FS-UAE; absolute BSS source to literal offset"]
fn compact_bss_to_struct_controls_fs_uae() {
    hunk_sections::native_hunk_source(&bss_to_struct_source(BSS_TO_LITERAL_OFFSET));
}

#[test]
#[ignore = "requires configured FS-UAE; immediate BSS address to imported struct offset"]
fn compact_immediate_bss_to_struct_fs_uae() {
    hunk_sections::native_hunk_source(&bss_to_struct_source(IMMEDIATE_BSS_TO_STRUCT));
}

// Rust projects the package-declared member target. Native retains its explicit
// unsupported-row barrier until that projection is implemented there.
const PC_TO_ABSOLUTE_MEMBER: &str =
    ".cpu m68020\n.org $1000\nentry:\n move.l 6(pc),(reserved).l\nreserved: .long 0\n.end\n";

#[test]
fn compact_pc_to_absolute_member_rust_oracle() {
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(
        &PC_TO_ABSOLUTE_MEMBER.lines().collect::<Vec<_>>(),
        true,
    )
    .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let bytes = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    assert_eq!(bytes, [0x23, 0xfa, 0, 6, 0, 0, 0x10, 8, 0, 0, 0, 0]);
}

#[test]
#[ignore = "requires configured FS-UAE; matching PC-tuple unsupported barrier"]
fn compact_pc_to_absolute_member_barrier_fs_uae() {
    assert_native_files_rejection(
        &[("input.asm", PC_TO_ABSOLUTE_MEMBER)],
        "m68020",
        Some("[file 00000001, line 00000004]"),
    );
}

// A real nested indirect root matches the unsupported package path. The
// scalar proof must not turn it into a later absolute-address candidate.
const NESTED_INDIRECT: &str = ".cpu m68020\n move.w ([a0,d1.l*4],8.w),d2\n.end\n";

#[test]
fn compact_absolute_nested_barrier_rust_oracle() {
    let (_, diagnostics) = assemble_source_entries_with_runtime_mode(
        &NESTED_INDIRECT.lines().collect::<Vec<_>>(),
        true,
    )
    .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
}

#[test]
#[ignore = "requires configured FS-UAE; matching nested indirect barrier"]
fn compact_absolute_nested_barrier_fs_uae() {
    assert_native_files_rejection(
        &[("input.asm", NESTED_INDIRECT)],
        "m68020",
        Some("[file 00000001, line 00000002]"),
    );
}

// Rust has no unqualified numeric CLR fallback: a literal must not match
// the symbol-bearing absolute-long target recipe.
const NUMERIC_CLEAR: &str = ".cpu m68020\n clr.w 8\n.end\n";

#[test]
fn compact_absolute_numeric_clear_rust_rejection() {
    let (_, diagnostics) =
        assemble_source_entries_with_runtime_mode(&NUMERIC_CLEAR.lines().collect::<Vec<_>>(), true)
            .unwrap();
    assert!(!diagnostics.is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; literal cannot satisfy target predicate"]
fn compact_absolute_numeric_clear_fs_uae() {
    assert_native_files_rejection(
        &[("input.asm", NUMERIC_CLEAR)],
        "m68020",
        Some("[file 00000001, line 00000002]"),
    );
}
