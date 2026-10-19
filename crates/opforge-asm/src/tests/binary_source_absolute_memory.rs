//! Qualified absolute-memory operands from the tokenizer runtime.
use super::*;

#[test]
fn compact_absolute_memory_to_memory_defined_labels_rust_oracle() {
    for (width, opcode) in [
        ("b", [0x13, 0xf9]),
        ("w", [0x33, 0xf9]),
        ("l", [0x23, 0xf9]),
    ] {
        let input = format!(".cpu m68020\n.org $1000\n move.{width} source+2,destination+4\nsource: .long 0\ndestination: .long 0\n.end\n");
        let (entries, diagnostics) =
            assemble_source_entries_with_runtime_mode(&input.lines().collect::<Vec<_>>(), true)
                .unwrap();
        assert!(diagnostics.is_empty(), "MOVE.{width}: {diagnostics:?}");
        let bytes = entries
            .into_iter()
            .map(|(_, byte)| byte)
            .collect::<Vec<_>>();
        assert_eq!(
            &bytes[..10],
            &[opcode[0], opcode[1], 0, 0, 0x10, 12, 0, 0, 0x10, 18],
            "MOVE.{width}"
        );
    }
}

#[test]
fn compact_absolute_memory_to_memory_relocations_rust_hunk_oracle() {
    for (width, opcode) in [
        ("b", [0x13, 0xf9]),
        ("w", [0x33, 0xf9]),
        ("l", [0x23, 0xf9]),
    ] {
        for (source, destination, source_target, destination_target) in [
            ("payload", "payload", 2, 2),
            ("moduleCount", "includeCount", 1, 1),
            ("payload", "moduleCount", 2, 1),
            ("moduleCount", "payload", 1, 2),
        ] {
            let body = format!(" move.{width} {source}+2,{destination}+4\n");
            let oracle = hunk_sections::rust_hunk_source(&bss_to_struct_source(&body));
            let segments = hunk::segments(&oracle).unwrap();
            let destination_addend = if destination == "includeCount" { 8 } else { 4 };
            assert_eq!(
                &segments[0].payload[..12],
                &[
                    opcode[0],
                    opcode[1],
                    0,
                    0,
                    0,
                    2,
                    0,
                    0,
                    0,
                    destination_addend,
                    0x4e,
                    0x75
                ],
                "{body}"
            );
            let mut relocations = segments[0].relocations.clone();
            relocations.sort_unstable();
            assert_eq!(
                relocations,
                [(2, source_target), (6, destination_target)],
                "{body}"
            );
            assert!(segments[1].payload.is_empty());
            assert!(segments[1].relocations.is_empty());
            assert_eq!(segments[2].payload, [0xaa, 0xbb, 0xcc, 0]);
            assert!(segments[2].relocations.is_empty());
        }
    }
}

#[test]
fn compact_absolute_memory_to_memory_invalid_destinations_rust_rejection() {
    for width in ["b", "w", "l"] {
        for destination in ["#4", "4(pc)"] {
            let input =
                format!(".cpu m68020\n move.{width} source,{destination}\nsource: .long 0\n.end\n");
            let (_, diagnostics) =
                assemble_source_entries_with_runtime_mode(&input.lines().collect::<Vec<_>>(), true)
                    .unwrap();
            assert!(!diagnostics.is_empty(), "MOVE.{width} source,{destination}");
        }
    }
}

#[test]
fn compact_absolute_memory_to_numeric_destination_rust_oracle() {
    let input = ".cpu m68020\n.org $1000\n move.l source+2,$123456\nsource: .long 0\n.end\n";
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&input.lines().collect::<Vec<_>>(), true)
            .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let bytes = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    assert_eq!(
        &bytes[..10],
        &[0x23, 0xf9, 0, 0, 0x10, 12, 0, 0x12, 0x34, 0x56]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; absolute pairs, scalar destination and retained wrappers"]
fn compact_absolute_memory_to_memory_native_matrix_fs_uae() {
    hunk_sections::native_hunk_source(&bss_to_struct_source(concat!(
        " move.b payload+2,moduleCount+4\n",
        " move.w moduleCount+2,payload+4\n",
        " move.l moduleCount+2,includeCount+4\n",
        " move.l moduleCount+2,$123456\n",
        " move.l (a0),(a1)\n",
        " move.l 4(a0),d0\n",
    )));
}

const MOS_REGISTER_SPELLING_SCALAR: &str =
    ".cpu m6502\n.org $1000\nx=$1234\nptr=$1234\n jmp (ptr)\n .word x\n lda ($20,x)\n lda ($20),y\n.end\n";

#[test]
fn compact_indirect_register_spelling_scalar_rust_oracle() {
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(
        &MOS_REGISTER_SPELLING_SCALAR.lines().collect::<Vec<_>>(),
        true,
    )
    .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let bytes = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    // The existing Rust selectors do not support JMP (x). Exercise scalar
    // parentheses with an ordinary symbol, then the register spelling in
    // scalar data and index positions without extending that instruction form.
    assert_eq!(
        bytes,
        [0x6c, 0x34, 0x12, 0x34, 0x12, 0xa1, 0x20, 0xb1, 0x20]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; scalar parentheses, x value and index register x"]
fn compact_indirect_register_spelling_scalar_fs_uae() {
    assert_binary_source(MOS_REGISTER_SPELLING_SCALAR.into(), "m6502".into());
}

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

#[test]
fn compact_immediate_bss_addend_to_memory_rust_hunk_oracle() {
    for (body, expected) in [
        (
            " move.l #moduleCount+4,app.Frame.ModuleCount(a0)\n",
            vec![0x21, 0x7c, 0, 0, 0, 4, 0, 4, 0x4e, 0x75],
        ),
        (
            " move.l #(moduleCount+4),app.Frame.ModuleCount(a0)\n",
            vec![0x21, 0x7c, 0, 0, 0, 4, 0, 4, 0x4e, 0x75],
        ),
        (
            " move.l #moduleCount+4,4(a0)\n",
            vec![0x21, 0x7c, 0, 0, 0, 4, 0, 4, 0x4e, 0x75],
        ),
        (
            " move.l #moduleCount+4,(a0)\n",
            vec![0x20, 0xbc, 0, 0, 0, 4, 0x4e, 0x75],
        ),
        (
            " move.l #moduleCount+4,(a0)+\n",
            vec![0x20, 0xfc, 0, 0, 0, 4, 0x4e, 0x75],
        ),
        (
            " move.l #moduleCount+4,-(a0)\n",
            vec![0x21, 0x3c, 0, 0, 0, 4, 0x4e, 0x75],
        ),
    ] {
        let oracle = hunk_sections::rust_hunk_source(&bss_to_struct_source(body));
        let segments = hunk::segments(&oracle).unwrap();
        assert_eq!(segments.len(), 3, "{body}");
        // The helper places code, BSS, data in that order. Compare the actual
        // code from its beginning, excluding only final alignment padding.
        assert_eq!(&segments[0].payload[..expected.len()], expected, "{body}");
        assert_eq!(
            segments[0].payload.len(),
            expected.len().next_multiple_of(4),
            "{body}"
        );
        assert_eq!(segments[0].relocations, [(2, 1)], "{body}");
        assert!(segments[1].payload.is_empty(), "{body}");
        assert!(segments[1].relocations.is_empty(), "{body}");
        assert_eq!(segments[2].payload, [0xaa, 0xbb, 0xcc, 0], "{body}");
        assert!(segments[2].relocations.is_empty(), "{body}");
    }
}
