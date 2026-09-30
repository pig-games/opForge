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
