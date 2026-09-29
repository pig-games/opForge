//! Package-owned automatic requests preserve canonical branch sizing policy.
use super::*;

fn far_source(explicit: bool) -> String {
    let suffix = if explicit { ".w" } else { "" };
    let mut source = format!(".cpu m68020\n.org 0\n beq{suffix} target\n bne{suffix} target\n bra{suffix} target\n bhs{suffix} target\n");
    source.push_str(&".byte 0\n".repeat(160));
    source.push_str("target\n rts\n.end\n");
    source
}

const NEAR: &str = ".cpu m68020\n.org 0\n beq target\n nop\ntarget\n rts\n.end\n";
const EXPLICIT_SHORT: &str = ".cpu m68020\n.org 0\n beq.s target\n nop\ntarget\n rts\n.end\n";

fn oracle(source: &str) -> Vec<u8> {
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live Rust branch oracle");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn compact_branch_width_rust_oracles() {
    let far = oracle(&far_source(false));
    assert_eq!(far, oracle(&far_source(true)));
    assert_eq!(
        &far[..16],
        &[0x67, 0, 0, 174, 0x66, 0, 0, 170, 0x60, 0, 0, 166, 0x64, 0, 0, 162]
    );
    assert_eq!(oracle(NEAR), [0x67, 0, 0, 4, 0x4e, 0x71, 0x4e, 0x75]);
    assert_eq!(oracle(EXPLICIT_SHORT), [0x67, 2, 0x4e, 0x71, 0x4e, 0x75]);
}

#[test]
#[ignore = "requires configured FS-UAE; package automatic branches selecting word width"]
fn compact_branch_auto_far_fs_uae() {
    assert_binary_source(far_source(false), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; explicit short branch control"]
fn compact_branch_explicit_short_fs_uae() {
    assert_binary_source(EXPLICIT_SHORT.into(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; automatic near target preserves package word policy"]
fn compact_branch_auto_near_fs_uae() {
    assert_binary_source(NEAR.into(), "m68020".into());
}
