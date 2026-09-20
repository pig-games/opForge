//! Production reference regressions for immutable absolute dependency resolution.
use super::*;

fn bytes(source: &str) -> Vec<u8> {
    let assembler = run_passes(&source.lines().collect::<Vec<_>>());
    assembler
        .image()
        .entries()
        .unwrap()
        .into_iter()
        .map(|(_, byte)| byte)
        .collect()
}

#[test]
fn constant_dependencies_mixed_definition_forms() {
    assert_eq!(
        bytes("first = second+1\nsecond .const third+1\nthird = 7\n.byte first,second,third"),
        [9, 8, 7]
    );
}

#[test]
fn constant_dependencies_refresh_instruction_width_and_labels() {
    assert_eq!(bytes(".cpu m6502\n.org $1000\naddress = base+offset\nbase = page<<8\npage = 2\noffset = 4\nstart:\n lda address\nfinish:\n.word finish,start,finish-start"),
        [0xad,4,2,3,0x10,0,0x10,3,0]);
}

#[test]
fn constant_dependencies_keep_block_bindings_separate() {
    assert_eq!(bytes("left .block\nfirst = later+1\nlater = seed+1\nseed = 2\n.byte first\n.endblock\nright .block\nfirst .const later+2\nlater = seed+1\nseed = 6\n.byte first\n.endblock"), [4,9]);
}

#[test]
fn constant_dependencies_resolve_import_alias_in_definition_scope() {
    assert_eq!(bytes(".module library\n.pub\nvalue = seed+1\nseed = root+1\nroot = 5\n.endmodule\n.module consumer\n.use library as lib\nresult = lib.value+1\n.byte result\n.endmodule"), [8]);
}

#[test]
fn constant_dependencies_ignore_inactive_definitions() {
    assert_eq!(
        bytes(".if 0\nfirst = absent+1\n.endif\nfirst = second+1\nsecond = 4\n.byte first"),
        [5]
    );
}

#[test]
fn constant_dependencies_preserve_mutable_snapshots_and_layout_values() {
    assert_eq!(bytes(".org $1000\ntrigger = next+1\nnext = last+1\nlast = 1\nvariable := 3\nsaved = variable+1\nvariable := 8\n.byte saved,variable\nposition = $\n.word position\nvals .const {10,20,30}\n.byte vals[2]"), [4,8,2,0x10,30]);
}

#[test]
fn constant_dependencies_reject_cycles() {
    for body in [
        "left = right+1\nright = left+1",
        "left = $+right\nright = left",
        "self = self+1",
    ] {
        let assembler = run_pass1(&body.lines().collect::<Vec<_>>());
        assert!(
            assembler
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.severity == Severity::Error),
            "cycle accepted: {body}"
        );
    }
}
