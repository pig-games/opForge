//! F3: forward absolute constants, source-order layout values and bit operations.
use super::*;

const PIXELS: &str = include_str!("../../fixtures/binary-source/pixel-mask.asm");
const CONTROL: &str = include_str!("../../fixtures/binary-source/control-word.asm");

fn bytes(source: &str) -> Vec<u8> {
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live Rust oracle");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

fn operators(cpu: &str) -> String {
    format!(".cpu {cpu}\nearly = later+1\nlater = 4\nshared = early ^ later\nprecedence = 1 | 2 ^ 3 & 6\nshifted = 1 << 2+1\nchained = 32 >> 1 >> 2\nmask = (~later) & 255\noriginValue = 4096\n.org originValue\nstart:\nposition = $\nrelative = position-start\n.byte early,shared,precedence,shifted,chained,mask,relative\n.word position\n.end\n")
}

#[test]
fn binary_dependencies_pixels_oracle() {
    assert_eq!(
        bytes(PIXELS),
        [0xa5, 0x24, 0x29, 0xcf, 0x09, 0x30, 0x85, 0x24, 0x60, 9, 0, 0x30, 0xcf, 0x30]
    );
}

#[test]
fn binary_dependencies_control_oracle() {
    assert_eq!(
        bytes(CONTROL),
        [
            0x02, 0x80, 0, 0, 0, 0xff, 0x00, 0x80, 0, 0, 0x3a, 0, 0x4e, 0x75, 0x3a, 0, 0, 0xff, 0,
            14
        ]
    );
}

#[test]
fn binary_dependencies_operators_oracle() {
    assert_eq!(bytes(&operators("m6502")), [5, 1, 1, 8, 4, 251, 0, 0, 16]);
    assert_eq!(bytes(&operators("m68000")), [5, 1, 1, 8, 4, 251, 0, 16, 0]);
}

#[test]
#[ignore = "requires configured FS-UAE; symbolic packed-pixel routine"]
fn binary_dependencies_pixels_fs_uae() {
    assert_binary_source(PIXELS.into(), "m6502".into());
}
#[test]
#[ignore = "requires configured FS-UAE; symbolic control-word routine"]
fn binary_dependencies_control_fs_uae() {
    assert_binary_source(CONTROL.into(), "m68000".into());
}
#[test]
#[ignore = "requires configured FS-UAE; precedence and definition-site PC"]
fn binary_dependencies_operators_fs_uae() {
    let cpu = std::env::var("OPFORGE_COMPARE_CPU").unwrap_or_else(|_| "m6502".into());
    assert_binary_source(operators(&cpu), cpu);
}

fn chain() -> String {
    let mut source = String::from(".cpu m6502\n.org $1000\n");
    for n in 0..127 {
        source.push_str(&format!("value{n} = value{}+1\n", n + 1));
    }
    source.push_str("value127 = 1\n.word value0,value63,value127\n.end\n");
    source
}
#[test]
fn binary_dependencies_chain_oracle() {
    assert_eq!(bytes(&chain()), [128, 0, 65, 0, 1, 0]);
}
#[test]
#[ignore = "requires configured FS-UAE; deep chain uses bounded explicit work stack"]
fn binary_dependencies_chain_fs_uae() {
    assert_binary_source(chain(), "m6502".into());
}

// Accepted by Rust where marked; native intentionally rejects unsupported
// layout dependencies and cycles instead of treating provisional zeros as proof.
const REJECTIONS: &[(&str, &str, bool)] = &[
    ("cycle", "left = right+1\nright = left+1\n.byte left", true),
    (
        "tainted_cycle",
        "left = $+right\nright = left\n.byte 0",
        true,
    ),
    ("missing", "left = right+1\n.byte 0", false),
    (
        "forward_label",
        "length = finish-start\nstart:\n.byte 0\nfinish:\n.word length",
        true,
    ),
    ("duplicate", "value = 1\nvalue = 1\n.byte value", false),
    ("collision", "value = 1\nvalue:\n.byte 0", false),
    ("overflow", "value = 1 << 31\n.long value", true),
];
fn rejection(body: &str) -> String {
    format!(".cpu m6502\n.org $1000\n{body}\n.end\n")
}
#[test]
fn binary_dependencies_rejection_oracles() {
    for (name, body, accepted) in REJECTIONS {
        let source = rejection(body);
        let result =
            assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true);
        assert_eq!(
            matches!(result,Ok((_,ref diagnostics)) if diagnostics.is_empty()),
            *accepted,
            "{name}"
        );
    }
}
#[test]
#[ignore = "requires OPFORGE_DEPENDENCY_REJECTION and configured FS-UAE"]
fn binary_dependencies_rejection_fs_uae() {
    let key = std::env::var("OPFORGE_DEPENDENCY_REJECTION").unwrap();
    let (_, body, _) = REJECTIONS.iter().find(|(name, _, _)| *name == key).unwrap();
    assert_native_rejection(&rejection(body), "m6502");
}
