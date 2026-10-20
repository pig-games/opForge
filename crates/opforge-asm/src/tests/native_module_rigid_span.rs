//! Automatic branch geometry across proved, retained fragment spans.
use super::{private_oracle, private_pipeline_native_cases, PrivateCase, PRIVATE_OUTPUTS};

fn case(name: &'static str, body: &str, accepted_modules: u32) -> PrivateCase {
    PrivateCase {
        name,
        accepted_modules,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use worker\n.section code,kind=code\n.long worker.entry\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            // Mandatory preceding content moves both worker anchors from their
            // private origin. It also changes alignment padding in the controls.
            ("library/base.asm", ".module base\n.cpu 68020\n.pub\n.section code,kind=code\nanchor .word $1357\n.endsection\n.endmodule\n".into()),
            ("library/worker.asm", format!(".module worker\n.cpu 68020\n.use base\n.pub\n.section code,kind=code\n{body}.endsection\n.endmodule\n")),
        ],
    }
}

pub(super) fn cases() -> Vec<PrivateCase> {
    let prunable = format!(
        "entry .block\n beq helper\n rts\n.bend\n.priv\ndead .block\n{}.bend\nhelper\n.word $2468\n rts\n",
        ".word $dead\n".repeat(80)
    );
    let backward_prunable = format!(
        ".priv\nhelper\n.word $2468\n rts\ndead .block\n{}.bend\n.pub\nentry .block\n bne helper\n rts\n.bend\n",
        ".word $dead\n".repeat(80)
    );
    // A real byte payload preserves the 33,000-byte span without relying on
    // the native large-count .fill path (an independent reference-path gap).
    let mut large_prunable = case(
        "private-rigid-span-large-prunable-gap",
        "entry .block\n beq helper\n rts\n.bend\n.priv\ndead .block\n.incbin \"gap.bin\"\n.bend\nhelper\n.word $2468\n rts\n",
        1,
    );
    large_prunable
        .files
        .push(("library/gap.bin", "A".repeat(33000)));
    vec![
        case(
            "private-rigid-span-forward",
            "entry .block\n beq helper\n rts\n.bend\n.priv\nhelper\n.word $2468\n rts\n",
            2,
        ),
        case(
            "private-rigid-span-backward",
            ".priv\nhelper\n.word $2468\n rts\n.pub\nentry .block\n bne helper\n rts\n.bend\n",
            2,
        ),
        // Root references only entry. Retaining the branch recipe must keep
        // the independently owned helper block live through its dependency.
        case(
            "private-rigid-span-two-owners",
            "entry .block\n beq helper\n rts\n.bend\n.priv\nhelper .block\n.word $2468\n rts\n.bend\n",
            2,
        ),
        // Package AUTO starts with its word candidate. Every possible pruning
        // subset stays inside that candidate's range, despite a changing delta.
        case("private-rigid-span-prunable-gap", &prunable, 2),
        case("private-rigid-span-backward-prunable-gap", &backward_prunable, 2),
        case(
            "private-rigid-span-addend-prunable-gap",
            &prunable.replace("beq helper", "beq helper+2"),
            2,
        ),
        case(
            "private-rigid-span-forward-negative-addend",
            &prunable
                .replace("beq helper", "beq helper-2")
                .replace("helper\n.word $2468\n rts", "targetBase\n.word $2468\nhelper\n rts"),
            2,
        ),
        case(
            "private-rigid-span-backward-negative-addend",
            &backward_prunable
                .replace("bne helper", "bne helper-2")
                .replace("helper\n.word $2468\n rts", "targetBase\n.word $2468\nhelper\n rts"),
            2,
        ),
        // An explicit request keeps its selected fixed width after pruning.
        case(
            "private-rigid-span-explicit-prunable-gap",
            &prunable.replace("beq helper", "beq.l helper"),
            2,
        ),
        // This original AUTO long candidate can become word-sized when the
        // gap is omitted. Fixed-span replay must not silently retain that width.
        large_prunable,
        // The first alignment has no padding at the private origin; preceding
        // content changes it nonetheless. Both alignment controls must reject.
        case(
            "private-rigid-span-zero-align",
            "entry .block\n beq helper\n rts\n.word $cafe,$babe\n.bend\n.priv\n.align 8\nhelper\n.word $2468\n rts\n",
            1,
        ),
        case(
            "private-rigid-span-padded-align",
            "entry .block\n beq helper\n rts\n.bend\n.priv\n.align 8\nhelper\n.word $2468\n rts\n",
            1,
        ),
        case(
            "private-rigid-span-section-excursion",
            "entry .block\n beq helper\n rts\n.bend\n.endsection\n.section data,kind=data\n.word $beef\n.endsection\n.section code,kind=code\n.priv\nhelper\n.word $2468\n rts\n",
            1,
        ),
        case(
            "private-rigid-span-external",
            "entry .block\n beq base.anchor\n rts\n.bend\n",
            1,
        ),
    ]
}

pub(super) fn check(name: &str, binary: &[u8]) {
    assert!(
        binary.starts_with(&[0x13, 0x57]),
        "base precedes the moved worker"
    );
    assert!(binary.ends_with(&[0x11, 0x22]));
    if name != "private-rigid-span-external" {
        assert!(
            binary.windows(2).any(|bytes| bytes == [0x24, 0x68]),
            "helper remains live"
        );
    }
    if name.ends_with("prunable-gap") {
        assert!(
            !binary.windows(2).any(|bytes| bytes == [0xde, 0xad]),
            "unreferenced gap is omitted"
        );
        assert!(!binary.windows(2).any(|bytes| bytes == [0xde, 0xde]));
    }
    if name == "private-rigid-span-large-prunable-gap" {
        assert!(
            !binary.windows(2).any(|bytes| bytes == [b'A', b'A']),
            "the 33,000-byte binary gap is omitted"
        );
    }
}

pub(super) fn object_counts(name: &str) -> Option<(usize, usize)> {
    name.starts_with("private-rigid-span-").then_some((
        0,
        match name {
            "private-rigid-span-forward"
            | "private-rigid-span-backward"
            | "private-rigid-span-two-owners"
            | "private-rigid-span-prunable-gap"
            | "private-rigid-span-backward-prunable-gap"
            | "private-rigid-span-addend-prunable-gap"
            | "private-rigid-span-forward-negative-addend"
            | "private-rigid-span-backward-negative-addend"
            | "private-rigid-span-explicit-prunable-gap" => 2,
            _ => 1,
        },
    ))
}

#[test]
fn module_rigid_span_live_rust_oracles() {
    for case in cases() {
        private_oracle(&case);
    }
}

#[test]
#[ignore = "requires instrumented compact CLI; automatic candidates over retained rigid spans"]
fn native_module_rigid_span_disk_progress() {
    private_pipeline_native_cases(true, true, cases());
}
