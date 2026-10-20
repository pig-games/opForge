//! Selected relative fixups freeze only when exact endpoints share a fragment.
use super::{private_oracle, private_pipeline_native_cases, PrivateCase, PRIVATE_OUTPUTS};

const SAME_FRAGMENT: &str = r#"entry .block
backward
 .word $2468
 dbra d0,forward
 dbf d7,backward
 dbra d3,backward+2
forward
 dbf d5,forward
 rts
.bend
"#;

fn case(name: &'static str, body: &str, accepted_modules: u32) -> PrivateCase {
    PrivateCase {
        name,
        accepted_modules,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use worker\n.section code,kind=code\n.long worker.entry\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            // This mandatory dependency moves the worker away from its private
            // origin. Its raw bytes and the root pointer are checked by Rust.
            ("library/base.asm", ".module base\n.cpu 68020\n.section code,kind=code\n.word $1357\n.endsection\n.endmodule\n".into()),
            ("library/worker.asm", format!(".module worker\n.cpu 68020\n.use base\n.pub\n.section code,kind=code\n{body}.endsection\n.endmodule\n")),
        ],
    }
}

pub(super) fn cases() -> Vec<PrivateCase> {
    vec![
        case("private-pc-cancel-same-fragment", SAME_FRAGMENT, 2),
        // Same whole block is insufficient: moving alignment changes the span.
        case(
            "private-pc-cancel-cross-align",
            "entry .block\n dbra d2,target\n .align 8\ntarget\n rts\n.bend\n",
            1,
        ),
        // A separately movable target must retain reference processing.
        case(
            "private-pc-cancel-cross-block",
            "entry .block\n dbf d6,target\n rts\n.bend\ntarget .block\n rts\n.bend\n",
            1,
        ),
        // A positional expression needs an exact anchor, even when its value
        // currently coincides with the instruction's address.
        case(
            "private-pc-cancel-unanchored-pc",
            "entry .block\n dbra d1,$\n rts\n.bend\n",
            1,
        ),
    ]
}

pub(super) fn check(name: &str, binary: &[u8]) {
    assert!(
        binary.starts_with(&[0x13, 0x57]),
        "dependency must precede the moved worker"
    );
    assert!(binary.ends_with(&[0x11, 0x22]));
    if name == "private-pc-cancel-same-fragment" {
        assert_eq!(&binary[2..4], [0x24, 0x68]);
        for register_bits in [0xc8, 0xcf, 0xcb, 0xcd] {
            assert!(
                binary
                    .windows(2)
                    .any(|bytes| bytes == [0x51, register_bits]),
                "counter register encoding must survive freezing"
            );
        }
    }
}

pub(super) fn object_counts(name: &str) -> Option<(usize, usize)> {
    name.starts_with("private-pc-cancel-").then_some((
        0,
        if name.ends_with("same-fragment") {
            2
        } else {
            1
        },
    ))
}

#[test]
fn module_pc_cancellation_live_rust_oracles() {
    for case in cases() {
        private_oracle(&case);
    }
}

#[test]
#[ignore = "requires instrumented compact CLI; exact target cancellation and conservative fallback"]
fn native_module_pc_cancellation_disk_progress() {
    private_pipeline_native_cases(true, true, cases());
}
