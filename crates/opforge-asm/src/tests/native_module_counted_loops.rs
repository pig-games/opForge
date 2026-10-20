//! Fixed module images retain only reached, placement-independent counted loops.
use super::{private_oracle, private_pipeline_native_cases, PrivateCase, PRIVATE_OUTPUTS};

struct LoopCase {
    name: &'static str,
    prefix: &'static str,
    body: &'static str,
    accepted: u32,
    bytes: &'static [u8],
    entry: u32,
}

const CASES: &[LoopCase] = &[
    LoopCase {
        name: "private-counted-loop-zero",
        prefix: "",
        body: ".for 0\n.for $\n.byte $ee\n.endfor\n.endfor\n rts\n",
        accepted: 1,
        bytes: &[0x4e, 0x75],
        entry: 0,
    },
    LoopCase {
        name: "private-counted-loop-nested",
        prefix: "COUNT .const 2\n",
        body: ".for COUNT\n.for 3\n.byte $45\n.endfor\n.endfor\n.word $1234\n rts\n",
        accepted: 1,
        bytes: &[0x45, 0x45, 0x45, 0x45, 0x45, 0x45, 0x12, 0x34, 0x4e, 0x75],
        entry: 0,
    },
    LoopCase {
        name: "private-counted-loop-assignment-arithmetic",
        prefix: "START = 1\nSTOP = 4\n",
        body: ".for STOP-START-1\n.byte $45\n.endfor\n rts\n",
        accepted: 1,
        bytes: &[0x45, 0x45, 0x4e, 0x75],
        entry: 0,
    },
    LoopCase {
        name: "private-counted-loop-relative-table",
        prefix: "",
        body: "table\n.for 2\n.word helper-table\n.endfor\nhelper\n rts\n",
        accepted: 1,
        bytes: &[0, 4, 0, 4, 0x4e, 0x75],
        entry: 0,
    },
    LoopCase {
        name: "private-counted-loop-current-fallback",
        prefix: ".byte $aa\n",
        body: ".for $\n.byte $44\n.endfor\n rts\n",
        accepted: 0,
        bytes: &[0xaa, 0x44, 0x4e, 0x75],
        entry: 1,
    },
    // Use a stable zero delta: nonzero forward loop-size replay has a
    // separate Rust pass-count gap. Strict proof must reject even this delta.
    LoopCase {
        name: "private-counted-loop-label-difference-fallback",
        prefix: "before\n.byte $aa\n",
        body: ".for before-before\n.byte $44\n.endfor\n rts\n",
        accepted: 0,
        bytes: &[0xaa, 0x4e, 0x75],
        entry: 1,
    },
    LoopCase {
        // Static collection expansion already finishes in the shared frontend;
        // no runtime iterable control reaches the fixed module image.
        name: "private-counted-loop-static-iterable",
        prefix: "",
        body: ".for item in {1,2}\n.byte item\n.endfor\n rts\n",
        accepted: 1,
        bytes: &[1, 2, 0x4e, 0x75],
        entry: 0,
    },
    LoopCase {
        name: "private-counted-loop-scoped-fallback",
        prefix: "",
        body: ".bfor 2\n.byte $44\n.endfor\n rts\n",
        accepted: 0,
        bytes: &[0x44, 0x44, 0x4e, 0x75],
        entry: 0,
    },
    LoopCase {
        name: "private-counted-loop-while-fallback",
        prefix: "",
        body: ".while 0\n.byte missing\n.endwhile\n rts\n",
        accepted: 0,
        bytes: &[0x4e, 0x75],
        entry: 0,
    },
];

pub(super) fn cases() -> Vec<PrivateCase> {
    CASES.iter().map(|case| PrivateCase {
        name: case.name,
        accepted_modules: case.accepted,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use provider\n.section code,kind=code\n.long provider.entry\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/provider.asm", format!(".module provider\n.cpu 68020\n.pub\n.section code,kind=code\n{}entry .block\n{}.bend\n.endsection\n.endmodule\n", case.prefix, case.body)),
        ],
    }).collect()
}

pub(super) fn object_counts(name: &str) -> Option<(usize, usize)> {
    CASES
        .iter()
        .find(|case| case.name == name)
        .map(|case| (0, case.accepted as usize))
}

pub(super) fn check(name: &str, binary: &[u8]) {
    let case = CASES.iter().find(|case| case.name == name).unwrap();
    let mut expected = case.bytes.to_vec();
    expected.extend_from_slice(&case.entry.to_be_bytes());
    expected.extend_from_slice(&[0x11, 0x22]);
    assert_eq!(binary, expected, "{name}");
}

#[test]
fn module_counted_loops_live_rust_oracles() {
    for case in cases() {
        private_oracle(&case);
    }
}

#[test]
#[ignore = "requires instrumented compact CLI; strict counted-loop image geometry"]
fn native_module_counted_loops_disk_progress() {
    private_pipeline_native_cases(true, true, cases());
}
