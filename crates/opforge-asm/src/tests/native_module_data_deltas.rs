//! Exact symbolic data deltas retain their two anchors through module placement.
use super::{private_oracle, private_pipeline_native_cases, PrivateCase, PRIVATE_OUTPUTS};

struct DeltaCase {
    name: &'static str,
    prefix: &'static str,
    body: &'static str,
    accepted: u32,
    bytes: &'static [u8],
}

const CASES: &[DeltaCase] = &[
    DeltaCase {
        name: "private-data-delta-target-alignment",
        prefix: "",
        body: "base\n.word target-base\n.long target-base\n.align 8\ntarget\n rts\n",
        accepted: 1,
        bytes: &[0, 8, 0, 0, 0, 8, 0, 0, 0x4e, 0x75],
    },
    DeltaCase {
        name: "private-data-delta-negative",
        prefix: "",
        body: "target\n rts\n.align 8\nbase\n.word target-base\n.long target-base\n rts\n",
        accepted: 1,
        bytes: &[
            0x4e, 0x75, 0, 0, 0, 0, 0, 0, 0xff, 0xf8, 0xff, 0xff, 0xff, 0xf8, 0x4e, 0x75,
        ],
    },
    DeltaCase {
        name: "private-data-delta-base-alignment-fallback",
        prefix: "",
        body: "base\n.byte $aa\n.align 4\n.word target-base\ntarget\n rts\n",
        accepted: 0,
        bytes: &[0xaa, 0, 0, 0, 0, 6, 0x4e, 0x75],
    },
    DeltaCase {
        name: "private-data-delta-mixed-fallback",
        prefix: "",
        body: "base\n.word target-base+1\ntarget\n rts\n",
        accepted: 0,
        bytes: &[0, 3, 0x4e, 0x75],
    },
    DeltaCase {
        name: "private-data-delta-constants",
        prefix: "START = 1\nSTOP = 4\n",
        body: ".word STOP-START\n.long START-STOP\n rts\n",
        accepted: 1,
        bytes: &[0, 3, 0xff, 0xff, 0xff, 0xfd, 0x4e, 0x75],
    },
];

pub(super) fn cases() -> Vec<PrivateCase> {
    let mut cases: Vec<_> = CASES.iter().map(|case| PrivateCase {
        name: case.name,
        accepted_modules: case.accepted,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use provider\n.section code,kind=code\n.long provider.entry\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/provider.asm", format!(".module provider\n.cpu 68020\n.pub\n.section code,kind=code\n{}entry .block\n{}.bend\n.endsection\n.endmodule\n", case.prefix, case.body)),
        ],
    }).collect();
    // Moving the producer by two bytes removes its original alignment padding.
    // Both fields must change from 8 to 6 while keeping their widths.
    let mut moved = PrivateCase {
        name: "private-data-delta-moved-target-alignment",
        accepted_modules: 2,
        files: cases[0].files.clone(),
    };
    moved.files[1].1 = moved.files[1]
        .1
        .replace(".cpu 68020\n", ".cpu 68020\n.use prelude\n");
    moved.files.push(("library/prelude.asm", ".module prelude\n.cpu 68020\n.section code,kind=code\n.word $1357\n.endsection\n.endmodule\n".into()));
    cases.push(moved);
    cases
}

pub(super) fn object_counts(name: &str) -> Option<(usize, usize)> {
    if name == "private-data-delta-moved-target-alignment" {
        return Some((0, 2));
    }
    CASES
        .iter()
        .find(|case| case.name == name)
        .map(|case| (0, case.accepted as usize))
}

pub(super) fn check(name: &str, binary: &[u8]) {
    if name == "private-data-delta-moved-target-alignment" {
        assert_eq!(
            binary,
            [0x13, 0x57, 0, 6, 0, 0, 0, 6, 0x4e, 0x75, 0, 0, 0, 2, 0x11, 0x22]
        );
        return;
    }
    let case = CASES.iter().find(|case| case.name == name).unwrap();
    let mut expected = case.bytes.to_vec();
    expected.extend_from_slice(&0u32.to_be_bytes());
    expected.extend_from_slice(&[0x11, 0x22]);
    assert_eq!(binary, expected, "{name}");
}

#[test]
fn module_data_deltas_live_rust_oracles() {
    for case in cases() {
        private_oracle(&case);
    }
}

#[test]
#[ignore = "requires instrumented compact CLI; symbolic data-delta placement"]
fn native_module_data_deltas_disk_progress() {
    private_pipeline_native_cases(true, true, cases());
}
