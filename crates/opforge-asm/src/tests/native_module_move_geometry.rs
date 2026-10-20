//! Absolute memory-source MOVE fields from the remaining real-module failures.
use super::{private_oracle, private_pipeline_native_cases, PrivateCase, PRIVATE_OUTPUTS};

fn local_case(name: &'static str, instruction: &str) -> PrivateCase {
    PrivateCase {
        name,
        accepted_modules: 1,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use worker\n.section code,kind=code\n.long worker.entry\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/worker.asm", format!(".module worker\n.cpu 68020\n.pub\n.section code,kind=code\nentry .block\n {instruction}\n rts\n.bend\n.endsection\n.section data,kind=data\ncell .block\n.long $12345678,$9abcdef0\n.bend\n.endsection\n.endmodule\n")),
        ],
    }
}

fn imported_case() -> PrivateCase {
    PrivateCase {
        name: "private-move-imported-memory-long",
        accepted_modules: 2,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use worker\n.section code,kind=code\n.long worker.entry\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/state.asm", ".module state\n.cpu 68020\n.pub\n.section data,kind=data\ncell .block\n.long $12345678,$9abcdef0\n.bend\n.endsection\n.endmodule\n".into()),
            ("library/worker.asm", ".module worker\n.cpu 68020\n.use state\n.pub\n.section code,kind=code\nentry .block\n move.l state.cell,d0\n rts\n.bend\n.endsection\n.endmodule\n".into()),
        ],
    }
}

fn bare_bss_case() -> PrivateCase {
    let mut case = local_case("private-move-bare-bss-word-addend", "move.w cell+2,d0");
    case.files[0].1 = case.files[0].1.replace(
        PRIVATE_OUTPUTS,
        &PRIVATE_OUTPUTS
            .replace(
                ".place data in ram",
                ".place data in ram\n.place storage in ram",
            )
            .replace(
                "format=hunk,sections=code,data",
                "format=hunk,sections=code,data,storage",
            ),
    );
    case.files[1].1 = case.files[1].1.replace(
        ".section data,kind=data\ncell .block\n.long $12345678,$9abcdef0\n.bend",
        ".section storage,kind=bss\ncell\n.res byte,8",
    );
    case
}

fn scalar_difference_case(
    name: &'static str,
    alignment: &str,
    accepted_modules: u32,
) -> PrivateCase {
    let mut case = local_case(name, "move.l #sizeEnd-sizeStart,d0");
    case.accepted_modules = accepted_modules;
    case.files[1].1 = case.files[1].1.replace(
        "cell .block\n.long $12345678,$9abcdef0\n.bend",
        &format!("sizeStart\n.long $12345678,$9abcdef0\n{alignment}sizeEnd\n"),
    );
    case
}

fn other_owner_difference_case() -> PrivateCase {
    let mut case = local_case(
        "private-move-scalar-difference-other-owner",
        "move.l #cell.sizeEnd-cell,d0",
    );
    case.accepted_modules = 0;
    case.files[1].1 = case.files[1].1.replace(
        ".long $12345678,$9abcdef0\n.bend",
        ".long $12345678,$9abcdef0\nsizeEnd\n.bend",
    );
    case
}

pub(super) fn cases() -> Vec<PrivateCase> {
    vec![
        local_case("private-move-local-memory-long", "move.l cell,d0"),
        local_case("private-move-local-memory-word", "move.w cell,d0"),
        local_case("private-move-local-memory-long-addend", "move.l cell+2,d0"),
        local_case("private-move-local-memory-word-addend", "move.w cell+2,d0"),
        local_case(
            "private-move-local-address-to-displacement",
            "move.l #cell,4(a0)",
        ),
        local_case(
            "private-move-immediate-pointer-to-absolute",
            "move.l #cell,cell+4",
        ),
        local_case(
            "private-move-immediate-constant-to-absolute",
            "move.l #$12345678,cell",
        ),
        scalar_difference_case("private-move-scalar-difference-same-fragment", "", 1),
        scalar_difference_case(
            "private-move-scalar-difference-across-align",
            ".align 8\n",
            0,
        ),
        other_owner_difference_case(),
        bare_bss_case(),
        imported_case(),
    ]
}

pub(super) fn check(name: &str, binary: &[u8]) {
    let opcode = if name.contains("word") {
        [0x30, 0x39]
    } else if name == "private-move-local-address-to-displacement" {
        [0x21, 0x7c]
    } else if name.starts_with("private-move-immediate-") {
        [0x23, 0xfc]
    } else if name.starts_with("private-move-scalar-difference-") {
        [0x20, 0x3c]
    } else {
        [0x20, 0x39]
    };
    assert!(binary.windows(2).any(|bytes| bytes == opcode));
    if !name.contains("bss") {
        assert!(binary
            .windows(8)
            .any(|bytes| bytes == [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0]));
    }
    assert!(binary.ends_with(&[0x11, 0x22]));
}

pub(super) fn relocation_count(name: &str) -> usize {
    if name.starts_with("private-move-scalar-difference-") {
        1 // only the root's pointer to entry; the proved difference is absolute
    } else if name == "private-move-immediate-pointer-to-absolute" {
        3 // the root pointer and both independent absolute MOVE fields
    } else {
        2 // the root pointer and one absolute MOVE field
    }
}

pub(super) fn object_counts(name: &str) -> Option<(usize, usize)> {
    name.starts_with("private-move-").then_some((
        0,
        if name == "private-move-imported-memory-long" {
            2
        } else if matches!(
            name,
            "private-move-scalar-difference-across-align"
                | "private-move-scalar-difference-other-owner"
        ) {
            0
        } else {
            1
        },
    ))
}

#[test]
fn module_move_geometry_live_rust_oracles() {
    for case in cases() {
        private_oracle(&case);
    }
}

#[test]
#[ignore = "requires instrumented compact CLI; local and imported absolute memory MOVE fields"]
fn native_module_move_geometry_disk_progress() {
    private_pipeline_native_cases(true, true, cases());
}
