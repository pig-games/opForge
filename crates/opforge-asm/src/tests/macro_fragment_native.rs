//! Exact native proof for raw macro-fragment recipes using the Rust PRVM oracle.
use super::*;
use package::package::{
    macro_fragment_program, PARSER_VM_MACRO_FRAGMENT_ENTRY, PARSER_VM_MACRO_VERSION,
};
use vm::macro_fragment_vm::execute;

const RESULT_BYTES: usize = 2048;

#[derive(Clone)]
struct Case {
    source: String,
    capacity: usize,
    budget: usize,
    program: Vec<u8>,
}

fn case(source: &str) -> Case {
    Case {
        source: source.to_owned(),
        capacity: 64,
        budget: 4096,
        program: macro_fragment_program(),
    }
}

fn batch() -> (Vec<u8>, Vec<u8>, usize) {
    let mut cases = [
        "",
        "literal only",
        "@1 @9 .name .{name} .@",
        "é@2/ü.{foo_2}",
        "'@1' \".{quoted}\" \\@3",
        "@0 @10 .{} .{bad .{a-b} ._x .a_b",
        "@1@2.name.name",
        "..name @1x",
        "%!%3!%![named]",
        "text .{long_name_7} tail",
    ]
    .into_iter()
    .map(case)
    .collect::<Vec<_>>();

    // Grammar changes: positional marker %, list marker !, indices 3..5, brackets.
    let alternate = [0x86, 1, b'%', b'!', b'3', b'5', b'[', b']', 0x83, 0];
    cases[8].program = alternate.to_vec();
    for source in ["", "x", "@1", ".name", ".{name}"] {
        let mut exact = case(source);
        exact.budget = match source {
            "" => 10,
            "x" => 12,
            "@1" => 12,
            ".name" | ".{name}" => 16,
            _ => unreachable!(),
        };
        cases.push(exact.clone());
        exact.budget -= 1;
        cases.push(exact);
    }

    let mut cap_zero = case("@1");
    cap_zero.capacity = 0;
    cases.push(cap_zero);
    let mut cap_one = case("@1/@2");
    cap_one.capacity = 1;
    cases.push(cap_one);
    let base = macro_fragment_program();
    for program in [
        base[..9].to_vec(), // truncated
        {
            let mut p = base.clone();
            p[0] = 0xf0;
            p
        }, // unknown opcode
        {
            let mut p = base.clone();
            p[2] = b'.';
            p
        }, // duplicate markers
        {
            let mut p = base.clone();
            p[4] = b'9';
            p[5] = b'1';
            p
        }, // reversed digits
        {
            let mut p = base.clone();
            p[1] = 2;
            p
        }, // unsupported policy
        {
            let mut p = base.clone();
            p.push(0);
            p
        }, // trailing opcode
        {
            let mut p = base.clone();
            p[6] = b'a';
            p
        }, // invalid punctuation marker
    ] {
        let mut invalid = case("@1.name");
        invalid.program = program;
        cases.push(invalid);
    }

    cases.push(case(&"x".repeat(1024)));
    cases.push(case(&"x".repeat(1025)));

    let count = cases.len();
    let mut input = (count as u32).to_be_bytes().to_vec();
    let mut oracle = Vec::with_capacity(count * (16 + RESULT_BYTES));
    for item in cases {
        assert!(item.source.len() <= 1025);
        assert!(RESULT_BYTES <= 2048);
        input.extend((item.budget as u32).to_be_bytes());
        input.extend((item.capacity as u32 * 32).to_be_bytes());
        input.extend((item.source.len() as u16).to_be_bytes());
        input.extend((item.program.len() as u16).to_be_bytes());
        input.extend(0u16.to_be_bytes()); // entry 4 consumes raw bytes; no token spans
        input.extend(PARSER_VM_MACRO_FRAGMENT_ENTRY.to_be_bytes());
        input.extend(&item.program);
        if item.program.len() % 2 != 0 {
            input.push(0);
        }
        input.extend(item.source.as_bytes());
        if item.source.len() % 2 != 0 {
            input.push(0);
        }

        let mut output = vec![0xa5; RESULT_BYTES];
        let (status, offset, count) = match execute(
            PARSER_VM_MACRO_FRAGMENT_ENTRY,
            PARSER_VM_MACRO_VERSION,
            &item.program,
            &item.source,
            item.capacity,
            item.budget,
        ) {
            Ok(records) => {
                for (index, record) in records.iter().enumerate() {
                    output[index * 32..(index + 1) * 32].copy_from_slice(&record.encode());
                }
                (0, 0, records.len() as u32)
            }
            Err(error) => (error.status, error.offset, 0),
        };
        for value in [status, offset, count, count * 32] {
            oracle.extend(value.to_be_bytes());
        }
        oracle.extend(output);
    }
    (input, oracle, count)
}

#[test]
fn macro_fragment_live_rust_oracle() {
    let (_, oracle, count) = batch();
    assert_eq!(oracle.len(), count * (16 + RESULT_BYTES));
    for (index, record) in oracle.chunks_exact(16 + RESULT_BYTES).enumerate() {
        let status = u32::from_be_bytes(record[..4].try_into().unwrap());
        if status != 0 {
            assert!(
                record[16..].iter().all(|byte| *byte == 0xa5),
                "failure publication at case {index}"
            );
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; exact macro fragment records"]
fn macro_fragment_native_fs_uae() {
    native(false);
}

#[test]
#[ignore = "requires configured FS-UAE; macro fragment telemetry preservation"]
fn macro_fragment_profiled_fs_uae() {
    native(true);
}

fn native(profiled: bool) {
    let (input, oracle, count) = batch();
    let result = crate::fs_uae_smoke::run_prvm_macro_harness_from_env(
        &workspace_root(),
        &input,
        &oracle,
        profiled,
    )
    .expect("fresh macro fragment comparison");
    let crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real native macro fragment proof required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    eprintln!(
        "MACRO_FRAGMENTS cases={count} profiled={profiled} seconds={:?}",
        runs[0].start_to_done_host_seconds
    );
}
