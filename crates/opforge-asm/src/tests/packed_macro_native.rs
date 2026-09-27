//! Fresh packed-boundary comparison; string/composite payloads stay opaque.
use super::*;
use package::package::{
    packed_macro_call_program, PARSER_VM_MACRO_VERSION, PARSER_VM_PACKED_MACRO_ENTRY,
};

const RESULT_BYTES: usize = 64 * 32;

fn line(tokens: &[u8]) -> Vec<u8> {
    let mut bytes = vec![0, 0, 0, 1];
    bytes.extend(tokens);
    bytes[0] = (bytes.len() - 1) as u8;
    bytes
}

fn batch() -> (Vec<u8>, Vec<u8>, usize) {
    let head = [7, 0, 0, 7, 0];
    let mut cases = vec![];
    for args in [
        vec![],
        vec![2, 0, 0, 0, 1, 4, 2, 0, 0, 0, 2],
        vec![
            14, 10, 2, 0, 0, 0, 1, 4, 2, 0, 0, 0, 2, 11, 4, 3, 3, b'a', b',', b'b', 15,
        ],
        vec![41, 7, 0, 1, 0, 3, b'a', b',', b'b'],
        vec![15, 4, 2, 0, 0, 0, 7],
        vec![4, 2, 0, 0, 0, 7],
        vec![2, 0, 0, 0, 1, 4, 4, 2, 0, 0, 0, 2],
        vec![14, 2, 0, 0, 0, 1],
        vec![3, 4, b'a'],
    ] {
        let mut tokens = head.to_vec();
        tokens.extend(args);
        cases.push((line(&tokens), 64usize, 4096usize, 0u8));
    }
    let mut labeled = vec![0, 0, 9, 0, 5];
    labeled.extend(head);
    labeled.extend([2, 0, 0, 0, 3]);
    cases.push((line(&labeled), 64, 4096, 0));
    let mut with_plan = line(&head);
    with_plan[1] |= 32;
    with_plan.extend([42, 0, 0, 0, 1, 6]);
    with_plan[0] = (with_plan.len() - 1) as u8;
    cases.push((with_plan, 64, 4096, 0));
    cases.push((line(&head), 64, 1, 0));
    cases.push((line(&head), 0, 4096, 0));
    cases.push((line(&head), 64, 4096, 1));
    cases.push((line(&head), 64, 4096, 2));
    // Preserve the original packed argument validator: matched delimiters,
    // at most 16 nested delimiters, and balanced completion.
    for args in [
        [vec![10; 16], vec![2, 0, 0, 0, 1], vec![11; 16]].concat(),
        vec![10, 15],
        vec![10, 2, 0, 0, 0, 1],
        [vec![10; 17], vec![2, 0, 0, 0, 1], vec![11; 17]].concat(),
    ] {
        let mut tokens = head.to_vec();
        tokens.extend(args);
        cases.push((line(&tokens), 64, 4096, 0));
    }
    let count = cases.len();
    let mut input = (count as u32).to_be_bytes().to_vec();
    let mut oracle = vec![];
    for (source, capacity, steps, variation) in cases {
        let mut program = packed_macro_call_program();
        match variation {
            1 => program[1] = 128,
            2 => program.insert(program.len() - 1, 0xf0),
            _ => (),
        }
        input.extend((steps as u32).to_be_bytes());
        input.extend((capacity as u32 * 32).to_be_bytes());
        input.extend((source.len() as u16).to_be_bytes());
        input.extend((program.len() as u16).to_be_bytes());
        input.extend(0u16.to_be_bytes());
        input.extend(PARSER_VM_PACKED_MACRO_ENTRY.to_be_bytes());
        input.extend(&program);
        if program.len() % 2 != 0 {
            input.push(0);
        }
        input.extend(&source);
        if source.len() % 2 != 0 {
            input.push(0);
        }
        let mut result = vec![0xa5; RESULT_BYTES];
        let (status, offset, records) = match vm::packed_macro_vm::execute(
            PARSER_VM_PACKED_MACRO_ENTRY,
            PARSER_VM_MACRO_VERSION,
            &program,
            &source,
            capacity,
            steps,
        ) {
            Ok(records) => {
                for (index, record) in records.iter().enumerate() {
                    result[index * 32..(index + 1) * 32].copy_from_slice(&record.encode());
                }
                (0, 0, records.len() as u32)
            }
            Err(error) => (error.status, error.offset, 0),
        };
        for value in [status, offset, records, records * 32] {
            oracle.extend(value.to_be_bytes());
        }
        oracle.extend(result);
    }
    (input, oracle, count)
}

#[test]
fn packed_macro_live_oracle() {
    let (_, oracle, count) = batch();
    assert_eq!(oracle.len(), count * (16 + RESULT_BYTES));
    let successes = [0, 1, 2, 3, 5, 9, 10, 15];
    for (index, output) in oracle.chunks_exact(16 + RESULT_BYTES).enumerate() {
        assert_eq!(
            output[..4] == [0; 4],
            successes.contains(&index),
            "case {index}"
        );
        if !successes.contains(&index) {
            assert!(output[16..].iter().all(|byte| *byte == 0xa5));
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; exact packed macro boundaries"]
fn packed_macro_native_fs_uae() {
    let (input, oracle, count) = batch();
    let outcome = crate::fs_uae_smoke::run_prvm_macro_harness_from_env(
        &workspace_root(),
        &input,
        &oracle,
        false,
    )
    .expect("fresh packed macro boundary comparison");
    let crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native proof required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    eprintln!(
        "PACKED_MACRO cases={count} seconds={:?}",
        runs[0].start_to_done_host_seconds
    );
}
