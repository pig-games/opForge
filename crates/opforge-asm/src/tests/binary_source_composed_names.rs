//! Package-owned name recipes compared with fresh native TKVM metadata.
use super::*;
use crate::fs_uae_smoke::FsUaeSmokeOutcome;
use vm::portable_contract::PortableComposedName;
use vm::runtime_portable_types::{PortableTokenizeRequest, PortableTokenizerByteStream};

fn batch() -> (Vec<u8>, Vec<u8>) {
    let model = load_opasm_model_from_package_bytes(&tkpkg_smoke_package_bytes());
    let template = model
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap();
    let policy = model.resolve_token_policy("m68020", None).unwrap();
    let defaults = [
        "@1suffix",
        "symbol@1",
        "symbol@1@2suffix",
        "@1@2suffix",
        "@1",
        "@9",
        "@0suffix",
        "symbol@0",
        "symbol@1@",
        "symbol@1$tail",
        "symbol@1.tail",
        "plain",
        "name@word",
        "symbol @1",
        "@1 suffix",
        "@1_suffix",
        "symbol@10",
        "symbol@1,other",
        "module.symbol@1",
        ".local@1",
    ];
    let mut cases: Vec<_> = defaults
        .into_iter()
        .map(|source| (source, template.program.clone()))
        .collect();
    let mut custom = template.program[..template.program.len() - 69].to_vec();
    custom.extend([20, b'$', 2, 3, 1, b'a', 0]);
    for source in ["module.name$2a", "name$2b", "@2a", "name$1a"] {
        cases.push((source, custom.clone()));
    }
    let mut input = (cases.len() as u32).to_be_bytes().to_vec();
    let mut oracle = Vec::new();
    for (source, bytes) in cases {
        let mut program = template.clone();
        program.program = bytes;
        let request = PortableTokenizeRequest {
            family_id: "motorola68000",
            cpu_id: "m68020",
            dialect_id: "motorola68k",
            source_line: source,
            source_stream: PortableTokenizerByteStream::from_source_line(source),
            line_num: 1,
            token_policy: policy.clone(),
        };
        let tokens = model.tokenize_with_vm_core(&request, &program).unwrap();
        input.extend(template.limits.max_steps_per_line.to_be_bytes());
        input.extend((program.program.len() as u16).to_be_bytes());
        // Reporting only: the VM receives the identical source and package program.
        input.extend((0x2000 | source.len() as u16).to_be_bytes());
        input.extend(&program.program);
        input.extend(source.as_bytes());
        if source.len() % 2 != 0 {
            input.push(0);
        }
        oracle.extend(0u32.to_be_bytes());
        let (status, consumed, payload) = match &tokens[0].composed_name {
            Some(PortableComposedName::Recipe {
                consumed_tokens,
                packed_payload,
            }) => (
                0x8000u32,
                u32::from(*consumed_tokens),
                packed_payload.as_slice(),
            ),
            Some(PortableComposedName::Invalid) => (0x4000, 0, &[][..]),
            None => (0, 0, &[][..]),
        };
        oracle.extend(status.to_be_bytes());
        oracle.extend(consumed.to_be_bytes());
        oracle.extend((payload.len() as u32).to_be_bytes());
        oracle.extend(payload);
        oracle.resize(oracle.len() + 256 - payload.len(), 0);
    }
    assert!(input.len() < 8192);
    (input, oracle)
}

#[test]
fn composed_name_live_oracle() {
    let (_, oracle) = batch();
    assert_eq!(oracle.len(), 24 * 272);
}

#[test]
#[ignore = "requires configured FS-UAE; fresh package-selected composed-name metadata"]
fn composed_name_fs_uae() {
    let (input, oracle) = batch();
    let result =
        crate::fs_uae_smoke::run_tkvm_branch_harness_from_env(&workspace_root(), &input, &oracle)
            .expect("fresh composed-name VM comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real native proof required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    eprintln!(
        "COMPOSED_NAMES cases=24 seconds={:?}",
        runs[0].start_to_done_host_seconds
    );
}

#[test]
#[ignore = "requires configured FS-UAE; recipe capacity failure retains raw head and committed extent"]
fn composed_name_capacity_fs_uae() {
    let model = load_opasm_model_from_package_bytes(&tkpkg_smoke_package_bytes());
    let template = model
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap();
    let mut input = 1u32.to_be_bytes().to_vec();
    input.extend(template.limits.max_steps_per_line.to_be_bytes());
    input.extend((template.program.len() as u16).to_be_bytes());
    input.extend(0x6006u16.to_be_bytes()); // Reporting flags, fixed 14-byte scratch.
    input.extend(&template.program);
    input.extend(b"name@1");
    let mut oracle = 3u32.to_be_bytes().to_vec();
    for word in [0u32, 6, 0, 0, 0, 0] {
        oracle.extend(word.to_be_bytes());
    }
    oracle.resize(296, 0); // No published recipe on failure.
    let result =
        crate::fs_uae_smoke::run_tkvm_branch_harness_from_env(&workspace_root(), &input, &oracle)
            .expect("fresh composed-name capacity comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real native proof required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
}

#[test]
#[ignore = "requires configured FS-UAE; number boundaries preserve package tokenization"]
fn composed_name_number_boundaries_fs_uae() {
    let model = load_opasm_model_from_package_bytes(&tkpkg_smoke_package_bytes());
    let program = model
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap();
    let policy = model.resolve_token_policy("m68020", None).unwrap();
    let sources = ["1$1", "1%1", "1@1", "1$g"];
    let mut input = (sources.len() as u32).to_be_bytes().to_vec();
    let mut oracle = Vec::new();
    for source in sources {
        let request = PortableTokenizeRequest {
            family_id: "motorola68000",
            cpu_id: "m68020",
            dialect_id: "motorola68k",
            source_line: source,
            source_stream: PortableTokenizerByteStream::from_source_line(source),
            line_num: 1,
            token_policy: policy.clone(),
        };
        let tokens = model.tokenize_with_vm_core(&request, &program).unwrap();
        input.extend(program.limits.max_steps_per_line.to_be_bytes());
        input.extend((program.program.len() as u16).to_be_bytes());
        input.extend((0x1000 | source.len() as u16).to_be_bytes());
        input.extend(&program.program);
        input.extend(source.as_bytes());
        if source.len() % 2 != 0 {
            input.push(0);
        }
        oracle.extend(0u32.to_be_bytes());
        oracle.extend((tokens.len() as u32).to_be_bytes());
        for index in 0..8 {
            let kind = match tokens.get(index).map(|token| &token.kind) {
                Some(PortableTokenKind::Identifier(_)) => 0u32,
                Some(PortableTokenKind::Number { .. }) => 2,
                Some(PortableTokenKind::Dollar) => 6,
                Some(PortableTokenKind::At) => 40,
                Some(PortableTokenKind::Operator(
                    vm::portable_contract::PortableOperatorKind::Mod,
                )) => 23,
                None => u32::MAX,
                other => panic!("unexpected token: {other:?}"),
            };
            oracle.extend(kind.to_be_bytes());
        }
    }
    let result =
        crate::fs_uae_smoke::run_tkvm_branch_harness_from_env(&workspace_root(), &input, &oracle)
            .expect("fresh number-boundary comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real native proof required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
}
