//! Exact native descriptor proof from live generic TKVM spans and Rust PRVM.
#[path = "binary_source_hunk.rs"]
mod hunk;
use super::*;
use package::package::{macro_descriptor_program, PARSER_VM_MACRO_ENTRY, PARSER_VM_MACRO_VERSION};
use vm::macro_descriptor_vm::{execute, TokenSpan};
use vm::portable_contract::{PortableOperatorKind, PortableTokenKind};
use vm::runtime_portable_types::{PortableTokenizeRequest, PortableTokenizerByteStream};

const RESULT_BYTES: usize = 64 * 32;

fn kind_code(kind: &PortableTokenKind) -> u16 {
    match kind {
        PortableTokenKind::Identifier(_) => 0,
        PortableTokenKind::Register(_) => 1,
        PortableTokenKind::Number { .. } => 2,
        PortableTokenKind::String { .. } => 3,
        PortableTokenKind::Comma => 4,
        PortableTokenKind::Colon => 5,
        PortableTokenKind::Dollar => 6,
        PortableTokenKind::Dot => 7,
        PortableTokenKind::Hash => 8,
        PortableTokenKind::Question => 9,
        PortableTokenKind::OpenBracket => 10,
        PortableTokenKind::CloseBracket => 11,
        PortableTokenKind::OpenBrace => 12,
        PortableTokenKind::CloseBrace => 13,
        PortableTokenKind::OpenParen => 14,
        PortableTokenKind::CloseParen => 15,
        PortableTokenKind::At => 40,
        PortableTokenKind::Operator(op) => {
            let ordered = [
                PortableOperatorKind::Range,
                PortableOperatorKind::RangeInclusive,
                PortableOperatorKind::Plus,
                PortableOperatorKind::Minus,
                PortableOperatorKind::Multiply,
                PortableOperatorKind::Power,
                PortableOperatorKind::Divide,
                PortableOperatorKind::Mod,
                PortableOperatorKind::Shl,
                PortableOperatorKind::Shr,
                PortableOperatorKind::BitNot,
                PortableOperatorKind::LogicNot,
                PortableOperatorKind::BitAnd,
                PortableOperatorKind::BitOr,
                PortableOperatorKind::BitXor,
                PortableOperatorKind::LogicAnd,
                PortableOperatorKind::LogicOr,
                PortableOperatorKind::LogicXor,
                PortableOperatorKind::Eq,
                PortableOperatorKind::Ne,
                PortableOperatorKind::Ge,
                PortableOperatorKind::Gt,
                PortableOperatorKind::Le,
                PortableOperatorKind::Lt,
            ];
            16 + ordered.iter().position(|value| value == op).unwrap() as u16
        }
    }
}

fn batch() -> (Vec<u8>, Vec<u8>, usize) {
    let model = load_opasm_model_from_package_bytes(&tkpkg_smoke_package_bytes());
    let tokenizer = model
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap();
    let policy = model.resolve_token_policy("m68020", None).unwrap();
    let at_limit = format!(".emit {}", vec!["a"; 63].join(","));
    let beyond_limit = format!(".emit {}", vec!["a"; 64].join(","));
    let mut cases = vec![];
    for source in [
        ".emit",
        ".emit  a , [1, 2], {3,4}  ",
        ".emit(a, \"x,y\")",
        "  entry: .emit , #0, Frame.Value(a4)",
        "inside .emit [1,(2,3)],4",
        ".emit ), 7",
        ".emit \"a;\\\"b\", 'x,y' ; ignored, tail",
        ".emit( a, (b,c) )  ; comment",
        ".emit a,,b",
        ".emit,",
        ".emit(a) extra",
        ".emit(a",
    ] {
        cases.push((source, false, 64usize, 4096usize, 0u8));
    }
    for source in [
        "emit .macro first, second=7",
        ".macro emit(byte first=[1,2], second=\"x,y\")",
        "  emit: .segment first=1, second= ",
        ".macro emit",
        "emit .macro(first)",
        ".segment emit(first=1) extra",
        ".macro emit(first=\"a=b\", second={2,3}) ; comment",
    ] {
        cases.push((source, true, 64, 4096, 0));
    }
    // Capacity/budget/policy failures must leave all caller output bytes intact.
    cases.extend([
        (".emit a,b", false, 2, 4096, 0),
        (".emit a,b", false, 64, 1, 0),
        (".emit a,b", false, 64, 4096, 1),
        (".emit a,b", false, 64, 4096, 2),
        (".emit a,b", false, 64, 4096, 3),
    ]);
    cases.extend([
        (".emit a,b", false, 64, 4096, 4),
        (".emit a,b", false, 64, 4096, 5),
        (".emit a,b", false, 64, 4096, 6),
        (".emit a,b", false, 64, 4096, 7),
        (".emit a,b", false, 64, 4096, 8),
        (at_limit.as_str(), false, 64, 4096, 0),
        (beyond_limit.as_str(), false, 64, 4096, 0),
    ]);
    for variation in 9..=15 {
        cases.push((".emit a,b", false, 64, 4096, variation));
    }
    let count = cases.len();
    let mut input = (count as u32).to_be_bytes().to_vec();
    let mut oracle = Vec::new();
    for (source, header, capacity, budget, variation) in cases {
        let request = PortableTokenizeRequest {
            family_id: "motorola68000",
            cpu_id: "m68020",
            dialect_id: "motorola68k",
            source_line: source,
            source_stream: PortableTokenizerByteStream::from_source_line(source),
            line_num: 1,
            token_policy: policy.clone(),
        };
        let tokens = model
            .tokenize_with_vm_core(&request, &tokenizer)
            .expect(source);
        let mut spans = tokens
            .iter()
            .map(|token| TokenSpan {
                start: (token.span.col_start - 1) as u32,
                end: (token.span.col_end - 1) as u32,
            })
            .collect::<Vec<_>>();
        match variation {
            4 => spans[0].end = source.len() as u32 + 1,
            5 => {
                spans[0].start = 1;
                spans[0].end = 1;
            }
            6 => {
                spans.pop();
            }
            _ => (),
        }
        let mut program = macro_descriptor_program(header);
        match variation {
            1 => program[2] = 0x80,
            2 => program.pop().map(|_| ()).unwrap(),
            3 => program[4] = 0,
            7 => program[0] = 0xf0,
            8 => program.push(0),
            9 => program = vec![0x80, 1],
            10 => program.splice(3..3, [0x80, 1, 15]).for_each(drop),
            11 => program.splice(6..6, [0x81, 1, b',']).for_each(drop),
            12 => program = vec![0x83, 0],
            13 => program.insert(program.len() - 1, 0xf0),
            14 => program.insert(program.len() - 1, 0x83),
            15 => {
                program.remove(program.len() - 2);
            }
            _ => (),
        }
        input.extend((budget as u32).to_be_bytes());
        input.extend((capacity as u32 * 32).to_be_bytes());
        input.extend((source.len() as u16).to_be_bytes());
        input.extend((program.len() as u16).to_be_bytes());
        input.extend((spans.len() as u16).to_be_bytes());
        input.extend(PARSER_VM_MACRO_ENTRY.to_be_bytes());
        input.extend(&program);
        if program.len() % 2 != 0 {
            input.push(0);
        }
        input.extend(source.as_bytes());
        if source.len() % 2 != 0 {
            input.push(0);
        }
        for (token, span) in tokens.iter().zip(&spans) {
            input.extend(kind_code(&token.kind).to_be_bytes());
            input.extend(0u16.to_be_bytes());
            input.extend((span.start + 1).to_be_bytes());
            input.extend((span.end + 1).to_be_bytes());
            // Descriptor VM consumes source spans, never decoded lexeme data.
            input.extend(0u32.to_be_bytes());
            input.extend(0u32.to_be_bytes());
        }
        let mut output = vec![0xa5; RESULT_BYTES];
        let (status, offset, count) = match execute(
            PARSER_VM_MACRO_ENTRY,
            PARSER_VM_MACRO_VERSION,
            &program,
            source,
            &spans,
            capacity,
            budget,
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
fn macro_descriptor_live_token_oracle() {
    let (_, oracle, count) = batch();
    assert_eq!(oracle.len(), count * (16 + RESULT_BYTES));
    let successful = [0, 1, 2, 3, 4, 5, 6, 7, 12, 13, 14, 15, 18, 29];
    for (index, record) in oracle.chunks_exact(16 + RESULT_BYTES).enumerate() {
        let status = u32::from_be_bytes(record[..4].try_into().unwrap());
        assert_eq!(status == 0, successful.contains(&index), "case {index}");
        if status != 0 {
            assert!(
                record[16..].iter().all(|byte| *byte == 0xa5),
                "case {index}"
            );
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; exact macro descriptors and failed publication"]
fn macro_descriptor_native_fs_uae() {
    native(false);
}

#[test]
#[ignore = "requires configured FS-UAE; macro descriptor telemetry preservation"]
fn macro_descriptor_profiled_fs_uae() {
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
    .expect("fresh macro descriptor comparison");
    let crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real native descriptor proof required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    let image = &runs[0].captured_artifacts[&PathBuf::from("Work/build/prvm_macro_harness")];
    eprintln!(
        "MACRO_DESCRIPTORS cases={count} profiled={profiled} seconds={:?} image_bytes={} linked_reserved_bytes={}",
        runs[0].start_to_done_host_seconds,
        image.len(),
        hunk::allocation(image).unwrap().total()
    );
}

#[test]
fn generated_spelling_boundaries_use_configured_vm() {
    let model = load_opasm_model_from_package_bytes(&tkpkg_smoke_package_bytes());
    let tokenizer = model
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap();
    let policy = model.resolve_token_policy("m68020", None).unwrap();
    for (source, expected) in [
        ("(a,b), c", vec!["(a,b)", "c"]),
        ("  a , [1,2]  ", vec!["a", "[1,2]"]),
        ("\"x,y\", 'z'", vec!["\"x,y\"", "'z'"]),
        ("), b", vec![")", "b"]),
        ("a ; trailing,comment", vec!["a"]),
    ] {
        let request = PortableTokenizeRequest {
            family_id: "motorola68000",
            cpu_id: "m68020",
            dialect_id: "motorola68k",
            source_line: source,
            source_stream: PortableTokenizerByteStream::from_source_line(source),
            line_num: 1,
            token_policy: policy.clone(),
        };
        let records = vm::macro_spelling_vm::execute(
            &model,
            &tokenizer,
            &request,
            &package::package::macro_spelling_program(),
            64,
            4096,
        )
        .unwrap();
        let actual = records
            .iter()
            .skip(1)
            .map(|r| &source[r.source_start as usize..r.source_end as usize])
            .collect::<Vec<_>>();
        assert_eq!(actual, expected, "{source}");
    }
}
