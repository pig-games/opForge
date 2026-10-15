//! Shared token/spelling descriptors compared with live TKVM and eager PRVM.
use super::*;
use package::package::{macro_descriptor_program, macro_fragment_program};
use vm::binary_spelling_vm::{capture, Token};
use vm::macro_descriptor_vm::{Descriptor, DescriptorError, TokenSpan};
use vm::runtime_portable_types::{PortableTokenizeRequest, PortableTokenizerByteStream};

fn same_result(
    source: &str,
    eager: Result<Vec<Descriptor>, DescriptorError>,
    binary: Result<Vec<Descriptor>, DescriptorError>,
) {
    match (eager, binary) {
        (Ok(a), Ok(b)) => assert_eq!(a, b, "{source}"),
        (Err(a), Err(b)) => assert_eq!((a.status, a.offset), (b.status, b.offset), "{source}"),
        (a, b) => panic!("{source}: eager={a:?}, binary={b:?}"),
    }
}

#[test]
fn shared_binary_spelling_descriptors_match_live_tkvm() {
    let model = load_opasm_model_from_package_bytes(&tkpkg_smoke_package_bytes());
    let tokenizer = model
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap();
    let policy = model.resolve_token_policy("m68020", None).unwrap();
    for (header, source) in [
        (false, ".emit"),
        (false, ".emit  a , [1, 2], {3,4}  "),
        (false, ".emit(a, \"x,y\")"),
        (false, "  entry: .emit , #0, Frame.Value(a4)"),
        (false, "inside .emit [1,(2,3)],4"),
        (false, ".emit ), 7"),
        (false, ".emit \"a;\\\"b\", 'x,y' ; ignored, tail"),
        (false, ".emit( a, (b,c) )  ; comment"),
        (false, ".emit a,,b"),
        (false, ".emit,"),
        (false, ".emit(a) extra"),
        (false, ".emit(a"),
        (false, ".emit 1 + 2"),
        (false, ".emit 1\t+\t2"),
        (false, ".emit a + b, c - d"),
        (false, ".emit .{name},@1,.@"),
        (false, ".emit \"\u{2003}@1\u{2003}\""),
        (true, "emit .macro first, second=7"),
        (true, ".macro emit(byte first=[1,2], second=\"x,y\")"),
        (true, "  emit: .segment first=1, second= "),
        (true, ".macro emit"),
        (true, "emit .macro(first)"),
        (true, ".segment emit(first=1) extra"),
        (true, ".macro emit(first=\"a=b\", second={2,3}) ; comment"),
        (true, ".macro emit first=, second=4"),
        (true, ".macro emit byte first, word second"),
        (true, ".macro emit first second third"),
        (true, ".macro emit =5"),
        (true, ".macro emit \"a=b\""),
    ] {
        let portable = model
            .tokenize_with_vm_core(
                &PortableTokenizeRequest {
                    family_id: "motorola68000",
                    cpu_id: "m68020",
                    dialect_id: "motorola68k",
                    source_line: source,
                    source_stream: PortableTokenizerByteStream::from_source_line(source),
                    line_num: 1,
                    token_policy: policy.clone(),
                },
                &tokenizer,
            )
            .expect(source);
        let tokens = portable
            .iter()
            .map(|t| Token {
                kind: super::macro_descriptor_native::kind_code(&t.kind),
                start: (t.span.col_start - 1) as u16,
                end: (t.span.col_end - 1) as u16,
            })
            .collect::<Vec<_>>();
        let spans = tokens
            .iter()
            .map(|t| TokenSpan {
                start: t.start as u32,
                end: t.end as u32,
            })
            .collect::<Vec<_>>();
        let (code, _) = opcore::text_utils::split_comment(source);
        let block = capture(code, &tokens).expect(source);
        let program = macro_descriptor_program(header);
        for (capacity, budget) in [(64, 65536), (1, 65536), (64, 1)] {
            same_result(
                source,
                vm::macro_descriptor_vm::execute(2, 2, &program, source, &spans, capacity, budget),
                vm::binary_macro_descriptor_vm::execute(2, 2, &program, &block, capacity, budget),
            );
        }
        // Recipe ranges use the exact spelling owned by the descriptor, with
        // whitespace and escaped marker bytes still preceding string decoding.
        if let Ok(rows) = vm::binary_macro_descriptor_vm::execute(2, 2, &program, &block, 64, 65536)
        {
            for row in rows.iter().filter(|r| matches!(r.kind, 8 | 9 | 11)) {
                let a = row.source_start as usize;
                let b = row.source_end as usize;
                same_result(
                    source,
                    vm::macro_fragment_vm::execute(
                        4,
                        2,
                        &macro_fragment_program(),
                        &code[a..b],
                        64,
                        65536,
                    ),
                    vm::binary_macro_fragment_vm::execute_range(
                        4,
                        2,
                        &macro_fragment_program(),
                        &block,
                        a..b,
                        64,
                        65536,
                    ),
                );
            }
        }
    }
}

#[test]
fn shared_binary_spelling_fragments_keep_raw_string_marker_order() {
    let model = load_opasm_model_from_package_bytes(&tkpkg_smoke_package_bytes());
    let tokenizer = model
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap();
    let policy = model.resolve_token_policy("m68020", None).unwrap();
    for source in [
        r#".emit "x@1""#,
        r#".emit "\x401""#,
        r#".emit "\x2ename""#,
        r#".emit "\@1", .{name}, .@, @10"#,
        ".emit 1\t+\t2",
        ".emit 1 + 2",
    ] {
        let tokens = model
            .tokenize_with_vm_core(
                &PortableTokenizeRequest {
                    family_id: "motorola68000",
                    cpu_id: "m68020",
                    dialect_id: "motorola68k",
                    source_line: source,
                    source_stream: PortableTokenizerByteStream::from_source_line(source),
                    line_num: 1,
                    token_policy: policy.clone(),
                },
                &tokenizer,
            )
            .unwrap()
            .iter()
            .map(|t| Token {
                kind: super::macro_descriptor_native::kind_code(&t.kind),
                start: (t.span.col_start - 1) as u16,
                end: (t.span.col_end - 1) as u16,
            })
            .collect::<Vec<_>>();
        let block = capture(source, &tokens).unwrap();
        for capacity in [0, 1, 2, 64] {
            for budget in [0, 10, 20, 40, 65536] {
                same_result(
                    source,
                    vm::macro_fragment_vm::execute(
                        4,
                        2,
                        &macro_fragment_program(),
                        source,
                        capacity,
                        budget,
                    ),
                    vm::binary_macro_fragment_vm::execute(
                        4,
                        2,
                        &macro_fragment_program(),
                        &block,
                        capacity,
                        budget,
                    ),
                );
            }
        }
    }
}
