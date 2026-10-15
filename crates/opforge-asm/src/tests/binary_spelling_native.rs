//! Live TKVM inputs, pointer-free views and sentinel-backed native service proof.
use super::*;
use package::package::{macro_descriptor_program, macro_fragment_program};
use vm::binary_spelling_vm::{capture, Token};
use vm::macro_descriptor_vm::Descriptor;
use vm::runtime_portable_types::{PortableTokenizeRequest, PortableTokenizerByteStream};

const RESULT_BYTES: usize = 4096;
fn append_case(
    buffers: (&mut Vec<u8>, &mut Vec<u8>),
    entry: u16,
    source: &[u8],
    program: &[u8],
    tokens: &[Token],
    limits: (usize, usize),
    expected: Result<(Vec<u8>, u32, u32), (u32, u32)>,
) {
    let (input, oracle) = buffers;
    let (capacity, budget) = limits;
    input.extend((budget as u32).to_be_bytes());
    input.extend((capacity as u32).to_be_bytes());
    for n in [
        source.len() as u16,
        program.len() as u16,
        tokens.len() as u16,
        entry,
    ] {
        input.extend(n.to_be_bytes());
    }
    for bytes in [program, source] {
        input.extend(bytes);
        if bytes.len() % 2 != 0 {
            input.push(0);
        }
    }
    for t in tokens {
        input.extend(t.kind.to_be_bytes());
        input.extend(0u16.to_be_bytes());
        input.extend((t.start as u32 + 1).to_be_bytes());
        input.extend((t.end as u32 + 1).to_be_bytes());
        input.extend([0u8; 8]);
    }
    let mut result = vec![0xa5; RESULT_BYTES];
    let status = match expected {
        Ok((bytes, offset, count)) => {
            result[..bytes.len()].copy_from_slice(&bytes);
            [0, offset, count, bytes.len() as u32]
        }
        Err((status, offset)) => [status, offset, 0, 0],
    };
    for n in status {
        oracle.extend(n.to_be_bytes());
    }
    oracle.extend(result);
}
fn recipes(
    result: Result<Vec<Descriptor>, vm::macro_descriptor_vm::DescriptorError>,
) -> Result<(Vec<u8>, u32, u32), (u32, u32)> {
    result
        .map(|r| {
            (
                r.iter().flat_map(Descriptor::encode).collect(),
                0,
                r.len() as u32,
            )
        })
        .map_err(|e| (e.status, e.offset))
}
fn batch() -> (Vec<u8>, Vec<u8>, usize) {
    let model = load_opasm_model_from_package_bytes(&tkpkg_smoke_package_bytes());
    let tokenizer = model
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap();
    let policy = model.resolve_token_policy("m68020", None).unwrap();
    let mut input = vec![0; 4];
    let mut oracle = Vec::new();
    let mut count = 0usize;
    for (source, header) in [
        (".emit", false),
        (".emit\ta , [1, 2], {3,4}  ", false),
        ("entry: .emit(a, \"x,y\") ; comment", false),
        (".emit \"a;\\\"b\", 'x,y'", false),
        (".emit @10, .name, .{name}, .@, \"@2\"", false),
        (".emit a,,b", false),
        (".emit(a) extra", false),
        ("emit .macro first, second=7", true),
        (".macro emit(byte first=[1,2], second=\"x,y\")", true),
        ("emit: .segment first=1, second= ", true),
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
        let live = model
            .tokenize_with_vm_core(&request, &tokenizer)
            .expect(source);
        let tokens = live
            .iter()
            .map(|t| Token {
                kind: super::macro_descriptor_native::kind_code(&t.kind),
                start: (t.span.col_start - 1) as u16,
                end: (t.span.col_end - 1) as u16,
            })
            .collect::<Vec<_>>();
        let mut end = tokens.last().map_or(0, |t| t.end as usize);
        while matches!(source.as_bytes().get(end), Some(b' ' | b'\t')) {
            end += 1;
        }
        let block = capture(&source[..end], &tokens).unwrap();
        append_case(
            (&mut input, &mut oracle),
            0,
            source.as_bytes(),
            &[],
            &tokens,
            (RESULT_BYTES, 65536),
            Ok((block.clone(), end as u32, tokens.len() as u32)),
        );
        count += 1;
        for (entry, program) in [
            (2, macro_descriptor_program(header)),
            (4, macro_fragment_program()),
        ] {
            for (capacity, budget) in [(64usize, 65536usize), (0, 65536), (64, 1)] {
                let result = if entry == 2 {
                    vm::binary_macro_descriptor_vm::execute(
                        2, 2, &program, &block, capacity, budget,
                    )
                } else {
                    vm::binary_macro_fragment_vm::execute(4, 2, &program, &block, capacity, budget)
                };
                append_case(
                    (&mut input, &mut oracle),
                    entry,
                    &block,
                    &program,
                    &[],
                    (capacity * 32, budget),
                    recipes(result),
                );
                count += 1;
            }
        }
        let mut invalid_views = Vec::new();
        for (offset, value) in [
            (0, 0xff),
            (4, 0xff),
            (6, 0xff),
            (8, 0xff),
            (10, 1),
            (12, 0xff),
            (16, 0xff),
            (18, 0xff),
            (20, 0xff),
            (22, 0xff),
            (24, 0xff),
        ] {
            let mut bad = block.clone();
            bad[offset] = value;
            invalid_views.push(bad);
        }
        let mut bad = block.clone();
        bad.pop();
        invalid_views.push(bad);
        let mut bad = block.clone();
        *bad.last_mut().unwrap() = 0xc2;
        invalid_views.push(bad);
        let atom_count = u16::from_be_bytes(block[4..6].try_into().unwrap()) as usize;
        for i in 0..atom_count {
            let row = 16 + i * 12;
            if block[row..row + 2] == [0xff, 0xff] {
                let payload =
                    u32::from_be_bytes(block[row + 8..row + 12].try_into().unwrap()) as usize;
                let mut bad = block.clone();
                bad[payload] = b'x';
                invalid_views.push(bad);
                break;
            }
        }
        // The shared native validation ABI reports status, rather than a source
        // error coordinate, for malformed input blocks.
        for malformed in invalid_views {
            assert!(vm::binary_spelling_vm::View::parse(&malformed).is_err());
            for (entry, program) in [
                (2, macro_descriptor_program(header)),
                (4, macro_fragment_program()),
            ] {
                append_case(
                    (&mut input, &mut oracle),
                    entry,
                    &malformed,
                    &program,
                    &[],
                    (2048, 65536),
                    Err((5, 0)),
                );
                count += 1;
            }
        }
        append_case(
            (&mut input, &mut oracle),
            0,
            source.as_bytes(),
            &[],
            &tokens,
            (1, 65536),
            Err((7, 0)),
        );
        count += 1;
    }
    input[..4].copy_from_slice(&(count as u32).to_be_bytes());
    (input, oracle, count)
}
#[test]
fn binary_spelling_native_live_oracle() {
    let (_, oracle, count) = batch();
    assert_eq!(oracle.len(), count * (16 + RESULT_BYTES));
    let successful = oracle
        .chunks_exact(16 + RESULT_BYTES)
        .filter(|r| r[..4] == [0; 4])
        .count();
    assert!(
        successful >= 25,
        "positive builder and consumer coverage: {successful}"
    );
    for row in oracle.chunks_exact(16 + RESULT_BYTES) {
        if row[..4] != [0; 4] {
            assert!(row[16..].iter().all(|b| *b == 0xa5));
        }
    }
}
#[test]
fn binary_spelling_native_probe_assembles() {
    let root = workspace_root();
    let out = create_temp_dir("binary-spelling-probe");
    let source_path =
        root.join("native/motorola68000/amigaos/test-harnesses/prvm/binary_spelling_probe.asm");
    let input = out.join("binary_spelling_probe.asm");
    let source = fs::read_to_string(source_path).unwrap().replace(
        "build/binary_spelling_probe",
        &out.join("probe").display().to_string(),
    );
    fs::write(&input, source).unwrap();
    let mut args = vec![
        "opforge".to_string(),
        "-i".into(),
        input.display().to_string(),
    ];
    for dir in ["prvm", "tkvm", "tkpkg", "exprvm", "debug"] {
        args.extend([
            "-M".into(),
            root.join("native/motorola68000/amigaos")
                .join(dir)
                .display()
                .to_string(),
        ]);
    }
    args.extend([
        "-I".into(),
        root.join("native/motorola68000/amigaos/debug")
            .display()
            .to_string(),
    ]);
    run_with_cli_with_context(&Cli::parse_from(args)).expect("assemble direct atom service probe");
    fs::remove_dir_all(out).unwrap();
}
#[test]
#[ignore = "requires configured FS-UAE; live atom builder/descriptor/fragment proof"]
fn binary_spelling_native_fs_uae() {
    let (input, oracle, count) = batch();
    let result = crate::fs_uae_smoke::run_binary_spelling_harness_from_env(
        &workspace_root(),
        &input,
        &oracle,
    )
    .expect("fresh binary spelling service comparison");
    let crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real native proof required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    eprintln!(
        "BINARY_SPELLING cases={count} seconds={:?}",
        runs[0].start_to_done_host_seconds
    );
}
