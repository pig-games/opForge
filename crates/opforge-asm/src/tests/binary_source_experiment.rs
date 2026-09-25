//! Level B package checks and opt-in Level D native binary-source proof.
use super::*;
use crate::binary_source_experiment::prepare_package;
use crate::fs_uae_smoke::FsUaeSmokeOutcome;
use vm::runtime_model_core::RuntimeModelCore;

#[path = "binary_source_hunk.rs"]
mod hunk;

#[path = "binary_source_constants.rs"]
mod constants;

#[path = "binary_source_selection.rs"]
mod selection;

#[path = "binary_source_dependencies.rs"]
mod dependencies;

#[path = "binary_source_graph.rs"]
mod graph;

#[path = "binary_source_files.rs"]
mod files;

#[path = "binary_source_modules.rs"]
mod modules;

#[path = "binary_source_namespaces.rs"]
mod namespaces;

#[path = "binary_source_scopes.rs"]
mod scopes;

#[test]
fn binary_source_packages_prepare() {
    fn long(bytes: &[u8], offset: usize) -> usize {
        u32::from_be_bytes(bytes[offset..offset + 4].try_into().unwrap()) as usize
    }

    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000"] {
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let bytes = prepare_package(&core, &resolved).unwrap();
        assert_eq!(&bytes[..4], b"BSP3");
        assert_eq!(long(&bytes, 4), bytes.len());

        let runtime_bytes = long(&bytes, 72);
        assert!((76..=bytes.len()).contains(&runtime_bytes));
        assert_eq!(runtime_bytes % 2, 0);

        let rows = long(&bytes, 16);
        let row_count = long(&bytes, 20);
        let registers = long(&bytes, 24);
        let register_count = long(&bytes, 28);
        let programs = long(&bytes, 32);
        let program_count = long(&bytes, 36);
        for (offset, count, width) in [
            (rows, row_count, 32),
            (registers, register_count, 6),
            (programs, program_count, 12),
        ] {
            assert!(offset >= 76);
            assert!(offset + count * width <= runtime_bytes);
        }

        let mut runtime_references = vec![
            (rows, row_count * 32),
            (registers, register_count * 6),
            (programs, program_count * 12),
        ];
        for index in 0..row_count {
            let row = rows + index * 32;
            let input_count =
                u16::from_be_bytes(bytes[row + 10..row + 12].try_into().unwrap()) as usize;
            let inputs = long(&bytes, row + 12);
            let exclusions = long(&bytes, row + 24);
            if exclusions != 0 {
                assert!(exclusions >= programs + program_count * 12);
                assert!(exclusions + 2 <= runtime_bytes);
                let count =
                    u16::from_be_bytes(bytes[exclusions..exclusions + 2].try_into().unwrap())
                        as usize;
                assert!(count > 0);
                assert!(exclusions + 2 + count * 4 <= runtime_bytes);
                runtime_references.push((exclusions, 2 + count * 4));
                let name_count = u16::from_be_bytes(bytes[62..64].try_into().unwrap());
                for predicate in bytes[exclusions + 2..exclusions + 2 + count * 4].chunks_exact(4) {
                    assert!(u16::from_be_bytes(predicate[..2].try_into().unwrap()) < 2);
                    assert!(u16::from_be_bytes(predicate[2..].try_into().unwrap()) < name_count);
                }
            }
            let table = u16::from_be_bytes(bytes[row + 28..row + 30].try_into().unwrap());
            if bytes[row + 5] == 7 {
                assert!(usize::from(table) < program_count);
                assert_eq!(&bytes[programs + usize::from(table) * 12..][..2], &[0, 1]);
            } else {
                assert_eq!(table, u16::MAX);
            }
            if input_count == 0 {
                assert_eq!(inputs, 0);
            } else {
                assert!(inputs >= programs + program_count * 12);
                assert!(inputs + input_count * 12 <= runtime_bytes);
                runtime_references.push((inputs, input_count * 12));
            }
        }
        for index in 0..program_count {
            let program = programs + index * 12;
            let offset = long(&bytes, program + 4);
            let length = long(&bytes, program + 8);
            assert!(offset >= programs + program_count * 12);
            assert!(offset + length <= runtime_bytes);
            runtime_references.push((offset, length));
        }

        let dictionary = long(&bytes, 8);
        let tokenizer = long(&bytes, 40);
        let tokenizer_bytes = long(&bytes, 44);
        assert!(dictionary >= runtime_bytes);
        assert!(tokenizer >= dictionary);
        assert!(tokenizer + tokenizer_bytes <= bytes.len());

        let relocated_runtime = bytes[..runtime_bytes].to_vec();
        for (offset, length) in runtime_references {
            assert_eq!(
                &relocated_runtime[offset..offset + length],
                &bytes[offset..offset + length]
            );
        }
        assert_eq!(bytes, prepare_package(&core, &resolved).unwrap());
    }
}

#[test]
#[ignore = "requires explicit source/CPU and configured FS-UAE"]
fn binary_source_fs_uae() {
    let source = fs::read_to_string(std::env::var("OPFORGE_COMPARE_SOURCE").unwrap()).unwrap();
    let cpu = std::env::var("OPFORGE_COMPARE_CPU").unwrap();
    assert_binary_source(source, cpu);
}

#[test]
#[ignore = "requires configured FS-UAE; standalone compact Shell CLI"]
fn compact_cli_fs_uae() {
    let source = ".cpu m6502\nstart:\n lda #$12\n sta $40\n .byte 7\n.end\n";
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live Rust source oracle");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let oracle = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        Some(&oracle),
    )
    .expect("compact CLI must complete with Rust-identical output");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    let image = runs[0]
        .captured_artifacts
        .get(&PathBuf::from("Work/build/opforge_compact"))
        .expect("fresh compact CLI image");
    let allocation = hunk::allocation(image).expect("valid compact CLI Hunk");
    assert!(allocation.total() < 2 * 1024 * 1024);
}

#[test]
#[ignore = "requires configured FS-UAE; binary segment expansion in compact CLI"]
fn compact_cli_segment_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000"] {
        let instruction = if cpu == "m6502" {
            "lda #.v"
        } else {
            "moveq #.v,d0"
        };
        let source = format!(
            ".cpu {cpu}\nINLINE .segment v\n {instruction}\n .byte .v\n .byte .v+1\n.endsegment\n.segment ALT(v)\n {instruction}\n .byte .v\n .byte .v+1\n.endsegment\n .INLINE 3\n .ALT(5+1)\n .INLINE(9)\n.end\n"
        );
        let oracle_dir = create_temp_dir(&format!("compact-binary-segment-oracle-{cpu}"));
        let oracle_input = oracle_dir.join("input.asm");
        let oracle_output = oracle_dir.join("oracle.bin");
        fs::write(&oracle_input, &source).expect("write Rust oracle source");
        let cli = Cli::parse_from([
            "opForge".to_string(),
            oracle_input.to_string_lossy().into_owned(),
            "--bin".to_string(),
            oracle_output.to_string_lossy().into_owned(),
            "--cpu".to_string(),
            cpu.to_string(),
        ]);
        run_with_cli_with_context(&cli).expect("assemble live Rust CLI oracle");
        let oracle = fs::read(&oracle_output).expect("read Rust CLI oracle");
        fs::remove_dir_all(&oracle_dir).expect("remove Rust oracle scratch");
        let expected = if cpu == "m6502" {
            &[0xa9, 3, 3, 4, 0xa9, 6, 6, 7, 0xa9, 9, 9, 10][..]
        } else {
            &[0x70, 3, 3, 4, 0x70, 6, 6, 7, 0x70, 9, 9, 10][..]
        };
        assert_eq!(oracle, expected);
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let package = prepare_package(&core, &resolved).unwrap();
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            Some(&oracle),
        )
        .expect("compact CLI must expand binary segment templates into Rust-identical output");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
        let image = runs[0]
            .captured_artifacts
            .get(&PathBuf::from("Work/build/opforge_compact"))
            .expect("fresh compact CLI image");
        let allocation = hunk::allocation(image).expect("valid compact CLI Hunk");
        assert!(allocation.total() < 2 * 1024 * 1024);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; binary segment invocation labels in compact CLI"]
fn compact_cli_segment_label_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000"] {
        let source = format!(
            ".cpu {cpu}\n.org $2000\nINLINE .segment v\n .byte .v\n .byte .v+1\n.endsegment\nfirst .INLINE 7\nsecond: .INLINE(9)\n.word first,second\n.end\n"
        );
        let oracle_dir = create_temp_dir(&format!("compact-binary-segment-label-{cpu}"));
        let oracle_input = oracle_dir.join("input.asm");
        let oracle_output = oracle_dir.join("oracle.bin");
        fs::write(&oracle_input, &source).expect("write Rust oracle source");
        let cli = Cli::parse_from([
            "opForge".to_string(),
            oracle_input.to_string_lossy().into_owned(),
            "--bin".to_string(),
            oracle_output.to_string_lossy().into_owned(),
            "--cpu".to_string(),
            cpu.to_string(),
        ]);
        run_with_cli_with_context(&cli).expect("assemble live Rust CLI oracle");
        let oracle = fs::read(&oracle_output).expect("read Rust CLI oracle");
        fs::remove_dir_all(&oracle_dir).expect("remove Rust oracle scratch");
        let expected = if cpu == "m6502" {
            &[7, 8, 9, 10, 0, 0x20, 2, 0x20][..]
        } else {
            &[7, 8, 9, 10, 0x20, 0, 0x20, 2][..]
        };
        assert_eq!(oracle, expected);
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let package = prepare_package(&core, &resolved).unwrap();
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            Some(&oracle),
        )
        .expect("compact CLI must attach each call label to its binary expansion");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; binary macro scope parity in compact CLI"]
fn compact_cli_macro_scope_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000"] {
        let source = format!(
            ".cpu {cpu}\n.org $2000\nEMIT .macro v\nlocal:\n .byte .v\n .word local\n.endmacro\n .EMIT 3\nfirst .EMIT 5\n .EMIT(7)\n .word first\n.end\n"
        );
        let oracle_dir = create_temp_dir(&format!("compact-binary-macro-scope-{cpu}"));
        let oracle_input = oracle_dir.join("input.asm");
        let oracle_output = oracle_dir.join("oracle.bin");
        fs::write(&oracle_input, &source).expect("write Rust oracle source");
        let cli = Cli::parse_from([
            "opForge".to_string(),
            oracle_input.to_string_lossy().into_owned(),
            "--bin".to_string(),
            oracle_output.to_string_lossy().into_owned(),
            "--cpu".to_string(),
            cpu.to_string(),
        ]);
        run_with_cli_with_context(&cli).expect("assemble live Rust CLI oracle");
        let oracle = fs::read(&oracle_output).expect("read Rust CLI oracle");
        fs::remove_dir_all(&oracle_dir).expect("remove Rust oracle scratch");
        let expected = if cpu == "m6502" {
            &[3, 0, 0x20, 5, 3, 0x20, 7, 6, 0x20, 3, 0x20][..]
        } else {
            &[3, 0x20, 0, 5, 0x20, 3, 7, 0x20, 6, 0x20, 3][..]
        };
        assert_eq!(oracle, expected);
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let package = prepare_package(&core, &resolved).unwrap();
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            Some(&oracle),
        )
        .expect("compact CLI must instantiate local macro symbols per binary call");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
        let image = runs[0]
            .captured_artifacts
            .get(&PathBuf::from("Work/build/opforge_compact"))
            .expect("fresh compact CLI image");
        let allocation = hunk::allocation(image).expect("valid compact CLI Hunk");
        assert!(allocation.total() < 2 * 1024 * 1024);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; simple binary macro expansion in compact CLI"]
fn compact_cli_macro_simple_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let source = b".cpu m6502\nEMIT .macro v\n .byte .v\n.endmacro\n .EMIT 3\n .EMIT 5\n.end\n";
    let oracle_dir = create_temp_dir("compact-binary-macro-simple");
    let oracle_input = oracle_dir.join("input.asm");
    let oracle_output = oracle_dir.join("oracle.bin");
    fs::write(&oracle_input, source).expect("write Rust oracle source");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        oracle_input.to_string_lossy().into_owned(),
        "--bin".to_string(),
        oracle_output.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "m6502".to_string(),
    ]);
    run_with_cli_with_context(&cli).expect("assemble live Rust CLI oracle");
    let oracle = fs::read(&oracle_output).expect("read Rust CLI oracle");
    fs::remove_dir_all(&oracle_dir).expect("remove Rust oracle scratch");
    assert_eq!(oracle, [3, 5]);
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source,
        Some(&oracle),
    )
    .expect("compact CLI must expand two binary macro calls");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
}

#[test]
#[ignore = "requires configured FS-UAE; zero-argument and directive-first binary macros"]
fn compact_cli_macro_definition_forms_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000"] {
        let source = format!(
            ".cpu {cpu}\n.org $2000\nEMPTY .macro\n .byte $11\n.endm\n.macro FILL(value)\n .byte .value\n.endmacro\n .EMPTY\n .FILL(2)\n .FILL 3\n.end\n"
        );
        let oracle_dir = create_temp_dir(&format!("compact-binary-macro-forms-{cpu}"));
        let oracle_input = oracle_dir.join("input.asm");
        let oracle_output = oracle_dir.join("oracle.bin");
        fs::write(&oracle_input, &source).expect("write Rust oracle source");
        let cli = Cli::parse_from([
            "opForge".to_string(),
            oracle_input.to_string_lossy().into_owned(),
            "--bin".to_string(),
            oracle_output.to_string_lossy().into_owned(),
            "--cpu".to_string(),
            cpu.to_string(),
        ]);
        run_with_cli_with_context(&cli).expect("assemble live Rust CLI oracle");
        let oracle = fs::read(&oracle_output).expect("read Rust CLI oracle");
        fs::remove_dir_all(&oracle_dir).expect("remove Rust oracle scratch");
        assert_eq!(oracle, [0x11, 2, 3]);
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let package = prepare_package(&core, &resolved).unwrap();
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            Some(&oracle),
        )
        .expect("compact CLI must expand both binary macro definition forms");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; multi-argument binary macro parity"]
fn compact_cli_macro_multiple_arguments_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000"] {
        let source = format!(
            ".cpu {cpu}\n.org $2000\nPAIR .macro left, right\n .byte .left, .right\n.endmacro\n.macro MIX(x, y)\n .byte .1, .y, .2, .x\n.endmacro\n .PAIR 1+(2), 3+(4)\n .MIX(5, 6)\n.end\n"
        );
        let oracle_dir = create_temp_dir(&format!("compact-binary-macro-multiple-{cpu}"));
        let oracle_input = oracle_dir.join("input.asm");
        let oracle_output = oracle_dir.join("oracle.bin");
        fs::write(&oracle_input, &source).expect("write Rust oracle source");
        let cli = Cli::parse_from([
            "opForge".to_string(),
            oracle_input.to_string_lossy().into_owned(),
            "--bin".to_string(),
            oracle_output.to_string_lossy().into_owned(),
            "--cpu".to_string(),
            cpu.to_string(),
        ]);
        run_with_cli_with_context(&cli).expect("assemble live Rust CLI oracle");
        let oracle = fs::read(&oracle_output).expect("read Rust CLI oracle");
        fs::remove_dir_all(&oracle_dir).expect("remove Rust oracle scratch");
        assert_eq!(oracle, [3, 7, 5, 6, 6, 5]);
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let package = prepare_package(&core, &resolved).unwrap();
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            Some(&oracle),
        )
        .expect("compact CLI must substitute packed named and positional arguments");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; packed macro defaults and omitted/extra arguments"]
fn compact_cli_macro_defaults_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000"] {
        let source = format!(
            ".cpu {cpu}\n.org $2000\nPAIR .macro a, b=2\n .byte .a, .b\n.endmacro\n.macro DEFAULT(value=9)\n .byte .value\n.endmacro\nEXTRA .macro first\n .byte .1, .2, .3\n.endmacro\nONLY .macro a, unused\n .byte .a\n.endmacro\n .PAIR 1\n .PAIR(3, 4)\n .DEFAULT\n .EXTRA 5, 6, 7\n .ONLY 8\n.end\n"
        );
        let oracle_dir = create_temp_dir(&format!("compact-binary-macro-defaults-{cpu}"));
        let oracle_input = oracle_dir.join("input.asm");
        let oracle_output = oracle_dir.join("oracle.bin");
        fs::write(&oracle_input, &source).expect("write Rust oracle source");
        let cli = Cli::parse_from([
            "opForge".to_string(),
            oracle_input.to_string_lossy().into_owned(),
            "--bin".to_string(),
            oracle_output.to_string_lossy().into_owned(),
            "--cpu".to_string(),
            cpu.to_string(),
        ]);
        run_with_cli_with_context(&cli).expect("assemble live Rust CLI oracle");
        let oracle = fs::read(&oracle_output).expect("read Rust CLI oracle");
        fs::remove_dir_all(&oracle_dir).expect("remove Rust oracle scratch");
        assert_eq!(oracle, [1, 2, 3, 4, 9, 5, 6, 7, 8]);
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let package = prepare_package(&core, &resolved).unwrap();
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            Some(&oracle),
        )
        .expect("compact CLI must use binary defaults and retain positional extras");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; default identifiers bind at the macro call"]
fn compact_cli_macro_default_caller_scope_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000"] {
        let source = format!(
            ".cpu {cpu}\n.org $2000\nPICK .macro value=amount\n .byte .value\n.endmacro\ncaller .block\namount = 3\n .PICK\n.bend\n.end\n"
        );
        let oracle_dir = create_temp_dir(&format!("compact-binary-macro-default-scope-{cpu}"));
        let oracle_input = oracle_dir.join("input.asm");
        let oracle_output = oracle_dir.join("oracle.bin");
        fs::write(&oracle_input, &source).expect("write Rust oracle source");
        let cli = Cli::parse_from([
            "opForge".to_string(),
            oracle_input.to_string_lossy().into_owned(),
            "--bin".to_string(),
            oracle_output.to_string_lossy().into_owned(),
            "--cpu".to_string(),
            cpu.to_string(),
        ]);
        run_with_cli_with_context(&cli).expect("assemble live Rust CLI oracle");
        let oracle = fs::read(&oracle_output).expect("read Rust CLI oracle");
        fs::remove_dir_all(&oracle_dir).expect("remove Rust oracle scratch");
        assert_eq!(oracle, [3]);
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let package = prepare_package(&core, &resolved).unwrap();
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            Some(&oracle),
        )
        .expect("compact CLI must rebind packed default identifiers in the caller scope");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; nested packed macro and segment calls"]
fn compact_cli_macro_nested_calls_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000"] {
        let source = format!(
            ".cpu {cpu}\n.org $2000\nINNER .macro v\n .byte .v\n.endmacro\nTAIL .segment v\n .byte .v+1\n.endsegment\nOUTER .macro v\n .INNER .v\n .TAIL .v\n.endmacro\n .OUTER 3\n .OUTER 5\n.end\n"
        );
        let oracle_dir = create_temp_dir(&format!("compact-binary-macro-nested-{cpu}"));
        let oracle_input = oracle_dir.join("input.asm");
        let oracle_output = oracle_dir.join("oracle.bin");
        fs::write(&oracle_input, &source).expect("write Rust oracle source");
        let cli = Cli::parse_from([
            "opForge".to_string(),
            oracle_input.to_string_lossy().into_owned(),
            "--bin".to_string(),
            oracle_output.to_string_lossy().into_owned(),
            "--cpu".to_string(),
            cpu.to_string(),
        ]);
        run_with_cli_with_context(&cli).expect("assemble live Rust CLI oracle");
        let oracle = fs::read(&oracle_output).expect("read Rust CLI oracle");
        fs::remove_dir_all(&oracle_dir).expect("remove Rust oracle scratch");
        assert_eq!(oracle, [3, 4, 5, 6]);
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let package = prepare_package(&core, &resolved).unwrap();
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            Some(&oracle),
        )
        .expect("compact CLI must expand nested macro and segment calls from packed records");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; unclosed binary segment must reject"]
fn compact_cli_unclosed_segment_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let source = b".cpu m6502\nINLINE .segment v\n .byte .v\n.end\n";
    let result =
        crate::fs_uae_smoke::run_compact_cli_from_env(&workspace_root(), &package, source, None)
            .expect("compact CLI must reject an unclosed binary segment");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
    assert!(runs[0].stdout.contains("unsupported or invalid input"));
}

#[test]
#[ignore = "requires configured FS-UAE; unsupported segment arity must reject"]
fn compact_cli_segment_arity_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let source = b".cpu m6502\n.segment INLINE(v)\n .byte .v\n.endsegment\n .INLINE(1,2)\n.end\n";
    let result =
        crate::fs_uae_smoke::run_compact_cli_from_env(&workspace_root(), &package, source, None)
            .expect("compact CLI must reject an unsupported second segment argument");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
    assert!(runs[0].stdout.contains("unsupported or invalid input"));
}

#[test]
#[ignore = "requires configured FS-UAE; compact CLI rejects unsupported input"]
fn compact_cli_rejection_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        b" .unsupported 1\n",
        None,
    )
    .expect("compact CLI must reject unsupported input");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
}

fn assert_binary_source(source: String, cpu: String) -> serde_json::Value {
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live Rust source oracle");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let oracle = entries.into_iter().map(|(_, byte)| byte).collect();
    assert_binary_files(&[("input.asm", &source)], &cpu, oracle)
}

fn assert_binary_files(files: &[(&str, &str)], cpu: &str, oracle: Vec<u8>) -> serde_json::Value {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
    let preparation_started = std::time::Instant::now();
    let input = prepare_package(&core, &resolved).unwrap();
    let package_preparation_seconds = preparation_started.elapsed().as_secs_f64();
    let package_bytes = input.len();
    let sources = files
        .iter()
        .map(|(name, text)| (*name, text.as_bytes()))
        .collect::<Vec<_>>();
    let native_root = std::env::var_os("OPFORGE_COMPARE_NATIVE_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(workspace_root);
    let result = crate::fs_uae_smoke::run_binary_source_harness_from_env(
        &native_root,
        &input,
        &sources,
        &oracle,
    )
    .expect("completed native binary-source comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("explicit comparison requires real native execution");
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.success && run.protocol_completed);
    assert_eq!(run.exit_code, Some(0));
    let image = run
        .captured_artifacts
        .get(&PathBuf::from("Work/build/binary_source_harness"))
        .expect("fresh native image capture");
    let allocation = hunk::allocation(image).expect("valid captured native Hunk allocation table");
    let memory = if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let record = run
            .captured_artifacts
            .get(&PathBuf::from("Work/memory.bin"))
            .expect("fresh memory telemetry capture");
        assert_eq!(record.len(), 1756);
        let words: Vec<u32> = record
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect();
        assert_eq!(words[0], 0x4d454d35);
        assert_eq!(words[1], 0, "all tracked allocations released");
        assert_eq!(words[3], words[4], "allocated and freed capacities balance");
        assert_eq!(words[11], 0, "cleanup has no live allocation");
        assert!(words[2] >= words[5] && words[2] >= words[10]);
        assert!(
            words[6] > 0,
            "preparation allocations were freed before assembly"
        );
        let stamp = |offset: usize| -> u64 {
            u64::from(words[offset]) * 24 * 60 * 60 * 50
                + u64::from(words[offset + 1]) * 60 * 50
                + u64::from(words[offset + 2])
        };
        let preparation_ticks = stamp(22)
            .checked_sub(stamp(19))
            .expect("ordered preparation clock");
        let assembly_ticks = stamp(25)
            .checked_sub(stamp(22))
            .expect("ordered assembly clock");
        assert!(u64::from(words[18]) >= u64::from(words[16]) * 2);
        assert!(words[28] > 0, "E-clock frequency is available");
        assert_eq!(words[29], 0, "preparation profiling completed cleanly");
        let names = [
            "other",
            "package_setup",
            "tokenization",
            "binding_and_raw_records",
            "expression_preparation",
            "runtime_finalization",
        ];
        let mut stages = serde_json::Map::new();
        let mut total_ticks = 0_u64;
        for (index, name) in names.iter().enumerate() {
            let ticks = (u64::from(words[30 + 2 * index]) << 32) | u64::from(words[31 + 2 * index]);
            total_ticks = total_ticks.checked_add(ticks).expect("bounded stage total");
            stages.insert(
                (*name).to_owned(),
                serde_json::json!({
                    "ticks": ticks, "seconds": ticks as f64 / f64::from(words[28]),
                    "calls": words[42 + index],
                }),
            );
        }
        assert_eq!(words[43], 1, "one package setup");
        assert_eq!(words[47], 1, "one finalization");
        assert_eq!(words[44], words[45], "each tokenized line binds once");
        assert_eq!(words[45], words[46], "each bound line prepares once");
        assert_eq!(
            words[44] as usize,
            files
                .iter()
                .map(|(_, text)| text.lines().count())
                .sum::<usize>()
        );
        let stage_seconds = total_ticks as f64 / f64::from(words[28]);
        assert!(
            (stage_seconds - preparation_ticks as f64 / 50.0).abs() <= 0.04,
            "E-clock stages reconcile with coarse preparation: {stage_seconds}"
        );

        let opcodes = &words[48..67];
        let pairs = &words[67..428];
        let work = &words[428..435];
        let opcode_total: u64 = opcodes.iter().map(|n| u64::from(*n)).sum();
        let pair_total: u64 = pairs.iter().map(|n| u64::from(*n)).sum();
        assert_eq!(opcodes[0], words[44], "each successful line ends once");
        assert_eq!(pair_total + u64::from(words[44]), opcode_total);
        assert_eq!(
            work[0] as usize,
            files
                .iter()
                .map(|(_, text)| text.bytes().filter(|b| *b != b'\n').count())
                .sum::<usize>()
        );
        for (taken, opcode) in [(4, 8), (5, 9), (6, 10)] {
            assert!(work[taken] <= opcodes[opcode]);
        }
        let scope = |index: usize| {
            let offset = 435 + index * 2;
            let ticks = (u64::from(words[offset]) << 32) | u64::from(words[offset + 1]);
            ticks as f64 / f64::from(words[28])
        };
        let helpers_seconds = scope(0);
        let commit_seconds = scope(1);
        assert!(commit_seconds <= helpers_seconds);
        assert!(helpers_seconds <= stages["tokenization"]["seconds"].as_f64().unwrap());
        serde_json::json!({
            "tokenizer": {
                "opcodes": opcodes, "opcode_pairs": pairs, "opcode_total": opcode_total,
                "line_bytes": work[0], "tokens": work[1], "lexeme_bytes": work[2],
                "source_reads": work[3], "taken_eol": work[4], "taken_byte": work[5],
                "taken_class": work[6], "helpers_seconds": helpers_seconds,
                "commit_seconds": commit_seconds,
            },
            "preparation_stages": stages, "stage_total_seconds": stage_seconds,
            "eclock_frequency": words[28], "profiling_errors": words[29],
            "expressions_compiled": words[16], "expressions_evaluated": words[17],
            "compiled_program_bytes": words[18],
            "instrumented_preparation_seconds": preparation_ticks as f64 / 50.0,
            "instrumented_assembly_seconds": assembly_ticks as f64 / 50.0,
            "peak_allocated_bytes": words[2], "total_allocated_bytes": words[3],
            "retained_after_preparation_bytes": words[5], "freed_before_assembly_bytes": words[6],
            "free_bytes_at_program_entry": words[7], "largest_free_block_at_program_entry": words[8],
            "exec_version": words[9], "allocated_after_assembly_bytes": words[10],
            "live_after_cleanup_bytes": words[11], "dos_version": words[12],
            "runtime_prefix_bytes": words[13], "packed_source_bytes": words[14], "source_bytes": words[15],
        })
    } else {
        serde_json::Value::Null
    };
    eprintln!(
        "BINARY_SOURCE_COMPARISON {}",
        serde_json::json!({
            "cpu": cpu, "source_bytes": sources.iter().map(|(_, bytes)| bytes.len()).sum::<usize>(),
            "source_files": files.iter().map(|(name, _)| name).collect::<Vec<_>>(), "runtime_package_bytes": package_bytes,
            "host_package_preparation_seconds": package_preparation_seconds,
            "native_image_bytes": image.len(),
            "native_linked_reserved_bytes": allocation.total(),
            "native_linked_code_reserved_bytes": allocation.code,
            "native_linked_data_reserved_bytes": allocation.data,
            "native_linked_bss_reserved_bytes": allocation.bss,
            "native_linked_segments": allocation.segments,
            "guest_start_to_done_host_seconds": run.start_to_done_host_seconds,
            "native_image_digest": run.native_image_digest,
            "exact_output": oracle, "guest_exit": run.exit_code,
            "preparation_storage_released_before_passes": true,
            "memory": memory,
            "guest_memory_before_launch": run.captured_artifacts.get(&PathBuf::from("Work/guest-memory.txt")).map(|bytes| String::from_utf8_lossy(bytes).into_owned()),
        })
    );
    memory
}

#[test]
#[ignore = "requires explicit source/CPU and configured FS-UAE"]
fn binary_source_rejection_fs_uae() {
    let source = fs::read_to_string(std::env::var("OPFORGE_COMPARE_SOURCE").unwrap()).unwrap();
    let cpu = std::env::var("OPFORGE_COMPARE_CPU").unwrap();
    let oracle =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true);
    assert!(
        match &oracle {
            Err(_) => true,
            Ok((_, diagnostics)) => !diagnostics.is_empty(),
        },
        "negative case must be rejected by the live Rust assembler",
    );
    assert_native_rejection(&source, &cpu);
}

fn assert_native_rejection(source: &str, cpu: &str) {
    assert_native_files_rejection(&[("input.asm", source)], cpu, None);
}

fn assert_native_files_rejection(files: &[(&str, &str)], cpu: &str, diagnostic: Option<&str>) {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
    let input = prepare_package(&core, &resolved).unwrap();
    let sources = files
        .iter()
        .map(|(name, text)| (*name, text.as_bytes()))
        .collect::<Vec<_>>();
    let result = crate::fs_uae_smoke::run_binary_source_rejection_from_env(
        &workspace_root(),
        &input,
        &sources,
        diagnostic,
    )
    .expect("fresh completed native rejection with diagnostic");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("explicit rejection contract requires native execution");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let record = runs[0]
            .captured_artifacts
            .get(&PathBuf::from("Work/memory.bin"))
            .expect("fresh negative-path memory telemetry");
        assert_eq!(record.len(), 1756);
        let words: Vec<u32> = record
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect();
        assert_eq!(words[0], 0x4d454d35);
        assert!(words[28] > 0, "E-clock initialized on rejection path");
        assert_eq!(words[29] & !16, 0, "only incomplete preparation is allowed");
        assert_eq!(words[1], 0, "failure releases all owned blocks");
        assert_eq!(words[3], words[4]);
        assert_eq!(words[11], 0);
    }
}

fn assert_expression_limit(expression: &str, rust_accepts: bool) {
    let source = format!(".cpu m6502\n.org $1000\n.byte {expression}\n.end\n");
    let rust = assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true);
    let accepted = matches!(rust, Ok((_, ref diagnostics)) if diagnostics.is_empty());
    assert_eq!(accepted, rust_accepts, "live Rust domain for {expression}");
    assert_native_rejection(&source, "m6502");
}

// Each test is a separate bounded real-native invocation, including allocation
// cleanup when OPFORGE_COMPARE_MEMORY=1. Rust-accepted cases identify explicit
// experimental limits; they are not claims of diagnostic/language parity.
macro_rules! expression_limit_case {
    ($name:ident, $expression:expr, $accepted:expr) => {
        #[test]
        #[ignore = "requires configured FS-UAE; one bounded rejection case"]
        fn $name() {
            assert_expression_limit(&$expression, $accepted);
        }
    };
}
expression_limit_case!(
    binary_expression_limit_overflow_fs_uae,
    "($7fffffff+1)-$7fffffff",
    true
);
expression_limit_case!(
    binary_expression_limit_program_fs_uae,
    ["1"; 24].join("+"),
    true
);
expression_limit_case!(
    binary_expression_limit_stack_fs_uae,
    format!("{}1{}", "1+(".repeat(8), ")".repeat(8)),
    true
);
expression_limit_case!(
    binary_expression_limit_depth_fs_uae,
    format!("{}1{}", "(".repeat(17), ")".repeat(17)),
    true
);
expression_limit_case!(binary_expression_limit_incomplete_fs_uae, "1+", false);
// Shared Rust data emission accepts this wrapped result; the native experiment
// deliberately rejects its high-bit literal under the existing signed32 limit.
expression_limit_case!(binary_expression_limit_literal_fs_uae, "-$ffffffff", true);
expression_limit_case!(
    binary_expression_limit_multiply_overflow_fs_uae,
    "(50000*50000)*0",
    true
);
expression_limit_case!(
    binary_expression_limit_negate_overflow_fs_uae,
    "-(-$7fffffff-1)+(-$7fffffff-1)",
    true
);

#[test]
#[ignore = "requires configured FS-UAE; constant and dynamic subtree folding"]
fn binary_expression_folding_fs_uae() {
    let memory = assert_binary_source(
        ".cpu m6502\n.org $1000\n\
         .byte 5*3-2,-(-5),(-7)*(-3)\n\
         .word fold_target+(3*2),(3*2)+fold_target,($-$)+(3*2),fold_target-fold_target+(3*2)\n\
         .long ($7fffffff-1)-$7fffffff,-$7fffffff-1\n\
         fold_target:\n nop\n.end\n"
            .into(),
        "m6502".into(),
    );
    if !memory.is_null() {
        // Ten expressions: literals use explicit signed widths; symbol/PC
        // subtrees retain their compact operators. This proves folding actually ran,
        // in addition to the complete output comparison with live Rust.
        assert_eq!(memory["expressions_compiled"], 10);
        assert_eq!(memory["compiled_program_bytes"], 54);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; complete positive expression boundary case"]
fn binary_expression_boundary_fs_uae() {
    let source = format!(
        ".cpu m6502\n.org $1000\n.byte {}1{}\n.byte {}1{}\n\
         .byte +(+1),1+2*3,(1+2)*3\n.long $7fffffff,-$7fffffff-1\n.end\n",
        "1+(".repeat(7),
        ")".repeat(7),
        "(".repeat(16),
        ")".repeat(16),
    );
    assert_binary_source(source, "m6502".into());
}

#[test]
#[ignore = "requires configured FS-UAE; compact literal width transitions"]
fn binary_expression_widths_fs_uae() {
    let memory = assert_binary_source(
        ".cpu m6502\n.org $1000\n\
         .long 127,128,-128,-129\n\
         .long 32767,32768,-32768,-32769\n\
         .long -$7fffffff-1,$7fffffff\n.end\n"
            .into(),
        "m6502".into(),
    );
    if !memory.is_null() {
        assert_eq!(memory["expressions_compiled"], 11);
        assert_eq!(memory["compiled_program_bytes"], 50);
    }
}

fn binding_capacity_source(count: usize) -> String {
    let mut source = String::from(".cpu m6502\n.org $1000\n.word bound_511-bound_000\n");
    for index in 0..count {
        source.push_str(&format!("bound_{index:03}:\n.byte {}\n", index & 255));
    }
    source.push_str(".word bound_511-bound_000\n.end\n");
    source
}

#[test]
fn binary_binding_capacity_live_oracle() {
    for count in [512, 513] {
        let source = binding_capacity_source(count);
        let (entries, diagnostics) =
            assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
                .unwrap();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let mut expected = vec![255, 1];
        expected.extend((0..count).map(|index| (index & 255) as u8));
        expected.extend([255, 1]);
        assert_eq!(
            entries
                .into_iter()
                .map(|(_, byte)| byte)
                .collect::<Vec<_>>(),
            expected
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; colliding symbols at the existing capacity"]
fn binary_binding_capacity_fs_uae() {
    // More than 256 distinct names necessarily collide in a 256-bucket index.
    // Both first/last names are interned by a forward reference before definitions.
    assert_binary_source(binding_capacity_source(512), "m6502".into());
}

#[test]
#[ignore = "requires configured FS-UAE; unchanged 512-symbol experimental limit"]
fn binary_binding_capacity_overflow_fs_uae() {
    // Rust accepts this complete source; the experimental native boundary is 512.
    assert_native_rejection(&binding_capacity_source(513), "m6502");
}

fn binding_alias_source() -> String {
    // These distinct names share the index bucket used by `moveq`; exact spelling
    // comparison must still separate them and package lookup must precede symbols.
    let names = [
        "item_136",
        "item_217",
        "item_370",
        "item_451",
        "item_532",
        "item_613",
        "item_1988",
        "item_2798",
    ];
    let mut source = String::from(".cpu m68000\n.org $1000\n");
    for (index, name) in names.iter().enumerate() {
        source.push_str(&format!(
            "  MoVeQ #{index},D0\n  BcC.S {name}\n.word 0\n{name}:\n"
        ));
    }
    source.push_str("  BhS.S finish\n.word 0\nfinish:\n");
    for (index, name) in names.iter().enumerate() {
        source.push_str(&format!(".word {name}-item_136-{}\n", index * 6));
    }
    source.push_str(".end\n");
    source
}

#[test]
fn binary_binding_alias_live_oracle() {
    let source = binding_alias_source();
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let mut expected = Vec::new();
    for index in 0..8 {
        expected.extend([0x70, index, 0x64, 2, 0, 0]);
    }
    expected.extend([0x64, 2, 0, 0]);
    expected.extend([0; 16]);
    assert_eq!(
        entries
            .into_iter()
            .map(|(_, byte)| byte)
            .collect::<Vec<_>>(),
        expected
    );
}

#[test]
#[ignore = "requires configured FS-UAE; collisions, mnemonic case, aliases and references"]
fn binary_binding_alias_fs_uae() {
    assert_binary_source(binding_alias_source(), "m68000".into());
}
