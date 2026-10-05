//! Level B package checks and opt-in Level D native binary-source proof.

#[path = "binary_source_text_recipe_semantics.rs"]
mod text_recipe_semantics;

use super::*;
use crate::binary_source_experiment::prepare_package;
use crate::fs_uae_smoke::FsUaeSmokeOutcome;
use vm::runtime_model_core::RuntimeModelCore;

#[path = "binary_source_hunk.rs"]
mod hunk;

#[path = "binary_source_hunk_data_relocations.rs"]
mod hunk_data_relocations;
#[path = "binary_source_hunk_offsets.rs"]
mod hunk_offsets;
#[path = "binary_source_hunk_sections.rs"]
mod hunk_sections;

#[path = "binary_source_emit.rs"]
mod emit;

#[path = "binary_source_constants.rs"]
mod constants;

#[path = "binary_source_conditionals.rs"]
mod conditionals;

#[path = "binary_source_loops.rs"]
mod loops;

#[path = "binary_source_template_storage.rs"]
mod template_storage;

#[path = "binary_source_struct_layout.rs"]
mod struct_layout;

#[path = "binary_source_macro_calls.rs"]
mod macro_calls;
#[path = "binary_source_macro_name_collisions.rs"]
mod macro_name_collisions;

#[path = "binary_source_macro_profile.rs"]
mod macro_profile;

#[path = "binary_source_capture.rs"]
mod capture;
#[path = "binary_source_cpu_names.rs"]
mod cpu_names;
#[path = "binary_source_full_width.rs"]
mod full_width;
#[path = "binary_source_identity_storage.rs"]
mod identity_storage;
#[path = "binary_source_input_lines.rs"]
mod input_lines;
#[path = "binary_source_numeric_normalization.rs"]
mod numeric_normalization;
#[path = "binary_source_scope_growth.rs"]
mod scope_growth;

#[path = "binary_source_arithmetic.rs"]
mod arithmetic;

#[path = "binary_source_indexed.rs"]
mod indexed;

#[path = "binary_source_branches.rs"]
mod branches;

#[path = "binary_source_progress.rs"]
mod progress;

#[path = "binary_source_selection.rs"]
mod selection;

#[path = "binary_source_memory_move.rs"]
mod memory_move;

#[path = "binary_source_movem_restore.rs"]
mod movem_restore;

#[path = "binary_source_absolute_memory.rs"]
mod absolute_memory;

#[path = "binary_source_callback.rs"]
mod callback;

#[path = "binary_source_address_addends.rs"]
mod address_addends;
#[path = "binary_source_address_alu.rs"]
mod address_alu;
#[path = "binary_source_callback_scope.rs"]
mod callback_scope;
#[path = "binary_source_immediate_memory.rs"]
mod immediate_memory;
#[path = "binary_source_keyword_labels.rs"]
mod keyword_labels;
#[path = "binary_source_members.rs"]
mod members;
#[path = "binary_source_mnemonic_labels.rs"]
mod mnemonic_labels;
#[path = "binary_source_selfhost_data.rs"]
mod selfhost_data;

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
fn binary_source_runtime_target_identity_is_relocatable() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for (cpu, little_endian) in [("m68020", 0u16), ("m6502", 1u16)] {
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let bytes = prepare_package(&core, &resolved).unwrap();
        let long =
            |offset| u32::from_be_bytes(bytes[offset..offset + 4].try_into().unwrap()) as usize;
        let word = |offset| u16::from_be_bytes(bytes[offset..offset + 2].try_into().unwrap());
        let target_offset = long(124);
        let target_bytes = usize::from(word(128));
        let runtime_bytes = long(72);
        let expected = format!("{cpu}--{}", resolved.dialect_id);
        assert_eq!(&bytes[..4], b"BS27");
        let rows_offset = long(16);
        assert!(rows_offset >= 200);
        assert_eq!((rows_offset - 200) % 4, 0);
        assert_eq!(word(130) & 2 != 0, rows_offset > 200);
        assert_eq!(word(188), package::PARSER_VM_MACRO_VERSION);
        assert_eq!(word(190), 0);
        let declaration_offset = long(180);
        assert_eq!(long(184), 13);
        let heads = [2, 5, 8].map(|slot| {
            u16::from_be_bytes(
                bytes[declaration_offset + slot..declaration_offset + slot + 2]
                    .try_into()
                    .unwrap(),
            )
        });
        assert_eq!(
            &bytes[declaration_offset..declaration_offset + 13],
            package::packed_declaration_program(heads)
        );
        assert_eq!(word(64), little_endian);
        assert_eq!(word(130) & 1, u16::from(cpu == "m6502"));
        assert_eq!(word(130) & !3, 0);
        assert!(target_offset >= 200);
        assert_eq!(target_bytes, expected.len());
        assert_eq!(
            &bytes[target_offset..target_offset + target_bytes],
            expected.as_bytes()
        );
        assert_eq!(long(144), (target_offset + target_bytes + 1) & !1);
        assert_eq!(long(148), 16);
        assert_eq!(long(160), long(144) + long(148));
        assert_eq!(long(168), long(160) + long(164) * 8);
        assert_eq!(long(172), 4);
        assert_eq!(
            word(176),
            package::PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT
        );
        assert_eq!(word(178), 0);
        assert_eq!(
            &bytes[long(168)..long(168) + long(172)],
            package::inline_head_policy_program()
        );
        assert_eq!(long(180), long(168) + long(172));
        let declaration_end = (long(180) + long(184) + 1) & !1;
        if long(192) == 0 {
            assert_eq!(long(196), 0);
            assert_eq!(runtime_bytes, declaration_end);
        } else {
            assert_eq!(long(192), declaration_end);
            assert_eq!(runtime_bytes, long(192) + long(196));
        }
        assert_eq!(
            word(142),
            core.cpu_execution_properties(cpu)
                .unwrap()
                .unwrap()
                .word_size_bytes as u16
        );
        assert_eq!(
            &bytes[long(144)..long(160)],
            package::packed_data_program(word(140), word(52), word(54), word(56), word(142))
        );
        assert_eq!(long(8), runtime_bytes);

        // Moving the runtime prefix within another allocation preserves block-relative offsets.
        let mut embedded = vec![0xa5; 19];
        embedded.extend_from_slice(&bytes[..runtime_bytes]);
        let moved = &embedded[19..];
        let moved_offset = u32::from_be_bytes(moved[124..128].try_into().unwrap()) as usize;
        let moved_bytes = usize::from(u16::from_be_bytes(moved[128..130].try_into().unwrap()));
        assert_eq!(
            &moved[moved_offset..moved_offset + moved_bytes],
            expected.as_bytes()
        );
    }
}

#[test]
fn binary_source_runtime_target_identity_rejects_unsafe_or_long_names() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for dialect in ["", "unsafe/name", "abcdefghijklmnopqrstuvwxyz"] {
        let mut resolved = core.resolve_pipeline("m6502", None).unwrap();
        resolved.dialect_id = dialect.to_string();
        let error = prepare_package(&core, &resolved).unwrap_err();
        assert!(error.contains("runtime package target key"), "{error}");
    }
}

#[test]
fn binary_source_packages_prepare() {
    fn long(bytes: &[u8], offset: usize) -> usize {
        u32::from_be_bytes(bytes[offset..offset + 4].try_into().unwrap()) as usize
    }

    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000", "m68040", "m68080"] {
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let bytes = prepare_package(&core, &resolved).unwrap();
        assert_eq!(&bytes[..4], b"BS27");
        assert_eq!(long(&bytes, 4), bytes.len());

        let runtime_bytes = long(&bytes, 72);
        assert!((200..=bytes.len()).contains(&runtime_bytes));
        let numeric =
            vm::binary_source_package::BinarySourcePackage::prepare(&core, &resolved).unwrap();
        let state_offset = long(&bytes, 192);
        let state_bytes = long(&bytes, 196);
        if numeric.state.defaults.is_empty() {
            assert_eq!((state_offset, state_bytes), (0, 0));
            assert!(numeric.candidates.iter().all(|c| c.state_guard == 0));
        } else {
            assert!(state_offset >= 200 && state_offset + state_bytes <= runtime_bytes);
            let plan = &bytes[state_offset..state_offset + state_bytes];
            assert_eq!(
                usize::from(u16::from_be_bytes(plan[0..2].try_into().unwrap())),
                numeric.state.defaults.len()
            );
            let rows = long(&bytes, 16);
            let count = long(&bytes, 20);
            for index in 0..count {
                let guard = u16::from_be_bytes(
                    bytes[rows + index * crate::binary_source_experiment::ROW + 30
                        ..rows + index * crate::binary_source_experiment::ROW + 32]
                        .try_into()
                        .unwrap(),
                );
                assert!(usize::from(guard) <= numeric.state.guards.len());
            }
            let mut dictionary = long(&bytes, 8);
            let mut bindings = std::collections::BTreeMap::new();
            let mut state_bindings = std::collections::BTreeMap::new();
            for _ in 0..long(&bytes, 12) {
                let length = usize::from(u16::from_be_bytes(
                    bytes[dictionary..dictionary + 2].try_into().unwrap(),
                ));
                let id =
                    u16::from_be_bytes(bytes[dictionary + 2..dictionary + 4].try_into().unwrap());
                let spelling = std::str::from_utf8(&bytes[dictionary + 6..dictionary + 6 + length])
                    .unwrap()
                    .to_string();
                let roles = bytes[dictionary + 5];
                if roles == 4 {
                    assert!(state_bindings.insert(spelling, id).is_none());
                } else {
                    assert!(bindings.insert(spelling, (id, roles)).is_none());
                }
                dictionary = (dictionary + 6 + length + 1) & !1;
            }
            if cpu == "m68040" {
                assert_eq!(bindings["68040"].0, bindings["m68040"].0);
                assert_ne!(state_bindings["68040"], bindings["68040"].0);
                assert!(!state_bindings.contains_key("m68040"));
            }
            let directive_rows = long(plan, 12);
            for (directive_index, directive) in numeric.state.directives.iter().enumerate() {
                assert_eq!(
                    bindings[&numeric.names[directive.head as usize]].0,
                    directive.head
                );
                let arguments_offset = long(plan, directive_rows + directive_index * 12 + 8);
                for (argument_index, argument) in directive.arguments.iter().enumerate() {
                    assert_eq!(argument.kind, 0);
                    let id = argument.matched as u16;
                    let bound = state_bindings[&numeric.names[id as usize]];
                    assert_eq!(bound, id);
                    assert_eq!(
                        long(plan, arguments_offset + argument_index * 12 + 4),
                        usize::from(bound)
                    );
                }
            }
        }
        assert_eq!(runtime_bytes % 2, 0);
        let fragments = long(&bytes, 116);
        let fragment_bytes = long(&bytes, 120);
        assert!(fragments >= runtime_bytes);
        assert_eq!(
            &bytes[fragments..fragments + fragment_bytes],
            package::package::macro_fragment_program()
        );

        let rows = long(&bytes, 16);
        let row_count = long(&bytes, 20);
        let registers = long(&bytes, 24);
        let register_count = long(&bytes, 28);
        let programs = long(&bytes, 32);
        let program_count = long(&bytes, 36);
        for (offset, count, width) in [
            (rows, row_count, crate::binary_source_experiment::ROW),
            (registers, register_count, 6),
            (programs, program_count, 12),
        ] {
            assert!(offset >= 200);
            assert!(offset + count * width <= runtime_bytes);
        }

        let mut runtime_references = vec![
            (rows, row_count * crate::binary_source_experiment::ROW),
            (registers, register_count * 6),
            (programs, program_count * 12),
        ];
        for index in 0..row_count {
            let row = rows + index * crate::binary_source_experiment::ROW;
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
                    assert!(u16::from_be_bytes(predicate[..2].try_into().unwrap()) < 3);
                    assert!(u16::from_be_bytes(predicate[2..].try_into().unwrap()) < name_count);
                }
            }
            let table = u16::from_be_bytes(bytes[row + 28..row + 30].try_into().unwrap());
            if bytes[row + 5] == 7 || (bytes[row + 5] == 9 && table != u16::MAX) {
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
        let macro_call = long(&bytes, 80);
        let macro_call_bytes = long(&bytes, 84);
        let macro_header = long(&bytes, 88);
        let macro_header_bytes = long(&bytes, 92);
        let macro_packed = long(&bytes, 100);
        let macro_packed_bytes = long(&bytes, 104);
        let macro_spelling = long(&bytes, 108);
        let macro_spelling_bytes = long(&bytes, 112);
        assert_eq!(u16::from_be_bytes(bytes[96..98].try_into().unwrap()), 2);
        let for_id = u16::from_be_bytes(bytes[66..68].try_into().unwrap());
        let endfor_id = u16::from_be_bytes(bytes[98..100].try_into().unwrap());
        let name_count = u16::from_be_bytes(bytes[62..64].try_into().unwrap());
        assert!(for_id < name_count && endfor_id < name_count);
        assert_ne!(for_id, endfor_id);
        let expected_call = package::macro_descriptor_program(false);
        let expected_header = package::macro_descriptor_program(true);
        let expected_packed = package::packed_macro_call_program();
        let expected_spelling = package::macro_spelling_program();
        assert_eq!(macro_call_bytes, expected_call.len());
        assert_eq!(macro_header_bytes, expected_header.len());
        assert_eq!(macro_packed_bytes, expected_packed.len());
        assert_eq!(macro_spelling_bytes, expected_spelling.len());
        assert_eq!(
            &bytes[macro_call..macro_call + macro_call_bytes],
            expected_call
        );
        assert_eq!(
            &bytes[macro_header..macro_header + macro_header_bytes],
            expected_header
        );
        assert_eq!(
            &bytes[macro_packed..macro_packed + macro_packed_bytes],
            expected_packed
        );
        assert_eq!(
            &bytes[macro_spelling..macro_spelling + macro_spelling_bytes],
            expected_spelling
        );
        assert!(macro_call >= tokenizer + tokenizer_bytes);
        assert!(macro_call + macro_call_bytes <= macro_header);
        assert!(macro_header + macro_header_bytes <= macro_packed);
        assert!(macro_packed + macro_packed_bytes <= macro_spelling);
        assert!(macro_spelling + macro_spelling_bytes <= bytes.len());
        let file_plan = long(&bytes, 132);
        let file_plan_bytes = long(&bytes, 136);
        assert_eq!(file_plan_bytes, package::packed_file_program(0).len());
        assert!(file_plan >= macro_spelling + macro_spelling_bytes);
        assert!(file_plan + file_plan_bytes <= bytes.len());
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
#[ignore = "requires configured FS-UAE; `.end` word as a source label"]
fn binary_end_directive_name_symbol_fs_uae() {
    let source = ".module test\n.cpu m68020\nstart .block\n bra.w end\nend\n rts\n.bend\n.byte start\n.endmodule\n.end\n";
    assert_binary_source(source.to_string(), "m68020".to_string());
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
    let memory = if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let record = runs[0]
            .captured_artifacts
            .get(&PathBuf::from("Work/memory.bin"))
            .expect("fresh compact CLI memory telemetry");
        assert_eq!(record.len(), 2280);
        let words = record
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(words[0], 0x4d454d44);
        assert_eq!(words[1], 0, "all tracked allocations released");
        assert_eq!(words[3], words[4], "allocation capacities balance");
        assert_eq!(words[11], 0, "cleanup has no live allocation");
        assert!(words[2] > 0 && words[14] > 0 && words[15] > 0);
        assert!(words[28] > 0, "E-clock frequency available");
        assert_eq!(words[29], 0, "profiling completed cleanly");
        let stamp = |index: usize| -> u64 {
            u64::from(words[index]) * 24 * 60 * 60 * 50
                + u64::from(words[index + 1]) * 60 * 50
                + u64::from(words[index + 2])
        };
        let preparation_ticks = stamp(22).checked_sub(stamp(19)).expect("ordered clocks");
        let assembly_ticks = stamp(25).checked_sub(stamp(22)).expect("ordered clocks");
        serde_json::json!({
            "peak_owned_bytes": words[2],
            "prepared_live_bytes": words[5],
            "assembly_live_bytes": words[10],
            "runtime_bytes": words[13],
            "packed_source_bytes": words[14],
            "source_bytes": words[15],
            "preparation_clock_seconds": preparation_ticks as f64 / 50.0,
            "assembly_clock_seconds": assembly_ticks as f64 / 50.0,
            "preparation_stage_seconds": (0..7).map(|index| {
                let ticks = (u64::from(words[30 + 2 * index]) << 32)
                    | u64::from(words[31 + 2 * index]);
                ticks as f64 / f64::from(words[28])
            }).collect::<Vec<_>>(),
        })
    } else {
        assert!(!runs[0]
            .captured_artifacts
            .contains_key(&PathBuf::from("Work/memory.bin")));
        serde_json::Value::Null
    };
    eprintln!(
        "COMPACT_CLI_MEASUREMENT {}",
        serde_json::json!({
            "guest_start_to_done_host_seconds": runs[0].start_to_done_host_seconds,
            "linked_reserved_bytes": allocation.total(),
            "input_bytes": source.len(),
            "output_bytes": oracle.len(),
            "instrumented_memory": memory,
        })
    );
}

#[test]
#[ignore = "requires configured FS-UAE; full current-source self-host and exact Rust Hunk"]
fn compact_cli_self_host_entry_readiness_fs_uae() {
    // The live Rust assembly determines the exact source manifest and Hunk
    // oracle. Fresh native completion must always match the complete Hunk.
    // A fixed source tree lets before/after runs measure the CLI implementation
    // against identical self-host input even when the implementation changes.
    let source_override = std::env::var_os("OPFORGE_SELF_HOST_SOURCE_ROOT");
    let explicit_source_tree = source_override.is_some();
    let root = source_override
        .map(PathBuf::from)
        .unwrap_or_else(|| workspace_root().join("native/motorola68000/amigaos"));
    let root = fs::canonicalize(root).expect("canonical self-host source root");
    let entry = "experimental/opforge_compact_cli.asm";
    let module_roots = [
        "experimental",
        "opforge-cli",
        "tkpkg",
        "tkvm",
        "prvm",
        "exprvm",
        "opcore",
        "opasm",
    ];
    let oracle_dir = create_temp_dir("compact-self-host-rust-oracle");
    let dependency_path = oracle_dir.join("dependencies.d");
    let mut command = vec![
        "opForge".to_string(),
        root.join(entry).to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "68020".to_string(),
        "--dependencies".to_string(),
        dependency_path.to_string_lossy().into_owned(),
    ];
    for directory in module_roots.into_iter().chain(["debug"]) {
        command.extend([
            "-M".to_string(),
            root.join(directory).to_string_lossy().into_owned(),
        ]);
    }
    command.extend([
        "-I".to_string(),
        root.join("debug").to_string_lossy().into_owned(),
    ]);
    let cli = Cli::parse_from(command);
    let mut config = validate_cli(&cli).expect("validate Rust compact self-build");
    config.out_dir = Some(oracle_dir.clone());
    run_with_validated_cli_with_context(&cli, &config)
        .expect("live Rust compact self-build succeeds");
    let hunk_oracle =
        fs::read(oracle_dir.join("build/opforge_compact")).expect("read fresh Rust compact Hunk");
    assert!(!hunk_oracle.is_empty());
    let hunk_allocation = hunk::allocation(&hunk_oracle).expect("valid Rust compact Hunk");
    let dependencies = fs::read_to_string(&dependency_path).expect("read live dependency manifest");
    let (_, prerequisite_text) = dependencies
        .split_once(": ")
        .expect("Makefile dependency rule with prerequisites");
    let mut sources = prerequisite_text
        .split_whitespace()
        .map(|path| {
            let path = PathBuf::from(path);
            let relative = path
                .strip_prefix(&root)
                .expect("self-host dependency remains in native AmigaOS tree");
            (
                relative.to_string_lossy().into_owned(),
                fs::read(&path).expect("read source from Rust dependency manifest"),
            )
        })
        .collect::<Vec<_>>();
    assert!(sources.iter().any(|(path, _)| path == entry));
    sources.sort_by(|left, right| left.0.cmp(&right.0));
    let entry_index = sources.iter().position(|(path, _)| path == entry).unwrap();
    sources.swap(0, entry_index);
    // The guest CLI validates each -M directory before reading the entry.
    // A root with no Rust dependency has no staged directory to validate.
    let native_roots = module_roots
        .iter()
        .copied()
        .filter(|directory| {
            sources
                .iter()
                .any(|(path, _)| path.starts_with(&format!("{directory}/")))
        })
        .collect::<Vec<_>>();
    fs::remove_dir_all(&oracle_dir).expect("remove Rust oracle scratch");
    let source_refs = sources
        .iter()
        .map(|(path, bytes)| (path.as_str(), bytes.as_slice()))
        .collect::<Vec<_>>();
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    // Compare native revisions against the same frozen input and Rust oracle.
    let native_root = std::env::var_os("OPFORGE_COMPARE_NATIVE_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(workspace_root);
    // Identify inputs before execution, including when the native proof fails.
    // This diagnostic fingerprint is separate from the fresh-run proof contract.
    let mut ordered_sources = sources.iter().collect::<Vec<_>>();
    ordered_sources.sort_by(|left, right| left.0.cmp(&right.0));
    let mut manifest_bytes = Vec::new();
    for (path, bytes) in ordered_sources {
        manifest_bytes.extend_from_slice(path.as_bytes());
        manifest_bytes.push(0);
        manifest_bytes.extend_from_slice(bytes);
        manifest_bytes.push(0);
    }
    let input = serde_json::json!({
        "source_kind": if explicit_source_tree {
            "explicit_source_tree"
        } else {
            "current_checkout"
        },
        "source_root": root,
        "source_manifest_digest": crate::fs_uae_smoke::opforge_self_host_package_digest(&manifest_bytes),
        "source_bytes": sources.iter().map(|(_, bytes)| bytes.len()).sum::<usize>(),
        "staged_files": source_refs.len(),
        "native_root": native_root,
        "rust_hunk_bytes": hunk_oracle.len(),
        "runtime_package_bytes": package.len(),
    });
    drop(manifest_bytes);
    eprintln!("COMPACT_SELF_HOST_INPUT {input}");
    let native_started = std::time::Instant::now();
    let result = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &native_root,
        &package,
        &source_refs,
        &native_roots,
        &["debug"],
        Some(hunk_oracle.as_slice()),
        false,
    )
    .unwrap_or_else(|error| {
        panic!(
            "fresh bounded native self-host entry probe: {error}\nnative_run_host_seconds={:.9} (includes host preparation and emulator startup)",
            native_started.elapsed().as_secs_f64()
        )
    });
    let native_run_host_seconds = native_started.elapsed().as_secs_f64();
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    let image = &runs[0].captured_artifacts[&PathBuf::from("Work/build/opforge_compact")];
    let native_allocation = hunk::allocation(image).expect("native executable allocation");
    let assembly_position = progress::failure_position(&runs[0].stdout);
    if std::env::var("OPFORGE_PREPARATION_PROGRESS").as_deref() == Ok("1")
        && runs[0].stdout.contains("progress p=00000018")
    {
        assert!(
            assembly_position.is_some(),
            "self-host assembly position capture"
        );
    }
    let memory = if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let record = runs[0]
            .captured_artifacts
            .get(&PathBuf::from("Work/memory.bin"))
            .expect("fresh self-host readiness telemetry");
        assert_eq!(record.len(), 2280);
        let words = record
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(words[0], 0x4d454d44);
        assert_eq!(words[1], 0, "terminal path releases tracked memory");
        assert_eq!(words[3], words[4]);
        assert_eq!(words[11], 0);
        let phase_seconds = |start: usize, end: usize| {
            let stamp = |offset: usize| {
                u64::from(words[offset]) * 24 * 60 * 60 * 50
                    + u64::from(words[offset + 1]) * 60 * 50
                    + u64::from(words[offset + 2])
            };
            let start = stamp(start);
            let end = stamp(end);
            (start != 0 && end != 0)
                .then(|| end.checked_sub(start))
                .flatten()
                .map(|ticks| ticks as f64 / 50.0)
        };
        let preparation_stages = (words[28] != 0).then(|| {
            [
                "source_io_and_other",
                "package_setup",
                "tokenization",
                "binding_and_raw_records",
                "expression_preparation",
                "runtime_finalization",
                "module_discovery",
            ]
            .iter()
            .enumerate()
            .map(|(index, name)| {
                let ticks =
                    (u64::from(words[30 + 2 * index]) << 32) | u64::from(words[31 + 2 * index]);
                (
                    (*name).to_owned(),
                    serde_json::json!({
                        "seconds": ticks as f64 / f64::from(words[28]),
                        "calls": words[44 + index],
                    }),
                )
            })
            .collect::<serde_json::Map<_, _>>()
        });
        let binding_detail =
            (std::env::var("OPFORGE_BINDING_DETAIL").as_deref() == Ok("1")).then(|| {
                assert_eq!(words[29], 0, "binding detail clocks completed cleanly");
                let names = [
                    "source_line_packed_write_and_bind",
                    "initial_line_plan",
                    "conditionals_scopes_imports",
                    "record_finalization_and_append",
                    "string_line_plan",
                    "template_dispatch",
                    "template_next",
                ];
                let scopes = names
                    .iter()
                    .enumerate()
                    .map(|(index, name)| {
                        let ticks = (u64::from(words[528 + 2 * index]) << 32)
                            | u64::from(words[529 + 2 * index]);
                        (
                            (*name).to_owned(),
                            serde_json::json!({
                                "seconds": ticks as f64 / f64::from(words[28]),
                                "calls": words[542 + index],
                            }),
                        )
                    })
                    .collect::<serde_json::Map<_, _>>();
                let sample_ticks = (u64::from(words[549]) << 32) | u64::from(words[550]);
                let sample_seconds = sample_ticks as f64 / f64::from(words[28]);
                serde_json::json!({
                    "scopes": scopes,
                    "binding_calls": words[551],
                    "binding_samples": words[552],
                    "binding_sample_seconds": sample_seconds,
                    "binding_estimated_seconds": sample_seconds * f64::from(words[551])
                        / f64::from(words[552].max(1)),
                })
            });
        let template_work =
            (std::env::var("OPFORGE_TEMPLATE_WORK").as_deref() == Ok("1")).then(|| {
                assert_eq!(words[29], 0, "template work counts completed cleanly");
                [
                    "role_calls",
                    "role_candidate_searches",
                    "role_failed_candidates",
                    "role_matched_candidates",
                    "line_candidate_searches",
                    "line_candidates_examined",
                    "line_invocations",
                    "line_regular_returns",
                    "line_body_captures",
                    "line_definition_headers",
                    "initial_plan_vm_runs",
                    "string_plan_captures",
                ]
                .iter()
                .enumerate()
                .map(|(index, name)| ((*name).to_owned(), serde_json::json!(words[553 + index])))
                .collect::<serde_json::Map<_, _>>()
            });
        let input_collection =
            (std::env::var("OPFORGE_INPUT_DETAIL").as_deref() == Ok("1")).then(|| {
                assert_eq!(
                    words[29], 0,
                    "input collection clock completed cleanly: {}",
                    runs[0].stdout
                );
                assert!(words[28] > 0);
                let ticks = (u64::from(words[565]) << 32) | u64::from(words[566]);
                serde_json::json!({
                    "seconds": ticks as f64 / f64::from(words[28]),
                    "calls": words[567],
                    "bytes": words[568],
                    "reads": words[569],
                })
            });
        serde_json::json!({
            "peak_owned_bytes": words[2],
            "total_allocated_bytes": words[3],
            "prepared_live_bytes": words[5],
            "assembly_live_bytes": words[10],
            "instrumented_preparation_seconds": phase_seconds(19, 22),
            "instrumented_assembly_seconds": phase_seconds(22, 25),
            "free_at_entry_bytes": words[7],
            "largest_at_entry_bytes": words[8],
            "source_bytes_read": words[15],
            "packed_source_bytes": words[14],
            "profiling_errors": words[29],
            "allocation_failure_flags": words[29] & (64 | 128),
            "allocation_failure_count": words[524],
            "last_failed_request_bytes": words[525],
            "last_failed_block_capacity_bytes": words[526],
            "last_failed_block_used_bytes": words[527],
            "preparation_stages": preparation_stages,
            "binding_detail": binding_detail,
            "input_collection": input_collection,
            "template_work": template_work,
        })
    } else {
        serde_json::Value::Null
    };
    eprintln!(
        "COMPACT_SELF_HOST_READINESS {}",
        serde_json::json!({
            "input": input,
            "staged_files": source_refs.len(),
            "source_bytes": sources.iter().map(|(_, bytes)| bytes.len()).sum::<usize>(),
            "rust_hunk_bytes": hunk_oracle.len(),
            "rust_hunk_segments": hunk_allocation.segments,
            "rust_hunk_linked_reserved_bytes": hunk_allocation.total(),
            "runtime_package_bytes": package.len(),
            "native_image_bytes": image.len(),
            "native_linked_reserved_bytes": native_allocation.total(),
            "native_run_host_seconds": native_run_host_seconds,
            "assembly_position": assembly_position,
            "guest_start_to_done_host_seconds": runs[0].start_to_done_host_seconds,
            "diagnostic": runs[0].stdout,
            "instrumented_memory": memory,
        })
    );
    assert_eq!(
        runs[0].captured_artifacts[&PathBuf::from("Work/output.bin")],
        hunk_oracle
    );
}

#[test]
#[ignore = "requires configured FS-UAE; isolate the next full self-host source module"]
fn compact_cli_self_host_descriptor_module_fs_uae() {
    let root = workspace_root().join("native/motorola68000/amigaos");
    let descriptor = fs::read(root.join("prvm/prvm_macro_descriptors.asm")).unwrap();
    let packed_macro = fs::read(root.join("prvm/prvm_packed_macro.asm")).unwrap();
    let entry = b".module descriptor_probe\n.cpu m68020\n.use prvm.amigaos.macro_descriptors as desc\n.use prvm.amigaos.packed_macro as packed\n.section entry, kind=code\n.long desc.State.ListEnd,packed.State.ListEnd\n.endsection\n.output \"build/descriptors.hunk\", format=hunk, sections=entry,code\n.endmodule\n";
    let abi = fs::read(root.join("prvm/prvm_abi.asm")).unwrap();
    let telemetry = fs::read(root.join("debug/telemetry_macros.i")).unwrap();
    let oracle_dir = create_temp_dir("compact-self-host-descriptor-oracle");
    fs::create_dir_all(oracle_dir.join("experimental")).unwrap();
    fs::create_dir_all(oracle_dir.join("prvm")).unwrap();
    fs::create_dir_all(oracle_dir.join("debug")).unwrap();
    let input = oracle_dir.join("experimental/probe.asm");
    fs::write(&input, entry).unwrap();
    fs::write(
        oracle_dir.join("prvm/prvm_macro_descriptors.asm"),
        &descriptor,
    )
    .unwrap();
    fs::write(oracle_dir.join("prvm/prvm_packed_macro.asm"), &packed_macro).unwrap();
    fs::write(oracle_dir.join("prvm/prvm_abi.asm"), &abi).unwrap();
    fs::write(oracle_dir.join("debug/telemetry_macros.i"), &telemetry).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "68020".to_string(),
        "-M".to_string(),
        oracle_dir.join("prvm").to_string_lossy().into_owned(),
        "-I".to_string(),
        oracle_dir.join("debug").to_string_lossy().into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(oracle_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("fresh Rust descriptor module");
    let expected = fs::read(oracle_dir.join("build/descriptors.hunk")).unwrap();
    fs::remove_dir_all(&oracle_dir).unwrap();
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let files = [
        ("experimental/probe.asm", entry.as_slice()),
        ("prvm/prvm_macro_descriptors.asm", descriptor.as_slice()),
        ("prvm/prvm_packed_macro.asm", packed_macro.as_slice()),
        ("prvm/prvm_abi.asm", abi.as_slice()),
        ("debug/telemetry_macros.i", telemetry.as_slice()),
    ];
    let outcome = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &package,
        &files,
        &["prvm"],
        &["debug"],
        Some(&expected),
        false,
    )
    .expect("fresh exact descriptor module comparison");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    eprintln!(
        "COMPACT_SELF_HOST_DESCRIPTOR bytes={} seconds={:?}",
        expected.len(),
        runs[0].start_to_done_host_seconds
    );
}

#[test]
#[ignore = "requires configured FS-UAE; binary segment expansion in compact CLI"]
fn compact_cli_segment_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000", "m68040", "m68080"] {
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
    for cpu in ["m6502", "m68000", "m68040", "m68080"] {
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
    for cpu in ["m6502", "m68000", "m68040", "m68080"] {
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
    for cpu in ["m6502", "m68000", "m68040", "m68080"] {
        let source = format!(
            ".cpu {cpu}\n.org $2000\nEMPTY .macro\n .byte $11\n.endm\n.macro FILL(value)\n .byte .value\n.endmacro\n .EMPTY\n .EMPTY()\n .FILL(2)\n .FILL 3\n.end\n"
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
        assert_eq!(oracle, [0x11, 0x11, 2, 3]);
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
    for cpu in ["m6502", "m68000", "m68040", "m68080"] {
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
    for cpu in ["m6502", "m68000", "m68040", "m68080"] {
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
    for cpu in ["m6502", "m68000", "m68040", "m68080"] {
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
    for cpu in ["m6502", "m68000", "m68040", "m68080"] {
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
#[ignore = "requires configured FS-UAE; imported packed macro visibility"]
fn compact_cli_imported_macro_fs_uae() {
    let source = ".module lib\n.pub\nEMIT .macro value\n .byte .value\n.endmacro\n.endmodule\n.module app\n.cpu m6502\n.use lib (*)\n.org $2000\n .EMIT 3\n.endmodule\n.end\n";
    let oracle_dir = create_temp_dir("compact-binary-imported-macro");
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
    assert_eq!(oracle, [3]);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        Some(&oracle),
    )
    .expect("compact CLI must resolve an imported packed macro");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
}

#[test]
#[ignore = "requires configured FS-UAE; selected packed macro import"]
fn compact_cli_imported_macro_selected_fs_uae() {
    let source = ".module lib\n.pub\nEMIT .macro value\n .byte .value\n.endmacro\n.endmodule\n.module app\n.cpu m6502\n.use lib (EMIT)\n.org $2000\n .EMIT 3\n.endmodule\n.end\n";
    let oracle_dir = create_temp_dir("compact-binary-imported-macro-selected");
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
    assert_eq!(oracle, [3]);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        Some(&oracle),
    )
    .expect("compact CLI must resolve a selected packed macro");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
}

#[test]
#[ignore = "requires configured FS-UAE; qualified module alias packed macro import"]
fn compact_cli_imported_macro_qualified_alias_fs_uae() {
    let source = ".module lib\n.pub\nEMIT .macro value\n .byte .value\n.endmacro\n.endmodule\n.module app\n.cpu m6502\n.use lib as L\n.org $2000\n .L.EMIT 3\n.endmodule\n.end\n";
    let oracle_dir = create_temp_dir("compact-binary-imported-macro-qualified-alias");
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
    assert_eq!(oracle, [3]);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        Some(&oracle),
    )
    .expect("compact CLI must resolve a qualified module alias packed macro");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
}

#[test]
#[ignore = "requires configured FS-UAE; packed macro @ placeholders"]
fn compact_cli_macro_at_placeholders_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000", "m68040", "m68080"] {
        let source = format!(
            ".cpu {cpu}\n.org $2000\nBOTH .macro left,right\n .byte @1\n .byte .@\n .byte .{{right}}\n.endmacro\n .BOTH 3,4\n .BOTH(5,6)\n.end\n"
        );
        let oracle_dir = create_temp_dir(&format!("compact-binary-macro-at-{cpu}"));
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
        assert_eq!(oracle, [3, 3, 4, 4, 5, 5, 6, 6]);
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let package = prepare_package(&core, &resolved).unwrap();
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            Some(&oracle),
        )
        .expect("compact CLI must substitute packed @ placeholders");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; embedded packed macro substitutions"]
fn compact_cli_macro_embedded_substitutions_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let source = b".cpu m6502\n.org $2000\nEMIT .macro suffix\nsymbol@1:\n .word \"x@1\"\n .word symbol@1\n.endmacro\n .EMIT A\n.end\n";
    let oracle_dir = create_temp_dir("compact-binary-macro-embedded");
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
    assert_eq!(oracle, [0x78, 0x41, 0x00, 0x20]);
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source,
        Some(&oracle),
    )
    .expect("compact CLI must expand embedded packed placeholders");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
}

#[test]
#[ignore = "requires configured FS-UAE; multiple embedded packed substitutions"]
fn compact_cli_macro_multiple_embedded_substitutions_fs_uae() {
    let source = b".cpu m6502\n.org $2000\nEMIT .macro left,right\nsymbol@1@2suffix:\n .byte \"x@1-@2\"\n .word symbol@1@2suffix\n.endmacro\n .EMIT A,B\n.end\n";
    let oracle_dir = create_temp_dir("compact-binary-macro-multiple-embedded");
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
    assert_eq!(oracle, [0x78, 0x41, 0x2d, 0x42, 0x00, 0x20]);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source,
        Some(&oracle),
    )
    .expect("compact CLI must expand multiple embedded placeholders");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
}

#[test]
#[ignore = "requires configured FS-UAE; embedded defaults and leading placeholders"]
fn compact_cli_macro_embedded_default_spelling_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    for (source, expected) in [
        (
            ".cpu m6502\n.org $2000\nEMIT .macro value=A\n@1suffix:\n .byte \"n@1\"\n .word @1suffix\n.endmacro\n .EMIT\n.end\n",
            &[0x6e, 0x41, 0x00, 0x20][..],
        ),
        (
            ".cpu m6502\nEMIT .macro value=$0A\n .byte \"n@1\"\n.endmacro\n .EMIT\n.end\n",
            &[0x6e, 0x24, 0x30, 0x41][..],
        ),
        (
            ".cpu m6502\nEMIT .macro value=$0A\n .byte \"n@1\"\n.endmacro\n .EMIT()\n.end\n",
            &[0x6e, 0x24, 0x30, 0x41][..],
        ),
    ] {
        let oracle_dir = create_temp_dir("compact-binary-macro-embedded-default");
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
        assert_eq!(oracle, expected);
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            Some(&oracle),
        )
        .expect("compact CLI must preserve default text inside placeholders");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; nested exact macro argument spelling"]
fn compact_cli_macro_nested_embedded_spelling_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    for argument in ["@1", ".value", ".1", ".{value}", ".@"] {
        let source = format!(
            ".cpu m6502\nINNER .macro value\n .byte \"n@1\"\n.endmacro\nOUTER .macro value\n .INNER {argument}\n.endmacro\n .OUTER $0A\n.end\n"
        );
        let oracle_dir = create_temp_dir("compact-binary-macro-nested-spelling");
        let oracle_input = oracle_dir.join("input.asm");
        let oracle_output = oracle_dir.join("oracle.bin");
        fs::write(&oracle_input, &source).expect("write Rust oracle source");
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
        assert_eq!(oracle, [0x6e, 0x24, 0x30, 0x41]);
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            Some(&oracle),
        )
        .expect("nested compact macro must preserve exact argument spelling");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; quoted macro arguments and header state"]
fn compact_cli_macro_quoted_arguments_and_header_state_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    for (source, expected) in [
        (
            ".cpu m6502\nM .macro value\n .byte .value\n.endmacro\n .M \"A\"\n.end\n",
            &[0x41][..],
        ),
        (
            ".cpu m6502\nM .macro value\n .byte .value\n.endmacro\n .M(\"A\")\n.end\n",
            &[0x41][..],
        ),
        (
            ".cpu m6502\nM .macro value=\"B\"\n .byte .value\n.endmacro\n .M\n.end\n",
            &[0x42][..],
        ),
        (
            ".cpu m6502\n.macro FIRST(v)\n .byte .v\n.endmacro\nSECOND .macro v=2\n .byte .v\n.endmacro\n .FIRST(1)\n .SECOND\n.end\n",
            &[1, 2][..],
        ),
    ] {
        let oracle_dir = create_temp_dir("compact-binary-macro-quoted-header");
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
        assert_eq!(oracle, expected);
        let result = crate::fs_uae_smoke::run_compact_cli_from_env(
            &workspace_root(),
            &package,
            source.as_bytes(),
            Some(&oracle),
        )
        .expect("compact CLI must preserve quoted arguments and header state");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; nested full-list spacing"]
fn compact_cli_macro_nested_full_list_spacing_fs_uae() {
    let source = ".cpu m6502\nINNER .macro value\n .byte \"n@1\"\n.endmacro\nOUTER .macro x,y\n .INNER ((.@))\n.endmacro\n .OUTER($01 ,  $02)\n.end\n";
    let oracle_dir = create_temp_dir("compact-binary-macro-full-list-spacing");
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
    assert_eq!(oracle, b"n($01 ,  $02)");
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        Some(&oracle),
    )
    .expect("nested full-list substitution must preserve exact spacing");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
}

#[test]
#[ignore = "requires configured FS-UAE; named and full-list substitutions in strings"]
fn compact_cli_macro_embedded_dotted_strings_fs_uae() {
    let source = ".cpu m6502\nM .macro left,right\n .byte \"n.left\"\n .byte \"n.1\"\n .byte \"n.{right}\"\n .byte \"n.@\"\n.endmacro\n .M A ,  B\n .byte \"n.left\"\n.end\n";
    let oracle_dir = create_temp_dir("compact-binary-macro-dotted-strings");
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
    assert_eq!(oracle, b"nAnAnBnA ,  Bn.left");
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        Some(&oracle),
    )
    .expect("embedded dotted substitutions must preserve exact spelling");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
}

#[test]
#[ignore = "requires configured FS-UAE; embedded packed identifier substitution"]
fn compact_cli_macro_embedded_identifier_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let source = b".cpu m6502\n.org $2000\nEMIT .macro suffix\nsymbol@1:\n .word symbol@1\n.endmacro\n .EMIT A\n.end\n";
    let oracle_dir = create_temp_dir("compact-binary-macro-embedded-id");
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
    assert_eq!(oracle, [0, 0x20]);
    let resolved = core.resolve_pipeline("m6502", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source,
        Some(&oracle),
    )
    .expect("compact CLI must expand embedded identifiers");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
}

#[test]
#[ignore = "requires configured FS-UAE; packed string data operands"]
fn compact_cli_string_data_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000", "m68040", "m68080"] {
        let source = format!(
            ".cpu {cpu}\n.org $2000\n.byte \"A\", \"BC\"\n.word \"D\", \"EF\"\n.long \"G\", \"HI\"\n.byte \"\\0\"\n.byte \"x@1\"\n.end\n"
        );
        let oracle_dir = create_temp_dir(&format!("compact-binary-string-{cpu}"));
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
            &[
                0x41, 0x42, 0x43, 0x44, 0, 0x45, 0x46, 0x47, 0, 0, 0, 0x48, 0x49, 0, 0x78, 0x40,
                0x31,
            ][..]
        } else {
            &[
                0x41, 0x42, 0x43, 0, 0x44, 0x45, 0x46, 0, 0, 0, 0x47, 0x48, 0x49, 0, 0x78, 0x40,
                0x31,
            ][..]
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
        .expect("compact CLI must emit packed string operands");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].success && runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(0));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; bounded compact CLI macro comparison"]
fn compact_cli_macro_repeat_comparison_fs_uae() {
    let cpu = std::env::var("OPFORGE_COMPARE_CPU").expect("comparison CPU");
    assert!(matches!(cpu.as_str(), "m6502" | "m68000"));
    let blocks: usize = std::env::var("OPFORGE_COMPARE_BLOCKS")
        .expect("comparison block count")
        .parse()
        .expect("numeric comparison block count");
    assert!(matches!(blocks, 8 | 32));
    let source_path = std::env::var("OPFORGE_COMPARE_SOURCE").expect("comparison source path");
    let source = fs::read(&source_path).expect("read comparison source");
    let mut lines = vec![
        format!(".cpu {cpu}"),
        ".org $1000".to_string(),
        "EMIT .macro value".to_string(),
        " .byte .value".to_string(),
        " nop".to_string(),
        ".endmacro".to_string(),
    ];
    let nop: &[u8] = if cpu == "m6502" {
        &[0xea]
    } else {
        &[0x4e, 0x71]
    };
    let mut expected = Vec::with_capacity(blocks * 8 * (nop.len() + 1));
    for block in 0..blocks {
        for item in 0..8 {
            let value = (block * 8 + item) as u8;
            lines.push(format!(" .EMIT({value})"));
            expected.push(value);
            expected.extend_from_slice(nop);
        }
    }
    lines.push(".end".to_string());
    assert_eq!(source, (lines.join("\n") + "\n").as_bytes());

    let oracle_dir = create_temp_dir("compact-macro-repeat-comparison");
    let oracle_input = oracle_dir.join("input.asm");
    let oracle_output = oracle_dir.join("oracle.bin");
    fs::write(&oracle_input, &source).expect("write Rust oracle source");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        oracle_input.to_string_lossy().into_owned(),
        "--bin".to_string(),
        oracle_output.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        cpu.clone(),
    ]);
    let rust_started = std::time::Instant::now();
    run_with_cli_with_context(&cli).expect("assemble live Rust CLI oracle");
    let rust_cli_seconds = rust_started.elapsed().as_secs_f64();
    let oracle = fs::read(&oracle_output).expect("read Rust CLI oracle");
    fs::remove_dir_all(&oracle_dir).expect("remove Rust CLI oracle scratch");
    assert_eq!(
        oracle, expected,
        "Rust CLI must match independent macro bytes"
    );

    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(&cpu, None).unwrap();
    let package_started = std::time::Instant::now();
    let package = prepare_package(&core, &resolved).unwrap();
    let package_preparation_seconds = package_started.elapsed().as_secs_f64();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        &source,
        Some(&oracle),
    )
    .expect("compact native CLI must match live Rust and independent macro bytes");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.success && run.protocol_completed);
    assert_eq!(run.exit_code, Some(0));
    let image = run
        .captured_artifacts
        .get(&PathBuf::from("Work/build/opforge_compact"))
        .expect("fresh compact CLI image");
    let allocation = hunk::allocation(image).expect("valid compact CLI Hunk");
    assert!(allocation.total() < 2 * 1024 * 1024);
    eprintln!(
        "COMPACT_MACRO_COMPARISON {}",
        serde_json::json!({
            "cpu": cpu,
            "blocks": blocks,
            "source_bytes": source.len(),
            "output_bytes": expected.len(),
            "runtime_package_bytes": package.len(),
            "native_linked_reserved_bytes": allocation.total(),
            "native_linked_code_bytes": allocation.code,
            "native_linked_data_reserved_bytes": allocation.data,
            "native_linked_bss_reserved_bytes": allocation.bss,
            "host_package_preparation_seconds": package_preparation_seconds,
            "rust_cli_seconds": rust_cli_seconds,
            "guest_start_to_done_host_seconds": run.start_to_done_host_seconds,
            "native_image_digest": run.native_image_digest,
            "exact_output": oracle,
            "guest_exit": run.exit_code,
            "memory": macro_profile::report(run),
        })
    );
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
        assert_eq!(record.len(), 2280);
        let words: Vec<u32> = record
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect();
        assert_eq!(words[0], 0x4d454d44);
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
            "source_io_and_other",
            "package_setup",
            "tokenization",
            "binding_and_raw_records",
            "expression_preparation",
            "runtime_finalization",
            "module_discovery",
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
                    "calls": words[44 + index],
                }),
            );
        }
        assert_eq!(words[45], 1, "one package setup");
        assert_eq!(words[49], 1, "one finalization");
        assert_eq!(words[46], words[47], "each tokenized line binds once");
        assert_eq!(words[47], words[48], "each bound line prepares once");
        assert_eq!(
            words[46] as usize,
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

        let opcodes = &words[51..72];
        let pairs = &words[72..513];
        let work = &words[513..520];
        let opcode_total: u64 = opcodes.iter().map(|n| u64::from(*n)).sum();
        let pair_total: u64 = pairs.iter().map(|n| u64::from(*n)).sum();
        assert_eq!(opcodes[0], words[46], "each successful line ends once");
        assert_eq!(pair_total + u64::from(words[46]), opcode_total);
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
            let offset = 520 + index * 2;
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
        assert_eq!(record.len(), 2280);
        let words: Vec<u32> = record
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect();
        assert_eq!(words[0], 0x4d454d44);
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

#[test]
fn binary_source_all_supported_profiles_and_dialects_prepare() {
    let registry = default_registry();
    let dialects = vm::builder::build_hierarchy_chunks_from_registry(&registry)
        .unwrap()
        .dialects;
    let core = RuntimeModelCore::from_registry(&registry).unwrap();
    for (cpu, _, _) in core.supported_cpus() {
        let resolved = core.resolve_pipeline(&cpu, None).unwrap();
        prepare_package(&core, &resolved).unwrap_or_else(|error| panic!("{cpu}: {error}"));
        for dialect in &dialects {
            if let Ok(resolved) = core.resolve_pipeline(&cpu, Some(&dialect.id)) {
                prepare_package(&core, &resolved)
                    .unwrap_or_else(|error| panic!("{cpu}/{}: {error}", dialect.id));
            }
        }
    }
}
