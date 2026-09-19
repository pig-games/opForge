//! Level B package checks and opt-in Level D native binary-source proof.
use super::*;
use crate::binary_source_experiment::prepare_package;
use crate::fs_uae_smoke::FsUaeSmokeOutcome;
use vm::runtime_model_core::RuntimeModelCore;

#[path = "binary_source_hunk.rs"]
mod hunk;

#[test]
fn binary_source_packages_prepare() {
    fn long(bytes: &[u8], offset: usize) -> usize {
        u32::from_be_bytes(bytes[offset..offset + 4].try_into().unwrap()) as usize
    }

    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m6502", "m68000"] {
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let bytes = prepare_package(&core, &resolved).unwrap();
        assert_eq!(&bytes[..4], b"BSP2");
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
            (rows, row_count, 24),
            (registers, register_count, 6),
            (programs, program_count, 12),
        ] {
            assert!(offset >= 76);
            assert!(offset + count * width <= runtime_bytes);
        }

        let mut runtime_references = vec![
            (rows, row_count * 24),
            (registers, register_count * 6),
            (programs, program_count * 12),
        ];
        for index in 0..row_count {
            let row = rows + index * 24;
            let input_count =
                u16::from_be_bytes(bytes[row + 10..row + 12].try_into().unwrap()) as usize;
            let inputs = long(&bytes, row + 12);
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

fn assert_binary_source(source: String, cpu: String) -> serde_json::Value {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(&cpu, None).unwrap();
    let preparation_started = std::time::Instant::now();
    let mut input = prepare_package(&core, &resolved).unwrap();
    let package_preparation_seconds = preparation_started.elapsed().as_secs_f64();
    let package_bytes = input.len();
    input.extend_from_slice(source.as_bytes());
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live Rust source oracle");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let oracle: Vec<u8> = entries.into_iter().map(|(_, byte)| byte).collect();
    let native_root = std::env::var_os("OPFORGE_COMPARE_NATIVE_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(workspace_root);
    let result =
        crate::fs_uae_smoke::run_binary_source_harness_from_env(&native_root, &input, &oracle)
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
        assert_eq!(record.len(), 192);
        let words: Vec<u32> = record
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect();
        assert_eq!(words[0], 0x4d454d34);
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
        assert_eq!(words[44] as usize, source.lines().count());
        let stage_seconds = total_ticks as f64 / f64::from(words[28]);
        assert!(
            (stage_seconds - preparation_ticks as f64 / 50.0).abs() <= 0.04,
            "E-clock stages reconcile with coarse preparation: {stage_seconds}"
        );

        serde_json::json!({
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
            "cpu": cpu, "source_bytes": source.len(), "runtime_package_bytes": package_bytes,
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
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
    let mut input = prepare_package(&core, &resolved).unwrap();
    input.extend_from_slice(source.as_bytes());
    let result =
        crate::fs_uae_smoke::run_binary_source_rejection_from_env(&workspace_root(), &input)
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
        assert_eq!(record.len(), 192);
        let words: Vec<u32> = record
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect();
        assert_eq!(words[0], 0x4d454d34);
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
