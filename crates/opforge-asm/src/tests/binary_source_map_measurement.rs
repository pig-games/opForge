//! Bounded two-map scaling case; the same generated sources feed both oracles.
use super::*;

const ITEMS_PER_IMPORT: usize = 64;
const MIXED_BLOCKS: usize = 24;

fn sources(cpu: &str) -> [String; 3] {
    // Each pair emits three concrete bytes, one unowned byte, one block-entry
    // byte, and four bytes per item. Its region ends before the next pair.
    let first_size = 5 + 4 * ITEMS_PER_IMPORT;
    let second_start = 0x1000 + first_size;
    let first_end = second_start - 1;
    let second_end = second_start + first_size - 1;
    let main = format!(
        ".module main\n.cpu {cpu}\n.region rom_a, $1000, ${first_end:04x}\n.region rom_b, ${second_start:04x}, ${second_end:04x}\n.use dep_a (entry) as left map {{ code_a -> app_a }}\n.use dep_b (entry) as right map {{ code_b -> app_b }}\n.section app_a\n.byte $a0\n.word left.entry\n.endsection\n.section app_b\n.byte $a1\n.word right.entry\n.endsection\n.place app_a in rom_a\n.place app_b in rom_b\n.endmodule\n.end\n"
    );
    let dependency = |name: &str, section: &str, prefix: u8, seed: u8| {
        let mut source = format!(
            ".module {name}\n.cpu {cpu}\n.pub\n.section {section}, logical\n.byte ${prefix:02x}\nentry .block\n.byte ${seed:02x}\n"
        );
        for index in 0..ITEMS_PER_IMPORT {
            let value = seed.wrapping_add(index as u8);
            source.push_str(&format!(
                ".byte ${value:02x}, ${:02x}\n.word entry + {}\n",
                value ^ 0x5a,
                index + 1
            ));
        }
        source.push_str(".bend\nunused .block\n.byte $99\n.bend\n.endsection\n.endmodule\n.end\n");
        source
    };
    [
        main,
        dependency("dep_a", "code_a", 0xb0, 0x10),
        dependency("dep_b", "code_b", 0xc0, 0x20),
    ]
}

fn files<'a>(sources: &'a [String; 3]) -> [(&'static str, &'a str); 3] {
    [
        ("main.asm", &sources[0]),
        ("library/dep_a.asm", &sources[1]),
        ("library/dep_b.asm", &sources[2]),
    ]
}

// Repeat a useful instruction-and-data kernel to exercise selection, branches,
// expressions and imported-block reachability without constructing a huge input.
fn mixed_sources(cpu: &str) -> ([String; 2], Vec<u8>) {
    mixed_sources_with_blocks(cpu, MIXED_BLOCKS)
}

fn mixed_sources_with_blocks(cpu: &str, blocks: usize) -> ([String; 2], Vec<u8>) {
    mixed_sources_variant(cpu, blocks, true)
}

fn mixed_sources_variant(
    cpu: &str,
    blocks: usize,
    exercise_pruning_and_offset: bool,
) -> ([String; 2], Vec<u8>) {
    assert!(["m6502", "m68000"].contains(&cpu));
    let main = format!(
        ".module main\n.cpu {cpu}\n.use worker as work\n.word work.run\n.endmodule\n.end\n"
    );
    let mut worker = format!(".module worker\n.cpu {cpu}\n.org $1000\n.pub\nrun .block\n");
    let mut expected = Vec::new();
    for index in 0..blocks {
        let value = (index % 64 + 1) as u8;
        let other = ((index * 3) % 64 + 1) as u8;
        let address = 0x1000 + expected.len();
        let offset = usize::from(exercise_pruning_and_offset);
        if cpu == "m6502" {
            worker.push_str(&format!(
                "block{index}:\n lda #{value}\n ldx #{other}\n sta $2000\n bne next{index}\n .byte 0\nnext{index}: nop\n .word block{index}{offset_expr}\n",
                offset_expr = if exercise_pruning_and_offset { "+1" } else { "" },
            ));
            expected
                .extend_from_slice(&[0xa9, value, 0xa2, other, 0x8d, 0, 0x20, 0xd0, 1, 0, 0xea]);
            expected.extend_from_slice(&((address + offset) as u16).to_le_bytes());
        } else {
            worker.push_str(&format!(
                "block{index}:\n moveq #{value},d0\n moveq #{other},d1\n move.b d0,($2000).w\n bne.s next{index}\n .word 0\nnext{index}: nop\n .long block{index}{offset_expr}\n",
                offset_expr = if exercise_pruning_and_offset { "+2" } else { "" },
            ));
            expected.extend_from_slice(&[
                0x70, value, 0x72, other, 0x11, 0xc0, 0x20, 0, 0x66, 2, 0, 0, 0x4e, 0x71,
            ]);
            expected.extend_from_slice(&((address + 2 * offset) as u32).to_be_bytes());
        }
    }
    worker.push_str(".bend\n");
    if exercise_pruning_and_offset {
        worker.push_str("unused .block\n.byte $99\n.bend\n");
    }
    worker.push_str(".endmodule\n.end\n");
    if cpu == "m6502" {
        expected.extend_from_slice(&0x1000u16.to_le_bytes());
    } else {
        expected.extend_from_slice(&0x1000u16.to_be_bytes());
    }
    ([main, worker], expected)
}

fn mixed_files<'a>(sources: &'a [String; 2]) -> [(&'static str, &'a str); 2] {
    [
        ("main.asm", &sources[0]),
        ("library/worker.asm", &sources[1]),
    ]
}

#[test]
fn compact_mixed_measurement_rust_oracle() {
    for cpu in ["m6502", "m68000"] {
        let (sources, independent) = mixed_sources(cpu);
        let actual = oracle_with_roots(&mixed_files(&sources), &["library"]).unwrap();
        assert_eq!(actual, independent);
    }
}

#[test]
fn full_cli_comparable_mixed_rust_oracle() {
    for cpu in ["m6502", "m68000"] {
        let (sources, independent) = mixed_sources_variant(cpu, 8, false);
        let actual = oracle_with_roots(&mixed_files(&sources), &["library"]).unwrap();
        assert_eq!(actual, independent);
    }
}

#[test]
#[ignore = "requires configured 68020 / 2 MiB FS-UAE; bounded mixed-instruction baseline"]
fn compact_mixed_measurement_fs_uae() {
    let cpu = std::env::var("OPFORGE_MEASURE_CPU").expect("select m6502 or m68000");
    let blocks = std::env::var("OPFORGE_COMPARE_BLOCKS")
        .ok()
        .map(|value| value.parse::<usize>().expect("numeric block count"))
        .unwrap_or(MIXED_BLOCKS);
    assert!((1..=MIXED_BLOCKS).contains(&blocks));
    let (sources, independent) = mixed_sources_with_blocks(&cpu, blocks);
    let files = mixed_files(&sources);
    let rust_started = std::time::Instant::now();
    let expected = oracle_with_roots(&files, &["library"]).expect("live Rust image");
    let rust_seconds = rust_started.elapsed().as_secs_f64();
    assert_eq!(expected, independent);
    run_measurement(
        &cpu,
        &files,
        &expected,
        rust_seconds,
        "COMPACT_MIXED_MEASUREMENT",
        None,
        None,
    );
}

#[test]
#[ignore = "requires configured expanded-memory FS-UAE; full and compact CLI comparison"]
fn full_compact_mixed_comparison_fs_uae() {
    use crate::fs_uae_smoke::{
        OpforgeNativeCliGuestFile, OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase,
        OpforgeNativeCliProof,
    };

    assert_ne!(
        std::env::var("OPFORGE_FS_UAE_MEMORY_PROFILE").as_deref(),
        Ok("2m"),
        "the full CLI cannot fit the constrained 2 MiB profile"
    );
    let cpu = std::env::var("OPFORGE_MEASURE_CPU").expect("select m6502 or m68000");
    let blocks = std::env::var("OPFORGE_COMPARE_BLOCKS")
        .ok()
        .map(|value| value.parse::<usize>().expect("numeric block count"))
        .unwrap_or(8);
    assert!((1..=MIXED_BLOCKS).contains(&blocks));
    // Keep the richer compact-only case as a separate correctness probe.
    // Compare the CLIs on their common expression and block-inclusion subset.
    let (sources, independent) = mixed_sources_variant(&cpu, blocks, false);
    let files = mixed_files(&sources);
    let expected = oracle_with_roots(&files, &["library"]).expect("live Rust image");
    assert_eq!(expected, independent);
    let full_package = build_hierarchy_package_from_registry(&default_registry()).unwrap();
    let compact_core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let compact_pipeline = compact_core.resolve_pipeline(&cpu, None).unwrap();
    let compact_package = prepare_package(&compact_core, &compact_pipeline).unwrap();
    let full_files = [OpforgeNativeCliGuestFile {
        relative_path: "worker.asm",
        bytes: sources[1].as_bytes(),
    }];
    let command = format!(
        "{{input}} --bin {{bin}} --cpu {cpu} --opasm-package {{package}} -M {{guest_work_dir}}"
    );
    let full_case = [OpforgeNativeCliParityCase {
        name: "full-compact-mixed-comparison",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(sources[0].as_bytes()),
        command_template: Some(&command),
        package_mode: OpforgeNativeCliPackageMode::Explicit(&full_package),
        extra_guest_files: &full_files,
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle: &expected,
        },
    }];
    let full = crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(
        &workspace_root(),
        &full_case,
    )
    .expect("fresh full CLI run");
    let FsUaeSmokeOutcome::Completed { runs } = full else {
        panic!("real FS-UAE full CLI run required");
    };
    let full = &runs[0];
    assert!(full.success && full.protocol_completed);
    assert_eq!(full.exit_code, Some(0));
    let compact_bytes = files
        .iter()
        .map(|(name, source)| (*name, source.as_bytes()))
        .collect::<Vec<_>>();
    let compact = crate::fs_uae_smoke::run_compact_cli_files_from_env(
        &workspace_root(),
        &compact_package,
        &compact_bytes,
        &["library"],
        &[],
        Some(&expected),
        false,
    )
    .expect("fresh compact CLI run");
    let FsUaeSmokeOutcome::Completed { runs } = compact else {
        panic!("real FS-UAE compact CLI run required");
    };
    let compact = &runs[0];
    assert!(compact.success && compact.protocol_completed);
    assert_eq!(compact.exit_code, Some(0));
    eprintln!(
        "FULL_COMPACT_MIXED_COMPARISON {}",
        serde_json::json!({
            "cpu": cpu,
            "blocks": blocks,
            "source_lines": sources.iter().map(|source| source.lines().count()).sum::<usize>(),
            "source_bytes": sources.iter().map(String::len).sum::<usize>(),
            "output_bytes": expected.len(),
            "full_package_bytes": full_package.len(),
            "compact_package_bytes": compact_package.len(),
            "full_seconds": full.start_to_done_host_seconds,
            "compact_seconds": compact.start_to_done_host_seconds,
            "full_image_bytes": full.captured_artifacts[&std::path::PathBuf::from("Work/build/opforge_cli")].len(),
            "compact_image_bytes": compact.captured_artifacts[&std::path::PathBuf::from("Work/build/opforge_compact")].len(),
        })
    );
}

#[test]
fn two_map_measurement_rust_oracle() {
    for cpu in ["m6502", "m68000"] {
        let sources = sources(cpu);
        let expected = oracle_address_ordered_with_roots(&files(&sources), &["library"])
            .expect("contiguous live Rust image");
        assert_eq!(expected.len(), 2 * (5 + 4 * ITEMS_PER_IMPORT));
        assert!(!expected.contains(&0x99));
    }
}

#[test]
#[ignore = "requires configured 68020 / 2 MiB FS-UAE; bounded two-map measurement"]
fn two_map_measurement_fs_uae() {
    let cpu = std::env::var("OPFORGE_MEASURE_CPU").expect("select m6502 or m68000");
    assert!(["m6502", "m68000"].contains(&cpu.as_str()));
    let sources = sources(&cpu);
    let files = files(&sources);
    let rust_started = std::time::Instant::now();
    let expected = oracle_address_ordered_with_roots(&files, &["library"])
        .expect("contiguous live Rust image");
    let rust_seconds = rust_started.elapsed().as_secs_f64();
    run_measurement(
        &cpu,
        &files,
        &expected,
        rust_seconds,
        "TWO_MAP_MEASUREMENT",
        Some(ITEMS_PER_IMPORT),
        Some(5),
    );
}

fn run_measurement(
    cpu: &str,
    files: &[(&str, &str)],
    expected: &[u8],
    rust_seconds: f64,
    report_name: &str,
    items_per_import: Option<usize>,
    sweeps_per_pass: Option<u32>,
) {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let bytes = files
        .iter()
        .map(|(name, source)| (*name, source.as_bytes()))
        .collect::<Vec<_>>();
    let instrumented = std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1");
    let result = if instrumented {
        crate::fs_uae_smoke::run_binary_discovery_with_includes_from_env(
            &workspace_root(),
            &package,
            &bytes,
            Some(expected),
            None,
            &["library"],
            &[],
        )
    } else {
        crate::fs_uae_smoke::run_compact_cli_files_from_env(
            &workspace_root(),
            &package,
            &bytes,
            &["library"],
            &[],
            Some(expected),
            false,
        )
    }
    .expect("fresh native run");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE run required");
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.success && run.protocol_completed);
    assert_eq!(run.exit_code, Some(0));
    let image_name = if instrumented {
        "Work/build/binary_source_harness"
    } else {
        "Work/build/opforge_compact"
    };
    let image = run
        .captured_artifacts
        .get(&PathBuf::from(image_name))
        .unwrap();
    let allocation = hunk::allocation(image).unwrap();
    let source_lines = files
        .iter()
        .map(|(_, source)| source.lines().count())
        .sum::<usize>();
    let memory = if instrumented {
        let record = run
            .captured_artifacts
            .get(&PathBuf::from("Work/memory.bin"))
            .expect("fresh MEM5 record");
        assert_eq!(record.len(), 1756);
        let words = record
            .chunks_exact(4)
            .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(words[0], 0x4d454d35);
        assert_eq!(words[1], 0);
        assert_eq!(words[3], words[4]);
        assert_eq!(words[11], 0);
        assert!(words[28] > 0);
        assert!(words[44] > 0 && words[44] as usize <= source_lines);
        let stamp = |offset: usize| -> u64 {
            u64::from(words[offset]) * 24 * 60 * 60 * 50
                + u64::from(words[offset + 1]) * 60 * 50
                + u64::from(words[offset + 2])
        };
        let stage_names = [
            "other",
            "package_setup",
            "tokenization",
            "binding_and_raw_records",
            "expression_preparation",
            "runtime_finalization",
        ];
        let stages = stage_names
            .iter()
            .enumerate()
            .map(|(index, name)| {
                let ticks =
                    (u64::from(words[30 + index * 2]) << 32) | u64::from(words[31 + index * 2]);
                (
                    (*name).to_owned(),
                    serde_json::json!({
                        "seconds": ticks as f64 / f64::from(words[28]),
                        "calls": words[42 + index],
                    }),
                )
            })
            .collect::<serde_json::Map<_, _>>();
        Some(serde_json::json!({
            "peak_owned_bytes": words[2],
            "retained_after_preparation_bytes": words[5],
            "packed_source_bytes": words[14],
            "source_bytes": words[15],
            "tokenized_lines": words[44],
            "derived_full_record_inspections": sweeps_per_pass.map(|sweeps| words[44] * sweeps * 2),
            "preparation_stages": stages,
            "preparation_seconds": (stamp(22) - stamp(19)) as f64 / 50.0,
            "assembly_seconds": (stamp(25) - stamp(22)) as f64 / 50.0,
        }))
    } else {
        None
    };
    eprintln!(
        "{} {}",
        report_name,
        serde_json::json!({
            "cpu": cpu,
            "mode": if instrumented { "instrumented_harness" } else { "release_compact_cli" },
            "items_per_import": items_per_import,
            "source_files": files.len(),
            "source_lines": source_lines,
            "source_bytes": files.iter().map(|(_, source)| source.len()).sum::<usize>(),
            "output_bytes": expected.len(),
            "package_bytes": package.len(),
            "rust_oracle_host_seconds": rust_seconds,
            "guest_start_to_done_host_seconds": run.start_to_done_host_seconds,
            "image_bytes": image.len(),
            "linked_reserved_bytes": allocation.total(),
            "memory": memory,
        })
    );
}
