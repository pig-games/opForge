//! Bounded two-map scaling case; the same generated sources feed both oracles.
use super::*;

const ITEMS_PER_IMPORT: usize = 64;

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
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(&cpu, None).unwrap();
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
            Some(&expected),
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
            Some(&expected),
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
    let source_lines = sources
        .iter()
        .map(|source| source.lines().count())
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
            "derived_full_record_inspections": words[44] * 5 * 2,
            "preparation_stages": stages,
            "preparation_seconds": (stamp(22) - stamp(19)) as f64 / 50.0,
            "assembly_seconds": (stamp(25) - stamp(22)) as f64 / 50.0,
        }))
    } else {
        None
    };
    eprintln!(
        "TWO_MAP_MEASUREMENT {}",
        serde_json::json!({
            "cpu": cpu,
            "mode": if instrumented { "instrumented_harness" } else { "release_compact_cli" },
            "items_per_import": ITEMS_PER_IMPORT,
            "source_files": files.len(),
            "source_lines": source_lines,
            "source_bytes": sources.iter().map(String::len).sum::<usize>(),
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
