// SPDX-License-Identifier: GPL-3.0-or-later

use super::prepared_source_experiment::PreparedSourceExperiment;
use crate::engine::Assembler;
use crate::listing::ListingWriter;
use families::{
    register_intel8080_family_stack, register_mos6502_family_stack,
    register_motorola68000_family_stack, register_motorola6800_family_stack,
};
use registry::cpu::CpuType;
use registry::registry::ModuleRegistry;
use std::time::Instant;
use vm::runtime_model_core::RuntimeModelCore;

fn registry() -> ModuleRegistry {
    let mut registry = ModuleRegistry::new();
    register_intel8080_family_stack(&mut registry);
    register_mos6502_family_stack(&mut registry);
    register_motorola68000_family_stack(&mut registry);
    register_motorola6800_family_stack(&mut registry);
    registry
}

fn ordinary_assembly(cpu: &str, source: &str, origin: u32) -> Vec<(u32, u8)> {
    let registry = registry();
    let cpu = match cpu {
        "m6502" => "m6502",
        "8085" => "8085",
        "m68000" => "m68000",
        _ => panic!("unsupported test CPU"),
    };
    let mut assembler = Assembler::with_cpu_and_registry(CpuType::new(cpu), registry);
    let mut lines = vec![format!(".org ${origin:x}")];
    lines.extend(source.lines().map(str::to_string));
    let pass1 = assembler.pass1(&lines);
    assert_eq!(
        pass1.errors, 0,
        "oracle pass1 diagnostics: {:?}",
        assembler.diagnostics
    );
    let mut listing_bytes = Vec::new();
    let mut listing = ListingWriter::new(&mut listing_bytes, false);
    let pass2 = assembler.pass2(&lines, &mut listing).expect("oracle pass2");
    assert_eq!(
        pass2.errors, 0,
        "oracle pass2 diagnostics: {:?}",
        assembler.diagnostics
    );
    assembler.image().entries().expect("oracle image entries")
}

fn prepared_assembly(cpu: &str, source: &str) -> PreparedSourceExperiment {
    let registry = registry();
    let model = RuntimeModelCore::from_registry(&registry).expect("package model");
    let resolved = model.resolve_pipeline(cpu, None).expect("resolve test CPU");
    let prepared = PreparedSourceExperiment::prepare(source, &model, &resolved)
        .unwrap_or_else(|error| panic!("prepare {cpu} source: {error}"));
    assert!(prepared.retained_bytes() > 0);
    prepared
}

#[test]
fn complete_source_replays_package_program_and_forward_data_at_fresh_origins() {
    const SOURCE: &str =
        "start:\n  NOP\n.byte later - start\n.word later + 1\nlater:\n.byte later - start\n";
    for cpu in ["m6502", "m68000"] {
        let source = String::from(SOURCE);
        let prepared = prepared_assembly(cpu, &source);
        drop(source);
        assert_eq!(
            prepared.program_count(),
            1,
            "repeated NOP should deduplicate"
        );
        for origin in [0x100, 0x230] {
            let actual = prepared.run(origin).expect("prepared source replay");
            let expected = ordinary_assembly(cpu, SOURCE, origin);
            assert_eq!(
                actual, expected,
                "prepared/oracle mismatch for {cpu} at ${origin:x}"
            );
        }
        let first = prepared.run(0x100).unwrap();
        let second = prepared.run(0x230).unwrap();
        let first_bytes = first.iter().map(|(_, byte)| *byte).collect::<Vec<_>>();
        let second_bytes = second.iter().map(|(_, byte)| *byte).collect::<Vec<_>>();
        let nop_bytes = if cpu == "m68000" { 2 } else { 1 };
        assert_eq!(
            first_bytes[nop_bytes], second_bytes[nop_bytes],
            "relative forward byte remains origin independent for {cpu}"
        );
        assert_eq!(
            first_bytes[nop_bytes + 3],
            second_bytes[nop_bytes + 3],
            "relative data after the word remains origin independent for {cpu}"
        );
        assert_ne!(
            &first_bytes[nop_bytes + 1..nop_bytes + 3],
            &second_bytes[nop_bytes + 1..nop_bytes + 3],
            "absolute forward word must be recomputed for {cpu}"
        );
        assert_eq!(first.first().map(|(address, _)| *address), Some(0x100));
        assert_eq!(second.first().map(|(address, _)| *address), Some(0x230));
    }
}

#[test]
fn word_endianness_comes_from_package_cpu_properties() {
    let source = ".word $1234\n";
    let little = prepared_assembly("8085", source).run(0).unwrap();
    let big = prepared_assembly("m68000", source).run(0).unwrap();
    assert_eq!(little, vec![(0, 0x34), (1, 0x12)]);
    assert_eq!(big, vec![(0, 0x12), (1, 0x34)]);
}

#[test]
fn prepared_source_telemetry_separates_prepare_and_replay_when_enabled() {
    let _session = types::vm_work::install();
    let source =
        String::from("start:\n  NOP\n.byte later - start\n.word later + 1\nlater:\n  NOP\n");
    let registry = registry();
    let model = RuntimeModelCore::from_registry(&registry).expect("package model");
    let resolved = model.resolve_pipeline("m6502", None).unwrap();
    let prepared = {
        let _phase = types::vm_work::phase("prepared_source_prepare");
        PreparedSourceExperiment::prepare(&source, &model, &resolved).unwrap()
    };
    drop(source);
    drop(resolved);
    drop(model);
    drop(registry);
    {
        let _phase = types::vm_work::phase("prepared_source_replay");
        prepared.run(0x100).unwrap();
    }
    let report = types::vm_work::snapshot().expect("installed telemetry session");
    let events = report["events"].as_array().expect("event list");
    let count = |phase: &str, label: &str| {
        events
            .iter()
            .find(|event| event["phase"] == phase && event["label"] == label)
            .and_then(|event| event["count"].as_u64())
            .unwrap_or(0)
    };
    if vm::prepared_encoding::TELEMETRY_ENABLED {
        assert_eq!(
            count("prepared_source_prepare", "prepared.source.name_lookup"),
            4
        );
        assert_eq!(
            count(
                "prepared_source_prepare",
                "prepared.source.symbol_name_lookup"
            ),
            5
        );
        for label in [
            "prepared.source.line_parse",
            "prepared.source.name_lookup",
            "prepared.source.symbol_name_lookup",
            "package.name_lookup",
        ] {
            assert_eq!(
                count("prepared_source_replay", label),
                0,
                "{label} after freeze"
            );
        }
        assert_eq!(
            count("prepared_source_replay", "prepared.source.record_visit"),
            12
        );
        assert_eq!(
            count("prepared_source_replay", "prepared.source.symbol_id_load"),
            3
        );
        assert_eq!(
            count("prepared_source_replay", "prepared.source.expression_op"),
            6
        );
        assert_eq!(count("prepared_source_prepare", "package.name_lookup"), 5);
        assert_eq!(report["overflow"], false);
    }
    let execution_calls = report["rows"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|row| row["phase"] == "prepared_source_replay")
        .map(|row| row["calls"].as_u64().unwrap_or(0))
        .sum::<u64>();
    assert_eq!(
        execution_calls, 4,
        "two NOPs execute during layout and emission"
    );
}

fn benchmark_source(blocks: usize) -> String {
    let mut source = String::new();
    for index in 0..blocks {
        source.push_str(&format!(
            "start{index}:\n  NOP\n.byte end{index} - start{index}\n.word end{index} + 1\nend{index}:\n  NOP\n"
        ));
    }
    source
}

#[test]
#[ignore = "bounded raw-layout decoding probe, not an assembly benchmark"]
fn prepared_source_packed_benchmark() {
    use super::prepared_source_experiment::{PackedLayout, RecordAlignment, TokenWidth};
    for blocks in [8usize, 32] {
        let source = benchmark_source(blocks);
        for cpu in ["m6502", "m68000"] {
            let prepared = prepared_assembly(cpu, &source);
            for alignment in [RecordAlignment::BytePacked, RecordAlignment::WordAligned] {
                for token_width in [TokenWidth::Byte, TokenWidth::Word] {
                    let report = prepared
                        .packed_probe(
                            PackedLayout {
                                token_width,
                                alignment,
                            },
                            256,
                        )
                        .unwrap();
                    println!("packed cpu={cpu} blocks={blocks} source_bytes={} token={token_width:?} alignment={alignment:?} packed_bytes={} records={} expressions={} pack_us={:.3} decode_scan_us={:.3} scans={} checksum={:x}", source.len(), report.packed_bytes, report.line_records, report.expression_records, report.pack_time.as_secs_f64()*1e6, report.decode_time.as_secs_f64()*1e6 / report.decode_iterations as f64, report.decode_iterations, report.checksum);
                }
            }
        }
    }
}

// Medians of independent samples; timing and telemetry are deliberately separate.
fn median_us(samples: &mut [f64]) -> f64 {
    samples.sort_by(f64::total_cmp);
    samples[samples.len() / 2]
}

#[test]
#[ignore = "bounded manual source preparation/replay comparison"]
fn prepared_source_experiment_benchmark() {
    assert!(
        !vm::prepared_encoding::TELEMETRY_ENABLED,
        "measure counters in a separate run"
    );
    let package = vm::builder::build_hierarchy_package_from_registry(&registry()).unwrap();
    let native_package = std::fs::read(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"
    ))
    .unwrap();
    assert_eq!(
        package, native_package,
        "host/native package definitions must match"
    );
    for blocks in [8usize, 32] {
        let source = benchmark_source(blocks);
        for cpu in ["m6502", "m68000"] {
            let mut samples = vec![Vec::new(); 8];
            for sample in 0..5 {
                // Both paths construct the same registry. The bootstrap implementations
                // differ, so report them separately and do not subtract one from the other.
                let mut ordinary = || {
                    let started = Instant::now();
                    let mut assembler =
                        Assembler::with_cpu_and_registry(CpuType::new(cpu), registry());
                    assembler.set_collect_runtime_traces(false);
                    assert!(assembler.prepare_runtime_execution_model());
                    let setup = started.elapsed().as_secs_f64() * 1e6;
                    let started = Instant::now();
                    let mut lines = vec![".org $100".to_string()];
                    lines.extend(source.lines().map(str::to_string));
                    assert_eq!(assembler.pass1(&lines).errors, 0);
                    let pass1 = started.elapsed().as_secs_f64() * 1e6;
                    let started = Instant::now();
                    let mut sink = Vec::new();
                    assert_eq!(
                        assembler
                            .pass2(&lines, &mut ListingWriter::new(&mut sink, false))
                            .unwrap()
                            .errors,
                        0
                    );
                    let output = assembler.image().entries().unwrap();
                    let pass2 = started.elapsed().as_secs_f64() * 1e6;
                    samples[0].push(setup);
                    samples[1].push(pass1);
                    samples[2].push(pass2);
                    samples[3].push(setup + pass1 + pass2);
                    output
                };
                // Alternate ordering to reduce systematic warm-cache/order bias.
                let first = if sample % 2 == 0 {
                    Some(ordinary())
                } else {
                    None
                };
                let started = Instant::now();
                let registry = registry();
                let model = RuntimeModelCore::from_registry(&registry).unwrap();
                let resolved = model.resolve_pipeline(cpu, None).unwrap();
                let setup = started.elapsed().as_secs_f64() * 1e6;
                let started = Instant::now();
                let prepared =
                    PreparedSourceExperiment::prepare(&source, &model, &resolved).unwrap();
                let prepare = started.elapsed().as_secs_f64() * 1e6;
                if sample == 0 {
                    let mut names = std::collections::HashSet::new();
                    for (forms, owner) in [
                        (&model.family_forms, &resolved.family_id),
                        (&model.cpu_forms, &resolved.cpu_id),
                        (&model.dialect_forms, &resolved.dialect_id),
                    ] {
                        if let Some(forms) = forms.get(owner) {
                            names.extend(forms.iter().map(|name| name.to_ascii_lowercase()));
                        }
                    }
                    println!("layout cpu={cpu} blocks={blocks} source_bytes={} retained_bytes={} workspace_bytes={} package_mnemonic_spellings={} normalized_instruction_count=unmeasured", source.len(), prepared.retained_bytes(), prepared.replay_workspace_bytes(), names.len());
                }
                drop(resolved);
                drop(model);
                drop(registry);
                let expected = first.unwrap_or_else(&mut ordinary);
                assert_eq!(prepared.run(0x100).unwrap(), expected);
                let started = Instant::now();
                for _ in 0..128 {
                    std::hint::black_box(prepared.run(0x100).unwrap());
                }
                let replay = started.elapsed().as_secs_f64() * 1e6 / 128.0;
                samples[4].push(setup);
                samples[5].push(prepare);
                samples[6].push(replay);
                samples[7].push(setup + prepare + replay);
            }
            let medians: Vec<_> = samples.iter_mut().map(|values| median_us(values)).collect();
            println!("timing_us cpu={cpu} blocks={blocks} samples=5 ordinary_setup={:.3} ordinary_parse_layout={:.3} ordinary_final_output={:.3} ordinary_total={:.3} prepared_setup={:.3} prepared_parse_bind={:.3} prepared_two_pass_replay={:.3} prepared_total={:.3}", medians[0],medians[1],medians[2],medians[3],medians[4],medians[5],medians[6],medians[7]);
        }
    }
}

#[test]
fn source_errors_are_stable_for_unsupported_duplicate_and_undefined_inputs() {
    let registry = registry();
    let model = RuntimeModelCore::from_registry(&registry).unwrap();
    let resolved = model.resolve_pipeline("m6502", None).unwrap();
    for (source, line, detail) in [
        ("x:\nx:\n", 2, "duplicate symbol id"),
        (".byte missing\n", 1, "undefined symbol id"),
        (
            ".byte first, second\n.byte third\n",
            1,
            "undefined symbol id 0",
        ),
        (".macro m\n", 1, "unsupported directive id"),
        (".cpu m6502\n", 1, "unsupported directive id"),
        ("  LDA #1\n", 1, "has operands"),
    ] {
        let error = match PreparedSourceExperiment::prepare(source, &model, &resolved) {
            Ok(_) => panic!("expected source preparation to fail"),
            Err(error) => error,
        };
        assert_eq!(error.line, line);
        assert!(error.detail.contains(detail), "{error}");
    }
}

#[test]
fn replay_rejects_data_out_of_range_and_invalid_origin() {
    let registry = registry();
    let model = RuntimeModelCore::from_registry(&registry).unwrap();
    let resolved = model.resolve_pipeline("m6502", None).unwrap();
    let byte = PreparedSourceExperiment::prepare(".byte 256\n", &model, &resolved).unwrap();
    assert!(byte
        .run(0)
        .unwrap_err()
        .detail
        .contains("byte value out of range"));
    assert!(byte
        .run(0x1_0000)
        .unwrap_err()
        .detail
        .contains("origin exceeds"));
}
