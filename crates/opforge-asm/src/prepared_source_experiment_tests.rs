// SPDX-License-Identifier: GPL-3.0-or-later

use super::prepared_source_experiment::PreparedSourceExperiment;
use crate::engine::Assembler;
use crate::listing::ListingWriter;
use families::{
    register_intel8080_family_stack, register_mos6502_family_stack,
    register_motorola68000_family_stack,
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

#[test]
#[ignore = "bounded manual source preparation/replay comparison"]
fn prepared_source_experiment_benchmark() {
    for blocks in [8usize, 32] {
        let mut source = String::new();
        for index in 0..blocks {
            source.push_str(&format!(
                "start{index}:\n  NOP\n.byte end{index} - start{index}\n.word end{index} + 1\nend{index}:\n  NOP\n"
            ));
        }
        let source_bytes = source.len();
        for cpu in ["m6502", "m68000"] {
            let registry = registry();
            let model_started = Instant::now();
            let model = RuntimeModelCore::from_registry(&registry).expect("package model");
            let model_time = model_started.elapsed();
            let resolved = model.resolve_pipeline(cpu, None).unwrap();
            let started = Instant::now();
            let prepared = PreparedSourceExperiment::prepare(&source, &model, &resolved)
                .expect("prepare benchmark source");
            let preparation_time = started.elapsed();
            drop(resolved);
            drop(model);
            drop(registry);

            let oracle_started = Instant::now();
            let expected = ordinary_assembly(cpu, &source, 0x100);
            let oracle_time = oracle_started.elapsed();
            let actual = prepared.run(0x100).expect("prepared replay");
            assert_eq!(
                actual, expected,
                "benchmark correctness for {blocks} blocks on {cpu}"
            );
            let replay_started = Instant::now();
            for _ in 0..16 {
                std::hint::black_box(prepared.run(0x100).expect("prepared replay"));
            }
            let replay_16_time = replay_started.elapsed();
            println!(
                "cpu={cpu} blocks={blocks} source_bytes={source_bytes} source_records_bytes={} expression_bytes={} directory_bytes={} replay_workspace_bytes={} retained_total_bytes={} model_build={model_time:?} source_prepare={preparation_time:?} replay_16={replay_16_time:?} ordinary_total={oracle_time:?} ordinary_oracle_includes_model_build=true",
                prepared.retained_source_bytes(),
                prepared.retained_expression_bytes(),
                prepared.retained_encoding_bytes(),
                prepared.replay_workspace_bytes(),
                prepared.retained_bytes(),
            );
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
