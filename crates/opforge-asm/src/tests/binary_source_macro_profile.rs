//! Decode the gated MEM8 accounting for compact macro comparisons.
use super::*;

pub(super) fn report(run: &crate::fs_uae_smoke::FsUaeSmokeRun) -> serde_json::Value {
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() != Ok("1") {
        assert!(!run
            .captured_artifacts
            .contains_key(&PathBuf::from("Work/memory.bin")));
        return serde_json::Value::Null;
    }
    let record = &run.captured_artifacts[&PathBuf::from("Work/memory.bin")];
    assert_eq!(record.len(), 2100);
    let words = record
        .chunks_exact(4)
        .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap()))
        .collect::<Vec<_>>();
    assert_eq!(words[0], 0x4d454d38);
    assert_eq!(words[1], 0, "all owned blocks released");
    assert_eq!(words[3], words[4], "allocation/free capacities balance");
    assert_eq!(words[11], 0);
    assert_eq!(words[29], 0, "profiling completed without errors");
    assert!(words[28] > 0);
    let stamp = |offset: usize| {
        u64::from(words[offset]) * 24 * 60 * 60 * 50
            + u64::from(words[offset + 1]) * 60 * 50
            + u64::from(words[offset + 2])
    };
    let preparation = stamp(22).checked_sub(stamp(19)).unwrap() as f64 / 50.0;
    let assembly = stamp(25).checked_sub(stamp(22)).unwrap() as f64 / 50.0;
    let names = [
        "other",
        "package_setup",
        "tokenization",
        "binding_and_raw_records",
        "expression_preparation",
        "runtime_finalization",
    ];
    let ticks = |offset: usize| (u64::from(words[offset]) << 32) | u64::from(words[offset + 1]);
    let stages = names
        .iter()
        .enumerate()
        .map(|(index, name)| {
            (
                (*name).to_owned(),
                serde_json::json!({
                    "seconds": ticks(30 + index * 2) as f64 / f64::from(words[28]),
                    "entries": words[42 + index],
                }),
            )
        })
        .collect::<serde_json::Map<_, _>>();
    let total = (0..6).map(|i| ticks(30 + i * 2)).sum::<u64>() as f64 / f64::from(words[28]);
    assert!(
        (total - preparation).abs() <= 0.04,
        "stages reconcile with preparation clock"
    );
    let opcode_total = words[48..69].iter().map(|v| u64::from(*v)).sum::<u64>();
    let pair_total = words[69..510].iter().map(|v| u64::from(*v)).sum::<u64>();
    assert_eq!(pair_total + u64::from(words[48]), opcode_total);
    serde_json::json!({
        "peak_owned_bytes": words[2], "total_allocated_bytes": words[3],
        "prepared_live_bytes": words[5], "assembly_live_bytes": words[10],
        "instrumented_preparation_seconds": preparation,
        "instrumented_assembly_seconds": assembly,
        "preparation_stages": stages,
        "tokenizer": {
            "completed_invocations": words[48], "opcode_total": opcode_total,
            "line_bytes": words[510], "committed_tokens": words[511],
            "committed_lexeme_bytes": words[512], "source_reads": words[513],
            "helpers_seconds": ticks(517) as f64 / f64::from(words[28]),
            "commit_seconds": ticks(519) as f64 / f64::from(words[28]),
        },
        "profiling_errors": words[29],
    })
}
