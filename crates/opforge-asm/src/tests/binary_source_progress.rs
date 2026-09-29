//! Interpret the compact CLI's gated textual failure-position capture.
use std::collections::BTreeMap;

pub(super) fn failure_position(stdout: &str) -> Option<serde_json::Value> {
    let mut events = BTreeMap::new();
    for line in stdout
        .lines()
        .filter(|line| line.starts_with("progress p="))
    {
        let fields = line.split_ascii_whitespace().collect::<Vec<_>>();
        assert_eq!(fields.len(), 6, "malformed progress line: {line}");
        let words = ["p=", "f=", "l=", "r=", "m="]
            .into_iter()
            .zip(&fields[1..])
            .map(|(prefix, field)| {
                let hex = field.strip_prefix(prefix).expect("progress field prefix");
                assert_eq!(hex.len(), 8, "progress hex width");
                u32::from_str_radix(hex, 16).expect("progress hex value")
            })
            .collect::<Vec<_>>();
        if (26..=28).contains(&words[0]) {
            assert!(events.insert(words[0], words[1..4].to_vec()).is_none());
        }
    }
    if events.is_empty() {
        return None;
    }
    let position = events.get(&26).expect("failure position capture");
    let records = events.get(&27).expect("failure record capture");
    let sections = events.get(&28).expect("failure section capture");
    let [pass, sweep, section] = position[..] else {
        unreachable!()
    };
    let [offset, total, _] = records[..] else {
        unreachable!()
    };
    let [mode, count, _] = sections[..] else {
        unreachable!()
    };
    assert!(pass <= 2, "bounded assembly pass");
    assert!(
        pass == 0 || total > 0,
        "active pass requires a record buffer"
    );
    let offset = (offset != u32::MAX).then_some(offset);
    if let Some(offset) = offset {
        assert!(offset <= total, "record offset exceeds buffer");
    }
    let section_sweep = (mode == 5 && sweep >= 8 && sweep - 8 < count).then(|| sweep - 8 + 1);
    Some(serde_json::json!({
        "pass": pass,
        "raw_sweep": sweep,
        "section_mode": mode,
        "section_sweep": section_sweep,
        "section_sweeps": (mode == 5).then_some(count),
        "section_id": section_sweep.map(|_| section),
        "record_offset_bytes": offset,
        "record_total_bytes": total,
        "record_buffer_percent": offset.filter(|_| total != 0)
            .map(|offset| f64::from(offset) * 100.0 / f64::from(total)),
    }))
}

#[test]
fn compact_failure_position_decodes_sweep_and_byte_position() {
    let stdout = "progress p=0000001a f=00000001 l=00000009 r=00000003 m=00000000\n\
progress p=0000001b f=00000080 l=00000100 r=00000020 m=00000000\n\
progress p=0000001c f=00000005 l=00000004 r=00000000 m=00000000\n";
    let report = failure_position(stdout).unwrap();
    assert_eq!(report["pass"], 1);
    assert_eq!(report["section_sweep"], 2);
    assert_eq!(report["section_id"], 3);
    assert_eq!(report["section_sweeps"], 4);
    assert_eq!(report["record_buffer_percent"], 50.0);
    let outside = stdout.replace("l=00000009", "l=00000000");
    assert!(failure_position(&outside).unwrap()["section_id"].is_null());
    let before_records = stdout.replace("f=00000080", "f=ffffffff");
    assert!(failure_position(&before_records).unwrap()["record_buffer_percent"].is_null());
    assert!(failure_position("ordinary release diagnostic").is_none());
}

#[test]
#[should_panic(expected = "record offset exceeds buffer")]
fn compact_failure_position_rejects_impossible_capture() {
    failure_position(
        "progress p=0000001a f=00000001 l=00000008 r=00000001 m=00000000\n\
progress p=0000001b f=00000101 l=00000100 r=00000000 m=00000000\n\
progress p=0000001c f=00000005 l=00000004 r=00000000 m=00000000\n",
    );
}

#[test]
#[ignore = "requires FS-UAE with memory/progress gates; relocated snapshot fields"]
fn compact_assembly_position_failure_fs_uae() {
    use super::*;
    let source = ".module probe\n.cpu m68020\n.section entry, kind=code\n nop\n.endsection\n.section code, kind=code\n move.w 8(a2), (a1)\n.endsection\n.section data, kind=data\n.byte 7\n.endsection\n.section bss, kind=bss\n.res long, 1\n.endsection\n.output \"probe.hunk\", format=hunk, sections=entry,code,data,bss\n.endmodule\n.end\n";
    // Rust accepts the same input; this probe isolates the compact instruction
    // rejection and observes position, rather than claiming artifact parity.
    let dir = create_temp_dir("assembly-position-oracle");
    let input = dir.join("input.asm");
    fs::write(&input, source).unwrap();
    let cli = Cli::parse_from(["opforge", input.to_str().unwrap()]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    assert!(!fs::read(dir.join("probe.hunk")).unwrap().is_empty());
    fs::remove_dir_all(dir).unwrap();
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let result = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        None,
    )
    .expect("fresh positioned native rejection");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(20));
    let report = failure_position(&runs[0].stdout).expect("gated assembly snapshot");
    assert_eq!(report["pass"], 1);
    assert_eq!(report["section_mode"], 5);
    assert_eq!(report["section_sweep"], 2);
    assert_eq!(report["section_sweeps"], 4);
    assert!(report["record_offset_bytes"].as_u64().unwrap() > 0);
    assert!(report["record_buffer_percent"].as_f64().unwrap() < 100.0);
    eprintln!("COMPACT_ASSEMBLY_POSITION {report}");
}

#[test]
#[should_panic(expected = "active pass requires a record buffer")]
fn compact_failure_position_rejects_unrelocated_zero_fields() {
    failure_position(
        "progress p=0000001a f=00000001 l=00000009 r=00000000 m=00000000\n\
progress p=0000001b f=00000000 l=00000000 r=00000000 m=00000000\n\
progress p=0000001c f=00000000 l=00000000 r=00000000 m=00000000\n",
    );
}
