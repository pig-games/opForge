//! Placement changes reported addresses, while Hunk payloads remain section relative.
use super::*;

fn cases() -> Vec<(&'static str, String, String)> {
    let address = source(".section code,kind=code\nentry\n.long $,entry+3,payload+1\n move.l #$+3,d0\n move.l payload+2,d1\n bra.w end\n bra.w $+4\nend\n rts\n.endsection\n.section data,kind=data\npayload .long entry+1\n.endsection\n", "code,data");
    let shared = source(".section code,kind=code,align=4\nentry .long payload+1,$\n.endsection\n.section data,kind=data,align=16\npayload .byte $aa,$bb,$cc\n.endsection\n", "code,data");
    let bss = source(".section code,kind=code\nentry .long reserved+1,$\n.endsection\n.section bss,kind=bss,align=16\n.res byte,3\nreserved .res byte,5\n.endsection\n", "code,bss");
    let mapped = mapped_hunk_cases()
        .into_iter()
        .find(|(name, _)| *name == "mapped-pc-block")
        .unwrap()
        .1;
    [
        (
            "placed-instruction-negative-addend",
            source(
                ".section code,kind=code\nentry move.l entry-1,d0\n.endsection\n",
                "code",
            ),
            ".region ram,$2000,$20ff\n.place code in ram\n",
        ),
        (
            "placed-addresses",
            address,
            ".region ram,$2000,$20ff\n.place code in ram\n.place data in ram\n",
        ),
        (
            "placed-shared-alignment",
            shared.clone(),
            ".region ram,$2001,$20ff\n.place code in ram\n.place data in ram\n",
        ),
        (
            "placed-reverse-order",
            shared.clone(),
            ".region ram,$2001,$20ff\n.place data in ram\n.place code in ram\n",
        ),
        (
            "placed-directive-alignment",
            shared,
            ".region ram,$2001,$20ff,align=4\n.place code in ram,align=16\n.place data in ram\n",
        ),
        (
            "placed-bss",
            bss,
            ".region ram,$2000,$20ff\n.place code in ram\n.place bss in ram\n",
        ),
        (
            "placed-mapped-block",
            mapped,
            ".region ram,$2000,$20ff\n.place code in ram\n.place data in ram\n",
        ),
    ]
    .into_iter()
    .map(|(name, unplaced, placement)| {
        let placed = unplaced.replacen(".output", &format!("{placement}.output"), 1);
        (name, unplaced, placed)
    })
    .collect()
}

#[test]
fn compact_hunk_placement_rust_oracles() {
    let dir = create_temp_dir("hunk-placement-oracles");
    let _cleanup = Cleanup(dir.clone());
    for (name, unplaced, placed) in cases() {
        let reference = project_oracle(&dir, &unplaced, true, &[]).unwrap();
        let bytes = project_oracle(&dir, &placed, true, &[])
            .unwrap_or_else(|error| panic!("{name}: {error}"));
        assert_eq!(
            bytes, reference,
            "{name}: placement must preserve all Hunk bytes and relocations"
        );
        let parts = hunk::segments(&bytes).unwrap();
        let expected = match name {
            "placed-instruction-negative-addend" => vec![vec![(2, 0)]],
            "placed-addresses" => {
                vec![vec![(0, 0), (4, 0), (14, 0), (8, 1), (20, 1)], vec![(0, 0)]]
            }
            "placed-shared-alignment" | "placed-reverse-order" | "placed-directive-alignment" => {
                vec![vec![(4, 0), (0, 1)], vec![]]
            }
            "placed-bss" => vec![vec![(4, 0), (0, 1)], vec![]],
            "placed-mapped-block" => vec![vec![(0, 1)], vec![(4, 1), (8, 1)]],
            _ => unreachable!(),
        };
        assert_eq!(
            parts
                .iter()
                .map(|part| part.relocations.clone())
                .collect::<Vec<_>>(),
            expected,
            "{name}"
        );
        if name == "placed-instruction-negative-addend" {
            assert_eq!(parts[0].payload, [0x20, 0x39, 0xff, 0xff, 0xff, 0xff, 0, 0]);
        }
        if name == "placed-addresses" {
            assert_eq!(
                parts[0].payload,
                [
                    0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 1, 0x20, 0x3c, 0, 0, 0, 15, 0x22, 0x39, 0, 0,
                    0, 2, 0x60, 0, 0, 6, 0x60, 0, 0, 2, 0x4e, 0x75, 0, 0,
                ]
            );
            assert_eq!(parts[1].payload, [0, 0, 0, 1]);
        }
        if matches!(name, "placed-shared-alignment" | "placed-reverse-order") {
            assert_eq!(parts[0].payload, [0, 0, 0, 1, 0, 0, 0, 0]);
            assert_eq!(parts[1].payload, [0xaa, 0xbb, 0xcc, 0]);
        }
        if name == "placed-bss" {
            assert_eq!(parts[1].kind, 0x3eb);
            assert_eq!(parts[1].reserved_bytes, 8);
            assert!(parts[1].payload.is_empty());
            assert_eq!(parts[0].payload, [0, 0, 0, 4, 0, 0, 0, 0]);
        }
    }
}

#[test]
fn compact_hunk_placement_reported_labels() {
    for (name, _, placed) in cases()
        .into_iter()
        .filter(|(name, _, _)| *name != "placed-mapped-block")
    {
        let lines = placed.lines().map(str::to_owned).collect::<Vec<_>>();
        let mut assembler = Assembler::new();
        let pass = assembler.pass1(&lines);
        assert_eq!(pass.errors, 0, "{name}: {:?}", assembler.diagnostics);
        let expected = match name {
            "placed-instruction-negative-addend" => vec![("entry", 0x2000)],
            "placed-addresses" => vec![("entry", 0x2000), ("end", 0x2020), ("payload", 0x2022)],
            "placed-shared-alignment" => vec![("entry", 0x2004), ("payload", 0x2010)],
            "placed-reverse-order" => vec![("entry", 0x2014), ("payload", 0x2010)],
            "placed-directive-alignment" => vec![("entry", 0x2010), ("payload", 0x2020)],
            "placed-bss" => vec![("entry", 0x2000), ("reserved", 0x2013)],
            _ => unreachable!(),
        };
        for (label, value) in expected {
            assert_eq!(
                assembler
                    .symbols
                    .entry(&format!("probe.{label}"))
                    .map(|entry| entry.val),
                Some(value),
                "{name}/{label}"
            );
        }
    }
}

fn invalid_cases() -> Vec<(&'static str, String, &'static str)> {
    let body = ".section code,kind=code\n.long 0,0\n.endsection\n";
    [
        (
            "placed-overlap",
            ".region ram,$2000,$20ff\n.region other,$2080,$21ff\n.place code in ram\n",
            "Region range overlaps existing region",
        ),
        (
            "placed-overflow",
            ".region ram,$2000,$2006\n.place code in ram\n",
            "Section placement overflows region",
        ),
        (
            "placed-duplicate",
            ".region ram,$2000,$20ff\n.place code in ram\n.place code in ram\n",
            "Section has already been placed",
        ),
    ]
    .into_iter()
    .map(|(name, placement, diagnostic)| {
        (
            name,
            source(&format!("{body}{placement}"), "code"),
            diagnostic,
        )
    })
    .collect()
}

#[test]
fn compact_hunk_placement_invalid_rust_oracles() {
    let dir = create_temp_dir("hunk-placement-invalid");
    let _cleanup = Cleanup(dir.clone());
    for (name, source, diagnostic) in invalid_cases() {
        let error = project_oracle(&dir, &source, true, &[]).expect_err(name);
        assert!(error.contains(diagnostic), "{name}: {error}");
    }
}

#[test]
#[ignore = "requires fresh FS-UAE explicit placement Hunk artifact parity"]
fn compact_hunk_placement_fs_uae() {
    native_expected_cases(
        cases()
            .into_iter()
            .map(|(name, _, source)| (name.into(), "m68020", source, NativeExpected::MatchHunk))
            .collect(),
        "OPFORGE_HUNK_PLACEMENT_REPORT",
    );
}

#[test]
#[ignore = "requires fresh FS-UAE invalid placement completion and rejection"]
fn compact_hunk_placement_invalid_fs_uae() {
    native_expected_cases(
        invalid_cases()
            .into_iter()
            .map(|(name, source, _)| {
                (
                    name.into(),
                    "m68020",
                    source,
                    NativeExpected::RejectInvalidHunk,
                )
            })
            .collect(),
        "OPFORGE_HUNK_PLACEMENT_REPORT",
    );
}

fn odd_origin_source(placed: bool) -> String {
    source(
        &format!(
            ".section code,kind=code\nentry .byte 1\n.align 4\naligned .byte 2\n.endsection\n{}",
            if placed {
                ".region ram,$2001,$20ff\n.place code in ram\n"
            } else {
                ""
            }
        ),
        "code",
    )
}

#[test]
fn compact_hunk_placement_odd_origin_alignment_rust_oracle() {
    let dir = create_temp_dir("hunk-placement-odd-align");
    let _cleanup = Cleanup(dir.clone());
    for placed in [false, true] {
        let source = odd_origin_source(placed);
        let bytes = project_oracle(&dir, &source, true, &[]).unwrap();
        let parts = hunk::segments(&bytes).unwrap();
        assert_eq!(
            parts[0].payload,
            if placed {
                vec![1, 0, 0, 2]
            } else {
                vec![1, 0, 0, 0, 2, 0, 0, 0]
            }
        );
        assert_eq!(parts[0].reserved_bytes, 8);
        let mut assembler = Assembler::new();
        let lines = source.lines().map(str::to_owned).collect::<Vec<_>>();
        assert_eq!(assembler.pass1(&lines).errors, 0);
        assert_eq!(
            assembler.symbols.entry("probe.aligned").unwrap().val,
            if placed { 0x2004 } else { 4 }
        );
    }
}

fn label_artifacts(dir: &Path, source: &str) -> (Vec<u8>, Vec<u8>) {
    fs::create_dir_all(dir).unwrap();
    let input = dir.join("main.asm");
    let labels = dir.join("labels.txt");
    fs::write(&input, source).unwrap();
    let cli = Cli::parse_from([
        "opForge",
        input.to_str().unwrap(),
        "--labels",
        labels.to_str().unwrap(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    (
        fs::read(dir.join("out.hunk")).unwrap(),
        fs::read(labels).unwrap(),
    )
}

#[test]
fn compact_hunk_placement_cli_labels_rust_oracles() {
    let dir = create_temp_dir("hunk-placement-cli-labels");
    let _cleanup = Cleanup(dir.clone());
    for (name, _, source) in cases() {
        let (_, labels) = label_artifacts(&dir.join(name), &source);
        let labels = String::from_utf8(labels).unwrap();
        let expected: &[&str] = match name {
            "placed-instruction-negative-addend" => &["probe.entry = $2000"],
            "placed-addresses" => &[
                "probe.entry = $2000",
                "probe.end = $2020",
                "probe.payload = $2022",
            ],
            "placed-mapped-block" => &["pc_dep.entry = $2008"],
            "placed-shared-alignment" => &["probe.entry = $2004", "probe.payload = $2010"],
            "placed-reverse-order" => &["probe.entry = $2014", "probe.payload = $2010"],
            "placed-directive-alignment" => &["probe.entry = $2010", "probe.payload = $2020"],
            "placed-bss" => &["probe.entry = $2000", "probe.reserved = $2013"],
            _ => unreachable!(),
        };
        for expected in expected {
            assert!(
                labels.lines().any(|line| line == *expected),
                "{name}: missing {expected}: {labels}"
            );
        }
    }
}

#[test]
#[ignore = "requires fresh FS-UAE odd placed origin internal alignment Hunk parity"]
fn compact_hunk_placement_odd_origin_alignment_fs_uae() {
    native_expected_cases(
        [false, true]
            .into_iter()
            .map(|placed| {
                (
                    format!("odd-origin-align/{placed}"),
                    "m68020",
                    odd_origin_source(placed),
                    NativeExpected::MatchHunk,
                )
            })
            .collect(),
        "OPFORGE_HUNK_PLACEMENT_ALIGNMENT_REPORT",
    );
}

#[test]
#[ignore = "two fresh release timings for placed scalar snapshot Hunk input"]
fn compact_hunk_placement_measurement_fs_uae() {
    let source = scalar_snapshot_measurement().replacen(
        ".output",
        ".region ram,$2000,$ffff\n.place code in ram\n.place data in ram\n.output",
        1,
    );
    native_expected_cases(
        (0..2)
            .map(|round| {
                (
                    format!("placed-scalar-snapshots/{round}"),
                    "m68020",
                    source.clone(),
                    NativeExpected::MatchHunk,
                )
            })
            .collect(),
        "OPFORGE_HUNK_PLACEMENT_MEASUREMENT_REPORT",
    );
}

fn high_water_cases() -> Vec<(&'static str, String)> {
    let following = source(".section code,kind=code\nentry .byte 1\n.align 4\naligned .byte 2\n.endsection\n.section data,kind=data\npayload .byte 3\n.endsection\n.region ram,$2001,$20ff\n.place code in ram\n.place data in ram\n", "code,data");
    let mapped = concat!(
        ".module main\n.cpu m68020\n",
        ".use placement_dep (entry) as dep map { logical_code -> code }\n",
        ".section code,kind=code\nstart .byte 1\n.align 4\naligned .byte 2\n.endsection\n",
        ".section data,kind=data\n.long dep.entry\n.endsection\n",
        ".region ram,$2001,$20ff\n.place code in ram\n",
        ".output \"out.hunk\",format=hunk,sections=code,data\n.endmodule\n",
        ".module placement_dep\n.cpu m68020\n.pub\n",
        ".section logical_code,kind=code,logical\nentry .block\n.long $\n.bend\n.endsection\n.endmodule\n",
    ).into();
    vec![
        ("odd-align-following-section", following),
        ("odd-align-mapped-fragment", mapped),
    ]
}

#[test]
fn compact_hunk_placement_high_water_rust_oracles() {
    let dir = create_temp_dir("hunk-placement-high-water");
    let _cleanup = Cleanup(dir.clone());
    for (name, source) in high_water_cases() {
        let (bytes, labels) = label_artifacts(&dir.join(name), &source);
        let parts = hunk::segments(&bytes).unwrap();
        let labels = String::from_utf8(labels).unwrap();
        match name {
            "odd-align-following-section" => {
                assert_eq!(parts[0].payload, [1, 0, 0, 2]);
                assert_eq!(parts[0].reserved_bytes, 8);
                assert_eq!(parts[1].payload, [3, 0, 0, 0]);
                assert_eq!(parts[1].reserved_bytes, 4);
                assert!(parts.iter().all(|part| part.relocations.is_empty()));
                for label in [
                    "probe.entry = $2001",
                    "probe.aligned = $2004",
                    "probe.payload = $2006",
                ] {
                    assert!(
                        labels.lines().any(|line| line == label),
                        "{name}: missing {label}: {labels}"
                    );
                }
            }
            "odd-align-mapped-fragment" => {
                assert_eq!(parts[0].payload, [1, 0, 0, 2, 0, 0, 0, 0, 5, 0, 0, 0]);
                assert_eq!(parts[0].reserved_bytes, 12);
                assert_eq!(parts[0].relocations, [(5, 0)]);
                assert_eq!(parts[1].payload, [0, 0, 0, 5]);
                assert_eq!(parts[1].reserved_bytes, 4);
                assert_eq!(parts[1].relocations, [(0, 0)]);
                for label in [
                    "main.start = $2001",
                    "main.aligned = $2004",
                    "placement_dep.entry = $2006",
                ] {
                    assert!(
                        labels.lines().any(|line| line == label),
                        "{name}: missing {label}: {labels}"
                    );
                }
            }
            _ => unreachable!(),
        }
    }
}

#[test]
#[ignore = "requires fresh FS-UAE odd-origin allocation and following placement/mapping Hunk parity"]
fn compact_hunk_placement_high_water_fs_uae() {
    native_expected_cases(
        high_water_cases()
            .into_iter()
            .map(|(name, source)| (name.into(), "m68020", source, NativeExpected::MatchHunk))
            .collect(),
        "OPFORGE_HUNK_PLACEMENT_HIGH_WATER_REPORT",
    );
}
