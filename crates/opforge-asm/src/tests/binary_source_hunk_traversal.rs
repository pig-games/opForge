//! Hunk serialization order must not reorder statement-time state.
use super::*;

#[path = "binary_source_hunk.rs"]
mod hunk;

fn source(body: &str, order: &str) -> String {
    format!(".module probe\n.cpu m68020\n{body}.output \"out.hunk\",format=hunk,sections={order}\n.endmodule\n")
}

fn cases() -> Vec<(&'static str, String)> {
    vec![
        ("forward-snapshot", source("n .var 3\n.section code,kind=code\nsnapshot .const end-start+n\nstart\n.long snapshot\nend\n.long n\n.endsection\n", "code")),
        ("outside-pc", source(".section code,kind=code\n.long 1\n.endsection\ncursor .var $\n.section data,kind=data\n.long cursor\n.endsection\n", "code,data")),
        ("reopened", hunk_layout_source().into()),
        (
            "unselected",
            source(
                r#"n .var 1
.section hidden,kind=data
n .set 7
inside
.long n
.long inside*2
.endsection
.section code,kind=code
.long n
.endsection
"#,
                "code",
            ),
        ),
        (
            "snapshots-relocations-bss",
            source(
                r#"n .var 1
.section data,kind=data
.long n
n .set n+1
snapshot .const n
.endsection
.section code,kind=code
entry
.long n,snapshot,payload,reserved
 moveq #n,d0
 rts
.endsection
n .set n+1
.section bss,kind=bss
.res byte,n
.align 4
reserved
.res byte,n
.endsection
.section data,kind=data
payload
.long n
.endsection
"#,
                "code,bss,data",
            ),
        ),
        (
            "nested-and-zero-loops",
            source(
                r#"n .var 1
.for 0
.section data,kind=data
n .set 99
.long n
.endsection
.endfor
.for 2
.section data,kind=data
.for 2
.long n
.endfor
.endsection
.section code,kind=code
.long n
.endsection
.endfor
n .set n+1
.section code,kind=code
.long n
.endsection
"#,
                "code,data",
            ),
        ),
    ]
}

#[test]
fn compact_hunk_traversal_rust_oracles() {
    let dir = create_temp_dir("hunk-traversal-oracles");
    let _cleanup = Cleanup(dir.clone());
    for (name, source) in cases() {
        let bytes = project_oracle(&dir, &source, true, &[]).unwrap();
        let segments = hunk::segments(&bytes).unwrap();
        match name {
            "forward-snapshot" => assert_eq!(segments[0].payload, [0, 0, 0, 7, 0, 0, 0, 3]),
            "outside-pc" => assert_eq!(segments[1].payload, 0u32.to_be_bytes()),
            "reopened" => {
                assert_eq!(segments[0].payload, 2u32.to_be_bytes());
                assert_eq!(segments[1].payload, [0, 0, 0, 1, 0, 0, 0, 3]);
            }
            "unselected" => {
                assert_eq!(segments.len(), 1);
                assert_eq!(segments[0].payload, 7u32.to_be_bytes());
            }
            "snapshots-relocations-bss" => {
                assert_eq!(&segments[0].payload[..8], [0, 0, 0, 2, 0, 0, 0, 2]);
                assert_eq!(segments[0].relocations, [(12, 1), (8, 2)]);
                assert_eq!(segments[1].reserved_bytes, 8);
                assert_eq!(segments[2].payload, [0, 0, 0, 1, 0, 0, 0, 3]);
            }
            "nested-and-zero-loops" => {
                assert_eq!(segments[0].payload, [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 2]);
                assert_eq!(
                    segments[1].payload,
                    [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1]
                );
            }
            _ => unreachable!(),
        }
    }
}

#[test]
#[ignore = "requires fresh FS-UAE source-order Hunk traversal and exact live Rust files"]
fn compact_hunk_traversal_fs_uae() {
    native_expected_cases(
        cases()
            .into_iter()
            .map(|(name, source)| (name.into(), "m68020", source, NativeExpected::MatchHunk))
            .collect(),
        "OPFORGE_HUNK_TRAVERSAL_REPORT",
    );
}

fn address_snapshot() -> String {
    source("n .var 1\n.section data,kind=data\npayload\n.long n\nsnapshot .const payload+n\n.endsection\n.section code,kind=code\n.long snapshot\n.endsection\n", "code,data")
}

fn address_alias_cases() -> Vec<(&'static str, String)> {
    vec![
        ("address-snapshot", address_snapshot()),
        (
            "address-snapshot-mutable-addend",
            source(
                "n .var 1\n.section data,kind=data\npayload .long 0\nsnapshot .const payload+n\nalias .const snapshot+2\nn .set 9\n.long alias\n.endsection\n.section code,kind=code\n.long snapshot,alias,n\n.endsection\n",
                "code,data",
            ),
        ),
        (
            "address-alias-cancellation",
            source(
                ".section data,kind=data\nbase .long 0\nend .long 0\nleft .const base+1\nright .const end+3\n.long right-left,left-right\n.endsection\n.section code,kind=code\n.long right-left,left-right\n.endsection\n",
                "code,data",
            ),
        ),
        (
            "address-alias-forward",
            source(
                ".section code,kind=code\nalias .const later+3\n.long alias\n.endsection\n.section data,kind=data\n.long 0\nlater .long 7\n.endsection\n",
                "code,data",
            ),
        ),
        (
            "address-snapshot-pc",
            source(
                ".section data,kind=data\n.long 0\nsnapshot .const $+3\n.long 0\n.long snapshot\n.endsection\n.section code,kind=code\n.long snapshot\n.endsection\n",
                "code,data",
            ),
        ),
        (
            "address-alias-positional-instruction",
            source(
                ".section code,kind=code\nalias .const later\n bra.w alias\n.long 0\nlater rts\n.endsection\n",
                "code",
            ),
        ),
        (
            "address-alias-instructions",
            source(
                ".section data,kind=data\n.long 0\npayload .long 7\nalias .const payload\n.long alias\n.endsection\n.section code,kind=code\n move.l #alias,d0\n lea alias,a0\n.endsection\n",
                "code,data",
            ),
        ),
    ]
}

#[test]
fn compact_hunk_address_alias_rust_oracles() {
    let dir = create_temp_dir("hunk-address-alias-oracles");
    let _cleanup = Cleanup(dir.clone());
    for (name, source) in address_alias_cases() {
        let bytes = project_oracle(&dir, &source, true, &[])
            .unwrap_or_else(|error| panic!("{name}: {error}"));
        let segments = hunk::segments(&bytes).unwrap();
        let words = |values: &[u32]| -> Vec<u8> {
            values.iter().flat_map(|word| word.to_be_bytes()).collect()
        };
        let expected = match name {
            "address-snapshot" => vec![(words(&[1]), vec![(0, 1)]), (words(&[1]), vec![])],
            "address-snapshot-mutable-addend" => vec![
                (words(&[1, 3, 9]), vec![(0, 1), (4, 1)]),
                (words(&[0, 3]), vec![(4, 1)]),
            ],
            "address-alias-cancellation" => vec![
                (words(&[6, (-6i32) as u32]), vec![]),
                (words(&[0, 0, 6, (-6i32) as u32]), vec![]),
            ],
            "address-alias-forward" => vec![(words(&[7]), vec![(0, 1)]), (words(&[0, 7]), vec![])],
            // The snapshot captures DATA offset 4, before either following .long.
            "address-snapshot-pc" => vec![
                (words(&[7]), vec![(0, 1)]),
                (words(&[0, 0, 7]), vec![(8, 1)]),
            ],
            "address-alias-positional-instruction" => {
                vec![(vec![0x60, 0, 0, 6, 0, 0, 0, 0, 0x4e, 0x75, 0, 0], vec![])]
            }
            "address-alias-instructions" => vec![
                (
                    vec![0x20, 0x3c, 0, 0, 0, 4, 0x41, 0xf9, 0, 0, 0, 4],
                    vec![(2, 1), (8, 1)],
                ),
                (words(&[0, 7, 4]), vec![(8, 1)]),
            ],
            _ => unreachable!(),
        };
        assert_eq!(segments.len(), expected.len(), "{name}");
        for (index, (segment, (payload, relocations))) in segments.iter().zip(expected).enumerate()
        {
            assert_eq!(segment.payload, payload, "{name}/{index} payload");
            assert_eq!(
                segment.relocations, relocations,
                "{name}/{index} relocations"
            );
        }
    }
}

#[test]
#[ignore = "requires fresh FS-UAE address alias value and relocation comparison"]
fn compact_hunk_address_snapshot_fs_uae() {
    native_expected_cases(
        address_alias_cases()
            .into_iter()
            .map(|(name, source)| (name.into(), "m68020", source, NativeExpected::MatchHunk))
            .collect(),
        "OPFORGE_HUNK_ADDRESS_ALIAS_REPORT",
    );
}

fn invalid_address_alias_cases() -> Vec<(&'static str, String)> {
    let mut cases: Vec<_> = [
        ("address-alias-multiply", "alias*2"),
        ("address-alias-add-address", "alias+payload"),
        ("address-alias-cross-section-subtract", "alias-entry"),
    ]
    .map(|(name, expression)| {
        (
            name,
            source(
                &format!(".section data,kind=data\npayload .long 0\nalias .const payload\n.endsection\n.section code,kind=code\nentry .long {expression}\n.endsection\n"),
                "code,data",
            ),
        )
    })
    .into();
    cases.extend([
        ("address-alias-hidden-multiply", "bad .const payload*2\n", "bad"),
        ("address-alias-hidden-multiply-chain", "bad .const payload*2\nalias .const bad\n", "alias"),
        ("address-alias-hidden-multiply-cancellation", "bad .const payload*2\nalias .const bad\n", "alias-alias"),
    ].map(|(name, declarations, expression)| {
        (
            name,
            source(
                &format!(".section data,kind=data\npayload .long 0\n{declarations}.endsection\n.section code,kind=code\n.long {expression}\n.endsection\n"),
                "code,data",
            ),
        )
    }));
    cases
}

fn invalid_address_alias_instruction_cases() -> Vec<(&'static str, String)> {
    [
        ("address-alias-hidden-multiply-immediate", " move.l #alias,d0"),
    ]
    .map(|(name, instruction)| {
        (
            name,
            source(
                &format!(".section data,kind=data\npayload .long 0\nbad .const payload*2\nalias .const bad\n.endsection\n.section code,kind=code\n{instruction}\n.endsection\n"),
                "code,data",
            ),
        )
    })
    .into()
}

#[test]
fn compact_hunk_address_alias_invalid_rust_oracles() {
    let dir = create_temp_dir("hunk-address-alias-invalid-oracles");
    let _cleanup = Cleanup(dir.clone());
    for (name, source) in invalid_address_alias_cases()
        .into_iter()
        .chain(invalid_address_alias_instruction_cases())
    {
        assert!(
            project_oracle(&dir, &source, true, &[]).is_err(),
            "{name} must be rejected"
        );
    }
}

#[test]
#[ignore = "requires fresh FS-UAE invalid address arithmetic through aliases"]
fn compact_hunk_address_alias_invalid_fs_uae() {
    native_expected_cases(
        invalid_address_alias_cases()
            .into_iter()
            .chain(invalid_address_alias_instruction_cases())
            .map(|(name, source)| {
                (
                    name.into(),
                    "m68020",
                    source,
                    NativeExpected::RejectInvalidHunk,
                )
            })
            .collect(),
        "OPFORGE_HUNK_ADDRESS_ALIAS_REPORT",
    );
}

fn scalar_snapshot_measurement() -> String {
    let mut body = String::from("n .var 1\n");
    for index in 0..128 {
        body.push_str(&format!(
            ".section data,kind=data\nsnap{index} .const n\nn .set n+1\n.long snap{index},n\n.endsection\n.section code,kind=code\n.long n\n.endsection\n"
        ));
    }
    source(&body, "code,data")
}

#[test]
fn compact_hunk_scalar_snapshot_measurement_rust_oracle() {
    let dir = create_temp_dir("hunk-scalar-snapshot-measurement-oracle");
    let _cleanup = Cleanup(dir.clone());
    let bytes = project_oracle(&dir, &scalar_snapshot_measurement(), true, &[]).unwrap();
    let segments = hunk::segments(&bytes).unwrap();
    let code: Vec<u8> = (2u32..=129).flat_map(u32::to_be_bytes).collect();
    let data: Vec<u8> = (1u32..=128)
        .flat_map(|value| [value, value + 1])
        .flat_map(u32::to_be_bytes)
        .collect();
    assert_eq!(segments.len(), 2);
    assert_eq!(segments[0].payload, code);
    assert_eq!(segments[1].payload, data);
    assert!(segments.iter().all(|part| part.relocations.is_empty()));
}

#[test]
#[ignore = "two fresh release timings for identical mutable scalar snapshot Hunk input"]
fn compact_hunk_scalar_snapshot_measurement_fs_uae() {
    native_expected_cases(
        (0..2)
            .map(|round| {
                (
                    format!("scalar-snapshots/{round}"),
                    "m68020",
                    scalar_snapshot_measurement(),
                    NativeExpected::MatchHunk,
                )
            })
            .collect(),
        "OPFORGE_HUNK_ADDRESS_ALIAS_REPORT",
    );
}

fn readonly_measurement() -> String {
    let mut body = String::new();
    for _ in 0..128 {
        body.push_str(".section data,kind=data\n.long $11223344\n.byte 1,2,3,4\n.endsection\n.section code,kind=code\n moveq #7,d0\n addq.l #1,d0\n move.l d0,d1\n.endsection\n");
    }
    source(&body, "code,data")
}

#[test]
fn compact_hunk_traversal_measurement_rust_oracle() {
    let dir = create_temp_dir("hunk-traversal-measurement-oracle");
    let _cleanup = Cleanup(dir.clone());
    let bytes = project_oracle(&dir, &readonly_measurement(), true, &[]).unwrap();
    let segments = hunk::segments(&bytes).unwrap();
    assert_eq!(segments[0].payload.len(), 128 * 6);
    assert_eq!(segments[1].payload.len(), 128 * 8);
    assert!(segments.iter().all(|part| part.relocations.is_empty()));
}

#[test]
#[ignore = "two fresh release timings for identical readonly reordered Hunk input"]
fn compact_hunk_traversal_measurement_fs_uae() {
    native_expected_cases(
        (0..2)
            .map(|round| {
                (
                    format!("readonly/{round}"),
                    "m68020",
                    readonly_measurement(),
                    NativeExpected::MatchHunk,
                )
            })
            .collect(),
        "OPFORGE_HUNK_TRAVERSAL_REPORT",
    );
}

fn reference_repair_cases() -> Vec<(String, &'static str, String, NativeExpected)> {
    vec![
        ("section-pc".into(), "m68020", source(".section code,kind=code\n.long $\n.long $\n.endsection\n", "code"), NativeExpected::MatchHunk),
        ("scalar-snapshot-instruction".into(), "m68020", source("n .var 1\n.section data,kind=data\n.long 0\nsnapshot .const n\nn .set 2\n.endsection\n.section code,kind=code\n moveq #snapshot,d1\n.long snapshot,n\n.endsection\n", "code,data"), NativeExpected::MatchHunk),
    ]
}

#[test]
fn compact_hunk_reference_repair_rust_oracles() {
    let dir = create_temp_dir("hunk-reference-repair-oracles");
    let _cleanup = Cleanup(dir.clone());
    for (name, _, source, _) in reference_repair_cases() {
        let bytes = project_oracle(&dir, &source, true, &[]).unwrap();
        let segments = hunk::segments(&bytes).unwrap();
        match name.as_str() {
            "section-pc" => {
                assert_eq!(segments[0].payload, [0, 0, 0, 0, 0, 0, 0, 4]);
                assert_eq!(segments[0].relocations, [(0, 0), (4, 0)]);
            }
            "scalar-snapshot-instruction" => {
                assert_eq!(segments[0].payload, [0x72, 1, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0]);
                assert!(segments
                    .iter()
                    .all(|segment| segment.relocations.is_empty()));
            }
            _ => unreachable!(),
        }
    }
}

#[test]
#[ignore = "fresh native Hunk scalar instruction snapshot comparison"]
fn compact_hunk_scalar_snapshot_instruction_fs_uae() {
    native_expected_cases(
        reference_repair_cases()
            .into_iter()
            .filter(|(name, _, _, _)| name == "scalar-snapshot-instruction")
            .collect(),
        "OPFORGE_HUNK_REFERENCE_REPAIR_REPORT",
    );
}

#[test]
#[ignore = "requires fresh FS-UAE section-relative DATA PC relocation comparison"]
fn compact_hunk_section_pc_relocation_fs_uae() {
    native_expected_cases(
        section_pc_cases()
            .into_iter()
            .filter(|(name, _)| *name != "section-pc-mapped-block")
            .map(|(name, source)| (name.into(), "m68020", source, NativeExpected::MatchHunk))
            .collect(),
        "OPFORGE_HUNK_SECTION_PC_REPORT",
    );
}

fn mapped_hunk_cases() -> Vec<(&'static str, String)> {
    let (_, pc) = section_pc_cases()
        .into_iter()
        .find(|(name, _)| *name == "section-pc-mapped-block")
        .unwrap();
    let scalar = pc.replace(".long $\n.long $\n", ".long 1\n.long 2\n");
    let alias = pc.replace(
        ".section code,kind=code\n.long dep.entry\n",
        "root_alias .const dep.entry+2\n.section code,kind=code\n.long dep.entry,root_alias\n",
    );
    let reversed = alias
        .replace("sections=code,data", "sections=data,code")
        .replace("kind=data", "kind=code");
    let dependency_start = alias.find(".module pc_dep\n").unwrap();
    let dependency_first = format!(
        "{}{}",
        &alias[dependency_start..],
        &alias[..dependency_start]
    );
    let reopened = dependency_first.replace(
        ".section data,kind=data\n.long 0\n.endsection\n",
        ".section data,kind=data\n.long $11\n.endsection\n.section data,kind=data\n.long $22,$33\n.endsection\n",
    );
    let bss = pc
        .replace("kind=data", "kind=bss")
        .replace(
            ".section data,kind=bss\n.long 0",
            ".section data,kind=bss\n.res byte,4",
        )
        .replace(".long $\n.long $", ".res byte,8");
    let mutable = pc
        .replace(".use pc_dep", "n .var 1\n.use pc_dep")
        .replace(
            ".section code,kind=code\n.long dep.entry\n.endsection\n.section data,kind=data\n.long 0\n.endsection\n",
            ".section data,kind=data\n.long n\nn .set n+1\nsnap .const n\n.endsection\n.section code,kind=code\n.long n,snap,dep.entry\nn .set n+1\n.endsection\n.section data,kind=data\n.long n,snap\n.endsection\n",
        );
    let two_maps = concat!(
        ".module main\n.cpu m68020\n",
        ".use dep_a (entry) as left map { logical_a -> data_a }\n",
        ".use dep_b (entry) as right map { logical_b -> data_b }\n",
        ".section code,kind=code\n.long left.entry,right.entry\n.endsection\n",
        ".section data_a,kind=data\n.long $11\n.endsection\n",
        ".section data_b,kind=data\n.long $22,$33\n.endsection\n",
        ".output \"out.hunk\",format=hunk,sections=code,data_b,data_a\n.endmodule\n",
        ".module dep_a\n.cpu m68020\n.pub\n",
        ".section logical_a,kind=data,logical\nentry .block\n.long $\n.bend\n.endsection\n.endmodule\n",
        ".module dep_b\n.cpu m68020\n.pub\n",
        ".section logical_b,kind=data,logical\nentry .block\n.long $\n.bend\n.endsection\n.endmodule\n",
    );
    vec![
        ("mapped-pc-block", pc),
        ("mapped-scalar-block", scalar),
        ("mapped-imported-alias", alias),
        ("mapped-reversed-output", reversed),
        ("mapped-dependency-first", dependency_first),
        ("mapped-two-targets", two_maps.into()),
        ("mapped-reopened-prefix", reopened),
        ("mapped-bss-block", bss),
        ("mapped-mutable-snapshot-order", mutable),
    ]
}

#[test]
fn compact_hunk_mapped_logical_sections_rust_oracles() {
    let dir = create_temp_dir("hunk-mapped-logical-sections-oracles");
    let _cleanup = Cleanup(dir.clone());
    for (name, source) in mapped_hunk_cases() {
        let bytes = project_oracle(&dir, &source, true, &[])
            .unwrap_or_else(|error| panic!("{name}: {error}"));
        let segments = hunk::segments(&bytes).unwrap();
        if name == "mapped-bss-block" {
            assert_eq!(segments[1].kind, 0x3eb);
            assert_eq!(segments[1].reserved_bytes, 12);
        }
        let expected = match name {
            "mapped-pc-block" => vec![
                (vec![4], vec![(0, 1)]),
                (vec![0, 4, 8], vec![(4, 1), (8, 1)]),
            ],
            "mapped-scalar-block" => vec![(vec![4], vec![(0, 1)]), (vec![0, 1, 2], vec![])],
            "mapped-imported-alias" | "mapped-dependency-first" => vec![
                (vec![4, 6], vec![(0, 1), (4, 1)]),
                (vec![0, 4, 8], vec![(4, 1), (8, 1)]),
            ],
            "mapped-reversed-output" => vec![
                (vec![0, 4, 8], vec![(4, 0), (8, 0)]),
                (vec![4, 6], vec![(0, 0), (4, 0)]),
            ],
            "mapped-reopened-prefix" => vec![
                (vec![12, 14], vec![(0, 1), (4, 1)]),
                (vec![0x11, 0x22, 0x33, 12, 16], vec![(12, 1), (16, 1)]),
            ],
            "mapped-bss-block" => vec![(vec![4], vec![(0, 1)]), (vec![], vec![])],
            "mapped-mutable-snapshot-order" => vec![
                (vec![2, 2, 12], vec![(8, 1)]),
                (vec![1, 3, 2, 12, 16], vec![(12, 1), (16, 1)]),
            ],
            "mapped-two-targets" => vec![
                (vec![4, 8], vec![(4, 1), (0, 2)]),
                (vec![0x22, 0x33, 8], vec![(8, 1)]),
                (vec![0x11, 4], vec![(4, 2)]),
            ],
            _ => unreachable!(),
        };
        assert_eq!(segments.len(), expected.len(), "{name}");
        for (index, (segment, (words, relocations))) in segments.iter().zip(expected).enumerate() {
            let payload: Vec<u8> = words.into_iter().flat_map(u32::to_be_bytes).collect();
            assert_eq!(segment.payload, payload, "{name}/{index} payload");
            assert_eq!(
                segment.relocations, relocations,
                "{name}/{index} relocations"
            );
        }
    }
}

#[test]
#[ignore = "requires fresh FS-UAE mapped logical section placement and relocation comparison"]
fn compact_hunk_mapped_logical_sections_fs_uae() {
    native_expected_cases(
        mapped_hunk_cases()
            .into_iter()
            .map(|(name, source)| (name.into(), "m68020", source, NativeExpected::MatchHunk))
            .collect(),
        "OPFORGE_HUNK_MAPPED_REPORT",
    );
}

fn mapped_hunk_measurement() -> String {
    let (_, pc) = mapped_hunk_cases()
        .into_iter()
        .find(|(name, _)| *name == "mapped-pc-block")
        .unwrap();
    pc.replace(".long 0\n", &".long $11223344\n".repeat(128))
        .replace(".long $\n.long $\n", &".long $\n".repeat(128))
}

#[test]
fn compact_hunk_mapped_measurement_rust_oracle() {
    let dir = create_temp_dir("hunk-mapped-measurement-oracle");
    let _cleanup = Cleanup(dir.clone());
    let bytes = project_oracle(&dir, &mapped_hunk_measurement(), true, &[]).unwrap();
    let segments = hunk::segments(&bytes).unwrap();
    assert_eq!(segments.len(), 2);
    assert_eq!(segments[0].payload, 512u32.to_be_bytes());
    assert_eq!(segments[0].relocations, [(0, 1)]);
    let offsets = (512u32..1024).step_by(4);
    let payload: Vec<u8> = std::iter::repeat_n(0x11223344u32, 128)
        .chain(offsets.clone())
        .flat_map(u32::to_be_bytes)
        .collect();
    assert_eq!(segments[1].payload, payload);
    assert_eq!(
        segments[1].relocations,
        offsets.map(|offset| (offset, 1)).collect::<Vec<_>>()
    );
}

#[test]
#[ignore = "two fresh timings for identical mapped Hunk prefix and logical payload"]
fn compact_hunk_mapped_measurement_fs_uae() {
    native_expected_cases(
        (0..2)
            .map(|round| {
                (
                    format!("mapped-measurement/{round}"),
                    "m68020",
                    mapped_hunk_measurement(),
                    NativeExpected::MatchHunk,
                )
            })
            .collect(),
        "OPFORGE_HUNK_MAPPED_REPORT",
    );
}

fn flat_reopened_logical_source() -> String {
    concat!(
        ".module main\n.cpu m6502\n.region image,$1000,$10ff\n",
        ".section code\n.byte 1\n.endsection\n",
        ".section code,logical\n.byte 2\n.endsection\n",
        ".place code in image\n.output \"output.bin\",format=bin,sections=code\n.endmodule\n",
    )
    .into()
}

#[test]
fn compact_flat_reopened_logical_rust_oracle() {
    let dir = create_temp_dir("flat-reopened-logical-oracle");
    let _cleanup = Cleanup(dir.clone());
    assert_eq!(
        project_oracle(&dir, &flat_reopened_logical_source(), false, &[]).unwrap(),
        [1, 2],
    );
}

#[test]
#[ignore = "requires fresh FS-UAE flat logical section reopening without an explicit map"]
fn compact_flat_reopened_logical_fs_uae() {
    native_expected_cases(
        vec![(
            "flat-reopened-logical".into(),
            "6502",
            flat_reopened_logical_source(),
            NativeExpected::MatchRust,
        )],
        "OPFORGE_HUNK_MAPPED_REPORT",
    );
}

fn invalid_mapped_hunk_cases() -> Vec<(&'static str, String)> {
    let (_, pc) = section_pc_cases()
        .into_iter()
        .find(|(name, _)| *name == "section-pc-mapped-block")
        .unwrap();
    let (_, two_maps) = mapped_hunk_cases()
        .into_iter()
        .find(|(name, _)| *name == "mapped-two-targets")
        .unwrap();
    vec![
        (
            "mapped-kind-mismatch",
            pc.replace(".section data,kind=data", ".section data,kind=code"),
        ),
        ("mapped-equal-leaf", pc.replace("logical_data", "data")),
        (
            "mapped-two-owner-equal-leaf",
            two_maps
                .replace("logical_a", "logical_data")
                .replace("logical_b", "logical_data"),
        ),
    ]
}

#[test]
fn compact_hunk_mapped_invalid_rust_oracles() {
    let dir = create_temp_dir("hunk-mapped-invalid-oracles");
    let _cleanup = Cleanup(dir.clone());
    for (name, source) in invalid_mapped_hunk_cases() {
        assert!(project_oracle(&dir, &source, true, &[]).is_err(), "{name}");
    }
}

#[test]
#[ignore = "requires fresh FS-UAE unsupported mapped section contract rejection"]
fn compact_hunk_mapped_invalid_fs_uae() {
    native_expected_cases(
        invalid_mapped_hunk_cases()
            .into_iter()
            .map(|(name, source)| {
                (
                    name.into(),
                    "m68020",
                    source,
                    NativeExpected::RejectInvalidHunk,
                )
            })
            .collect(),
        "OPFORGE_HUNK_MAPPED_REPORT",
    );
}

fn section_pc_cases() -> Vec<(&'static str, String)> {
    vec![
        (
            "section-pc",
            source(".section code,kind=code\n.long $\n.long $\n.endsection\n", "code"),
        ),
        (
            "section-pc-absolute-addends",
            source(
                "Offset .const 3\n.section code,kind=code\n.long 0\n.long $+Offset\n.long Offset+$\n.long $-Offset\n.endsection\n",
                "code",
            ),
        ),
        (
            "section-pc-cancellation",
            source(
                ".section code,kind=code\n.long 0\nanchor\n.long 0\n.long $-anchor,anchor-$\n.endsection\n",
                "code",
            ),
        ),
        (
            "section-pc-same-line",
            source(
                ".section code,kind=code\n.long 0\n.long $,$\n.endsection\n",
                "code",
            ),
        ),
        (
            "section-pc-narrow-cancellation",
            source(
                ".section code,kind=code\n.long 0\n.word $-$\n.byte $-$\n.byte 0\n.long $\n.endsection\n",
                "code",
            ),
        ),
        (
            "section-pc-shared-emit",
            source(
                ".section code,kind=code\n.emit long,0\n.emit long,$,$+3\n.endsection\n",
                "code",
            ),
        ),
        (
            "section-pc-unselected",
            source(
                ".section hidden,kind=data\n.long $\n.endsection\n.section code,kind=code\n.long $\n.endsection\n",
                "code",
            ),
        ),
        (
            "section-pc-reopened-reordered",
            source(
                ".section data,kind=data\n.long $\n.endsection\n.section code,kind=code\n.long 0\n.long $\n.endsection\n.section data,kind=data\n.long $,$\n.endsection\n",
                "code,data",
            ),
        ),
        (
            "section-pc-mapped-block",
            concat!(
                ".module main\n.cpu m68020\n",
                ".use pc_dep (entry) as dep map { logical_data -> data }\n",
                ".section code,kind=code\n.long dep.entry\n.endsection\n",
                ".section data,kind=data\n.long 0\n.endsection\n",
                ".output \"out.hunk\",format=hunk,sections=code,data\n.endmodule\n",
                ".module pc_dep\n.cpu m68020\n.pub\n",
                ".section logical_data,kind=data,logical\nentry .block\n",
                ".long $\n.long $\n.bend\n.endsection\n.endmodule\n",
            )
            .into(),
        ),
    ]
}

#[test]
fn compact_hunk_section_pc_relocation_rust_oracles() {
    let dir = create_temp_dir("hunk-section-pc-oracles");
    let _cleanup = Cleanup(dir.clone());
    for (name, source) in section_pc_cases() {
        let bytes = project_oracle(&dir, &source, true, &[]).unwrap();
        let segments = hunk::segments(&bytes).unwrap();
        let expected = match name {
            "section-pc" => vec![(vec![0, 4], vec![(0, 0), (4, 0)])],
            "section-pc-absolute-addends" => {
                vec![(vec![0, 7, 11, 9], vec![(4, 0), (8, 0), (12, 0)])]
            }
            // Both operands see the line's PC (8), and anchor is at 4.
            "section-pc-cancellation" => vec![(vec![0, 0, 4, (-4i32) as u32], vec![])],
            "section-pc-same-line" => vec![(vec![0, 4, 4], vec![(4, 0), (8, 0)])],
            "section-pc-narrow-cancellation" => vec![(vec![0, 0, 8], vec![(8, 0)])],
            "section-pc-shared-emit" => vec![(vec![0, 4, 7], vec![(4, 0), (8, 0)])],
            "section-pc-unselected" => vec![(vec![0], vec![(0, 0)])],
            "section-pc-reopened-reordered" => vec![
                (vec![0, 4], vec![(4, 0)]),
                (vec![0, 4, 4], vec![(0, 1), (4, 1), (8, 1)]),
            ],
            "section-pc-mapped-block" => vec![
                (vec![4], vec![(0, 1)]),
                (vec![0, 4, 8], vec![(4, 1), (8, 1)]),
            ],
            _ => unreachable!(),
        };
        assert_eq!(segments.len(), expected.len(), "{name}");
        for (index, (segment, (words, relocations))) in segments.iter().zip(expected).enumerate() {
            let payload: Vec<u8> = words.into_iter().flat_map(u32::to_be_bytes).collect();
            assert_eq!(segment.payload, payload, "{name}/{index} payload");
            assert_eq!(
                segment.relocations, relocations,
                "{name}/{index} relocations"
            );
        }
    }
}

fn invalid_section_pc_cases() -> Vec<(&'static str, String)> {
    let mut cases: Vec<_> = [
        ("section-pc-multiply", ".long $*2\n"),
        ("section-pc-add-address", ".long $+anchor\n"),
        ("section-pc-word", ".word $\n"),
        ("section-pc-malformed", ".long $+\n"),
    ]
    .map(|(name, data)| {
        (
            name,
            source(
                &format!(".section code,kind=code\nanchor .long 0\n{data}.endsection\n"),
                "code",
            ),
        )
    })
    .into();
    cases.extend(
        [
            ("section-pc-cross-section-subtract", "$-other"),
            ("section-pc-cross-section-reverse-subtract", "other-$"),
        ]
        .map(|(name, expression)| {
            (
                name,
                // Both offsets are zero, but their distinct section bases must not cancel.
                source(
                    &format!(".section data,kind=data\nother .long 0\n.endsection\n.section code,kind=code\n.long {expression}\n.long 0\n.endsection\n"),
                    "code,data",
                ),
            )
        }),
    );
    cases
}

#[test]
fn compact_hunk_section_pc_invalid_rust_oracles() {
    let dir = create_temp_dir("hunk-section-pc-invalid-oracles");
    let _cleanup = Cleanup(dir.clone());
    for (name, source) in invalid_section_pc_cases() {
        assert!(
            project_oracle(&dir, &source, true, &[]).is_err(),
            "{name} must be rejected"
        );
    }
}

#[test]
#[ignore = "requires fresh FS-UAE failure completion for unsupported or malformed DATA PC"]
fn compact_hunk_section_pc_invalid_fs_uae() {
    native_expected_cases(
        invalid_section_pc_cases()
            .into_iter()
            .map(|(name, source)| {
                (
                    name.into(),
                    "m68020",
                    source,
                    NativeExpected::RejectInvalidHunk,
                )
            })
            .collect(),
        "OPFORGE_HUNK_SECTION_PC_REPORT",
    );
}

fn flat_pc_source() -> String {
    ".module flat_pc\n.cpu m68020\n.org $1234\n.long $,$\n.long $\n.endmodule\n".into()
}

#[test]
fn compact_flat_data_pc_rust_oracle() {
    let dir = create_temp_dir("flat-data-pc-oracle");
    let _cleanup = Cleanup(dir.clone());
    let expected: Vec<u8> = [0x1234u32, 0x1234, 0x123c]
        .into_iter()
        .flat_map(u32::to_be_bytes)
        .collect();
    assert_eq!(oracle(&dir, &flat_pc_source()).unwrap(), expected);
}

#[test]
#[ignore = "requires fresh FS-UAE flat absolute PC DATA comparison"]
fn compact_flat_data_pc_fs_uae() {
    native_expected_cases(
        vec![(
            "flat-data-pc".into(),
            "m68020",
            flat_pc_source(),
            NativeExpected::MatchRust,
        )],
        "OPFORGE_HUNK_SECTION_PC_REPORT",
    );
}

fn instruction_pc_cases() -> Vec<(&'static str, String)> {
    let direct = source(
        ".section code,kind=code\n.long 0\n move.l #$,d0\n bra.w $\n.endsection\n",
        "code",
    );
    let mapped = section_pc_cases()
        .into_iter()
        .find(|(name, _)| *name == "section-pc-mapped-block")
        .unwrap()
        .1
        .replace("kind=data", "kind=code")
        .replace(".long $\n.long $\n", " move.l #$,d0\n bra.w $\n");
    vec![
        ("instruction-pc-unplaced", direct.clone()),
        ("instruction-pc-placed", direct.replace(".output", ".region ram,$2000,$20ff\n.place code in ram\n.output")),
        ("instruction-pc-addends", source("Offset .const 3\n.section code,kind=code\n.long 0\n move.l #$+Offset,d0\n move.l #Offset+$,d0\n move.l #$-Offset,d0\n.endsection\n", "code")),
        ("instruction-pc-differences", source(".section code,kind=code\n.long 0\nanchor\n move.l #$-anchor,d0\n move.l #anchor-$,d0\n.endsection\n", "code")),
        ("instruction-pc-branch-addend", source(".section code,kind=code\n bra.w $+4\n.endsection\n", "code")),
        ("instruction-pc-literal-addends", source(".section code,kind=code\n.long 0\n move.l #$+3,d0\n move.l #3+$,d0\n.endsection\n", "code")),
        ("instruction-pc-interleaved", source(".section data,kind=code\n move.l #$,d0\n bra.w $\n.endsection\n.section code,kind=code\n move.l #$+3,d0\n bra.w $+4\n.endsection\n.section data,kind=code\n bra.w $\n move.l #3+$,d0\n.endsection\n", "code,data")),
        ("instruction-pc-mapped", mapped),
    ]
}

#[test]
fn compact_hunk_instruction_pc_rust_oracles() {
    let dir = create_temp_dir("hunk-instruction-pc-oracles");
    let _cleanup = Cleanup(dir.clone());
    for (name, source) in instruction_pc_cases() {
        let bytes = project_oracle(&dir, &source, true, &[])
            .unwrap_or_else(|error| panic!("{name}: {error}"));
        let segments = hunk::segments(&bytes).unwrap();
        let expected = match name {
            "instruction-pc-unplaced" | "instruction-pc-placed" => vec![(
                vec![
                    0, 0, 0, 0, 0x20, 0x3c, 0, 0, 0, 4, 0x60, 0, 0xff, 0xfe, 0, 0,
                ],
                vec![(6, 0)],
            )],
            "instruction-pc-literal-addends" => vec![(
                vec![0, 0, 0, 0, 0x20, 0x3c, 0, 0, 0, 7, 0x20, 0x3c, 0, 0, 0, 13],
                vec![(6, 0), (12, 0)],
            )],
            "instruction-pc-interleaved" => vec![
                (
                    vec![0x20, 0x3c, 0, 0, 0, 3, 0x60, 0, 0, 2, 0, 0],
                    vec![(2, 0)],
                ),
                (
                    vec![
                        0x20, 0x3c, 0, 0, 0, 0, 0x60, 0, 0xff, 0xfe, 0x60, 0, 0xff, 0xfe, 0x20,
                        0x3c, 0, 0, 0, 17,
                    ],
                    vec![(2, 1), (16, 1)],
                ),
            ],
            "instruction-pc-addends" => vec![(
                vec![
                    0, 0, 0, 0, 0x20, 0x3c, 0, 0, 0, 7, 0x20, 0x3c, 0, 0, 0, 13, 0x20, 0x3c, 0, 0,
                    0, 13, 0, 0,
                ],
                vec![(6, 0), (12, 0), (18, 0)],
            )],
            "instruction-pc-differences" => vec![(
                vec![
                    0, 0, 0, 0, 0x20, 0x3c, 0, 0, 0, 0, 0x20, 0x3c, 0xff, 0xff, 0xff, 0xfa,
                ],
                vec![],
            )],
            "instruction-pc-branch-addend" => vec![(vec![0x60, 0, 0, 2], vec![])],
            "instruction-pc-mapped" => vec![
                (vec![0, 0, 0, 4], vec![(0, 1)]),
                (
                    vec![
                        0, 0, 0, 0, 0x20, 0x3c, 0, 0, 0, 4, 0x60, 0, 0xff, 0xfe, 0, 0,
                    ],
                    vec![(6, 1)],
                ),
            ],
            _ => unreachable!(),
        };
        assert_eq!(segments.len(), expected.len(), "{name}");
        for (index, (segment, (payload, relocations))) in segments.iter().zip(expected).enumerate()
        {
            assert_eq!(segment.payload, payload, "{name}/{index} payload");
            assert_eq!(
                segment.relocations, relocations,
                "{name}/{index} relocations"
            );
        }
    }
}

#[test]
#[ignore = "requires fresh FS-UAE direct instruction PC exact live Rust Hunk comparison"]
fn compact_hunk_instruction_pc_fs_uae() {
    native_expected_cases(
        instruction_pc_cases()
            .into_iter()
            // Explicit Hunk placement is an existing preparation gap, tracked
            // by the separate reproducer below rather than counted as parity.
            .filter(|(name, _)| *name != "instruction-pc-placed")
            .map(|(name, source)| (name.into(), "m68020", source, NativeExpected::MatchHunk))
            .collect(),
        "OPFORGE_HUNK_INSTRUCTION_PC_REPORT",
    );
}

#[test]
#[ignore = "known native gap: explicit Hunk placement rejects during preparation"]
fn compact_hunk_instruction_pc_placed_gap_fs_uae() {
    native_expected_cases(
        instruction_pc_cases()
            .into_iter()
            .filter(|(name, _)| *name == "instruction-pc-placed")
            .map(|(name, source)| (name.into(), "m68020", source, NativeExpected::MatchHunk))
            .collect(),
        "OPFORGE_HUNK_INSTRUCTION_PC_REPORT",
    );
}

fn invalid_instruction_pc_cases() -> Vec<(&'static str, String)> {
    [
        ("instruction-pc-multiply", "$*2"),
        ("instruction-pc-add-address", "$+anchor"),
        ("instruction-pc-cross-section", "$-other"),
        ("instruction-pc-reverse-cross-section", "other-$"),
        ("instruction-pc-malformed", "$+"),
    ].map(|(name, expression)| (name, source(&format!(
        ".section data,kind=data\nother .long 0\n.endsection\n.section code,kind=code\nanchor .long 0\n move.l #{expression},d0\n.endsection\n"), "code,data"))).into()
}

#[test]
fn compact_hunk_instruction_pc_invalid_rust_oracles() {
    let dir = create_temp_dir("hunk-instruction-pc-invalid-oracles");
    let _cleanup = Cleanup(dir.clone());
    for (name, source) in invalid_instruction_pc_cases() {
        assert!(
            project_oracle(&dir, &source, true, &[]).is_err(),
            "{name} must be rejected"
        );
    }
}

#[test]
#[ignore = "requires fresh FS-UAE direct instruction PC invalid arithmetic completion"]
fn compact_hunk_instruction_pc_invalid_fs_uae() {
    native_expected_cases(
        invalid_instruction_pc_cases()
            .into_iter()
            .map(|(name, source)| {
                (
                    name.into(),
                    "m68020",
                    source,
                    NativeExpected::RejectInvalidHunk,
                )
            })
            .collect(),
        "OPFORGE_HUNK_INSTRUCTION_PC_REPORT",
    );
}

fn invalid_instruction_pc_branch_cases() -> Vec<(&'static str, String)> {
    [
        ("instruction-pc-branch-multiply", "$*2"),
        ("instruction-pc-branch-add-address", "$+anchor"),
        ("instruction-pc-branch-cross-section", "$-other"),
        ("instruction-pc-branch-invalid-alias", "alias"),
    ].map(|(name, expression)| (name, source(&format!(
        ".section data,kind=data\nother .long 0\n.endsection\n.section code,kind=code\nanchor .long 0\nbad .const anchor*2\nalias .const bad\n bra.w {expression}\n.endsection\n"), "code,data"))).into()
}

#[test]
fn compact_hunk_instruction_pc_branch_invalid_rust_oracles() {
    let dir = create_temp_dir("hunk-instruction-pc-branch-invalid-oracles");
    let _cleanup = Cleanup(dir.clone());
    for (name, source) in invalid_instruction_pc_branch_cases() {
        assert!(
            project_oracle(&dir, &source, true, &[]).is_err(),
            "{name} must be rejected"
        );
    }
}

#[test]
#[ignore = "requires fresh FS-UAE unsupported positional branch arithmetic completion"]
fn compact_hunk_instruction_pc_branch_invalid_fs_uae() {
    native_expected_cases(
        invalid_instruction_pc_branch_cases()
            .into_iter()
            .map(|(name, source)| {
                (
                    name.into(),
                    "m68020",
                    source,
                    NativeExpected::RejectInvalidHunk,
                )
            })
            .collect(),
        "OPFORGE_HUNK_INSTRUCTION_PC_REPORT",
    );
}

#[test]
fn compact_flat_instruction_pc_arithmetic_rust_oracle() {
    let dir = create_temp_dir("flat-instruction-pc-arithmetic-oracle");
    let _cleanup = Cleanup(dir.clone());
    let source = flat_instruction_pc_arithmetic_source();
    assert_eq!(
        oracle(&dir, &source).unwrap(),
        [
            0, 0, 0, 0, 0x20, 0x3c, 0, 0, 0, 8, 0x60, 0, 0xff, 0xf4, 0x22, 0x3c, 0, 0, 0, 14, 0x60,
            0, 0xff, 0xfe
        ]
    );
}

fn instruction_pc_measurement() -> String {
    source(
        &format!(
            "Offset .const 3\n.section code,kind=code\n{}.endsection\n",
            " move.l #$+Offset,d0\n bra.w $+4\n".repeat(128)
        ),
        "code",
    )
}

#[test]
fn compact_hunk_instruction_pc_measurement_rust_oracle() {
    let dir = create_temp_dir("hunk-instruction-pc-measurement-oracle");
    let _cleanup = Cleanup(dir.clone());
    let bytes = project_oracle(&dir, &instruction_pc_measurement(), true, &[]).unwrap();
    let segments = hunk::segments(&bytes).unwrap();
    let mut payload = Vec::new();
    for offset in (0u32..1280).step_by(10) {
        payload.extend([0x20, 0x3c]);
        payload.extend((offset + 3).to_be_bytes());
        payload.extend([0x60, 0, 0, 2]);
    }
    assert_eq!(segments.len(), 1);
    assert_eq!(segments[0].payload, payload);
    assert_eq!(
        segments[0].relocations,
        (0u32..1280)
            .step_by(10)
            .map(|offset| (offset + 2, 0))
            .collect::<Vec<_>>()
    );
}

#[test]
#[ignore = "two fresh release timings for identical direct instruction PC mixed workload"]
fn compact_hunk_instruction_pc_measurement_fs_uae() {
    native_expected_cases(
        (0..2)
            .map(|round| {
                (
                    format!("instruction-pc-measurement/{round}"),
                    "m68020",
                    instruction_pc_measurement(),
                    NativeExpected::MatchHunk,
                )
            })
            .collect(),
        "OPFORGE_HUNK_INSTRUCTION_PC_REPORT",
    );
}

fn flat_instruction_pc_arithmetic_source() -> String {
    ".module flat_pc\n.cpu m68020\n.org 0\nanchor .long 0\nbad .const anchor*2\nalias .const bad\n move.l #$*2,d0\n bra.w alias\n move.l #$,d1\n bra.w $\n.endmodule\n".into()
}

#[test]
#[ignore = "requires fresh FS-UAE flat numeric PC and alias arithmetic exact Rust comparison"]
fn compact_flat_instruction_pc_arithmetic_fs_uae() {
    native_expected_cases(
        vec![(
            "flat-instruction-pc-arithmetic".into(),
            "m68020",
            flat_instruction_pc_arithmetic_source(),
            NativeExpected::MatchRust,
        )],
        "OPFORGE_HUNK_INSTRUCTION_PC_REPORT",
    );
}

fn forward_block_branch_source() -> String {
    source(
        &format!(
            ".section code,kind=code\nentry .block\n bra.w consumed\n bra.w consumed\n bra.w consumed\n{}consumed\n rts\n.bend\n.endsection\n",
            ".byte 0,0,0,0\n".repeat(64)
        ),
        "code",
    )
}

#[test]
fn compact_hunk_forward_block_branches_rust_oracle() {
    let dir = create_temp_dir("hunk-forward-block-branches-oracle");
    let _cleanup = Cleanup(dir.clone());
    let bytes = project_oracle(&dir, &forward_block_branch_source(), true, &[]).unwrap();
    let segments = hunk::segments(&bytes).unwrap();
    let mut payload = vec![0x60, 0, 1, 10, 0x60, 0, 1, 6, 0x60, 0, 1, 2];
    payload.extend([0; 256]);
    payload.extend([0x4e, 0x75, 0, 0]);
    assert_eq!(segments.len(), 1);
    assert_eq!(segments[0].payload, payload);
    assert!(segments[0].relocations.is_empty());
}

#[test]
#[ignore = "requires fresh FS-UAE forward ordinary internal block labels and word branch parity"]
fn compact_hunk_forward_block_branches_fs_uae() {
    native_expected_cases(
        vec![(
            "forward-block-branches".into(),
            "m68020",
            forward_block_branch_source(),
            NativeExpected::MatchHunk,
        )],
        "OPFORGE_HUNK_INSTRUCTION_PC_REPORT",
    );
}
