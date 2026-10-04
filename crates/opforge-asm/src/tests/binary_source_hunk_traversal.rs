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

#[test]
fn compact_hunk_address_snapshot_rust_oracle() {
    let dir = create_temp_dir("hunk-address-snapshot-oracle");
    let _cleanup = Cleanup(dir.clone());
    let bytes = project_oracle(&dir, &address_snapshot(), true, &[]).unwrap();
    let segments = hunk::segments(&bytes).unwrap();
    assert_eq!(segments[0].payload, 1u32.to_be_bytes());
    assert_eq!(segments[0].relocations, [(0, 1)]);
}

#[test]
#[ignore = "fresh native address-derived snapshots retain the unsupported alias boundary"]
fn compact_hunk_address_snapshot_fs_uae() {
    native_expected_cases(
        vec![(
            "address-snapshot".into(),
            "m68020",
            address_snapshot(),
            NativeExpected::RejectHunk,
        )],
        "OPFORGE_HUNK_TRAVERSAL_REPORT",
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

#[test]
#[ignore = "known native mapped Hunk boundary; requires fresh FS-UAE desired PC comparison"]
fn compact_hunk_mapped_section_pc_relocation_fs_uae() {
    native_expected_cases(
        section_pc_cases()
            .into_iter()
            .filter(|(name, _)| *name == "section-pc-mapped-block")
            .map(|(name, source)| (name.into(), "m68020", source, NativeExpected::MatchHunk))
            .collect(),
        "OPFORGE_HUNK_SECTION_PC_REPORT",
    );
}

#[test]
#[ignore = "requires fresh FS-UAE scalar control for the existing mapped Hunk rejection"]
fn compact_hunk_mapped_section_pc_scalar_control_fs_uae() {
    let (_, source) = section_pc_cases()
        .into_iter()
        .find(|(name, _)| *name == "section-pc-mapped-block")
        .unwrap();
    native_expected_cases(
        vec![(
            "section-pc-mapped-block-scalar-control".into(),
            "m68020",
            source.replace(".long $\n.long $\n", ".long 1\n.long 2\n"),
            NativeExpected::RejectHunk,
        )],
        "OPFORGE_HUNK_SECTION_PC_REPORT",
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
