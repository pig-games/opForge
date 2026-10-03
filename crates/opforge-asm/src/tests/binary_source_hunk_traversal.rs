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
    assert!(segments[0].relocations.is_empty());
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
