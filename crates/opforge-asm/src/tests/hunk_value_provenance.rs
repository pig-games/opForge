// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Numeric values alone cannot distinguish addresses from scalar snapshots.
use super::*;

#[path = "binary_source_hunk.rs"]
mod hunk;

fn assemble(body: &str, placed: bool) -> Assembler {
    let placement = if placed {
        ".region ram,$2000,$20ff\n.place code in ram\n.place data in ram\n"
    } else {
        ""
    };
    let source = format!(".module main\n.cpu m68020\n{body}{placement}.output \"out.hunk\",format=hunk,sections=code,data\n.endmodule\n");
    run_passes(&source.lines().collect::<Vec<_>>())
}

fn payload(assembler: &Assembler) -> Result<Vec<u8>, AsmError> {
    build_linker_output_payload(
        &assembler.root_metadata.linker_outputs[0],
        assembler.sections(),
    )
}

#[test]
fn hunk_value_provenance_pc_and_frozen_address_snapshots() {
    for placed in [false, true] {
        let assembler = assemble("n .var 1\n.section data,kind=data\npayload .long 0\naddress .const payload+n\nn .set 9\nalias = address\n.endsection\n.section code,kind=code\n.long $,alias,alias-1\n.endsection\n", placed);
        let bytes = payload(&assembler).expect("PC and address snapshots have Hunk fixups");
        let segments = hunk::segments(&bytes).unwrap();
        assert_eq!(segments[0].payload, [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0]);
        assert_eq!(segments[0].relocations, [(0, 0), (4, 1), (8, 1)]);
        assert_eq!(segments[1].payload, [0; 4]);
        assert!(segments[1].relocations.is_empty());
        let base = assembler.sections()["data"].base_addr.unwrap_or(0);
        assert_eq!(
            assembler.symbols.entry("main.address").unwrap().val,
            base + 1
        );
        assert_eq!(assembler.symbols.entry("main.alias").unwrap().val, base + 1);
    }
}

#[test]
fn hunk_value_provenance_scalar_snapshots_are_not_addresses() {
    for placed in [false, true] {
        let assembler = assemble("n .var 1\n.section data,kind=data\n.long 0\nsnapshot .const n\nn .set 2\n.endsection\n.section code,kind=code\n moveq #snapshot,d1\n.long snapshot,n\n.endsection\n", placed);
        let bytes = payload(&assembler).expect("scalar instruction snapshots need no relocation");
        let segments = hunk::segments(&bytes).unwrap();
        assert_eq!(segments[0].payload, [0x72, 1, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0]);
        assert!(segments
            .iter()
            .all(|segment| segment.relocations.is_empty()));
    }
}

#[test]
fn hunk_value_provenance_alias_cannot_hide_unsupported_address_arithmetic() {
    for expression in ["payload*2", "payload+payload", "-payload"] {
        let assembler = assemble(&format!(".section data,kind=data\npayload .long 0\nbad .const {expression}\nalias = bad\n.endsection\n.section code,kind=code\n.long alias\n.endsection\n"), false);
        let error = payload(&assembler).expect_err("an unsupported alias must fail closed");
        assert!(
            error.message().contains("symbolic"),
            "{expression}: {error:?}"
        );
    }
}

#[test]
fn hunk_value_provenance_mutation_replaces_section_identity() {
    let assembler = assemble(".section data,kind=data\npayload .long 0\n.endsection\nv .var payload\nv .set 3\nsnapshot .const v\n.section code,kind=code\n.long snapshot\nv += 1\n.long v\n.endsection\n", false);
    let bytes = payload(&assembler).unwrap();
    let segments = hunk::segments(&bytes).unwrap();
    assert_eq!(segments[0].payload, [0, 0, 0, 3, 0, 0, 0, 4]);
    assert!(segments[0].relocations.is_empty());
}

#[test]
fn hunk_value_provenance_compound_address_and_cancellation() {
    let assembler = assemble(".section data,kind=data\npayload .long 0\n.endsection\nv .var 3\nv += payload\naddress .const v\nv -= v\n.section code,kind=code\n.long address,v\n.endsection\n", false);
    let bytes = payload(&assembler).unwrap();
    let segments = hunk::segments(&bytes).unwrap();
    assert_eq!(segments[0].payload, [0, 0, 0, 3, 0, 0, 0, 0]);
    assert_eq!(segments[0].relocations, [(0, 1)]);
}

#[test]
fn hunk_value_provenance_scalar_index_snapshot() {
    let assembler = assemble("values = {1,2}\nsnapshot .const values[1]\n.section data,kind=data\n.long 0\n.endsection\n.section code,kind=code\n moveq #snapshot,d1\n.long snapshot\n.endsection\n", false);
    let bytes = payload(&assembler).expect("indexing scalar data preserves scalar identity");
    let segments = hunk::segments(&bytes).unwrap();
    assert_eq!(segments[0].payload, [0x72, 2, 0, 0, 0, 2, 0, 0]);
    assert!(segments[0].relocations.is_empty());
}

#[test]
fn hunk_value_provenance_mapped_block_alias() {
    let source = r#".module proof.main
.cpu m68020
.use presenter.worker as worker map { logical_code -> worker_code }
.region image,$2000,$20ff
.section header,kind=code
address .const worker.entry+2
.long address
.endsection
.section worker_code,kind=code
.long 0
.endsection
.place header in image
.place worker_code in image
.output "out.hunk",format=hunk,sections=header,worker_code
.endmodule
.module presenter.worker
.cpu m68020
.pub
.section logical_code,kind=code,logical
entry .block
    rts
.long entry,$
.bend
.endsection
.endmodule
"#;
    let assembler = run_passes(&source.lines().collect::<Vec<_>>());
    let bytes = payload(&assembler).expect("mapped alias targets the concrete Hunk section");
    let segments = hunk::segments(&bytes).unwrap();
    assert_eq!(segments[0].payload, [0, 0, 0, 6]);
    assert_eq!(segments[0].relocations, [(0, 1)]);
    assert_eq!(
        segments[1].payload,
        [0, 0, 0, 0, 0x4e, 0x75, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0]
    );
    assert_eq!(segments[1].relocations, [(6, 1), (10, 1)]);
    assert_eq!(
        assembler
            .symbols
            .entry("presenter.worker.entry")
            .unwrap()
            .val,
        0x2008
    );
}

#[test]
fn hunk_value_provenance_mutable_address_instruction() {
    let assembler = assemble(".section data,kind=data\npayload .long 0\n.endsection\nv .var payload+1\n.section code,kind=code\n move.l #v,d0\n.endsection\n", false);
    let bytes = payload(&assembler).expect("VM queries preserve mutable address identity");
    let segments = hunk::segments(&bytes).unwrap();
    assert_eq!(segments[0].payload, [0x20, 0x3c, 0, 0, 0, 1, 0, 0]);
    assert_eq!(segments[0].relocations, [(2, 1)]);
}

#[test]
fn hunk_value_provenance_mapped_pc_relative_alias() {
    let source = r#".module main
.cpu m68020
.use dep as worker map { logical -> code }
.region ram,$2000,$20ff
.section header,kind=code
.long worker.entry
.endsection
.section code,kind=code
.long 0
.endsection
.place header in ram
.place code in ram
.output "out.hunk",format=hunk,sections=header,code
.endmodule
.module dep
.cpu m68020
.pub
.section logical,kind=code,logical
entry .block
nearby .const after
    lea nearby(PC),a0
after
    rts
.bend
.endsection
.endmodule
"#;
    let assembler = run_passes(&source.lines().collect::<Vec<_>>());
    let bytes = payload(&assembler).expect("same-section PC alias in mapped code");
    let segments = hunk::segments(&bytes).unwrap();
    assert_eq!(segments[0].payload, [0, 0, 0, 4]);
    assert_eq!(segments[0].relocations, [(0, 1)]);
    assert_eq!(
        segments[1].payload,
        [0, 0, 0, 0, 0x41, 0xfa, 0, 2, 0x4e, 0x75, 0, 0]
    );
    assert!(segments[1].relocations.is_empty());
}

#[test]
fn hunk_value_provenance_position_proofs_preserve_sectioned_binary_algebra() {
    for (instruction, expected) in [
        ("lea alias(PC),a0", vec![0, 0, 0, 0, 0x41, 0xfa, 0xff, 0xfa]),
        ("bra.w alias", vec![0, 0, 0, 0, 0x60, 0, 0xff, 0xfa]),
        ("bra.w $*2", vec![0, 0, 0, 0, 0x60, 0, 0, 2]),
        ("move.l #$*2,d0", vec![0, 0, 0, 0, 0x20, 0x3c, 0, 0, 0, 8]),
    ] {
        for format in ["bin", "hunk"] {
            let source = format!(".module main\n.cpu m68020\n.section code,kind=code\nanchor .long 0\nbad .const anchor*2\nalias .const bad\n {instruction}\n.endsection\n.region image,0,$ff\n.place code in image\n.output \"out.{format}\",format={format},sections=code\n.endmodule\n");
            let assembler = run_passes(&source.lines().collect::<Vec<_>>());
            let result = payload(&assembler);
            if format == "bin" {
                assert_eq!(result.unwrap(), expected, "{instruction}");
            } else {
                let error = result.expect_err("unsupported position proof must reject Hunk output");
                assert!(
                    error.message().contains("symbolic"),
                    "{instruction}: {error:?}"
                );
            }
        }
    }
}
