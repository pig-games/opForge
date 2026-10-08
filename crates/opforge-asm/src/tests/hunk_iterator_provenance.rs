// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Active iterator bindings are scalars, including when they shadow addresses.
use super::*;

#[path = "binary_source_hunk.rs"]
mod hunk;

fn image(cpu: &str, body: &str) -> Vec<u8> {
    let source = format!(
        ".module main\n.cpu {cpu}\n{body}\n.output \"out.hunk\",format=hunk,sections=code,data\n.endmodule\n"
    );
    let assembler = run_passes(&source.lines().collect::<Vec<_>>());
    build_linker_output_payload(
        &assembler.root_metadata.linker_outputs[0],
        assembler.sections(),
    )
    .expect("iterator scalars and genuine addresses must produce a valid Hunk")
}

fn scalar_bytes(cpu: &str, values: &[u32]) -> Vec<u8> {
    let mut bytes = Vec::new();
    for &value in values {
        bytes.push(value as u8);
        if cpu == "6502" {
            bytes.extend_from_slice(&(value as u16).to_le_bytes());
            bytes.extend_from_slice(&value.to_le_bytes());
        } else {
            bytes.extend_from_slice(&(value as u16).to_be_bytes());
            bytes.extend_from_slice(&value.to_be_bytes());
        }
    }
    bytes.resize(bytes.len().div_ceil(4) * 4, 0);
    bytes
}

#[test]
fn hunk_iterator_provenance_for_direct_data_is_scalar_in_both_endiannesses() {
    for cpu in ["68020", "6502"] {
        let bytes = image(cpu, ".section code,kind=code\n.for i in {$12,$34}\n.byte i\n.word i\n.long i\n.endfor\n.endsection\n.section data,kind=data\n.long 0\n.endsection");
        let segments = hunk::segments(&bytes).unwrap();
        assert_eq!(segments[0].payload, scalar_bytes(cpu, &[0x12, 0x34]));
        assert!(segments.iter().all(|s| s.relocations.is_empty()));
    }
}

#[test]
fn hunk_iterator_provenance_bfor_direct_data_is_scalar_in_both_endiannesses() {
    for cpu in ["68020", "6502"] {
        let bytes = image(cpu, ".section code,kind=code\nitems .bfor i in {$12,$34}\n.byte i\n.word i\n.long i\n.endfor\n.endsection\n.section data,kind=data\n.long 0\n.endsection");
        let segments = hunk::segments(&bytes).unwrap();
        assert_eq!(segments[0].payload, scalar_bytes(cpu, &[0x12, 0x34]));
        assert!(segments.iter().all(|s| s.relocations.is_empty()));
    }
}

#[test]
fn hunk_iterator_provenance_scoped_constants_capture_scalar_values() {
    for cpu in ["68020", "6502"] {
        let bytes = image(cpu, ".section code,kind=code\nitems .bfor i in {1,2}\nsnapshot .const i+1\n.byte snapshot\n.word snapshot\n.long snapshot\n.endfor\n.endsection\n.section data,kind=data\n.long 0\n.endsection");
        let segments = hunk::segments(&bytes).unwrap();
        assert_eq!(segments[0].payload, scalar_bytes(cpu, &[2, 3]));
        assert!(segments.iter().all(|s| s.relocations.is_empty()));
    }
}

#[test]
fn hunk_iterator_provenance_nested_case_insensitive_shadowing_restores_outer() {
    let bytes = image("68020", ".section code,kind=code\n.for Index in {1,2}\n.long INDEX\n.bfor index in {3,4}\n.long InDeX\n.endfor\n.long index\n.endfor\n.endsection\n.section data,kind=data\n.long 0\n.endsection");
    let segments = hunk::segments(&bytes).unwrap();
    let expected: Vec<_> = [1u32, 3, 4, 1, 2, 3, 4, 2]
        .into_iter()
        .flat_map(u32::to_be_bytes)
        .collect();
    assert_eq!(segments[0].payload, expected);
    assert!(segments.iter().all(|s| s.relocations.is_empty()));
}

#[test]
fn hunk_iterator_provenance_shadowed_address_keeps_qualified_and_after_loop_fixups() {
    for cpu in ["68020", "6502"] {
        let bytes = image(cpu, ".section data,kind=data\n.long 0\nIndex .long 0\n.endsection\n.section code,kind=code\n.for index in {1,2}\n.long INDEX,main.Index\n.endfor\n.long index\n.endsection");
        let segments = hunk::segments(&bytes).unwrap();
        let expected: Vec<_> = [1u32, 4, 2, 4, 4]
            .into_iter()
            .flat_map(|value| {
                if cpu == "6502" {
                    value.to_le_bytes()
                } else {
                    value.to_be_bytes()
                }
            })
            .collect();
        assert_eq!(segments[0].payload, expected);
        assert_eq!(segments[0].relocations, [(4, 1), (12, 1), (16, 1)]);
        assert_eq!(segments[1].payload, [0; 8]);
        assert!(segments[1].relocations.is_empty());
    }
}

#[test]
fn hunk_iterator_provenance_instruction_fixups_preserve_scalar_and_address_identity() {
    let bytes = image("68020", ".section data,kind=data\n.long 0\nIndex .long 0\n.endsection\n.section code,kind=code\n.for index in {1,2}\n move.l #INDEX,d0\n move.l index.l,d1\n move.l #main.Index,d2\n.endfor\n move.l #index,d3\n.endsection");
    let segments = hunk::segments(&bytes).unwrap();
    assert_eq!(
        segments[0].payload,
        [
            0x20, 0x3c, 0, 0, 0, 1, 0x22, 0x39, 0, 0, 0, 1, 0x24, 0x3c, 0, 0, 0, 4, 0x20, 0x3c, 0,
            0, 0, 2, 0x22, 0x39, 0, 0, 0, 2, 0x24, 0x3c, 0, 0, 0, 4, 0x26, 0x3c, 0, 0, 0, 4, 0, 0,
        ]
    );
    assert_eq!(segments[0].relocations, [(14, 1), (32, 1), (38, 1)]);
}
