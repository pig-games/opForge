// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Indexed repetition records preserve scalar and section-address identity.
use super::*;

#[path = "binary_source_hunk.rs"]
mod hunk;

fn assemble(cpu: &str, body: &str, placed: bool, format: &str) -> Assembler {
    let placement = if placed {
        ".region ram,$2000,$20ff\n.place code in ram\n.place data in ram\n"
    } else {
        ""
    };
    let source = format!(".module main\n.cpu {cpu}\n{body}{placement}.output \"out.{format}\",format={format},sections=code,data\n.endmodule\n");
    run_passes(&source.lines().collect::<Vec<_>>())
}

fn payload(assembler: &Assembler) -> Result<Vec<u8>, AsmError> {
    build_linker_output_payload(
        &assembler.root_metadata.linker_outputs[0],
        assembler.sections(),
    )
}

fn longs(cpu: &str, values: &[u32]) -> Vec<u8> {
    values
        .iter()
        .flat_map(|value| {
            if cpu == "6502" {
                value.to_le_bytes()
            } else {
                value.to_be_bytes()
            }
        })
        .collect()
}

#[test]
fn hunk_repeat_records_indexed_addresses_and_scalar_members() {
    let body = ".section data,kind=data\nitems .bfor item in {2,5}\n.byte item\nentry .byte item+1\nvalue .const item+10\n.endfor\n.endsection\n.section code,kind=code\n.long items[0],items[1],items[1].entry,items[0].value,items[1].value\n.endsection\n";
    for cpu in ["68020", "6502"] {
        for placed in [false, true] {
            let assembler = assemble(cpu, body, placed, "hunk");
            let bytes = payload(&assembler).expect("indexed addresses retain section identity");
            let segments = hunk::segments(&bytes).unwrap();
            assert_eq!(segments[0].payload, longs(cpu, &[0, 2, 3, 12, 15]));
            assert_eq!(segments[0].relocations, [(0, 1), (4, 1), (8, 1)]);
            assert_eq!(segments[1].payload, [2, 3, 5, 6]);
            assert!(segments[1].relocations.is_empty());
        }
    }
}

#[test]
fn hunk_repeat_records_address_snapshots_addends_and_cancellation() {
    let body = ".section data,kind=data\nitems .bfor 2\n.byte 7\nentry .byte 9\n.endfor\nsnapshot .const items[1].entry\nalias = snapshot\nspan .const items[1].entry-items[0].entry\n.endsection\n.section code,kind=code\n.long snapshot,alias+1,span,items[1]-items[0]\n.endsection\n";
    for cpu in ["68020", "6502"] {
        for placed in [false, true] {
            let assembler = assemble(cpu, body, placed, "hunk");
            let bytes = payload(&assembler).expect("record snapshots retain relocatable addends");
            let segments = hunk::segments(&bytes).unwrap();
            assert_eq!(segments[0].payload, longs(cpu, &[3, 4, 2, 2]));
            assert_eq!(segments[0].relocations, [(0, 1), (4, 1)]);
            assert_eq!(segments[1].payload, [7, 9, 7, 9]);
            assert!(segments[1].relocations.is_empty());
            let base = assembler.sections()["data"].base_addr.unwrap_or(0);
            assert_eq!(
                assembler.symbols.entry("main.snapshot").unwrap().val,
                base + 3
            );
            assert_eq!(assembler.symbols.entry("main.span").unwrap().val, 2);
        }
    }
}

#[test]
fn hunk_repeat_records_cross_section_member_targets() {
    let body = ".section data,kind=data\n.byte 0\npayload .byte 9\n.endsection\n.section code,kind=code\nitems .bfor 2\nentry .byte 7\nremote .const payload+1\nvalue .const 13\n.endfor\n.long items[1].entry,items[1].remote,items[1].value\n.endsection\n";
    for cpu in ["68020", "6502"] {
        for placed in [false, true] {
            let assembler = assemble(cpu, body, placed, "hunk");
            let bytes = payload(&assembler).expect("members retain their own target section");
            let segments = hunk::segments(&bytes).unwrap();
            let mut expected = vec![7, 7];
            expected.extend(longs(cpu, &[1, 2, 13]));
            expected.extend([0, 0]);
            assert_eq!(segments[0].payload, expected);
            assert_eq!(segments[0].relocations, [(2, 0), (6, 1)]);
            assert_eq!(segments[1].payload, [0, 9, 0, 0]);
            assert!(segments[1].relocations.is_empty());
        }
    }
}

#[test]
fn hunk_repeat_records_unsafe_address_arithmetic_fails_closed() {
    for expression in ["items[1].entry*2", "items[0]+items[1]", "-items[1].entry"] {
        let body = format!(".section data,kind=data\nitems .bfor 2\nentry .byte 7\n.endfor\nbad .const {expression}\nalias = bad\n.endsection\n.section code,kind=code\n.long alias\n.endsection\n");
        for cpu in ["68020", "6502"] {
            let assembler = assemble(cpu, &body, false, "hunk");
            let error =
                payload(&assembler).expect_err("record aliases cannot hide unsupported arithmetic");
            assert!(
                error.message().contains("symbolic"),
                "{cpu}: {expression}: {error:?}"
            );
        }
    }
}

#[test]
fn hunk_repeat_records_nested_member_chains() {
    let body = ".section data,kind=data\nouter .bfor item in {2,4}\ninner .bfor child in {7,8}\nentry .byte child\nvalue .const item+child\n.endfor\n.endfor\n.endsection\n.section code,kind=code\n.long outer[1].inner[1],outer[1].inner[1].entry,outer[1].inner[1].value\n.endsection\n";
    for cpu in ["68020", "6502"] {
        for placed in [false, true] {
            let assembler = assemble(cpu, body, placed, "hunk");
            let bytes = payload(&assembler).expect("nested records retain typed member values");
            let segments = hunk::segments(&bytes).unwrap();
            assert_eq!(segments[0].payload, longs(cpu, &[3, 3, 12]));
            assert_eq!(segments[0].relocations, [(0, 1), (4, 1)]);
            assert_eq!(segments[1].payload, [7, 8, 7, 8]);
            assert!(segments[1].relocations.is_empty());
        }
    }
}

#[test]
fn hunk_repeat_records_whole_record_alias_members() {
    let body = ".section data,kind=data\nitems .bfor 2\nentry .byte 7\nvalue .const 13\n.endfor\nsaved .const items[1]\n.endsection\n.section code,kind=code\n.long saved.entry,saved.value\n.endsection\n";
    for cpu in ["68020", "6502"] {
        let assembler = assemble(cpu, body, true, "hunk");
        let bytes = payload(&assembler).expect("record aliases retain their members' provenance");
        let segments = hunk::segments(&bytes).unwrap();
        assert_eq!(segments[0].payload, longs(cpu, &[1, 13]));
        assert_eq!(segments[0].relocations, [(0, 1)]);
        assert_eq!(segments[1].payload, [7, 7, 0, 0]);
        assert!(segments[1].relocations.is_empty());
    }
}

#[test]
fn hunk_repeat_records_while_member_snapshots() {
    let body = "n .var 0\n.section data,kind=data\nitems .bwhile n<2\nentry .byte n\nvalue .const n+10\nn += 1\n.endwhile\n.endsection\n.section code,kind=code\n.long items[1],items[1].entry,items[0].value,items[1].value\n.endsection\n";
    for cpu in ["68020", "6502"] {
        for placed in [false, true] {
            let assembler = assemble(cpu, body, placed, "hunk");
            let bytes = payload(&assembler).expect("while records freeze each iteration's members");
            let segments = hunk::segments(&bytes).unwrap();
            assert_eq!(segments[0].payload, longs(cpu, &[1, 1, 10, 11]));
            assert_eq!(segments[0].relocations, [(0, 1), (4, 1)]);
            assert_eq!(segments[1].payload, [0, 1, 0, 0]);
            assert!(segments[1].relocations.is_empty());
        }
    }
}

#[test]
fn hunk_repeat_records_nested_flat_indexing_without_host_fallback() {
    struct FailpointReset;
    impl Drop for FailpointReset {
        fn drop(&mut self) {
            set_host_expr_eval_failpoint_for_tests(false);
        }
    }
    let _reset = FailpointReset;
    set_host_expr_eval_failpoint_for_tests(true);
    let body = ".section code,kind=code\nstart\nouter .bfor item in {2,4}\nentry .byte item\ninner .bfor child in {7,8}\nentry .byte child\nvalue .const item+child\n.endfor\n.byte inner[1]-entry,inner[1].entry-entry,inner[1].value\n.endfor\n.byte outer[1]-start,outer[1].entry-start\nvalues={11,13}\n.byte values[1]\n.endsection\n.section data,kind=data\n.byte 9\n.endsection\n";
    for cpu in ["68020", "6502"] {
        let assembler = assemble(cpu, body, true, "bin");
        assert_eq!(
            payload(&assembler).unwrap(),
            [2, 7, 8, 2, 2, 10, 4, 7, 8, 2, 2, 12, 6, 6, 13, 9]
        );
    }
}

#[test]
fn hunk_repeat_records_lists_retain_record_aliases_and_addresses() {
    let body = ".section data,kind=data\nitems .bfor 2\nentry .byte 7\n.endfor\nfirst .const items[1]\nselected={first}\naddresses={items[0].entry,items[1].entry}\n.endsection\n.section code,kind=code\n.long selected[0].entry,addresses[1]\n.endsection\n";
    for cpu in ["68020", "6502"] {
        for placed in [false, true] {
            let assembler = assemble(cpu, body, placed, "hunk");
            let bytes = payload(&assembler).unwrap();
            let segments = hunk::segments(&bytes).unwrap();
            assert_eq!(segments[0].payload, longs(cpu, &[1, 1]));
            assert_eq!(segments[0].relocations, [(0, 1), (4, 1)]);
        }
    }
}

#[test]
fn hunk_repeat_records_forward_list_bindings_gain_resolved_provenance() {
    let body = ".section code,kind=code\nforward = {tail}\nalias = forward\n.long forward[0],alias[0]\n.endsection\n.section data,kind=data\ntail .byte 77\n.endsection\n";
    for cpu in ["68020", "6502"] {
        for placed in [false, true] {
            let assembler = assemble(cpu, body, placed, "hunk");
            let bytes = payload(&assembler).unwrap();
            let segments = hunk::segments(&bytes).unwrap();
            assert_eq!(segments[0].payload, longs(cpu, &[0, 0]));
            assert_eq!(segments[0].relocations, [(0, 1), (4, 1)]);
            assert_eq!(segments[1].payload, [77, 0, 0, 0]);
        }
    }
}

#[test]
fn hunk_repeat_records_forced_host_scalar_route_keeps_shared_member_provenance() {
    use types::asm_value::{AsmValue, ScalarProvenance};
    for (cpu, family) in [(m68020_cpu_id, "motorola68000"), (m6502_cpu_id, "mos6502")] {
        let mut symbols = SymbolTable::new();
        let registry = default_registry();
        let mut asm = AsmLine::with_cpu(&mut symbols, cpu, &registry);
        asm.opthread_expr_eval_force_host_families
            .push(family.into());
        assert_ne!(
            asm.process(".section data,kind=data", 1, 0, 2),
            LineStatus::Error
        );
        let address = AsmValue::RelocatableScalar {
            value: 3,
            provenance: ScalarProvenance::Section("data".into()),
        };
        asm.set_value_symbol(
            "items",
            AsmValue::Values(vec![AsmValue::Record {
                scalar: Box::new(address.clone()),
                fields: vec![("entry".into(), address)],
            }]),
        );
        assert_eq!(
            asm.process("alias .const items[0].entry", 2, 0, 2),
            LineStatus::DirEqu,
            "{:?}",
            asm.error()
        );
        let alias = Expr::Identifier("alias".into(), Span::default());
        let target = AssemblerContext::absolute_relocation(&asm, &alias)
            .unwrap()
            .unwrap();
        assert_eq!(target.target, "data");
        assert_eq!(target.value, 3);
        assert_eq!(target.addend, 3);
    }
}
