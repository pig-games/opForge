//! Immediate section addresses stored in imported frame displacements.
use super::*;
use vm::binary_source_package::BinarySourcePackage;

const CALLBACKS: &str = r#".module probe
.cpu m68020
.use app
.section code,kind=code
entry
 move.l #allocateOutput,app.Frame.Allocate(a0)
 move.l #payload,app.Frame.Context(a0)
 move.l #reserved,app.Frame.Work(a0)
allocateOutput
 rts
.align 4
.endsection
.section data,kind=data
payload .byte $aa,$bb,$cc
.endsection
.section bss,kind=bss
reserved .res long,3
.endsection
.output "build/sections.hunk",format=hunk,sections=code,bss,data
.endmodule
.module app
.cpu m68020
.pub
Frame .struct
Pad .long ?
Allocate .long ?
Context .long ?
Work .long ?
.endstruct
.endmodule
"#;

const BLOCK_CALLBACKS: &str = r#".module probe
.cpu m68020
Frame .struct
Pad .long ?
Allocate .long ?
Context .long ?
AddReloc .long ?
Work .long ?
.endstruct
.section code,kind=code
execute .block
 move.l #allocateOutput,Frame.Allocate(a0)
 move.l #Context,Frame.Context(a0)
 move.l #appendReloc,Frame.AddReloc(a0)
 move.l #reserved,Frame.Work(a0)
 rts
.bend
allocateOutput .block
 rts
.bend
appendReloc .block
 rts
.bend
.align 4
.endsection
.section data,kind=data
Context .byte $aa,$bb,$cc
.endsection
.section bss,kind=bss
reserved .res long,3
.endsection
.output "build/sections.hunk",format=hunk,sections=code,bss,data
.endmodule
"#;

fn constants_source() -> String {
    CALLBACKS
        .replace(".use app\n", ".use app\nLimit=8\n")
        .replace("#allocateOutput", "#Limit")
        .replace("#payload", "#8")
        .replace("#reserved", "#Limit+1")
}

#[test]
fn compact_callback_constants_rust_hunk_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&constants_source());
    assert!(oracle.windows(24).any(|bytes| bytes
        == [
            0x21, 0x7c, 0, 0, 0, 8, 0, 4, 0x21, 0x7c, 0, 0, 0, 8, 0, 8, 0x21, 0x7c, 0, 0, 0, 9, 0,
            12,
        ]));
    assert!(
        !oracle
            .chunks_exact(4)
            .any(|bytes| bytes == 0x3ec_u32.to_be_bytes()),
        "absolute constants and literals must not create Hunk relocations"
    );
}

#[test]
#[ignore = "requires configured FS-UAE; symbolic absolute constants and numeric immediates"]
fn compact_callback_constants_fs_uae() {
    hunk_sections::native_hunk_source(&constants_source());
}

#[test]
#[ignore = "requires configured FS-UAE; compound immediate address remains fail-closed"]
fn compact_callback_compound_address_barrier_fs_uae() {
    let source = CALLBACKS.replace("#allocateOutput", "#allocateOutput+2");
    assert_native_files_rejection(
        &[("input.asm", &source)],
        "m68020",
        Some("[file 00000001, line 00000006]"),
    );
}

#[test]
fn compact_callback_block_scope_rust_hunk_oracle() {
    let oracle = hunk_sections::rust_hunk_source(BLOCK_CALLBACKS);
    assert!(oracle
        .windows(8)
        .any(|bytes| bytes == [0x21, 0x7c, 0, 0, 0, 34, 0, 4]));
    assert!(oracle
        .windows(8)
        .any(|bytes| bytes == [0x21, 0x7c, 0, 0, 0, 36, 0, 12]));
}

#[test]
#[ignore = "requires configured FS-UAE; forward block-scoped callbacks and same-module frame"]
fn compact_callback_block_scope_fs_uae() {
    hunk_sections::native_hunk_source(BLOCK_CALLBACKS);
}

#[test]
fn compact_callback_section_addresses_rust_hunk_oracle() {
    let oracle = hunk_sections::rust_hunk_source(CALLBACKS);
    assert!(oracle.windows(24).any(|bytes| bytes
        == [
            0x21, 0x7c, 0, 0, 0, 24, 0, 4, 0x21, 0x7c, 0, 0, 0, 0, 0, 8, 0x21, 0x7c, 0, 0, 0, 0, 0,
            12,
        ]));
    let words: Vec<_> = oracle
        .chunks_exact(4)
        .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap()))
        .collect();
    assert!(
        words
            .windows(12)
            .any(|words| words == [0x3ec, 1, 0, 2, 1, 1, 18, 1, 2, 10, 0, 0x3f2,]),
        "CODE, BSS and DATA targets must each retain their immediate relocation"
    );
}

#[test]
fn compact_callback_package_sequence_inventory() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let wire = prepare_package(&core, &resolved).unwrap();
    let rows = u32::from_be_bytes(wire[16..20].try_into().unwrap()) as usize;
    let count = u32::from_be_bytes(wire[20..24].try_into().unwrap()) as usize;
    let move_id = numeric
        .names
        .iter()
        .position(|name| name == "move")
        .unwrap() as u16;
    let qualifier = numeric
        .qualifiers
        .iter()
        .position(|name| name == "l")
        .unwrap() as u8
        + 1;
    let mut immediate_rows = Vec::new();
    for index in 0..count {
        let row = rows + index * 32;
        if u16::from_be_bytes(wire[row..row + 2].try_into().unwrap()) == move_id
            && wire[row + 2] == qualifier
            && wire[row + 3] == 8
        {
            if u16::from_be_bytes(wire[row + 6..row + 8].try_into().unwrap()) == 111 {
                let stage =
                    u32::from_be_bytes(wire[row + 12..row + 16].try_into().unwrap()) as usize;
                let inputs =
                    u32::from_be_bytes(wire[stage + 8..stage + 12].try_into().unwrap()) as usize;
                assert_eq!(wire[stage], 0, "first stage must match without emitting");
                assert_eq!(
                    &wire[inputs..inputs + 2],
                    &[17, 0],
                    "callback source must use the atomic target predicate"
                );
            }
            immediate_rows.push((
                u16::from_be_bytes(wire[row + 6..row + 8].try_into().unwrap()),
                wire[row + 5],
                wire[row + 19],
                u16::from_be_bytes(wire[row + 22..row + 24].try_into().unwrap()),
            ));
        }
    }
    assert!(
        immediate_rows.contains(&(111, 9, 0, 0)),
        "canonical address/fixup/displacement sequence must be executable"
    );
    assert_eq!(
        immediate_rows
            .iter()
            .filter(|row| row.1 == 6)
            .copied()
            .collect::<Vec<_>>(),
        [(99, 6, 0x56, 0), (120, 6, 0x56, 0), (121, 6, 0x56, 0)],
        "earlier unsupported rows must retain their scalar/member match facts"
    );
    assert_eq!(
        immediate_rows.iter().filter(|row| row.0 == 111).count(),
        1,
        "the exact-address priority must be unique"
    );
    assert!(
        immediate_rows.iter().position(|row| row.0 == 111).unwrap()
            < immediate_rows.iter().position(|row| row.0 == 115).unwrap(),
        "the relocation sequence must precede the numeric fallback"
    );
}

#[test]
#[ignore = "requires configured FS-UAE; immediate CODE/DATA/BSS frame pointers"]
fn compact_callback_section_addresses_fs_uae() {
    hunk_sections::native_hunk_source(CALLBACKS);
}
