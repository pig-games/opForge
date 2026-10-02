//! Statement mnemonics do not reserve bare source-expression names.
use super::*;
use vm::binary_source_package::{BinarySourcePackage, CandidateRecipe, Projection};

const SOURCE: &str = r#".cpu 68020
.org 0
order .block
reset
 nop
 bra.w reset
 bra.w stop
stop
 rts
.bend
 reset
 move.w #8,d0
 tst.b 0(a0,d3.w)
 move.w (8).w,d0
.cpu "m68020"
 .long 68020
.end
"#;

const DATA_SOURCE: &str = r#".cpu 6502
.org 0
first .block
lda=7
 .byte lda
 lda #1
.bend
.cpu "m6502"
 .word 6502
.end
"#;

fn oracle(source: &str) -> Vec<u8> {
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("live Rust mnemonic-label oracle");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn compact_mnemonic_labels_rust_oracles() {
    assert_eq!(
        oracle(SOURCE),
        [
            0x4e, 0x71, 0x60, 0, 0xff, 0xfc, 0x60, 0, 0, 2, 0x4e, 0x75, 0x4e, 0x70, 0x30, 0x3c, 0,
            8, 0x4a, 0x30, 0x30, 0, 0x30, 0x38, 0, 8, 0, 1, 9, 0xb4,
        ]
    );
    assert_eq!(oracle(DATA_SOURCE), [7, 0xa9, 1, 0x66, 0x19]);
}

#[test]
#[ignore = "requires configured FS-UAE; mnemonic labels plus package operand and CPU-name controls"]
fn compact_mnemonic_labels_fs_uae() {
    assert_binary_source(SOURCE.into(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; mnemonic constants and numeric/quoted CPU names"]
fn compact_mnemonic_data_fs_uae() {
    // Isolate the mnemonic-name control from the quoted CPU-name control.
    assert_binary_source(DATA_SOURCE.replace(".cpu \"m6502\"\n", ""), "m6502".into());
    assert_binary_source(DATA_SOURCE.into(), "m6502".into());
}

fn wire() -> Vec<u8> {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    prepare_package(&core, &resolved).unwrap()
}

fn dictionary_offsets(wire: &[u8]) -> BTreeMap<String, usize> {
    let long = |offset| u32::from_be_bytes(wire[offset..offset + 4].try_into().unwrap()) as usize;
    let mut offset = long(8);
    let mut entries = BTreeMap::new();
    for _ in 0..long(12) {
        let length = u16::from_be_bytes(wire[offset..offset + 2].try_into().unwrap()) as usize;
        let spelling = std::str::from_utf8(&wire[offset + 6..offset + 6 + length]).unwrap();
        entries.insert(spelling.to_string(), offset);
        offset = (offset + 6 + length + 1) & !1;
    }
    entries
}

#[test]
fn compact_mnemonic_dictionary_roles() {
    let wire = wire();
    assert_eq!(&wire[..4], b"BS16");
    let offsets = dictionary_offsets(&wire);
    for spelling in ["reset", "word", "m68020", "68020"] {
        assert_eq!(wire[offsets[spelling] + 5], 0, "{spelling} is contextual");
    }
    for spelling in ["d0", "d3.w", "cacr"] {
        assert_eq!(
            wire[offsets[spelling] + 5],
            1,
            "{spelling} owns operand identity"
        );
    }
    assert_eq!(
        wire[offsets["w"] + 5],
        2,
        "w owns member identity only after a dot"
    );
    assert!(offsets.values().all(|offset| wire[offset + 5] <= 3));
}

const QUOTED_CONTROL: &str = ".cpu \"m68020\"\n .byte \"reset\"\n rts\n.end\n";

#[test]
fn compact_mnemonic_quoted_name_rust_oracle() {
    assert_eq!(
        oracle(QUOTED_CONTROL),
        [0x72, 0x65, 0x73, 0x65, 0x74, 0x4e, 0x75]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; only configured package-name operands lower quoted names"]
fn compact_mnemonic_quoted_name_fs_uae() {
    assert_binary_source(QUOTED_CONTROL.into(), "m68020".into());
}

fn reject_wire(wire: &[u8]) {
    let source = ".cpu m68020\n rts\n.end\n";
    let outcome = crate::fs_uae_smoke::run_binary_source_rejection_from_env(
        &workspace_root(),
        wire,
        &[("input.asm", source.as_bytes())],
        None,
    )
    .expect("fresh native rejection for invalid dictionary contract");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
}

#[test]
#[ignore = "requires configured FS-UAE; unused dictionary entries must reject unknown roles"]
fn compact_mnemonic_unknown_dictionary_role_fs_uae() {
    let mut wire = wire();
    let reset = dictionary_offsets(&wire)["reset"];
    wire[reset + 5] = 4;
    // RESET never occurs in reject_wire's valid source; validate the whole dictionary.
    reject_wire(&wire);
}

#[test]
#[ignore = "requires configured FS-UAE; BS10 does not carry the BS12 lexical-role contract"]
fn compact_mnemonic_stale_contract_fs_uae() {
    let mut wire = wire();
    wire[..4].copy_from_slice(b"BS10");
    reject_wire(&wire);
}

const MEMBER_SOURCE: &str = r#".cpu m68020
.org 0
loops .block
w
 nop
 bra.w w
 bra.w finish
finish
 rts
.bend
values .block
w=7
Frame .struct
w .word ?
.endstruct
 .byte w,Frame.w
 .word Frame
 tst.b 0(a0,d3.w)
 move.w (8).w,d0
.bend
.end
"#;

#[test]
fn compact_mnemonic_member_context_rust_oracle() {
    assert_eq!(
        oracle(MEMBER_SOURCE),
        [
            0x4e, 0x71, 0x60, 0, 0xff, 0xfc, 0x60, 0, 0, 2, 0x4e, 0x75, 7, 0, 0, 2, 0x4a, 0x30,
            0x30, 0, 0x30, 0x38, 0, 8,
        ]
    );
    let (_, diagnostics) = assemble_source_entries_with_runtime_mode(
        &".cpu m68020\nscope .block\nd0\n nop\n bra.w d0\n.bend\n.end\n"
            .lines()
            .collect::<Vec<_>>(),
        true,
    )
    .unwrap();
    assert!(
        !diagnostics.is_empty(),
        "a bare register is not a branch expression"
    );
}

#[test]
#[ignore = "requires configured FS-UAE; bare member spellings are symbols outside member context"]
fn compact_mnemonic_member_context_fs_uae() {
    assert_binary_source(MEMBER_SOURCE.into(), "m68020".into());
}

fn primary_source() -> String {
    SOURCE.replace(" move.w (8).w,d0\n", "")
}

const BARE_MEMBER_SOURCE: &str = r#".cpu m68020
.org 0
loops .block
w
 nop
 bra.w w
 bra.w finish
finish
 rts
.bend
values .block
w=7
 .byte w
.bend
.end
"#;

fn field_source(scoped: bool) -> String {
    let frame = r#"Frame .struct
w .word ?
.endstruct
 .byte Frame.w
 .word Frame
"#;
    let body = if scoped {
        format!("values .block\n{frame}.bend\n")
    } else {
        frame.into()
    };
    format!(".cpu m68020\n.org 0\n{body}.end\n")
}

#[test]
fn compact_mnemonic_isolation_rust_oracles() {
    let mut primary = oracle(SOURCE);
    primary.drain(22..26);
    assert_eq!(oracle(&primary_source()), primary);
    assert_eq!(
        oracle(BARE_MEMBER_SOURCE),
        [0x4e, 0x71, 0x60, 0, 0xff, 0xfc, 0x60, 0, 0, 2, 0x4e, 0x75, 7]
    );
    for scoped in [false, true] {
        assert_eq!(oracle(&field_source(scoped)), [0, 0, 2]);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; primary26B proof separate from original30B snapshot"]
fn compact_mnemonic_primary_fs_uae() {
    assert_binary_source(primary_source(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; barew branch and assigned data expression isolation"]
fn compact_mnemonic_bare_member_fs_uae() {
    assert_binary_source(BARE_MEMBER_SOURCE.into(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; module-root versus block-local qualified w field isolation"]
fn compact_mnemonic_field_scope_fs_uae() {
    for scoped in [false, true] {
        assert_binary_source(field_source(scoped), "m68020".into());
    }
}

#[test]
#[ignore = "requires configured FS-UAE; current member-value input convergence readiness"]
fn compact_mnemonic_member_value_known_gap_fs_uae() {
    assert_native_files_rejection(
        &[("input.asm", SOURCE)],
        "m68020",
        Some("[file 00000001, line 0000000E]"),
    );
}

#[test]
fn compact_mnemonic_member_package_barrier() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let wire = prepare_package(&core, &resolved).unwrap();
    let word = |offset| u16::from_be_bytes(wire[offset..offset + 2].try_into().unwrap());
    let rows = u32::from_be_bytes(wire[16..20].try_into().unwrap()) as usize;
    let count = u32::from_be_bytes(wire[20..24].try_into().unwrap()) as usize;
    let mut barriers = 0;
    for candidate in &numeric.candidates {
        if numeric.names[candidate.mnemonic as usize] != "move"
            || numeric.names[candidate.shape as usize] != "direct_register"
            || candidate
                .qualifier
                .is_none_or(|q| numeric.qualifiers[q as usize] != "w")
        {
            continue;
        }
        let CandidateRecipe::SemanticInputs { inputs, .. } = &candidate.recipe else {
            continue;
        };
        if !inputs.iter().any(|input| {
            matches!(input, Projection::RequiredValueProgram { source, .. }
                if matches!(source.as_ref(), Projection::Member { operand: 0, qualifier }
                    if numeric.names[*qualifier as usize].eq_ignore_ascii_case("w")))
        }) {
            continue;
        }
        let row = (0..count)
            .map(|index| rows + index * 32)
            .find(|&row| {
                word(row) == candidate.mnemonic
                    && wire[row + 2] == candidate.qualifier.unwrap() + 1
                    && wire[row + 3] == 6
                    && wire[row + 4] == candidate.owner_rank
                    && word(row + 6) == candidate.priority
                    && word(row + 20) == candidate.mode
            })
            .expect("serialized canonical member candidate");
        assert_eq!(
            wire[row + 5],
            6,
            "current member-value export remains closed"
        );
        assert_eq!(word(row + 10), 0, "no executable inputs for a barrier");
        assert_eq!(&wire[row + 12..row + 16], &[0; 4]);
        barriers += 1;
    }
    assert!(
        barriers > 0,
        "fixture must exercise a canonical member-value export barrier"
    );
}
