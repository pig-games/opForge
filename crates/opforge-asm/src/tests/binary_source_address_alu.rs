//! Package-owned address ALU sources and their live Hunk relocations.
use super::*;

const FORMS: [(&str, u16); 6] = [
    ("adda.w", 0xd0f9),
    ("adda.l", 0xd1f9),
    ("suba.w", 0x90f9),
    ("suba.l", 0x91f9),
    ("cmpa.w", 0xb0f9),
    ("cmpa.l", 0xb1f9),
];

fn memory_source() -> String {
    let mut body = String::new();
    for target in ["reserved", "payload", "entry", "Limit"] {
        for (register, (mnemonic, _)) in FORMS.iter().enumerate() {
            body.push_str(&format!(" {mnemonic} {target},a{register}\n"));
        }
    }
    format!(
        ".module address_alu_probe\n.cpu m68020\n.use state\nLimit=8\nOffset=2\n.section code,kind=code\nentry:\n{body} rts\n.endsection\n.section data,kind=data\npayload: .long 0,0\n.endsection\n.section bss,kind=bss\nreserved: .res long,2\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n.module state\n.cpu m68020\n.pub\n.section bss,kind=bss\nCount: .res long,1\n.endsection\n.endmodule\n"
    )
}

fn long(bytes: &[u8], offset: usize) -> usize {
    u32::from_be_bytes(bytes[offset..offset + 4].try_into().unwrap()) as usize
}

fn code_and_relocations(bytes: &[u8]) -> (&[u8], BTreeMap<usize, usize>) {
    let kind = 20 + long(bytes, 8) * 4;
    assert_eq!(long(bytes, kind), 0x3e9);
    let start = kind + 8;
    let end = start + long(bytes, kind + 4) * 4;
    let mut relocations = BTreeMap::new();
    let mut cursor = end;
    if long(bytes, cursor) == 0x3ec {
        cursor += 4;
        loop {
            let count = long(bytes, cursor);
            cursor += 4;
            if count == 0 {
                break;
            }
            let target = long(bytes, cursor);
            cursor += 4;
            for _ in 0..count {
                assert!(relocations.insert(long(bytes, cursor), target).is_none());
                cursor += 4;
            }
        }
    }
    assert_eq!(long(bytes, cursor), 0x3f2);
    (&bytes[start..end], relocations)
}

#[test]
fn compact_address_alu_memory_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&memory_source());
    let (code, relocations) = code_and_relocations(&oracle);
    let mut expected_relocations = BTreeMap::new();
    for (group, target) in [2, 1, 0].into_iter().enumerate() {
        for (register, (_, opcode)) in FORMS.iter().enumerate() {
            let offset = (group * 6 + register) * 6;
            assert_eq!(
                &code[offset..offset + 2],
                &(opcode + ((register as u16) << 9)).to_be_bytes()
            );
            expected_relocations.insert(offset + 2, target);
        }
    }
    for (register, (_, opcode)) in FORMS.iter().enumerate() {
        let offset = (18 + register) * 6;
        assert_eq!(
            &code[offset..offset + 2],
            &(opcode + ((register as u16) << 9)).to_be_bytes()
        );
        assert_eq!(&code[offset + 2..offset + 6], &[0, 0, 0, 8]);
    }
    assert_eq!(relocations, expected_relocations);
}

#[test]
#[ignore = "requires configured FS-UAE; all address ALU widths, sections and constants"]
fn compact_address_alu_memory_fs_uae() {
    hunk_sections::native_hunk_source(&memory_source());
}

const CONTROLS: &str = ".cpu m68020\n.org 0\nOffset=4\n adda.w d0,a1\n adda.l a0,a1\n suba.w d1,a2\n suba.l a3,a3\n cmpa.w a4,a5\n cmpa.l d6,a7\n adda.w #8,a0\n suba.l #8,a1\n cmpa.l #8,a2\n adda.l Offset(a1),a2\n suba.w 4(a2),a3\n cmpa.l 4(a3),a4\n adda.w (8).w,a0\n suba.l (8).w,a1\n cmpa.l (8).w,a2\n.end\n";

fn imported_and_affine_source() -> String {
    let mut body = String::new();
    for (register, (mnemonic, _)) in FORMS.iter().enumerate() {
        body.push_str(&format!(" {mnemonic} state.Count,a{register}\n"));
    }
    for ((mnemonic, _), target) in FORMS.iter().zip([
        "reserved+2",
        "2+reserved",
        "reserved+Offset",
        "reserved-2",
        "state.Count+2",
        "payload+2",
    ]) {
        body.push_str(&format!(" {mnemonic} {target},a0\n"));
    }
    memory_source()
        .replace("entry:\n", &format!("entry\n{body}"))
        .replace("payload:", "payload")
        .replace("reserved:", "reserved")
        .replace("Count:", "Count")
}

#[test]
fn compact_address_alu_imported_affine_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&imported_and_affine_source());
    let (code, relocations) = code_and_relocations(&oracle);
    assert_eq!(relocations.len(), 30);
    for (register, (_, opcode)) in FORMS.iter().enumerate() {
        let offset = register * 6;
        assert_eq!(
            &code[offset..offset + 2],
            &(opcode + ((register as u16) << 9)).to_be_bytes()
        );
        assert_eq!(relocations[&(offset + 2)], 2);
    }
    for (index, (_, opcode)) in FORMS.iter().enumerate() {
        let offset = (6 + index) * 6;
        assert_eq!(&code[offset..offset + 2], &opcode.to_be_bytes());
        assert_eq!(relocations[&(offset + 2)], if index == 5 { 1 } else { 2 });
    }
    assert!((30..36).all(|index| !relocations.contains_key(&(index * 6 + 2))));
}

#[test]
#[ignore = "requires configured FS-UAE; imported address ALU names and affine relocation identities"]
fn compact_address_alu_imported_affine_fs_uae() {
    hunk_sections::native_hunk_source(&imported_and_affine_source());
}

#[test]
#[ignore = "requires configured FS-UAE; unsafe address ALU relocation algebra fails closed"]
fn compact_address_alu_unsafe_affine_fs_uae() {
    for target in [
        "state.Count+reserved",
        "reserved*2",
        "2-reserved",
        "-reserved",
    ] {
        let source = imported_and_affine_source().replacen(
            " adda.w state.Count,a0",
            &format!(" adda.w {target},a0"),
            1,
        );
        assert_native_files_rejection(
            &[("input.asm", &source)],
            "m68020",
            Some("[file 00000001, line 00000008]"),
        );
    }
}

#[test]
fn compact_address_alu_memory_packet_sequences() {
    use vm::binary_source_package::{BinarySourcePackage, CandidateRecipe, Projection};
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    for cpu in ["m68000", "m68020"] {
        let resolved = core.resolve_pipeline(cpu, None).unwrap();
        let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
        let packet = prepare_package(&core, &resolved).unwrap();
        for (mnemonic, _) in FORMS {
            let (base, size) = mnemonic.split_once('.').unwrap();
            let candidate = numeric
                .candidates
                .iter()
                .find(|candidate| {
                    numeric.names[usize::from(candidate.mnemonic)] == base
                        && candidate.qualifier.is_some_and(|qualifier| {
                            numeric.qualifiers[usize::from(qualifier)] == size
                        })
                        && numeric.names[usize::from(candidate.shape)] == "direct_register"
                        && candidate.priority == 9
                })
                .expect("family-owned bare-memory candidate");
            let CandidateRecipe::SemanticSequence { stages } = &candidate.recipe else {
                panic!("bare-memory candidate must lower to a numeric sequence");
            };
            assert_eq!(stages.len(), 3);
            assert_eq!(stages[0].program, None);
            assert_eq!(
                stages[0].inputs,
                [
                    Projection::TargetExpression(0),
                    Projection::Register {
                        operand: 1,
                        class: 1
                    }
                ]
            );
            assert_eq!(
                numeric.names[usize::from(stages[1].program.unwrap())],
                "enc.template.field-9"
            );
            assert!(stages[2].fixup);
            assert_eq!(stages[2].inputs, [Projection::TargetExpression(0)]);
            let row = (0..long(&packet, 20))
                .map(|index| long(&packet, 16) + index * crate::binary_source_experiment::ROW)
                .find(|&row| {
                    u16::from_be_bytes(packet[row..row + 2].try_into().unwrap())
                        == candidate.mnemonic
                        && packet[row + 2] == candidate.qualifier.unwrap() as u8 + 1
                        && packet[row + 3] == 6
                        && u16::from_be_bytes(packet[row + 6..row + 8].try_into().unwrap()) == 9
                })
                .expect("exported bare-memory candidate");
            assert_eq!(packet[row + 5], 9);
            assert_eq!(
                u16::from_be_bytes(packet[row + 10..row + 12].try_into().unwrap()),
                3
            );
            let stages = long(&packet, row + 12);
            assert_eq!(
                [packet[stages], packet[stages + 12], packet[stages + 24]],
                [0, 1, 2]
            );
            let inputs = long(&packet, stages + 8);
            assert_eq!(&packet[inputs..inputs + 4], &[15, 0, 0, 0]);
            assert_eq!(&packet[inputs + 12..inputs + 16], &[1, 1, 0, 1]);
            let fixup_inputs = long(&packet, stages + 24 + 8);
            assert_eq!(&packet[fixup_inputs..fixup_inputs + 4], &[15, 0, 0, 0]);
        }
    }
}

fn rust_bytes(source: &str) -> Vec<u8> {
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn compact_address_alu_controls_rust_oracle() {
    assert_eq!(
        rust_bytes(CONTROLS),
        [
            0xd2, 0xc0, 0xd3, 0xc8, 0x94, 0xc1, 0x97, 0xcb, 0xba, 0xcc, 0xbf, 0xc6, 0xd0, 0xfc, 0,
            8, 0x93, 0xfc, 0, 0, 0, 8, 0xb5, 0xfc, 0, 0, 0, 8, 0xd5, 0xe9, 0, 4, 0x96, 0xea, 0, 4,
            0xb9, 0xeb, 0, 4, 0xd0, 0xf8, 0, 8, 0x93, 0xf8, 0, 8, 0xb5, 0xf8, 0, 8,
        ]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; address ALU register/immediate/displacement/numeric controls"]
fn compact_address_alu_controls_fs_uae() {
    assert_binary_source(CONTROLS.into(), "m68020".into());
}

const INVALID: [&str; 5] = [
    "adda.w target,d0",
    "suba.l target,d0",
    "cmpa.l target,d0",
    "cmpa.b d0,a0",
    "cmpa.l 8,a0",
];

fn invalid_source(instruction: &str) -> String {
    format!(".cpu m68020\n.org 0\n {instruction}\ntarget: .long 0\n.end\n")
}

#[test]
fn compact_address_alu_invalid_rust_oracle() {
    for instruction in INVALID {
        let source = invalid_source(instruction);
        let (_, diagnostics) =
            assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
                .unwrap();
        assert!(
            !diagnostics.is_empty(),
            "unexpected acceptance: {instruction}"
        );
    }
}

#[test]
fn compact_address_alu_member_packet_sequences() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric =
        vm::binary_source_package::BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let packet = prepare_package(&core, &resolved).unwrap();
    let field = numeric.names.iter().position(|name| name == "l").unwrap() as u16;
    for (mnemonic, _) in FORMS {
        let (base, size) = mnemonic.split_once('.').unwrap();
        let name = numeric.names.iter().position(|name| name == base).unwrap() as u16;
        let qualifier = numeric
            .qualifiers
            .iter()
            .position(|name| name == size)
            .unwrap() as u8
            + 1;
        let row = (0..long(&packet, 20))
            .map(|index| long(&packet, 16) + index * crate::binary_source_experiment::ROW)
            .find(|&row| {
                u16::from_be_bytes(packet[row..row + 2].try_into().unwrap()) == name
                    && packet[row + 2] == qualifier
                    && packet[row + 3] == 6
                    && u16::from_be_bytes(packet[row + 6..row + 8].try_into().unwrap()) == 5
                    && packet[row + 5] == 9
                    && {
                        // An indexed-address sequence also has priority 5.
                        // Select the package's member-shape match explicitly.
                        let stages = long(&packet, row + 12);
                        let inputs = long(&packet, stages + 8);
                        packet[stages] == 0 && packet[inputs..inputs + 2] == [19, 0]
                    }
            })
            .expect("explicit member candidate must be exported");
        assert_eq!(packet[row + 5], 9);
        assert_eq!(
            u16::from_be_bytes(packet[row + 10..row + 12].try_into().unwrap()),
            3
        );
        let stages = long(&packet, row + 12);
        assert_eq!(
            [packet[stages], packet[stages + 12], packet[stages + 24]],
            [0, 1, 2]
        );
        let inputs = long(&packet, stages + 8);
        assert_eq!(&packet[inputs..inputs + 2], &[19, 0]);
        assert_eq!(&packet[inputs + 2..inputs + 4], &field.to_be_bytes());
        assert_eq!(&packet[inputs + 12..inputs + 16], &[1, 1, 0, 1]);
        let fixup = long(&packet, stages + 24 + 8);
        assert_eq!(&packet[fixup..fixup + 2], &[16, 0]);
        assert_eq!(&packet[fixup + 2..fixup + 4], &field.to_be_bytes());
    }
}

#[test]
#[ignore = "requires configured FS-UAE; address ALU class, size and bare-literal rejection"]
fn compact_address_alu_invalid_fs_uae() {
    for instruction in INVALID {
        let source = invalid_source(instruction);
        assert_native_files_rejection(
            &[("input.asm", &source)],
            "m68020",
            Some("[file 00000001, line 00000003]"),
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; explicit absolute-long address ALU member matches Rust"]
fn compact_address_alu_member_fs_uae() {
    let source = invalid_source("cmpa.l (target).l,a0");
    assert_eq!(rust_bytes(&source), [0xb1, 0xf9, 0, 0, 0, 6, 0, 0, 0, 0]);
    assert_binary_source(source, "m68020".into());
}
