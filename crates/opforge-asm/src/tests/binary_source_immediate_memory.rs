//! Positive scalar immediates and forward memory targets across Hunk sections.
use super::*;
use vm::binary_source_package::{BinarySourcePackage, CandidateRecipe, Projection};

const WIDTHS: [(&str, &[u8]); 3] = [
    ("b", &[0x13, 0xfc, 0, 20]),
    ("w", &[0x33, 0xfc, 0, 20]),
    ("l", &[0x23, 0xfc, 0, 0, 0, 20]),
];

fn stores() -> String {
    let mut body = String::new();
    for target in ["CodeTarget", "DataTarget", "ReturnCode"] {
        for (width, _) in WIDTHS {
            body.push_str(&format!(" move.{width} #20,{target}\n"));
        }
    }
    body
}

fn hunk_source() -> String {
    format!(
        ".module immediate_memory_probe\n.cpu m68020\n.section code,kind=code\nentry\n{} rts\nCodeTarget .long 0\n.endsection\n.section data,kind=data\n.byte 1,2,3,4\nDataTarget .long 0\n.endsection\n.section bss,kind=bss\n.res byte,1\n.align 4\nReturnCode .res long,2\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule\n",
        stores()
    )
}

fn flat_source() -> String {
    format!(
        ".cpu m68020\n.org 0\n{} rts\nCodeTarget .long 0\nDataTarget .long 0\nReturnCode .long 0\n.end\n",
        stores()
    )
}

fn expected_code(values: [u32; 3]) -> Vec<u8> {
    let mut bytes = Vec::new();
    for value in values {
        for (_, prefix) in WIDTHS {
            bytes.extend_from_slice(prefix);
            bytes.extend_from_slice(&value.to_be_bytes());
        }
    }
    bytes.extend_from_slice(&[0x4e, 0x75]);
    bytes
}

fn long(bytes: &[u8], offset: usize) -> usize {
    u32::from_be_bytes(bytes[offset..offset + 4].try_into().unwrap()) as usize
}

fn code_and_relocations(oracle: &[u8]) -> (&[u8], BTreeMap<usize, usize>) {
    let kind = 20 + long(oracle, 8) * 4;
    assert_eq!(long(oracle, kind), 0x3e9);
    let start = kind + 8;
    let end = start + long(oracle, kind + 4) * 4;
    let relocations = &oracle[end..];
    let mut actual = BTreeMap::new();
    assert_eq!(long(relocations, 0), 0x3ec);
    let mut cursor = 4;
    loop {
        let count = long(relocations, cursor);
        cursor += 4;
        if count == 0 {
            break;
        }
        let target = long(relocations, cursor);
        cursor += 4;
        for _ in 0..count {
            assert!(actual.insert(long(relocations, cursor), target).is_none());
            cursor += 4;
        }
    }
    assert_eq!(long(relocations, cursor), 0x3f2);
    (&oracle[start..end], actual)
}

#[test]
fn compact_immediate_memory_hunk_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&hunk_source());
    let mut expected = expected_code([80, 4, 4]);
    expected.extend_from_slice(&[0; 4]);
    let (code, actual) = code_and_relocations(&oracle);
    assert_eq!(code, expected);
    assert_eq!(
        actual,
        BTreeMap::from([
            (4, 0),
            (12, 0),
            (22, 0),
            (30, 2),
            (38, 2),
            (48, 2),
            (56, 1),
            (64, 1),
            (74, 1),
        ])
    );
}

const LITERAL_CONTROLS: &str = ".cpu m68020\n.org 0\nValue=20\nNegative=-20\nDestination=$1234\nNegativeDestination=-4\n move.l #20,$1234\n move.l #20,Destination\n move.l #Value,$1234\n move.l #Value,Destination\n move.l #Negative,$1234\n move.l #Negative,Destination\n move.l #20,NegativeDestination\n.end\n";

const FLAT_LABEL_CONTROLS: &str = ".cpu m68020\n.org 0\n.byte 1,2,3,4\nStart .byte $11,$22\nEnd\nDestination=$1234\n move.l #Start*2,$1234\n move.l #End-Start,Destination\n move.l #-Start,End\n move.l #20,Start*2\n move.l #20,End-Start\n move.l #20,-Start\n move.l #End,Destination\n move.l #20,End\n rts\n.end\n";

#[test]
fn compact_immediate_memory_flat_label_controls_rust_oracle() {
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(
        &FLAT_LABEL_CONTROLS.lines().collect::<Vec<_>>(),
        true,
    )
    .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let mut expected = vec![1, 2, 3, 4, 0x11, 0x22];
    for (source, destination) in [
        (8_u32, 0x1234_u32),
        (2, 0x1234),
        (0xffff_fffc, 6),
        (20, 8),
        (20, 2),
        (20, 0xffff_fffc),
        (6, 0x1234),
        (20, 6),
    ] {
        expected.extend_from_slice(&[0x23, 0xfc]);
        expected.extend_from_slice(&source.to_be_bytes());
        expected.extend_from_slice(&destination.to_be_bytes());
    }
    expected.extend_from_slice(&[0x4e, 0x75]);
    assert_eq!(
        entries
            .into_iter()
            .map(|(_, byte)| byte)
            .collect::<Vec<_>>(),
        expected
    );
}

#[test]
#[ignore = "requires configured FS-UAE; flat labels remain scalar under multiplication, difference and negation"]
fn compact_immediate_memory_flat_label_controls_fs_uae() {
    assert_binary_source(FLAT_LABEL_CONTROLS.into(), "m68020".into());
}

const FLAT_FORWARD_LABEL_CONTROLS: &str = ".cpu m68020\n.org 0\n.byte 1,2,3,4\n move.l #Start*2,End-Start\n move.l #End-Start,-Start\n move.l #-Start,End\nStart .byte $11,$22\nEnd\n rts\n.end\n";

#[test]
fn compact_immediate_memory_flat_forward_label_controls_rust_oracle() {
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(
        &FLAT_FORWARD_LABEL_CONTROLS.lines().collect::<Vec<_>>(),
        true,
    )
    .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    // Start=34, End=36: both instruction operands remain scalar in flat output.
    let expected = [
        1, 2, 3, 4, 0x23, 0xfc, 0, 0, 0, 68, 0, 0, 0, 2, 0x23, 0xfc, 0, 0, 0, 2, 0xff, 0xff, 0xff,
        0xde, 0x23, 0xfc, 0xff, 0xff, 0xff, 0xde, 0, 0, 0, 36, 0x11, 0x22, 0x4e, 0x75,
    ];
    assert_eq!(
        entries
            .into_iter()
            .map(|(_, byte)| byte)
            .collect::<Vec<_>>(),
        expected
    );
}

#[test]
#[ignore = "requires configured FS-UAE; provisional forward labels become scalar flat operands"]
fn compact_immediate_memory_flat_forward_label_controls_fs_uae() {
    assert_binary_source(FLAT_FORWARD_LABEL_CONTROLS.into(), "m68020".into());
}

const UNDEFINED_IMMEDIATE_SOURCE: &str = ".cpu m68020\n.org 0\n move.l #Missing*2,$1234\n.end\n";

#[test]
fn compact_immediate_memory_undefined_source_rust_rejects() {
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(
        &UNDEFINED_IMMEDIATE_SOURCE.lines().collect::<Vec<_>>(),
        true,
    )
    .unwrap();
    assert!(
        entries.is_empty(),
        "undefined source emitted output: {entries:?}"
    );
    assert!(
        diagnostics
            .iter()
            .any(|diagnostic| diagnostic.contains("Label not found: Missing")),
        "missing expected undefined-label error: {diagnostics:?}"
    );
}

#[test]
#[ignore = "requires configured FS-UAE; permanently undefined immediate source must reject"]
fn compact_immediate_memory_undefined_source_fs_uae() {
    assert_native_rejection(UNDEFINED_IMMEDIATE_SOURCE, "m68020");
}

fn layout_alias_hunk(source: &str) -> String {
    format!(".module alias_probe\n.cpu m68020\n.section code,kind=code\nCodeTarget .long 0\nAlias=CodeTarget\n move.l #{source},DataTarget\n rts\n.endsection\n.section data,kind=data\nDataTarget .long 0\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,data\n.endmodule\n")
}

fn rust_layout_alias_hunk(source: &str) -> Result<Vec<u8>, String> {
    let dir = create_temp_dir("immediate-memory-layout-alias");
    fs::create_dir_all(dir.join("build")).unwrap();
    let input = dir.join("input.asm");
    fs::write(&input, layout_alias_hunk(source)).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "68020".to_string(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.clone());
    let result = run_with_validated_cli_with_context(&cli, &config)
        .map_err(|error| format!("{error:?}"))
        .map(|_| fs::read(dir.join("build/sections.hunk")).unwrap());
    fs::remove_dir_all(&dir).unwrap();
    result
}

#[test]
fn compact_immediate_memory_layout_alias_hunk_rust_oracle() {
    for (source, addend) in [("Alias", 0), ("Alias+4", 4)] {
        let oracle = rust_layout_alias_hunk(source).expect("aliases retain their relocation base");
        let (code, relocations) = code_and_relocations(&oracle);
        assert_eq!(
            code,
            [0, 0, 0, 0, 0x23, 0xfc, 0, 0, 0, addend, 0, 0, 0, 0, 0x4e, 0x75]
        );
        assert_eq!(relocations, BTreeMap::from([(6, 0), (10, 1)]));
    }
}

#[test]
fn compact_immediate_memory_layout_alias_direct_hunk_rust_oracle() {
    for (source, addend) in [("CodeTarget", 0), ("CodeTarget+4", 4)] {
        let oracle = rust_layout_alias_hunk(source).unwrap();
        assert_eq!(long(&oracle, 8), 2);
        let (code, relocations) = code_and_relocations(&oracle);
        assert_eq!(
            code,
            [0, 0, 0, 0, 0x23, 0xfc, 0, 0, 0, addend, 0, 0, 0, 0, 0x4e, 0x75]
        );
        assert_eq!(relocations, BTreeMap::from([(6, 0), (10, 1)]));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; opaque Hunk layout aliases must fail closed"]
fn compact_immediate_memory_layout_alias_hunk_barrier_fs_uae() {
    for source in ["Alias", "Alias+4"] {
        assert_native_rejection(&layout_alias_hunk(source), "m68020");
    }
}

const FLAT_LAYOUT_ALIAS: &str = ".cpu m68020\n.org 0\nCodeTarget .long 0\nAlias=CodeTarget\n move.l #Alias+4,DataTarget\n move.l #Alias,DataTarget\n rts\nDataTarget .long 0\n.end\n";

#[test]
fn compact_immediate_memory_layout_alias_flat_rust_oracle() {
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(
        &FLAT_LAYOUT_ALIAS.lines().collect::<Vec<_>>(),
        true,
    )
    .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert_eq!(
        entries
            .into_iter()
            .map(|(_, byte)| byte)
            .collect::<Vec<_>>(),
        [
            0, 0, 0, 0, 0x23, 0xfc, 0, 0, 0, 4, 0, 0, 0, 26, 0x23, 0xfc, 0, 0, 0, 0, 0, 0, 0, 26,
            0x4e, 0x75, 0, 0, 0, 0
        ]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; flat layout aliases remain numeric"]
fn compact_immediate_memory_layout_alias_flat_fs_uae() {
    assert_binary_source(FLAT_LAYOUT_ALIAS.into(), "m68020".into());
}

fn source_address_hunk() -> &'static str {
    ".module immediate_address_probe\n.cpu m68020\n.section code,kind=code\nentry\n move.l #CodeTarget,DataTarget\n move.l #CodeTarget+4,ReturnCode\n move.l #DataTarget,$1234\n rts\nCodeTarget .long 0\n.endsection\n.section data,kind=data\n.byte 1,2,3,4\nDataTarget .long 0\n.endsection\n.section bss,kind=bss\n.res byte,1\n.align 4\nReturnCode .res long,2\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule\n"
}

#[test]
fn compact_immediate_memory_literal_controls_rust_oracle() {
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(
        &LITERAL_CONTROLS.lines().collect::<Vec<_>>(),
        true,
    )
    .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let instruction = [0x23, 0xfc, 0, 0, 0, 20, 0, 0, 0x12, 0x34];
    let mut expected = instruction.repeat(4);
    expected.extend_from_slice(&[0x23, 0xfc, 0xff, 0xff, 0xff, 0xec, 0, 0, 0x12, 0x34].repeat(2));
    expected.extend_from_slice(&[0x23, 0xfc, 0, 0, 0, 20, 0xff, 0xff, 0xff, 0xfc]);
    assert_eq!(
        entries
            .into_iter()
            .map(|(_, byte)| byte)
            .collect::<Vec<_>>(),
        expected
    );
}

#[test]
fn compact_immediate_memory_source_address_rust_oracle() {
    let oracle = hunk_sections::rust_hunk_source(source_address_hunk());
    let (code, relocations) = code_and_relocations(&oracle);
    assert_eq!(
        code,
        [
            0x23, 0xfc, 0, 0, 0, 32, 0, 0, 0, 4, 0x23, 0xfc, 0, 0, 0, 36, 0, 0, 0, 4, 0x23, 0xfc,
            0, 0, 0, 4, 0, 0, 0x12, 0x34, 0x4e, 0x75, 0, 0, 0, 0,
        ]
    );
    assert_eq!(
        relocations,
        BTreeMap::from([(2, 0), (6, 2), (12, 0), (16, 1), (22, 2)])
    );
}

#[test]
#[ignore = "requires configured FS-UAE; numeric literal and absolute equate destinations"]
fn compact_immediate_memory_literal_controls_fs_uae() {
    assert_binary_source(LITERAL_CONTROLS.into(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; source and destination relocation identities"]
fn compact_immediate_memory_source_address_fs_uae() {
    hunk_sections::native_hunk_source(source_address_hunk());
}

#[test]
#[ignore = "requires configured FS-UAE; unsafe source or destination algebra must fail closed"]
fn compact_immediate_memory_unsafe_address_fs_uae() {
    for source in unsafe_address_sources() {
        assert_native_rejection(&source, "m68020");
    }
}

fn unsafe_address_sources() -> Vec<String> {
    let mut sources = Vec::new();
    for instruction in [
        " move.l #CodeTarget*2,DataTarget\n",
        " move.l #CodeTarget+DataTarget,ReturnCode\n",
        " move.l #CodeTarget,DataTarget*2\n",
        " move.l #CodeTarget,DataTarget+ReturnCode\n",
    ] {
        sources.push(source_address_hunk().replace(
            " move.l #CodeTarget,DataTarget\n move.l #CodeTarget+4,ReturnCode\n move.l #DataTarget,$1234\n",
            instruction,
        ));
    }
    sources
}

#[test]
fn compact_immediate_memory_unsafe_address_rust_rejects() {
    for source in unsafe_address_sources() {
        let dir = create_temp_dir("immediate-memory-unsafe");
        fs::create_dir_all(dir.join("build")).unwrap();
        let input = dir.join("input.asm");
        fs::write(&input, &source).unwrap();
        let cli = Cli::parse_from([
            "opForge".to_string(),
            input.to_string_lossy().into_owned(),
            "--cpu".to_string(),
            "68020".to_string(),
        ]);
        let mut config = validate_cli(&cli).unwrap();
        config.out_dir = Some(dir.clone());
        let result = run_with_validated_cli_with_context(&cli, &config);
        fs::remove_dir_all(&dir).unwrap();
        assert!(
            result.is_err(),
            "unsafe Hunk address was accepted: {source}"
        );
    }
}

#[test]
fn compact_immediate_memory_flat_rust_oracle() {
    let source = flat_source();
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .unwrap();
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let mut expected = expected_code([80, 84, 88]);
    expected.extend_from_slice(&[0; 12]);
    assert_eq!(
        entries
            .into_iter()
            .map(|(_, byte)| byte)
            .collect::<Vec<_>>(),
        expected
    );
}

#[test]
fn compact_immediate_memory_packet_sequence() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let packet = prepare_package(&core, &resolved).unwrap();
    for (width, _) in WIDTHS {
        let priority = if width == "l" { 130 } else { 122 };
        let candidate = numeric
            .candidates
            .iter()
            .find(|candidate| {
                numeric.names[usize::from(candidate.mnemonic)] == "move"
                    && candidate.qualifier.is_some_and(|qualifier| {
                        numeric.qualifiers[usize::from(qualifier)] == width
                    })
                    && numeric.names[usize::from(candidate.shape)] == "immediate_direct"
                    && candidate.priority == priority
            })
            .unwrap();
        let CandidateRecipe::SemanticSequence { stages } = &candidate.recipe else {
            panic!("immediate memory store must be an executable sequence");
        };
        if width == "l" {
            assert_eq!(stages.len(), 3);
            assert_eq!(stages[0].inputs, [Projection::Constant(0x23fc)]);
            for (operand, stage) in stages[1..].iter().enumerate() {
                assert!(stage.fixup);
                assert_eq!(stage.inputs, [Projection::Expression(operand as u8)]);
            }
            assert!(
                !numeric.candidates.iter().any(|other| {
                    other.mnemonic == candidate.mnemonic
                        && other.qualifier == candidate.qualifier
                        && other.shape == candidate.shape
                        && [100, 122].contains(&other.priority)
                }),
                "scalar fallback and destination-only long row must be removed"
            );
        } else {
            assert_eq!(stages.len(), 3);
            assert_eq!(
                stages[0].inputs,
                [Projection::Expression(0), Projection::TargetExpression(1)]
            );
            assert!(
                matches!(&stages[1].inputs[1], Projection::RequiredValueProgram { source, .. } if **source == Projection::Expression(0))
            );
            assert!(stages[2].fixup);
            assert_eq!(stages[2].inputs, [Projection::TargetExpression(1)]);
        }
        let rows = (0..long(&packet, 20))
            .map(|index| long(&packet, 16) + index * crate::binary_source_experiment::ROW)
            .filter(|&row| {
                u16::from_be_bytes(packet[row..row + 2].try_into().unwrap()) == candidate.mnemonic
                    && packet[row + 2] == candidate.qualifier.unwrap() as u8 + 1
                    && packet[row + 3] == 8
            })
            .map(|row| {
                (
                    u16::from_be_bytes(packet[row + 6..row + 8].try_into().unwrap()),
                    row,
                )
            })
            .collect::<BTreeMap<_, _>>();
        let row = rows[&priority];
        assert_eq!(packet[row + 5], 9);
        let steps = long(&packet, row + 12);
        if width == "l" {
            assert!(rows[&99] < rows[&130]);
            assert_eq!(
                packet[rows[&99] + 5],
                9,
                "explicit member target must carry its executable sequence"
            );
            let member_steps = long(&packet, rows[&99] + 12);
            assert_eq!(
                [
                    packet[member_steps],
                    packet[member_steps + 12],
                    packet[member_steps + 24]
                ],
                [0, 1, 2]
            );
            let member_inputs = long(&packet, member_steps + 8);
            assert_eq!(&packet[member_inputs..member_inputs + 4], &[0, 0, 0, 0]);
            assert_eq!(&packet[member_inputs + 12..member_inputs + 14], &[19, 1]);
            let field = numeric.names.iter().position(|name| name == "l").unwrap() as u16;
            assert_eq!(
                &packet[member_inputs + 14..member_inputs + 16],
                &field.to_be_bytes()
            );
            let member_fixup = long(&packet, member_steps + 32);
            assert_eq!(&packet[member_fixup..member_fixup + 2], &[16, 1]);
            assert_eq!(
                &packet[member_fixup + 2..member_fixup + 4],
                &field.to_be_bytes()
            );
            assert!(rows[&111] < rows[&130]);
            assert!(rows[&115] < rows[&130]);
            assert_eq!(
                [packet[steps], packet[steps + 12], packet[steps + 24]],
                [1, 2, 2]
            );
            for operand in 0..2 {
                let inputs = long(&packet, steps + (operand + 1) * 12 + 8);
                assert_eq!(&packet[inputs..inputs + 4], &[0, operand as u8, 0, 0]);
            }
        } else {
            let input = long(&packet, steps + 12 + 8) + 12;
            assert_eq!(&packet[input..input + 4], &[0, 0, 0, 0]);
            assert_ne!(
                u16::from_be_bytes(packet[input + 8..input + 10].try_into().unwrap()),
                u16::MAX
            );
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; positive immediate stores into CODE/DATA/BSS with forward targets"]
fn compact_immediate_memory_hunk_fs_uae() {
    hunk_sections::native_hunk_source(&hunk_source());
}

#[test]
#[ignore = "requires configured FS-UAE; positive immediate stores to forward flat targets"]
fn compact_immediate_memory_flat_fs_uae() {
    assert_binary_source(flat_source(), "m68020".into());
}
