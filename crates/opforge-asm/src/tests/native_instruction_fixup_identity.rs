//! Original source identities survive actual package-owned instruction fixups.
use super::native_iterable_loops::{oracle, scratch};
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use package::{compile_branch_program, BranchCandidateSpec, BranchProgramSpec, EncodingEndian};
use vm::binary_source_package::BinarySourcePackage;
use vm::runtime_model_core::RuntimeModelCore;

const SOURCE: &str = r#" .module probe
 .cpu 68020
 .org $1000
first .block
 .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 .bend
second .block
 .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 .bend
 move.l first,first
 move.l first,second
 move.l #first+4,d0
 move.l #7,d0
 bsr.w second
 bsr second
 .endmodule
"#;

fn prepare(dir: &Path) -> Vec<u8> {
    let core = RuntimeModelCore::from_registry(&engine::build_default_asm_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let package =
        crate::native_runtime_package::prepare_package_with_names(&core, &resolved).unwrap();
    let name = |text: &str| {
        package
            .names
            .iter()
            .position(|entry| entry.eq_ignore_ascii_case(text))
            .unwrap() as u16
    };
    let first = package.names.len() as u16;
    let second = first + 1;
    let qualified_first = second + 1;
    let qualified_second = second + 2;
    let projected = second + 3;
    let unknown = projected + 1;
    let qualifier = numeric
        .qualifiers
        .iter()
        .position(|entry| entry.eq_ignore_ascii_case("l"))
        .unwrap()
        + 1;
    let word_qualifier = numeric
        .qualifiers
        .iter()
        .position(|entry| entry.eq_ignore_ascii_case("w"))
        .unwrap()
        + 1;
    let byte_qualifier = numeric
        .qualifiers
        .iter()
        .position(|entry| entry.eq_ignore_ascii_case("b"))
        .unwrap()
        + 1;
    let symbol = |id: u16| vec![0x81, 4, 0x12, id as u8, (id >> 8) as u8, 0];
    let register = |id: u16| vec![0, (id >> 8) as u8, id as u8, 0];
    let mut repeated = symbol(first);
    repeated.push(4);
    repeated.extend(symbol(first));
    let mut distinct = symbol(first);
    distinct.push(4);
    distinct.extend(symbol(second));
    let mut addend = vec![
        8,
        0x81,
        7,
        0x12,
        first as u8,
        (first >> 8) as u8,
        0x13,
        4,
        0x31,
        0,
    ];
    addend.push(4);
    addend.extend(register(name("d0")));
    let mut absolute = vec![8, 0x81, 3, 0x13, 7, 0, 4];
    absolute.extend(register(name("d0")));
    let mut fixture = format!(
        "NAME_COUNT={}\nSYMBOL_COUNT={}\nFIRST_ID={}\nSECOND_ID={}\nMOVE_ID={}\nLONG_QUALIFIER={}\nBSR_ID={}\nWORD_QUALIFIER={}\nQUALIFIED_FIRST_ID={}\nQUALIFIED_SECOND_ID={}\nPROJECTED_ID={}\nMEMBER_ID={}\nFPU_ID={}\nNONE_ID={}\nENCODE_ID={}\nMOVEQ_ID={}\n",
        first, unknown + 1, first, second, name("move"), qualifier, name("bsr"), word_qualifier,
        qualified_first, qualified_second, projected, name("d0"), name("fpu"), name("none"), name("encode"), name("moveq")
    );
    let postincrement = oracle(dir, " .cpu 68020\n move.b (a0)+,(a1)+\n")
        .expect("live Rust constant postincrement instruction");
    assert_eq!(postincrement.len(), 2);
    fixture.push_str(&format!(
        "BYTE_QUALIFIER={byte_qualifier}\nPOSTINCREMENT_WORD={}\n",
        u16::from_be_bytes([postincrement[0], postincrement[1]])
    ));
    let mut postincrement_operand = vec![14];
    postincrement_operand.extend(register(name("a0")));
    postincrement_operand.extend([15, 18, 4, 14]);
    postincrement_operand.extend(register(name("a1")));
    postincrement_operand.extend([15, 18]);
    let mut no_reset_instruction = vec![0, 1, 0, 0];
    no_reset_instruction.extend(register(name("move")));
    no_reset_instruction.extend(&postincrement_operand);
    no_reset_instruction[0] = (no_reset_instruction.len() - 1) as u8;
    let difference = |left: u16, right: u16| {
        vec![
            0x81,
            8,
            0x12,
            left as u8,
            (left >> 8) as u8,
            0x12,
            right as u8,
            (right >> 8) as u8,
            0x32,
            0,
        ]
    };
    for (label, bytes) in [
        ("NoResetInstruction", no_reset_instruction),
        ("PostincrementOperand", postincrement_operand),
        ("BlockDifference", difference(second, first)),
        (
            "TypedDifference",
            difference(qualified_second, qualified_first),
        ),
        ("UnknownExpression", symbol(unknown)),
        ("CurrentExpression", vec![0x81, 2, 0x11, 0]),
    ] {
        fixture.push_str(&format!(
            "{label} .byte {}\n{label}End\n",
            bytes
                .iter()
                .map(u8::to_string)
                .collect::<Vec<_>>()
                .join(",")
        ));
    }
    // A real path's scale terminal must retain scalar observations even when
    // its allowed-value predicate rejects the evaluated result.
    let product = |scalar: Vec<u8>| {
        let left = register(name("d0"));
        let mut bytes = vec![
            0x82,
            (2 + left.len() + scalar.len()) as u8,
            20,
            left.len() as u8,
        ];
        bytes.extend(left);
        bytes.extend(scalar);
        bytes
    };
    for (label, bytes) in [
        ("PathSectionProduct", product(symbol(first))),
        ("PathThreeProduct", product(vec![0x81, 3, 0x13, 3, 0])),
        ("PathTwoProduct", product(vec![0x81, 3, 0x13, 2, 0])),
    ] {
        fixture.push_str(&format!(
            "{label} .byte {}\n{label}End\n",
            bytes
                .iter()
                .map(u8::to_string)
                .collect::<Vec<_>>()
                .join(",")
        ));
    }
    let candidate =
        |id, automatic_classes, displacement_width, position_adjustment, reserved_values| {
            BranchCandidateSpec {
                id,
                automatic_classes,
                suffix: Vec::new(),
                displacement_width,
                endian: EncodingEndian::Big,
                position_adjustment,
                unresolved_placeholder: 1,
                reserved_values,
            }
        };
    for (label, unresolved_candidate, candidates) in [
        (
            "IntervalHoleProgram",
            7,
            vec![
                candidate(3, 0, 2, 0, vec![]),
                candidate(7, 1, 1, 0, vec![0]),
            ],
        ),
        (
            "IntervalLaterProgram",
            6,
            vec![candidate(6, 1, 1, 0, vec![]), candidate(7, 1, 2, 0, vec![])],
        ),
        (
            "IntervalOverflowProgram",
            9,
            vec![candidate(9, 1, 4, 1, vec![])],
        ),
    ] {
        let bytes = compile_branch_program(&BranchProgramSpec {
            opcode_input: 0,
            target_input: 0,
            unresolved_candidate,
            candidates,
        })
        .expect("current generic branch interval program");
        fixture.push_str(&format!(
            "{label} .byte {}\n{label}End\n",
            bytes
                .iter()
                .map(u8::to_string)
                .collect::<Vec<_>>()
                .join(",")
        ));
    }
    for (index, operands) in [repeated, distinct, addend, absolute].iter().enumerate() {
        fixture.push_str(&format!(
            "Operands{index} .byte {}\nOperands{index}End\n",
            operands
                .iter()
                .map(u8::to_string)
                .collect::<Vec<_>>()
                .join(",")
        ));
    }
    fixture.push_str(&format!(
        "BranchOperand .byte {}\nBranchOperandEnd\n",
        symbol(second)
            .iter()
            .map(u8::to_string)
            .collect::<Vec<_>>()
            .join(",")
    ));
    let mut range_operand = vec![8];
    range_operand.extend(symbol(first));
    range_operand.push(4);
    range_operand.extend(register(name("d0")));
    fixture.push_str(&format!(
        "RangeOperand .byte {}\nRangeOperandEnd\n",
        range_operand
            .iter()
            .map(u8::to_string)
            .collect::<Vec<_>>()
            .join(",")
    ));
    for (label, mut expression) in [
        ("QualifiedFirst", symbol(qualified_first)),
        ("QualifiedSecond", symbol(qualified_second)),
        ("QualifiedAddend", symbol(qualified_first)),
    ] {
        if label == "QualifiedAddend" {
            expression.pop();
            expression[1] += 3;
            expression.extend([0x13, 3, 0x31, 0]);
        }
        fixture.push_str(&format!(
            "{label} .byte {}\n{label}End\n",
            expression
                .iter()
                .map(u8::to_string)
                .collect::<Vec<_>>()
                .join(",")
        ));
    }
    fs::write(dir.join("instruction_fixup_identity_fixture.i"), fixture).unwrap();
    fs::write(
        dir.join("instruction_fixup_identity_package.bin"),
        package.bytes,
    )
    .unwrap();

    let bytes = oracle(dir, SOURCE).expect("live Rust MOVE instruction oracle");
    assert_eq!(bytes.len(), 72);
    let mut expected = Vec::new();
    let mut cursor = 32;
    for (length, fields) in [
        (10, vec![(2, first, 0x1000), (6, first, 0x1000)]),
        (10, vec![(2, first, 0x1000), (6, second, 0x1010)]),
        (6, vec![(2, first, 0x1004)]),
        (6, vec![]),
    ] {
        expected.extend_from_slice(&(length as u32).to_be_bytes());
        expected.extend_from_slice(&(fields.len() as u32).to_be_bytes());
        expected.extend_from_slice(&bytes[cursor..cursor + length]);
        cursor += length;
        for (offset, identity, value) in fields {
            for word in [offset, 4, 1, value, identity as u32] {
                expected.extend_from_slice(&word.to_be_bytes());
            }
        }
    }
    for (length, request) in [(4, 1_i32), (4, -1_i32)] {
        expected.extend_from_slice(&(length as u32).to_be_bytes());
        expected.extend_from_slice(&bytes[cursor..cursor + length]);
        cursor += length;
        expected.extend_from_slice(&request.to_be_bytes());
    }
    expected
}

fn build_probe(root: &Path, dir: &Path) -> Vec<u8> {
    let input = dir.join("fixup_identity_probe.asm");
    fs::copy(
        root.join("native/motorola68000/amigaos/tests/assembler/fixup_identity_probe.asm"),
        &input,
    )
    .unwrap();
    let native = root.join("native/motorola68000/amigaos");
    let cli = Cli::parse_from([
        "opforge".to_owned(),
        input.to_string_lossy().into_owned(),
        "-M".to_owned(),
        native.to_string_lossy().into_owned(),
        "-I".to_owned(),
        native.to_string_lossy().into_owned(),
        "-I".into(),
        native.join("debug").to_string_lossy().into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_path_buf());
    if let Err(error) = run_with_validated_cli_with_context(&cli, &config) {
        match error {
            cli_core::CliRunError::Assembler { error, .. } => {
                panic!(
                    "native identity probe: {}\n{:?}",
                    error.summary(),
                    error.diagnostics()
                );
            }
            error => panic!("native identity probe: {error:?}"),
        }
    }
    fs::read(dir.join("instruction-fixup-identity.hunk")).unwrap()
}

#[test]
fn native_instruction_fixup_identity_host_assembles() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    assert!(!prepare(&dir.0).is_empty());
    assert!(!build_probe(&root, &dir.0).is_empty());
}

#[test]
fn native_module_carrier_host_assembles() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let input = dir.0.join("carrier_host.asm");
    fs::write(
        &input,
        r#" .module tests.amigaos.carrier_host
 .cpu 68020
 .use opforge.assembler.modules.carrier as carrier
 .use opforge.assembler.modules.image as image
 .use opforge.assembler.modules.records as records
 .use opforge.platform.memory as memory
 .use opforge.assembler.source.records as source
 .use opforge.assembler.layout.sections as sections
 .use opforge.assembler.layout.block_index as blockIndex
 .use opforge.assembler.symbols.scope_layout as carrierScope
 .use opforge.assembler.symbols.scopes as carrierScopes
 .use opforge.assembler.symbols.lexical_store as carrierLexical
 .use opforge.assembler.modules.bindings as carrierModules
 .use opforge.assembler.symbols.imports as carrierImports
 .use opforge.assembler.modules.graph as carrierGraph
 .use opforge.assembler.values.values as carrierValues
 .section entry, kind=code
 .pub
start .block
 jsr carrierChecks
 rts
 .bend
 .endsection
 .section code, kind=code
 .priv
 .include "module_carrier_checks.i"
 .endsection
 .output "carrier-host.hunk",format=hunk,sections=entry,code,data,bss
 .endmodule
"#,
    )
    .unwrap();
    let native = root.join("native/motorola68000/amigaos");
    let cli = Cli::parse_from([
        "opforge".to_owned(),
        input.to_string_lossy().into_owned(),
        "-M".to_owned(),
        native.to_string_lossy().into_owned(),
        "-I".to_owned(),
        native.to_string_lossy().into_owned(),
        "-I".into(),
        native.join("debug").to_string_lossy().into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.0.clone());
    if let Err(error) = run_with_validated_cli_with_context(&cli, &config) {
        match error {
            cli_core::CliRunError::Assembler { error, .. } => {
                panic!("carrier host harness: {}", error.summary());
            }
            error => panic!("carrier host harness: {error:?}"),
        }
    }
    assert!(!fs::read(dir.0.join("carrier-host.hunk"))
        .unwrap()
        .is_empty());
}

#[test]
#[ignore = "requires FS-UAE; actual package encoding and original per-field identities"]
fn native_instruction_fixup_identity_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let expected = prepare(&dir.0);
    let image = build_probe(&root, &dir.0);
    let case = OpforgeNativeCliParityCase {
        name: "instruction-fixup-source-identities",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(SOURCE.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/output.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh package encoding identity execution");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed && runs[0].success);
    assert_eq!(runs[0].exit_code, Some(0));
    assert_eq!(
        runs[0].verified_output.as_deref(),
        Some(expected.as_slice())
    );
}
