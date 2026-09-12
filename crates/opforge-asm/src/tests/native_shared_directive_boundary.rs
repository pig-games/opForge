//! Shared directive ownership: live Rust oracles and bounded real-native cases.
use super::*;
use crate::fs_uae_smoke::{
    FsUaeSmokeOutcome, OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase,
    OpforgeNativeCliProof,
};

fn data_source(cpu: &str) -> String {
    let mut source = format!(".cpu {cpu}\n.org 0\nBASE .const $20\n");
    for index in 0..8 {
        source.push_str(&format!(
            "data{index} .byte (BASE+{index}),0,#7\n.word (2+3)*4\n.long $1234+1\n"
        ));
    }
    source.push_str(if cpu == "m6502" {
        " lda ($20),y\n nop\n"
    } else {
        " move.l (a0)+,d0\n nop\n"
    });
    source
}

fn rust_bytes(source: &str) -> Vec<u8> {
    let lines = source.lines().collect::<Vec<_>>();
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("shared directive Rust assembly");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_shared_directive_rust_oracles() {
    // Level A: complete source outputs, including a retained instruction surface.
    for cpu in ["m6502", "m68020"] {
        let bytes = rust_bytes(&data_source(cpu));
        let mut expected = Vec::new();
        for index in 0..8 {
            expected.extend_from_slice(&[0x20 + index, 0, 7]);
            expected.extend_from_slice(if cpu == "m6502" {
                &[20, 0, 0x35, 0x12, 0, 0]
            } else {
                &[0, 20, 0, 0, 0x12, 0x35]
            });
        }
        expected.extend_from_slice(if cpu == "m6502" {
            &[0xb1, 0x20, 0xea]
        } else {
            &[0x20, 0x18, 0x4e, 0x71]
        });
        assert_eq!(bytes, expected, "{cpu}");
        let source = format!(".cpu {cpu}\nlabel .nop\n");
        let (_, diagnostics) =
            assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
                .expect("unknown dot Rust diagnostic");
        assert!(diagnostics.iter().any(|d| d.contains("Unknown directive")));
    }
}

#[test]
fn native_shared_directive_boundary_contract() {
    // Level B source contract only: retained parser kind controls both passes;
    // directive extension omits shape inference but retains scoped snapshots.
    let root = workspace_root();
    let driver = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .unwrap();
    for name in ["emitPackageStateReady", "advancePackageStateReady"] {
        let route = &driver[driver.find(&format!("{name}\n")).unwrap()..];
        assert!(source_contains_in_order(
            route,
            &[
                "bsr.w classifyStatementDirectiveV1",
                "cmpi.w #-1, d3",
                "beq.w unknownDirective",
            ]
        ));
    }
    let helper = driver
        .split("statementIsDirectiveV1\t.block")
        .nth(1)
        .unwrap()
        .split("\t.bend")
        .next()
        .unwrap();
    assert!(helper.contains("eng.opasmEngineGetStatementKindV1"));
    assert!(!helper.contains("GetStatementSourceText"));
    let session = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/assembly_session.asm"),
    )
    .unwrap();
    let fallback = session
        .split("opforgeNativeCliRecordSourceStatementMnemonic\t.block")
        .nth(1)
        .unwrap()
        .split("\t.bend")
        .next()
        .unwrap();
    assert!(source_contains_in_order(
        fallback,
        &[
            "cmpi.b #'.', (a2)",
            "bne.s copyMnemonic",
            "move.w #constants.NCLI_PARSER_DIRECTIVE_GENERIC, state.NativeCliStmtDirectiveKind",
            "addq.l #1, state.NativeCliStmtMnemStart",
        ]
    ));
    let evaluation =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_operand_eval.asm"))
            .unwrap();
    assert!(source_contains_in_order(
        &evaluation,
        &[
            "prepareExtensionCommon\t.block",
            "eng.opasmEngineGetStatementKindV1",
            "beq.s instructionExtension",
            "eng.prepareDirectiveEvaluateExpressionExtensionV1",
            "bra.s extensionReady",
            "instructionExtension",
            "eng.prepareEvaluateExpressionExtensionV1",
            "extensionReady",
            "bsr.w materializeScopedSnapshot",
        ]
    ));
}

fn run_native_case(cpu: &str, negative: bool) {
    // Level D only when Completed: fresh guest protocol and live Rust bytes.
    let root = workspace_root();
    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .unwrap();
    let source = if negative {
        format!(".cpu {cpu}\nlabel .nop\n")
    } else {
        data_source(cpu)
    };
    let oracle = if negative {
        Vec::new()
    } else {
        rust_bytes(&source)
    };
    let case = OpforgeNativeCliParityCase {
        name: if negative {
            "unknown-dot"
        } else {
            "shared-data"
        },
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some("{input} --bin {bin} --cpu m6502 --opasm-package {package}"),
        package_mode: OpforgeNativeCliPackageMode::Explicit(&package),
        extra_guest_files: &[],
        proof: if negative {
            OpforgeNativeCliProof::ExpectedFailureContaining("Unknown directive")
        } else {
            OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/opforge_native_out.bin",
                rust_oracle: &oracle,
            }
        },
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &[case])
        .expect("shared directive native proof")
    {
        FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP (no native proof): {reason}"),
        FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(run.protocol_completed);
            if negative {
                assert!(run.exit_code.is_some_and(|code| code != 0));
            } else {
                assert!(run.success);
                assert_eq!(run.exit_code, Some(0));
            }
        }
    }
}

#[test]
fn native_shared_directive_m6502_data_fs_uae() {
    run_native_case("m6502", false);
}
#[test]
fn native_shared_directive_m6502_unknown_fs_uae() {
    run_native_case("m6502", true);
}
#[test]
fn native_shared_directive_m68020_data_fs_uae() {
    run_native_case("m68020", false);
}
#[test]
fn native_shared_directive_m68020_unknown_fs_uae() {
    run_native_case("m68020", true);
}
