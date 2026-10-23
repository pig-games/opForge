//! Component proof for the actual native package validator, independent of CLI loading.
use super::*;
use std::path::{Path, PathBuf};

const BRANCH_PLAN_OFFSET: usize = 220;
const BRANCH_PLAN_BYTES_OFFSET: usize = 224;

struct Scratch(PathBuf);
impl Scratch {
    fn new() -> Self {
        let path = std::env::temp_dir().join(format!(
            "opforge-package-validation-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir(&path).unwrap();
        Self(path)
    }
}
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn fixtures() -> Vec<(&'static str, Vec<u8>, u32)> {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68030", None).unwrap();
    let valid = prepare_package(&core, &resolved).unwrap();
    let rows = u32::from_be_bytes(valid[16..20].try_into().unwrap()) as usize;
    let count = u32::from_be_bytes(valid[20..24].try_into().unwrap()) as usize;
    assert!(count > 0);
    assert_eq!(&valid[..4], b"BS38");
    assert!(rows + count * crate::binary_source_experiment::ROW <= valid.len());
    assert_eq!(&valid[rows + 34..rows + 36], &[0, 0]);
    assert!(valid[rows + 32] <= 11);
    let mut invalid_ds = valid.clone();
    let name_count = u16::from_be_bytes(valid[62..64].try_into().unwrap());
    invalid_ds[crate::binary_source_experiment::DS_DIRECTIVE
        ..crate::binary_source_experiment::DS_DIRECTIVE + 2]
        .copy_from_slice(&name_count.to_be_bytes());
    let mut reserved = valid.clone();
    reserved[rows + 35] = 1;
    let mut form = valid.clone();
    form[rows + 32] = 12;
    let mut cases = vec![
        ("valid-package", valid.clone(), 0),
        ("invalid-ds-directive-id", invalid_ds, 1),
        ("nonzero-row-reserved", reserved, 1),
        ("invalid-third-form", form, 1),
    ];
    let role_row = (0..count)
        .map(|index| rows + index * crate::binary_source_experiment::ROW)
        .find(|row| u16::from_be_bytes(valid[row + 36..row + 38].try_into().unwrap()) != 0)
        .expect("generated package includes independent operand roles");
    let mut role_reserved = valid.clone();
    role_reserved[role_row + 39] = 1;
    cases.push(("nonzero-role-reserved", role_reserved, 1));
    let mut role_count = valid.clone();
    role_count[role_row + 36..role_row + 38].copy_from_slice(&17_u16.to_be_bytes());
    cases.push(("excessive-role-count", role_count, 1));
    let mut role_absent = valid.clone();
    role_absent[role_row + 36..role_row + 38].fill(0);
    cases.push(("absent-role-nonzero-offset", role_absent, 1));
    for (name, offset) in [
        ("role-header-overlap", 4_u32),
        (
            "role-odd-offset",
            u32::from_be_bytes(valid[role_row + 40..role_row + 44].try_into().unwrap()) | 1,
        ),
        ("role-out-of-bounds", valid.len() as u32),
        ("role-span-overflow", u32::MAX),
    ] {
        let mut malformed = valid.clone();
        malformed[role_row + 40..role_row + 44].copy_from_slice(&offset.to_be_bytes());
        cases.push((name, malformed, 1));
    }
    for (name, offset, value) in [
        ("expression-absent", 200, 0),
        ("expression-header-overlap", 200, 206),
        ("expression-odd-offset", 200, 209),
        ("expression-past-runtime", 200, valid.len() as u32),
        ("expression-empty", 204, 0),
        ("expression-too-large", 204, 65536),
        ("expression-span-overflow", 204, u32::MAX),
    ] {
        let mut malformed = valid.clone();
        malformed[offset..offset + 4].copy_from_slice(&value.to_be_bytes());
        cases.push((name, malformed, 1));
    }
    for (name, offset, value) in [
        ("builtin-name-out-of-bounds", 208, u16::MAX),
        ("builtin-reserved", 210, 1),
    ] {
        let mut malformed = valid.clone();
        malformed[offset..offset + 2].copy_from_slice(&value.to_be_bytes());
        cases.push((name, malformed, 1));
    }
    let loop_offset = u32::from_be_bytes(valid[212..216].try_into().unwrap()) as usize;
    assert_eq!(u32::from_be_bytes(valid[216..220].try_into().unwrap()), 17);
    let mut odd = valid.clone();
    odd[loop_offset + 1..loop_offset + 18].copy_from_slice(&valid[loop_offset..loop_offset + 17]);
    odd[212..216].copy_from_slice(&((loop_offset + 1) as u32).to_be_bytes());
    cases.push(("loop-odd-offset", odd, 1));
    let branch_offset = u32::from_be_bytes(
        valid[BRANCH_PLAN_OFFSET..BRANCH_PLAN_OFFSET + 4]
            .try_into()
            .unwrap(),
    ) as usize;
    let branch_bytes = u32::from_be_bytes(
        valid[BRANCH_PLAN_BYTES_OFFSET..BRANCH_PLAN_BYTES_OFFSET + 4]
            .try_into()
            .unwrap(),
    ) as usize;
    assert_eq!(branch_bytes, package::package::PACKED_BRANCH_PROGRAM_BYTES);
    for (name, offset) in [
        ("branch-header-overlap", 4_u32),
        ("branch-odd-offset", (branch_offset as u32) | 1),
        ("branch-out-of-bounds", valid.len() as u32),
        ("branch-span-overflow", u32::MAX),
    ] {
        let mut malformed = valid.clone();
        malformed[BRANCH_PLAN_OFFSET..BRANCH_PLAN_OFFSET + 4]
            .copy_from_slice(&offset.to_be_bytes());
        cases.push((name, malformed, 1));
    }
    for (name, byte) in [
        ("branch-corrupt-opcode", 0),
        ("branch-corrupt-count", 1),
        ("branch-invalid-role", 4),
        ("branch-invalid-expression-policy", 5),
        ("match-invalid-role", 2 + 4 * 4 + 2),
        ("match-invalid-expression-policy", 2 + 4 * 4 + 3),
        ("case-invalid-expression-policy", 2 + 5 * 4 + 3),
        ("default-invalid-expression-policy", 2 + 6 * 4 + 3),
        ("branch-corrupt-end", branch_bytes - 1),
    ] {
        let mut malformed = valid.clone();
        malformed[branch_offset + byte] = 0xff;
        cases.push((name, malformed, 1));
    }
    let mut branch_name = valid.clone();
    branch_name[branch_offset + 2..branch_offset + 4].copy_from_slice(&u16::MAX.to_be_bytes());
    cases.push(("branch-invalid-name", branch_name, 1));
    let mut branch_truncated = valid.clone();
    branch_truncated[BRANCH_PLAN_BYTES_OFFSET..BRANCH_PLAN_BYTES_OFFSET + 4]
        .copy_from_slice(&((branch_bytes - 1) as u32).to_be_bytes());
    cases.push(("branch-truncated-program", branch_truncated, 1));
    for (name, offset, value) in [
        ("loop-header-overlap", 212, 216u32),
        ("loop-out-of-bounds", 212, valid.len() as u32),
        ("loop-truncated-program", 216, 12u32),
    ] {
        let mut malformed = valid.clone();
        malformed[offset..offset + 4].copy_from_slice(&value.to_be_bytes());
        cases.push((name, malformed, 1));
    }
    for (name, offset, width) in [
        ("loop-corrupt-opcode", loop_offset, 1),
        ("loop-wrong-head", loop_offset + 1, 2),
        ("loop-name-out-of-bounds", loop_offset + 3, 2),
        ("loop-invalid-endfor", loop_offset + 5, 2),
        ("loop-invalid-while", loop_offset + 7, 2),
        ("loop-invalid-endwhile", loop_offset + 9, 2),
        ("loop-invalid-bfor", loop_offset + 11, 2),
        ("loop-invalid-bwhile", loop_offset + 13, 2),
        ("loop-corrupt-return", loop_offset + 15, 1),
        ("loop-corrupt-stop", loop_offset + 16, 1),
    ] {
        let mut malformed = valid.clone();
        malformed[offset..offset + width].fill(255);
        cases.push((name, malformed, 1));
    }
    let mut superseded = valid.clone();
    superseded[..4].copy_from_slice(b"BS37");
    cases.push(("superseded-package", superseded, 1));
    use crate::binary_source_experiment::{STATEMENT_COMPILE, STATEMENT_HEAD, STATEMENT_MATCH};
    for (field, names) in [
        (
            STATEMENT_COMPILE,
            [
                "statement-compile-overlap",
                "statement-compile-odd",
                "statement-compile-empty",
                "statement-compile-overflow",
                "statement-compile-opcode",
            ],
        ),
        (
            STATEMENT_MATCH,
            [
                "statement-match-overlap",
                "statement-match-odd",
                "statement-match-empty",
                "statement-match-overflow",
                "statement-match-opcode",
            ],
        ),
        (
            STATEMENT_HEAD,
            [
                "statement-head-overlap",
                "statement-head-odd",
                "statement-head-empty",
                "statement-head-overflow",
                "statement-head-opcode",
            ],
        ),
    ] {
        let program = u32::from_be_bytes(valid[field..field + 4].try_into().unwrap());
        for (name, offset, value) in [
            (
                names[0],
                field,
                crate::binary_source_experiment::HEADER as u32,
            ),
            (names[1], field, program | 1),
            (names[2], field + 4, 0),
            (names[3], field + 4, u32::MAX),
        ] {
            let mut malformed = valid.clone();
            malformed[offset..offset + 4].copy_from_slice(&value.to_be_bytes());
            cases.push((name, malformed, 1));
        }
        let mut malformed = valid.clone();
        malformed[program as usize] = 0xff;
        cases.push((names[4], malformed, 1));
    }
    let head = u32::from_be_bytes(
        valid[STATEMENT_HEAD..STATEMENT_HEAD + 4]
            .try_into()
            .unwrap(),
    ) as usize;
    for (name, offset) in [
        ("statement-definition-id", head + 2),
        ("statement-end-id", head + 4),
    ] {
        let mut malformed = valid.clone();
        malformed[offset..offset + 2].copy_from_slice(&name_count.to_be_bytes());
        cases.push((name, malformed, 1));
    }
    cases.push((
        "truncated-current-header",
        valid[..crate::binary_source_experiment::HEADER - 1].to_vec(),
        1,
    ));
    cases
}

fn source(root: &Path) -> String {
    let mut source = String::from(
        r#".module validation_probe
.cpu 68020
.use opforge.packages.validation as validation
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_READ = -42
DOS_WRITE = -48
MODE_NEWFILE = 1006
MODE_OLDFILE = 1005
PACKAGE_CAPACITY = 1048576
.section entry,kind=code
.pub
; Shell entry; preserve D2-D7/A2-A6. Status artifact is the validator's D0,
; while a nonzero Shell exit means the harness could not publish that result.
start .block
 movem.l d2-d7/a2-a6,-(sp)
 lea DosName,a1
 moveq #36,d0
 movea.l 4.w,a6
 jsr EXEC_OPEN_LIBRARY(a6)
 tst.l d0
 beq.w unavailable
 movea.l d0,a5
 movea.l a5,a6
 ; One frozen validator receives the actual isolated case's input bytes.
 ; Read failure cannot publish a validator result or supply native proof.
 move.l #InputName,d1
 move.l #MODE_OLDFILE,d2
 jsr DOS_OPEN(a6)
 tst.l d0
 beq.w failed
 move.l d0,d4
 move.l d4,d1
 move.l #Payload,d2
 move.l #PACKAGE_CAPACITY,d3
 jsr DOS_READ(a6)
 move.l d0,d5
 move.l d4,d1
 jsr DOS_CLOSE(a6)
 tst.l d0
 beq.w failed
 tst.l d5
 bmi.w failed
 cmpi.l #PACKAGE_CAPACITY,d5
 bhs.w failed
 lea Payload,a0
 move.l d5,d0
 suba.l a1,a1
 jsr validation.validate
 move.l d0,Result
 move.l #OutputName,d1
 move.l #MODE_NEWFILE,d2
 jsr DOS_OPEN(a6)
 tst.l d0
 beq.w failed
 move.l d0,d4
 move.l d4,d1
 move.l #Result,d2
 moveq #4,d3
 jsr DOS_WRITE(a6)
 move.l d0,d5
 move.l d4,d1
 jsr DOS_CLOSE(a6)
 tst.l d0
 beq.w failed
 cmpi.l #4,d5
 bne.w failed
 moveq #0,d7
 bra.w closeLibrary
failed
 moveq #20,d7
closeLibrary
 movea.l a5,a1
 movea.l 4.w,a6
 jsr EXEC_CLOSE_LIBRARY(a6)
 move.l d7,d0
 bra.w done
unavailable
 moveq #20,d0
done
 movem.l (sp)+,d2-d7/a2-a6
 tst.l d0
 rts
.bend ; start
.endsection
.section data,kind=data
DosName .byte "dos.library",0
InputName .byte "Work:input.bin",0
OutputName .byte "Work:validation.bin",0
.align 2
Result .long 0
.endsection
.section bss,kind=bss
Payload .res byte,PACKAGE_CAPACITY
.endsection
.output "validation-probe.hunk",format=hunk,sections=entry,code,data,bss
.endmodule
"#,
    );
    let directory = root.join("native/motorola68000/amigaos");
    for name in [
        "vm/prvm/abi.asm",
        "vm/prvm/packed_branch.asm",
        "vm/prvm/packed_declaration.asm",
        "vm/prvm/packed_text.asm",
        "vm/prvm/statement_abi.asm",
        "vm/prvm/statement_inputs.asm",
        "vm/prvm/statement_text.asm",
        "vm/prvm/statement_facets.asm",
        "vm/prvm/statement_policy.asm",
        "vm/prvm/statement_request.asm",
        "vm/prvm/statement_plan.asm",
        "vm/prvm/statement_compile.asm",
        "vm/prvm/statement_match.asm",
        "vm/prvm/statement_head.asm",
    ] {
        source.push_str(
            &std::fs::read_to_string(root.join("native/motorola68000/amigaos").join(name)).unwrap(),
        );
        source.push('\n');
    }
    for name in [
        "packages/format.asm",
        "assembler/layout/state.asm",
        "platform/memory.asm",
        "packages/validation.asm",
    ] {
        let module = std::fs::read_to_string(directory.join(name)).unwrap();
        source.push_str(&module);
        source.push('\n');
    }
    source.push_str(".end\n");
    source
}

#[test]
fn compact_package_validation_component_fixture_contract() {
    let cases = fixtures();
    let valid = &cases[0].1;
    let rows = u32::from_be_bytes(valid[16..20].try_into().unwrap()) as usize;
    let count = u32::from_be_bytes(valid[20..24].try_into().unwrap()) as usize;
    let role_row = (0..count)
        .map(|index| rows + index * crate::binary_source_experiment::ROW)
        .find(|row| u16::from_be_bytes(valid[row + 36..row + 38].try_into().unwrap()) != 0)
        .unwrap();
    let loop_offset = u32::from_be_bytes(valid[212..216].try_into().unwrap()) as usize;
    let branch_offset = u32::from_be_bytes(
        valid[BRANCH_PLAN_OFFSET..BRANCH_PLAN_OFFSET + 4]
            .try_into()
            .unwrap(),
    ) as usize;
    for (name, bytes, expected) in cases.iter().skip(1) {
        let changed = valid
            .iter()
            .zip(bytes)
            .enumerate()
            .filter_map(|(offset, (left, right))| (left != right).then_some(offset))
            .collect::<Vec<_>>();
        assert_eq!(*expected, 1, "{name}");
        if *name == "loop-odd-offset" {
            assert_eq!(
                &bytes[loop_offset + 1..loop_offset + 18],
                &valid[loop_offset..loop_offset + 17]
            );
            assert!(changed.iter().all(|offset| (212..216).contains(offset)
                || (loop_offset..loop_offset + 18).contains(offset)));
            continue;
        }
        let field = match *name {
            "nonzero-row-reserved" => rows + 35..rows + 36,
            "invalid-third-form" => rows + 32..rows + 33,
            "nonzero-role-reserved" => role_row + 38..role_row + 40,
            "excessive-role-count" | "absent-role-nonzero-offset" => role_row + 36..role_row + 38,
            "role-header-overlap"
            | "role-odd-offset"
            | "role-out-of-bounds"
            | "role-span-overflow" => role_row + 40..role_row + 44,
            "expression-absent"
            | "expression-header-overlap"
            | "expression-odd-offset"
            | "expression-past-runtime" => 200..204,
            "expression-empty" | "expression-too-large" | "expression-span-overflow" => 204..208,
            "builtin-name-out-of-bounds" => 208..210,
            "builtin-reserved" => 210..212,
            "loop-header-overlap" | "loop-out-of-bounds" => 212..216,
            "loop-truncated-program" => 216..220,
            "loop-corrupt-opcode" => loop_offset..loop_offset + 1,
            "loop-wrong-head" => loop_offset + 1..loop_offset + 3,
            "loop-name-out-of-bounds" => loop_offset + 3..loop_offset + 5,
            "loop-invalid-endfor" => loop_offset + 5..loop_offset + 7,
            "loop-invalid-while" => loop_offset + 7..loop_offset + 9,
            "loop-invalid-endwhile" => loop_offset + 9..loop_offset + 11,
            "loop-invalid-bfor" => loop_offset + 11..loop_offset + 13,
            "loop-invalid-bwhile" => loop_offset + 13..loop_offset + 15,
            "loop-corrupt-return" => loop_offset + 15..loop_offset + 16,
            "loop-corrupt-stop" => loop_offset + 16..loop_offset + 17,
            "branch-header-overlap"
            | "branch-odd-offset"
            | "branch-out-of-bounds"
            | "branch-span-overflow" => BRANCH_PLAN_OFFSET..BRANCH_PLAN_OFFSET + 4,
            "branch-truncated-program" => BRANCH_PLAN_BYTES_OFFSET..BRANCH_PLAN_BYTES_OFFSET + 4,
            "branch-corrupt-opcode" => branch_offset..branch_offset + 1,
            "branch-corrupt-count" => branch_offset + 1..branch_offset + 2,
            "branch-invalid-name" => branch_offset + 2..branch_offset + 4,
            "branch-invalid-role" => branch_offset + 4..branch_offset + 5,
            "branch-invalid-expression-policy" => branch_offset + 5..branch_offset + 6,
            "match-invalid-role" => branch_offset + 20..branch_offset + 21,
            "match-invalid-expression-policy" => branch_offset + 21..branch_offset + 22,
            "case-invalid-expression-policy" => branch_offset + 25..branch_offset + 26,
            "default-invalid-expression-policy" => branch_offset + 29..branch_offset + 30,
            "branch-corrupt-end" => {
                branch_offset + package::package::PACKED_BRANCH_PROGRAM_BYTES - 1
                    ..branch_offset + package::package::PACKED_BRANCH_PROGRAM_BYTES
            }
            "invalid-ds-directive-id" => {
                crate::binary_source_experiment::DS_DIRECTIVE
                    ..crate::binary_source_experiment::DS_DIRECTIVE + 2
            }
            "superseded-package" => 0..4,
            name if name.starts_with("statement-") => {
                use crate::binary_source_experiment::{
                    STATEMENT_COMPILE, STATEMENT_HEAD, STATEMENT_MATCH,
                };
                let field = if name.starts_with("statement-compile-") {
                    STATEMENT_COMPILE
                } else if name.starts_with("statement-match-") {
                    STATEMENT_MATCH
                } else {
                    STATEMENT_HEAD
                };
                let program =
                    u32::from_be_bytes(valid[field..field + 4].try_into().unwrap()) as usize;
                if name.ends_with("opcode") {
                    program..program + 1
                } else if name == "statement-definition-id" {
                    program + 2..program + 4
                } else if name == "statement-end-id" {
                    program + 4..program + 6
                } else if name.ends_with("empty") || name.ends_with("overflow") {
                    field + 4..field + 8
                } else {
                    field..field + 4
                }
            }
            "truncated-current-header" => {
                assert_eq!(bytes.len(), crate::binary_source_experiment::HEADER - 1);
                assert_eq!(bytes, &valid[..crate::binary_source_experiment::HEADER - 1]);
                assert!(changed.is_empty());
                continue;
            }
            _ => panic!("unclassified invalid fixture {name}"),
        };
        assert_eq!(bytes.len(), valid.len(), "{name}");
        assert!(!changed.is_empty(), "{name}");
        assert!(
            changed.iter().all(|offset| field.contains(offset)),
            "{name}"
        );
        assert_eq!(&bytes[..field.start], &valid[..field.start], "{name}");
        assert_eq!(&bytes[field.end..], &valid[field.end..], "{name}");
    }
}

#[test]
fn compact_package_validation_component_host_assembles() {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let scratch = Scratch::new();
    let input = scratch.0.join("entry.asm");
    std::fs::write(&input, source(&workspace_root())).unwrap();
    // The concatenated real owners retain their debug includes and each include's
    // own relative dependencies; do not replace them with a partial copied closure.
    let debug = workspace_root().join("native/motorola68000/amigaos/debug");
    let cli = Cli::parse_from([
        "opForge",
        input.to_str().unwrap(),
        "--cpu",
        "68020",
        "-I",
        debug.to_str().unwrap(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(scratch.0.clone());
    run_with_validated_cli_with_context(&cli, &config)
        .expect("assemble current validator and its shared packed dependencies");
    assert!(
        std::fs::metadata(scratch.0.join("validation-probe.hunk"))
            .unwrap()
            .len()
            > 0
    );
}

#[test]
#[ignore = "requires configured FS-UAE; actual native validator component status, not full CLI parity"]
fn compact_package_validation_component_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_breadth_from_env, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

    let root = workspace_root();
    let scratch = Scratch::new();
    let source = source(&root);
    let input = scratch.0.join("entry.asm");
    std::fs::write(&input, &source).unwrap();
    let mut cases = fixtures();
    let selected = std::env::var("OPFORGE_PACKAGE_VALIDATION_CASES").ok();
    if let Some(names) = &selected {
        assert!(names
            .split(',')
            .all(|name| cases.iter().any(|case| case.0 == name)));
    }
    cases.retain(|case| {
        selected
            .as_ref()
            .is_none_or(|names| names.split(',').any(|name| name == case.0))
    });
    assert!(!cases.is_empty());
    assert!(cases.iter().all(|case| case.1.len() < 1048576));
    // The concatenated real owners retain their debug includes and each include's
    // own relative dependencies; do not replace them with a partial copied closure.
    let debug = workspace_root().join("native/motorola68000/amigaos/debug");
    let cli = Cli::parse_from([
        "opForge",
        input.to_str().unwrap(),
        "--cpu",
        "68020",
        "-I",
        debug.to_str().unwrap(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(scratch.0.clone());
    run_with_validated_cli_with_context(&cli, &config)
        .expect("assemble one native package validator for all isolated cases");
    let image = std::fs::read(scratch.0.join("validation-probe.hunk")).unwrap();
    let expected: Vec<_> = cases.iter().map(|case| case.2.to_be_bytes()).collect();
    let requests: Vec<_> = cases
        .iter()
        .zip(&expected)
        .map(
            |((name, package, _), expected)| OpforgeNativeCliParityCase {
                name,
                cpu_override: "68020",
                extra_assembly_defines: &[],
                source_override: Some(package),
                command_template: Some(""),
                package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
                extra_guest_files: &[],
                proof: OpforgeNativeCliProof::ExactArtifact {
                    relative_path: "Work/validation.bin",
                    rust_oracle: expected,
                },
            },
        )
        .collect();
    let results = run_prebuilt_compact_cli_breadth_from_env(&root, &requests, &image, 64)
        .expect("fresh native package validation batch");
    assert_eq!(results.len(), cases.len());
    let failures: Vec<_> = cases
        .iter()
        .zip(&results)
        .filter_map(|(case, result)| match &result.run {
            Some(run)
                if result.error.is_none()
                    && run.protocol_completed
                    && run.exit_code == Some(0)
                    && run.success =>
            {
                None
            }
            _ => Some(format!("{}: {:?}", case.0, result.error)),
        })
        .collect();
    assert!(
        failures.is_empty(),
        "validator component failures: {failures:?}"
    );
    eprintln!("PACKAGE_VALIDATOR cases={} fresh_exact=true", cases.len());
}
