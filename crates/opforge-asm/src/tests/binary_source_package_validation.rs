//! Component proof for the actual native package validator, independent of CLI loading.
use super::*;
use std::path::{Path, PathBuf};

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
    assert_eq!(&valid[..4], b"BS31");
    assert!(rows + count * crate::binary_source_experiment::ROW <= valid.len());
    assert_eq!(&valid[rows + 34..rows + 36], &[0, 0]);
    assert!(valid[rows + 32] <= 11);
    let mut reserved = valid.clone();
    reserved[rows + 35] = 1;
    let mut form = valid.clone();
    form[rows + 32] = 12;
    let mut cases = vec![
        ("valid-package", valid.clone(), 0),
        ("nonzero-row-reserved", reserved, 1),
        ("invalid-third-form", form, 1),
    ];
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
    let mut superseded = valid.clone();
    superseded[..4].copy_from_slice(b"BS30");
    cases.push(("superseded-package", superseded, 1));
    cases.push(("truncated-current-header", valid[..211].to_vec(), 1));
    cases
}

fn source(root: &Path) -> String {
    let mut source = String::from(
        r#".module validation_probe
.cpu 68020
.use experimental.amigaos.binary_package_validation as validation
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
.section entry,kind=code
.pub
; Shell entry; preserve D2-D7/A2-A6. Status artifact is the validator's D0,
; while a nonzero Shell exit means the harness could not publish that result.
start .block
 movem.l d2-d7/a2-a6,-(sp)
 lea Payload,a0
 move.l #PayloadEnd-Payload,d0
 suba.l a1,a1
 jsr validation.validate
 move.l d0,Result
 lea DosName,a1
 moveq #36,d0
 movea.l 4.w,a6
 jsr EXEC_OPEN_LIBRARY(a6)
 tst.l d0
 beq.w unavailable
 movea.l d0,a5
 movea.l a5,a6
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
OutputName .byte "Work:validation.bin",0
.align 2
Result .long 0
Payload .incbin "package.bin"
PayloadEnd
.endsection
.output "validation-probe.hunk",format=hunk,sections=entry,code,data,bss
.endmodule
"#,
    );
    let directory = root.join("native/motorola68000/amigaos/experimental");
    for name in [
        "binary_package.asm",
        "binary_state.asm",
        "binary_memory.asm",
        "binary_package_validation.asm",
    ] {
        let module = std::fs::read_to_string(directory.join(name)).unwrap();
        if name == "binary_memory.asm" {
            let telemetry = std::fs::read_to_string(
                root.join("native/motorola68000/amigaos/debug/memory_telemetry.i"),
            )
            .unwrap();
            source.push_str(&module.replace(".include \"memory_telemetry.i\"", &telemetry));
        } else {
            source.push_str(&module);
        }
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
    for (name, bytes, expected) in cases.iter().skip(1) {
        let changed = valid
            .iter()
            .zip(bytes)
            .enumerate()
            .filter_map(|(offset, (left, right))| (left != right).then_some(offset))
            .collect::<Vec<_>>();
        assert_eq!(*expected, 1, "{name}");
        let field = match *name {
            "nonzero-row-reserved" => rows + 35..rows + 36,
            "invalid-third-form" => rows + 32..rows + 33,
            "expression-absent"
            | "expression-header-overlap"
            | "expression-odd-offset"
            | "expression-past-runtime" => 200..204,
            "expression-empty" | "expression-too-large" | "expression-span-overflow" => 204..208,
            "builtin-name-out-of-bounds" => 208..210,
            "builtin-reserved" => 210..212,
            "superseded-package" => 0..4,
            "truncated-current-header" => {
                assert_eq!(bytes.len(), 211);
                assert_eq!(bytes, &valid[..211]);
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
#[ignore = "requires configured FS-UAE; actual native validator component status, not full CLI parity"]
fn compact_package_validation_component_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

    let root = workspace_root();
    let scratch = Scratch::new();
    let source = source(&root);
    let input = scratch.0.join("entry.asm");
    std::fs::write(&input, &source).unwrap();
    let mut failures = Vec::new();
    for (name, package, expected_status) in fixtures() {
        std::fs::write(scratch.0.join("package.bin"), &package).unwrap();
        let cli = Cli::parse_from(["opForge", input.to_str().unwrap(), "--cpu", "68020"]);
        let mut config = validate_cli(&cli).unwrap();
        config.out_dir = Some(scratch.0.clone());
        run_with_validated_cli_with_context(&cli, &config)
            .expect("assemble actual native package validation component");
        let image = std::fs::read(scratch.0.join("validation-probe.hunk")).unwrap();
        let expected = expected_status.to_be_bytes();
        let case = OpforgeNativeCliParityCase {
            name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(&package),
            command_template: Some(""),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/validation.bin",
                rust_oracle: &expected,
            },
        };
        match run_prebuilt_compact_cli_case_from_env(&root, &case, &image) {
            Ok(FsUaeSmokeOutcome::Completed { runs }) => {
                assert_eq!(runs.len(), 1);
                assert!(runs[0].protocol_completed);
                assert_eq!(runs[0].exit_code, Some(0));
                assert!(runs[0].success);
            }
            Ok(_) => failures.push(format!("{name}: native execution required")),
            Err(error) => failures.push(format!("{name}: {error}")),
        }
    }
    assert!(
        failures.is_empty(),
        "validator component failures: {failures:?}"
    );
}
