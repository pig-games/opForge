//! Actual native call preparation, isolated from source binding and row selection.
use super::*;
use std::path::{Path, PathBuf};
use vm::binary_source_package::BinarySourcePackage;

struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn fixture() -> (Vec<u8>, Vec<u8>) {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68030", None).unwrap();
    let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let exported_names = u16::from_be_bytes(package[62..64].try_into().unwrap());
    let id = |name: &str| {
        u16::try_from(
            numeric
                .names
                .iter()
                .position(|entry| entry.eq_ignore_ascii_case(name))
                .unwrap_or_else(|| panic!("missing package name {name}")),
        )
        .unwrap()
    };
    let mut record = vec![0, 1, 0, 3];
    let name = |kind: u8, register: &str, qualifier: u8| {
        let id = id(register);
        [kind, (id >> 8) as u8, id as u8, qualifier]
    };
    let width = u8::try_from(
        numeric
            .qualifiers
            .iter()
            .position(|entry| entry.eq_ignore_ascii_case("w"))
            .unwrap()
            + 1,
    )
    .unwrap();
    record.extend(name(0, "cas2", width));
    for (index, (left, right)) in [("d0", "d1"), ("d2", "d3"), ("a0", "a1")]
        .into_iter()
        .enumerate()
    {
        if index != 0 {
            record.push(4);
        }
        // Match frontend allocation after a module name: an unqualified
        // lexical call name follows the package's initial name arena.
        record.push(7);
        let callee = exported_names.checked_add(2).unwrap();
        record.extend([0, (callee >> 8) as u8, callee as u8, 0]);
        record.push(14);
        for (argument, register) in [left, right].into_iter().enumerate() {
            if argument != 0 {
                record.push(4);
            }
            if index == 2 {
                record.push(14);
            }
            record.extend(name(0, register, 0));
            if index == 2 {
                record.push(15);
            }
        }
        record.push(15);
    }
    record[0] = u8::try_from(record.len() - 1).unwrap();
    (package, record)
}

fn source(output_bytes: usize) -> String {
    format!(
        r#".module prepare_probe
.cpu 68020
.use experimental.amigaos.binary_prepare as prepare
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
.section entry,kind=code
.pub
start .block
 movem.l d2-d7/a2-a6,-(sp)
 lea Record,a0
 lea Output+4,a1
 lea Payload,a2
 jsr prepare.line
 move.l d0,Output
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
 move.l #Output,d2
 move.l #{output_bytes},d3
 jsr DOS_WRITE(a6)
 move.l d0,d5
 move.l d4,d1
 jsr DOS_CLOSE(a6)
 tst.l d0
 beq.w failed
 cmpi.l #{output_bytes},d5
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
OutputName .byte "Work:prepared.bin",0
.align 2
Payload .incbin "package.bin"
Record .incbin "record.bin"
.endsection
.section bss,kind=bss
Output .res byte,260
.endsection
.output "prepare-probe.hunk",format=hunk,sections=entry,code,data,bss
.endmodule
.end
"#
    )
}

fn assemble(root: &Path, scratch: &Path, package: &[u8], record: &[u8]) -> Vec<u8> {
    assemble_source(root, scratch, package, record, &source(4 + record.len()))
}

fn assemble_source(
    root: &Path,
    scratch: &Path,
    package: &[u8],
    record: &[u8],
    source: &str,
) -> Vec<u8> {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    std::fs::write(scratch.join("package.bin"), package).unwrap();
    std::fs::write(scratch.join("record.bin"), record).unwrap();
    let input = scratch.join("entry.asm");
    std::fs::write(&input, source).unwrap();
    let argv = vec![
        "opForge".to_owned(),
        input.to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        root.join("native/motorola68000")
            .to_string_lossy()
            .into_owned(),
        "-I".into(),
        root.join("native/motorola68000/amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ];
    let cli = Cli::parse_from(argv);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(scratch.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => {
            panic!("assemble native call component: {:?}", error.diagnostics())
        }
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("assemble native call component: {error}")
        }
        _ => panic!("assemble native call component: warnings treated as errors"),
    });
    std::fs::read(scratch.join("prepare-probe.hunk")).unwrap()
}

fn frontend_source(package: &[u8], output_bytes: usize) -> String {
    let dictionary = u32::from_be_bytes(package[12..16].try_into().unwrap());
    let setup = r#" lea Frame,a0
 move.l #Payload,frontend.Frame.Package(a0)
 move.l #FrontendScratch,frontend.Frame.Scratch(a0)
 lea Output+4,a1
 move.l a1,frontend.Frame.Output(a0)
 move.l #256,frontend.Frame.Capacity(a0)
 jsr frontend.begin
 tst.l d0
 bne.w frontendDone
 move.l #ModuleLine,frontend.Frame.Source(a0)
 move.l #ModuleLineEnd-ModuleLine,frontend.Frame.SourceBytes(a0)
 jsr frontend.line
 tst.l d0
 bne.w frontendDone
 move.l #CpuLine,frontend.Frame.Source(a0)
 move.l #CpuLineEnd-CpuLine,frontend.Frame.SourceBytes(a0)
 jsr frontend.line
 tst.l d0
 bne.w frontendDone
 move.l #CallLine,frontend.Frame.Source(a0)
 move.l #CallLineEnd-CallLine,frontend.Frame.SourceBytes(a0)
 jsr frontend.line
frontendDone
 move.l d0,Output
 jsr frontend.finish
"#;
    source(output_bytes)
        .replace(
            ".use experimental.amigaos.binary_prepare as prepare",
            ".use experimental.amigaos.binary_frontend as frontend",
        )
        .replace(
            " lea Record,a0\n lea Output+4,a1\n lea Payload,a2\n jsr prepare.line\n move.l d0,Output\n",
            setup,
        )
        .replace("Work:prepared.bin", "Work:frontend-status.bin")
        .replace(
            "Record .incbin \"record.bin\"",
            "Record .incbin \"record.bin\"\nModuleLine .byte \".module component\"\nModuleLineEnd\nCpuLine .byte \".cpu m68030\"\nCpuLineEnd\nCallLine .byte \" cas2.w .pair(d0,d1),.pair(d2,d3),.pair((a0),(a1))\"\nCallLineEnd",
        )
        .replace(
            "Output .res byte,260",
            &format!("Output .res byte,260\n.align 4\nFrame .res byte,frontend.FRAME_BYTES\n.align 4\nFrontendScratch .res byte,frontend.SCRATCH_BYTES+{dictionary}*8"),
        )
}

#[test]
fn compact_call_frontend_component_host_assembly() {
    let (package, record) = fixture();
    let scratch = scratch();
    let source = frontend_source(&package, 4 + record.len());
    assert!(!assemble_source(&workspace_root(), &scratch.0, &package, &record, &source).is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; dotted first operand survives frontend label normalization"]
fn compact_call_frontend_component_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let root = workspace_root();
    let scratch = scratch();
    let (package, record) = fixture();
    let source = frontend_source(&package, 4 + record.len());
    let image = assemble_source(&root, &scratch.0, &package, &record, &source);
    let mut expected = vec![0; 4];
    expected.extend_from_slice(&record);
    let case = OpforgeNativeCliParityCase {
        name: "actual-call-frontend-component",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/frontend-status.bin",
            rust_oracle: &expected,
        },
    };
    let result = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh actual native call frontend component completion");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    assert!(runs[0].success);
}

fn scratch() -> Scratch {
    let path = std::env::temp_dir().join(format!(
        "opforge-call-component-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&path).unwrap();
    Scratch(path)
}

#[test]
fn compact_call_preparation_component_host_assembly() {
    let (package, record) = fixture();
    let scratch = scratch();
    assert!(!assemble(&workspace_root(), &scratch.0, &package, &record).is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; numeric call preparation component, not frontend or full CLI parity"]
fn compact_call_preparation_component_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let root = workspace_root();
    let scratch = scratch();
    let (package, record) = fixture();
    let image = assemble(&root, &scratch.0, &package, &record);
    let mut expected = vec![0; 4];
    expected.extend_from_slice(&record);
    let case = OpforgeNativeCliParityCase {
        name: "actual-call-preparation-component",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(&record),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/prepared.bin",
            rust_oracle: &expected,
        },
    };
    let result = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh actual native call preparation component completion");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    assert!(runs[0].success);
}
