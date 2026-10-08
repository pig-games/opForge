//! Guard block release and reuse are observed at the native component boundary.
use std::path::{Path, PathBuf};

const OUTPUT: &str = "Work/guard-reuse.bin";
const HARNESS_NAME: &str = "guard-reuse-probe.hunk";

struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn harness_source() -> String {
    r#".module guard_reuse_probe
.cpu 68020
.use experimental.amigaos.binary_guard_values as guards
.use experimental.amigaos.binary_memory as memory
.use experimental.amigaos.binary_package as pkg
.use opasm.amigaos.binary_expression as expression
.use prvm.amigaos.abi as abi
.use exprvm.amigaos.runtime as runtime
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
COMPILED_EXPRESSION_BYTES = 1+2+3
WHILE_RECORD_BYTES = abi.LoopRecord.VariableKind+COMPILED_EXPRESSION_BYTES
ENDWHILE_RECORD_BYTES = abi.LoopRecord.VariableKind
GuardCondition .struct
Tag .byte ?
ProgramBytes .word ?
Opcode .byte ?
Value .byte ?
End .byte ?
.endstruct
ResultFields .struct
ReserveStatus .long ?
InitialUsed .long ?
InitialCapacity .long ?
InitialPointer .long ?
WhilePrepareStatus .long ?
WhilePassStatus .long ?
WhileEnterStatus .long ?
WhileLeaveStatus .long ?
WhilePreparedPass .long ?
WhileUsed .long ?
WhileCapacity .long ?
WhilePointer .long ?
WhileReleaseUsed .long ?
WhileReleaseCapacity .long ?
WhileReleasePointer .long ?
EmptyPrepareStatus .long ?
EmptyPassStatus .long ?
EmptyEnterStatus .long ?
EmptyLeaveStatus .long ?
EmptyUsed .long ?
EmptyCapacity .long ?
EmptyPointer .long ?
EmptyReleaseUsed .long ?
EmptyReleaseCapacity .long ?
EmptyReleasePointer .long ?
.endstruct
.section entry,kind=code
.pub
start .block
 movem.l d2-d7/a2-a6,-(sp)
 ; Start with one valid allocated row, as if a preceding prepare succeeded.
 lea Guard+guards.State.Block,a0
 move.l #guards.ROW_BYTES,d0
 jsr memory.reserve
 move.l d0,Results+ResultFields.ReserveStatus
 move.l #guards.ROW_BYTES,Guard+guards.State.Block+memory.Block.Used
 lea Guard,a0
 jsr guards.release
 move.l Guard+guards.State.Block+memory.Block.Used,Results+ResultFields.InitialUsed
 move.l Guard+guards.State.Block+memory.Block.Capacity,Results+ResultFields.InitialCapacity
 move.l Guard+guards.State.Block+memory.Block.Pointer,Results+ResultFields.InitialPointer

 ; Construct the exact bounded raw records consumed by guards.prepare.
 lea LoopRecords,a0
 move.b #WHILE_RECORD_BYTES-1,(a0)
 move.b #abi.PRVM_TOKEN_KIND_DOT,abi.LoopRecord.HeadDot(a0)
 move.b #abi.PRVM_TOKEN_KIND_IDENTIFIER,abi.LoopRecord.HeadKind(a0)
 lea Package,a1
 adda.l pkg.Header.LoopPlan(a1),a1
 move.b abi.LoopProgram.WhileHigh(a1),abi.LoopRecord.HeadHigh(a0)
 move.b abi.LoopProgram.WhileLow(a1),abi.LoopRecord.HeadLow(a0)
 move.b #expression.COMPILED_TAG,abi.LoopRecord.VariableKind+GuardCondition.Tag(a0)
 move.w #3,abi.LoopRecord.VariableKind+GuardCondition.ProgramBytes(a0)
 move.b #runtime.COMPACT_I8,abi.LoopRecord.VariableKind+GuardCondition.Opcode(a0)
 move.b #0,abi.LoopRecord.VariableKind+GuardCondition.Value(a0)
 move.b #runtime.EXPRVM_OPCODE_END,abi.LoopRecord.VariableKind+GuardCondition.End(a0)
 lea WHILE_RECORD_BYTES(a0),a0
 move.b #ENDWHILE_RECORD_BYTES-1,(a0)
 move.b #abi.PRVM_TOKEN_KIND_DOT,abi.LoopRecord.HeadDot(a0)
 move.b #abi.PRVM_TOKEN_KIND_IDENTIFIER,abi.LoopRecord.HeadKind(a0)
 move.b abi.LoopProgram.EndwhileHigh(a1),abi.LoopRecord.HeadHigh(a0)
 move.b abi.LoopProgram.EndwhileLow(a1),abi.LoopRecord.HeadLow(a0)

 lea Guard,a0
 lea LoopRecords,a1
 moveq #WHILE_RECORD_BYTES+ENDWHILE_RECORD_BYTES,d0
 lea Package,a3
 move.l a3,ContextData+pkg.Context.Package
 lea ContextData,a2
 jsr guards.prepare
 move.l d0,Results+ResultFields.WhilePrepareStatus
 move.l Guard+guards.State.Pass,Results+ResultFields.WhilePreparedPass
 moveq #1,d0
 lea Guard,a0
 jsr guards.beginPass
 move.l d0,Results+ResultFields.WhilePassStatus
 lea Guard,a0
 lea ContextData,a2
 jsr guards.enter
 move.l d0,Results+ResultFields.WhileEnterStatus
 lea Guard,a0
 lea ContextData,a2
 jsr guards.leave
 move.l d0,Results+ResultFields.WhileLeaveStatus
 move.l Guard+guards.State.Block+memory.Block.Used,Results+ResultFields.WhileUsed
 move.l Guard+guards.State.Block+memory.Block.Capacity,Results+ResultFields.WhileCapacity
 move.l Guard+guards.State.Block+memory.Block.Pointer,Results+ResultFields.WhilePointer
 lea Guard,a0
 jsr guards.release
 move.l Guard+guards.State.Block+memory.Block.Used,Results+ResultFields.WhileReleaseUsed
 move.l Guard+guards.State.Block+memory.Block.Capacity,Results+ResultFields.WhileReleaseCapacity
 move.l Guard+guards.State.Block+memory.Block.Pointer,Results+ResultFields.WhileReleasePointer

 ; Reuse the same state for a bounded empty package record stream.
 lea Guard,a0
 suba.l a1,a1
 moveq #0,d0
 lea ContextData,a2
 jsr guards.prepare
 move.l d0,Results+ResultFields.EmptyPrepareStatus
 moveq #1,d0
 lea Guard,a0
 jsr guards.beginPass
 move.l d0,Results+ResultFields.EmptyPassStatus
 lea Guard,a0
 lea ContextData,a2
 jsr guards.enter
 move.l d0,Results+ResultFields.EmptyEnterStatus
 lea Guard,a0
 lea ContextData,a2
 jsr guards.leave
 move.l d0,Results+ResultFields.EmptyLeaveStatus
 move.l Guard+guards.State.Block+memory.Block.Used,Results+ResultFields.EmptyUsed
 move.l Guard+guards.State.Block+memory.Block.Capacity,Results+ResultFields.EmptyCapacity
 move.l Guard+guards.State.Block+memory.Block.Pointer,Results+ResultFields.EmptyPointer
 lea Guard,a0
 jsr guards.release
 move.l Guard+guards.State.Block+memory.Block.Used,Results+ResultFields.EmptyReleaseUsed
 move.l Guard+guards.State.Block+memory.Block.Capacity,Results+ResultFields.EmptyReleaseCapacity
 move.l Guard+guards.State.Block+memory.Block.Pointer,Results+ResultFields.EmptyReleasePointer

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
 move.l #Results,d2
 move.l #ResultFields.EmptyReleasePointer+4,d3
 jsr DOS_WRITE(a6)
 move.l d0,d5
 move.l d4,d1
 jsr DOS_CLOSE(a6)
 tst.l d0
 beq.w failed
 cmpi.l #ResultFields.EmptyReleasePointer+4,d5
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
OutputName .byte "Work:guard-reuse.bin",0
.align 2
Package .incbin "package.bin"
.align 2
.endsection
.section bss,kind=bss
.align 2
ContextData .res byte,pkg.CONTEXT_BYTES
Guard .res byte,guards.STATE_BYTES
LoopRecords .res byte,WHILE_RECORD_BYTES+ENDWHILE_RECORD_BYTES
Results .res byte,ResultFields.EmptyReleasePointer+4
.endsection
.output "guard-reuse-probe.hunk",format=hunk,sections=entry,code,data,bss
.endmodule
"#
    .to_owned()
}

fn assemble(root: &Path, scratch: &Path) -> Vec<u8> {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let input = scratch.join("entry.asm");
    std::fs::write(&input, harness_source()).unwrap();
    let cli = Cli::parse_from([
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
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(scratch.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config)
        .expect("assemble native guard reuse component harness");
    std::fs::read(scratch.join(HARNESS_NAME)).unwrap()
}

fn package() -> Vec<u8> {
    let core = vm::runtime_model_core::RuntimeModelCore::from_registry(
        &::engine::build_default_asm_registry(),
    )
    .expect("runtime model");
    let resolved = core
        .resolve_pipeline("m68020", None)
        .expect("68020 pipeline");
    crate::binary_source_experiment::prepare_package(&core, &resolved)
        .expect("current 68020 package contract")
}

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

fn expected() -> Vec<u8> {
    // Statuses are zero; release clears pointer/capacity while guard state keeps
    // Used zero. The first prepare sees a valid while envelope and no constants.
    #[derive(Default)]
    struct Fields {
        reserve_status: u32,
        initial_used: u32,
        initial_capacity: u32,
        initial_pointer: u32,
        while_prepare_status: u32,
        while_pass_status: u32,
        while_enter_status: u32,
        while_leave_status: u32,
        while_prepared_pass: u32,
        while_used: u32,
        while_capacity: u32,
        while_pointer: u32,
        while_release_used: u32,
        while_release_capacity: u32,
        while_release_pointer: u32,
        empty_prepare_status: u32,
        empty_pass_status: u32,
        empty_enter_status: u32,
        empty_leave_status: u32,
        empty_used: u32,
        empty_capacity: u32,
        empty_pointer: u32,
        empty_release_used: u32,
        empty_release_capacity: u32,
        empty_release_pointer: u32,
    }
    let fields = Fields {
        while_prepared_pass: 1,
        ..Fields::default()
    };
    [
        fields.reserve_status,
        fields.initial_used,
        fields.initial_capacity,
        fields.initial_pointer,
        fields.while_prepare_status,
        fields.while_pass_status,
        fields.while_enter_status,
        fields.while_leave_status,
        fields.while_prepared_pass,
        fields.while_used,
        fields.while_capacity,
        fields.while_pointer,
        fields.while_release_used,
        fields.while_release_capacity,
        fields.while_release_pointer,
        fields.empty_prepare_status,
        fields.empty_pass_status,
        fields.empty_enter_status,
        fields.empty_leave_status,
        fields.empty_used,
        fields.empty_capacity,
        fields.empty_pointer,
        fields.empty_release_used,
        fields.empty_release_capacity,
        fields.empty_release_pointer,
    ]
    .into_iter()
    .flat_map(u32::to_be_bytes)
    .collect()
}

#[test]
fn native_guard_reuse_component_host_assembly() {
    let root = workspace_root();
    let scratch = Scratch(std::env::temp_dir().join(format!(
        "opforge-guard-reuse-host-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    )));
    std::fs::create_dir_all(&scratch.0).unwrap();
    let package = package();
    std::fs::write(scratch.0.join("package.bin"), package).unwrap();
    let image = assemble(&root, &scratch.0);
    assert!(!image.is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; native guard-state reuse component observation only"]
fn native_guard_reuse_component_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let root = workspace_root();
    let scratch = Scratch(std::env::temp_dir().join(format!(
        "opforge-guard-reuse-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    )));
    std::fs::create_dir_all(&scratch.0).unwrap();
    let package = package();
    std::fs::write(scratch.0.join("package.bin"), &package).unwrap();
    let image = assemble(&root, &scratch.0);
    if let Some(path) = std::env::var_os("OPFORGE_GUARD_REUSE_SAVE_IMAGE") {
        std::fs::write(path, &image).unwrap();
    }
    let expected = expected();
    let case = OpforgeNativeCliParityCase {
        name: "guard-state-reuse-component",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(&package),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: OUTPUT,
            rust_oracle: &expected,
        },
    };
    let result = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh native guard reuse component completion");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed && runs[0].success);
    assert_eq!(runs[0].exit_code, Some(0));
    eprintln!(
        "GUARD_REUSE bytes={expected:02x?} image_bytes={}",
        image.len()
    );
}
