//! Actual capture constructor on poisoned storage; host assembly is not native execution.
use super::native_iterable_loops::scratch;
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

const SOURCE_BYTES: u32 = 0x1234_5678;
const ORIGIN: u32 = 0x8765_4321;
const OUTCOMES: [u32; 7] = [
    0x1020_3040,
    0x5060_7080,
    0x90a0_b0c0,
    0xd0e0_f001,
    0x1122_3344,
    0x5566_7788,
    0x99aa_bbcc,
];

fn token_rows(count: usize, lexemes: usize) -> Vec<u8> {
    let mut bytes = Vec::new();
    for row in 0..count {
        bytes.extend_from_slice(&(if row % 2 == 0 { 1u16 } else { 40 }).to_be_bytes());
        bytes.extend_from_slice(&(0xa500u16 + row as u16).to_be_bytes());
        for value in [
            0x0010_2030 + row as u32,
            0x0010_2031 + row as u32,
            0,
            lexemes as u32,
        ] {
            bytes.extend_from_slice(&value.to_be_bytes());
        }
    }
    bytes
}

fn oracle(count: usize, lexemes: usize, plans: usize) -> Vec<u8> {
    // Independent wire-layout oracle: explicit fields, complete raw token rows,
    // and zero-filled alignment. Frame.Reserved deliberately is not serialized.
    let tokens = token_rows(count, lexemes);
    let plan_offset = (64 + tokens.len() + lexemes + 3) & !3;
    let bytes = (plan_offset + plans + 3) & !3;
    let mut result = Vec::new();
    for value in [
        0x4243_5031,
        bytes as u32,
        count as u32,
        lexemes as u32,
        SOURCE_BYTES,
        ORIGIN,
        65535,
    ] {
        result.extend_from_slice(&value.to_be_bytes());
    }
    result.extend_from_slice(&1u16.to_be_bytes());
    result.extend_from_slice(&0u16.to_be_bytes());
    result.extend_from_slice(&(plans as u32).to_be_bytes());
    for value in OUTCOMES {
        result.extend_from_slice(&value.to_be_bytes());
    }
    assert_eq!(result.len(), 64);
    result.extend(tokens);
    result.extend((0..lexemes).map(|i| 0x31 + i as u8));
    result.resize(plan_offset, 0);
    result.extend((0..plans).map(|i| 0xe1 + i as u8));
    result.resize(bytes, 0);
    result
}

fn emit_bytes(label: &str, bytes: &[u8]) -> String {
    let mut source = format!("{label}\n");
    for chunk in bytes.chunks(20) {
        source.push_str(" .byte ");
        source.push_str(
            &chunk
                .iter()
                .map(|v| format!("${v:02x}"))
                .collect::<Vec<_>>()
                .join(","),
        );
        source.push('\n');
    }
    source.push_str(" .align 4\n");
    source
}

fn cases() -> Vec<(usize, usize, usize)> {
    let mut cases = Vec::new();
    for lexemes in 0..4 {
        for plans in 0..4 {
            cases.push((2, lexemes, plans));
        }
    }
    cases.extend([(0, 0, 0), (64, 7, 7)]);
    cases
}

fn probe_source() -> String {
    let root = workspace_root();
    let capture = fs::read_to_string(
        root.join("native/motorola68000/amigaos/experimental/binary_capture.asm"),
    )
    .unwrap();
    let memory = fs::read_to_string(
        root.join("native/motorola68000/amigaos/experimental/binary_memory.asm"),
    )
    .unwrap();
    let writer = fs::read_to_string(
        root.join("native/motorola68000/amigaos/experimental/binary_source.asm"),
    )
    .unwrap();
    // The writer implementation brings the whole lowering pipeline. Capture
    // consumes only its public Token ABI; take that declaration from its owner.
    let token_abi = writer.split_once("\nToken\t.struct\n").unwrap().1;
    let token_abi = token_abi.split_once("\t.endstruct").unwrap().0;
    let mut checks = String::new();
    let mut data = String::new();
    for (index, (count, lexemes, plans)) in cases().into_iter().enumerate() {
        let expected = oracle(count, lexemes, plans);
        data.push_str(&emit_bytes(
            &format!("Tokens{index}"),
            &token_rows(count, lexemes),
        ));
        data.push_str(&emit_bytes(&format!("Expected{index}"), &expected));
        checks.push_str(&format!(
            r#"
 bsr.w poison
 move.l #{count},capture.Frame.Count+Frame
 move.l #Tokens{index},capture.Frame.Tokens+Frame
 move.l #{lexemes},capture.Frame.LexemeBytes+Frame
 move.l #{plans},memory.Block.Used+Plans
 move.l #Expected{index},Expected
 move.l #{bytes},RecordBytes
 move.l #1,ExpectedHandle
 move.l #{bytes},ExpectedUsed
 bsr.w checkedCreate
 bne.w bad
 bsr.w compareRecords
 bne.w bad
 move.l #{handle},ExpectedHandle
 move.l #{twice},ExpectedUsed
 bsr.w checkedCreate
 bne.w bad
 bsr.w compareRecords
 bne.w bad
 cmpi.b #$a5,ArenaBytes+{twice}
 bne.w bad
"#,
            bytes = expected.len(),
            handle = expected.len() + 1,
            twice = expected.len() * 2
        ));
    }
    // Leave a valid published prefix, then reject every boundary without
    // publishing the next record. LIMIT rejection precedes all payload reads.
    for setup in [
        "move.l #65,capture.Frame.Count+Frame",
        "move.l #65536,capture.Frame.Line+Frame",
        "move.l #InvalidTokens,capture.Frame.Tokens+Frame",
        "move.l #$fffffffc,capture.Frame.LexemeBytes+Frame",
        "move.l #memory.LIMIT+1,capture.Frame.LexemeBytes+Frame",
    ] {
        checks.push_str(&format!(
            r#"
 move.l #2,capture.Frame.Count+Frame
 move.l #Tokens15,capture.Frame.Tokens+Frame
 move.l #3,capture.Frame.LexemeBytes+Frame
 move.l #65535,capture.Frame.Line+Frame
 move.l #3,memory.Block.Used+Plans
 {setup}
 move.l memory.Block.Used+Arena,ExpectedUsed
 clr.l ExpectedHandle
 bsr.w checkedCreate
 bne.w bad
 bsr.w compareRecords
 bne.w bad
"#
        ));
    }
    format!(
        r#".module capture.initialization_probe
.cpu 68020
.use experimental.amigaos.binary_capture as capture
.use experimental.amigaos.binary_memory as memory
.section entry,kind=code
start .block
 movem.l d1-d7/a0-a6,-(sp)
 bsr.w exercise
 movem.l (sp)+,d1-d7/a0-a6
 rts
.bend
exercise .block
{checks}
 moveq #0,d0
 rts
bad
 moveq #20,d0
 rts
.bend
; Reuse a preallocated region without invoking AllocMem or freeing static data.
poison .block
 clr.l memory.Block.Used+Arena
 lea ArenaBytes,a0
 move.w #4095,d0
next
 move.b #$a5,(a0)+
 dbra d0,next
 rts
.bend
; Assert actual public register, stack and D0/CCR status ABI on every call.
checkedCreate .block
 movem.l Seeds,d0-d7/a0-a6
 lea Frame,a0
 movem.l d2-d7/a0-a6,Before
 move.l sp,BeforeStack
 move.w #31,ccr
 jsr capture.create
 move.w ccr,AfterCCR
 movem.l d2-d7/a0-a6,After
 cmpa.l BeforeStack,sp
 bne.w bad
 tst.l ExpectedHandle
 beq.w rejected
 tst.l d0
 bne.w bad
 cmp.l ExpectedHandle,d1
 bne.w bad
 moveq #4,d0
 bra.w status
rejected
 tst.l d0
 beq.w bad
 tst.l d1
 bne.w bad
 moveq #0,d0
status
 move.w AfterCCR,d1
 andi.w #15,d1
 cmp.w d0,d1
 bne.w bad
 move.l memory.Block.Used+Arena,d0
 cmp.l ExpectedUsed,d0
 bne.w bad
 lea ArenaBytes,a0
 cmpa.l memory.Block.Pointer+Arena,a0
 bne.w bad
 cmpi.l #4096,memory.Block.Capacity+Arena
 bne.w bad
 lea Before,a0
 lea After,a1
 moveq #12,d0
registers
 move.l (a0)+,d1
 cmp.l (a1)+,d1
 bne.w bad
 dbra d0,registers
 moveq #0,d0
 rts
bad
 moveq #20,d0
 rts
.bend
; Compare both complete records, also preserving the previously published one.
compareRecords .block
 lea ArenaBytes,a0
 move.l ExpectedUsed,d2
nextRecord
 movea.l Expected,a1
 move.l RecordBytes,d0
nextByte
 move.b (a0)+,d1
 cmp.b (a1)+,d1
 bne.w bad
 subq.l #1,d0
 bne.w nextByte
 sub.l RecordBytes,d2
 bne.w nextRecord
 moveq #0,d0
 rts
bad
 moveq #20,d0
 rts
.bend
.endsection
.section data,kind=data
Seeds .long $101,$202,$303,$404,$505,$606,$707,$808
 .long $909,$a0a,$b0b,$c0c,$d0d,$e0e,$f0f
Arena .long ArenaBytes,4096,0
Plans .long PlanBytes,7,0
Frame .long Arena,0,0,LexemeBytes,0,${SOURCE_BYTES:08x},${ORIGIN:08x},65535
 .word 1,$beef
 .long Plans,{outcomes}
LexemeBytes .byte $31,$32,$33,$34,$35,$36,$37
PlanBytes .byte $e1,$e2,$e3,$e4,$e5,$e6,$e7
 .align 4
InvalidTokens .word 1,$cdef
 .long $102030,$102031,0,4
 .word 40,$abcd
 .long $102032,$102033,0,0
{data}
.endsection
.section bss,kind=bss
ArenaBytes .res byte,4096
Before .res long,13
After .res long,13
BeforeStack .res long,1
Expected .res long,1
RecordBytes .res long,1
ExpectedHandle .res long,1
ExpectedUsed .res long,1
AfterCCR .res word,1
.endsection
.output "build/capture-initialization-probe",format=hunk,sections=entry,code,data,bss
.endmodule
{capture}
{memory}
.module experimental.amigaos.binary_source
.cpu 68020
.pub
Token .struct
{token_abi}.endstruct
.endmodule
.end
"#,
        outcomes = OUTCOMES
            .iter()
            .map(|v| format!("${v:08x}"))
            .collect::<Vec<_>>()
            .join(",")
    )
}

fn build_probe(dir: &Path) -> (String, Vec<u8>) {
    let source = probe_source();
    let path = dir.join("capture-initialization-probe.asm");
    fs::write(&path, &source).unwrap();
    let cli = Cli::parse_from([
        "opForge",
        path.to_str().unwrap(),
        "-I",
        workspace_root()
            .join("native/motorola68000/amigaos/debug")
            .to_str()
            .unwrap(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "constructor probe assembly: {}; {:?}; source: {:?}",
            error.summary(),
            error.diagnostics(),
            error
                .diagnostics()
                .iter()
                .map(|d| source.lines().nth(d.line as usize - 1))
                .collect::<Vec<_>>()
        ),
        error => panic!("constructor probe assembly: {error:?}"),
    });
    let image = fs::read(dir.join("build/capture-initialization-probe")).unwrap();
    (source, image)
}

#[test]
fn native_capture_initialization_rust_oracle_and_actual_probe_host_assembles() {
    for (count, lexemes, plans) in cases() {
        let expected = oracle(count, lexemes, plans);
        assert_eq!(&expected[30..32], &[0, 0]);
        assert_eq!(&expected[64..64 + count * 20], &token_rows(count, lexemes));
        let end_lexemes = 64 + count * 20 + lexemes;
        let start_plans = (end_lexemes + 3) & !3;
        assert!(expected[end_lexemes..start_plans].iter().all(|&v| v == 0));
        assert!(expected[start_plans + plans..].iter().all(|&v| v == 0));
    }
    let dir = scratch();
    let (_, image) = build_probe(&dir.0);
    assert_eq!(&image[..4], &0x3f3u32.to_be_bytes());
}

#[test]
#[ignore = "requires configured FS-UAE; actual capture constructor poisoned-storage and ABI probe"]
fn native_capture_initialization_poisoned_storage_fs_uae() {
    let dir = scratch();
    let (source, image) = build_probe(&dir.0);
    let case = OpforgeNativeCliParityCase {
        name: "capture-constructor-poisoned-storage-probe",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::SuccessfulExitWithoutArtifacts(&["Work/out.bin"]),
    };
    let FsUaeSmokeOutcome::Completed { runs } =
        run_prebuilt_compact_cli_case_from_env(&workspace_root(), &case, &image)
            .expect("fresh actual capture constructor probe")
    else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed && runs[0].success);
    assert_eq!(
        runs[0].exit_code,
        Some(0),
        "every constructor byte, publication and ABI check passed"
    );
}
