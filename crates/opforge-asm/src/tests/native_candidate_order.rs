// Shared discovery sort is assembled here independently of the complete app.
fn candidate_order_probe(root: &Path, dir: &Path) -> (Vec<u8>, Vec<u8>) {
    let permutations: &[&[&str]] = &[
        &["zz-entry.asm", "b.asm", "a.asm", "a.asm", "Z.asm"],
        &["zz-entry.asm", "Z.asm", "a.asm", "b.asm", "a.asm"],
        &["zz-entry.asm", "a.asm", "b.asm", "Z.asm", "a.asm"],
    ];
    let mut source = String::from(
        r#".module tests.amigaos.candidate_order
.cpu 68020
PATH_BYTES=256
PATH_COUNT=5
PERMUTATION_COUNT=3
RESULT_BYTES=PATH_BYTES*PATH_COUNT*PERMUTATION_COUNT
EXEC_BASE=4
EXEC_OPEN_LIBRARY=-552
EXEC_CLOSE_LIBRARY=-414
DOS_OPEN=-30
DOS_WRITE=-48
DOS_CLOSE=-36
MODE_NEWFILE=1006
DOS_MIN_VERSION=36
.section entry,kind=code
.pub
start .block
 movem.l d2-d7/a2-a6,-(sp)
"#,
    );
    for index in 0..permutations.len() {
        source.push_str(&format!(
            " lea Rows{index},a0\n moveq #PATH_COUNT,d0\n bsr.w orderCandidatePaths\n"
        ));
    }
    source.push_str(
        r#" lea DosName,a1
 moveq #DOS_MIN_VERSION,d0
 movea.l EXEC_BASE.w,a6
 jsr EXEC_OPEN_LIBRARY(a6)
 move.l d0,DosBase
 beq.w failed
 movea.l d0,a6
 move.l #OutputName,d1
 move.l #MODE_NEWFILE,d2
 jsr DOS_OPEN(a6)
 move.l d0,Handle
 beq.w closeLibrary
 move.l d0,d1
 move.l #Rows0,d2
 move.l #RESULT_BYTES,d3
 jsr DOS_WRITE(a6)
 move.l d0,Written
 move.l Handle,d1
 jsr DOS_CLOSE(a6)
 tst.l d0
 beq.w closeLibrary
 cmpi.l #RESULT_BYTES,Written
 bne.w closeLibrary
 clr.l ReturnCode
closeLibrary
 movea.l DosBase,a1
 movea.l EXEC_BASE.w,a6
 jsr EXEC_CLOSE_LIBRARY(a6)
failed
 move.l ReturnCode,d0
 movem.l (sp)+,d2-d7/a2-a6
 rts
 .bend ; start
 .include "candidate_order.i"
.endsection
.section data,kind=data
ReturnCode .long 20
DosName .byte "dos.library",0
OutputName .byte "Work:output.bin",0
.align 4
"#,
    );
    let mut oracle = Vec::new();
    for (index, permutation) in permutations.iter().enumerate() {
        source.push_str(&format!("Rows{index}\n"));
        for name in *permutation {
            source.push_str(&format!(
                " .byte \"{name}\",0\n .fill byte, {}, 0\n",
                255 - name.len()
            ));
        }
        let mut sorted = permutation[1..].to_vec();
        sorted.sort();
        for name in std::iter::once(permutation[0]).chain(sorted) {
            oracle.extend_from_slice(name.as_bytes());
            oracle.resize(oracle.len() + 256 - name.len(), 0);
        }
    }
    source.push_str(
        r#".endsection
.section bss,kind=bss
DosBase .res long,1
Handle .res long,1
Written .res long,1
.endsection
.output "candidate-order.hunk",format=hunk,sections=entry,data,bss
.endmodule
"#,
    );
    let path = dir.join("candidate_order.asm");
    fs::write(&path, source).unwrap();
    let cli = Cli::parse_from([
        "opforge".to_owned(),
        path.to_string_lossy().into_owned(),
        "-I".to_owned(),
        root.join("native/motorola68000/amigaos/assembler/source")
            .to_string_lossy()
            .into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    (fs::read(dir.join("candidate-order.hunk")).unwrap(), oracle)
}

#[test]
fn native_candidate_order_host_assembles() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    let (image, oracle) = candidate_order_probe(&root, &dir);
    assert!(!image.is_empty());
    assert_eq!(oracle.len(), 3 * 5 * 256);
}

#[test]
#[ignore = "requires configured FS-UAE; actual shared discovery sort with permutations and duplicates"]
fn native_candidate_order_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    let (image, oracle) = candidate_order_probe(&root, &dir);
    let case = OpforgeNativeCliParityCase {
        name: "native-candidate-order",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(b""),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/output.bin",
            rust_oracle: &oracle,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image).unwrap();
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed && runs[0].success);
    assert_eq!(runs[0].exit_code, Some(0));
    assert_eq!(runs[0].verified_output.as_deref(), Some(oracle.as_slice()));
}
