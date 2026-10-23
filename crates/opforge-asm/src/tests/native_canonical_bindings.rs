//! Canonical lookup must not turn an unqualified source name into an explicit one.
use super::native_iterable_loops::scratch;
use super::*;

const PROBE: &str = r#"
SOURCE_ID_BASE = 100
REMAPPED_TARGET = 777
; A component probe uses the public binder ABI and persisted lexical records.
probeRun .block
    movem.l d1-d7/a0-a6, -(sp)
    moveq #88, d7
    lea Scope, a0
    moveq #SOURCE_ID_BASE, d0
    moveq #0, d1
    jsr scopes.begin
    bne.w finish
    lea Outer, a0
    moveq #5, d0
    lea Scope, a1
    jsr scopes.bindOrigin
    bne.w finish
    cmpi.l #SOURCE_ID_BASE, d1
    bne.w finish
    ; Current stores lexical index plus one, independently of source-ID base.
    subi.l #SOURCE_ID_BASE-1, d1
    move.w d1, Scope+layout.State.Current
    lea Item, a0
    moveq #4, d0
    lea Scope, a1
    jsr scopes.bindOrigin
    bne.w finish
    cmpi.l #SOURCE_ID_BASE+1, d1
    bne.w finish
    move.l d1, d6
    bsr.w readItem
    bne.w finish
    move.w Entry+records.Entry.Flags, d0
    andi.w #scopes.EXPLICIT, d0
    bne.w finish
    ; A remapped entry makes Target and source identity observably different.
    move.w #REMAPPED_TARGET, Entry+records.Entry.Target
    bsr.w writeItem
    bne.w finish
    clr.w Scope+layout.State.Current
    move.w #23, Scope+layout.State.FirstBound
    move.w #29, Scope+layout.State.FirstExplicit
    lea Qualified, a0
    moveq #10, d0
    lea Scope, a1
    jsr scopes.bindCanonical
    bne.w finish
    cmpi.l #REMAPPED_TARGET, d1
    bne.w finish
    bsr.w unchanged
    bne.w finish
    lea Qualified, a0
    moveq #10, d0
    lea Scope, a1
    jsr scopes.bindCanonicalOrigin
    bne.w finish
    cmp.l d6, d1
    bne.w finish
    bsr.w unchanged
    bne.w finish
    ; A newly allocated canonical miss also preserves source-line metadata.
    lea Missing, a0
    moveq #13, d0
    lea Scope, a1
    jsr scopes.bindCanonicalOrigin
    bne.w finish
    cmpi.l #SOURCE_ID_BASE+2, d1
    bne.w finish
    bsr.w unchanged
    bne.w finish
    lea Scope+layout.LEXICAL_STATE, a0
    lea Entry, a1
    moveq #2, d0
    jsr lexical.readEntry
    bne.w finish
    move.w Entry+records.Entry.Flags, d0
    andi.w #scopes.EXPLICIT, d0
    bne.w finish
    ; Ordinary source qualification still marks the entry and first name.
    lea Scope, a0
    jsr scopes.startLine
    lea Qualified, a0
    moveq #10, d0
    lea Scope, a1
    jsr scopes.bind
    bne.w finish
    cmpi.l #REMAPPED_TARGET, d1
    bne.w finish
    cmpi.w #1, Scope+layout.State.FirstBound
    bne.w finish
    cmpi.w #1, Scope+layout.State.FirstExplicit
    bne.w finish
    bsr.w readItem
    bne.w finish
    move.w Entry+records.Entry.Flags, d0
    andi.w #scopes.EXPLICIT, d0
    beq.w finish
    ; Canonical lookup cannot clear an existing explicit-source flag, and
    ; cannot claim the first source name on an otherwise empty line.
    lea Scope, a0
    jsr scopes.startLine
    .for originMode in {0, 1}
    lea Qualified, a0
    moveq #10, d0
    lea Scope, a1
    .if originMode
    jsr scopes.bindCanonicalOrigin
    .else
    jsr scopes.bindCanonical
    .endif
    bne.w finish
    tst.w Scope+layout.State.FirstBound
    bne.w finish
    tst.w Scope+layout.State.FirstExplicit
    bne.w finish
    bsr.w readItem
    bne.w finish
    move.w Entry+records.Entry.Flags, d0
    andi.w #scopes.EXPLICIT, d0
    beq.w finish
    .endfor
    moveq #0, d7
finish
    lea Scope, a0
    jsr scopes.release
    move.l d7, d0
    movem.l (sp)+, d1-d7/a0-a6
    tst.l d0
    rts
    .bend ; probeRun
readItem .block
    lea Scope+layout.LEXICAL_STATE, a0
    lea Entry, a1
    move.l d6, d0
    subi.l #SOURCE_ID_BASE, d0
    jsr lexical.readEntry
    rts
    .bend ; readItem
writeItem .block
    lea Scope+layout.LEXICAL_STATE, a0
    lea Entry, a1
    move.l d6, d0
    subi.l #SOURCE_ID_BASE, d0
    jsr lexical.writeEntry
    rts
    .bend ; writeItem
unchanged .block
    cmpi.w #23, Scope+layout.State.FirstBound
    bne.w bad
    cmpi.w #29, Scope+layout.State.FirstExplicit
    bne.w bad
    bsr.w readItem
    bne.w bad
    move.w Entry+records.Entry.Flags, d0
    andi.w #scopes.EXPLICIT, d0
    rts
bad
    moveq #88, d0
    rts
    .bend ; unchanged
"#;

fn source(root: &Path) -> String {
    // Reuse the maintained DOS artifact/exit protocol rather than another runner.
    let harness = fs::read_to_string(
        root.join("native/motorola68000/amigaos/tests/assembler/name_store_harness.asm"),
    )
    .unwrap();
    for (marker, count) in [
        ("opforge.tests.assembler.name_store_harness", 2),
        (".use opforge.tests.assembler.name_store_probe as probe", 1),
        ("jsr probe.run", 1),
        ("\t.endsection", 3),
        ("name-store-probe", 2),
        ("DosName\t.byte", 1),
        ("ReturnCode\t.res long, 1", 1),
    ] {
        assert_eq!(
            harness.matches(marker).count(),
            count,
            "harness drift: {marker}"
        );
    }
    harness
    .replace("opforge.tests.assembler.name_store_harness", "opforge.tests.assembler.canonical_binding_harness")
    .replace(
        ".use opforge.tests.assembler.name_store_probe as probe",
        concat!(
            ".use opforge.assembler.symbols.scopes as scopes\n",
            "    .use opforge.assembler.symbols.scope_layout as layout\n",
            "    .use opforge.assembler.symbols.binding_records as records\n",
            "    .use opforge.assembler.symbols.lexical_store as lexical"
        ),
    )
    .replace("jsr probe.run", "bsr.w probeRun")
    .replacen("\t.endsection", &format!("{PROBE}\n\t.endsection"), 1)
    .replace("name-store-probe", "canonical-binding-probe")
    .replace("DosName\t.byte", "Outer .byte \"outer\"\nItem .byte \"item\"\nQualified .byte \"outer.item\"\nMissing .byte \"outer.missing\"\nDosName\t.byte")
    .replace("ReturnCode\t.res long, 1", "ReturnCode\t.res long, 1\nScope .res byte, scopes.SCRATCH_BYTES\nEntry .res byte, records.ENTRY_BYTES")
}

fn image(root: &Path, dir: &Path, source: &str) -> Vec<u8> {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let entry = dir.join("entry.asm");
    fs::write(&entry, source).unwrap();
    let native = root.join("native/motorola68000/amigaos");
    let cli = Cli::parse_from(vec![
        "opForge".into(),
        entry.to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        native.to_string_lossy().into_owned(),
        "-I".into(),
        native.to_string_lossy().into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.into());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "canonical binding probe: {error}; diagnostics: {:?}",
            error.diagnostics()
        ),
        other => panic!("canonical binding probe: {other:?}"),
    });
    fs::read(dir.join("canonical-binding-probe.hunk")).unwrap()
}

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

#[test]
fn native_canonical_bindings_host_assembles() {
    let root = root();
    let dir = scratch();
    let image = image(&root, &dir.0, &source(&root));
    assert!(!image.is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; canonical binder flags, metadata and IDs"]
fn native_canonical_bindings_fs_uae() {
    let root = root();
    let dir = scratch();
    let source = source(&root);
    let image = image(&root, &dir.0, &source);
    let case = OpforgeNativeCliParityCase {
        name: "canonical-binding-probe",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/canonical-binding-probe.bin",
            rust_oracle: &[0x5a],
        },
    };
    let _guard = fs_uae_native_cli_smoke_lock().lock().unwrap();
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .unwrap_or_else(|error| panic!("fresh canonical binding proof: {error}"));
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
}
