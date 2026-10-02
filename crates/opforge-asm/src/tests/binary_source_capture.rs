//! Owned lexical records replay after relocation and destruction of source text.
use super::*;

const SOURCE: &str = r#".module app
.cpu m68020
Count = 3
Frame .struct
Bytes .res Count
.endstruct
shared .macro value=7
.if .value==7
.byte "x@1"
.else
.byte .value
.endif
.endmacro
shared = 9
routine .block
.namespace inner
Local = 2
.shared 7
.shared 4
.byte Frame,shared,Local
.endnamespace
.bend
.byte routine
.byte "outside@1"
.endmodule
"#;

fn oracle(text: &str) -> Vec<u8> {
    let dir = create_temp_dir("capture-replay-oracle");
    let source = dir.join("main.asm");
    let output = dir.join("output.bin");
    fs::write(&source, text).unwrap();
    let cli = Cli::parse_from([
        "opforge".to_owned(),
        source.to_string_lossy().into_owned(),
        "--bin".to_owned(),
        output.to_string_lossy().into_owned(),
    ]);
    run_with_cli_with_context(&cli).expect("live Rust capture/replay oracle");
    let bytes = fs::read(output).unwrap();
    fs::remove_dir_all(dir).unwrap();
    bytes
}

fn capture_source() -> String {
    SOURCE
        .replace("shared = 9", "Other = 9")
        .replace("Frame,shared,Local", "Frame,Other,Local")
        .replace(".if .value==7", ".if .value")
        .replace(".shared 4", ".shared 0")
}

#[test]
fn compact_capture_replay_rust_oracle() {
    assert_eq!(
        oracle(&capture_source()),
        b"x7\x00\x03\x09\x02\x00outside@1"
    );
    for text in [SOURCE.to_owned(), SOURCE.replace("shared", "emit")] {
        assert_eq!(oracle(&text), b"x7\x04\x03\x09\x02\x00outside@1");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; direct and owned-record replay comparison"]
fn compact_capture_replay_fs_uae() {
    // Run separately with and without the explicit harness capture define. The
    // capture harness moves storage, frees its old block and poisons the original
    // source before replay; production behavior never inspects a case identity.
    native(&capture_source());
}

#[test]
#[ignore = "requires FS-UAE; retained complex macro-condition regression"]
fn compact_capture_macro_comparison_fs_uae() {
    // The unchanged 52f5ff9f reference also rejects the emit spelling at its
    // first call. Root cause remains unqualified; keep this positive regression.
    for text in [SOURCE.to_owned(), SOURCE.replace("shared", "emit")] {
        native(&text);
    }
}

fn native(text: &str) {
    let expected = oracle(text);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let native_root = std::env::var_os("OPFORGE_COMPARE_NATIVE_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(workspace_root);
    let outcome = crate::fs_uae_smoke::run_binary_source_harness_from_env(
        &native_root,
        &package,
        &[("main.asm", text.as_bytes())],
        &expected,
    )
    .expect("fresh native capture/replay proof");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.success && run.protocol_completed);
    assert_eq!(run.exit_code, Some(0));
    let image = &run.captured_artifacts[&PathBuf::from("Work/build/binary_source_harness")];
    eprintln!(
        "CAPTURE_REPLAY enabled={} seconds={:?} image_bytes={} linked_reserved_bytes={}",
        std::env::var("OPFORGE_CAPTURE_REPLAY_TEST").as_deref() == Ok("1"),
        run.start_to_done_host_seconds,
        image.len(),
        hunk::allocation(image).unwrap().total(),
    );
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        let memory = &run.captured_artifacts[&PathBuf::from("Work/memory.bin")];
        let words = memory
            .chunks_exact(4)
            .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(words[1], 0, "no live owned storage after cleanup");
        assert_eq!(words[3], words[4], "all allocation capacities are freed");
        eprintln!("CAPTURE_REPLAY_INSTRUMENTED peak_owned_bytes={}", words[2]);
    }
}
