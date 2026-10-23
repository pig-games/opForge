//! Bounded native Hunk generator: assembly and separately opt-in native proof.
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn scratch() -> Scratch {
    let path = std::env::temp_dir().join(format!(
        "opforge-hunk-stream-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir_all(&path).unwrap();
    Scratch(path)
}

fn live_oracle(dir: &Path) -> Vec<u8> {
    let path = dir.join("oracle.asm");
    fs::write(&path, oracle_source()).unwrap();
    let cli = Cli::parse_from(["opForge".into(), path.to_string_lossy().into_owned()]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_owned());
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    fs::read(dir.join("out.hunk")).unwrap()
}

fn oracle_source() -> &'static str {
    ".module entry\n.cpu 68020\n\
     .section code,kind=code\n\
     .for index in 0..32\n.long dataEntry,bssEntry\n.endfor\n\
     .byte $a5\n.endsection\n\
     .section data,kind=data\ndataEntry .byte 7,8,9\n.endsection\n\
     .section vars,kind=bss\nbssEntry .ds 17\n.endsection\n\
     .output \"out.hunk\",format=hunk,sections=code,data,vars\n.endmodule\n"
}

fn build_harness(root: &Path, dir: &Path) -> Vec<u8> {
    let source = root
        .join("native/motorola68000/amigaos/test-harnesses/experimental/hunk_stream_harness.asm");
    let cli = Cli::parse_from([
        "opForge".into(),
        source.to_string_lossy().into_owned(),
        "-M".into(),
        root.join("native/motorola68000")
            .to_string_lossy()
            .into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_owned());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "assemble Hunk stream harness: {}; diagnostics: {:?}",
            error.error(),
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("assemble Hunk stream harness: {error}")
        }
        cli_core::CliRunError::WarningsAsErrors { .. } => {
            panic!("assemble Hunk stream harness: warnings treated as errors")
        }
    });
    fs::read(dir.join("build/hunk_stream_harness")).unwrap()
}

#[test]
fn native_hunk_stream_host_assembles_and_has_live_rust_oracle() {
    let dir = scratch();
    let image = build_harness(&workspace_root(), &dir.0);
    assert!(!image.is_empty());
    let oracle = live_oracle(&dir.0);
    let segments = crate::fs_uae_smoke::hunk::segments(&oracle).unwrap();
    assert_eq!(oracle.len(), 612);
    assert_eq!(segments.len(), 3);
    assert_eq!(segments[0].payload.len(), 260);
    assert_eq!(&segments[0].payload[256..], &[0xa5, 0, 0, 0]);
    assert_eq!(segments[0].relocations.len(), 64);
    let expected: Vec<_> = [1, 2]
        .into_iter()
        .flat_map(|target| (0..32).map(move |i| (i * 8 + (target - 1) * 4, target)))
        .collect();
    assert_eq!(segments[0].relocations, expected);
    assert_eq!(segments[1].payload, &[7, 8, 9, 0]);
    assert_eq!(segments[2].reserved_bytes, 20);
    assert!(segments[2].payload.is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; fresh stream/reference/live Rust comparison"]
fn native_hunk_stream_fs_uae() {
    let dir = scratch();
    let root = workspace_root();
    let image = build_harness(&root, &dir.0);
    let oracle = live_oracle(&dir.0);
    let case = crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "bounded-hunk-stream",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        // The direct harness owns its fixture and ignores CLI input. Carry
        // the exact live oracle source in the fresh run's case identity too.
        source_override: Some(oracle_source().as_bytes()),
        command_template: Some(""),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/output.hunk",
            rust_oracle: &oracle,
        },
    };
    let outcome = crate::fs_uae_smoke::run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("run focused Hunk stream harness");
    match outcome {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => panic!("{reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            assert!(runs[0].success && runs[0].protocol_completed);
            assert_eq!(runs[0].exit_code, Some(0));
            assert_eq!(verified_fs_uae_output(&runs[0]), oracle);
        }
    }
}
