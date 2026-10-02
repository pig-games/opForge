//! Assembly-stage provenance and structural instruction forms used by self-host.
use super::*;
use crate::native_package_build::{build_native_packages, EmbedSelection};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

const SOURCE: &str = r#".module main
.cpu 68020
.use storage as dep
Header .struct
Pad .res 130
Flags .word ?
.endstruct
.section code,kind=code
start
 btst #0,Header.Flags+1(a2)
 cmpa.l a4,a3
 blo.w bad
 move.w state.l,d0
 move.w dep.value.l,d0
 move.w $1234.l,d0
 move.w $1234.w,d0
 move.w (state).l,d0
 move.w d0,state.l
 move.b (a0)+,(a3)+
 dbra d1,start
bad
 rts
.endsection
.section bss,kind=bss
state .res word,1
.endsection
.output "output.hunk",format=hunk,sections=code,bss
.endmodule
"#;
const STORAGE: &str =
    ".module storage\n.pub\n.section bss,kind=bss\nvalue .res word,1\n.endsection\n.endmodule\n";
const IMPORT_ROOT: &str = ".module main\n.cpu 68020\n.use dep as imported\n.section code,kind=code\n jsr imported.value\n.endsection\n.output \"bad.hunk\",format=hunk,sections=code\n.endmodule\n";
const IMPORT: &str = ".module dep\n.cpu 68020\n.pub\n.section code,kind=code\nvalue .block\n moveq #256,d0\n rts\n.bend\n.endsection\n.endmodule\n";

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

fn oracle(dir: &Path) -> Vec<u8> {
    let input = dir.join("main.asm");
    let output = dir.join("output.hunk");
    fs::write(&input, SOURCE).unwrap();
    fs::write(dir.join("storage.asm"), STORAGE).unwrap();
    let cli = Cli::parse_from(["opForge".into(), input.to_string_lossy().into_owned()]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    fs::read(output).unwrap()
}

#[test]
fn compact_cli_self_host_instruction_oracle() {
    let dir = create_artifact_dir(&workspace_root(), "compact-cli-diagnostics").unwrap();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    let bytes = oracle(&dir);
    assert!(!bytes.is_empty());
    hunk::allocation(&bytes).unwrap();
    fs::write(dir.join("main.asm"), IMPORT_ROOT).unwrap();
    fs::write(dir.join("dep.asm"), IMPORT).unwrap();
    let cli = Cli::parse_from([
        "opForge".into(),
        dir.join("main.asm").to_string_lossy().into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.clone());
    let Err(cli_core::CliRunError::Assembler { error, .. }) =
        run_with_validated_cli_with_context(&cli, &config)
    else {
        panic!("Rust must reject the imported immediate at assembly time")
    };
    assert!(error.diagnostics().iter().any(|diagnostic| {
        diagnostic.line == 6
            && diagnostic
                .error
                .message()
                .contains("out of signed 8-bit range")
    }));
}

#[test]
#[ignore = "fresh real-native Hunk encoding and imported assembly diagnostic"]
fn compact_cli_assembly_diagnostics_fs_uae() {
    let root = workspace_root();
    let dir = create_artifact_dir(&root, "compact-cli-diagnostics").unwrap();
    let _cleanup = EphemeralArtifactDir(dir.clone());
    let expected = oracle(&dir);
    let image = match std::env::var_os("OPFORGE_COMPACT_DIAGNOSTIC_BOOTSTRAP") {
        Some(path) => fs::read(path).unwrap(),
        None => {
            let build = build_native_packages(
                &engine::build_default_asm_registry(),
                &dir.join("native"),
                &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
                &EmbedSelection::Targets(vec!["68020".into()]),
            )
            .unwrap();
            compact_cli_input::assemble_cli(&root, &build)
        }
    };
    let imported = [OpforgeNativeCliGuestFile {
        relative_path: "dep.asm",
        bytes: IMPORT.as_bytes(),
    }];
    let storage = [OpforgeNativeCliGuestFile {
        relative_path: "storage.asm",
        bytes: STORAGE.as_bytes(),
    }];
    let mut failures = Vec::new();
    for (name, source, command, files, expected_proof, exit) in [
        (
            "self-host-structural-encoding",
            SOURCE,
            "Work:main.asm",
            &storage[..],
            OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/output.hunk",
                rust_oracle: &expected,
            },
            0,
        ),
        (
            "imported-assembly-origin",
            IMPORT_ROOT,
            "Work:main.asm",
            &imported[..],
            OpforgeNativeCliProof::ExpectedFailureContaining("source: Work:dep.asm"),
            20,
        ),
    ] {
        let guest_files: Vec<_> = std::iter::once(OpforgeNativeCliGuestFile {
            relative_path: "main.asm",
            bytes: source.as_bytes(),
        })
        .chain(files.iter().map(|file| OpforgeNativeCliGuestFile {
            relative_path: file.relative_path,
            bytes: file.bytes,
        }))
        .collect();
        let case = OpforgeNativeCliParityCase {
            name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(source.as_bytes()),
            command_template: Some(command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &guest_files,
            proof: expected_proof,
        };
        let outcome = match run_prebuilt_compact_cli_case_from_env(&root, &case, &image) {
            Ok(result) => result,
            Err(error) => {
                failures.push(format!("{name}: {error}"));
                continue;
            }
        };
        let FsUaeSmokeOutcome::Completed { runs } = outcome else {
            failures.push(format!("{name}: fresh native execution required"));
            continue;
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(exit));
        eprintln!(
            "COMPACT_DIAGNOSTIC case={name} exit={:?} seconds={:?} stdout={:?}",
            runs[0].exit_code, runs[0].start_to_done_host_seconds, runs[0].stdout
        );
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
