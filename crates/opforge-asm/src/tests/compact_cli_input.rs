// SPDX-License-Identifier: GPL-3.0-or-later
//! CLI-input checkpoint: live Rust file trees and fresh compact Shell proofs.
//! Output qualification and source-selected initial CPUs are separate checkpoints.
use super::*;
use crate::native_package_build::{build_native_packages, EmbedSelection, NativePackageBuild};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

type Tree = &'static [(&'static str, &'static [u8])];

const SEARCH_TREE: Tree = &[
    ("project space/main.asm", b".module main\n.cpu 68020\n.use values as v\n.org $1000\n.include \"nested/outer.inc\"\n.byte v.value\n.endmodule\n.end\n"),
    ("project space/values.asm", b".module values\n.cpu 68020\n.pub\nvalue = 7\n.endmodule\n"),
    ("project space/nested/outer.inc", b".include \"own.inc\"\n.include \"root.inc\"\n.include \"explicit.inc\"\n"),
    ("project space/nested/own.inc", b".byte $11\n"),
    ("project space/own.inc", b".byte $ee\n"),
    ("project space/root.inc", b".byte $22\n"),
    ("includes space/own.inc", b".byte $ef\n"),
    ("includes space/root.inc", b".byte $fe\n"),
    ("includes space/explicit.inc", b".byte $33\n"),
];
const MODULE_TREE: Tree = &[
    ("entry.asm", b".module main\n.cpu 68020\n.use remote as r\n.org $1000\n moveq #r.value,d0\n rts\n.endmodule\n.end\n"),
    ("modules/remote.asm", b".module remote\n.cpu 68020\n.pub\nvalue = 42\n.endmodule\n"),
];
const HUNK_TREE: Tree = &[(
    "hunk.asm",
    b".module main\n.cpu 68020\n.section code, kind=code\n moveq #42,d0\n rts\n.endsection\n.output \"source.hunk\", format=hunk, sections=code\n.endmodule\n",
)];
const CURRENT_TREE: Tree = &[(
    "main.asm",
    b".module main\n.cpu 68020\n.byte $45,$67\n.endmodule\n",
)];

struct InputCase {
    name: &'static str,
    tree: Tree,
    entry: &'static str,
    include_roots: &'static [&'static str],
    module_roots: &'static [&'static str],
    command: &'static str,
    hunk: bool,
    binary_bytes: Option<&'static [u8]>,
    expected_path: &'static str,
    omitted_filename: bool,
    output_name: Option<&'static str>,
}

const INPUT_CASES: &[InputCase] = &[
    InputCase {
        name: "dot-current-directory",
        tree: CURRENT_TREE,
        entry: ".",
        include_roots: &[],
        module_roots: &[],
        command: "--cpu 68020 . --bin Work:output.bin",
        hunk: false,
        binary_bytes: Some(&[0x45, 0x67]),
        expected_path: "Work/output.bin",
        omitted_filename: false,
        output_name: None,
    },
    InputCase {
        name: "omitted-current-directory",
        tree: CURRENT_TREE,
        entry: ".",
        include_roots: &[],
        module_roots: &[],
        command: "--cpu 68020 --bin Work:output.bin",
        hunk: false,
        binary_bytes: Some(&[0x45, 0x67]),
        expected_path: "Work/output.bin",
        omitted_filename: false,
        output_name: None,
    },
    InputCase {
        name: "directory-main-and-root-defaults",
        tree: SEARCH_TREE,
        entry: "project space",
        include_roots: &["includes space"],
        module_roots: &[],
        command: "--cpu 68020 \"Work:project space\" --bin Work:output.bin -I \"Work:includes space\"",
        hunk: false,
        binary_bytes: Some(&[0x11, 0x22, 0x33, 7]),
        expected_path: "Work/output.bin",
        omitted_filename: false,
        output_name: None,
    },
    InputCase {
        name: "quoted-file-attached-input-and-long-equals",
        tree: SEARCH_TREE,
        entry: "project space/main.asm",
        include_roots: &["includes space"],
        module_roots: &[],
        command: "--cpu=68020 -i\"Work:project space/main.asm\" --include-path=\"Work:includes space\" --bin=Work:output.bin",
        hunk: false,
        binary_bytes: Some(&[0x11, 0x22, 0x33, 7]),
        expected_path: "Work/output.bin",
        omitted_filename: false,
        output_name: None,
    },
    InputCase {
        name: "separator-positional-and-attached-search-output",
        tree: MODULE_TREE,
        entry: "entry.asm",
        include_roots: &[],
        module_roots: &["modules"],
        command: "--cpu=68020 -MWork:modules -bWork:output.bin -- Work:entry.asm",
        hunk: false,
        binary_bytes: Some(&[0x70, 0x2a, 0x4e, 0x75]),
        expected_path: "Work/output.bin",
        omitted_filename: false,
        output_name: None,
    },
    InputCase {
        name: "attached-equals-and-hidden-output",
        tree: MODULE_TREE,
        entry: "entry.asm",
        include_roots: &[],
        module_roots: &["modules"],
        command: "--cpu=68020 -i=Work:entry.asm -M=Work:modules -b=Work:.image",
        hunk: false,
        binary_bytes: Some(&[0x70, 0x2a, 0x4e, 0x75]),
        expected_path: "Work/.image.bin",
        omitted_filename: false,
        output_name: Some(".image"),
    },
    InputCase {
        name: "runtime-package-source-configured-hunk",
        tree: HUNK_TREE,
        entry: "hunk.asm",
        include_roots: &[],
        module_roots: &[],
        command: "--runtime-package=Work:runtime.bin --infile=Work:hunk.asm --hunk=Work:output.hunk",
        hunk: true,
        binary_bytes: None,
        expected_path: "Work/output.hunk",
        omitted_filename: false,
        output_name: None,
    },
    InputCase {
        name: "file-binary-filename-default",
        tree: MODULE_TREE,
        entry: "entry.asm",
        include_roots: &[],
        module_roots: &["modules"],
        command: "--cpu 68020 Work:entry.asm -MWork:modules --bin",
        hunk: false,
        binary_bytes: Some(&[0x70, 0x2a, 0x4e, 0x75]),
        expected_path: "Work/entry.bin",
        omitted_filename: true,
        output_name: None,
    },
    InputCase {
        name: "directory-binary-filename-default",
        tree: SEARCH_TREE,
        entry: "project space",
        include_roots: &["includes space"],
        module_roots: &[],
        command: "--cpu 68020 \"Work:project space\" -I\"Work:includes space\" --bin",
        hunk: false,
        binary_bytes: Some(&[0x11, 0x22, 0x33, 7]),
        expected_path: "Work/project space.bin",
        omitted_filename: true,
        output_name: None,
    },
];

fn scratch() -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "opforge-compact-cli-input-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir(&dir).unwrap();
    dir
}

fn rust_oracle(base: &Path, case: &InputCase) -> Vec<u8> {
    let dir = base.join(case.name);
    for (relative_path, bytes) in case.tree {
        let path = dir.join(relative_path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, bytes).unwrap();
    }
    let output = dir.join(if case.omitted_filename || case.output_name.is_some() {
        case.expected_path.strip_prefix("Work/").unwrap()
    } else if case.hunk {
        "source.hunk"
    } else {
        "oracle.bin"
    });
    let mut argv = vec![
        "opForge".to_string(),
        dir.join(case.entry).to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
    ];
    // The native --hunk selects the source-configured artifact. Rust's --hunk
    // also synthesizes a separate artifact, so use the actual source declaration
    // as this Hunk oracle, as the existing native Hunk comparisons do.
    if !case.hunk {
        argv.push("--bin".into());
        if !case.omitted_filename {
            argv.push(
                case.output_name
                    .map(|name| dir.join(name))
                    .unwrap_or_else(|| output.clone())
                    .to_string_lossy()
                    .into_owned(),
            );
        }
    }
    for (flag, roots) in [("-M", case.module_roots), ("-I", case.include_roots)] {
        for root in roots {
            argv.extend([flag.into(), dir.join(root).to_string_lossy().into_owned()]);
        }
    }
    let cli = Cli::parse_from(argv);
    let mut config = validate_cli(&cli).expect("validate fresh Rust CLI oracle");
    config.out_dir = Some(dir);
    run_with_validated_cli_with_context(&cli, &config).expect("assemble actual Rust file tree");
    let oracle = fs::read(output).expect("read fresh Rust artifact");
    assert!(!oracle.is_empty());
    if let Some(expected) = case.binary_bytes {
        assert_eq!(
            oracle, expected,
            "{}: meaningful live Rust oracle",
            case.name
        );
    } else {
        assert_eq!(hunk::allocation(&oracle).unwrap().segments, 1);
    }
    oracle
}

pub(crate) fn assemble_cli(root: &Path, build: &NativePackageBuild) -> Vec<u8> {
    assemble_cli_with_defines(root, build, &[])
}

pub(super) fn assemble_cli_with_defines(
    root: &Path,
    build: &NativePackageBuild,
    defines: &[&str],
) -> Vec<u8> {
    let mut argv = vec![
        "opForge".to_string(),
        build.cli_source_path.to_string_lossy().into_owned(),
    ];
    let mut roots: Vec<_> = fs::read_dir(root.join("native"))
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.is_dir())
        .collect();
    roots.sort();
    for path in roots {
        argv.extend(["-M".into(), path.to_string_lossy().into_owned()]);
    }
    argv.extend([
        "-I".into(),
        root.join("native/motorola68000/amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ]);
    for define in defines {
        argv.extend(["--define".into(), (*define).into()]);
    }
    let cli = Cli::parse_from(argv);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(build.output_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => {
            panic!("assemble current compact CLI: {:?}", error.diagnostics())
        }
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("assemble current compact CLI: {error}")
        }
        cli_core::CliRunError::WarningsAsErrors { .. } => {
            panic!("assemble current compact CLI: warnings treated as errors")
        }
    });
    fs::read(build.output_dir.join("build/opforge_compact")).unwrap()
}

#[test]
fn compact_cli_input_live_rust_oracles() {
    let scratch = scratch();
    let _cleanup = EphemeralArtifactDir(scratch.clone());
    for case in INPUT_CASES {
        rust_oracle(&scratch, case);
    }
}

#[test]
#[ignore = "fresh real-native CLI input proof; requires configured FS-UAE"]
fn compact_cli_input_defaults_and_argument_integration() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let scratch = scratch();
    let _cleanup = EphemeralArtifactDir(scratch.clone());
    let registry = engine::build_default_asm_registry();
    let build = build_native_packages(
        &registry,
        &scratch.join("native"),
        &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::Targets(vec!["68020".into()]),
    )
    .unwrap();
    let image = assemble_cli(&root, &build);
    let package_name = crate::native_package_build::resolve_embeds(
        &registry,
        &EmbedSelection::Targets(vec!["68020".into()]),
    )
    .unwrap()
    .into_iter()
    .next()
    .unwrap();
    let package = fs::read(build.output_dir.join("packages").join(package_name)).unwrap();
    let selected = std::env::var("OPFORGE_CLI_INPUT_CASES").ok();
    let wanted = |name: &str| {
        selected
            .as_ref()
            .is_none_or(|names| names.split(',').any(|item| item == name))
    };
    let mut attempted = 0;
    let mut failures = Vec::new();
    let mut run = |name: &str,
                   source: &[u8],
                   command: &str,
                   files: &[OpforgeNativeCliGuestFile<'_>],
                   proof: OpforgeNativeCliProof<'_>,
                   exit: i32| {
        if !wanted(name) {
            return;
        }
        attempted += 1;
        let case = OpforgeNativeCliParityCase {
            name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(source),
            command_template: Some(command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: files,
            proof: proof,
        };
        match run_prebuilt_compact_cli_case_from_env(&root, &case, &image) {
            Ok(FsUaeSmokeOutcome::Completed { runs })
                if runs.len() == 1
                    && runs[0].protocol_completed
                    && runs[0].exit_code == Some(exit) =>
            {
                eprintln!("{name}: fresh native completion, exit {exit}");
            }
            Ok(FsUaeSmokeOutcome::Completed { runs }) => failures.push(format!(
                "{name}: invalid completion/exit; {} runs",
                runs.len()
            )),
            Ok(FsUaeSmokeOutcome::Skipped(reason)) => {
                failures.push(format!("{name}: skipped: {reason}"))
            }
            Err(error) => failures.push(format!("{name}: {error}")),
        }
    };
    for case in INPUT_CASES {
        let oracle = rust_oracle(&scratch, case);
        let mut files: Vec<_> = case
            .tree
            .iter()
            .map(|(relative_path, bytes)| OpforgeNativeCliGuestFile {
                relative_path,
                bytes,
            })
            .collect();
        files.push(OpforgeNativeCliGuestFile {
            relative_path: "runtime.bin",
            bytes: &package,
        });
        run(
            case.name,
            case.tree[0].1,
            case.command,
            &files,
            OpforgeNativeCliProof::ExactArtifact {
                relative_path: case.expected_path,
                rust_oracle: &oracle,
            },
            0,
        );
    }
    for (name, command, text) in [
        (
            "help-success",
            "--help",
            "Usage: opforge_compact [OPTIONS] FILE|DIRECTORY",
        ),
        ("version-success", "-V", "opForge compact native | BS17"),
    ] {
        run(
            name,
            b"",
            command,
            &[],
            OpforgeNativeCliProof::SuccessfulExitContaining(text),
            0,
        );
    }
    let files = [OpforgeNativeCliGuestFile {
        relative_path: "entry.asm",
        bytes: MODULE_TREE[0].1,
    }];
    for (name, command) in [
        (
            "missing-option-value",
            "Work:entry.asm --bin Work:output.bin --cpu",
        ),
        (
            "multiple-positional-inputs",
            "--cpu 68020 --bin Work:output.bin Work:entry.asm Work:second.asm",
        ),
        (
            "mixed-input-styles",
            "--cpu 68020 -i Work:entry.asm --bin Work:output.bin Work:second.asm",
        ),
        ("malformed-help-value", "--help=unexpected"),
        (
            "conflicting-package-selection",
            "--runtime-package Work:runtime.bin --cpu 68020 Work:entry.asm --bin Work:output.bin",
        ),
        (
            "unsupported-binary-range",
            "--cpu 68020 Work:entry.asm --bin Work:output.bin:1000:1001",
        ),
    ] {
        run(
            name,
            MODULE_TREE[0].1,
            command,
            &files,
            OpforgeNativeCliProof::ExpectedFailureContaining(
                "compact CLI: invalid or unsupported arguments",
            ),
            20,
        );
    }
    let current_files: Vec<_> = CURRENT_TREE
        .iter()
        .map(|(relative_path, bytes)| OpforgeNativeCliGuestFile {
            relative_path,
            bytes,
        })
        .collect();
    run(
        "missing-initial-target",
        b"",
        "",
        &current_files,
        OpforgeNativeCliProof::ExpectedFailureContaining(
            "compact CLI: initial target requires --cpu or --runtime-package",
        ),
        20,
    );
    let invalid_files = [OpforgeNativeCliGuestFile {
        relative_path: "input.txt",
        bytes: CURRENT_TREE[0].1,
    }];
    run(
        "unsupported-source-extension",
        CURRENT_TREE[0].1,
        "--cpu 68020 Work:input.txt --bin Work:output.bin",
        &invalid_files,
        OpforgeNativeCliProof::ExpectedFailureContaining("compact CLI: invalid input"),
        20,
    );
    assert!(attempted > 0, "CLI input selector must match a real case");
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
