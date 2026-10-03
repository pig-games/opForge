//! Parent-relative include resolution in selected module files.
use super::*;
use crate::fs_uae_smoke::{
    compact_cli_input::assemble_cli, run_prebuilt_compact_cli_case_from_env,
    OpforgeNativeCliGuestFile, OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase,
    OpforgeNativeCliProof,
};
use crate::native_package_build::{build_native_packages, EmbedSelection};

const PARENT_INCLUDE: &[(&str, &str)] = &[
    (
        "project/main.asm",
        ".module main\n.cpu m6502\n.use dep as d\n.org $1001\n.byte d.value\n.endmodule\n.end\n",
    ),
    (
        "project/library/dep.asm",
        ".module dep\n.cpu m6502\n.org $1000\n.pub\n.include \"./../common/part.inc\"\n.endmodule\n.end\n",
    ),
    ("project/common/part.inc", "value = 7\n.byte value\n"),
];
const NORMALIZED_INCLUDE_CYCLE: &[(&str, &str)] = &[
    PARENT_INCLUDE[0],
    (
        "project/library/dep.asm",
        ".module dep\n.cpu m6502\n.include \"parts/loop.inc\"\n.endmodule\n.end\n",
    ),
    (
        "project/library/parts/loop.inc",
        ".include \"../parts/loop.inc\"\n",
    ),
];
const OUTSIDE_ENTRY_ROOT: &[(&str, &str)] = &[
    PARENT_INCLUDE[0],
    ("project/library/dep.asm", ".module dep\n.cpu m6502\n.org $1000\n.pub\n.include \"../../common/part.inc\"\n.endmodule\n.end\n"),
    ("common/part.inc", PARENT_INCLUDE[2].1),
];

// The native helper stages these at Work:main.asm, with no directory slash.
const VOLUME_SIBLING: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.org $1000\n.include \"part.inc\"\n.byte value\n.endmodule\n.end\n",
    ),
    ("part.inc", "value = 7\n.byte value\n"),
];
const VOLUME_SEARCH_ROOT: &[(&str, &str)] =
    &[VOLUME_SIBLING[0], ("common/part.inc", VOLUME_SIBLING[1].1)];
const VOLUME_CYCLE: &[(&str, &str)] = &[(
    "main.asm",
    ".module main\n.cpu m6502\n.include \"./main.asm\"\n.endmodule\n.end\n",
)];
const VOLUME_ASSET: &[(&str, &str)] = &[
    (
        "main.asm",
        ".module main\n.cpu m6502\n.org $1000\n.incbin \"asset.bin\"\n.endmodule\n.end\n",
    ),
    ("asset.bin", "ABC"),
];

fn asset_oracle() -> Vec<u8> {
    cli_oracle(VOLUME_ASSET, &[], &[]).unwrap()
}

// Use the actual CLI when defaults or file resources are part of the contract.
fn cli_oracle(
    files: &[(&str, &str)],
    modules: &[&str],
    includes: &[&str],
) -> Result<Vec<u8>, String> {
    let dir = create_temp_dir("include-cli-oracle");
    let result = (|| {
        for (name, source) in files {
            let path = dir.join(name);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, source).unwrap();
        }
        let output = dir.join("output.bin");
        let mut args = vec![
            "opForge".into(),
            dir.join(files[0].0).to_string_lossy().into_owned(),
            "--bin".into(),
            output.to_string_lossy().into_owned(),
        ];
        for (flag, roots) in [("-M", modules), ("-I", includes)] {
            for root in roots {
                args.extend([flag.into(), dir.join(root).to_string_lossy().into_owned()]);
            }
        }
        let cli = cli_core::Cli::parse_from(args);
        let config = cli_core::validate_cli(&cli).map_err(|error| error.to_string())?;
        cli_core::run_with_validated_cli_with_context(&cli, &config)
            .map_err(|error| format!("{error:?}"))?;
        fs::read(output).map_err(|error| error.to_string())
    })();
    fs::remove_dir_all(dir).unwrap();
    result
}

#[test]
fn binary_discovery_volume_include_rust_oracles() {
    assert_eq!(oracle_with_roots(VOLUME_SIBLING, &[]).unwrap(), [7, 7]);
    assert_eq!(asset_oracle(), b"ABC");
    assert_eq!(
        oracle_with_search_roots(VOLUME_SEARCH_ROOT, &[], &["common"]).unwrap(),
        [7, 7]
    );
    let cycle = oracle_with_roots(VOLUME_CYCLE, &[]).unwrap_err();
    assert!(
        cycle.contains("cycle") || cycle.contains("recursive"),
        "{cycle}"
    );
}

#[test]
fn binary_discovery_include_cli_defaults_rust_oracles() {
    assert_eq!(
        cli_oracle(PARENT_INCLUDE, &["project/library"], &[]).unwrap(),
        [7, 7]
    );
    let error = cli_oracle(OUTSIDE_ENTRY_ROOT, &["project/library"], &[]).unwrap_err();
    assert!(error.contains("INCLUDE file not found"), "{error}");
}

#[test]
#[ignore = "requires configured FS-UAE; sibling include from a volume-root entry"]
fn compact_cli_volume_sibling_include_fs_uae() {
    let expected = oracle_with_roots(VOLUME_SIBLING, &[]).unwrap();
    volume_cli(VOLUME_SIBLING, &[], Some(&expected));
}

#[test]
#[ignore = "requires configured FS-UAE; root search after a volume-root sibling miss"]
fn compact_cli_volume_search_include_fs_uae() {
    let expected = oracle_with_search_roots(VOLUME_SEARCH_ROOT, &[], &["common"]).unwrap();
    volume_cli(VOLUME_SEARCH_ROOT, &["common"], Some(&expected));
}

#[test]
#[ignore = "requires configured FS-UAE; normalized volume-root include cycle rejection"]
fn compact_cli_volume_cycle_include_fs_uae() {
    volume_cli(VOLUME_CYCLE, &[], None);
}

#[test]
#[ignore = "requires configured FS-UAE; parent components cannot ascend above a volume"]
fn compact_cli_volume_parent_escape_fs_uae() {
    volume_cli(
        &[
            (
                "main.asm",
                ".module main\n.cpu m6502\n.include \"../part.inc\"\n.endmodule\n.end\n",
            ),
            VOLUME_SIBLING[1],
        ],
        &[],
        None,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; binary assets share volume-root parent resolution"]
fn compact_cli_volume_asset_include_fs_uae() {
    let expected = asset_oracle();
    volume_cli(VOLUME_ASSET, &[], Some(&expected));
}

// The general source-set helper uses Work:sources/. Exercise the actual volume
// root explicitly; a slash-containing path would miss this regression.
fn volume_cli(files: &[(&str, &str)], roots: &[&str], expected: Option<&[u8]>) {
    let root = workspace_root();
    let dir = create_temp_dir("volume-include-cli");
    let build = build_native_packages(
        &engine::build_default_asm_registry(),
        &dir.join("build"),
        &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::ExternalOnly,
    )
    .unwrap();
    let image = assemble_cli(&root, &build);
    let package = fs::read(build.output_dir.join("packages/m6502--transparent.bin")).unwrap();
    let mut guest: Vec<_> = files
        .iter()
        .map(|(path, source)| OpforgeNativeCliGuestFile {
            relative_path: path,
            bytes: source.as_bytes(),
        })
        .collect();
    guest.push(OpforgeNativeCliGuestFile {
        relative_path: "package.bin",
        bytes: &package,
    });
    let mut command =
        "--runtime-package Work:package.bin -i Work:main.asm --bin Work:output.bin".to_string();
    for include_root in roots {
        command.push_str(&format!(" -I Work:{include_root}"));
    }
    let case = OpforgeNativeCliParityCase {
        name: "volume-root-include",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(&package),
        command_template: Some(&command),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &guest,
        proof: match expected {
            Some(rust_oracle) => OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/output.bin",
                rust_oracle,
            },
            None => OpforgeNativeCliProof::ExpectedFailureContaining(
                "binary source: unsupported or invalid input",
            ),
        },
    };
    let result = run_prebuilt_compact_cli_case_from_env(&root, &case, &image);
    fs::remove_dir_all(dir).unwrap();
    let FsUaeSmokeOutcome::Completed { runs } = result.expect("fresh volume-root include proof")
    else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].success, expected.is_some());
    assert_eq!(
        runs[0].exit_code,
        Some(if expected.is_some() { 0 } else { 20 })
    );
    eprintln!(
        "COMPACT_VOLUME_INCLUDE seconds={:?} image_bytes={}",
        runs[0].start_to_done_host_seconds,
        image.len()
    );
}

#[test]
fn binary_discovery_parent_include_rust_oracle() {
    assert_eq!(
        oracle_with_search_roots(PARENT_INCLUDE, &["project/library"], &["project"]).unwrap(),
        [7, 7]
    );
}

#[test]
fn binary_discovery_parent_include_requires_allowed_root_rust_oracle() {
    assert!(oracle_with_roots(PARENT_INCLUDE, &["project/library"])
        .unwrap_err()
        .contains("INCLUDE file not found"));
}

#[test]
#[ignore = "requires configured FS-UAE; normalized parent-relative include"]
fn compact_cli_parent_include_fs_uae() {
    let expected =
        oracle_with_search_roots(PARENT_INCLUDE, &["project/library"], &["project"]).unwrap();
    compact_cli(
        PARENT_INCLUDE,
        &["project/library"],
        &["project"],
        Some(&expected),
        false,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; CLI default include root permits an entry-tree sibling"]
fn compact_cli_parent_include_default_root_fs_uae() {
    let expected = cli_oracle(PARENT_INCLUDE, &["project/library"], &[]).unwrap();
    compact_cli(
        PARENT_INCLUDE,
        &["project/library"],
        &[],
        Some(&expected),
        false,
    );
}

#[test]
#[ignore = "requires configured FS-UAE; parent include outside all allowed roots rejects"]
fn compact_cli_parent_include_outside_roots_fs_uae() {
    compact_cli(OUTSIDE_ENTRY_ROOT, &["project/library"], &[], None, false);
}

#[test]
fn binary_discovery_parent_include_cycle_rust_oracle() {
    let error = oracle_with_search_roots(
        NORMALIZED_INCLUDE_CYCLE,
        &["project/library"],
        &["project/library"],
    )
    .unwrap_err();
    assert!(
        error.contains("cycle") || error.contains("recursive"),
        "{error}"
    );
}

#[test]
#[ignore = "requires configured FS-UAE; normalized include cycle rejection"]
fn compact_cli_parent_include_cycle_fs_uae() {
    compact_cli(
        NORMALIZED_INCLUDE_CYCLE,
        &["project/library"],
        &["project/library"],
        None,
        false,
    );
}
