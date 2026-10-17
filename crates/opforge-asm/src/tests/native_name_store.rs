//! Standalone native paged-name storage component proof.
use super::native_iterable_loops::scratch;
use super::*;

fn copy_tree(from: &Path, to: &Path) {
    fs::create_dir_all(to).unwrap();
    for entry in fs::read_dir(from).unwrap() {
        let entry = entry.unwrap();
        let target = to.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_tree(&entry.path(), &target);
        } else {
            fs::copy(entry.path(), target).unwrap();
        }
    }
}

fn name_store_image(dir: &Path) -> (Vec<u8>, String) {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let source =
        fs::read_to_string(root.join(
            "native/motorola68000/amigaos/test-harnesses/experimental/name_store_harness.asm",
        ))
        .unwrap();
    let entry = dir.join("entry.asm");
    fs::write(&entry, &source).unwrap();
    let mut source_root = std::env::var_os("OPFORGE_NAME_STORE_SOURCE_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|| root.join("native/motorola68000"));
    let mut defines = Vec::new();
    if let Some(mode) = std::env::var_os("OPFORGE_NAME_STORE_FAULT") {
        let (call, define) = match mode.to_str().unwrap() {
            "first" => ("reserveBounded", "OPFORGE_NAME_STORE_EXPECT_FIRST_FAILURE"),
            "page" => ("reserveExact", "OPFORGE_NAME_STORE_EXPECT_PAGE_FAILURE"),
            other => panic!("unknown name-store fault: {other}"),
        };
        let fixture = dir.join("fault-native");
        copy_tree(&source_root, &fixture);
        let module = fixture.join("amigaos/experimental/binary_name_store.asm");
        let text = fs::read_to_string(&module).unwrap();
        let needle = format!("\tjsr memory.{call}\n");
        assert_eq!(text.matches(&needle).count(), 1, "one fault call required");
        fs::write(&module, text.replacen(&needle, "\tmoveq #1, d0\n", 1)).unwrap();
        source_root = fixture;
        defines.extend(["-D".to_owned(), define.to_owned()]);
    }
    let mut args = vec![
        "opForge".into(),
        entry.to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        source_root.to_string_lossy().into_owned(),
        "-I".into(),
        source_root
            .join("amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ];
    args.extend(defines);
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.into());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "name store harness: {error}; diagnostics: {:?}",
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => {
            panic!("name store harness: {error}")
        }
        _ => panic!("name store harness warnings"),
    });
    (fs::read(dir.join("name-store-probe.hunk")).unwrap(), source)
}

#[test]
fn native_name_store_host_assembles() {
    let dir = scratch();
    let image = name_store_image(&dir.0).0;
    assert!(!image.is_empty());
    if let Some(path) = std::env::var_os("OPFORGE_NAME_STORE_SAVE_IMAGE") {
        fs::write(path, &image).unwrap();
    }
}

#[test]
#[ignore = "requires configured FS-UAE; actual paged-name storage and cleanup"]
fn native_name_store_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let (image, source) = if let Some(path) = std::env::var_os("OPFORGE_NAME_STORE_IMAGE") {
        (
            fs::read(path).unwrap(),
            fs::read_to_string(root.join(
                "native/motorola68000/amigaos/test-harnesses/experimental/name_store_harness.asm",
            ))
            .unwrap(),
        )
    } else {
        name_store_image(&dir.0)
    };
    let expected = [0x5a];
    let case = OpforgeNativeCliParityCase {
        name: "name-store-pages",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/name-store-probe.bin",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .unwrap_or_else(|error| panic!("fresh name store proof: {error}"));
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
}
