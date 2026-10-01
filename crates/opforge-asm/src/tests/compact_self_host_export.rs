//! Local, host-only export for a physical Amiga compact self-host run.
//! OPFORGE_COMPACT_EXPORT_DIR=/absolute/new/bundle cargo test -p asm
//! export_compact_self_host_bundle -- --ignored --nocapture
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use vm::runtime_model_core::RuntimeModelCore;

const ENTRY: &str = "experimental/opforge_compact_cli.asm";
const MODULE_ROOTS: &[&str] = &[
    "experimental",
    "opforge-cli",
    "tkpkg",
    "tkvm",
    "prvm",
    "exprvm",
    "opcore",
    "opasm",
];
const INSTRUMENTED_DEFINES: &[&str] = &[
    "OPFORGE_DEBUG_CONTRACTS",
    "OPFORGE_MEMORY_TELEMETRY",
    "OPFORGE_TOKEN_DETAIL_TELEMETRY",
    "OPFORGE_PREPARATION_PROGRESS",
    "OPFORGE_BINDING_DETAIL_TELEMETRY",
    "OPFORGE_TEMPLATE_WORK_TELEMETRY",
    "OPFORGE_INPUT_TELEMETRY",
    "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
];

#[test]
#[ignore = "host-only export; requires a new OPFORGE_COMPACT_EXPORT_DIR"]
fn export_compact_self_host_bundle() {
    let output = PathBuf::from(
        std::env::var_os("OPFORGE_COMPACT_EXPORT_DIR")
            .expect("set OPFORGE_COMPACT_EXPORT_DIR to a new local directory"),
    );
    assert!(output.is_absolute(), "export directory must be absolute");
    assert!(!output.exists(), "export directory must not already exist");
    let workspace = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    let source_root = fs::canonicalize(
        std::env::var_os("OPFORGE_SELF_HOST_SOURCE_ROOT")
            .map(PathBuf::from)
            .unwrap_or_else(|| workspace.join("native/motorola68000/amigaos")),
    )
    .expect("canonical source root");
    let scratch = create_artifact_dir(&workspace, "compact-self-host-export").unwrap();
    let cleanup = EphemeralArtifactDir(scratch.clone());
    let dependencies = scratch.join("dependencies.d");
    let mut args = vec![
        "opForge".to_string(),
        source_root.join(ENTRY).to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "68020".to_string(),
        "--dependencies".to_string(),
        dependencies.to_string_lossy().into_owned(),
    ];
    for root in MODULE_ROOTS.iter().copied().chain(["debug"]) {
        args.extend([
            "-M".to_string(),
            source_root.join(root).to_string_lossy().into_owned(),
        ]);
    }
    args.extend([
        "-I".to_string(),
        source_root.join("debug").to_string_lossy().into_owned(),
    ]);
    let cli = Cli::parse_from(args.clone());
    let mut config = validate_cli(&cli).expect("validate live release self-build");
    assert!(
        config.defines.is_empty(),
        "release oracle requires no defines"
    );
    config.out_dir = Some(scratch.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("fresh release self-build");
    let oracle = fs::read(scratch.join("build/opforge_compact")).unwrap();
    let allocation = hunk::allocation(&oracle).expect("strict release Hunk allocation");
    let dependency_text = fs::read_to_string(&dependencies).unwrap();
    let instrumented = std::env::var("OPFORGE_COMPACT_EXPORT_INSTRUMENTED").as_deref() == Ok("1");
    // This case assembles the unchanged external-only source with an embedded
    // bootstrap. It does not exercise native .incbin self-assembly yet.
    let embedded = match std::env::var("OPFORGE_COMPACT_EXPORT_EMBED") {
        Err(std::env::VarError::NotPresent) => false,
        Ok(value) if value == "68020" => true,
        _ => panic!("OPFORGE_COMPACT_EXPORT_EMBED currently accepts only 68020"),
    };
    let bootstrap_defines = if instrumented {
        INSTRUMENTED_DEFINES
    } else {
        &[]
    };
    let embedded_files = if embedded {
        use crate::native_package_build::{build_native_packages, EmbedSelection};
        let registry = engine::build_default_asm_registry();
        let build = build_native_packages(
            &registry,
            &scratch.join("embedded"),
            &source_root.join(ENTRY),
            &EmbedSelection::Targets(vec!["68020".into()]),
        )
        .expect("generate single-CPU embedded catalog");
        assert_eq!(build.embedded_files.len(), 1);
        args[1] = build.cli_source_path.to_string_lossy().into_owned();
        build.embedded_files.into_iter().collect::<Vec<_>>()
    } else {
        Vec::new()
    };
    let bootstrap = if instrumented || embedded {
        let bootstrap_cli = Cli::parse_from(args);
        config = validate_cli(&bootstrap_cli).expect("validate configured bootstrap");
        config.out_dir = Some(scratch.clone());
        config.defines = bootstrap_defines
            .iter()
            .map(|name| (*name).to_owned())
            .collect();
        run_with_validated_cli_with_context(&bootstrap_cli, &config)
            .expect("fresh configured bootstrap build");
        fs::read(scratch.join("build/opforge_compact")).unwrap()
    } else {
        oracle.clone()
    };
    let bootstrap_allocation =
        hunk::allocation(&bootstrap).expect("strict bootstrap Hunk allocation");
    let (_, prerequisites) = dependency_text
        .split_once(": ")
        .expect("Makefile dependency prerequisites");
    let mut sources =
        prerequisites
            .split_whitespace()
            .map(|name| {
                let path = PathBuf::from(name);
                let logical = path
                    .strip_prefix(&source_root)
                    .expect("dependency stays in native source root")
                    .to_path_buf();
                assert!(logical
                    .components()
                    .all(|part| { matches!(part, std::path::Component::Normal(_)) }));
                assert!(logical.to_str().unwrap().bytes().all(|byte| {
                    byte > 32 && byte < 127 && !matches!(byte, b':' | b'\\' | b'"')
                }));
                // Preserve include literals and the exact successful compact input tree.
                let staged = logical.clone();
                (logical, staged, fs::read(path).unwrap())
            })
            .collect::<Vec<_>>();
    sources.sort_by(|left, right| left.0.cmp(&right.0));
    assert!(sources
        .iter()
        .any(|(logical, _, _)| logical == Path::new(ENTRY)));
    let mut staged_names = BTreeSet::new();
    let mut manifest_bytes = Vec::new();
    for (logical, staged, bytes) in &sources {
        assert!(staged_names.insert(staged.to_string_lossy().to_ascii_lowercase()));
        manifest_bytes.extend_from_slice(logical.to_str().unwrap().as_bytes());
        manifest_bytes.push(0);
        manifest_bytes.extend_from_slice(bytes);
        manifest_bytes.push(0);
    }
    // Match the registry used by the compact self-host parity test.
    let mut registry = ModuleRegistry::new();
    families::register_intel8080_family_stack(&mut registry);
    families::register_mos6502_family_stack(&mut registry);
    families::register_motorola6800_family_stack(&mut registry);
    families::register_motorola68000_family_stack(&mut registry);
    let core = RuntimeModelCore::from_registry(&registry).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = crate::binary_source_experiment::prepare_package(&core, &resolved).unwrap();
    if embedded {
        assert_eq!(
            fs::read(scratch.join("embedded/packages").join(&embedded_files[0])).unwrap(),
            package,
            "embedded payload must match the self-host package"
        );
        assert_eq!(
            bootstrap
                .windows(package.len())
                .filter(|w| *w == package)
                .count(),
            1
        );
    }
    let module_roots = MODULE_ROOTS
        .iter()
        .copied()
        .filter(|root| {
            sources
                .iter()
                .any(|(logical, _, _)| logical.starts_with(root))
        })
        .collect::<Vec<_>>();
    let entry = Path::new(ENTRY);
    let selection = if embedded { "--cpu 68020" } else { "p.bin" };
    let package_file = embedded_files.first().map_or("p.bin", String::as_str);
    let mut command = format!("opforge {selection} src/{} output.hunk", entry.display());
    for root in &module_roots {
        command.push_str(&format!(" -M src/{root}"));
    }
    command.push_str(" -I src/debug");
    assert!(
        command.len() <= 255,
        "classic Shell command exceeds 255 bytes"
    );
    let files = sources
        .iter()
        .map(|(logical, staged, bytes)| {
            json!({
                "logical_path": logical,
                "staged_path": Path::new("src").join(staged),
                "bytes": bytes.len(),
                "digest": opforge_self_host_package_digest(bytes),
            })
        })
        .collect::<Vec<_>>();
    let long_components = sources
        .iter()
        .flat_map(|(_, staged, _)| staged.components())
        .map(|part| part.as_os_str().to_str().unwrap())
        .filter(|name| name.len() > AMIGAOS_CLASSIC_FILENAME_COMPONENT_MAX)
        .collect::<BTreeSet<_>>();
    assert!(
        long_components.is_empty(),
        "physical Amiga export requires filenames of at most {} bytes: {:?}",
        AMIGAOS_CLASSIC_FILENAME_COMPONENT_MAX,
        long_components
    );
    let manifest = json!({
        "manifest_version": 1,
        "kind": "compact-self-host-local-export",
        "source_root": source_root,
        "source_manifest_digest": opforge_self_host_package_digest(&manifest_bytes),
        "source_files": sources.len(),
        "source_bytes": sources.iter().map(|(_, _, bytes)| bytes.len()).sum::<usize>(),
        "source_mapping": files,
        "filename_mapping": "identity; source include literals are unchanged",
        "classic_filename_component_limit": AMIGAOS_CLASSIC_FILENAME_COMPONENT_MAX,
        "classic_filename_compatible": long_components.is_empty(),
        "over_classic_limit_components": long_components,
        "release_defines": [],
        "release_hunk_bytes": oracle.len(),
        "release_hunk_digest": opforge_self_host_package_digest(&oracle),
        "release_hunk_allocation_bytes": allocation.total(),
        "bootstrap_defines": bootstrap_defines,
        "bootstrap_package_storage": if embedded { "embedded" } else { "external" },
        "embedded_packages": embedded_files,
        "output_package_storage": "external",
        "bootstrap_hunk_bytes": bootstrap.len(),
        "bootstrap_hunk_digest": opforge_self_host_package_digest(&bootstrap),
        "bootstrap_hunk_allocation_bytes": bootstrap_allocation.total(),
        "telemetry_file": instrumented.then_some("memory.bin"),
        "runtime_package_magic": "BS12",
        "runtime_package_file": package_file,
        "runtime_package_bytes": package.len(),
        "runtime_package_digest": opforge_self_host_package_digest(&package),
        "entry": format!("src/{}", entry.display()),
        "module_roots": module_roots.iter().map(|root| format!("src/{root}")).collect::<Vec<_>>(),
        "include_roots": ["src/debug"],
        "command": command,
        "native_validation": "not_run",
    });
    fs::create_dir(&output).expect("create new export directory");
    for (_, staged, bytes) in &sources {
        let path = output.join("src").join(staged);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, bytes).unwrap();
    }
    fs::write(output.join("opforge"), &bootstrap).unwrap();
    fs::write(output.join("oracle.hunk"), &oracle).unwrap();
    fs::write(output.join(package_file), package).unwrap();
    fs::write(output.join("command.txt"), format!("{command}\n")).unwrap();
    fs::write(
        output.join("manifest.json"),
        serde_json::to_vec_pretty(&manifest).unwrap(),
    )
    .unwrap();
    fs::write(
        output.join("README.txt"),
        "Fresh local self-host export; no native run has occurred.\nRun command.txt from this directory on AmigaOS. Set an executable protection\nbit on opforge after transfer if needed. Time the whole command externally.\nRequire exit 0 and compare output.hunk exactly with the release oracle.hunk.\nThe manifest records bootstrap instrumentation and package storage. An embedded\nbootstrap uses --cpu 68020; its named package is retained locally for identity\nverification and is not transferred by the hardware runner. An instrumented\nbootstrap writes memory.bin in its current directory. Output is the default\nexternal-package release executable, not a self-assembled embedded configuration.\nSource bytes and include filenames are unchanged. FNV digests identify inputs;\nexact artifact bytes remain the parity authority.\n",
    )
    .unwrap();
    drop(cleanup);
    assert!(!scratch.exists(), "host build scratch was removed");
    eprintln!(
        "COMPACT_SELF_HOST_EXPORT {}",
        json!({
            "directory": output,
            "source_files": manifest["source_files"],
            "source_bytes": manifest["source_bytes"],
            "source_manifest_digest": manifest["source_manifest_digest"],
            "release_hunk_bytes": oracle.len(),
            "bootstrap_hunk_bytes": bootstrap.len(),
            "bootstrap_defines": bootstrap_defines,
            "bootstrap_package_storage": manifest["bootstrap_package_storage"],
            "embedded_packages": manifest["embedded_packages"],
            "over_classic_limit_components": manifest["over_classic_limit_components"],
        })
    );
}
