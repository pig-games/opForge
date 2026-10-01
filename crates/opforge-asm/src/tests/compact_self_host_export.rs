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
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).expect("validate live release self-build");
    config.out_dir = Some(scratch.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("fresh release self-build");
    let oracle = fs::read(scratch.join("build/opforge_compact")).unwrap();
    let allocation = hunk::allocation(&oracle).expect("strict release Hunk allocation");
    let dependency_text = fs::read_to_string(&dependencies).unwrap();
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
    let mut command = format!("opforge p.bin src/{} output.hunk", entry.display());
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
        "runtime_package_magic": "BS11",
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
    fs::write(output.join("opforge"), &oracle).unwrap();
    fs::write(output.join("oracle.hunk"), &oracle).unwrap();
    fs::write(output.join("p.bin"), package).unwrap();
    fs::write(output.join("command.txt"), format!("{command}\n")).unwrap();
    fs::write(
        output.join("manifest.json"),
        serde_json::to_vec_pretty(&manifest).unwrap(),
    )
    .unwrap();
    fs::write(
        output.join("README.txt"),
        "Fresh local release export; no native run has occurred.\nRun command.txt from this directory on AmigaOS. Set an executable protection\nbit on opforge after transfer if needed. Time the whole command externally.\nRequire exit 0 and compare output.hunk exactly with oracle.hunk.\nThe manifest records unchanged source bytes and identity filename mappings.\nCheck over_classic_limit_components: the destination filesystem must support\nthese exact filenames. Include literals have not been rewritten.\nFNV digests identify inputs; exact artifact bytes remain the parity authority.\n",
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
            "over_classic_limit_components": manifest["over_classic_limit_components"],
        })
    );
}
