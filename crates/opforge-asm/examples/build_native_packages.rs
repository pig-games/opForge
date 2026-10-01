// SPDX-License-Identifier: GPL-3.0-or-later
//! Host-only package/catalog builder; run with --help for options.
use asm::native_package_build::{build_native_packages, select_embeds};
use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
struct Args {
    /// Fresh absolute output directory (parent must already exist).
    #[arg(long)]
    out_dir: PathBuf,
    /// JSON configuration: {"embed":["CPU", "CPU:DIALECT"]}.
    #[arg(long)]
    config: Option<PathBuf>,
    /// Embedded target; repeated values replace configuration defaults.
    #[arg(long, conflicts_with_all=["external_only","embed_all"])]
    embed: Vec<String>,
    #[arg(long, conflicts_with = "embed_all")]
    external_only: bool,
    #[arg(long)]
    embed_all: bool,
    /// Generate assets and configured source without assembling the executable.
    #[arg(long)]
    catalog_only: bool,
}
fn main() {
    if let Err(error) = run(Args::parse()) {
        eprintln!("{error}");
        std::process::exit(1);
    }
}
fn run(args: Args) -> Result<(), String> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .map_err(|e| e.to_string())?;
    let config = args
        .config
        .as_ref()
        .map(std::fs::read_to_string)
        .transpose()
        .map_err(|e| e.to_string())?;
    let selection = select_embeds(
        config.as_deref(),
        &args.embed,
        args.external_only,
        args.embed_all,
    )?;
    let registry = engine::build_default_asm_registry();
    let build = build_native_packages(
        &registry,
        &args.out_dir,
        &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &selection,
    )?;
    if !args.catalog_only {
        let build_dir = build.output_dir.join("build");
        std::fs::create_dir(&build_dir).map_err(|e| e.to_string())?;
        let executable = build_dir.join("opforge_compact");
        let mut cli_args = vec![
            "opForge".to_string(),
            build.cli_source_path.to_string_lossy().into_owned(),
        ];
        // Module namespaces are rooted at each native architecture directory.
        let mut roots: Vec<_> = std::fs::read_dir(root.join("native"))
            .map_err(|e| e.to_string())?
            .filter_map(|entry| entry.ok().map(|e| e.path()))
            .filter(|path| path.is_dir())
            .collect();
        roots.sort();
        for path in roots {
            cli_args.extend(["--module-path".into(), path.to_string_lossy().into_owned()]);
        }
        for path in [
            root.join("native/motorola68000/amigaos/experimental"),
            root.join("native/motorola68000/amigaos/debug"),
        ] {
            cli_args.extend(["--include-path".into(), path.to_string_lossy().into_owned()]);
        }
        let cli = cli_core::Cli::try_parse_from(cli_args).map_err(|e| e.to_string())?;
        let mut config = cli_core::validate_cli(&cli)
            .map_err(|e| format!("invalid native CLI assembly configuration: {e:?}"))?;
        config.out_dir = Some(build.output_dir.clone());
        cli_core::run_with_validated_cli_with_context(&cli, &config).map_err(
            |error| match error {
                cli_core::CliRunError::Assembler { error, .. } => {
                    let mut message = format!("native CLI assembly failed: {}", error.summary());
                    for diagnostic in error.diagnostics().iter().take(6) {
                        message.push_str(&format!("\n{diagnostic:?}"));
                    }
                    message
                }
                cli_core::CliRunError::Workflow { error, .. } => {
                    format!("native CLI assembly failed: {error}")
                }
                cli_core::CliRunError::WarningsAsErrors { .. } => {
                    "native CLI assembly failed: warnings treated as errors".into()
                }
            },
        )?;
        if !executable.is_file() {
            return Err(format!("assembly did not create {}", executable.display()));
        }
        let distributed_executable = build.output_dir.join("opforge_compact");
        std::fs::copy(&executable, &distributed_executable).map_err(|e| e.to_string())?;
        println!("Executable: {}", distributed_executable.display());
    }
    println!(
        "Catalog: {} ({} packages, {} embedded)",
        build.catalog_path.display(),
        build.package_paths.len(),
        build.embedded_files.len()
    );
    Ok(())
}
