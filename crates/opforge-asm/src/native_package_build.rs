// SPDX-License-Identifier: GPL-3.0-or-later
//! Provisional host builder for the native compact package catalog.
use registry::ModuleRegistry;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::fs;
use std::path::{Path, PathBuf};
use vm::runtime_model_core::RuntimeModelCore;

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum EmbedSelection {
    #[default]
    ExternalOnly,
    Targets(Vec<String>),
    All,
}

/// Explicit command-line selections replace the config's defaults.
pub fn select_embeds(
    config: Option<&str>,
    cli: &[String],
    external_only: bool,
    all: bool,
) -> Result<EmbedSelection, String> {
    if usize::from(!cli.is_empty()) + usize::from(external_only) + usize::from(all) > 1 {
        return Err("--embed, --external-only and --embed-all are mutually exclusive".into());
    }
    let configured = match config {
        Some(text) => {
            let value: serde_json::Value = serde_json::from_str(text).map_err(|e| e.to_string())?;
            let object = value.as_object().ok_or("config must be a JSON object")?;
            if object.keys().any(|key| key != "embed") {
                return Err("unknown config field".into());
            }
            match object.get("embed") {
                None => Vec::new(),
                Some(value) => value
                    .as_array()
                    .ok_or("embed must be an array")?
                    .iter()
                    .map(|v| {
                        v.as_str()
                            .map(str::to_owned)
                            .ok_or_else(|| "embed targets must be strings".to_string())
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            }
        }
        None => Vec::new(),
    };
    Ok(if all {
        EmbedSelection::All
    } else if external_only {
        EmbedSelection::ExternalOnly
    } else if !cli.is_empty() {
        EmbedSelection::Targets(cli.to_vec())
    } else if configured.is_empty() {
        EmbedSelection::ExternalOnly
    } else {
        EmbedSelection::Targets(configured)
    })
}

#[derive(Clone, Debug)]
pub struct NativePackageTarget {
    pub cpu: String,
    pub dialect: String,
    pub is_default: bool,
    pub filename: String,
}

pub fn package_targets(registry: &ModuleRegistry) -> Result<Vec<NativePackageTarget>, String> {
    let mut targets = Vec::new();
    let mut filenames = BTreeSet::new();
    for cpu in registry.cpu_ids() {
        let family = registry.cpu_family_id(cpu).ok_or("CPU family missing")?;
        let default = registry
            .cpu_default_dialect(cpu)
            .ok_or("default dialect missing")?;
        let mut dialects = registry.dialect_ids_for_family(family);
        dialects.push(default.to_owned());
        dialects.sort();
        dialects.dedup();
        for dialect in dialects {
            for id in [cpu.as_str(), dialect.as_str()] {
                if id.is_empty()
                    || !id
                        .bytes()
                        .all(|b| b.is_ascii_alphanumeric() || b == b'_' || b == b'-')
                {
                    return Err(format!("unsafe package identifier: {id}"));
                }
            }
            let filename = format!("{}--{dialect}.bin", cpu.as_str());
            if filename.len() > 30 || !filenames.insert(filename.to_ascii_lowercase()) {
                return Err(format!("invalid or duplicate package filename: {filename}"));
            }
            targets.push(NativePackageTarget {
                cpu: cpu.as_str().into(),
                is_default: dialect == default,
                dialect,
                filename,
            });
        }
    }
    targets.sort_by(|a, b| (&a.cpu, &a.dialect).cmp(&(&b.cpu, &b.dialect)));
    if targets.len() > u16::MAX as usize {
        return Err("too many catalog entries".into());
    }
    Ok(targets)
}

pub fn resolve_embeds(
    registry: &ModuleRegistry,
    selection: &EmbedSelection,
) -> Result<BTreeSet<String>, String> {
    let targets = package_targets(registry)?;
    match selection {
        EmbedSelection::ExternalOnly => Ok(BTreeSet::new()),
        EmbedSelection::All => Ok(targets.iter().map(|t| t.filename.clone()).collect()),
        EmbedSelection::Targets(names) => names
            .iter()
            .map(|name| {
                let (cpu_name, dialect) = name
                    .split_once(':')
                    .map_or((name.as_str(), None), |(c, d)| (c, Some(d)));
                let cpu = registry
                    .resolve_cpu_name(cpu_name)
                    .ok_or_else(|| format!("unknown CPU: {cpu_name}"))?;
                let dialect = dialect.unwrap_or(
                    registry
                        .cpu_default_dialect(cpu)
                        .ok_or("default dialect missing")?,
                );
                targets
                    .iter()
                    .find(|t| t.cpu == cpu.as_str() && t.dialect.eq_ignore_ascii_case(dialect))
                    .map(|t| t.filename.clone())
                    .ok_or_else(|| format!("unknown target: {name}"))
            })
            .collect(),
    }
}

fn quoted_path(path: &Path) -> Result<String, String> {
    let value = path.to_str().ok_or("path must be UTF-8")?;
    if value.contains(['"', '\n', '\r']) {
        return Err("path cannot contain quotes or newlines".into());
    }
    Ok(format!("\"{value}\""))
}

fn catalog(
    registry: &ModuleRegistry,
    targets: &[NativePackageTarget],
    payloads: &BTreeMap<String, (PathBuf, usize)>,
) -> Result<String, String> {
    let mut aliases = BTreeMap::new();
    for cpu in registry.cpu_ids() {
        aliases.insert(cpu.as_str().to_string(), cpu.as_str().to_string());
    }
    for name in registry.cpu_name_list() {
        if let Some(cpu) = registry.resolve_cpu_name(&name) {
            aliases.insert(name, cpu.as_str().to_string());
        }
    }
    for name in aliases.keys() {
        if name.is_empty()
            || name.len() > 255
            || !name.is_ascii()
            || name.contains(['\0', '"', '\n', '\r'])
        {
            return Err(format!("invalid catalog alias: {name:?}"));
        }
    }
    if aliases.len() > u16::MAX as usize {
        return Err("too many catalog aliases".into());
    }
    let mut out = String::from("; Generated by build_native_packages; data only, offsets relative to Catalog.\nCatalog\n\t.long CatalogEnd-Catalog\n\t.long CatalogEntries-Catalog\n\t.long CatalogAliases-Catalog\n");
    writeln!(
        out,
        "\t.word {}\n\t.word {}\nCatalogEntries",
        targets.len(),
        aliases.len()
    )
    .unwrap();
    for (i, t) in targets.iter().enumerate() {
        writeln!(out,"\t.long CatalogKey{i}-Catalog\n\t.word {}\n\t.word {}\n\t.long CatalogCpu{i}-Catalog\n\t.word {}\n\t.word {}\n\t.long CatalogDialect{i}-Catalog",t.filename.len()-4,u8::from(t.is_default),t.cpu.len(),t.dialect.len()).unwrap();
        if let Some((_, size)) = payloads.get(&t.filename) {
            writeln!(out, "\t.long CatalogPayload{i}-Catalog\n\t.long {size}").unwrap();
        } else {
            out.push_str("\t.long 0\n\t.long 0\n");
        }
    }
    out.push_str("CatalogAliases\n");
    for (i, (name, cpu)) in aliases.iter().enumerate() {
        writeln!(out,"\t.long CatalogAliasName{i}-Catalog\n\t.word {}\n\t.word {}\n\t.long CatalogAliasCpu{i}-Catalog",name.len(),cpu.len()).unwrap();
    }
    for (i, t) in targets.iter().enumerate() {
        writeln!(out,"CatalogKey{i}\n\t.byte \"{}--{}\",0\nCatalogCpu{i}\n\t.byte \"{}\",0\nCatalogDialect{i}\n\t.byte \"{}\",0",t.cpu,t.dialect,t.cpu,t.dialect).unwrap();
    }
    for (i, (name, cpu)) in aliases.iter().enumerate() {
        writeln!(
            out,
            "CatalogAliasName{i}\n\t.byte \"{name}\",0\nCatalogAliasCpu{i}\n\t.byte \"{cpu}\",0"
        )
        .unwrap();
    }
    for (i, t) in targets.iter().enumerate() {
        if let Some((path, _)) = payloads.get(&t.filename) {
            writeln!(
                out,
                "\t.align 2\nCatalogPayload{i}\n\t.incbin {}",
                quoted_path(path)?
            )
            .unwrap();
        }
    }
    out.push_str("\t.align 2\nCatalogEnd\n");
    Ok(out)
}

/// Render the checked-in external-only catalog without generating package assets.
pub fn render_default_catalog(registry: &ModuleRegistry) -> Result<String, String> {
    catalog(registry, &package_targets(registry)?, &BTreeMap::new())
}

/// Package-independent Shell bootstrap assets, derived from shared VM grammar
/// and the engine's default rather than a native target-specific fallback.
pub fn render_target_bootstrap(default_cpu: &str) -> Result<String, String> {
    if default_cpu.is_empty()
        || default_cpu.len() > 255
        || !default_cpu
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'_' | b'-'))
    {
        return Err("invalid bootstrap default identity".into());
    }
    let mut out =
        String::from("; Generated shared target bootstrap; no target package or project inputs.\n");
    for (label, bytes) in [
        (
            "BootstrapTokenizer",
            vm::builder::shared_tokenizer_vm_program_bytes(),
        ),
        (
            "BootstrapParser",
            package::package::target_bootstrap_program(),
        ),
    ] {
        writeln!(out, "{label}").unwrap();
        for row in bytes.chunks(16) {
            out.push_str("\t.byte ");
            for (i, byte) in row.iter().enumerate() {
                if i != 0 {
                    out.push(',');
                }
                write!(out, "${byte:02x}").unwrap();
            }
            out.push('\n');
        }
        writeln!(out, "{label}End").unwrap();
    }
    writeln!(
        out,
        "BootstrapDefault\n\t.byte \"{}\",0\n\t.align 2",
        default_cpu
    )
    .unwrap();
    Ok(out)
}

#[derive(Debug)]
pub struct NativePackageBuild {
    pub output_dir: PathBuf,
    pub catalog_path: PathBuf,
    pub cli_source_path: PathBuf,
    pub manifest_path: PathBuf,
    pub package_paths: Vec<PathBuf>,
    pub embedded_files: BTreeSet<String>,
}

/// Generate every external package plus a selected embedded catalog and CLI source.
/// The caller owns registry construction and any subsequent host assembly.
pub fn build_native_packages(
    registry: &ModuleRegistry,
    output_dir: &Path,
    cli_template: &Path,
    selection: &EmbedSelection,
) -> Result<NativePackageBuild, String> {
    if !output_dir.is_absolute() || output_dir.exists() {
        return Err("output directory must be fresh and absolute".into());
    }
    let embedded_files = resolve_embeds(registry, selection)?;
    let targets = package_targets(registry)?;
    let core = RuntimeModelCore::from_registry(registry).map_err(|e| e.to_string())?;
    let mut packages = Vec::new();
    for target in &targets {
        let resolved = core
            .resolve_pipeline(&target.cpu, Some(&target.dialect))
            .map_err(|e| e.to_string())?;
        packages.push(crate::binary_source_experiment::prepare_package(
            &core, &resolved,
        )?);
    }
    let parent = output_dir
        .parent()
        .ok_or("output parent missing")?
        .canonicalize()
        .map_err(|e| e.to_string())?;
    let output_dir = parent.join(
        output_dir
            .file_name()
            .ok_or("output directory name missing")?,
    );
    let packages_dir = output_dir.join("packages");
    let manifest_path = output_dir.join("manifest.json");
    let catalog_path = output_dir.join("catalog.i");
    let cli_source_path = output_dir.join("opforge_compact_cli.asm");
    let template = fs::read_to_string(cli_template).map_err(|e| e.to_string())?;
    let marker = ".include \"package_catalog.i\"";
    if template.matches(marker).count() != 1 {
        return Err("CLI template must contain exactly one package_catalog.i include".into());
    }
    // Generated sources and assets form one relocatable build directory.
    let source = template.replace(marker, ".include \"catalog.i\"");
    let payloads = targets
        .iter()
        .zip(&packages)
        .filter(|(t, _)| embedded_files.contains(&t.filename))
        .map(|(t, p)| {
            (
                t.filename.clone(),
                (Path::new("packages").join(&t.filename), p.len()),
            )
        })
        .collect();
    let catalog = catalog(registry, &targets, &payloads)?;
    fs::create_dir(&output_dir).map_err(|e| e.to_string())?;
    fs::create_dir(&packages_dir).map_err(|e| e.to_string())?;
    let mut package_paths = Vec::new();
    for (target, bytes) in targets.iter().zip(packages) {
        let path = packages_dir.join(&target.filename);
        fs::write(&path, bytes).map_err(|e| e.to_string())?;
        package_paths.push(path);
    }
    let manifest = serde_json::json!({
        "format": "BS21",
        "scope": "host catalog build inputs; no native execution or parity claim",
        "embedded_files": embedded_files,
        "targets": targets.iter().map(|target| serde_json::json!({
            "cpu": target.cpu,
            "dialect": target.dialect,
            "is_default": target.is_default,
            "embedded": embedded_files.contains(&target.filename),
            "file": format!("packages/{}", target.filename),
        })).collect::<Vec<_>>()
    });
    fs::write(
        &manifest_path,
        serde_json::to_vec_pretty(&manifest).map_err(|e| e.to_string())?,
    )
    .map_err(|e| e.to_string())?;
    fs::write(&catalog_path, catalog).map_err(|e| e.to_string())?;
    fs::write(&cli_source_path, source).map_err(|e| e.to_string())?;
    Ok(NativePackageBuild {
        output_dir,
        catalog_path,
        cli_source_path,
        manifest_path,
        package_paths,
        embedded_files,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn cli_selection_replaces_config_defaults() {
        assert_eq!(
            select_embeds(
                Some(r#"{"embed":["missing"]}"#),
                &["chosen".into()],
                false,
                false
            )
            .unwrap(),
            EmbedSelection::Targets(vec!["chosen".into()])
        );
        assert_eq!(
            select_embeds(Some(r#"{"embed":["missing"]}"#), &[], true, false).unwrap(),
            EmbedSelection::ExternalOnly
        );
        assert!(select_embeds(None, &["chosen".into()], false, true).is_err());
    }
    #[test]
    fn aliases_resolve_to_canonical_default_packages() {
        let registry = engine::build_default_asm_registry();
        for alias in registry.cpu_name_list() {
            let cpu = registry.resolve_cpu_name(&alias).unwrap();
            let expected = format!(
                "{}--{}.bin",
                cpu.as_str(),
                registry.cpu_default_dialect(cpu).unwrap()
            );
            assert_eq!(
                resolve_embeds(
                    &registry,
                    &EmbedSelection::Targets(vec![alias.to_ascii_uppercase()])
                )
                .unwrap(),
                BTreeSet::from([expected])
            );
        }
    }
    #[test]
    fn unknown_selection_creates_no_output() {
        let registry = engine::build_default_asm_registry();
        let path =
            std::env::temp_dir().join(format!("opforge-invalid-selection-{}", std::process::id()));
        assert!(build_native_packages(
            &registry,
            &path,
            Path::new("absent.asm"),
            &EmbedSelection::Targets(vec!["not-a-cpu".into()])
        )
        .is_err());
        assert!(!path.exists());
    }
    #[test]
    fn default_catalog_is_asset_independent_and_relative() {
        let registry = engine::build_default_asm_registry();
        let text = render_default_catalog(&registry).unwrap();
        assert_eq!(
            text,
            include_str!("../../../native/motorola68000/amigaos/experimental/package_catalog.i")
        );
        assert!(!text.contains(".incbin"));
        assert!(text.contains("CatalogEntries-Catalog"));
        for target in package_targets(&registry).unwrap() {
            assert!(text.contains(&format!("\"{}--{}\",0", target.cpu, target.dialect)));
        }
    }

    #[test]
    fn shared_target_bootstrap_matches_checked_in_asset() {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../native/motorola68000/amigaos/experimental/target_bootstrap.i");
        assert_eq!(
            fs::read_to_string(path).unwrap(),
            render_target_bootstrap(engine::default_cpu().as_str()).unwrap()
        );
    }

    #[test]
    #[ignore = "explicit shared bootstrap asset regeneration"]
    fn export_native_target_bootstrap_asset() {
        let path = std::env::var_os("OPFORGE_TARGET_BOOTSTRAP_EXPORT")
            .map(PathBuf::from)
            .expect("explicit export path");
        assert!(path.is_absolute());
        fs::write(
            path,
            render_target_bootstrap(engine::default_cpu().as_str()).unwrap(),
        )
        .unwrap();
    }
}
