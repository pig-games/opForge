// SPDX-License-Identifier: GPL-3.0-or-later

use std::path::PathBuf;

use serde_json::json;

use crate::path_display::{stable_path_string, stable_path_text};
use crate::symbol::SymbolTable;

#[derive(Debug, Clone)]
pub struct DependencyOutputPolicy {
    pub path: PathBuf,
    pub append: bool,
    pub make_phony: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LabelOutputFormat {
    #[default]
    Default,
    Vice,
    Ctags,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OutputFormat {
    #[default]
    Text,
    Json,
}

fn make_escape_path(path: &str) -> String {
    path.replace(' ', "\\ ")
}

pub fn format_addr(addr: u32) -> String {
    if addr <= 0xFFFF {
        format!("{addr:04X}")
    } else if addr <= 0xFF_FFFF {
        format!("{addr:06X}")
    } else {
        format!("{addr:08X}")
    }
}

#[must_use]
pub fn render_labels(
    format: LabelOutputFormat,
    output_format: OutputFormat,
    symbols: &SymbolTable,
) -> String {
    let mut entries = symbols.entries().to_vec();
    entries.sort_by(|left, right| {
        left.name
            .to_ascii_lowercase()
            .cmp(&right.name.to_ascii_lowercase())
    });

    if output_format == OutputFormat::Json && format == LabelOutputFormat::Default {
        let labels: Vec<serde_json::Value> = entries
            .into_iter()
            .map(|entry| {
                json!({
                    "name": entry.name,
                    "address": format_addr(entry.val),
                    "value": entry.val,
                })
            })
            .collect();
        return json!({ "labels": labels }).to_string();
    }

    let mut output = String::new();
    for entry in entries {
        let address = format_addr(entry.val);
        match format {
            LabelOutputFormat::Default => {
                output.push_str(&format!("{} = ${address}\n", entry.name));
            }
            LabelOutputFormat::Vice => {
                output.push_str(&format!("al C:${address} .{}\n", entry.name));
            }
            LabelOutputFormat::Ctags => {
                output.push_str(&format!(
                    "{}\tlabels\t/^{}$/;\"\tv\n",
                    entry.name, entry.name
                ));
            }
        }
    }
    output
}

pub fn render_dependencies(
    output_format: OutputFormat,
    targets: &[String],
    dependencies: &[PathBuf],
    make_phony: bool,
) -> Option<String> {
    let mut targets: Vec<String> = targets
        .iter()
        .filter(|target| !target.is_empty())
        .map(|target| make_escape_path(stable_path_text(target).as_str()))
        .collect();
    targets.sort();
    targets.dedup();
    if targets.is_empty() {
        return None;
    }

    let mut dependencies: Vec<String> = dependencies
        .iter()
        .map(|path| make_escape_path(stable_path_string(path).as_str()))
        .collect();
    dependencies.sort();
    dependencies.dedup();

    Some(if output_format == OutputFormat::Json {
        json!({
            "targets": targets,
            "dependencies": dependencies,
            "make_phony": make_phony,
            "phony_targets": if make_phony { dependencies.clone() } else { Vec::new() },
        })
        .to_string()
            + "\n"
    } else {
        let mut body = String::new();
        body.push_str(&format!(
            "{}: {}\n",
            targets.join(" "),
            dependencies.join(" ")
        ));
        if make_phony {
            for dependency in &dependencies {
                body.push_str(&format!("{dependency}:\n"));
            }
        }
        body
    })
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::{render_dependencies, OutputFormat};

    #[test]
    fn render_dependencies_normalizes_windows_style_paths() {
        let body = render_dependencies(
            OutputFormat::Text,
            &[
                String::from(r"\\?\C:\build\main.lst"),
                String::from(r"\\?\UNC\server\share\main.hex"),
            ],
            &[
                PathBuf::from(r"\\?\C:\src\main.asm"),
                PathBuf::from(r"\\server\share\dep.asm"),
            ],
            false,
        )
        .expect("dependencies should render");

        assert!(body.contains("C:/build/main.lst"), "body:\n{body}");
        assert!(body.contains("//server/share/main.hex"), "body:\n{body}");
        assert!(body.contains("C:/src/main.asm"), "body:\n{body}");
        assert!(body.contains("//server/share/dep.asm"), "body:\n{body}");
        assert!(!body.contains('\\'), "body:\n{body}");
    }
}
