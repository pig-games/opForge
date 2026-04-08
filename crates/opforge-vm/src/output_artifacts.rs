// SPDX-License-Identifier: GPL-3.0-or-later

use std::collections::HashMap;

use crate::output_model::{
    ExportSectionsDirective, ExportSectionsInclude, LinkerOutputDirective, MapFileDirective,
    MapSymbolsMode, RegionState, SectionKind, SectionState,
};
use types::artifacts::format_addr;
use types::symbol::{SymbolTable, SymbolTableEntry, SymbolVisibility};

#[derive(Debug, Clone)]
pub struct ArtifactBuildError {
    message: String,
    subject: Option<String>,
}

impl ArtifactBuildError {
    pub fn new(message: impl Into<String>, subject: Option<impl Into<String>>) -> Self {
        Self {
            message: message.into(),
            subject: subject.map(Into::into),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn subject(&self) -> Option<&str> {
        self.subject.as_deref()
    }
}

fn section_kind_name(kind: SectionKind) -> &'static str {
    match kind {
        SectionKind::Code => "code",
        SectionKind::Data => "data",
        SectionKind::Bss => "bss",
    }
}

pub fn build_linker_output_payload(
    output: &LinkerOutputDirective,
    sections: &HashMap<String, SectionState>,
) -> Result<Vec<u8>, ArtifactBuildError> {
    crate::output_components::build_linker_output_payload_via_registry(output, sections)
}

pub fn build_export_sections_payloads(
    directive: &ExportSectionsDirective,
    sections: &HashMap<String, SectionState>,
) -> Vec<(String, Vec<u8>)> {
    let mut names: Vec<&String> = sections.keys().collect();
    names.sort();

    let mut outputs = Vec::new();
    for name in names {
        let section = &sections[name];
        if directive.include == ExportSectionsInclude::NoBss && section.is_bss() {
            continue;
        }
        let mut filename = name.clone();
        filename.push_str(".bin");
        outputs.push((filename, section.bytes.clone()));
    }
    outputs
}

pub fn build_mapfile_text(
    directive: &MapFileDirective,
    regions: &HashMap<String, RegionState>,
    sections: &HashMap<String, SectionState>,
    symbols: &SymbolTable,
) -> String {
    let mut out = String::new();

    out.push_str("Regions\n");
    out.push_str("name start end used free align\n");
    let mut region_names: Vec<&String> = regions.keys().collect();
    region_names.sort();
    for name in region_names {
        let region = &regions[name];
        let capacity = u64::from(region.end)
            .checked_sub(u64::from(region.start))
            .and_then(|delta| delta.checked_add(1))
            .unwrap_or(0);
        let used = u64::from(region.cursor)
            .saturating_sub(u64::from(region.start))
            .min(capacity);
        let free = capacity.saturating_sub(used);
        out.push_str(&format!(
            "{} {} {} {} {} {}\n",
            region.name,
            format_addr(region.start),
            format_addr(region.end),
            used,
            free,
            region.align
        ));
    }
    out.push('\n');

    out.push_str("Sections\n");
    out.push_str("name base size kind region\n");
    let mut section_region: HashMap<String, String> = HashMap::new();
    for region in regions.values() {
        for placed in &region.placed {
            section_region.insert(placed.name.clone(), region.name.clone());
        }
    }
    let mut section_names: Vec<&String> = sections.keys().collect();
    section_names.sort();
    for name in section_names {
        let section = &sections[name];
        let base_text = section
            .base_addr
            .map(format_addr)
            .unwrap_or_else(|| "----".to_string());
        let region_name = section_region
            .get(name.as_str())
            .cloned()
            .unwrap_or_else(|| "-".to_string());
        out.push_str(&format!(
            "{} {} {} {} {}\n",
            name,
            base_text,
            section.size_bytes(),
            section_kind_name(section.kind),
            region_name
        ));
    }

    if directive.symbols != MapSymbolsMode::None {
        out.push('\n');
        out.push_str("Symbols\n");
        out.push_str("name value visibility\n");

        let mut entries: Vec<&SymbolTableEntry> = symbols.entries().iter().collect();
        entries.sort_by(|a, b| {
            a.name
                .to_ascii_lowercase()
                .cmp(&b.name.to_ascii_lowercase())
        });
        for entry in entries {
            if directive.symbols == MapSymbolsMode::Public
                && entry.visibility != SymbolVisibility::Public
            {
                continue;
            }
            let visibility = match entry.visibility {
                SymbolVisibility::Public => "public",
                SymbolVisibility::Private => "private",
            };
            out.push_str(&format!(
                "{} {} {}\n",
                entry.name,
                format_addr(entry.val),
                visibility
            ));
        }
    }

    out
}
