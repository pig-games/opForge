// SPDX-License-Identifier: GPL-3.0-or-later

use std::collections::HashMap;

use crate::output_model::{
    ExportSectionsDirective, ExportSectionsInclude, LinkerOutputDirective, LinkerOutputFormat,
    MapFileDirective, MapSymbolsMode, RegionState, SectionKind, SectionState,
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

#[derive(Debug, Clone)]
struct ResolvedLinkerSection {
    name: String,
    base: u32,
    bytes: Vec<u8>,
}

fn section_kind_name(kind: SectionKind) -> &'static str {
    match kind {
        SectionKind::Code => "code",
        SectionKind::Data => "data",
        SectionKind::Bss => "bss",
    }
}

fn collect_linker_sections(
    output: &LinkerOutputDirective,
    sections: &HashMap<String, SectionState>,
) -> Result<Vec<ResolvedLinkerSection>, ArtifactBuildError> {
    let mut resolved = Vec::with_capacity(output.sections.len());
    for section_name in &output.sections {
        let Some(section) = sections.get(section_name) else {
            return Err(ArtifactBuildError::new(
                "Unknown section referenced by .output",
                Some(section_name.clone()),
            ));
        };
        let Some(base) = section.base_addr else {
            return Err(ArtifactBuildError::new(
                "Section referenced by .output must be explicitly placed",
                Some(section_name.clone()),
            ));
        };
        resolved.push(ResolvedLinkerSection {
            name: section_name.clone(),
            base,
            bytes: section.bytes.clone(),
        });
    }
    resolved.sort_by_key(|section| section.base);
    Ok(resolved)
}

pub fn build_linker_output_payload(
    output: &LinkerOutputDirective,
    sections: &HashMap<String, SectionState>,
) -> Result<Vec<u8>, ArtifactBuildError> {
    let ordered = collect_linker_sections(output, sections)?;
    let mut payload = if let (Some(image_start), Some(image_end)) =
        (output.image_start, output.image_end)
    {
        let Some(fill) = output.fill else {
            return Err(ArtifactBuildError::new(
                "image output requires fill in .output",
                None::<String>,
            ));
        };
        let span_len = image_end
            .checked_sub(image_start)
            .and_then(|delta| delta.checked_add(1))
            .ok_or_else(|| {
                ArtifactBuildError::new(
                    "Invalid image span range in .output",
                    Some(output.path.clone()),
                )
            })?;
        let image_len = usize::try_from(span_len).map_err(|_| {
            ArtifactBuildError::new(
                "Image span is too large for this host",
                Some(output.path.clone()),
            )
        })?;
        let mut image = vec![fill; image_len];
        for section in &ordered {
            if section.bytes.is_empty() {
                continue;
            }
            let section_len_u32 = u32::try_from(section.bytes.len()).map_err(|_| {
                ArtifactBuildError::new(
                    "Section size is too large for address arithmetic in .output",
                    Some(section.name.clone()),
                )
            })?;
            let start = section.base;
            let end = start.checked_add(section_len_u32 - 1).ok_or_else(|| {
                ArtifactBuildError::new(
                    "Section address range overflows in .output",
                    Some(section.name.clone()),
                )
            })?;
            if start < image_start || end > image_end {
                return Err(ArtifactBuildError::new(
                    "Section falls outside image span in .output",
                    Some(section.name.clone()),
                ));
            }
            let offset_u32 = start.checked_sub(image_start).ok_or_else(|| {
                ArtifactBuildError::new(
                    "Section falls outside image span in .output",
                    Some(section.name.clone()),
                )
            })?;
            let offset = usize::try_from(offset_u32).map_err(|_| {
                ArtifactBuildError::new(
                    "Image offset is too large for this host",
                    Some(section.name.clone()),
                )
            })?;
            let end_offset = offset.checked_add(section.bytes.len()).ok_or_else(|| {
                ArtifactBuildError::new(
                    "Image offset arithmetic overflow in .output",
                    Some(section.name.clone()),
                )
            })?;
            if end_offset > image.len() {
                return Err(ArtifactBuildError::new(
                    "Section falls outside image span in .output",
                    Some(section.name.clone()),
                ));
            }
            image[offset..end_offset].copy_from_slice(&section.bytes);
        }
        image
    } else {
        if output.contiguous {
            let mut expected_base: Option<u32> = None;
            for section in ordered.iter().filter(|section| !section.bytes.is_empty()) {
                let base = section.base;
                let section_len_u32 = u32::try_from(section.bytes.len()).map_err(|_| {
                    ArtifactBuildError::new(
                        "Section size is too large for address arithmetic in .output",
                        Some(section.name.clone()),
                    )
                })?;
                if let Some(expected) = expected_base {
                    if base != expected {
                        let message = if base > expected {
                            format!(
                                "contiguous output requires adjacent sections; gap ${}..${}",
                                format_addr(expected),
                                format_addr(base - 1)
                            )
                        } else {
                            format!(
                                "contiguous output requires adjacent sections; overlap ${}..${}",
                                format_addr(base),
                                format_addr(expected - 1)
                            )
                        };
                        return Err(ArtifactBuildError::new(message, Some(section.name.clone())));
                    }
                }
                expected_base = Some(base.checked_add(section_len_u32).ok_or_else(|| {
                    ArtifactBuildError::new(
                        "Section address range overflows in contiguous output",
                        Some(section.name.clone()),
                    )
                })?);
            }
        }
        let total_len = ordered.iter().try_fold(0usize, |acc, section| {
            acc.checked_add(section.bytes.len()).ok_or_else(|| {
                ArtifactBuildError::new(
                    "Output payload is too large for this host",
                    Some(output.path.clone()),
                )
            })
        })?;
        let mut data = Vec::with_capacity(total_len);
        for section in &ordered {
            data.extend_from_slice(&section.bytes);
        }
        data
    };

    if output.format == LinkerOutputFormat::Prg {
        let loadaddr32 = output.loadaddr.unwrap_or_else(|| {
            ordered
                .iter()
                .find(|section| !section.bytes.is_empty())
                .or_else(|| ordered.first())
                .map(|section| section.base)
                .unwrap_or(0)
        });
        let loadaddr = match u16::try_from(loadaddr32) {
            Ok(v) => v,
            Err(_) => {
                return Err(ArtifactBuildError::new(
                    "PRG load address exceeds 16-bit range",
                    Some(output.path.clone()),
                ));
            }
        };
        let mut prg = Vec::with_capacity(payload.len() + 2);
        prg.push((loadaddr & 0x00ff) as u8);
        prg.push((loadaddr >> 8) as u8);
        prg.append(&mut payload);
        return Ok(prg);
    }

    Ok(payload)
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
