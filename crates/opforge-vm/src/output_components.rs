// SPDX-License-Identifier: GPL-3.0-or-later

use std::collections::HashMap;

use crate::output_artifacts::ArtifactBuildError;
use crate::output_model::{LinkerOutputDirective, LinkerOutputFormat, SectionState};
use types::artifacts::format_addr;

const BUILTIN_OUTPUT_FORMAT_IDS: &[&str] = &["bin", "prg"];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BuiltinOutputComponent {
    Bin,
    Prg,
}

impl BuiltinOutputComponent {
    fn format_id(self) -> &'static str {
        match self {
            Self::Bin => LinkerOutputFormat::Bin.format_id(),
            Self::Prg => LinkerOutputFormat::Prg.format_id(),
        }
    }

    fn build_payload(
        self,
        output: &LinkerOutputDirective,
        sections: &HashMap<String, SectionState>,
    ) -> Result<Vec<u8>, ArtifactBuildError> {
        let ordered = collect_linker_sections(output, sections)?;
        match self {
            Self::Bin => build_bin_payload(output, &ordered),
            Self::Prg => build_prg_payload(output, &ordered),
        }
    }
}

#[derive(Debug, Clone)]
struct ResolvedLinkerSection {
    name: String,
    base: u32,
    bytes: Vec<u8>,
}

pub(crate) fn supported_output_format_ids() -> &'static [&'static str] {
    BUILTIN_OUTPUT_FORMAT_IDS
}

pub(crate) fn resolve_output_component(
    format_id: &str,
) -> Result<BuiltinOutputComponent, ArtifactBuildError> {
    for component in [BuiltinOutputComponent::Bin, BuiltinOutputComponent::Prg] {
        if component.format_id().eq_ignore_ascii_case(format_id) {
            return Ok(component);
        }
    }

    Err(ArtifactBuildError::new(
        format!(
            "Unknown .output format '{}'; supported formats: {}",
            format_id,
            supported_output_format_ids().join(", ")
        ),
        None::<String>,
    ))
}

pub(crate) fn build_linker_output_payload_via_registry(
    output: &LinkerOutputDirective,
    sections: &HashMap<String, SectionState>,
) -> Result<Vec<u8>, ArtifactBuildError> {
    let component = resolve_output_component(&output.format_id)?;
    component.build_payload(output, sections)
}

fn collect_linker_sections(
    output: &LinkerOutputDirective,
    sections: &HashMap<String, SectionState>,
) -> Result<Vec<ResolvedLinkerSection>, ArtifactBuildError> {
    let section_names = output.option_text_list("sections").ok_or_else(|| {
        ArtifactBuildError::new("Missing sections option in .output", None::<String>)
    })?;

    let mut resolved = Vec::with_capacity(section_names.len());
    for section_name in section_names {
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

fn parse_bool_option(
    output: &LinkerOutputDirective,
    key: &str,
    default: bool,
) -> Result<bool, ArtifactBuildError> {
    let Some(value) = output.option_text(key) else {
        return Ok(default);
    };
    if value.eq_ignore_ascii_case("true") || value.eq_ignore_ascii_case("yes") || value == "1" {
        Ok(true)
    } else if value.eq_ignore_ascii_case("false")
        || value.eq_ignore_ascii_case("no")
        || value == "0"
    {
        Ok(false)
    } else {
        Err(ArtifactBuildError::new(
            format!("{key} must be true/false"),
            Some(output.path.clone()),
        ))
    }
}

fn parse_u32_option(
    output: &LinkerOutputDirective,
    key: &str,
) -> Result<Option<u32>, ArtifactBuildError> {
    let Some(value) = output.option_text(key) else {
        return Ok(None);
    };
    opcore::expression::parse_number_text(
        value,
        opcore::tokenizer::Span {
            line: 0,
            col_start: 0,
            col_end: 0,
        },
    )
    .map(Some)
    .map_err(|_| {
        ArtifactBuildError::new(
            format!("Invalid {key} value in .output"),
            Some(output.path.clone()),
        )
    })
}

fn parse_fill_option(output: &LinkerOutputDirective) -> Result<Option<u8>, ArtifactBuildError> {
    let Some(value) = parse_u32_option(output, "fill")? else {
        return Ok(None);
    };
    if value > u8::MAX as u32 {
        return Err(ArtifactBuildError::new(
            "fill must be in range 0..255 in .output",
            Some(output.path.clone()),
        ));
    }
    Ok(Some(value as u8))
}

fn parse_image_span(
    output: &LinkerOutputDirective,
) -> Result<Option<(u32, u32)>, ArtifactBuildError> {
    let Some(value) = output.option_text("image") else {
        return Ok(None);
    };
    let Some((start_text, end_text)) = value.split_once("..") else {
        return Err(ArtifactBuildError::new(
            "image must use start..end (quote it for now)",
            Some(output.path.clone()),
        ));
    };
    let span = opcore::tokenizer::Span {
        line: 0,
        col_start: 0,
        col_end: 0,
    };
    let start = opcore::expression::parse_number_text(start_text.trim(), span).map_err(|_| {
        ArtifactBuildError::new("Invalid image span value", Some(output.path.clone()))
    })?;
    let end = opcore::expression::parse_number_text(end_text.trim(), span).map_err(|_| {
        ArtifactBuildError::new("Invalid image span value", Some(output.path.clone()))
    })?;
    if start > end {
        return Err(ArtifactBuildError::new(
            "Invalid image span range in .output",
            Some(output.path.clone()),
        ));
    }
    Ok(Some((start, end)))
}

fn build_bin_payload(
    output: &LinkerOutputDirective,
    ordered: &[ResolvedLinkerSection],
) -> Result<Vec<u8>, ArtifactBuildError> {
    let image_span = parse_image_span(output)?;
    let fill = parse_fill_option(output)?;
    let contiguous = parse_bool_option(output, "contiguous", true)?;

    if let Some((image_start, image_end)) = image_span {
        let Some(fill) = fill else {
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
        for section in ordered {
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
        return Ok(image);
    }

    if fill.is_some() {
        return Err(ArtifactBuildError::new(
            "fill is only allowed with image output in .output",
            None::<String>,
        ));
    }

    if contiguous {
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
    for section in ordered {
        data.extend_from_slice(&section.bytes);
    }
    Ok(data)
}

fn build_prg_payload(
    output: &LinkerOutputDirective,
    ordered: &[ResolvedLinkerSection],
) -> Result<Vec<u8>, ArtifactBuildError> {
    let mut payload = build_bin_payload(output, ordered)?;
    let loadaddr32 = parse_u32_option(output, "loadaddr")?.unwrap_or_else(|| {
        ordered
            .iter()
            .find(|section| !section.bytes.is_empty())
            .or_else(|| ordered.first())
            .map(|section| section.base)
            .unwrap_or(0)
    });
    let loadaddr = match u16::try_from(loadaddr32) {
        Ok(value) => value,
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
    Ok(prg)
}

#[cfg(test)]
mod tests {
    use super::{resolve_output_component, BuiltinOutputComponent};

    #[test]
    fn resolve_output_component_accepts_bin_and_prg() {
        assert_eq!(
            resolve_output_component("bin").expect("bin should resolve"),
            BuiltinOutputComponent::Bin
        );
        assert_eq!(
            resolve_output_component("prg").expect("prg should resolve"),
            BuiltinOutputComponent::Prg
        );
    }

    #[test]
    fn resolve_output_component_rejects_unknown_format() {
        let err = resolve_output_component("hunk").expect_err("unknown format should fail");
        assert!(err.message().contains("supported formats: bin, prg"));
    }
}
