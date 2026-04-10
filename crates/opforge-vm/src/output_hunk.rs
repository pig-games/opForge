// SPDX-License-Identifier: GPL-3.0-or-later

use std::collections::HashMap;

use crate::output_artifacts::ArtifactBuildError;
use crate::output_model::{
    HunkOutputInput, HunkSegmentInput, LinkerOutputDirective, LinkerOutputRelocationDisposition,
    OutputFixupKind, SectionKind, SectionState, IMPLICIT_HUNK_CODE_SECTION_NAME,
};

const HUNK_HEADER: u32 = 0x0000_03f3;
const HUNK_CODE: u32 = 0x0000_03e9;
const HUNK_DATA: u32 = 0x0000_03ea;
const HUNK_BSS: u32 = 0x0000_03eb;
const HUNK_RELOC32: u32 = 0x0000_03ec;
const HUNK_END: u32 = 0x0000_03f2;

pub(crate) fn build_hunk_output_payload(
    output: &LinkerOutputDirective,
    sections: &HashMap<String, SectionState>,
) -> Result<Vec<u8>, ArtifactBuildError> {
    validate_hunk_options(output)?;
    let input = collect_hunk_output_input(output, sections)?;
    build_hunk_payload(&input)
}

fn validate_hunk_options(output: &LinkerOutputDirective) -> Result<(), ArtifactBuildError> {
    for key in ["image", "fill", "loadaddr", "contiguous"] {
        if output.option(key).is_some() {
            return Err(ArtifactBuildError::new(
                format!("format=hunk does not support {key} in v0.1"),
                Some(output.path.clone()),
            ));
        }
    }
    Ok(())
}

fn collect_hunk_output_input(
    output: &LinkerOutputDirective,
    sections: &HashMap<String, SectionState>,
) -> Result<HunkOutputInput, ArtifactBuildError> {
    let implicit_section_names;
    let section_names = if let Some(section_names) = output.option_text_list("sections") {
        section_names
    } else if sections.contains_key(IMPLICIT_HUNK_CODE_SECTION_NAME) {
        implicit_section_names = [IMPLICIT_HUNK_CODE_SECTION_NAME.to_string()];
        &implicit_section_names
    } else {
        return Err(ArtifactBuildError::new(
            "Missing sections option in .output",
            None::<String>,
        ));
    };

    let mut segments = Vec::new();
    for section_name in section_names {
        let Some(section) = sections.get(section_name) else {
            return Err(ArtifactBuildError::new(
                "Unknown section referenced by .output",
                Some(section_name.clone()),
            ));
        };
        if let Some(message) = section.hunk_fixup_error.as_ref() {
            return Err(ArtifactBuildError::new(
                message.clone(),
                Some(section_name.clone()),
            ));
        }
        if section.kind != SectionKind::Bss && section.bytes.is_empty() {
            continue;
        }
        let allocation_size_bytes = section.max_pc.max(section.bytes.len() as u32);
        segments.push(HunkSegmentInput {
            name: section_name.clone(),
            kind: section.kind,
            initialized_bytes: section.bytes.clone(),
            allocation_size_bytes,
            memory_type: section.hunk_memory_type,
            fixups: section.output_fixups.clone(),
        });
    }

    Ok(HunkOutputInput {
        segments,
        relocation_disposition: output.relocation_disposition,
    })
}

pub(crate) fn build_hunk_payload(input: &HunkOutputInput) -> Result<Vec<u8>, ArtifactBuildError> {
    match input.relocation_disposition {
        LinkerOutputRelocationDisposition::ProvenRelocationFree => {}
        LinkerOutputRelocationDisposition::RelocationRecordsPresent => {}
        LinkerOutputRelocationDisposition::Unknown => {
            return Err(ArtifactBuildError::new(
                "format=hunk requires explicit relocation-free proof in v0.1",
                None::<String>,
            ));
        }
    }

    if input.segments.is_empty() {
        return Err(ArtifactBuildError::new(
            "format=hunk requires at least one emitted segment in v0.1",
            None::<String>,
        ));
    }
    if input.segments[0].kind != SectionKind::Code {
        return Err(ArtifactBuildError::new(
            "format=hunk requires the first emitted segment to be code",
            Some(input.segments[0].name.clone()),
        ));
    }

    let segment_count = u32::try_from(input.segments.len()).map_err(|_| {
        ArtifactBuildError::new(
            "format=hunk segment count exceeds supported range",
            None::<String>,
        )
    })?;

    let mut bytes = Vec::new();
    push_be_u32(&mut bytes, HUNK_HEADER);
    push_be_u32(&mut bytes, 0);
    push_be_u32(&mut bytes, segment_count);
    push_be_u32(&mut bytes, 0);
    push_be_u32(&mut bytes, segment_count - 1);
    for segment in &input.segments {
        let allocation_longwords = longword_count(segment.allocation_size_bytes)?;
        push_be_u32(
            &mut bytes,
            allocation_longwords | segment.memory_type.segment_bits(),
        );
    }

    for segment in &input.segments {
        push_be_u32(&mut bytes, hunk_kind_word(segment.kind));
        let payload_size_bytes = if segment.kind == SectionKind::Bss {
            segment.allocation_size_bytes
        } else {
            u32::try_from(segment.initialized_bytes.len()).map_err(|_| {
                ArtifactBuildError::new(
                    "format=hunk payload exceeds supported size",
                    Some(segment.name.clone()),
                )
            })?
        };
        push_be_u32(&mut bytes, longword_count(payload_size_bytes)?);
        if segment.kind != SectionKind::Bss {
            bytes.extend_from_slice(&segment.initialized_bytes);
            let padded_len = padded_byte_len(segment.initialized_bytes.len())?;
            bytes.resize(
                bytes.len() + (padded_len - segment.initialized_bytes.len()),
                0,
            );
        }
        if input.relocation_disposition
            == LinkerOutputRelocationDisposition::RelocationRecordsPresent
        {
            append_relocation_hunks(&mut bytes, segment, &input.segments)?;
        }
        push_be_u32(&mut bytes, HUNK_END);
    }

    Ok(bytes)
}

fn append_relocation_hunks(
    bytes: &mut Vec<u8>,
    segment: &HunkSegmentInput,
    all_segments: &[HunkSegmentInput],
) -> Result<(), ArtifactBuildError> {
    if segment.fixups.is_empty() {
        return Ok(());
    }

    let payload_len = u32::try_from(segment.initialized_bytes.len()).map_err(|_| {
        ArtifactBuildError::new(
            "format=hunk payload exceeds supported size",
            Some(segment.name.clone()),
        )
    })?;

    let mut grouped_offsets: HashMap<u32, Vec<u32>> = HashMap::new();
    for fixup in &segment.fixups {
        if !fixup.source_section.eq_ignore_ascii_case(&segment.name) {
            return Err(ArtifactBuildError::new(
                "format=hunk fixup source section does not match emitted segment",
                Some(segment.name.clone()),
            ));
        }
        if fixup.kind != OutputFixupKind::Abs32 || !fixup.supports_hunk_reloc32() {
            return Err(ArtifactBuildError::new(
                "format=hunk only supports HUNK_RELOC32 records in v0.2",
                Some(segment.name.clone()),
            ));
        }
        let Some(target_section) = fixup.target_section_name() else {
            return Err(ArtifactBuildError::new(
                "format=hunk fixup target is not a section",
                Some(segment.name.clone()),
            ));
        };
        let Some(target_index) = all_segments
            .iter()
            .position(|target| target.name.eq_ignore_ascii_case(target_section))
        else {
            return Err(ArtifactBuildError::new(
                "format=hunk relocation references unknown target section",
                Some(target_section.to_string()),
            ));
        };
        if fixup.offset > payload_len.saturating_sub(4) {
            return Err(ArtifactBuildError::new(
                "format=hunk relocation offset exceeds initialized payload",
                Some(segment.name.clone()),
            ));
        }
        grouped_offsets
            .entry(u32::try_from(target_index).unwrap_or(u32::MAX))
            .or_default()
            .push(fixup.offset);
    }

    let mut grouped_entries: Vec<(u32, Vec<u32>)> = grouped_offsets.into_iter().collect();
    grouped_entries.sort_by_key(|(target_index, _)| *target_index);

    push_be_u32(bytes, HUNK_RELOC32);
    for (target_index, mut offsets) in grouped_entries {
        offsets.sort_unstable();
        push_be_u32(
            bytes,
            u32::try_from(offsets.len()).map_err(|_| {
                ArtifactBuildError::new(
                    "format=hunk relocation count exceeds supported range",
                    Some(segment.name.clone()),
                )
            })?,
        );
        push_be_u32(bytes, target_index);
        for offset in offsets {
            push_be_u32(bytes, offset);
        }
    }
    push_be_u32(bytes, 0);
    Ok(())
}

fn hunk_kind_word(kind: SectionKind) -> u32 {
    match kind {
        SectionKind::Code => HUNK_CODE,
        SectionKind::Data => HUNK_DATA,
        SectionKind::Bss => HUNK_BSS,
    }
}

fn push_be_u32(bytes: &mut Vec<u8>, value: u32) {
    bytes.extend_from_slice(&value.to_be_bytes());
}

fn longword_count(byte_len: u32) -> Result<u32, ArtifactBuildError> {
    let padded = byte_len.checked_add(3).ok_or_else(|| {
        ArtifactBuildError::new("format=hunk size arithmetic overflow", None::<String>)
    })?;
    Ok(padded / 4)
}

fn padded_byte_len(byte_len: usize) -> Result<usize, ArtifactBuildError> {
    let rem = byte_len % 4;
    if rem == 0 {
        Ok(byte_len)
    } else {
        byte_len.checked_add(4 - rem).ok_or_else(|| {
            ArtifactBuildError::new("format=hunk size arithmetic overflow", None::<String>)
        })
    }
}
