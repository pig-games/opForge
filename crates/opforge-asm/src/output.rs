// SPDX-License-Identifier: GPL-3.0-or-later

//! Output model facade over partitioned `.opasm` artifact stages.

use std::collections::HashMap;

pub use types::artifacts::{
    format_addr, render_dependencies, render_labels, DependencyOutputPolicy, LabelOutputFormat,
    OutputFormat,
};
pub use vm::output_model::{
    anchor_relative_output_path, is_valid_hex_2, resolve_bin_path_checked,
    resolve_output_path_checked,
};
pub use vm::vm_opasm::{
    build_export_sections_payloads, build_mapfile_text, parse_bin_output_arg, parse_bin_range_str,
    resolve_bin_path, resolve_output_base, resolve_output_path, BinOutputSpec, BinRange,
    ExportSectionsDirective, ExportSectionsFormat, ExportSectionsInclude, LinkerOutputDirective,
    LinkerOutputFormat, MapFileDirective, MapSymbolsMode, OutputConfig, PlacedSectionInfo,
    PlacementDirective, RegionState, RootMetadata, SectionKind, SectionOptions, SectionState,
};

use crate::error::{AsmError, AsmErrorKind};

pub fn section_kind_name(kind: SectionKind) -> &'static str {
    match kind {
        SectionKind::Code => "code",
        SectionKind::Data => "data",
        SectionKind::Bss => "bss",
    }
}

pub fn build_linker_output_payload(
    output: &LinkerOutputDirective,
    sections: &HashMap<String, SectionState>,
) -> Result<Vec<u8>, AsmError> {
    vm::vm_opasm::build_linker_output_payload(output, sections)
        .map_err(|err| AsmError::new(AsmErrorKind::Directive, err.message(), err.subject()))
}
