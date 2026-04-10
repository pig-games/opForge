// SPDX-License-Identifier: GPL-3.0-or-later

use std::collections::{BTreeMap, HashMap};
use std::path::{Component, Path, PathBuf};

use opcore::tokenizer::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SectionKind {
    #[default]
    Code,
    Data,
    Bss,
}

#[derive(Debug, Clone)]
pub struct SectionState {
    pub start_pc: u32,
    pub pc: u32,
    pub max_pc: u32,
    pub bytes: Vec<u8>,
    pub emitted: bool,
    pub layout_placed: bool,
    pub align: u32,
    pub kind: SectionKind,
    pub hunk_memory_type: HunkMemoryType,
    pub default_region: Option<String>,
    pub base_addr: Option<u32>,
    pub relocation_free_certified: bool,
    pub hunk_relocation_compatible: bool,
    pub hunk_fixup_error: Option<String>,
    pub output_fixups: Vec<OutputFixupRecord>,
}

impl Default for SectionState {
    fn default() -> Self {
        Self {
            start_pc: 0,
            pc: 0,
            max_pc: 0,
            bytes: Vec::new(),
            emitted: false,
            layout_placed: false,
            align: 0,
            kind: SectionKind::Code,
            hunk_memory_type: HunkMemoryType::Any,
            default_region: None,
            base_addr: None,
            relocation_free_certified: true,
            hunk_relocation_compatible: true,
            hunk_fixup_error: None,
            output_fixups: Vec::new(),
        }
    }
}

impl SectionState {
    #[must_use]
    pub fn size_bytes(&self) -> u32 {
        self.max_pc
    }

    #[must_use]
    pub fn is_bss(&self) -> bool {
        self.kind == SectionKind::Bss
    }
}

#[derive(Debug, Default, Clone)]
pub struct SectionOptions {
    pub align: Option<u32>,
    pub kind: Option<SectionKind>,
    pub hunk_memory_type: Option<HunkMemoryType>,
    pub region: Option<String>,
}

#[derive(Debug, Clone)]
pub struct PlacedSectionInfo {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct RegionState {
    pub name: String,
    pub start: u32,
    pub end: u32,
    pub cursor: u32,
    pub align: u32,
    pub placed: Vec<PlacedSectionInfo>,
}

#[derive(Debug, Clone)]
pub struct BinRange {
    pub start_str: String,
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, Clone)]
pub struct BinOutputSpec {
    pub name: Option<String>,
    pub range: Option<BinRange>,
}

#[derive(Debug, Default, Clone)]
pub struct OutputConfig {
    pub name: Option<String>,
    pub list_name: Option<String>,
    pub hex_name: Option<String>,
    pub bin_specs: Vec<BinOutputSpec>,
    pub fill_byte: Option<u8>,
}

impl OutputConfig {
    #[must_use]
    pub fn merge_override(&self, override_cfg: Option<&OutputConfig>) -> OutputConfig {
        let mut merged = self.clone();
        let Some(override_cfg) = override_cfg else {
            return merged;
        };
        if override_cfg.name.is_some() {
            merged.name = override_cfg.name.clone();
        }
        if override_cfg.list_name.is_some() {
            merged.list_name = override_cfg.list_name.clone();
        }
        if override_cfg.hex_name.is_some() {
            merged.hex_name = override_cfg.hex_name.clone();
        }
        if !override_cfg.bin_specs.is_empty() {
            merged.bin_specs = override_cfg.bin_specs.clone();
        }
        if override_cfg.fill_byte.is_some() {
            merged.fill_byte = override_cfg.fill_byte;
        }
        merged
    }
}

#[derive(Debug, Default, Clone)]
pub struct RootMetadata {
    pub root_module_id: Option<String>,
    pub name: Option<String>,
    pub version: Option<String>,
    pub output_default: OutputConfig,
    pub output_by_target: HashMap<String, OutputConfig>,
    pub linker_outputs: Vec<LinkerOutputDirective>,
    pub mapfiles: Vec<MapFileDirective>,
    pub export_sections: Vec<ExportSectionsDirective>,
}

impl RootMetadata {
    #[must_use]
    pub fn output_config_for_cpu(&self, cpu_name: &str) -> OutputConfig {
        let key = cpu_name.to_ascii_lowercase();
        let override_cfg = self.output_by_target.get(&key);
        self.output_default.merge_override(override_cfg)
    }

    pub fn output_config_mut(&mut self, target: Option<&str>) -> &mut OutputConfig {
        if let Some(target) = target {
            let key = target.to_ascii_lowercase();
            return self.output_by_target.entry(key).or_default();
        }
        &mut self.output_default
    }
}

#[derive(Debug, Clone)]
pub enum PlacementDirective {
    Place {
        section: String,
        region: String,
        align: Option<u32>,
        span: Span,
    },
    Pack {
        region: String,
        sections: Vec<String>,
        span: Span,
    },
}

impl PlacementDirective {
    #[must_use]
    pub fn line(&self) -> u32 {
        match self {
            Self::Place { span, .. } | Self::Pack { span, .. } => span.line,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum MapSymbolsMode {
    All,
    Public,
    #[default]
    None,
}

#[derive(Debug, Clone)]
pub struct MapFileDirective {
    pub path: String,
    pub symbols: MapSymbolsMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ExportSectionsFormat {
    #[default]
    Bin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ExportSectionsInclude {
    Bss,
    #[default]
    NoBss,
}

#[derive(Debug, Clone)]
pub struct ExportSectionsDirective {
    pub dir: String,
    pub format: ExportSectionsFormat,
    pub include: ExportSectionsInclude,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LinkerOutputFormat {
    #[default]
    Bin,
    Prg,
    Hunk,
}

impl LinkerOutputFormat {
    #[must_use]
    pub fn format_id(self) -> &'static str {
        match self {
            Self::Bin => "bin",
            Self::Prg => "prg",
            Self::Hunk => "hunk",
        }
    }

    #[must_use]
    pub fn from_format_id(format_id: &str) -> Option<Self> {
        if format_id.eq_ignore_ascii_case("bin") {
            Some(Self::Bin)
        } else if format_id.eq_ignore_ascii_case("prg") {
            Some(Self::Prg)
        } else if format_id.eq_ignore_ascii_case("hunk") {
            Some(Self::Hunk)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LinkerOutputRelocationDisposition {
    #[default]
    Unknown,
    ProvenRelocationFree,
    RelocationRecordsPresent,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum HunkMemoryType {
    #[default]
    Any,
    Chip,
    Fast,
    Slow,
}

impl HunkMemoryType {
    #[must_use]
    pub fn segment_bits(self) -> u32 {
        match self {
            Self::Any => 0,
            Self::Chip => 0x4000_0000,
            Self::Fast => 0x8000_0000,
            Self::Slow => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFixupKind {
    Abs32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OutputFixupTarget {
    Section(String),
}

impl OutputFixupTarget {
    #[must_use]
    pub fn section_name(&self) -> Option<&str> {
        match self {
            Self::Section(name) => Some(name.as_str()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct OutputFixupCompatibility {
    pub hunk_reloc32: bool,
}

impl OutputFixupCompatibility {
    #[must_use]
    pub const fn hunk_reloc32() -> Self {
        Self { hunk_reloc32: true }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OutputFixupRecord {
    pub source_section: String,
    pub offset: u32,
    pub kind: OutputFixupKind,
    pub target: OutputFixupTarget,
    pub encoded_addend: u32,
    pub compatibility: OutputFixupCompatibility,
}

impl OutputFixupRecord {
    #[must_use]
    pub fn hunk_abs32(
        source_section: String,
        offset: u32,
        encoded_addend: u32,
        target_section: String,
    ) -> Self {
        Self {
            source_section,
            offset,
            kind: OutputFixupKind::Abs32,
            target: OutputFixupTarget::Section(target_section),
            encoded_addend,
            compatibility: OutputFixupCompatibility::hunk_reloc32(),
        }
    }

    #[must_use]
    pub fn supports_hunk_reloc32(&self) -> bool {
        self.compatibility.hunk_reloc32
    }

    #[must_use]
    pub fn target_section_name(&self) -> Option<&str> {
        self.target.section_name()
    }
}

#[derive(Debug, Clone)]
pub struct HunkSegmentInput {
    pub name: String,
    pub kind: SectionKind,
    pub initialized_bytes: Vec<u8>,
    pub allocation_size_bytes: u32,
    pub memory_type: HunkMemoryType,
    pub fixups: Vec<OutputFixupRecord>,
}

#[derive(Debug, Clone)]
pub struct HunkOutputInput {
    pub segments: Vec<HunkSegmentInput>,
    pub relocation_disposition: LinkerOutputRelocationDisposition,
}

pub const IMPLICIT_HUNK_CODE_SECTION_NAME: &str = "__opforge_implicit_code";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LinkerOutputOptionValue {
    Text(String),
    TextList(Vec<String>),
}

impl LinkerOutputOptionValue {
    #[must_use]
    pub fn as_text(&self) -> Option<&str> {
        match self {
            Self::Text(value) => Some(value.as_str()),
            Self::TextList(_) => None,
        }
    }

    #[must_use]
    pub fn as_text_list(&self) -> Option<&[String]> {
        match self {
            Self::Text(_) => None,
            Self::TextList(values) => Some(values.as_slice()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LinkerOutputDirective {
    pub path: String,
    pub format_id: String,
    pub options: BTreeMap<String, LinkerOutputOptionValue>,
    pub relocation_disposition: LinkerOutputRelocationDisposition,
}

impl LinkerOutputDirective {
    #[must_use]
    pub fn format(&self) -> Option<LinkerOutputFormat> {
        LinkerOutputFormat::from_format_id(&self.format_id)
    }

    #[must_use]
    pub fn option(&self, key: &str) -> Option<&LinkerOutputOptionValue> {
        self.options.get(&key.to_ascii_lowercase())
    }

    #[must_use]
    pub fn option_text(&self, key: &str) -> Option<&str> {
        self.option(key).and_then(LinkerOutputOptionValue::as_text)
    }

    #[must_use]
    pub fn option_text_list(&self, key: &str) -> Option<&[String]> {
        self.option(key)
            .and_then(LinkerOutputOptionValue::as_text_list)
    }
}

pub fn is_valid_hex_2(s: &str) -> bool {
    s.len() == 2 && s.chars().all(|c| c.is_ascii_hexdigit())
}

fn is_valid_hex_4_to_8(s: &str) -> bool {
    (4..=8).contains(&s.len()) && s.chars().all(|c| c.is_ascii_hexdigit())
}

fn is_valid_bin_range(s: &str) -> bool {
    let Some((start, end)) = s.split_once(':') else {
        return false;
    };
    if end.contains(':') {
        return false;
    }
    is_valid_hex_4_to_8(start) && is_valid_hex_4_to_8(end)
}

fn split_range_suffix(s: &str) -> Option<(&str, &str, &str)> {
    let mut parts = s.rsplitn(3, ':');
    let end = parts.next()?;
    let start = parts.next()?;
    let name = parts.next()?;
    if is_valid_hex_4_to_8(start) && is_valid_hex_4_to_8(end) {
        Some((name, start, end))
    } else {
        None
    }
}

fn parse_bin_range_parts(start: &str, end: &str) -> Option<BinRange> {
    if !is_valid_hex_4_to_8(start) || !is_valid_hex_4_to_8(end) {
        return None;
    }
    let start_str = start.to_string();
    let end_str = end.to_string();
    let start = u32::from_str_radix(&start_str, 16).ok()?;
    let end = u32::from_str_radix(&end_str, 16).ok()?;
    if start > end {
        return None;
    }
    Some(BinRange {
        start_str,
        start,
        end,
    })
}

pub fn parse_bin_output_arg(arg: &str) -> Result<BinOutputSpec, &'static str> {
    const RANGE_ERR: &str =
        "Invalid -b/--bin range; must be ssss:eeee (4-8 hex digits, start <= end)";

    if arg.is_empty() {
        return Ok(BinOutputSpec {
            name: None,
            range: None,
        });
    }

    if let Some(range) = parse_bin_range_str(arg) {
        return Ok(BinOutputSpec {
            name: None,
            range: Some(range),
        });
    }

    if let Some((name_part, start, end)) = split_range_suffix(arg) {
        let range = parse_bin_range_parts(start, end).ok_or(RANGE_ERR)?;
        let name = if name_part.is_empty() {
            None
        } else {
            Some(name_part.to_string())
        };
        return Ok(BinOutputSpec {
            name,
            range: Some(range),
        });
    }

    if !arg.contains(':') {
        return Ok(BinOutputSpec {
            name: Some(arg.to_string()),
            range: None,
        });
    }

    if is_valid_bin_range(arg) {
        return Err(RANGE_ERR);
    }

    Err("Invalid -b/--bin argument; use ssss:eeee, name:ssss:eeee, or name only (4-8 hex digits)")
}

pub fn parse_bin_range_str(s: &str) -> Option<BinRange> {
    if !is_valid_bin_range(s) {
        return None;
    }
    let (start_text, end_text) = s.split_once(':')?;
    let start_str = start_text.to_string();
    let end_str = end_text.to_string();
    let start = u32::from_str_radix(&start_str, 16).ok()?;
    let end = u32::from_str_radix(&end_str, 16).ok()?;
    if start > end {
        return None;
    }
    Some(BinRange {
        start_str,
        start,
        end,
    })
}

#[must_use]
pub fn resolve_bin_path(
    base: &str,
    name: Option<&str>,
    range: Option<&BinRange>,
    bin_count: usize,
    index: usize,
) -> String {
    let name = match name {
        Some(name) if !name.is_empty() => name.to_string(),
        _ => {
            if bin_count == 1 {
                base.to_string()
            } else if let Some(range) = range {
                format!("{base}-{}", range.start_str)
            } else {
                format!("{base}-{}", index + 1)
            }
        }
    };
    let path = PathBuf::from(&name);
    if path.extension().is_none() {
        return format!("{name}.bin");
    }
    name
}

pub fn resolve_output_path(base: &str, name: Option<String>, extension: &str) -> Option<String> {
    let name = name?;
    if name.is_empty() {
        return Some(format!("{base}.{extension}"));
    }
    let mut path = PathBuf::from(&name);
    if path.extension().is_none() {
        path = PathBuf::from(format!("{name}.{extension}"));
    }
    if path.is_relative() {
        if let Some(parent) = std::path::Path::new(base).parent() {
            if !parent.as_os_str().is_empty() {
                path = parent.join(path);
            }
        }
    }
    Some(path.to_string_lossy().to_string())
}

fn normalize_path(path: &Path) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::Prefix(prefix) => normalized.push(prefix.as_os_str()),
            Component::RootDir => normalized.push(Path::new("/")),
            Component::CurDir => {}
            Component::ParentDir => {
                if path.is_absolute() {
                    if matches!(
                        normalized.components().next_back(),
                        Some(Component::Normal(_))
                    ) {
                        normalized.pop();
                    }
                } else {
                    match normalized.components().next_back() {
                        Some(Component::Normal(_)) => {
                            normalized.pop();
                        }
                        Some(Component::ParentDir) | None => normalized.push(".."),
                        Some(Component::RootDir) | Some(Component::Prefix(_)) => {}
                        Some(Component::CurDir) => {}
                    }
                }
            }
            Component::Normal(part) => normalized.push(part),
        }
    }
    if normalized.as_os_str().is_empty() && path.is_absolute() {
        PathBuf::from("/")
    } else {
        normalized
    }
}

fn path_depth(path: &Path) -> usize {
    path.components()
        .filter(|component| matches!(component, Component::Normal(_) | Component::ParentDir))
        .count()
}

pub fn anchor_relative_output_path(root: &Path, path: &Path) -> Result<PathBuf, String> {
    if path.is_absolute() {
        return Ok(path.to_path_buf());
    }

    let mut anchored = normalize_path(root);
    let min_depth = path_depth(&anchored);
    let mut depth = min_depth;

    for component in path.components() {
        match component {
            Component::Prefix(_) | Component::RootDir => return Ok(path.to_path_buf()),
            Component::CurDir => {}
            Component::ParentDir => {
                if depth == min_depth {
                    return Err(format!(
                        "Output path escapes resolved output root: {}",
                        path.display()
                    ));
                }
                anchored.pop();
                depth -= 1;
            }
            Component::Normal(part) => {
                anchored.push(part);
                depth += 1;
            }
        }
    }

    Ok(anchored)
}

pub fn resolve_output_path_checked(
    base: &str,
    name: Option<String>,
    extension: &str,
) -> Result<Option<String>, String> {
    let Some(name) = name else {
        return Ok(None);
    };
    if name.is_empty() {
        return Ok(Some(format!("{base}.{extension}")));
    }

    let mut path = PathBuf::from(&name);
    if path.extension().is_none() {
        path = PathBuf::from(format!("{name}.{extension}"));
    }
    if path.is_relative() {
        let root = Path::new(base)
            .parent()
            .filter(|parent| !parent.as_os_str().is_empty())
            .unwrap_or(Path::new("."));
        path = anchor_relative_output_path(root, &path)?;
    }
    Ok(Some(path.to_string_lossy().to_string()))
}

pub fn resolve_bin_path_checked(
    base: &str,
    name: Option<&str>,
    range: Option<&BinRange>,
    bin_count: usize,
    index: usize,
) -> Result<String, String> {
    let raw = resolve_bin_path(base, name, range, bin_count, index);
    let Some(name) = name.filter(|name| !name.is_empty()) else {
        return Ok(raw);
    };

    let mut path = PathBuf::from(name);
    if path.extension().is_none() {
        path = PathBuf::from(format!("{name}.bin"));
    }
    if path.is_relative() {
        let root = Path::new(base)
            .parent()
            .filter(|parent| !parent.as_os_str().is_empty())
            .unwrap_or(Path::new("."));
        path = anchor_relative_output_path(root, &path)?;
        return Ok(path.to_string_lossy().to_string());
    }

    Ok(raw)
}

#[must_use]
pub fn resolve_output_base(
    input_base: &str,
    out_dir: Option<&Path>,
    metadata: &RootMetadata,
    cpu_name: &str,
    outfile_override: Option<&str>,
) -> String {
    let output_config = metadata.output_config_for_cpu(cpu_name);
    let mut base = if out_dir.is_some() {
        input_base.to_string()
    } else if let Some(outfile) = outfile_override {
        outfile.to_string()
    } else if let Some(output) = output_config.name.as_deref() {
        output.to_string()
    } else {
        input_base.to_string()
    };

    if let Some(dir) = out_dir {
        let name = Path::new(&base)
            .file_name()
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_else(|| base.clone());
        base = dir.join(name).to_string_lossy().to_string();
    }

    base
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_metadata() -> RootMetadata {
        RootMetadata::default()
    }

    fn metadata_with_output_name(name: &str) -> RootMetadata {
        let mut meta = RootMetadata::default();
        meta.output_default.name = Some(name.to_string());
        meta
    }

    // --- resolve_output_base ---

    #[test]
    fn resolve_output_base_relative_input_no_out_dir_uses_input() {
        let meta = empty_metadata();
        let result = resolve_output_base("myfile", None, &meta, "8085", None);
        assert_eq!(result, "myfile");
    }

    #[test]
    fn resolve_output_base_absolute_input_no_out_dir_uses_input() {
        let meta = empty_metadata();
        let result = resolve_output_base("/abs/path/myfile", None, &meta, "8085", None);
        assert_eq!(result, "/abs/path/myfile");
    }

    #[test]
    fn resolve_output_base_relative_input_with_out_dir_produces_dir_plus_stem() {
        let meta = empty_metadata();
        let out_dir = PathBuf::from("/output/dir");
        let result = resolve_output_base("myfile", Some(&out_dir), &meta, "8085", None);
        assert_eq!(result, "/output/dir/myfile");
    }

    #[test]
    fn resolve_output_base_absolute_input_with_out_dir_rewrites_directory_only() {
        let meta = empty_metadata();
        let out_dir = PathBuf::from("/output/dir");
        // Key contract: out_dir replaces the directory portion; file name is preserved.
        let result = resolve_output_base(
            "/absolute/path/to/myfile",
            Some(&out_dir),
            &meta,
            "8085",
            None,
        );
        assert_eq!(result, "/output/dir/myfile");
    }

    #[test]
    fn resolve_output_base_absolute_input_with_out_dir_overrides_metadata_name() {
        let meta = metadata_with_output_name("from_meta");
        let out_dir = PathBuf::from("/out");
        // When out_dir is present, input_base is the selected base (metadata name is not used).
        let result = resolve_output_base("/abs/prog", Some(&out_dir), &meta, "8085", None);
        assert_eq!(result, "/out/prog");
    }

    #[test]
    fn resolve_output_base_no_out_dir_prefers_outfile_override() {
        let meta = empty_metadata();
        let result = resolve_output_base("default_base", None, &meta, "8085", Some("override_out"));
        assert_eq!(result, "override_out");
    }

    #[test]
    fn resolve_output_base_no_out_dir_prefers_metadata_over_input() {
        let meta = metadata_with_output_name("meta_name");
        let result = resolve_output_base("input_base", None, &meta, "8085", None);
        assert_eq!(result, "meta_name");
    }

    #[test]
    fn resolve_output_base_relative_subdir_input_with_out_dir_uses_filename_only() {
        let meta = empty_metadata();
        let out_dir = PathBuf::from("/out");
        // Only the final component of input_base is kept; the subdirectory is rewritten.
        let result = resolve_output_base("subdir/myfile", Some(&out_dir), &meta, "8085", None);
        assert_eq!(result, "/out/myfile");
    }

    #[test]
    fn anchor_relative_output_path_keeps_nested_paths_within_root() {
        let resolved =
            anchor_relative_output_path(Path::new("/virtual/out"), Path::new("build/main.hex"))
                .expect("nested output path should remain within root");

        assert_eq!(resolved, PathBuf::from("/virtual/out/build/main.hex"));
    }

    #[test]
    fn anchor_relative_output_path_rejects_escape_from_root() {
        let error =
            anchor_relative_output_path(Path::new("/virtual/out"), Path::new("../main.hex"))
                .expect_err("escape should be rejected");

        assert!(error.contains("escapes resolved output root"));
    }

    #[test]
    fn resolve_output_path_checked_rejects_parent_escape() {
        let error = resolve_output_path_checked("src/main", Some("../out".to_string()), "hex")
            .expect_err("path escape should be rejected");

        assert!(error.contains("escapes resolved output root"));
    }

    #[test]
    fn resolve_bin_path_checked_anchors_relative_explicit_name() {
        let resolved =
            resolve_bin_path_checked("/virtual/out/main", Some("build/image"), None, 1, 0)
                .expect("relative bin path should be anchored");

        assert_eq!(resolved, "/virtual/out/build/image.bin");
    }

    #[test]
    fn resolve_bin_path_checked_rejects_parent_escape() {
        let error = resolve_bin_path_checked("/virtual/out/main", Some("../image"), None, 1, 0)
            .expect_err("bin path escape should be rejected");

        assert!(error.contains("escapes resolved output root"));
    }
}
