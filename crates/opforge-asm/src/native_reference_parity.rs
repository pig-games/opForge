use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

pub(crate) const NATIVE_REFERENCE_FIXTURE_PATH: &str =
    "crates/opforge-asm/tests/fixtures/native_cli_reference_parity_schema.json";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum NativeReferenceSourceMode {
    SourceBinFromExample,
    SourceCpuPrgFromExample,
}

impl NativeReferenceSourceMode {
    fn from_fixture_str(value: &str) -> Result<Self, String> {
        match value {
            "source-bin-from-example" => Ok(Self::SourceBinFromExample),
            "source-cpu-prg-from-example" => Ok(Self::SourceCpuPrgFromExample),
            other => Err(format!(
                "unsupported native reference source_mode '{other}'; expected 'source-bin-from-example' or 'source-cpu-prg-from-example'"
            )),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct NativeReferenceCase {
    pub(crate) asm_path: String,
    pub(crate) cpu_id: String,
    pub(crate) source_mode: NativeReferenceSourceMode,
    pub(crate) command_template: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum NativeReferencePathMatcher {
    Exact(&'static str),
    Prefix(&'static str),
}

impl NativeReferencePathMatcher {
    fn matches(self, path: &str) -> bool {
        match self {
            Self::Exact(expected) => path == expected,
            Self::Prefix(prefix) => path.starts_with(prefix),
        }
    }

    fn specificity(self) -> (u8, usize) {
        match self {
            Self::Exact(path) => (2, path.len()),
            Self::Prefix(prefix) => (1, prefix.len()),
        }
    }

    fn describe(self) -> &'static str {
        match self {
            Self::Exact(path) | Self::Prefix(path) => path,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct NativeReferenceExclusionRule {
    pub(crate) matcher: NativeReferencePathMatcher,
    pub(crate) reason: &'static str,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum NativeReferenceAccounting<'a> {
    Case(&'a NativeReferenceCase),
    Opcore(&'a NativeOpcoreAssignment),
    Excluded(&'a NativeReferenceExclusionRule),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum NativeOpcoreShard {
    SyntaxExpression,
    ModuleMacroStatement,
    LayoutOutput,
    Diagnostic,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum NativeOpcoreStaging {
    DirectCpuNeutral,
    AdditiveMosAdaptation,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum NativeOpcoreRole {
    Root { reference_stem: &'static str },
    Support { owner: &'static str },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct NativeOpcoreAssignment {
    pub(crate) source_path: &'static str,
    pub(crate) shard: NativeOpcoreShard,
    pub(crate) staging: NativeOpcoreStaging,
    pub(crate) role: NativeOpcoreRole,
}

macro_rules! opcore_root {
    ($path:literal, $shard:ident, $staging:ident) => {
        NativeOpcoreAssignment {
            source_path: concat!("examples/opcore/", $path, ".asm"),
            shard: NativeOpcoreShard::$shard,
            staging: NativeOpcoreStaging::$staging,
            role: NativeOpcoreRole::Root {
                reference_stem: $path,
            },
        }
    };
    ($source:literal => $stem:literal, $shard:ident, $staging:ident) => {
        NativeOpcoreAssignment {
            source_path: concat!("examples/opcore/", $source),
            shard: NativeOpcoreShard::$shard,
            staging: NativeOpcoreStaging::$staging,
            role: NativeOpcoreRole::Root {
                reference_stem: $stem,
            },
        }
    };
}

macro_rules! opcore_support {
    ($path:literal => $owner:literal, $shard:ident, $staging:ident) => {
        NativeOpcoreAssignment {
            source_path: concat!("examples/opcore/", $path),
            shard: NativeOpcoreShard::$shard,
            staging: NativeOpcoreStaging::$staging,
            role: NativeOpcoreRole::Support {
                owner: concat!("examples/opcore/", $owner),
            },
        }
    };
}

// This is intentionally an exact-path inventory. Adding a source under
// examples/opcore must fail the inventory test until its applicability,
// staging mode, owning root, and one Item 6-9 shard have been reviewed.
pub(crate) const NATIVE_OPCORE_ASSIGNMENTS: &[NativeOpcoreAssignment] = &[
    opcore_root!(
        "bfor_labeled_struct_basic",
        SyntaxExpression,
        DirectCpuNeutral
    ),
    opcore_root!("cond_syntax", SyntaxExpression, DirectCpuNeutral),
    opcore_root!("expr_syntax", SyntaxExpression, AdditiveMosAdaptation),
    opcore_root!("for_collection_basic", SyntaxExpression, DirectCpuNeutral),
    opcore_root!("for_counter_basic", SyntaxExpression, DirectCpuNeutral),
    opcore_root!("grouping", SyntaxExpression, AdditiveMosAdaptation),
    opcore_root!("ranges_lists_basic", SyntaxExpression, DirectCpuNeutral),
    opcore_root!("scopes", SyntaxExpression, DirectCpuNeutral),
    opcore_root!("scopes_namespace", SyntaxExpression, DirectCpuNeutral),
    opcore_root!(
        "struct_literal_instance_basic",
        SyntaxExpression,
        DirectCpuNeutral
    ),
    opcore_root!(
        "struct_var_instance_basic",
        SyntaxExpression,
        DirectCpuNeutral
    ),
    opcore_root!("syntax", SyntaxExpression, AdditiveMosAdaptation),
    opcore_root!("testexpr", SyntaxExpression, AdditiveMosAdaptation),
    opcore_root!("text_encoding", SyntaxExpression, DirectCpuNeutral),
    opcore_root!(
        "text_encoding_definitions",
        SyntaxExpression,
        DirectCpuNeutral
    ),
    opcore_root!("while_basic", SyntaxExpression, DirectCpuNeutral),
    opcore_root!(
        "macro_cross_module_ok",
        ModuleMacroStatement,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "macro_invocation_native",
        ModuleMacroStatement,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "macro_segment_syntax",
        ModuleMacroStatement,
        AdditiveMosAdaptation
    ),
    opcore_root!("macro_syntax", ModuleMacroStatement, DirectCpuNeutral),
    opcore_root!("module_basics", ModuleMacroStatement, DirectCpuNeutral),
    opcore_root!("module_use", ModuleMacroStatement, DirectCpuNeutral),
    opcore_root!(
        "module_use_autoload",
        ModuleMacroStatement,
        DirectCpuNeutral
    ),
    opcore_root!("module_use_include", ModuleMacroStatement, DirectCpuNeutral),
    opcore_root!("module_visibility", ModuleMacroStatement, DirectCpuNeutral),
    opcore_root!("preproc_syntax", ModuleMacroStatement, DirectCpuNeutral),
    opcore_root!("project_root/main.asm" => "project_root-main", ModuleMacroStatement, DirectCpuNeutral),
    opcore_root!(
        "statement_boundary_span",
        ModuleMacroStatement,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "statement_capture_types",
        ModuleMacroStatement,
        DirectCpuNeutral
    ),
    opcore_root!(
        "statement_cross_module_ok",
        ModuleMacroStatement,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "statement_expansion",
        ModuleMacroStatement,
        DirectCpuNeutral
    ),
    opcore_root!(
        "statement_signatures",
        ModuleMacroStatement,
        DirectCpuNeutral
    ),
    opcore_root!(
        "use_wildcard_import",
        ModuleMacroStatement,
        AdditiveMosAdaptation
    ),
    opcore_support!("lib/example_autoload_lib.asm" => "module_use_autoload.asm", ModuleMacroStatement, DirectCpuNeutral),
    opcore_support!("lib/macro_export_lib.asm" => "macro_cross_module_ok.asm", ModuleMacroStatement, AdditiveMosAdaptation),
    opcore_support!("lib/statement_export_lib.asm" => "statement_cross_module_ok.asm", ModuleMacroStatement, AdditiveMosAdaptation),
    opcore_support!("module_use_lib.inc" => "module_use_include.asm", ModuleMacroStatement, DirectCpuNeutral),
    opcore_support!("preproc_syntax.inc" => "preproc_syntax.asm", ModuleMacroStatement, DirectCpuNeutral),
    opcore_support!("project_root/util.asm" => "project_root/main.asm", ModuleMacroStatement, DirectCpuNeutral),
    opcore_root!("align_simple", LayoutOutput, DirectCpuNeutral),
    opcore_root!("cli_json_outputs", LayoutOutput, DirectCpuNeutral),
    opcore_root!("led1", LayoutOutput, AdditiveMosAdaptation),
    opcore_root!("linker_regions_full", LayoutOutput, DirectCpuNeutral),
    opcore_root!("linker_regions_minimal", LayoutOutput, DirectCpuNeutral),
    opcore_root!("linker_regions_no_dsection", LayoutOutput, DirectCpuNeutral),
    opcore_root!(
        "linker_regions_pack_no_dsection",
        LayoutOutput,
        DirectCpuNeutral
    ),
    opcore_root!("module_metadata_block", LayoutOutput, DirectCpuNeutral),
    opcore_root!("module_metadata_output", LayoutOutput, DirectCpuNeutral),
    opcore_root!("module_metadata_outputs", LayoutOutput, DirectCpuNeutral),
    opcore_root!(
        "module_qualified_section_map",
        LayoutOutput,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "section_module_use_autoload",
        LayoutOutput,
        DirectCpuNeutral
    ),
    opcore_root!("section_module_use_include", LayoutOutput, DirectCpuNeutral),
    opcore_root!("section_simple", LayoutOutput, DirectCpuNeutral),
    opcore_root!(
        "segment_cross_module_ok",
        LayoutOutput,
        AdditiveMosAdaptation
    ),
    opcore_root!("sertest", LayoutOutput, AdditiveMosAdaptation),
    opcore_support!("cli_json_outputs.inc" => "cli_json_outputs.asm", LayoutOutput, DirectCpuNeutral),
    opcore_support!("lib/example_section_lib.asm" => "section_module_use_autoload.asm", LayoutOutput, DirectCpuNeutral),
    opcore_support!("lib/segment_export_lib.asm" => "segment_cross_module_ok.asm", LayoutOutput, AdditiveMosAdaptation),
    opcore_support!("section_module_use_lib.inc" => "section_module_use_include.asm", LayoutOutput, DirectCpuNeutral),
    opcore_root!(
        "conditional_missing_endif_fixit_error",
        Diagnostic,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "conditional_unmatched_endif_error",
        Diagnostic,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "dialect_mnemonic_fixit_error",
        Diagnostic,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "dialect_parser_fixit_error",
        Diagnostic,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "directive_typo_elseif_fixit_error",
        Diagnostic,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "directive_typo_endif_fixit_error",
        Diagnostic,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "directive_typo_endmatch_fixit_error",
        Diagnostic,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "directive_typo_endmodule_fixit_error",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!(
        "directive_typo_endsection_fixit_error",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!("errors", Diagnostic, AdditiveMosAdaptation),
    opcore_root!("for_unscoped_label_error", Diagnostic, DirectCpuNeutral),
    opcore_root!("index_out_of_bounds_error", Diagnostic, DirectCpuNeutral),
    opcore_root!(
        "linker_regions_phase6_contiguous_gap",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!(
        "linker_regions_phase6_emit_overflow",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!(
        "linker_regions_phase6_fill_in_bss_error",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!(
        "linker_regions_phase6_image_span_overflow",
        Diagnostic,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "linker_regions_phase6_invalid_section_option_key",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!(
        "linker_regions_phase6_missing_section_option_equals",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!(
        "linker_regions_phase6_region_binding_conflict",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!(
        "linker_regions_phase6_region_overlap",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!(
        "linker_regions_phase6_unknown_region",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!(
        "linker_regions_phase6_unknown_section",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!("loop_pass_instability_error", Diagnostic, DirectCpuNeutral),
    opcore_root!(
        "macro_cross_module_error",
        Diagnostic,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "module_missing_endmodule_error",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!("module_use_private_error", Diagnostic, DirectCpuNeutral),
    opcore_root!(
        "multi_error_reporting_error",
        Diagnostic,
        AdditiveMosAdaptation
    ),
    opcore_root!("range_step_direction_error", Diagnostic, DirectCpuNeutral),
    opcore_root!("range_step_zero_error", Diagnostic, DirectCpuNeutral),
    opcore_root!(
        "section_missing_endsection_error",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!(
        "segment_cross_module_error",
        Diagnostic,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "statement_cross_module_error",
        Diagnostic,
        AdditiveMosAdaptation
    ),
    opcore_root!(
        "statement_private_import_error",
        Diagnostic,
        AdditiveMosAdaptation
    ),
    opcore_root!("statement_signature_error", Diagnostic, DirectCpuNeutral),
    opcore_root!(
        "statement_unquoted_comma_error",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!("while_unscoped_label_error", Diagnostic, DirectCpuNeutral),
    opcore_support!("lib/statement_private_export_lib.asm" => "statement_private_import_error.asm", Diagnostic, AdditiveMosAdaptation),
];

pub(crate) const NATIVE_OPCORE_REFERENCE_EXCLUSIONS: &[(&str, &str)] = &[
    (
        "README.md",
        "documentation only; it is not a CLI-written artifact",
    ),
    (
        "diagnostics_v2_schema.json",
        "shared diagnostic schema authority rather than the output of one source root",
    ),
];

pub(crate) const NATIVE_REFERENCE_EXCLUSION_RULES: &[NativeReferenceExclusionRule] = &[
    NativeReferenceExclusionRule {
        matcher: NativeReferencePathMatcher::Prefix("examples/mos6502/45gs02_"),
        reason:
            "current manifest runner only ships m6502 and 65c02 package-backed native CLI coverage; 45gs02 examples need a 45gs02 package shard before they can be compared honestly",
    },
    NativeReferenceExclusionRule {
        matcher: NativeReferencePathMatcher::Prefix("examples/mos6502/65816_"),
        reason:
            "current manifest runner only ships m6502 and 65c02 package-backed native CLI coverage; 65816 examples need a 65816 package shard before they can be compared honestly",
    },
    NativeReferenceExclusionRule {
        matcher: NativeReferencePathMatcher::Exact(
            "examples/mos6502/mos_forward_ref_stability.asm",
        ),
        reason:
            "this example mixes 6502 and 65c02 source in one file, and the current native reference runner executes one native CLI session per case, so it cannot compare both CPU slices honestly yet",
    },
    NativeReferenceExclusionRule {
        matcher: NativeReferencePathMatcher::Prefix("examples/motorola6800/"),
        reason:
            "the manifest runner does not yet stage the motorola6800 package through the native CLI reference path, so this family remains explicitly excluded in the first slice",
    },
    NativeReferenceExclusionRule {
        matcher: NativeReferencePathMatcher::Prefix("examples/z80/"),
        reason:
            "the manifest runner does not yet stage the z80 package through the native CLI reference path, so this family remains explicitly excluded in the first slice",
    },
    NativeReferenceExclusionRule {
        matcher: NativeReferencePathMatcher::Prefix("examples/motorola68000/amigaos/"),
        reason:
            "these AmigaOS examples currently validate Rust-side hunk/reference behavior, but the manifest runner does not yet compare native CLI outputs for AmigaOS artifact surfaces",
    },
    NativeReferenceExclusionRule {
        matcher: NativeReferencePathMatcher::Prefix("examples/motorola68000/"),
        reason:
            "the manifest runner does not yet stage the motorola68000 package through the native CLI reference path, so this family remains explicitly excluded in the first slice",
    },
];

static NATIVE_REFERENCE_CASES: OnceLock<Vec<NativeReferenceCase>> = OnceLock::new();

fn native_reference_fixture_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join(NATIVE_REFERENCE_FIXTURE_PATH)
}

fn fixture_string_field(
    case_index: usize,
    entry: &serde_json::Map<String, serde_json::Value>,
    field: &str,
    fixture_path: &Path,
) -> Result<String, String> {
    let value = entry.get(field).ok_or_else(|| {
        format!(
            "native reference fixture {} entry {} is missing required field '{}'",
            fixture_path.display(),
            case_index,
            field
        )
    })?;
    let text = value.as_str().ok_or_else(|| {
        format!(
            "native reference fixture {} entry {} field '{}' must be a string",
            fixture_path.display(),
            case_index,
            field
        )
    })?;
    if text.trim().is_empty() {
        return Err(format!(
            "native reference fixture {} entry {} field '{}' must not be empty",
            fixture_path.display(),
            case_index,
            field
        ));
    }
    Ok(text.to_string())
}

fn load_native_reference_cases_from_fixture() -> Result<Vec<NativeReferenceCase>, String> {
    let fixture_path = native_reference_fixture_path();
    let fixture_text = fs::read_to_string(&fixture_path).map_err(|err| {
        format!(
            "read native reference fixture {}: {err}",
            fixture_path.display()
        )
    })?;
    parse_native_reference_cases_fixture(&fixture_path, &fixture_text)
}

fn parse_native_reference_cases_fixture(
    fixture_path: &Path,
    fixture_text: &str,
) -> Result<Vec<NativeReferenceCase>, String> {
    let parsed: serde_json::Value = serde_json::from_str(&fixture_text).map_err(|err| {
        format!(
            "parse native reference fixture {} as JSON: {err}",
            fixture_path.display()
        )
    })?;
    let entries = parsed.as_array().ok_or_else(|| {
        format!(
            "native reference fixture {} must contain a top-level JSON array",
            fixture_path.display()
        )
    })?;
    if entries.is_empty() {
        return Err(format!(
            "native reference fixture {} must declare at least one case",
            fixture_path.display()
        ));
    }

    let mut seen_paths = HashSet::new();
    let mut cases = Vec::with_capacity(entries.len());
    for (case_index, value) in entries.iter().enumerate() {
        let entry = value.as_object().ok_or_else(|| {
            format!(
                "native reference fixture {} entry {} must be an object",
                fixture_path.display(),
                case_index
            )
        })?;
        let asm_path = fixture_string_field(case_index, entry, "asm_path", &fixture_path)?;
        let cpu_id = fixture_string_field(case_index, entry, "cpu_id", &fixture_path)?;
        let source_mode_text =
            fixture_string_field(case_index, entry, "source_mode", &fixture_path)?;
        let command_template =
            fixture_string_field(case_index, entry, "command_template", &fixture_path)?;
        let source_mode = NativeReferenceSourceMode::from_fixture_str(source_mode_text.as_str())
            .map_err(|err| {
                format!(
                    "native reference fixture {} entry {}: {err}",
                    fixture_path.display(),
                    case_index
                )
            })?;
        if !asm_path.starts_with("examples/") {
            return Err(format!(
                "native reference fixture {} entry {} asm_path '{}' must stay under examples/",
                fixture_path.display(),
                case_index,
                asm_path
            ));
        }
        if !seen_paths.insert(asm_path.clone()) {
            return Err(format!(
                "native reference fixture {} contains duplicate asm_path '{}'",
                fixture_path.display(),
                asm_path
            ));
        }
        cases.push(NativeReferenceCase {
            asm_path,
            cpu_id,
            source_mode,
            command_template,
        });
    }

    Ok(cases)
}

pub(crate) fn native_reference_cases() -> &'static [NativeReferenceCase] {
    NATIVE_REFERENCE_CASES
        .get_or_init(|| {
            load_native_reference_cases_from_fixture().unwrap_or_else(|err| panic!("{err}"))
        })
        .as_slice()
}

pub(crate) fn native_reference_case_for_path(path: &str) -> Option<&'static NativeReferenceCase> {
    native_reference_cases()
        .iter()
        .find(|case| case.asm_path == path)
}

pub(crate) fn account_native_reference_path(
    path: &str,
) -> Result<NativeReferenceAccounting<'static>, String> {
    if let Some(case) = native_reference_case_for_path(path) {
        return Ok(NativeReferenceAccounting::Case(case));
    }
    if let Some(assignment) = NATIVE_OPCORE_ASSIGNMENTS
        .iter()
        .find(|assignment| assignment.source_path == path)
    {
        return Ok(NativeReferenceAccounting::Opcore(assignment));
    }

    let mut matches = NATIVE_REFERENCE_EXCLUSION_RULES
        .iter()
        .filter(|rule| rule.matcher.matches(path))
        .collect::<Vec<_>>();
    if matches.is_empty() {
        return Err(format!(
            "native reference example '{}' is not represented by a parity case or exclusion rule",
            path
        ));
    }

    matches.sort_by_key(|rule| rule.matcher.specificity());
    matches.reverse();
    let best = matches[0];
    if matches.len() > 1 && matches[1].matcher.specificity() == best.matcher.specificity() {
        return Err(format!(
            "native reference example '{}' matched multiple equally specific exclusion rules: '{}' and '{}'",
            path,
            best.matcher.describe(),
            matches[1].matcher.describe()
        ));
    }

    Ok(NativeReferenceAccounting::Excluded(best))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn native_reference_case_paths_are_unique() {
        // Proof level A. This test proves the checked-in manifest has one
        // canonical record per source path. This test does not prove that two
        // different source paths cannot describe semantically duplicate cases.
        let mut paths = native_reference_cases()
            .iter()
            .map(|case| case.asm_path.as_str())
            .collect::<Vec<_>>();
        paths.sort_unstable();
        paths.dedup();
        assert_eq!(paths.len(), native_reference_cases().len());
    }

    #[test]
    fn native_reference_accounting_prefers_more_specific_prefixes() {
        // Proof level A. This test proves overlapping exclusion prefixes select
        // the most specific rule. This test does not prove that the selected
        // exclusion is semantically appropriate for every matching source.
        let accounted =
            account_native_reference_path("examples/motorola68000/amigaos/helloworld.asm")
                .expect("amigaos example should be excluded");
        match accounted {
            NativeReferenceAccounting::Excluded(rule) => {
                assert_eq!(
                    rule.matcher,
                    NativeReferencePathMatcher::Prefix("examples/motorola68000/amigaos/")
                );
            }
            NativeReferenceAccounting::Case(case) => {
                panic!("expected exclusion, got case {}", case.asm_path)
            }
            NativeReferenceAccounting::Opcore(assignment) => {
                panic!("expected exclusion, got opcore {}", assignment.source_path)
            }
        }
    }

    #[test]
    fn native_reference_fixture_rejects_duplicate_case_paths() {
        // Proof level A. This test proves duplicate canonical paths are rejected
        // while loading schema metadata. This test does not prove filesystem
        // aliases or equivalent source contents are duplicates.
        let fixture = r#"[
            {
                "asm_path": "examples/mos6502/duplicate.asm",
                "cpu_id": "m6502",
                "source_mode": "source-bin-from-example",
                "command_template": "{input} --bin {bin} --cpu m6502"
            },
            {
                "asm_path": "examples/mos6502/duplicate.asm",
                "cpu_id": "m6502",
                "source_mode": "source-bin-from-example",
                "command_template": "{input} --bin {bin} --cpu m6502"
            }
        ]"#;
        let error = parse_native_reference_cases_fixture(Path::new("duplicate.json"), fixture)
            .expect_err("duplicate paths must fail");
        assert!(error.contains("duplicate asm_path 'examples/mos6502/duplicate.asm'"));
    }

    #[test]
    fn native_reference_accounting_rejects_unknown_new_scope() {
        // Proof level A. This test proves a newly added path outside every case
        // and reviewed exclusion fails accounting. This test does not prove an
        // existing broad-prefix exclusion is semantically valid.
        let error = account_native_reference_path("examples/new-family/new.asm")
            .expect_err("unreviewed scope must fail");
        assert!(error.contains("not represented by a parity case or exclusion rule"));
    }

    #[test]
    fn native_reference_opcore_inventory_is_exact_and_complete() {
        // Proof level A. This test proves every checked-in opcore source/support
        // file has exactly one reviewed assignment and a future file fails
        // closed. It does not prove the assigned case is native-compatible.
        let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
        let opcore_root = repo_root.join("examples/opcore");
        let mut actual = Vec::new();
        fn collect_sources(root: &Path, directory: &Path, paths: &mut Vec<String>) {
            for entry in fs::read_dir(directory).expect("read opcore directory") {
                let path = entry.expect("read opcore entry").path();
                if path.is_dir() {
                    collect_sources(root, &path, paths);
                } else if matches!(
                    path.extension().and_then(|extension| extension.to_str()),
                    Some("asm" | "inc")
                ) {
                    paths.push(
                        path.strip_prefix(root)
                            .expect("opcore path below repository")
                            .to_string_lossy()
                            .replace('\\', "/"),
                    );
                }
            }
        }
        collect_sources(&repo_root, &opcore_root, &mut actual);
        actual.sort();

        let mut assigned = NATIVE_OPCORE_ASSIGNMENTS
            .iter()
            .map(|assignment| assignment.source_path.to_string())
            .collect::<Vec<_>>();
        assigned.sort();
        let assigned_len = assigned.len();
        assigned.dedup();
        assert_eq!(assigned.len(), assigned_len, "duplicate opcore assignment");
        assert_eq!(assigned, actual);
        assert!(account_native_reference_path("examples/opcore/future-example.asm").is_err());
    }

    #[test]
    fn native_reference_opcore_roots_own_all_reference_artifacts() {
        // Proof level A. This test proves every checked-in opcore reference
        // artifact is owned by exactly one Item 6-9 root and each root has at
        // least one artifact. It does not prove artifact semantic parity.
        let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
        let reference_root = repo_root.join("examples/reference/opcore");
        let roots = NATIVE_OPCORE_ASSIGNMENTS
            .iter()
            .filter_map(|assignment| match assignment.role {
                NativeOpcoreRole::Root { reference_stem } => {
                    Some((reference_stem, assignment.shard))
                }
                NativeOpcoreRole::Support { .. } => None,
            })
            .collect::<Vec<_>>();
        let mut seen_stems = HashSet::new();
        for entry in fs::read_dir(reference_root).expect("read opcore references") {
            let path = entry.expect("read opcore reference entry").path();
            let file_name = path
                .file_name()
                .and_then(|name| name.to_str())
                .expect("UTF-8 opcore reference name");
            if let Some((_, reason)) = NATIVE_OPCORE_REFERENCE_EXCLUSIONS
                .iter()
                .find(|(excluded, _)| *excluded == file_name)
            {
                assert!(!reason.trim().is_empty());
                continue;
            }
            let artifact_stem = file_name.split('.').next().expect("reference stem");
            let owners = roots
                .iter()
                .filter(|(reference_stem, _)| *reference_stem == artifact_stem)
                .collect::<Vec<_>>();
            assert_eq!(
                owners.len(),
                1,
                "reference artifact {file_name} must have exactly one root owner"
            );
            seen_stems.insert(artifact_stem.to_string());
        }
        for (reference_stem, _) in roots {
            assert!(
                seen_stems.contains(reference_stem),
                "opcore root {reference_stem} has no reference artifact"
            );
        }
        for (excluded, _) in NATIVE_OPCORE_REFERENCE_EXCLUSIONS {
            assert!(
                repo_root
                    .join("examples/reference/opcore")
                    .join(excluded)
                    .is_file(),
                "stale opcore reference exclusion {excluded}"
            );
        }
    }

    #[test]
    fn native_reference_opcore_support_files_match_their_shard_owner() {
        // Proof level A. This test proves every auxiliary file names an existing
        // root in the same shard. It does not prove the staged tree is complete.
        for support in NATIVE_OPCORE_ASSIGNMENTS {
            let NativeOpcoreRole::Support { owner } = support.role else {
                continue;
            };
            let owner = NATIVE_OPCORE_ASSIGNMENTS
                .iter()
                .find(|assignment| assignment.source_path == owner)
                .unwrap_or_else(|| panic!("missing opcore support owner {owner}"));
            assert!(matches!(owner.role, NativeOpcoreRole::Root { .. }));
            assert_eq!(support.shard, owner.shard);
        }
    }
}
