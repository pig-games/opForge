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
    Motorola68000Reference,
    Excluded(&'a NativeReferenceExclusionRule),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum NativeMotorola68000ReferenceOutcome {
    Binary {
        payload_path: String,
        listing_path: String,
    },
    Diagnostic {
        error_path: String,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct NativeMotorola68000ReferenceCase {
    pub(crate) asm_path: String,
    pub(crate) outcome: NativeMotorola68000ReferenceOutcome,
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
    DirectMos65c02,
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

// Item 9 policy is inventory-owned rather than selected by the emulator test.
// These exact paths partition the directly stored diagnostic shard according to
// the behavior of each actual source and its live Rust CLI result.
pub(crate) const NATIVE_OPCORE_DIAGNOSTIC_REACHABLE_ROOTS: &[&str] = &[
    "examples/opcore/mos65c02_parser_error.asm",
    "examples/opcore/macro_cross_module_error.asm",
    "examples/opcore/segment_cross_module_error.asm",
    "examples/opcore/statement_cross_module_error.asm",
    "examples/opcore/statement_private_import_error.asm",
];

pub(crate) const NATIVE_OPCORE_DIAGNOSTIC_NATIVE_BLOCKERS: &[(&str, &str)] = &[
    (
        "examples/opcore/mos65c02_mnemonic_error.asm",
        "native emits OPC-NCLI026 unsupported addressing while Rust reports unknown instruction",
    ),
    (
        "examples/opcore/directive_typo_elseif_fixit_error.asm",
        "native routes the typo as OPC-NCLI025 unknown mnemonic and has no Rust-equivalent directive fixit",
    ),
    (
        "examples/opcore/directive_typo_endif_fixit_error.asm",
        "native routes the typo as OPC-NCLI025 unknown mnemonic and has no Rust-equivalent directive fixit",
    ),
    (
        "examples/opcore/directive_typo_endmatch_fixit_error.asm",
        "native routes the typo as OPC-NCLI025 unknown mnemonic and has no Rust-equivalent directive fixit",
    ),
    (
        "examples/opcore/directive_typo_endmodule_fixit_error.asm",
        "native stops at OPC-NCLI016 module-depth mismatch before Rust's unknown-directive fixit path",
    ),
    (
        "examples/opcore/directive_typo_endsection_fixit_error.asm",
        "native routes the typo as OPC-NCLI025 unknown mnemonic and has no Rust-equivalent directive fixit",
    ),
    (
        "examples/opcore/errors.asm",
        "native reports a generic tokenizer VM failure instead of Rust's invalid-number diagnostic",
    ),
    (
        "examples/opcore/linker_regions_phase6_image_span_overflow.asm",
        "native reports generic flat-output write failure instead of Rust's invalid image-span diagnostic",
    ),
];

pub(crate) const NATIVE_OPCORE_DIAGNOSTIC_SUCCESS_ROOTS: &[(&str, &str)] = &[
    (
        "examples/opcore/conditional_missing_endif_fixit_error.asm",
        "the current stored source succeeds in Rust and owns normal output references, not a diagnostic reference",
    ),
    (
        "examples/opcore/conditional_unmatched_endif_error.asm",
        "the current stored source succeeds in Rust and owns normal output references, not a diagnostic reference",
    ),
    (
        "examples/opcore/module_missing_endmodule_error.asm",
        "the current stored source succeeds in Rust and owns normal output references, not a diagnostic reference",
    ),
    (
        "examples/opcore/multi_error_reporting_error.asm",
        "the current stored source succeeds in Rust and owns normal output references, not a diagnostic reference",
    ),
    (
        "examples/opcore/section_missing_endsection_error.asm",
        "the current stored source succeeds in Rust and owns normal output references, not a diagnostic reference",
    ),
];

// These error roots intentionally exercise the same stored export modules as
// their successful counterparts. The mapping is explicit because each support
// file has one canonical inventory owner, while Item 9 still needs the exact
// file bytes in the error root's isolated guest tree.
pub(crate) const NATIVE_OPCORE_DIAGNOSTIC_SHARED_SUPPORT: &[(&str, &str)] = &[
    (
        "examples/opcore/macro_cross_module_error.asm",
        "examples/opcore/lib/macro_export_lib.asm",
    ),
    (
        "examples/opcore/segment_cross_module_error.asm",
        "examples/opcore/lib/segment_export_lib.asm",
    ),
    (
        "examples/opcore/statement_cross_module_error.asm",
        "examples/opcore/lib/statement_export_lib.asm",
    ),
];

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
    opcore_root!("expr_syntax", SyntaxExpression, DirectMos65c02),
    opcore_root!("for_collection_basic", SyntaxExpression, DirectCpuNeutral),
    opcore_root!("for_counter_basic", SyntaxExpression, DirectCpuNeutral),
    opcore_root!("grouping", SyntaxExpression, DirectMos65c02),
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
    opcore_root!("syntax", SyntaxExpression, DirectMos65c02),
    opcore_root!("testexpr", SyntaxExpression, DirectMos65c02),
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
        DirectMos65c02
    ),
    opcore_root!(
        "macro_invocation_native",
        ModuleMacroStatement,
        DirectMos65c02
    ),
    opcore_root!("macro_segment_syntax", ModuleMacroStatement, DirectMos65c02),
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
    opcore_root!("preproc_syntax", ModuleMacroStatement, DirectMos65c02),
    opcore_root!("project_root/main.asm" => "project_root-main", ModuleMacroStatement, DirectCpuNeutral),
    opcore_root!(
        "statement_boundary_span",
        ModuleMacroStatement,
        DirectMos65c02
    ),
    opcore_root!(
        "statement_capture_types",
        ModuleMacroStatement,
        DirectCpuNeutral
    ),
    opcore_root!(
        "statement_cross_module_ok",
        ModuleMacroStatement,
        DirectMos65c02
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
    opcore_root!("use_wildcard_import", ModuleMacroStatement, DirectMos65c02),
    opcore_support!("lib/example_autoload_lib.asm" => "module_use_autoload.asm", ModuleMacroStatement, DirectCpuNeutral),
    opcore_support!("lib/macro_export_lib.asm" => "macro_cross_module_ok.asm", ModuleMacroStatement, DirectCpuNeutral),
    opcore_support!("lib/statement_export_lib.asm" => "statement_cross_module_ok.asm", ModuleMacroStatement, DirectCpuNeutral),
    opcore_support!("module_use_lib.inc" => "module_use_include.asm", ModuleMacroStatement, DirectCpuNeutral),
    opcore_support!("preproc_syntax.inc" => "preproc_syntax.asm", ModuleMacroStatement, DirectMos65c02),
    opcore_support!("project_root/util.asm" => "project_root/main.asm", ModuleMacroStatement, DirectCpuNeutral),
    opcore_root!("align_simple", LayoutOutput, DirectCpuNeutral),
    opcore_root!("cli_json_outputs", LayoutOutput, DirectCpuNeutral),
    opcore_root!("led1", LayoutOutput, DirectMos65c02),
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
    opcore_root!("module_qualified_section_map", LayoutOutput, DirectMos65c02),
    opcore_root!(
        "section_module_use_autoload",
        LayoutOutput,
        DirectCpuNeutral
    ),
    opcore_root!("section_module_use_include", LayoutOutput, DirectCpuNeutral),
    opcore_root!("section_simple", LayoutOutput, DirectCpuNeutral),
    opcore_root!("segment_cross_module_ok", LayoutOutput, DirectMos65c02),
    opcore_root!("sertest", LayoutOutput, DirectMos65c02),
    opcore_support!("cli_json_outputs.inc" => "cli_json_outputs.asm", LayoutOutput, DirectCpuNeutral),
    opcore_support!("lib/example_section_lib.asm" => "section_module_use_autoload.asm", LayoutOutput, DirectCpuNeutral),
    opcore_support!("lib/segment_export_lib.asm" => "segment_cross_module_ok.asm", LayoutOutput, DirectMos65c02),
    opcore_support!("section_module_use_lib.inc" => "section_module_use_include.asm", LayoutOutput, DirectCpuNeutral),
    opcore_root!(
        "conditional_missing_endif_fixit_error",
        Diagnostic,
        DirectMos65c02
    ),
    opcore_root!(
        "conditional_unmatched_endif_error",
        Diagnostic,
        DirectMos65c02
    ),
    opcore_root!("mos65c02_mnemonic_error", Diagnostic, DirectMos65c02),
    opcore_root!("mos65c02_parser_error", Diagnostic, DirectMos65c02),
    opcore_root!(
        "directive_typo_elseif_fixit_error",
        Diagnostic,
        DirectMos65c02
    ),
    opcore_root!(
        "directive_typo_endif_fixit_error",
        Diagnostic,
        DirectMos65c02
    ),
    opcore_root!(
        "directive_typo_endmatch_fixit_error",
        Diagnostic,
        DirectMos65c02
    ),
    opcore_root!(
        "directive_typo_endmodule_fixit_error",
        Diagnostic,
        DirectMos65c02
    ),
    opcore_root!(
        "directive_typo_endsection_fixit_error",
        Diagnostic,
        DirectMos65c02
    ),
    opcore_root!("errors", Diagnostic, DirectMos65c02),
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
        DirectMos65c02
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
    opcore_root!("macro_cross_module_error", Diagnostic, DirectMos65c02),
    opcore_root!("module_missing_endmodule_error", Diagnostic, DirectMos65c02),
    opcore_root!("module_use_private_error", Diagnostic, DirectCpuNeutral),
    opcore_root!("multi_error_reporting_error", Diagnostic, DirectMos65c02),
    opcore_root!("range_step_direction_error", Diagnostic, DirectCpuNeutral),
    opcore_root!("range_step_zero_error", Diagnostic, DirectCpuNeutral),
    opcore_root!(
        "section_missing_endsection_error",
        Diagnostic,
        DirectMos65c02
    ),
    opcore_root!("segment_cross_module_error", Diagnostic, DirectMos65c02),
    opcore_root!("statement_cross_module_error", Diagnostic, DirectMos65c02),
    opcore_root!("statement_private_import_error", Diagnostic, DirectMos65c02),
    opcore_root!("statement_signature_error", Diagnostic, DirectCpuNeutral),
    opcore_root!(
        "statement_unquoted_comma_error",
        Diagnostic,
        DirectCpuNeutral
    ),
    opcore_root!("while_unscoped_label_error", Diagnostic, DirectCpuNeutral),
    opcore_support!("lib/statement_private_export_lib.asm" => "statement_private_import_error.asm", Diagnostic, DirectMos65c02),
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
    let parsed: serde_json::Value = serde_json::from_str(fixture_text).map_err(|err| {
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
        let asm_path = fixture_string_field(case_index, entry, "asm_path", fixture_path)?;
        let cpu_id = fixture_string_field(case_index, entry, "cpu_id", fixture_path)?;
        let source_mode_text =
            fixture_string_field(case_index, entry, "source_mode", fixture_path)?;
        let command_template =
            fixture_string_field(case_index, entry, "command_template", fixture_path)?;
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

fn is_top_level_motorola68000_example(path: &str) -> bool {
    let path = Path::new(path);
    path.extension().and_then(|extension| extension.to_str()) == Some("asm")
        && path.parent() == Some(Path::new("examples/motorola68000"))
}

fn repository_relative_path(repo_root: &Path, path: &Path) -> Result<String, String> {
    path.strip_prefix(repo_root)
        .map(|relative| relative.to_string_lossy().replace('\\', "/"))
        .map_err(|error| {
            format!(
                "reference path {} is not below repository root {}: {error}",
                path.display(),
                repo_root.display()
            )
        })
}

pub(crate) fn native_motorola68000_reference_cases(
    repo_root: &Path,
) -> Result<Vec<NativeMotorola68000ReferenceCase>, String> {
    let source_root = repo_root.join("examples/motorola68000");
    let reference_root = repo_root.join("examples/reference/motorola68000");
    let mut sources = fs::read_dir(&source_root)
        .map_err(|error| format!("read {}: {error}", source_root.display()))?
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| path.extension().and_then(|extension| extension.to_str()) == Some("asm"))
        .collect::<Vec<_>>();
    sources.sort();

    let mut owned_references = HashSet::new();
    let mut cases = Vec::with_capacity(sources.len());
    for source_path in sources {
        let stem = source_path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .ok_or_else(|| format!("non-UTF-8 source stem: {}", source_path.display()))?;
        let error_path = reference_root.join(format!("{stem}.err"));
        let payload_path = reference_root.join(format!("{stem}.srec"));
        let listing_path = reference_root.join(format!("{stem}.lst"));
        let outcome = if error_path.is_file() {
            if payload_path.exists() || listing_path.exists() {
                return Err(format!(
                    "diagnostic reference {} must not also own .srec/.lst artifacts",
                    error_path.display()
                ));
            }
            let relative = repository_relative_path(repo_root, &error_path)?;
            owned_references.insert(relative.clone());
            NativeMotorola68000ReferenceOutcome::Diagnostic {
                error_path: relative,
            }
        } else {
            if !payload_path.is_file() || !listing_path.is_file() {
                return Err(format!(
                    "successful reference source {} requires both {} and {}",
                    source_path.display(),
                    payload_path.display(),
                    listing_path.display()
                ));
            }
            let payload_relative = repository_relative_path(repo_root, &payload_path)?;
            let listing_relative = repository_relative_path(repo_root, &listing_path)?;
            owned_references.insert(payload_relative.clone());
            owned_references.insert(listing_relative.clone());
            NativeMotorola68000ReferenceOutcome::Binary {
                payload_path: payload_relative,
                listing_path: listing_relative,
            }
        };
        cases.push(NativeMotorola68000ReferenceCase {
            asm_path: repository_relative_path(repo_root, &source_path)?,
            outcome,
        });
    }

    let mut actual_references = fs::read_dir(&reference_root)
        .map_err(|error| format!("read {}: {error}", reference_root.display()))?
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| {
            matches!(
                path.extension().and_then(|extension| extension.to_str()),
                Some("srec" | "lst" | "err")
            )
        })
        .map(|path| repository_relative_path(repo_root, &path))
        .collect::<Result<Vec<_>, _>>()?;
    actual_references.sort();
    let mut expected_references = owned_references.into_iter().collect::<Vec<_>>();
    expected_references.sort();
    if actual_references != expected_references {
        return Err(format!(
            "top-level Motorola 68000 reference ownership mismatch: expected {expected_references:?}, actual {actual_references:?}"
        ));
    }
    Ok(cases)
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
    if is_top_level_motorola68000_example(path) {
        return Ok(NativeReferenceAccounting::Motorola68000Reference);
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

    const REVIEWED_FOREIGN_MNEMONICS: &[&str] = &[
        // Intel 8080/8085 instruction spellings that are not also MOS 6502
        // mnemonics. This includes every Intel spelling retired from the active
        // opcore corpus, plus the rest of that reviewed instruction family.
        "aci", "add", "adi", "ana", "ani", "call", "cc", "cm", "cma", "cmc", "cnc", "cnz", "cp",
        "cpe", "cpi", "cpo", "cz", "daa", "dad", "dcr", "dcx", "di", "ei", "hlt", "in", "inr",
        "inx", "jc", "jm", "jnc", "jnz", "jp", "jpe", "jpo", "jz", "ldax", "lhld", "lxi", "mov",
        "mvi", "ori", "out", "pchl", "pop", "push", "ral", "rar", "rc", "ret", "rim", "rlc", "rm",
        "rnc", "rnz", "rp", "rpe", "rpo", "rrc", "rst", "rz", "sbb", "sbi", "shld", "sim", "sphl",
        "stax", "stc", "sub", "sui", "xchg", "xra", "xri", "xthl",
        // Distinctive Z80 spellings and Motorola 680x0 spellings. Mnemonics
        // shared with MOS (for example ADC, CMP, JMP, LDA, NOP) are valid and
        // deliberately absent.
        "djnz", "ex", "exx", "im", "ind", "indr", "ini", "inir", "jr", "ld", "ldd", "lddr", "ldi",
        "ldir", "neg", "otdr", "otir", "outd", "outi", "res", "reti", "retn", "rl", "rla", "rlca",
        "rld", "rr", "rra", "rrca", "rrd", "sla", "sll", "sra", "srl", "bsr", "dbcc", "dbcs",
        "dbeq", "dbf", "dbge", "dbgt", "dbhi", "dble", "dbls", "dblt", "dbmi", "dbne", "dbpl",
        "dbra", "dbt", "dbvc", "dbvs", "ext", "extb", "illegal", "link", "move", "movea", "movec",
        "movem", "movep", "moveq", "nbcd", "pea", "reset", "rte", "rtr", "rtm", "sbcd", "stop",
        "swap", "tas", "trap", "trapv", "unlk",
    ];

    fn reviewed_foreign_mnemonic(source_line: &str) -> Option<&str> {
        let code = source_line.split(';').next().unwrap_or("");
        let tokens = code.split_ascii_whitespace().collect::<Vec<_>>();
        if tokens.is_empty() {
            return None;
        }

        // Indentation makes the first token the instruction. At column one the
        // grammar permits either an instruction or a label, so examine both the
        // first token and the possible post-label token. This intentionally
        // fails closed for a foreign mnemonic used as an ambiguous label.
        let candidates =
            if tokens[0].starts_with('.') || code.chars().next().is_some_and(char::is_whitespace) {
                &tokens[..1]
            } else {
                &tokens[..tokens.len().min(2)]
            };
        candidates.iter().find_map(|token| {
            if token.starts_with('.') {
                return None;
            }
            let normalized = token
                .trim_matches(|ch: char| !ch.is_ascii_alphanumeric())
                .to_ascii_lowercase();
            REVIEWED_FOREIGN_MNEMONICS
                .contains(&normalized.as_str())
                .then_some(*token)
        })
    }

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
            NativeReferenceAccounting::Motorola68000Reference => {
                panic!("expected exclusion, got top-level Motorola 68000 reference")
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
    fn native_reference_opcore_scope_contains_only_6502_family_source() {
        // Proof level A. This permanently enforces the reviewed corpus boundary:
        // every active opcore source is either CPU-neutral or explicitly selects
        // 6502/65C02, and no Intel, Z80, or 680x0 instruction spelling is staged.
        let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
        for assignment in NATIVE_OPCORE_ASSIGNMENTS {
            let source = fs::read_to_string(repo_root.join(assignment.source_path))
                .unwrap_or_else(|err| panic!("read {}: {err}", assignment.source_path));
            for (line_index, source_line) in source.lines().enumerate() {
                let code = source_line.split(';').next().unwrap_or("");
                let trimmed = code.trim_start();
                if trimmed.to_ascii_lowercase().starts_with(".cpu ") {
                    let cpu = trimmed
                        .split_ascii_whitespace()
                        .nth(1)
                        .unwrap_or("")
                        .trim_matches(['\"', '\''])
                        .to_ascii_lowercase();
                    assert!(
                        matches!(cpu.as_str(), "6502" | "m6502" | "65c02" | "m65c02"),
                        "foreign CPU '{}' in {}:{}",
                        cpu,
                        assignment.source_path,
                        line_index + 1
                    );
                }

                let lowercase = trimmed.to_ascii_lowercase();
                for foreign_target in [
                    ".8080",
                    ".8085",
                    ".z80",
                    ".65816",
                    ".68000",
                    ".68020",
                    ".end8080",
                    ".end8085",
                    ".endz80",
                    ".end65816",
                    ".end68000",
                    ".end68020",
                    ".meta.output.8080.",
                    ".meta.output.8085.",
                    ".meta.output.z80.",
                    ".meta.output.65816.",
                    ".meta.output.68000.",
                    ".meta.output.68020.",
                ] {
                    assert!(
                        !lowercase.starts_with(foreign_target),
                        "foreign output target '{}' in {}:{}",
                        foreign_target,
                        assignment.source_path,
                        line_index + 1
                    );
                }

                if let Some(mnemonic) = reviewed_foreign_mnemonic(source_line) {
                    panic!(
                        "foreign mnemonic '{}' in {}:{}",
                        mnemonic,
                        assignment.source_path,
                        line_index + 1
                    );
                }
            }
        }
    }

    #[test]
    fn native_reference_foreign_mnemonic_guard_rejects_indented_and_column_one_forms() {
        for source_line in [
            "        mvi a,1",
            "mvi a,1",
            "label mvi a,1",
            "        lxi h,$1234",
            "label lhld $1234",
            "moveq #1,d0",
            "label dbra d0,label",
            "        djnz label",
            "label sim",
            "        out $20",
        ] {
            assert!(
                reviewed_foreign_mnemonic(source_line).is_some(),
                "foreign mnemonic must be rejected: {source_line}"
            );
        }
        for source_line in [
            "        lda #$11",
            "lda #$11",
            "label lda #$11",
            "        adc #1",
            "label jmp $2000",
            ".byte $11",
        ] {
            assert_eq!(
                reviewed_foreign_mnemonic(source_line),
                None,
                "MOS source must remain accepted: {source_line}"
            );
        }
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
