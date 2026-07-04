use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

pub(crate) const NATIVE_REFERENCE_FIXTURE_PATH: &str =
    "crates/opforge-asm/tests/fixtures/native_cli_reference_parity_schema.json";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum NativeReferenceSourceMode {
    StrippedBinFromSource,
    SourceCpuPrgFromExample,
}

impl NativeReferenceSourceMode {
    fn from_fixture_str(value: &str) -> Result<Self, String> {
        match value {
            "stripped-bin-from-source" => Ok(Self::StrippedBinFromSource),
            "source-cpu-prg-from-example" => Ok(Self::SourceCpuPrgFromExample),
            other => Err(format!(
                "unsupported native reference source_mode '{other}'; expected 'stripped-bin-from-source' or 'source-cpu-prg-from-example'"
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
    Excluded(&'a NativeReferenceExclusionRule),
}

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
        matcher: NativeReferencePathMatcher::Prefix("examples/opcore/"),
        reason:
            "opcore examples are CPU-neutral reference fixtures; they need reviewed MOS-backed native parity copies or direct CPU-backed staging before the native CLI can assemble them through the real package path",
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
                "source_mode": "stripped-bin-from-source",
                "command_template": "{input} --bin {bin} --cpu m6502"
            },
            {
                "asm_path": "examples/mos6502/duplicate.asm",
                "cpu_id": "m6502",
                "source_mode": "stripped-bin-from-source",
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
    fn native_reference_broad_prefix_is_accounting_not_applicability_proof() {
        // Proof level A. This test proves the current opcore prefix is accounted
        // for by its reviewed exclusion rule. This test does not prove that a
        // hypothetical member is truly inapplicable to native parity.
        let accounted = account_native_reference_path("examples/opcore/future-example.asm")
            .expect("broad opcore prefix should remain explicitly accounted for");
        let NativeReferenceAccounting::Excluded(rule) = accounted else {
            panic!("broad opcore prefix unexpectedly resolved to a parity case");
        };
        assert_eq!(
            rule.matcher,
            NativeReferencePathMatcher::Prefix("examples/opcore/")
        );
    }
}
