#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum NativeReferenceSourceMode {
    StrippedBinFromSource,
    SourceCpuPrgFromExample,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct NativeReferenceCase {
    pub(crate) asm_path: &'static str,
    pub(crate) cpu_id: &'static str,
    pub(crate) source_mode: NativeReferenceSourceMode,
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

pub(crate) const NATIVE_REFERENCE_CASES: &[NativeReferenceCase] = &[
    NativeReferenceCase {
        asm_path: "examples/mos6502/6502_native_cli_smoke.asm",
        cpu_id: "m6502",
        source_mode: NativeReferenceSourceMode::StrippedBinFromSource,
    },
    NativeReferenceCase {
        asm_path: "examples/mos6502/6502_simple.asm",
        cpu_id: "m6502",
        source_mode: NativeReferenceSourceMode::StrippedBinFromSource,
    },
    NativeReferenceCase {
        asm_path: "examples/mos6502/6502_allmodes.asm",
        cpu_id: "m6502",
        source_mode: NativeReferenceSourceMode::StrippedBinFromSource,
    },
    NativeReferenceCase {
        asm_path: "examples/mos6502/6502_first_run_artifact_contract.asm",
        cpu_id: "m6502",
        source_mode: NativeReferenceSourceMode::SourceCpuPrgFromExample,
    },
    NativeReferenceCase {
        asm_path: "examples/mos6502/mos6502_modes.asm",
        cpu_id: "m6502",
        source_mode: NativeReferenceSourceMode::StrippedBinFromSource,
    },
    NativeReferenceCase {
        asm_path: "examples/mos6502/65c02_simple.asm",
        cpu_id: "65c02",
        source_mode: NativeReferenceSourceMode::StrippedBinFromSource,
    },
    NativeReferenceCase {
        asm_path: "examples/mos6502/65c02_allmodes.asm",
        cpu_id: "65c02",
        source_mode: NativeReferenceSourceMode::StrippedBinFromSource,
    },
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

pub(crate) fn native_reference_case_for_path(path: &str) -> Option<&'static NativeReferenceCase> {
    NATIVE_REFERENCE_CASES
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
        let mut paths = NATIVE_REFERENCE_CASES
            .iter()
            .map(|case| case.asm_path)
            .collect::<Vec<_>>();
        paths.sort_unstable();
        paths.dedup();
        assert_eq!(paths.len(), NATIVE_REFERENCE_CASES.len());
    }

    #[test]
    fn native_reference_accounting_prefers_more_specific_prefixes() {
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
}
