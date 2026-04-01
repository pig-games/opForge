#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NormalizedErrorClass {
    UnknownMnemonic,
    IllegalAddressingMode,
    UnsupportedCpuFeature,
    BranchOutOfRange,
    ValueOutOfRange,
    SyntaxError,
    MissingOperand,
    WrongOperandCount,
    Unclassified,
}

impl NormalizedErrorClass {
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::UnknownMnemonic => "unknown-mnemonic",
            Self::IllegalAddressingMode => "illegal-addressing-mode",
            Self::UnsupportedCpuFeature => "unsupported-cpu-feature",
            Self::BranchOutOfRange => "branch-out-of-range",
            Self::ValueOutOfRange => "value-out-of-range",
            Self::SyntaxError => "syntax-error",
            Self::MissingOperand => "missing-operand",
            Self::WrongOperandCount => "wrong-operand-count",
            Self::Unclassified => "unclassified",
        }
    }
}

pub(crate) fn normalize_opforge_diagnostics(text: &str) -> NormalizedErrorClass {
    let normalized = text.to_ascii_lowercase();

    if contains_any(&normalized, &["unknown mnemonic", "no instruction found for"]) {
        return NormalizedErrorClass::UnknownMnemonic;
    }
    if contains_any(
        &normalized,
        &[
            "instruction not supported",
            "is not supported on",
            "is only supported on",
        ],
    ) {
        return NormalizedErrorClass::UnsupportedCpuFeature;
    }
    if contains_any(&normalized, &["branch out of range"]) {
        return NormalizedErrorClass::BranchOutOfRange;
    }
    if contains_any(&normalized, &["operand count", "wrong operand count"]) {
        return NormalizedErrorClass::WrongOperandCount;
    }
    if contains_any(&normalized, &["missing operand", "expected operand"]) {
        return NormalizedErrorClass::MissingOperand;
    }
    if contains_any(&normalized, &["illegal addressing mode", "illegal operand types"]) {
        return NormalizedErrorClass::IllegalAddressingMode;
    }
    if contains_any(
        &normalized,
        &["out of range", "too large for", "invalid u8 operand", "invalid u16 operand"],
    )
        && contains_any(&normalized, &["branch"])
    {
        return NormalizedErrorClass::BranchOutOfRange;
    }
    if contains_any(
        &normalized,
        &["out of range", "too large for", "invalid u8 operand", "invalid u16 operand"],
    ) {
        return NormalizedErrorClass::ValueOutOfRange;
    }
    if contains_any(
        &normalized,
        &[
            "unexpected trailing tokens",
            "operand count",
            "wrong operand count",
            "too many operands",
            "too few operands",
        ],
    ) {
        return NormalizedErrorClass::WrongOperandCount;
    }
    if contains_any(
        &normalized,
        &[
            "unexpected token",
            "unexpected end of expression",
            "parser vm emitted diagnostic slot",
            "syntax error",
            "identifier expected",
            "wrong type",
        ],
    ) {
        return NormalizedErrorClass::SyntaxError;
    }

    NormalizedErrorClass::Unclassified
}

pub(crate) fn normalize_vasm_stderr(text: &str) -> NormalizedErrorClass {
    let normalized = text.to_ascii_lowercase();

    if contains_any(&normalized, &["unknown mnemonic"]) {
        return NormalizedErrorClass::UnknownMnemonic;
    }
    if contains_any(
        &normalized,
        &[
            "instruction not supported on selected architecture",
            "is not supported on",
            "is only supported on",
        ],
    ) {
        return NormalizedErrorClass::UnsupportedCpuFeature;
    }
    if contains_any(&normalized, &["illegal operand types", "illegal addressing mode"]) {
        return NormalizedErrorClass::IllegalAddressingMode;
    }
    if contains_any(&normalized, &["out of range"]) {
        return NormalizedErrorClass::ValueOutOfRange;
    }
    if contains_any(&normalized, &["missing operand", "operand expected"]) {
        return NormalizedErrorClass::MissingOperand;
    }
    if contains_any(&normalized, &["wrong number of operands", "operand count"]) {
        return NormalizedErrorClass::WrongOperandCount;
    }
    if contains_any(
        &normalized,
        &[
            "identifier expected",
            "syntax error",
            "unexpected",
            "too many closing parentheses",
            "wrong type",
        ],
    ) {
        return NormalizedErrorClass::SyntaxError;
    }

    NormalizedErrorClass::Unclassified
}

pub(crate) fn normalize_64tass_stderr(text: &str) -> NormalizedErrorClass {
    let normalized = text.to_ascii_lowercase();

    if contains_any(&normalized, &["unknown mnemonic"]) {
        return NormalizedErrorClass::UnknownMnemonic;
    }
    if contains_any(
        &normalized,
        &[
            "is not available in this cpu mode",
            "not available in this cpu mode",
        ],
    ) {
        return NormalizedErrorClass::UnsupportedCpuFeature;
    }
    if contains_any(
        &normalized,
        &[
            "invalid addressing mode",
            "addressing mode not possible",
            "wrong type",
        ],
    ) {
        return NormalizedErrorClass::IllegalAddressingMode;
    }
    if contains_any(&normalized, &["branch out of range"]) {
        return NormalizedErrorClass::BranchOutOfRange;
    }
    if contains_any(&normalized, &["too large for", "out of range"]) {
        return NormalizedErrorClass::ValueOutOfRange;
    }
    if contains_any(&normalized, &["missing argument", "missing operand", "no argument"]) {
        return NormalizedErrorClass::MissingOperand;
    }
    if contains_any(
        &normalized,
        &[
            "too many arguments",
            "too few arguments",
            "wrong number of arguments",
            "operand addressing mode",
        ],
    ) {
        return NormalizedErrorClass::WrongOperandCount;
    }
    if contains_any(&normalized, &["syntax error", "identifier expected", "unexpected"]) {
        return NormalizedErrorClass::SyntaxError;
    }

    NormalizedErrorClass::Unclassified
}

pub(crate) fn diagnostic_excerpt(text: &str) -> String {
    let preferred = text.lines().map(str::trim).find(|line| {
        !line.is_empty()
            && !line.starts_with("vasm ")
            && (line.starts_with("ERROR:")
                || line.starts_with("error ")
                || line.starts_with("warning "))
    });

    preferred
        .or_else(|| {
            text.lines().map(str::trim).find(|line| {
                !line.is_empty() && !line.starts_with("vasm ") && !line.starts_with('>')
            })
        })
        .map(truncate_excerpt)
        .unwrap_or_else(|| "<no diagnostic excerpt>".to_string())
}

fn contains_any(haystack: &str, needles: &[&str]) -> bool {
    needles.iter().any(|needle| haystack.contains(needle))
}

fn truncate_excerpt(line: &str) -> String {
    const MAX_EXCERPT_LEN: usize = 120;

    if line.len() <= MAX_EXCERPT_LEN {
        return line.to_string();
    }

    let mut excerpt = line[..MAX_EXCERPT_LEN].to_string();
    excerpt.push_str("...");
    excerpt
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalization_classifies_opforge_instruction_rejection() {
        let diagnostics = r#"fixture.asm:1: ERROR [asm402]
    1 |     bogus d0,d1^
ERROR: No instruction found for BOGUS"#;
        assert_eq!(
            normalize_opforge_diagnostics(diagnostics),
            NormalizedErrorClass::UnknownMnemonic
        );
    }

    #[test]
    fn normalization_classifies_opforge_architecture_specific_instruction_rejection() {
        let diagnostics = r#"summary: Errors detected in source.
[
    Diagnostic {
        error: AsmError {
            kind: Instruction,
            message: "CALLM is not supported on m68040",
        },
    },
]"#;
        assert_eq!(
            normalize_opforge_diagnostics(diagnostics),
            NormalizedErrorClass::UnsupportedCpuFeature
        );
    }

    #[test]
    fn normalization_classifies_opforge_single_cpu_instruction_rejection() {
        let diagnostics = r#"summary: Errors detected in source.
[
    Diagnostic {
        error: AsmError {
            kind: Instruction,
            message: "RTM is only supported on m68020",
        },
    },
]"#;
        assert_eq!(
            normalize_opforge_diagnostics(diagnostics),
            NormalizedErrorClass::UnsupportedCpuFeature
        );
    }

    #[test]
    fn normalization_classifies_opforge_syntax_errors() {
        let diagnostics = r#"fixture.asm:1: ERROR [otp001]
    1 |     ,
ERROR: parser VM emitted diagnostic slot 0"#;
        assert_eq!(
            normalize_opforge_diagnostics(diagnostics),
            NormalizedErrorClass::SyntaxError
        );
    }

    #[test]
    fn normalization_classifies_vasm_instruction_rejection() {
        let stderr = r#"error 2 in line 1 of "fixture.asm": unknown mnemonic <bogus>
>    bogus d0,d1"#;
        assert_eq!(
            normalize_vasm_stderr(stderr),
            NormalizedErrorClass::UnknownMnemonic
        );
    }

    #[test]
    fn normalization_classifies_vasm_range_errors() {
        let stderr = r#"error 2026 in line 1 of "fixture.asm": operand value out of range: 9 (valid: 0..7)
>    addq #9,d0"#;
        assert_eq!(
            normalize_vasm_stderr(stderr),
            NormalizedErrorClass::ValueOutOfRange
        );
    }

    #[test]
    fn normalization_classifies_vasm_syntax_errors() {
        let stderr = r#"error 1011 in line 1 of "fixture.asm": identifier expected
>    ,"#;
        assert_eq!(
            normalize_vasm_stderr(stderr),
            NormalizedErrorClass::SyntaxError
        );
    }

    #[test]
    fn normalization_extracts_useful_excerpt() {
        let stderr = "vasm 1.8g\n\nerror 2 in line 1 of \"fixture.asm\": unknown mnemonic <bogus>\n> bogus d0,d1\n";
        assert_eq!(
            diagnostic_excerpt(stderr),
            "error 2 in line 1 of \"fixture.asm\": unknown mnemonic <bogus>"
        );
    }

    #[test]
    fn normalization_classifies_64tass_unknown_mnemonic() {
        let stderr = "fixture.asm:1:1: error: unknown mnemonic 'bogus'\n";
        assert_eq!(
            normalize_64tass_stderr(stderr),
            NormalizedErrorClass::UnknownMnemonic
        );
    }

    #[test]
    fn normalization_classifies_64tass_illegal_addressing_mode() {
        let stderr = "fixture.asm:1:10: error: wrong type 'bits'\n";
        assert_eq!(
            normalize_64tass_stderr(stderr),
            NormalizedErrorClass::IllegalAddressingMode
        );
    }

    #[test]
    fn normalization_classifies_64tass_value_out_of_range() {
        let stderr = "fixture.asm:1:14: error: too large for a 8 bit unsigned integer bits '$123'\n";
        assert_eq!(
            normalize_64tass_stderr(stderr),
            NormalizedErrorClass::ValueOutOfRange
        );
    }
}

