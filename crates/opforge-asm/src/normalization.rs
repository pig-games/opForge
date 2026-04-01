#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NormalizedErrorClass {
    InstructionRejected,
    SyntaxError,
    RangeError,
    Unclassified,
}

impl NormalizedErrorClass {
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::InstructionRejected => "instruction-rejected",
            Self::SyntaxError => "syntax-error",
            Self::RangeError => "range-error",
            Self::Unclassified => "unclassified",
        }
    }
}

pub(crate) fn normalize_opforge_diagnostics(text: &str) -> NormalizedErrorClass {
    let normalized = text.to_ascii_lowercase();

    if contains_any(&normalized, &["out of range"]) {
        return NormalizedErrorClass::RangeError;
    }
    if contains_any(
        &normalized,
        &[
            "unexpected token",
            "unexpected trailing tokens",
            "unexpected end of expression",
            "parser vm emitted diagnostic slot",
        ],
    ) {
        return NormalizedErrorClass::SyntaxError;
    }
    if contains_any(
        &normalized,
        &[
            "no instruction found for",
            "unknown mnemonic",
            "instruction not supported",
            "is not supported on",
            "is only supported on",
        ],
    ) {
        return NormalizedErrorClass::InstructionRejected;
    }

    NormalizedErrorClass::Unclassified
}

pub(crate) fn normalize_vasm_stderr(text: &str) -> NormalizedErrorClass {
    let normalized = text.to_ascii_lowercase();

    if contains_any(&normalized, &["out of range"]) {
        return NormalizedErrorClass::RangeError;
    }
    if contains_any(
        &normalized,
        &[
            "identifier expected",
            "syntax error",
            "unexpected",
            "too many closing parentheses",
        ],
    ) {
        return NormalizedErrorClass::SyntaxError;
    }
    if contains_any(
        &normalized,
        &[
            "unknown mnemonic",
            "illegal operand types",
            "instruction not supported on selected architecture",
        ],
    ) {
        return NormalizedErrorClass::InstructionRejected;
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
            NormalizedErrorClass::InstructionRejected
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
            NormalizedErrorClass::InstructionRejected
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
            NormalizedErrorClass::InstructionRejected
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
            NormalizedErrorClass::InstructionRejected
        );
    }

    #[test]
    fn normalization_classifies_vasm_range_errors() {
        let stderr = r#"error 2026 in line 1 of "fixture.asm": operand value out of range: 9 (valid: 0..7)
>    addq #9,d0"#;
        assert_eq!(
            normalize_vasm_stderr(stderr),
            NormalizedErrorClass::RangeError
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
}
