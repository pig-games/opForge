// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::fs;
use std::path::Path;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CaseStyle {
    #[default]
    Keep,
    Upper,
    Lower,
}

impl CaseStyle {
    pub fn apply(self, value: &str) -> String {
        match self {
            CaseStyle::Keep => value.to_string(),
            CaseStyle::Upper => value.to_ascii_uppercase(),
            CaseStyle::Lower => value.to_ascii_lowercase(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum IndentChar {
    #[default]
    Space,
    Tab,
}

impl IndentChar {
    pub fn fill(self, count: usize) -> String {
        match self {
            IndentChar::Space => " ".repeat(count),
            IndentChar::Tab => "\t".repeat(count),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LabelCaseStyle {
    #[default]
    Keep,
    Upper,
    Lower,
    LowerCamel,
    UpperCamel,
    UpperSnake,
}

impl LabelCaseStyle {
    pub fn apply(self, value: &str) -> String {
        match self {
            LabelCaseStyle::Keep => value.to_string(),
            LabelCaseStyle::Upper => value.to_ascii_uppercase(),
            LabelCaseStyle::Lower => value.to_ascii_lowercase(),
            LabelCaseStyle::LowerCamel => to_camel_case(value, false),
            LabelCaseStyle::UpperCamel => to_camel_case(value, true),
            LabelCaseStyle::UpperSnake => to_upper_snake_case(value),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LabelColonStyle {
    #[default]
    Keep,
    With,
    Without,
}

/// Formatter settings used by the formatting engine.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatterConfig {
    pub preserve_line_endings: bool,
    pub preserve_final_newline: bool,
    pub indent_char: IndentChar,
    pub label_alignment_column: usize,
    pub max_consecutive_blank_lines: usize,
    pub align_unlabeled_instructions: bool,
    pub split_long_label_instructions: bool,
    pub label_colon_style: LabelColonStyle,
    pub directive_case: CaseStyle,
    pub label_case: LabelCaseStyle,
    pub routine_label_case: LabelCaseStyle,
    pub data_label_case: LabelCaseStyle,
    pub constant_label_case: LabelCaseStyle,
    pub mnemonic_case: CaseStyle,
    pub register_case: CaseStyle,
    pub hex_literal_case: CaseStyle,
}

impl Default for FormatterConfig {
    fn default() -> Self {
        Self {
            preserve_line_endings: true,
            preserve_final_newline: true,
            indent_char: IndentChar::Space,
            label_alignment_column: 8,
            max_consecutive_blank_lines: 1,
            align_unlabeled_instructions: true,
            split_long_label_instructions: true,
            label_colon_style: LabelColonStyle::Keep,
            directive_case: CaseStyle::Keep,
            label_case: LabelCaseStyle::Keep,
            routine_label_case: LabelCaseStyle::Keep,
            data_label_case: LabelCaseStyle::Keep,
            constant_label_case: LabelCaseStyle::Keep,
            mnemonic_case: CaseStyle::Keep,
            register_case: CaseStyle::Keep,
            hex_literal_case: CaseStyle::Keep,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatterConfigError {
    message: String,
}

impl FormatterConfigError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl Display for FormatterConfigError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for FormatterConfigError {}

impl FormatterConfig {
    pub fn load_from_path(path: &Path) -> Result<Self, FormatterConfigError> {
        let text = fs::read_to_string(path).map_err(|err| {
            FormatterConfigError::new(format!("failed to read '{}': {err}", path.display()))
        })?;
        Self::parse_toml(path, &text)
    }

    fn parse_toml(path: &Path, source: &str) -> Result<Self, FormatterConfigError> {
        let mut config = Self::default();
        let mut section = ConfigSection::Root;
        let mut seen_keys = HashSet::new();

        for (index, raw_line) in source.lines().enumerate() {
            let line_no = index + 1;
            let line = strip_toml_comment(raw_line).trim();
            if line.is_empty() {
                continue;
            }

            if line.starts_with('[') {
                if !line.ends_with(']') {
                    return Err(config_error(path, line_no, "invalid section header"));
                }
                let name = line[1..line.len() - 1].trim();
                section = if name.eq_ignore_ascii_case("formatter") {
                    ConfigSection::Formatter
                } else {
                    ConfigSection::Other
                };
                continue;
            }

            if section == ConfigSection::Other {
                continue;
            }

            let Some((raw_key, raw_value)) = line.split_once('=') else {
                return Err(config_error(path, line_no, "expected key = value"));
            };
            let key = raw_key.trim();
            let value = raw_value.trim();
            if key.is_empty() || value.is_empty() {
                return Err(config_error(path, line_no, "expected key = value"));
            }

            let canonical_key = normalize_key(key);
            let dedup_key = normalize_dedup_key(&canonical_key);
            if !seen_keys.insert(dedup_key) {
                return Err(config_error(
                    path,
                    line_no,
                    format!("duplicate key '{}'", key),
                ));
            }

            match canonical_key.as_str() {
                "preserve_line_endings" => {
                    config.preserve_line_endings = parse_bool(path, line_no, key, value)?
                }
                "preserve_final_newline" => {
                    config.preserve_final_newline = parse_bool(path, line_no, key, value)?
                }
                "indent_char" => config.indent_char = parse_indent_char(path, line_no, key, value)?,
                "label_alignment_column" | "code_column" => {
                    config.label_alignment_column = parse_usize(path, line_no, key, value, true)?
                }
                "max_consecutive_blank_lines" | "max_blank_lines" => {
                    config.max_consecutive_blank_lines =
                        parse_usize(path, line_no, key, value, false)?
                }
                "align_unlabeled_instructions" => {
                    config.align_unlabeled_instructions = parse_bool(path, line_no, key, value)?
                }
                "split_long_label_instructions" => {
                    config.split_long_label_instructions = parse_bool(path, line_no, key, value)?
                }
                "label_colon_style" => {
                    config.label_colon_style = parse_label_colon_style(path, line_no, key, value)?
                }
                "directive_case" => {
                    config.directive_case = parse_case_style(path, line_no, key, value)?
                }
                "label_case" => {
                    config.label_case = parse_label_case_style(path, line_no, key, value)?
                }
                "routine_label_case" => {
                    config.routine_label_case = parse_label_case_style(path, line_no, key, value)?
                }
                "data_label_case" => {
                    config.data_label_case = parse_label_case_style(path, line_no, key, value)?
                }
                "constant_label_case" => {
                    config.constant_label_case = parse_label_case_style(path, line_no, key, value)?
                }
                "mnemonic_case" | "opcode_case" => {
                    config.mnemonic_case = parse_case_style(path, line_no, key, value)?
                }
                "register_case" => {
                    config.register_case = parse_case_style(path, line_no, key, value)?
                }
                "hex_literal_case" => {
                    config.hex_literal_case = parse_case_style(path, line_no, key, value)?
                }
                "profile" => {
                    let profile = parse_string(path, line_no, key, value)?;
                    if !profile.eq_ignore_ascii_case("safe-preserve") {
                        return Err(config_error(
                            path,
                            line_no,
                            format!("unsupported profile '{}'", profile),
                        ));
                    }
                }
                _ => {
                    return Err(config_error(
                        path,
                        line_no,
                        format!("unknown key '{}'", key),
                    ));
                }
            }
        }

        Ok(config)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ConfigSection {
    Root,
    Formatter,
    Other,
}

fn config_error(path: &Path, line_no: usize, message: impl Into<String>) -> FormatterConfigError {
    FormatterConfigError::new(format!(
        "{}:{}: {}",
        path.display(),
        line_no,
        message.into()
    ))
}

fn normalize_key(key: &str) -> String {
    key.trim().to_ascii_lowercase().replace('-', "_")
}

fn normalize_dedup_key(key: &str) -> String {
    match key {
        "code_column" => "label_alignment_column".to_string(),
        "max_blank_lines" => "max_consecutive_blank_lines".to_string(),
        "opcode_case" => "mnemonic_case".to_string(),
        other => other.to_string(),
    }
}

fn parse_bool(
    path: &Path,
    line_no: usize,
    key: &str,
    value: &str,
) -> Result<bool, FormatterConfigError> {
    match value.trim().to_ascii_lowercase().as_str() {
        "true" => Ok(true),
        "false" => Ok(false),
        _ => Err(config_error(
            path,
            line_no,
            format!("invalid boolean for '{}': {}", key, value),
        )),
    }
}

fn parse_usize(
    path: &Path,
    line_no: usize,
    key: &str,
    value: &str,
    minimum_one: bool,
) -> Result<usize, FormatterConfigError> {
    let normalized = value.trim().replace('_', "");
    let parsed = normalized.parse::<usize>().map_err(|_| {
        config_error(
            path,
            line_no,
            format!("invalid integer for '{}': {}", key, value),
        )
    })?;
    if minimum_one && parsed == 0 {
        return Err(config_error(
            path,
            line_no,
            format!("'{}' must be >= 1", key),
        ));
    }
    Ok(parsed)
}

fn parse_string(
    path: &Path,
    line_no: usize,
    key: &str,
    value: &str,
) -> Result<String, FormatterConfigError> {
    let value = value.trim();
    if value.len() >= 2
        && ((value.starts_with('"') && value.ends_with('"'))
            || (value.starts_with('\'') && value.ends_with('\'')))
    {
        return Ok(value[1..value.len() - 1].to_string());
    }
    if value.contains(' ') || value.contains('\t') {
        return Err(config_error(
            path,
            line_no,
            format!("invalid string for '{}': {}", key, value),
        ));
    }
    Ok(value.to_string())
}

fn parse_case_style(
    path: &Path,
    line_no: usize,
    key: &str,
    value: &str,
) -> Result<CaseStyle, FormatterConfigError> {
    let normalized = parse_string(path, line_no, key, value)?;
    match normalized.to_ascii_lowercase().as_str() {
        "keep" => Ok(CaseStyle::Keep),
        "upper" => Ok(CaseStyle::Upper),
        "lower" => Ok(CaseStyle::Lower),
        _ => Err(config_error(
            path,
            line_no,
            format!("invalid case style for '{}': {}", key, value),
        )),
    }
}

fn parse_indent_char(
    path: &Path,
    line_no: usize,
    key: &str,
    value: &str,
) -> Result<IndentChar, FormatterConfigError> {
    let normalized = parse_string(path, line_no, key, value)?;
    match normalized.to_ascii_lowercase().as_str() {
        "space" | "spaces" => Ok(IndentChar::Space),
        "tab" | "tabs" => Ok(IndentChar::Tab),
        _ => Err(config_error(
            path,
            line_no,
            format!("invalid indent char for '{}': {}", key, value),
        )),
    }
}

fn parse_label_case_style(
    path: &Path,
    line_no: usize,
    key: &str,
    value: &str,
) -> Result<LabelCaseStyle, FormatterConfigError> {
    let normalized = parse_string(path, line_no, key, value)?;
    match normalized.to_ascii_lowercase().as_str() {
        "keep" => Ok(LabelCaseStyle::Keep),
        "upper" => Ok(LabelCaseStyle::Upper),
        "lower" => Ok(LabelCaseStyle::Lower),
        "lower_camel" | "lowercamel" => Ok(LabelCaseStyle::LowerCamel),
        "upper_camel" | "uppercamel" => Ok(LabelCaseStyle::UpperCamel),
        "upper_snake" | "uppersnake" => Ok(LabelCaseStyle::UpperSnake),
        _ => Err(config_error(
            path,
            line_no,
            format!("invalid label case style for '{}': {}", key, value),
        )),
    }
}

fn parse_label_colon_style(
    path: &Path,
    line_no: usize,
    key: &str,
    value: &str,
) -> Result<LabelColonStyle, FormatterConfigError> {
    let normalized = parse_string(path, line_no, key, value)?;
    match normalized.to_ascii_lowercase().as_str() {
        "keep" => Ok(LabelColonStyle::Keep),
        "with" => Ok(LabelColonStyle::With),
        "without" => Ok(LabelColonStyle::Without),
        _ => Err(config_error(
            path,
            line_no,
            format!("invalid label colon style for '{}': {}", key, value),
        )),
    }
}

fn strip_toml_comment(line: &str) -> &str {
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;

    for (idx, ch) in line.char_indices() {
        match ch {
            '\'' if !in_double => in_single = !in_single,
            '"' if !in_single && !escaped => in_double = !in_double,
            '#' if !in_single && !in_double => return &line[..idx],
            _ => {}
        }

        escaped = in_double && ch == '\\' && !escaped;
        if ch != '\\' {
            escaped = false;
        }
    }
    line
}

fn split_label_words(input: &str) -> Vec<String> {
    let mut words = Vec::new();
    let mut current = String::new();
    let chars: Vec<char> = input.chars().collect();

    for (idx, ch) in chars.iter().copied().enumerate() {
        if ch == '_' || ch == '-' {
            if !current.is_empty() {
                words.push(std::mem::take(&mut current));
            }
            continue;
        }

        let prev = idx.checked_sub(1).and_then(|pos| chars.get(pos)).copied();
        let next = chars.get(idx + 1).copied();
        let boundary_before = idx > 0
            && ch.is_ascii_uppercase()
            && (prev.is_some_and(|value| value.is_ascii_lowercase() || value.is_ascii_digit())
                || (prev.is_some_and(|value| value.is_ascii_uppercase())
                    && next.is_some_and(|value| value.is_ascii_lowercase())));
        if boundary_before && !current.is_empty() {
            words.push(std::mem::take(&mut current));
        }
        current.push(ch);
    }

    if !current.is_empty() {
        words.push(current);
    }

    words
}

fn to_camel_case(input: &str, upper_first: bool) -> String {
    let words = split_label_words(input);
    let mut out = String::new();
    for (idx, word) in words.iter().enumerate() {
        let lower = word.to_ascii_lowercase();
        if idx == 0 && !upper_first {
            out.push_str(&lower);
            continue;
        }
        let mut chars = lower.chars();
        if let Some(first) = chars.next() {
            out.push(first.to_ascii_uppercase());
            out.push_str(chars.as_str());
        }
    }
    out
}

fn to_upper_snake_case(input: &str) -> String {
    split_label_words(input)
        .into_iter()
        .map(|word| word.to_ascii_uppercase())
        .collect::<Vec<_>>()
        .join("_")
}

#[cfg(test)]
mod tests {
    use super::{
        CaseStyle, FormatterConfig, FormatterConfigError, IndentChar, LabelCaseStyle,
        LabelColonStyle,
    };
    use std::env;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::process;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn default_config_matches_phase1_safe_profile_contract() {
        let cfg = FormatterConfig::default();
        assert!(cfg.preserve_line_endings);
        assert!(cfg.preserve_final_newline);
        assert_eq!(cfg.indent_char, IndentChar::Space);
        assert_eq!(cfg.label_alignment_column, 8);
        assert_eq!(cfg.max_consecutive_blank_lines, 1);
        assert!(cfg.align_unlabeled_instructions);
        assert!(cfg.split_long_label_instructions);
        assert_eq!(cfg.label_colon_style, LabelColonStyle::Keep);
        assert_eq!(cfg.directive_case, CaseStyle::Keep);
        assert_eq!(cfg.label_case, LabelCaseStyle::Keep);
        assert_eq!(cfg.routine_label_case, LabelCaseStyle::Keep);
        assert_eq!(cfg.data_label_case, LabelCaseStyle::Keep);
        assert_eq!(cfg.constant_label_case, LabelCaseStyle::Keep);
        assert_eq!(cfg.mnemonic_case, CaseStyle::Keep);
        assert_eq!(cfg.register_case, CaseStyle::Keep);
        assert_eq!(cfg.hex_literal_case, CaseStyle::Keep);
    }

    #[test]
    fn load_from_path_parses_root_keys() {
        let path = create_temp_config(
            "root-keys",
            "preserve_line_endings = false
preserve_final_newline = false
indent_char = \"tab\"
label_alignment_column = 8
max_consecutive_blank_lines = 2
",
        );
        let cfg = FormatterConfig::load_from_path(&path).expect("load config");
        assert!(!cfg.preserve_line_endings);
        assert!(!cfg.preserve_final_newline);
        assert_eq!(cfg.indent_char, IndentChar::Tab);
        assert_eq!(cfg.label_alignment_column, 8);
        assert_eq!(cfg.max_consecutive_blank_lines, 2);
    }

    #[test]
    fn load_from_path_parses_formatter_section_with_alias_keys() {
        let path = create_temp_config(
            "formatter-section",
            "[formatter]
profile = \"safe-preserve\"
indent_char = \"tab\"
code_column = 10
max_blank_lines = 0
align_unlabeled_instructions = true
split_long_label_instructions = true
label_colon_style = \"without\"
directive_case = \"lower\"
label_case = \"lower_camel\"
routine_label_case = \"lower_camel\"
data_label_case = \"upper_camel\"
constant_label_case = \"upper_snake\"
opcode_case = \"lower\"
register_case = \"upper\"
hex_literal_case = \"lower\"
",
        );
        let cfg = FormatterConfig::load_from_path(&path).expect("load config");
        assert_eq!(cfg.indent_char, IndentChar::Tab);
        assert_eq!(cfg.label_alignment_column, 10);
        assert_eq!(cfg.max_consecutive_blank_lines, 0);
        assert!(cfg.align_unlabeled_instructions);
        assert!(cfg.split_long_label_instructions);
        assert_eq!(cfg.label_colon_style, LabelColonStyle::Without);
        assert_eq!(cfg.directive_case, CaseStyle::Lower);
        assert_eq!(cfg.label_case, LabelCaseStyle::LowerCamel);
        assert_eq!(cfg.routine_label_case, LabelCaseStyle::LowerCamel);
        assert_eq!(cfg.data_label_case, LabelCaseStyle::UpperCamel);
        assert_eq!(cfg.constant_label_case, LabelCaseStyle::UpperSnake);
        assert_eq!(cfg.mnemonic_case, CaseStyle::Lower);
        assert_eq!(cfg.register_case, CaseStyle::Upper);
        assert_eq!(cfg.hex_literal_case, CaseStyle::Lower);
    }

    #[test]
    fn load_from_path_rejects_unknown_key() {
        let path = create_temp_config("unknown-key", "oops = 1\n");
        let err = FormatterConfig::load_from_path(&path).expect_err("unknown key must fail");
        assert_error_contains(&err, "unknown key 'oops'");
    }

    #[test]
    fn load_from_path_rejects_unsupported_profile() {
        let path = create_temp_config("bad-profile", "profile = \"canonical\"\n");
        let err = FormatterConfig::load_from_path(&path).expect_err("unsupported profile");
        assert_error_contains(&err, "unsupported profile 'canonical'");
    }

    #[test]
    fn load_from_path_rejects_zero_label_alignment_column() {
        let path = create_temp_config("zero-column", "label_alignment_column = 0\n");
        let err = FormatterConfig::load_from_path(&path).expect_err("zero column should fail");
        assert_error_contains(&err, "'label_alignment_column' must be >= 1");
    }

    #[test]
    fn load_from_path_rejects_duplicate_keys_across_sections() {
        let path = create_temp_config(
            "duplicate",
            "label_alignment_column = 8
[formatter]
code_column = 9
",
        );
        let err = FormatterConfig::load_from_path(&path).expect_err("duplicate should fail");
        assert_error_contains(&err, "duplicate key 'code_column'");
    }

    #[test]
    fn load_from_path_rejects_invalid_case_style() {
        let path = create_temp_config("bad-case", "mnemonic_case = \"camel\"\n");
        let err = FormatterConfig::load_from_path(&path).expect_err("invalid case style");
        assert_error_contains(&err, "invalid case style");
    }

    #[test]
    fn load_from_path_rejects_invalid_indent_char() {
        let path = create_temp_config("bad-indent-char", "indent_char = \"dots\"\n");
        let err = FormatterConfig::load_from_path(&path).expect_err("invalid indent char");
        assert_error_contains(&err, "invalid indent char");
    }

    #[test]
    fn load_from_path_rejects_invalid_label_case_style() {
        let path = create_temp_config("bad-label-case", "routine_label_case = \"pascal\"\n");
        let err = FormatterConfig::load_from_path(&path).expect_err("invalid label case style");
        assert_error_contains(&err, "invalid label case style");
    }

    #[test]
    fn label_case_style_helpers_convert_expected_shapes() {
        assert_eq!(
            LabelCaseStyle::LowerCamel.apply("my_routine_name"),
            "myRoutineName"
        );
        assert_eq!(
            LabelCaseStyle::UpperCamel.apply("my_data_name"),
            "MyDataName"
        );
        assert_eq!(
            LabelCaseStyle::UpperSnake.apply("myDataName"),
            "MY_DATA_NAME"
        );
    }

    #[test]
    fn load_from_path_rejects_invalid_label_colon_style() {
        let path = create_temp_config("bad-colon", "label_colon_style = \"always\"\n");
        let err = FormatterConfig::load_from_path(&path).expect_err("invalid colon style");
        assert_error_contains(&err, "invalid label colon style");
    }

    fn assert_error_contains(err: &FormatterConfigError, needle: &str) {
        assert!(
            err.to_string().contains(needle),
            "error '{}' did not contain '{}'",
            err,
            needle
        );
    }

    fn create_temp_config(label: &str, content: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("target")
            .join(format!(
                "formatter-config-{label}-{}-{nanos}",
                process::id()
            ));
        fs::create_dir_all(&dir).expect("create temp dir");
        let path = dir.join(".opforgefmt.toml");
        fs::write(&path, content).expect("write config");
        assert!(Path::new(&path).exists());
        path
    }
}
