use crate::oracle::{
    ExternalOracleAdapter, OracleAssembleFailure, OracleAssembleRequest, OracleAssembleSuccess,
    OracleAvailability,
};
use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const OPT_IN_ENV: &str = "OPFORGE_EXTERNAL_ORACLE_64TASS";
const BIN_ENV: &str = "OPFORGE_64TASS_BIN";
const DEFAULT_EXECUTABLE: &str = "64tass";
const OUTPUT_FILENAME: &str = "output.bin";
const STDOUT_FILENAME: &str = "64tass.stdout.txt";
const STDERR_FILENAME: &str = "64tass.stderr.txt";
const NORMALIZED_SOURCE_FILENAME: &str = "64tass.normalized.asm";
const ORACLE_ID: &str = "64tass";
const ORACLE_PROFILE: &str = "tass_6502_flat_binary";
const SUPPORTED_FAMILY: &str = "mos6502";
const CPU_PROFILE_65816_NATIVE_MX_16_16: &str = "native_mx_16_16";

#[derive(Debug, Clone)]
pub(crate) struct Tass64Adapter {
    executable: Option<PathBuf>,
    availability: OracleAvailability,
}

impl Tass64Adapter {
    pub(crate) fn from_env() -> Self {
        Self::from_inputs(AvailabilityInputs {
            opt_in_enabled: opt_in_enabled(),
            configured_bin: env::var_os(BIN_ENV).map(PathBuf::from),
            path: env::var_os("PATH"),
        })
    }

    fn from_inputs(inputs: AvailabilityInputs) -> Self {
        if !inputs.opt_in_enabled {
            return Self {
                executable: None,
                availability: OracleAvailability::Disabled(format!(
                    "Set {OPT_IN_ENV}=1 to run the external-oracle 64tass success-path fixtures"
                )),
            };
        }

        let path_entries = split_path_entries(inputs.path.as_ref());
        let executable = inputs
            .configured_bin
            .as_deref()
            .and_then(|candidate| resolve_command_candidate(candidate, &path_entries))
            .or_else(|| resolve_command_candidate(Path::new(DEFAULT_EXECUTABLE), &path_entries));

        match executable {
            Some(executable) => Self {
                executable: Some(executable),
                availability: OracleAvailability::Ready,
            },
            None => Self {
                executable: None,
                availability: OracleAvailability::Missing(format!(
                    "Could not find {DEFAULT_EXECUTABLE} on PATH. Set {BIN_ENV} to a 64tass executable to enable the external-oracle fixtures"
                )),
            },
        }
    }
}

impl Default for Tass64Adapter {
    fn default() -> Self {
        Self::from_env()
    }
}

impl ExternalOracleAdapter for Tass64Adapter {
    fn oracle_id(&self) -> &'static str {
        ORACLE_ID
    }

    fn oracle_profile(&self) -> &'static str {
        ORACLE_PROFILE
    }

    fn supports_family(&self, family: &str) -> bool {
        family == SUPPORTED_FAMILY
    }

    fn supports_cpu(&self, cpu: &str) -> bool {
        canonical_cpu(cpu).is_some()
    }

    fn supports_profile(&self, cpu: &str, profile: Option<&str>) -> bool {
        match canonical_cpu(cpu) {
            Some("m6502" | "65c02" | "45gs02") => profile.is_none(),
            Some("65816") => matches!(profile, Some(CPU_PROFILE_65816_NATIVE_MX_16_16)),
            _ => false,
        }
    }

    fn availability(&self) -> OracleAvailability {
        self.availability.clone()
    }

    fn assemble_flat_binary(
        &self,
        request: OracleAssembleRequest<'_>,
    ) -> Result<OracleAssembleSuccess, OracleAssembleFailure> {
        let Some(executable) = &self.executable else {
            let stderr_path = request.output_dir.join(STDERR_FILENAME);
            let stdout_path = request.output_dir.join(STDOUT_FILENAME);
            let summary = match &self.availability {
                OracleAvailability::Disabled(reason) | OracleAvailability::Missing(reason) => {
                    reason.clone()
                }
                OracleAvailability::Ready => "64tass executable unexpectedly missing".to_string(),
            };
            let _ = fs::create_dir_all(request.output_dir);
            let _ = fs::write(&stdout_path, "");
            let _ = fs::write(&stderr_path, format!("{summary}\n"));
            return Err(OracleAssembleFailure {
                diagnostics_path: stderr_path.clone(),
                stdout_path: Some(stdout_path),
                stderr_path: Some(stderr_path),
                diagnostics_text: format!("{summary}\n"),
                summary,
            });
        };

        assemble_flat_binary_with_executable(executable, request)
    }
}

fn assemble_flat_binary_with_executable(
    executable: &Path,
    request: OracleAssembleRequest<'_>,
) -> Result<OracleAssembleSuccess, OracleAssembleFailure> {
    if let Err(err) = fs::create_dir_all(request.output_dir) {
        return Err(OracleAssembleFailure {
            diagnostics_path: request.output_dir.join(STDERR_FILENAME),
            stdout_path: Some(request.output_dir.join(STDOUT_FILENAME)),
            stderr_path: Some(request.output_dir.join(STDERR_FILENAME)),
            diagnostics_text: format!(
                "Create oracle output directory {}: {err}\n",
                request.output_dir.display()
            ),
            summary: format!(
                "Create oracle output directory {}: {err}",
                request.output_dir.display()
            ),
        });
    }

    let output_path = request.output_dir.join(OUTPUT_FILENAME);
    let stdout_path = request.output_dir.join(STDOUT_FILENAME);
    let stderr_path = request.output_dir.join(STDERR_FILENAME);
    let normalized_source_path = request.output_dir.join(NORMALIZED_SOURCE_FILENAME);
    if let Err(err) = write_normalized_source_file(request.source_path, &normalized_source_path) {
        let diagnostics_text = format!(
            "Normalize source for 64tass ({} -> {}): {err}\n",
            request.source_path.display(),
            normalized_source_path.display()
        );
        let _ = fs::write(&stderr_path, &diagnostics_text);
        let _ = fs::write(&stdout_path, "");
        return Err(OracleAssembleFailure {
            diagnostics_path: stderr_path.clone(),
            stdout_path: Some(stdout_path),
            stderr_path: Some(stderr_path),
            diagnostics_text,
            summary: format!(
                "Normalize source for 64tass ({} -> {}): {err}",
                request.source_path.display(),
                normalized_source_path.display()
            ),
        });
    }

    let normalized_request = OracleAssembleRequest {
        cpu: request.cpu,
        cpu_profile: request.cpu_profile,
        source_path: normalized_source_path.as_path(),
        output_dir: request.output_dir,
    };

    let command_args = match build_command_args(&output_path, &normalized_request) {
        Ok(args) => args,
        Err(diagnostics_text) => {
            let _ = fs::write(&stderr_path, &diagnostics_text);
            let _ = fs::write(&stdout_path, "");
            return Err(OracleAssembleFailure {
                diagnostics_path: stderr_path.clone(),
                stdout_path: Some(stdout_path),
                stderr_path: Some(stderr_path),
                diagnostics_text,
                summary: format!("Unsupported 64tass cpu '{}'", request.cpu),
            });
        }
    };

    let output = match Command::new(executable).args(&command_args).output() {
        Ok(output) => output,
        Err(err) => {
            let _ = fs::write(&stdout_path, "");
            let diagnostics_text = format!("failed to execute 64tass: {err}\n");
            let _ = fs::write(&stderr_path, &diagnostics_text);
            return Err(OracleAssembleFailure {
                diagnostics_path: stderr_path.clone(),
                stdout_path: Some(stdout_path),
                stderr_path: Some(stderr_path),
                diagnostics_text,
                summary: format!("Execute {}: {err}", executable.display()),
            });
        }
    };

    let _ = fs::write(&stdout_path, &output.stdout);
    let _ = fs::write(&stderr_path, &output.stderr);

    if !output.status.success() {
        let code = output
            .status
            .code()
            .map(|code| code.to_string())
            .unwrap_or_else(|| "signal".to_string());
        let diagnostics_text = String::from_utf8_lossy(&output.stderr).into_owned();
        return Err(OracleAssembleFailure {
            diagnostics_path: stderr_path.clone(),
            stdout_path: Some(stdout_path),
            stderr_path: Some(stderr_path),
            diagnostics_text,
            summary: format!("64tass exited with status {code}"),
        });
    }

    match fs::read(&output_path) {
        Ok(bytes) => Ok(OracleAssembleSuccess {
            output_path,
            bytes,
            stdout_path: Some(stdout_path),
            stderr_path: Some(stderr_path),
        }),
        Err(err) => Err(OracleAssembleFailure {
            diagnostics_path: stderr_path.clone(),
            stdout_path: Some(stdout_path),
            stderr_path: Some(stderr_path),
            diagnostics_text: format!("Read 64tass output {}: {err}\n", output_path.display()),
            summary: format!("Read 64tass output {}: {err}", output_path.display()),
        }),
    }
}

#[derive(Debug, Clone)]
struct AvailabilityInputs {
    opt_in_enabled: bool,
    configured_bin: Option<PathBuf>,
    path: Option<OsString>,
}

fn opt_in_enabled() -> bool {
    env::var(OPT_IN_ENV)
        .map(|value| {
            !matches!(
                value.trim().to_ascii_lowercase().as_str(),
                "" | "0" | "false" | "no" | "off"
            )
        })
        .unwrap_or(false)
}

fn split_path_entries(path: Option<&OsString>) -> Vec<PathBuf> {
    path.map(env::split_paths)
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
}

fn resolve_command_candidate(candidate: &Path, path_entries: &[PathBuf]) -> Option<PathBuf> {
    if candidate.components().count() > 1 || candidate.is_absolute() {
        return candidate.exists().then(|| candidate.to_path_buf());
    }

    path_entries
        .iter()
        .map(|root| root.join(candidate))
        .find(|path| path.exists())
}

fn build_command_args(
    output_path: &Path,
    request: &OracleAssembleRequest<'_>,
) -> Result<Vec<OsString>, String> {
    let cpu_flag = cpu_flag(request.cpu).ok_or_else(|| unsupported_cpu_message(request.cpu))?;
    let args = vec![
        OsString::from(cpu_flag),
        OsString::from("-b"),
        OsString::from("-f"),
        OsString::from("-o"),
        output_path.as_os_str().to_os_string(),
        request.source_path.as_os_str().to_os_string(),
    ];
    Ok(args)
}

fn unsupported_cpu_message(cpu: &str) -> String {
    format!(
        "unsupported 64tass cpu '{cpu}'; current slice supports m6502, 65c02, 65816, and 45gs02\n"
    )
}

fn write_normalized_source_file(input_path: &Path, output_path: &Path) -> std::io::Result<()> {
    let source = fs::read_to_string(input_path)?;
    let normalized = normalize_source_text_for_64tass(&source);
    fs::write(output_path, normalized)
}

fn normalize_source_text_for_64tass(source: &str) -> String {
    source
        .lines()
        .map(normalize_source_line_for_64tass)
        .collect::<Vec<_>>()
        .join("\n")
}

fn normalize_source_line_for_64tass(line: &str) -> String {
    let (indent_len, _) = line
        .char_indices()
        .find(|(_, ch)| !ch.is_whitespace())
        .unwrap_or((line.len(), ' '));
    let indent = &line[..indent_len];
    let trimmed = &line[indent_len..];

    if let Some(rest) = strip_directive_case_insensitive(trimmed, ".cpu") {
        let (operand_raw, comment) = split_comment(rest);
        let operand = operand_raw.trim();
        if let Some(cpu) = normalized_cpu_directive_value(operand) {
            let comment_suffix = comment.map(|c| format!(";{c}")).unwrap_or_default();
            return format!("{indent}.cpu \"{cpu}\"{comment_suffix}");
        }
    }

    if let Some(rest) = strip_directive_case_insensitive(trimmed, ".org") {
        let (expr_raw, comment) = split_comment(rest);
        let expr = expr_raw.trim();
        if !expr.is_empty() {
            let comment_suffix = comment.map(|c| format!(";{c}")).unwrap_or_default();
            return format!("{indent}* = {expr}{comment_suffix}");
        }
    }

    line.to_string()
}

fn strip_directive_case_insensitive<'a>(line: &'a str, directive: &str) -> Option<&'a str> {
    if line.len() < directive.len() {
        return None;
    }

    let prefix = &line[..directive.len()];
    if !prefix.eq_ignore_ascii_case(directive) {
        return None;
    }

    let rest = &line[directive.len()..];
    if rest.is_empty() || rest.starts_with(char::is_whitespace) {
        Some(rest)
    } else {
        None
    }
}

fn split_comment(line: &str) -> (&str, Option<&str>) {
    match line.split_once(';') {
        Some((head, tail)) => (head, Some(tail)),
        None => (line, None),
    }
}

fn normalized_cpu_directive_value(operand: &str) -> Option<&'static str> {
    let normalized = strip_surrounding_quotes(operand).trim();
    match normalized.to_ascii_lowercase().as_str() {
        "6502" | "m6502" => Some("6502"),
        "65c02" | "m65c02" => Some("65c02"),
        "65816" | "65c816" | "w65c816" | "m65816" => Some("65816"),
        "45gs02" | "m45gs02" | "mega65" => Some("45gs02"),
        _ => None,
    }
}

fn strip_surrounding_quotes(value: &str) -> &str {
    let value = value.trim();
    if value.len() >= 2 && value.starts_with('"') && value.ends_with('"') {
        &value[1..value.len() - 1]
    } else {
        value
    }
}

fn canonical_cpu(cpu: &str) -> Option<&'static str> {
    match cpu.to_ascii_lowercase().as_str() {
        "6502" | "m6502" => Some("m6502"),
        "65c02" | "m65c02" => Some("65c02"),
        "65816" | "65c816" | "w65c816" | "m65816" => Some("65816"),
        "45gs02" | "m45gs02" => Some("45gs02"),
        _ => None,
    }
}

fn cpu_flag(cpu: &str) -> Option<&'static str> {
    match canonical_cpu(cpu) {
        Some("m6502") => Some("--m6502"),
        Some("65c02") => Some("--m65c02"),
        Some("65816") => Some("--m65816"),
        Some("45gs02") => Some("--m45gs02"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_dir(label: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos();
        let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("target")
            .join(format!("{label}-{}-{nanos}", std::process::id()));
        fs::create_dir_all(&dir).expect("create temp dir");
        dir
    }

    #[test]
    fn external_oracle_64tass_availability_is_disabled_without_opt_in() {
        let adapter = Tass64Adapter::from_inputs(AvailabilityInputs {
            opt_in_enabled: false,
            configured_bin: None,
            path: None,
        });
        assert!(matches!(
            adapter.availability(),
            OracleAvailability::Disabled(reason)
                if reason.contains("OPFORGE_EXTERNAL_ORACLE_64TASS")
        ));
    }

    #[test]
    fn external_oracle_64tass_availability_reports_missing_binary() {
        let adapter = Tass64Adapter::from_inputs(AvailabilityInputs {
            opt_in_enabled: true,
            configured_bin: None,
            path: None,
        });
        assert!(matches!(
            adapter.availability(),
            OracleAvailability::Missing(reason) if reason.contains(DEFAULT_EXECUTABLE)
        ));
    }

    #[test]
    fn external_oracle_64tass_availability_prefers_configured_binary() {
        let dir = temp_dir("external-oracle-64tass-configured-bin");
        let configured = dir.join("opforge-64tass");
        fs::write(&configured, "#!/bin/sh\nexit 0\n").expect("write fake binary");

        let adapter = Tass64Adapter::from_inputs(AvailabilityInputs {
            opt_in_enabled: true,
            configured_bin: Some(configured.clone()),
            path: None,
        });

        assert_eq!(adapter.availability(), OracleAvailability::Ready);
        assert_eq!(adapter.executable.as_deref(), Some(configured.as_path()));
    }

    #[test]
    fn external_oracle_64tass_rejects_unsupported_cpu_before_spawn() {
        let dir = temp_dir("external-oracle-64tass-unsupported-cpu");
        let source_path = dir.join("fixture.asm");
        fs::write(&source_path, "rts\n").expect("write source");
        let request = OracleAssembleRequest {
            cpu: "65ce02",
            cpu_profile: None,
            source_path: source_path.as_path(),
            output_dir: &dir,
        };

        let err = assemble_flat_binary_with_executable(Path::new("/bin/echo"), request)
            .expect_err("unsupported cpu should fail early");
        assert!(err.summary.contains("Unsupported 64tass cpu '65ce02'"));
        assert!(err.diagnostics_path.exists());
    }

    #[test]
    fn external_oracle_64tass_builds_m6502_flat_binary_command_args() {
        let args = build_command_args(
            Path::new("/tmp/output.bin"),
            &OracleAssembleRequest {
                cpu: "m6502",
                cpu_profile: None,
                source_path: Path::new("/tmp/fixture.asm"),
                output_dir: Path::new("/tmp"),
            },
        )
        .expect("m6502 should be supported");

        assert_eq!(args[0], OsString::from("--m6502"));
        assert_eq!(args[1], OsString::from("-b"));
        assert_eq!(args[2], OsString::from("-f"));
        assert_eq!(args[3], OsString::from("-o"));
        assert_eq!(args[4], OsString::from("/tmp/output.bin"));
        assert_eq!(args[5], OsString::from("/tmp/fixture.asm"));
    }

    #[test]
    fn external_oracle_64tass_builds_65c02_flat_binary_command_args() {
        let args = build_command_args(
            Path::new("/tmp/output.bin"),
            &OracleAssembleRequest {
                cpu: "65c02",
                cpu_profile: None,
                source_path: Path::new("/tmp/fixture.asm"),
                output_dir: Path::new("/tmp"),
            },
        )
        .expect("65c02 should be supported");

        assert_eq!(args[0], OsString::from("--m65c02"));
        assert_eq!(args[1], OsString::from("-b"));
        assert_eq!(args[2], OsString::from("-f"));
        assert_eq!(args[3], OsString::from("-o"));
        assert_eq!(args[4], OsString::from("/tmp/output.bin"));
        assert_eq!(args[5], OsString::from("/tmp/fixture.asm"));
    }

    #[test]
    fn external_oracle_64tass_builds_65816_flat_binary_command_args() {
        let args = build_command_args(
            Path::new("/tmp/output.bin"),
            &OracleAssembleRequest {
                cpu: "65816",
                cpu_profile: Some(CPU_PROFILE_65816_NATIVE_MX_16_16),
                source_path: Path::new("/tmp/fixture.asm"),
                output_dir: Path::new("/tmp"),
            },
        )
        .expect("65816 should be supported");

        assert_eq!(args[0], OsString::from("--m65816"));
        assert_eq!(args[1], OsString::from("-b"));
        assert_eq!(args[2], OsString::from("-f"));
        assert_eq!(args[3], OsString::from("-o"));
        assert_eq!(args[4], OsString::from("/tmp/output.bin"));
        assert_eq!(args[5], OsString::from("/tmp/fixture.asm"));
    }

    #[test]
    fn external_oracle_64tass_65816_requires_explicit_cpu_profile() {
        let adapter = Tass64Adapter::from_inputs(AvailabilityInputs {
            opt_in_enabled: true,
            configured_bin: Some(PathBuf::from("/bin/echo")),
            path: None,
        });

        assert!(!adapter.supports_profile("65816", None));
        assert!(adapter.supports_profile("65816", Some(CPU_PROFILE_65816_NATIVE_MX_16_16)));
    }

    #[test]
    fn external_oracle_64tass_builds_45gs02_flat_binary_command_args() {
        let args = build_command_args(
            Path::new("/tmp/output.bin"),
            &OracleAssembleRequest {
                cpu: "45gs02",
                cpu_profile: None,
                source_path: Path::new("/tmp/fixture.asm"),
                output_dir: Path::new("/tmp"),
            },
        )
        .expect("45gs02 should be supported");

        assert_eq!(args[0], OsString::from("--m45gs02"));
        assert_eq!(args[1], OsString::from("-b"));
        assert_eq!(args[2], OsString::from("-f"));
        assert_eq!(args[3], OsString::from("-o"));
        assert_eq!(args[4], OsString::from("/tmp/output.bin"));
        assert_eq!(args[5], OsString::from("/tmp/fixture.asm"));
    }

    #[test]
    fn external_oracle_64tass_normalizes_cpu_numeric_and_org_directives() {
        let input = "        .cpu 6502\n        .org $1000\nrts\n";
        let normalized = normalize_source_text_for_64tass(input);

        assert!(normalized.contains(".cpu \"6502\""));
        assert!(normalized.contains("* = $1000"));
        assert!(!normalized.contains(".org"));
    }

    #[test]
    fn external_oracle_64tass_normalizes_cpu_alias_mega65() {
        let input = ".cpu mega65\n* = $1000\nrts\n";
        let normalized = normalize_source_text_for_64tass(input);

        assert!(normalized.contains(".cpu \"45gs02\""));
    }

    #[test]
    fn external_oracle_64tass_preserves_unrelated_lines_and_comments() {
        let input = "label .cpu 6502\n.cpu \"65c02\" ; keep comment\n.org $200 ; origin\n";
        let normalized = normalize_source_text_for_64tass(input);

        assert!(normalized.contains("label .cpu 6502"));
        assert!(normalized.contains(".cpu \"65c02\"; keep comment"));
        assert!(normalized.contains("* = $200; origin"));
    }
}
