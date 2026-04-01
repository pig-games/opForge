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
const ORACLE_ID: &str = "64tass";
const ORACLE_PROFILE: &str = "tass_6502_flat_binary";
const SUPPORTED_FAMILY: &str = "mos6502";

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
        canonical_cpu(cpu).is_some() && profile.is_none()
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
    let command_args = match build_command_args(&output_path, &request) {
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
        "unsupported 64tass cpu '{cpu}'; current slice supports m6502 and 65c02\n"
    )
}

fn canonical_cpu(cpu: &str) -> Option<&'static str> {
    match cpu.to_ascii_lowercase().as_str() {
        "6502" | "m6502" => Some("m6502"),
        "65c02" | "m65c02" => Some("65c02"),
        _ => None,
    }
}

fn cpu_flag(cpu: &str) -> Option<&'static str> {
    match canonical_cpu(cpu) {
        Some("m6502") => Some("--m6502"),
        Some("65c02") => Some("--m65c02"),
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
        let request = OracleAssembleRequest {
            cpu: "65816",
            cpu_profile: None,
            source_path: Path::new("fixture.asm"),
            output_dir: &dir,
        };

        let err = assemble_flat_binary_with_executable(Path::new("/bin/echo"), request)
            .expect_err("unsupported cpu should fail early");
        assert!(err.summary.contains("Unsupported 64tass cpu '65816'"));
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
}