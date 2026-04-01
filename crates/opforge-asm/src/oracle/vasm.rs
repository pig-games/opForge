use crate::oracle::{
    ExternalOracleAdapter, OracleAssembleFailure, OracleAssembleRequest, OracleAssembleSuccess,
    OracleAvailability,
};
use std::env;
use std::ffi::OsString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const OPT_IN_ENV: &str = "OPFORGE_EXTERNAL_ORACLE_VASM";
const BIN_ENV: &str = "OPFORGE_VASM_BIN";
const DEFAULT_EXECUTABLE: &str = "vasmm68k_mot";
const OUTPUT_FILENAME: &str = "output.bin";
const STDOUT_FILENAME: &str = "vasm.stdout.txt";
const STDERR_FILENAME: &str = "vasm.stderr.txt";
const ORACLE_ID: &str = "vasm";
const ORACLE_PROFILE: &str = "m68k_mot_flat_binary";
const SUPPORTED_FAMILY: &str = "motorola68000";
const NO_EXTRA_FLAGS: &[&str] = &[];
const FPU_68881_FLAGS: &[&str] = &["-m68881"];
const FPU_68882_FLAGS: &[&str] = &["-m68882"];

#[derive(Debug, Clone)]
pub(crate) struct VasmAdapter {
    executable: Option<PathBuf>,
    availability: OracleAvailability,
}

impl VasmAdapter {
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
                    "Set {OPT_IN_ENV}=1 to run the external-oracle vasm success-path fixtures"
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
                    "Could not find {DEFAULT_EXECUTABLE} on PATH. Set {BIN_ENV} to a vasm executable to enable the external-oracle fixtures"
                )),
            },
        }
    }
}

impl Default for VasmAdapter {
    fn default() -> Self {
        Self::from_env()
    }
}

impl ExternalOracleAdapter for VasmAdapter {
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
        cpu_flag(cpu).is_some()
    }

    fn supports_profile(&self, cpu: &str, profile: Option<&str>) -> bool {
        profile_flags(cpu, profile).is_some()
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
                OracleAvailability::Ready => "vasm executable unexpectedly missing".to_string(),
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
            let summary = if diagnostics_text.contains("cpu/profile combination") {
                format!(
                    "Unsupported vasm cpu/profile combination '{}'/'{}'",
                    request.cpu,
                    request.cpu_profile.unwrap_or("<none>")
                )
            } else {
                format!("Unsupported vasm cpu '{}'", request.cpu)
            };
            return Err(OracleAssembleFailure {
                diagnostics_path: stderr_path.clone(),
                stdout_path: Some(stdout_path),
                stderr_path: Some(stderr_path),
                diagnostics_text,
                summary,
            });
        }
    };

    let output = match Command::new(executable).args(&command_args).output() {
        Ok(output) => output,
        Err(err) => {
            let _ = fs::write(&stdout_path, "");
            let diagnostics_text = format!("failed to execute vasm: {err}\n");
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
            summary: format!("vasm exited with status {code}"),
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
            diagnostics_text: format!("Read vasm output {}: {err}\n", output_path.display()),
            summary: format!("Read vasm output {}: {err}", output_path.display()),
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
    let profile_flags = profile_flags(request.cpu, request.cpu_profile)
        .ok_or_else(|| unsupported_profile_message(request.cpu, request.cpu_profile))?;

    let mut args = vec![OsString::from("-Fbin"), OsString::from(cpu_flag)];
    args.extend(profile_flags.iter().copied().map(OsString::from));
    args.push(OsString::from("-o"));
    args.push(output_path.as_os_str().to_os_string());
    args.push(request.source_path.as_os_str().to_os_string());
    Ok(args)
}

fn unsupported_cpu_message(cpu: &str) -> String {
    format!("unsupported vasm cpu '{cpu}'; current slice supports 68000 through 68040\n")
}

fn unsupported_profile_message(cpu: &str, profile: Option<&str>) -> String {
    format!(
        "unsupported vasm cpu/profile combination '{cpu}'/'{}'\n",
        profile.unwrap_or("<none>")
    )
}

fn canonical_cpu(cpu: &str) -> Option<&'static str> {
    match cpu.to_ascii_lowercase().as_str() {
        "68000" | "mc68000" | "m68000" => Some("68000"),
        "68010" | "mc68010" | "m68010" => Some("68010"),
        "68020" | "mc68020" | "m68020" => Some("68020"),
        "68030" | "mc68030" | "m68030" => Some("68030"),
        "68040" | "mc68040" | "m68040" => Some("68040"),
        _ => None,
    }
}

fn cpu_flag(cpu: &str) -> Option<&'static str> {
    match canonical_cpu(cpu) {
        Some("68000") => Some("-m68000"),
        Some("68010") => Some("-m68010"),
        Some("68020") => Some("-m68020"),
        Some("68030") => Some("-m68030"),
        Some("68040") => Some("-m68040"),
        _ => None,
    }
}

fn profile_flags(cpu: &str, profile: Option<&str>) -> Option<&'static [&'static str]> {
    match (canonical_cpu(cpu)?, profile) {
        (_, None) => Some(NO_EXTRA_FLAGS),
        ("68020" | "68030", Some("fpu-68881")) => Some(FPU_68881_FLAGS),
        ("68020" | "68030", Some("fpu-68882")) => Some(FPU_68882_FLAGS),
        ("68030", Some("mmu-68030")) => Some(NO_EXTRA_FLAGS),
        ("68040", Some("fpu-68040" | "mmu-68040")) => Some(NO_EXTRA_FLAGS),
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
    fn external_oracle_vasm_availability_is_disabled_without_opt_in() {
        let adapter = VasmAdapter::from_inputs(AvailabilityInputs {
            opt_in_enabled: false,
            configured_bin: None,
            path: None,
        });
        assert!(matches!(
            adapter.availability(),
            OracleAvailability::Disabled(reason)
                if reason.contains("OPFORGE_EXTERNAL_ORACLE_VASM")
        ));
    }

    #[test]
    fn external_oracle_vasm_availability_reports_missing_binary() {
        let adapter = VasmAdapter::from_inputs(AvailabilityInputs {
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
    fn external_oracle_vasm_availability_prefers_configured_binary() {
        let dir = temp_dir("external-oracle-vasm-configured-bin");
        let configured = dir.join("opforge-vasm68k");
        fs::write(&configured, "#!/bin/sh\nexit 0\n").expect("write fake binary");

        let adapter = VasmAdapter::from_inputs(AvailabilityInputs {
            opt_in_enabled: true,
            configured_bin: Some(configured.clone()),
            path: None,
        });

        assert_eq!(adapter.availability(), OracleAvailability::Ready);
        assert_eq!(adapter.executable.as_deref(), Some(configured.as_path()));
    }

    #[test]
    fn external_oracle_vasm_rejects_unsupported_cpu_before_spawn() {
        let dir = temp_dir("external-oracle-vasm-unsupported-cpu");
        let request = OracleAssembleRequest {
            cpu: "68060",
            cpu_profile: None,
            source_path: Path::new("fixture.asm"),
            output_dir: &dir,
        };

        let err = assemble_flat_binary_with_executable(Path::new("/bin/echo"), request)
            .expect_err("unsupported cpu should fail early");
        assert!(err.summary.contains("Unsupported vasm cpu '68060'"));
        assert!(err.diagnostics_path.exists());
    }

    #[test]
    fn external_oracle_vasm_supports_family_cpu_flags_through_68040() {
        assert_eq!(cpu_flag("68000"), Some("-m68000"));
        assert_eq!(cpu_flag("68010"), Some("-m68010"));
        assert_eq!(cpu_flag("68020"), Some("-m68020"));
        assert_eq!(cpu_flag("68030"), Some("-m68030"));
        assert_eq!(cpu_flag("68040"), Some("-m68040"));
    }

    #[test]
    fn external_oracle_vasm_builds_profile_specific_command_args() {
        let args = build_command_args(
            Path::new("/tmp/output.bin"),
            &OracleAssembleRequest {
                cpu: "68020",
                cpu_profile: Some("fpu-68881"),
                source_path: Path::new("/tmp/fnop.asm"),
                output_dir: Path::new("/tmp"),
            },
        )
        .expect("68881 profile should be supported");

        assert_eq!(args[0], OsString::from("-Fbin"));
        assert_eq!(args[1], OsString::from("-m68020"));
        assert_eq!(args[2], OsString::from("-m68881"));
        assert_eq!(args[3], OsString::from("-o"));
        assert_eq!(args[4], OsString::from("/tmp/output.bin"));
        assert_eq!(args[5], OsString::from("/tmp/fnop.asm"));
    }
}
