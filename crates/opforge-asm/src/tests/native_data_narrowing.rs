//! Shared data narrowing retains byte layout and reports final-pass warnings.
use super::native_iterable_loops::{build_image, oracle, scratch};
use super::*;

const BODY: &str = "start\n.byte 1,-1,256,300,2,$100000000,$100000100\nafter\n.byte after-start\n.word 65536,-1,-65537,$100000001\n.long $100000001,-1\nemit .macro value\n.byte .value,9\n.endmacro\n.emit -2\n.for item in {-1,256}\n.byte item,3\n.endfor\n.byte after-start\n";

fn source(cpu: &str, body: &str) -> String {
    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n")
}

fn expected(cpu: &str) -> Vec<u8> {
    let mut bytes = vec![1, 255, 0, 44, 2, 0, 0, 7];
    for word in [0u16, 65535, 65535, 1] {
        bytes.extend(if cpu == "6502" {
            word.to_le_bytes()
        } else {
            word.to_be_bytes()
        });
    }
    for long in [1u32, u32::MAX] {
        bytes.extend(if cpu == "6502" {
            long.to_le_bytes()
        } else {
            long.to_be_bytes()
        });
    }
    bytes.extend([254, 9, 255, 3, 0, 3, 7]);
    bytes
}

#[test]
fn native_data_narrowing_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        assert_eq!(oracle(&dir.0, &source(cpu, BODY)).unwrap(), expected(cpu));
        for body in [".emit byte,256\n", ".emit word,65536\n"] {
            assert!(oracle(&dir.0, &source(cpu, body)).is_err());
        }
    }
}

struct Case<'a> {
    cpu: &'a str,
    body: &'a str,
    name: &'a str,
    werror: bool,
    strict_failure: bool,
    warnings: usize,
}

fn run(root: &Path, image: &[u8], case: &Case<'_>) {
    let Case {
        cpu,
        body,
        name,
        werror,
        strict_failure,
        warnings,
    } = *case;
    let dir = scratch();
    let text = source(cpu, body);
    let rust = oracle(&dir.0, &text);
    assert_eq!(rust.is_err(), strict_failure);
    let files = [OpforgeNativeCliGuestFile {
        relative_path: "entry.asm",
        bytes: text.as_bytes(),
    }];
    let case = OpforgeNativeCliParityCase {
        name,
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(text.as_bytes()),
        command_template: Some(if werror {
            "Work:entry.asm --bin Work:out.bin --Werror"
        } else {
            "Work:entry.asm --bin Work:out.bin"
        }),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: if werror {
            OpforgeNativeCliProof::ExpectedFailureContaining("--Werror: warnings treated as errors")
        } else if strict_failure {
            OpforgeNativeCliProof::ExpectedFailureWithDiagnostic
        } else {
            OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/out.bin",
                rust_oracle: rust.as_ref().unwrap(),
            }
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(root, &case, image)
        .expect("fresh data-policy completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    let result = &runs[0];
    assert!(result.protocol_completed);
    assert_eq!(
        result.exit_code,
        Some(if werror || strict_failure { 20 } else { 0 })
    );
    assert_eq!(
        result
            .stdout
            .matches("warning: Value truncated to byte")
            .count(),
        warnings
    );
    if warnings != 0 {
        assert!(result.stdout.contains("[file 00000001, line "));
        assert!(!result.stdout.contains("line 00000000]"));
        assert!(result.stdout.contains("source: Work:entry.asm"));
    }
    if !strict_failure {
        assert_eq!(
            &result.captured_artifacts[&PathBuf::from("Work/out.bin")],
            rust.as_ref().unwrap(),
            "including output under --Werror"
        );
    }
    eprintln!("DATA_NARROWING case={name} cpu={cpu} warnings={warnings} output_bytes={:?} seconds={:?} image_bytes={}", rust.as_ref().ok().map(Vec::len), result.start_to_done_host_seconds, image.len());
}

#[test]
#[ignore = "requires FS-UAE; data bytes, warning provenance and --Werror"]
fn native_data_narrowing_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_DATA_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let selection = std::env::var("OPFORGE_DATA_CASES").unwrap_or_else(|_| "all".into());
    let selected = |name: &str| {
        selection
            .split(',')
            .any(|part| part == "all" || part == name)
    };
    let mut failures = Vec::new();
    let mut attempted = 0;
    let mut check = |cpu, body, name, werror, strict, warnings| {
        if selected(name) {
            attempted += 1;
            let case = Case {
                cpu,
                body,
                name,
                werror,
                strict_failure: strict,
                warnings,
            };
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| run(&root, &image, &case)))
                .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
    };
    for cpu in ["68020", "6502"] {
        check(cpu, BODY, "matrix", false, false, 7);
    }
    check("68020", ".byte 300\n", "werror", true, false, 1);
    check("68020", ".emit byte,256\n", "strict-byte", false, true, 0);
    check("68020", ".emit word,65536\n", "strict-word", false, true, 0);
    assert!(attempted > 0, "no data-policy cases selected: {selection}");
    assert!(
        failures.is_empty(),
        "data-policy failures: {failures:?}; all cases attempted"
    );
}
