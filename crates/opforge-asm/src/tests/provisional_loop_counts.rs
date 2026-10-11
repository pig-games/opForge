//! Provisional counted loops require verification independent of target CPU.
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

fn root_source(cpu: &str, body: &str) -> String {
    format!(".cpu {cpu}\n{body}")
}

fn imported_source(cpu: &str, body: &str) -> String {
    format!(
        ".module app\n.cpu {cpu}\n.use provider\n.byte provider.routine.saved1.value\n.endmodule\n\
         .module provider\n.cpu {cpu}\n.pub\nroutine .block\n\
         Info .struct\nvalue .byte ?\n.endstruct\nsaved1 .const Info {{value:7}}\n\
         {body}.bend\n.endmodule\n"
    )
}

struct SourceFiles(PathBuf);

impl Drop for SourceFiles {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

#[test]
fn provisional_loop_counts_file_graph_verifies_forward_extent() {
    for cpu in ["6502", "68020"] {
        for (counted_byte, expected) in [(".byte 0\n", None), ("", Some(vec![7]))] {
            let dir = SourceFiles(std::env::temp_dir().join(format!(
                "opforge-provisional-count-{}-{}",
                process::id(),
                unix_nanos()
            )));
            fs::create_dir_all(&dir.0).unwrap();
            let entry = dir.0.join("entry.asm");
            let output = dir.0.join("out.bin");
            fs::write(
                &entry,
                format!(".module app\n.cpu {cpu}\n.use provider\n.byte provider.routine.saved1.value\n.endmodule\n"),
            )
            .unwrap();
            fs::write(
                dir.0.join("provider.asm"),
                format!(
                    ".module provider\n.cpu {cpu}\n.pub\nroutine .block\n\
                     Info .struct\nvalue .byte ?\n.endstruct\nsaved1 .const Info {{value:7}}\n\
                     .for count\n.byte 5\n.endfor\ncount .const finish-start\n\
                     start:\n{counted_byte}finish:\n.bend\n.endmodule\n"
                ),
            )
            .unwrap();
            let cli = Cli::parse_from([
                "opforge".to_string(),
                entry.to_string_lossy().into_owned(),
                "--bin".to_string(),
                output.to_string_lossy().into_owned(),
            ]);
            let config = validate_cli(&cli).unwrap();
            let result = run_with_validated_cli_with_context(&cli, &config);
            if let Some(expected) = expected {
                assert!(result.is_ok(), "{cpu}: {result:?}");
                assert_eq!(fs::read(&output).unwrap(), expected, "{cpu}");
            } else {
                assert!(
                    result.is_err(),
                    "{cpu}: changed provisional extent accepted"
                );
                assert!(!output.exists(), "{cpu}: rejected source published output");
            }
        }
    }
}

#[test]
fn provisional_loop_counts_layout_forward_change_rejects_all_cpus() {
    let body = ".for count\n.byte 5\n.endfor\ncount .const finish-start\nstart:\n.byte 0\nfinish:\n.byte count\n";
    for cpu in ["6502", "68020"] {
        for source in [root_source(cpu, body), imported_source(cpu, body)] {
            let assembler = run_pass1(&source.lines().collect::<Vec<_>>());
            assert!(
                assembler.diagnostics.iter().any(|diagnostic| {
                    diagnostic.severity == Severity::Error
                        && diagnostic
                            .error
                            .message()
                            .contains("loop iteration count changed between passes")
                }),
                "changed provisional extent accepted: {source}\n{:?}",
                assembler.diagnostics
            );
        }
    }
}

#[test]
fn provisional_loop_counts_stable_forward_and_absolute_controls() {
    for cpu in ["6502", "68020"] {
        for (body, expected) in [
            (
                ".for count\n.byte 5\n.endfor\ncount .const finish-start\nstart:\nfinish:\n.byte count\n",
                vec![0],
            ),
            (".for count\n.byte 5\n.endfor\ncount .const 0\n.byte 9\n", vec![9]),
            (".for count\n.byte 5\n.endfor\ncount .const 2\n.byte 9\n", vec![5, 5, 9]),
        ] {
            for (source, bytes) in [
                (root_source(cpu, body), expected.clone()),
                (imported_source(cpu, body), {
                    let mut bytes = vec![7];
                    bytes.extend_from_slice(&expected);
                    bytes
                }),
            ] {
                let assembler = run_passes(&source.lines().collect::<Vec<_>>());
                let actual = assembler
                    .image()
                    .entries()
                    .unwrap()
                    .into_iter()
                    .map(|(_, byte)| byte)
                    .collect::<Vec<_>>();
                assert_eq!(actual, bytes, "{source}");
            }
        }
    }
}
