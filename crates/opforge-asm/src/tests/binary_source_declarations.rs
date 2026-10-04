//! Shared scalar declaration policy and source-order immutable/mutable execution.
use super::*;
use crate::fs_uae_smoke::{
    compact_cli_input::{assemble_cli, assemble_cli_with_defines},
    run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliGuestFile,
    OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase, OpforgeNativeCliProof,
};
use crate::native_package_build::{build_native_packages, EmbedSelection};
use serde_json::json;

#[path = "binary_source_hunk_traversal.rs"]
mod hunk_traversal;

struct Cleanup(PathBuf);
impl Drop for Cleanup {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn source(cpu: &str, style: &str) -> String {
    let declaration = |name: &str, value: &str| match style {
        "equals" => format!("{name} = {value}\n"),
        "bare" => format!("{name} .const {value}\n"),
        "colon" => format!("{name}: .const {value}\n"),
        _ => unreachable!(),
    };
    let mut body = String::new();
    for (name, value) in [
        ("one", "later+1"),
        ("negative", "-17"),
        ("quotient", "negative/7"),
        ("location", "$"),
    ] {
        body.push_str(&declaration(name, value));
    }
    body.push_str("later = 7\nstart\n.byte one,negative+20\nend\n");
    body.push_str(&declaration("distance", "end-start"));
    body.push_str(".word distance,location\n.long quotient\n");
    for (flag, value, chosen) in [("off", "0", "pickedOff"), ("on", "1", "pickedOn")] {
        body.push_str(&declaration(flag, value));
        body.push_str(&format!(".if {flag}\n"));
        body.push_str(&declaration(chosen, "11"));
        body.push_str(".else\n");
        body.push_str(&declaration(chosen, "22"));
        body.push_str(".endif\n");
    }
    body.push_str(".byte pickedOff,pickedOn\nproduce .macro value\n");
    body.push_str(&declaration("captured", ".value+1"));
    body.push_str(".byte captured\n.endmacro\nfirst .produce 2\nsecond .produce 5\n");
    body.push_str(if cpu == "6502" {
        " lda #one\n"
    } else {
        " moveq #one,d0\n"
    });
    format!(".module probe\n.cpu {cpu}\n.org $1000\n{body}.endmodule\n")
}

fn oracle(dir: &Path, source: &str) -> Result<Vec<u8>, String> {
    project_oracle(dir, source, false, &[])
}

fn module_roots(files: &[(String, String)]) -> Vec<&Path> {
    files
        .iter()
        .filter_map(|(path, _)| Path::new(path).parent())
        .filter(|path| !path.as_os_str().is_empty())
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect()
}

fn project_oracle(
    dir: &Path,
    source: &str,
    hunk: bool,
    files: &[(String, String)],
) -> Result<Vec<u8>, String> {
    fs::create_dir_all(dir).unwrap();
    let input = dir.join("main.asm");
    let output = dir.join(if hunk { "out.hunk" } else { "output.bin" });
    fs::write(&input, source).unwrap();
    for (path, contents) in files {
        let path = dir.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }
    let mut args = vec!["opForge".to_string(), input.to_string_lossy().into_owned()];
    if !hunk {
        args.extend(["--bin".into(), output.to_string_lossy().into_owned()]);
    }
    for root in module_roots(files) {
        args.extend(["-M".into(), dir.join(root).to_string_lossy().into_owned()]);
    }
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).map_err(|error| format!("{error:?}"))?;
    config.out_dir = Some(dir.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config).map_err(|error| format!("{error:?}"))?;
    fs::read(output).map_err(|error| format!("{error:?}"))
}

fn invalid_sources() -> Vec<(&'static str, String)> {
    [
        ("duplicate", "value = 1\nvalue .const 2\n.byte value\n"),
        (
            "cycle",
            "left .const right+1\nright .const left+1\n.byte left\n",
        ),
        ("unlabelled", ".const 7\n.byte 1\n"),
        ("missing-value", "value .const\n.byte 1\n"),
    ]
    .map(|(name, body)| {
        (
            name,
            format!(".module probe\n.cpu m68020\n{body}.endmodule\n"),
        )
    })
    .into()
}

#[test]
fn compact_const_rust_oracles() {
    let dir = create_temp_dir("const-directive-oracles");
    let _cleanup = Cleanup(dir.clone());
    for cpu in ["6502", "m68020"] {
        let expected = if cpu == "6502" {
            vec![8, 3, 2, 0, 0, 16, 254, 255, 255, 255, 22, 11, 3, 6, 0xa9, 8]
        } else {
            vec![8, 3, 0, 2, 16, 0, 255, 255, 255, 254, 22, 11, 3, 6, 0x70, 8]
        };
        for style in ["equals", "bare", "colon"] {
            assert_eq!(
                oracle(&dir, &source(cpu, style)).unwrap(),
                expected,
                "{cpu}/{style}"
            );
        }
    }
    for (name, source) in invalid_sources() {
        assert!(oracle(&dir, &source).is_err(), "{name} must be rejected");
    }
}

#[test]
#[ignore = "requires fresh FS-UAE scalar .const parity and rejection controls"]
fn compact_const_fs_uae() {
    let mut cases = Vec::new();
    for cpu in ["6502", "m68020"] {
        for style in ["equals", "bare", "colon"] {
            cases.push((format!("{cpu}/{style}"), cpu, source(cpu, style), true));
        }
    }
    cases.extend(
        invalid_sources()
            .into_iter()
            .map(|(name, source)| (name.into(), "m68020", source, false)),
    );
    native_cases(cases, "OPFORGE_CONST_REPORT");
}

type DeclarationCase = (String, &'static str, String, bool);

#[derive(Clone, Copy, Debug)]
enum NativeExpected {
    MatchRust,
    MatchHunk,
    RejectHunk,
    RejectInvalid,
    RejectInvalidHunk,
    RejectLayout,
}
const LAYOUT_DIAGNOSTIC: &str = "mutable declarations are not supported in mapped outputs";

fn digest(bytes: &[u8]) -> String {
    let value = bytes.iter().fold(0xcbf29ce484222325u64, |hash, byte| {
        (hash ^ u64::from(*byte)).wrapping_mul(0x100000001b3)
    });
    format!("{value:016x}")
}

fn native_cases(cases: Vec<DeclarationCase>, report_env: &str) {
    native_expected_cases(
        cases
            .into_iter()
            .map(|(name, cpu, source, valid)| {
                (
                    name,
                    cpu,
                    source,
                    if valid {
                        NativeExpected::MatchRust
                    } else {
                        NativeExpected::RejectInvalid
                    },
                )
            })
            .collect(),
        report_env,
    );
}

struct NativeCase {
    name: String,
    cpu: &'static str,
    source: String,
    expectation: NativeExpected,
    files: Vec<(String, String)>,
}

fn native_expected_cases(
    cases: Vec<(String, &'static str, String, NativeExpected)>,
    report_env: &str,
) {
    native_project_cases(
        cases
            .into_iter()
            .map(|(name, cpu, source, expectation)| NativeCase {
                name,
                cpu,
                source,
                expectation,
                files: Vec::new(),
            })
            .collect(),
        report_env,
    );
}

fn native_project_cases(cases: Vec<NativeCase>, report_env: &str) {
    let selected = std::env::var("OPFORGE_DECLARATION_CASES").ok();
    let cases: Vec<_> = cases
        .into_iter()
        .filter(|case| {
            selected
                .as_ref()
                .is_none_or(|names| names.split(',').any(|name| name == case.name))
        })
        .collect();
    assert!(
        !cases.is_empty(),
        "declaration case selection matched no cases"
    );
    let root = workspace_root();
    let dir = std::env::temp_dir().join(format!(
        "opforge-declarations-{}-{}",
        process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    fs::create_dir(&dir).unwrap();
    let _cleanup = Cleanup(dir.clone());
    // A caller may supply an isolated source snapshot for an exact before/after run.
    let native_root = std::env::var_os("OPFORGE_DECLARATION_NATIVE_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|| root.clone());
    let build = build_native_packages(
        &engine::build_default_asm_registry(),
        &dir.join("build"),
        &native_root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::Targets(vec!["m68020".into()]),
    )
    .unwrap();
    let telemetry = std::env::var("OPFORGE_DECLARATION_TELEMETRY").as_deref() == Ok("1");
    let defines: &[&str] = if telemetry {
        &[
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_MEMORY_TELEMETRY",
            "OPFORGE_PREPARATION_PROGRESS",
        ]
    } else {
        &[]
    };
    let image = if telemetry {
        assemble_cli_with_defines(&native_root, &build, defines)
    } else {
        assemble_cli(&native_root, &build)
    };
    let packages = ["m6502--transparent.bin", "m68020--motorola68k.bin"].map(|name| {
        (
            name,
            fs::read(build.output_dir.join("packages").join(name)).unwrap(),
        )
    });
    let report = std::env::var_os(report_env)
        .map(PathBuf::from)
        .unwrap_or_else(|| panic!("absolute {report_env} required"));
    assert!(report.is_absolute());
    let mut rows = Vec::new();
    for (
        index,
        NativeCase {
            name,
            cpu,
            source,
            expectation,
            files: project_files,
        },
    ) in cases.iter().enumerate()
    {
        let case_dir = dir.join(index.to_string());
        let is_hunk = matches!(
            expectation,
            NativeExpected::MatchHunk
                | NativeExpected::RejectHunk
                | NativeExpected::RejectInvalidHunk
        );
        let valid = matches!(
            expectation,
            NativeExpected::MatchRust | NativeExpected::MatchHunk
        );
        let rust_valid = !matches!(
            expectation,
            NativeExpected::RejectInvalid | NativeExpected::RejectInvalidHunk
        );
        let expected = project_oracle(&case_dir, source, is_hunk, project_files);
        assert_eq!(
            expected.is_ok(),
            rust_valid,
            "Rust oracle {name}: {expected:?}"
        );
        let expected = expected.unwrap_or_default();
        let mut guest = vec![("main.asm".to_string(), source.as_bytes().to_vec())];
        guest.extend(
            project_files
                .iter()
                .map(|(path, contents)| (path.clone(), contents.as_bytes().to_vec())),
        );
        guest.extend(
            packages
                .iter()
                .map(|(name, bytes)| (format!("packages/{name}"), bytes.clone())),
        );
        let files = guest
            .iter()
            .map(|(name, bytes)| OpforgeNativeCliGuestFile {
                relative_path: name,
                bytes,
            })
            .collect::<Vec<_>>();
        let mut command = if is_hunk {
            format!("--cpu {cpu} Work:main.asm -P Work:packages")
        } else {
            format!("--cpu {cpu} Work:main.asm -P Work:packages --bin Work:output.bin")
        };
        for root in module_roots(project_files) {
            command.push_str(&format!(" -M Work:{}", root.display()));
        }
        let artifact = if is_hunk {
            "Work/out.hunk"
        } else {
            "Work/output.bin"
        };
        let case = OpforgeNativeCliParityCase {
            name,
            cpu_override: "68020",
            extra_assembly_defines: defines,
            source_override: Some(source.as_bytes()),
            command_template: Some(&command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &files,
            proof: if valid {
                OpforgeNativeCliProof::ExactArtifact {
                    relative_path: artifact,
                    rust_oracle: &expected,
                }
            } else {
                OpforgeNativeCliProof::ExpectedFailureContaining(
                    if matches!(expectation, NativeExpected::RejectLayout) {
                        LAYOUT_DIAGNOSTIC
                    } else {
                        "binary source: unsupported or invalid input"
                    },
                )
            },
        };
        let mut row = json!({
            "name": name, "expected_success": valid, "rust_expected_success": rust_valid, "expectation": format!("{expectation:?}"), "success": false,
            "source_bytes": source.len(), "source_fnv1a64": digest(source.as_bytes()),
            "project_files": project_files.iter().map(|(path, contents)| json!({"path":path,"bytes":contents.len(),"fnv1a64":digest(contents.as_bytes())})).collect::<Vec<_>>(),
            "image_bytes": image.len(), "image_fnv1a64": digest(&image),
            "oracle_bytes": expected.len(), "oracle_fnv1a64": digest(&expected),
            "command": command,
            "assembly_defines": defines,
            "packages": packages.iter().map(|(name, bytes)| json!({"name":name,"bytes":bytes.len(),"fnv1a64":digest(bytes)})).collect::<Vec<_>>(),
        });
        match run_prebuilt_compact_cli_case_from_env(&root, &case, &image) {
            Ok(FsUaeSmokeOutcome::Completed { runs }) => {
                assert_eq!(runs.len(), 1);
                let run = &runs[0];
                row["success"] = json!(
                    run.protocol_completed
                        && if valid {
                            run.success && run.exit_code == Some(0)
                        } else {
                            run.exit_code.is_some_and(|code| code != 0)
                        }
                );
                if matches!(
                    expectation,
                    NativeExpected::RejectLayout
                        | NativeExpected::RejectHunk
                        | NativeExpected::RejectInvalidHunk
                ) {
                    let no_output = !run
                        .captured_artifacts
                        .contains_key(&PathBuf::from(artifact));
                    row["no_output"] = json!(no_output);
                    row["success"] = json!(row["success"] == true && no_output);
                }
                row["native_seconds"] = json!(run.start_to_done_host_seconds);
                if telemetry {
                    row["native_stdout"] = json!(run.stdout);
                }
                row["exit_code"] = json!(run.exit_code);
            }
            Ok(FsUaeSmokeOutcome::Skipped(reason)) | Err(reason) => row["error"] = json!(reason),
        }
        eprintln!("DECLARATION_PARITY {row}");
        rows.push(row);
        fs::write(&report, serde_json::to_vec_pretty(&rows).unwrap()).unwrap();
    }
    assert!(
        rows.iter().all(|row| row["success"] == true),
        "inspect {}",
        report.display()
    );
}

fn mutable_probe(body: &str, cpu: &str) -> String {
    format!(".module probe\n.cpu {cpu}\n{body}.endmodule\n")
}

#[test]
fn compact_mutable_rust_oracles() {
    let dir = create_temp_dir("mutable-semantics");
    let _cleanup = Cleanup(dir.clone());
    for (body, expected) in [
        ("n .var 1\n.byte n\nn .set n+1\n.byte n\nn .var -17\n.long n/7\nn .set -21\n.long n/7\n", vec![1,2,254,255,255,255,253,255,255,255]),
        ("n .set 7\n.byte n\n", vec![7]),
        ("seed .var nextValue\nc .const seed\n.byte c\nnextValue .var 7\n", vec![7]),
        ("seed .var nextValue\nc .const seed\nd .const c\n.byte d\nnextValue .var 7\n", vec![7]),
        ("c .const n\n.byte c\nn .var 7\n", vec![7]),
        ("n .var 1\n.byte n\nc .const n\nn .set 2\n.byte c,n\n", vec![1,1,2]),
        ("n .var later\n.byte n\nlater = 7\n", vec![7]),
        (".byte n\nn .var 7\n", vec![7]),
        ("n .var n+1\n.byte n\n", vec![2]),
        ("n .var 1\n.if n\n.byte 11\n.else\n.byte 22\n.endif\nn .set 0\n.if n\n.byte 11\n.else\n.byte 22\n.endif\n", vec![11,22]),
    ] {
        assert_eq!(oracle(&dir, &mutable_probe(body,"6502")).unwrap(), expected, "{body}");
    }
    for (cpu, body, expected) in [
        (
            "6502",
            "n .var 1\n lda #n\nn .set 2\n lda #n\n",
            vec![0xa9, 1, 0xa9, 2],
        ),
        (
            "m68020",
            "n .var 1\n moveq #n,d0\nn .set 2\n moveq #n,d0\n",
            vec![0x70, 1, 0x70, 2],
        ),
    ] {
        assert_eq!(oracle(&dir, &mutable_probe(body, cpu)).unwrap(), expected);
    }
    for (_, body) in mutable_rejections() {
        assert!(
            oracle(&dir, &mutable_probe(body, "6502")).is_err(),
            "{body}"
        );
    }
}

fn mutable_rejections() -> Vec<(&'static str, &'static str)> {
    vec![
        ("readonly-update", "n .const 1\nn .var 2\n.byte n\n"),
        ("mutable-to-const", "n .var 1\nn .const 2\n.byte n\n"),
        ("mutable-to-equals", "n .var 1\nn = 2\n.byte n\n"),
        ("missing-label", ".set 1\n.byte 2\n"),
        ("missing-value", "n .var\n.byte 2\n"),
    ]
}

#[test]
fn compact_mutable_section_rust_oracle() {
    let dir = create_temp_dir("mutable-section-oracle");
    let _cleanup = Cleanup(dir.clone());
    let source = hunk_layout_source();
    let input = dir.join("main.asm");
    fs::write(&input, source).unwrap();
    let cli = Cli::parse_from(["opForge".to_string(), input.to_string_lossy().into_owned()]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    let bytes = fs::read(dir.join("out.hunk")).unwrap();
    // Output section order differs from source traversal: CODE sees 2, DATA 1 then 3.
    assert!(bytes
        .windows(12)
        .any(|b| b == [0, 0, 3, 0xe9, 0, 0, 0, 1, 0, 0, 0, 2]));
    assert!(bytes
        .windows(16)
        .any(|b| b == [0, 0, 3, 0xea, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 3]));
}

fn mutable_source(cpu: &str, colon: bool) -> String {
    let separator = if colon { ":" } else { "" };
    let body = r#"position .var $
.word position
.byte 3
position .set $
.word position
n .var 1
.byte n
c .const n
n .set n+1
.byte c,n
n .var -17
.long n/7
n .set -21
.long n/7
wide .var $ffffffff+1
.byte wide&255
.long wide/2
.if wide
.byte 1
.else
.byte 99
.endif
forward .var later
.byte forward
later = 7
on .var 1
.if on
.byte 11
.else
.byte 22
.endif
on .set 0
.if on
.byte 11
.else
.byte 22
.endif
produce .macro value
localValue .var .value
.byte localValue
localValue .set localValue+1
.byte localValue
.endmacro
first .produce 3
second .produce 5
"#;
    let body = body
        .lines()
        .map(|line| {
            if line.contains(" .var ") || line.contains(" .set ") {
                let (label, rest) = line.split_once(' ').unwrap();
                format!("{label}{separator} {rest}\n")
            } else {
                format!("{line}\n")
            }
        })
        .collect::<String>();
    format!(".module probe\n.cpu {cpu}\n.org $1000\n{body}.endmodule\n")
}

#[test]
fn compact_mutable_composite_rust_oracles() {
    let dir = create_temp_dir("mutable-composite");
    let _cleanup = Cleanup(dir.clone());
    for cpu in ["6502", "m68020"] {
        let mut expected = if cpu == "6502" {
            vec![
                0, 16, 3, 3, 16, 1, 1, 2, 254, 255, 255, 255, 253, 255, 255, 255,
            ]
        } else {
            vec![
                16, 0, 3, 16, 3, 1, 1, 2, 255, 255, 255, 254, 255, 255, 255, 253,
            ]
        };
        expected.extend(if cpu == "6502" {
            vec![0, 0, 0, 0, 128]
        } else {
            vec![0, 128, 0, 0, 0]
        });
        expected.extend([1, 7, 11, 22, 3, 4, 5, 6]);
        for colon in [false, true] {
            assert_eq!(
                oracle(&dir, &mutable_source(cpu, colon)).unwrap(),
                expected,
                "{cpu}/colon={colon}"
            );
        }
    }
}

#[test]
#[ignore = "requires fresh FS-UAE mutable scalar declarations, replay, scopes and rejection controls"]
fn compact_mutable_fs_uae() {
    let mut cases = Vec::new();
    for cpu in ["6502", "m68020"] {
        for colon in [false, true] {
            cases.push((
                format!("{cpu}/mutable/colon={colon}"),
                cpu,
                mutable_source(cpu, colon),
                true,
            ));
        }
    }
    for (name, body) in [
        ("set-creates", "n .set 7\n.byte n\n"),
        (
            "snapshot-update",
            "n .var 1\nc .const n\nn .set n+1\n.byte c,n\n",
        ),
        (
            "forward-snapshot",
            "seed .var nextValue\nc .const seed\n.byte c\nnextValue .var 7\n",
        ),
        (
            "derived-snapshot",
            "seed .var nextValue\nc .const seed\nd .const c\n.byte d\nnextValue .var 7\n",
        ),
        ("unresolved-snapshot", "c .const n\n.byte c\nn .var 7\n"),
        (
            "forward-and-self",
            ".byte n\nn .var 7\nselfValue .var selfValue+1\n.byte selfValue\n",
        ),
    ] {
        cases.push((name.into(), "6502", mutable_probe(body, "6502"), true));
    }
    for (name, cpu, body) in [
        (
            "operand-6502",
            "6502",
            "n .var 1\n lda #n\nn .set 2\n lda #n\n",
        ),
        (
            "operand-m68020",
            "m68020",
            "n .var 1\n moveq #n,d0\nn .set 2\n moveq #n,d0\n",
        ),
    ] {
        cases.push((name.into(), cpu, mutable_probe(body, cpu), true));
    }
    cases.extend(
        mutable_rejections()
            .into_iter()
            .map(|(name, body)| (name.into(), "m68020", mutable_probe(body, "m68020"), false)),
    );
    native_cases(cases, "OPFORGE_MUTABLE_REPORT");
}

fn layout_source(maps: usize) -> String {
    let mut body = String::from(".module main\n.cpu m6502\n.region rom_a, $1000, $1004\n");
    if maps == 2 {
        body.push_str(".region rom_b, $1005, $10ff\n");
    } else {
        body = body.replace("$1004", "$10ff");
    }
    for index in 0..maps {
        let letter = if index == 0 { "a" } else { "b" };
        body.push_str(&format!(
            ".use dep_{letter} (entry) as lib_{letter} map {{ code_{letter} -> app_{letter} }}\n"
        ));
    }
    for index in 0..maps.max(1) {
        let letter = if index == 0 { "a" } else { "b" };
        body.push_str(&format!(".section app_{letter}\n"));
        if index == 0 {
            body.push_str("n .var 1\n");
        }
        body.push_str(".byte n\nn .set n+1\n.byte n\n");
        if maps > 0 {
            body.push_str(&format!(".word lib_{letter}.entry\n"));
        }
        body.push_str(".endsection\n");
    }
    for index in 0..maps.max(1) {
        let letter = if index == 0 { "a" } else { "b" };
        body.push_str(&format!(".place app_{letter} in rom_{letter}\n"));
    }
    body.push_str(".endmodule\n.end\n");
    body
}

fn layout_files(maps: usize) -> Vec<(String, String)> {
    (0..maps).map(|index| {
        let letter = if index == 0 { "a" } else { "b" };
        (format!("library/dep_{letter}.asm"), format!(".module dep_{letter}\n.cpu m6502\n.pub\n.section code_{letter}, logical\nentry .block\n.byte $11\n.bend\n.endsection\n.endmodule\n.end\n"))
    }).collect()
}

fn concrete_pair_source() -> &'static str {
    ".module main\n.cpu 6502\nn .var 1\n.region rom_a,$1000,$1001\n.region rom_b,$1002,$10ff\n.section app_a\n.byte n\nn .set n+1\n.byte n\n.endsection\n.section app_b\n.byte n\nn .set n+1\n.byte n\n.endsection\n.place app_a in rom_a\n.place app_b in rom_b\n.endmodule\n"
}

fn hunk_layout_source() -> &'static str {
    ".module probe\n.cpu m68020\nn .var 1\n.section data,kind=data\n.long n\nn .set n+1\n.endsection\n.section code,kind=code\n.long n\nn .set n+1\n.endsection\n.section data,kind=data\n.long n\n.endsection\n.output \"out.hunk\",format=hunk,sections=code,data\n.endmodule\n"
}

#[test]
fn compact_mutable_layout_rust_oracles() {
    let dir = create_temp_dir("mutable-layout-oracles");
    let _cleanup = Cleanup(dir.clone());
    for (maps, expected) in [
        (0, vec![1, 2]),
        (1, vec![1, 2, 4, 16, 0x11]),
        (2, vec![1, 2, 4, 16, 0x11, 2, 3, 9, 16, 0x11]),
    ] {
        assert_eq!(
            project_oracle(&dir, &layout_source(maps), false, &layout_files(maps)).unwrap(),
            expected,
            "maps={maps}"
        );
    }
    assert_eq!(oracle(&dir, concrete_pair_source()).unwrap(), [1, 2, 2, 3]);
    assert!(oracle(&dir, hunk_layout_source()).is_ok());
}

#[test]
#[ignore = "requires fresh FS-UAE source-order Hunk and single-sweep mutable controls"]
fn compact_mutable_layout_fs_uae() {
    let readonly = hunk_layout_source()
        .replace("n .var 1\n", "n = 1\n")
        .replace("n .set n+1\n", "");
    let cases = vec![
        (
            "hunk-mutable",
            "m68020",
            hunk_layout_source().into(),
            NativeExpected::MatchHunk,
        ),
        (
            "single-section",
            "6502",
            layout_source(0),
            NativeExpected::MatchRust,
        ),
        (
            "flat-mutable",
            "6502",
            mutable_source("6502", false),
            NativeExpected::MatchRust,
        ),
        // $2b is the declaration marker; payload bytes are never scanned as records.
        (
            "hunk-payload-marker",
            "m68020",
            readonly.replace("n = 1", "n = $2b"),
            NativeExpected::MatchHunk,
        ),
        (
            "hunk-unused-macro",
            "m68020",
            readonly.replace("n = 1\n", "unused .macro\nlocal .var 1\n.endmacro\nn = 1\n"),
            NativeExpected::MatchHunk,
        ),
        (
            "hunk-inactive-declaration",
            "m68020",
            readonly.replace("n = 1\n", ".if 0\nignored .var 1\n.endif\nn=1\n"),
            NativeExpected::MatchHunk,
        ),
        (
            "hunk-readonly",
            "m68020",
            readonly.clone(),
            NativeExpected::MatchHunk,
        ),
        (
            "single-sweep-two-regions",
            "6502",
            concrete_pair_source().into(),
            NativeExpected::MatchRust,
        ),
        (
            "hunk-expanded-macro",
            "m68020",
            readonly
                .replace(
                    "n = 1\n",
                    "n=1\nproduce .macro\nlocalValue .var 1\n.long localValue\n.endmacro\n",
                )
                .replace(
                    ".section data,kind=data\n",
                    ".section data,kind=data\n.produce\n",
                ),
            NativeExpected::MatchHunk,
        ),
    ];
    native_expected_cases(
        cases
            .into_iter()
            .map(|(name, cpu, source, expectation)| (name.into(), cpu, source, expectation))
            .collect(),
        "OPFORGE_MUTABLE_LAYOUT_REPORT",
    );
}

#[test]
#[ignore = "requires fresh FS-UAE mapped mutable rejection"]
fn compact_mutable_mapped_layout_fs_uae() {
    let cases = (1..=2)
        .map(|maps| NativeCase {
            name: format!("layout-maps={maps}"),
            cpu: "6502",
            source: layout_source(maps),
            expectation: NativeExpected::RejectLayout,
            files: layout_files(maps),
        })
        .collect();
    native_project_cases(cases, "OPFORGE_MUTABLE_LAYOUT_REPORT");
}
