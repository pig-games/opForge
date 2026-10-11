//! Match controls use the shared packed branch policy and live Rust oracles.
use super::native_iterable_loops::{build_image, oracle, scratch};
use super::native_source_cases::{run_batch, Case};
use super::*;

const POSITIVE: &[(&str, &str, &[u8])] = &[
    ("discovery-constant", "value=4\n.match value\n.case 4\n.byte value\n.default\n.byte missing\n.endmatch\n.byte value+1\n", &[4,5]),
    ("discovery-constant-directive", "value .const 4\n.match value\n.case 4\n.byte value\n.endmatch\n.byte value+1\n", &[4,5]),
    ("discovery-use-parameter", "value=4\n.match value\n.case 4\n.byte 7\n.endmatch\n.use dep with (VALUE=value)\n.byte dep.result\n.endmodule\n.module dep\n.pub\nresult=VALUE+1\n", &[7,5]),
    ("discovery-ordinary-loop", "count=2\n.for count\n.match 1\n.case 1\n.byte 7\n.endmatch\n.endfor\n.match count\n.case 2\n.byte 8\n.endmatch\n", &[7,7,8]),
    ("scalar", ".match 2\n.case 1\n.byte missing\n.case 2,missing\n.byte 7\n.case missing\n.byte missing\n.default\n.byte missing\n.endmatch\n.byte 8\n", &[7, 8]),
    ("default", ".match 4\n.case 1,2,3\n.byte missing\n.default\n.byte 9\n.endmatch\n", &[9]),
    ("no-arm", ".match 4\n.case 1,2\n.byte missing\n.endmatch\n.byte 8\n", &[8]),
    ("wide", ".match ((1<<16)<<16)\n.case 0\n.byte 9\n.case ((1<<16)<<16)\n.byte missing\n.endmatch\n.match -1\n.case 4294967295\n.byte 10\n.case -1\n.byte missing\n.endmatch\n", &[9, 10]),
    ("forward-opener", "n .var 1\n.match later\n.case 1\nn .set 7\n.case 2\nn .set 9\n.default\nn .set 8\n.endmatch\nlater=2\n.byte n\n", &[9]),
    ("forward-case", ".match 2\n.case later\n.byte 7\n.default\n.byte 8\n.endmatch\nlater=2\n", &[7]),
    ("nested", ".match 2\n.case 1\n.if missing\n.byte missing\n.endif\n.case 2\n.if 1\n.match 3\n.case 3\n.byte 7\n.default\n.byte missing\n.endmatch\n.else\n.byte missing\n.endif\n.default\n.byte missing\n.endmatch\n", &[7]),
    ("inactive-parent", ".if 0\n.match missing\n.case missing\n.byte missing\n.default\n.byte missing\n.endmatch\n.else\n.byte 8\n.endif\n", &[8]),
    ("iterator", ".bfor item in {1,2,3}\n.match item\n.case 1,3\n.byte item+10\n.default\n.byte item+20\n.endmatch\n.endfor\n", &[11, 22, 13]),
    ("owned-constant", ".match later\n.case 1\nvalue=7\n.case 2\nvalue=8\n.default\nvalue=9\n.endmatch\nlater=2\n.byte value\n", &[8]),
    ("owned-count", ".match later\n.case 1\ncount=2\n.for count\n.byte 7\n.endfor\n.default\ncount=3\n.for count\n.byte 8\n.endfor\n.endmatch\nlater=1\n", &[7, 7]),
    ("forward-default-count", ".match later\n.case 1\ncount=2\n.for count\n.byte 7\n.endfor\n.default\ncount=3\n.for count\n.byte 8\n.endfor\n.endmatch\nlater=2\n", &[8, 8, 8]),
    ("forward-case-count", ".match 2\n.case later\ncount=2\n.for count\n.byte 7\n.endfor\n.default\ncount=3\n.for count\n.byte 8\n.endfor\n.endmatch\nlater=2\n", &[7, 7]),
    ("forward-case-shortcircuit-count", ".match later\n.case 1,missing\ncount=2\n.for count\n.byte 7\n.endfor\n.default\ncount=3\n.for count\n.byte 8\n.endfor\n.endmatch\nlater=1\n", &[7, 7]),
    ("labels", "opening: .match opening\narm: .case opening\n.byte arm\nending: .endmatch\n.byte ending\n", &[0, 1]),
];
const BAD: &[(&str, &str)] = &[
    ("missing-match-selector", ".match\n.case 0\n.byte 7\n.endmatch\n"),
    ("case-without-match", ".case 1\n"),
    ("default-without-match", ".default\n"),
    ("endmatch-without-match", ".endmatch\n"),
    ("wrong-close", ".match 1\n.endif\n"),
    ("wrong-arm", ".match 1\n.else\n.endmatch\n"),
    ("match-close-in-if", ".if 1\n.endmatch\n"),
    ("case-in-if", ".if 1\n.case 1\n.endif\n"),
    (
        "duplicate-default",
        ".match 1\n.default\n.default\n.endmatch\n",
    ),
    (
        "case-after-default",
        ".match 1\n.default\n.case 1\n.endmatch\n",
    ),
    ("mutable-forward-match-layout", ".match later\n.case 1\n.for 2\n.byte 7\n.endfor\n.default\n.for 3\n.byte 8\n.endfor\n.endmatch\nlater .var 1\n"),
    ("unclosed", ".match 1\n.case 1\n.byte 7\n"),
    (
        "unresolved",
        ".match missing\n.case 1\n.byte 7\n.endmatch\n",
    ),
    (
        "reached-unresolved-case",
        ".match 2\n.case 1,missing\n.byte 7\n.endmatch\n",
    ),
    (
        "multiple-match-values",
        ".match 1,2\n.case 1\n.byte 7\n.endmatch\n",
    ),
    ("empty-case", ".match 1\n.case\n.endmatch\n"),
];
// This Rust-supported preparation effect is an explicit remaining native gap:
// strict preparation retains match envelopes, and deferred template definitions
// are currently rejected. Do not include it among claimed parity successes.
const PREPARATION_GAP: &str =
    ".match 1\n.case 1\nput .macro\n.byte 7\n.endmacro\n.endmatch\n.put\n";

fn source(cpu: &str, body: &str) -> String {
    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n")
}

#[test]
fn native_match_controls_rust_oracles() {
    let dir = scratch();
    let mut failures = Vec::new();
    for cpu in ["68020", "6502"] {
        for (name, body, expected) in POSITIVE {
            match oracle(&dir.0, &source(cpu, body)) {
                Ok(actual) if actual == *expected => {}
                actual => failures.push(format!("{cpu}/{name}: {actual:?}")),
            }
        }
        for (name, body) in BAD {
            if oracle(&dir.0, &source(cpu, body)).is_ok() {
                failures.push(format!("Rust accepted {cpu}/{name}"));
            }
        }
        assert_eq!(oracle(&dir.0, &source(cpu, PREPARATION_GAP)).unwrap(), [7]);
    }
    assert!(
        failures.is_empty(),
        "Rust match control gaps: {}",
        failures.join("\n")
    );
}

#[test]
#[ignore = "requires configured FS-UAE; retained match envelopes and shared runtime replay"]
fn native_match_controls_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_MATCH_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter = std::env::var("OPFORGE_MATCH_FILTER").unwrap_or_default();
    let cpus = std::env::var("OPFORGE_MATCH_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let mut cases = Vec::new();
    for cpu in cpus.split(',').map(str::trim) {
        assert!(matches!(cpu, "68020" | "6502"));
        for (name, body, expected) in POSITIVE
            .iter()
            .map(|(name, body, bytes)| (*name, *body, Some(bytes.to_vec())))
            .chain(BAD.iter().map(|(name, body)| (*name, *body, None)))
        {
            if !filter.is_empty() && !filter.split(',').any(|item| item.trim() == name) {
                continue;
            }
            cases.push(Case {
                name: format!("match-{cpu}-{name}"),
                cpu: cpu.into(),
                files: vec![("entry.asm", source(cpu, body))],
                expected,
            });
        }
    }
    assert!(!cases.is_empty(), "no match cases selected");
    // Bounded groups share startup. The runner preserves fresh identity,
    // completion, exit and exact artifact proof for every individual source.
    run_batch(&root, &image, &cases, 64);
}

#[test]
#[ignore = "requires configured FS-UAE; documents retained-match preparation limitation"]
fn native_match_preparation_gap_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_MATCH_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let input = source("68020", PREPARATION_GAP);
    assert_eq!(oracle(&dir.0, &input).unwrap(), [7]);
    let files = [OpforgeNativeCliGuestFile {
        relative_path: "entry.asm",
        bytes: input.as_bytes(),
    }];
    let case = OpforgeNativeCliParityCase {
        name: "match-preparation-gap",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(input.as_bytes()),
        command_template: Some("Work:entry.asm --bin Work:out.bin"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
    };
    let FsUaeSmokeOutcome::Completed { runs } =
        run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
            .expect("fresh match preparation rejection")
    else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert!(!runs[0].success);
    assert_eq!(runs[0].exit_code, Some(20));
}
