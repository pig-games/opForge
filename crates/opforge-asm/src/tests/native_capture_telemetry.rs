//! Capture instrumentation boundaries; telemetry never substitutes for native proof.
use super::native_iterable_loops::scratch;
use super::*;
use crate::native_package_build::{build_native_packages, EmbedSelection};
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

fn assemble_source(source: &Path, dir: &Path, defines: &[String], output: &str) -> Vec<u8> {
    let mut args = vec![
        "opForge".to_string(),
        source.to_string_lossy().into_owned(),
        "-I".into(),
        dir.to_string_lossy().into_owned(),
    ];
    for define in defines {
        args.extend(["--define".into(), define.clone()]);
    }
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "capture fixture assembly: {}; diagnostics: {:?}",
            error.summary(),
            error.diagnostics()
        ),
        error => panic!("capture fixture assembly: {error:?}"),
    });
    fs::read(dir.join(output)).unwrap()
}

const GATES: [&str; 3] = [
    "OPFORGE_DEBUG_CONTRACTS",
    "OPFORGE_MEMORY_TELEMETRY",
    "OPFORGE_CAPTURE_TELEMETRY",
];
const INCLUDE: &str = "native/motorola68000/amigaos/debug/capture_telemetry.i";
const OBSERVER: &str = "native/motorola68000/amigaos/debug/opforge_capture_profile.asm";

fn wrapper_source(sites: &str, stub: bool) -> String {
    let stub = if stub {
        let mut source = String::from(".module debug.amigaos.capture_profile\n.cpu 68020\n.pub\nBlockView .struct\nPointer .long ?\nCapacity .long ?\nUsed .long ?\n.endstruct\n.section code, kind=code\n");
        for name in [
            "clear",
            "growBegin",
            "growEnd",
            "replayBegin",
            "construction",
            "append",
            "materialize",
            "grow",
            "retainedRelease",
            "snapshot",
            "report",
        ] {
            source.push_str(&format!("{name} .block\n rts\n.bend\n"));
        }
        source.push_str(".endsection\n.endmodule\n");
        source
    } else {
        String::new()
    };
    format!(
        r#".module capture.test
.cpu 68020
.region ram, 0, $ffff
.include "capture_telemetry.i"
.section code, kind=code
start .block
 moveq #7,d0
{sites}
 rts
.bend
.endsection
.section data, kind=data
Table .long 0,0,0
Temp .long 0,0,0
Order .long 0,0,0
.endsection
.place code in ram
.place data in ram
.output "build/capture-test", format=bin, sections=code,data
.endmodule
{stub}.end
"#
    )
}

fn assemble_wrapper(sites: &str, stub: bool, defines: &[String]) -> Vec<u8> {
    let dir = scratch();
    let source = dir.0.join("capture.asm");
    fs::write(&source, wrapper_source(sites, stub)).unwrap();
    fs::copy(
        workspace_root().join(INCLUDE),
        dir.0.join("capture_telemetry.i"),
    )
    .unwrap();
    assemble_source(&source, &dir.0, defines, "build/capture-test")
}

const SITES: &str = r#"
 .CAPTURE_CLEAR
 .CAPTURE_GROW_BEGIN
 .CAPTURE_GROW_END
 .CAPTURE_REPLAY_BEGIN
 .CAPTURE_CONSTRUCTION_CLEAR_BEGIN d4
 .CAPTURE_CONSTRUCTION_CLEAR_GAP d4, d5
 .CAPTURE_CONSTRUCTION d4, 8(a4), 12(a4), 8(a5)
 .CAPTURE_APPEND Temp, 64, 8, 12, 32, 16, d0
 .CAPTURE_COPY d6
 .CAPTURE_GROW d5, 4(a4), d7
 .CAPTURE_RETAINED_RELEASE 4(a4)
 .CAPTURE_SNAPSHOT CAPTURE_REPLAY, Table, d7, Temp, Order, 48, 20, d6, 8
 .CAPTURE_REPORT a6
"#;

#[test]
fn native_capture_telemetry_missing_gates_emit_zero_bytes() {
    for mask in 0..7 {
        let defines = GATES
            .iter()
            .enumerate()
            .filter(|(bit, _)| mask & (1 << bit) != 0)
            .map(|(_, gate)| (*gate).to_string())
            .collect::<Vec<_>>();
        assert_eq!(
            assemble_wrapper(SITES, false, &defines),
            assemble_wrapper("", false, &defines),
            "incomplete gate mask {mask} must emit no capture imports or code"
        );
    }
    let defines = GATES.map(str::to_string);
    assert_eq!(
        assemble_wrapper(" .CAPTURE_REPORT a6", true, &defines),
        assemble_wrapper("", true, &defines),
        "report additionally requires preparation progress"
    );
}

#[test]
fn native_capture_telemetry_wrappers_preserve_ccr_before_setup_and_balance_frames() {
    // Exact assembled wrapper proof. The real observer execution is a separate
    // ignored probe below; a stub return does not prove its preservation.
    let expected = r#"
 move.w ccr,-(sp)
 jsr capture_profile.clear
 move.w (sp)+,ccr
 move.w ccr,-(sp)
 jsr capture_profile.growBegin
 move.w (sp)+,ccr
 move.w ccr,-(sp)
 jsr capture_profile.growEnd
 move.w (sp)+,ccr
 move.w ccr,-(sp)
 jsr capture_profile.replayBegin
 move.w (sp)+,ccr
 move.w ccr,-(sp)
 moveq #2,d4
 move.w (sp)+,ccr
 move.w ccr,-(sp)
 add.l d5,d4
 move.w (sp)+,ccr
 move.w ccr,-(sp)
 movem.l d0-d3,-(sp)
 move.l d4,d0
 move.l 8(a4),d1
 move.l 12(a4),d2
 move.l 8(a5),d3
 jsr capture_profile.construction
 movem.l (sp)+,d0-d3
 move.w (sp)+,ccr
 move.w ccr,-(sp)
 movem.l d1-d6/a0,-(sp)
 move.l d0,d5
 lea Temp,a0
 movea.l capture_profile.BlockView.Pointer(a0),a0
 move.l 8(a0),d1
 move.l 12(a0),d2
 move.l 32(a0),d3
 move.l 16(a0),d4
 move.l #64,d6
 jsr capture_profile.append
 movem.l (sp)+,d1-d6/a0
 move.w (sp)+,ccr
 move.w ccr,-(sp)
 movem.l d0,-(sp)
 move.l d6,d0
 jsr capture_profile.materialize
 movem.l (sp)+,d0
 move.w (sp)+,ccr
 move.w ccr,-(sp)
 movem.l d1-d3,-(sp)
 move.l d5,d1
 move.l 4(a4),d2
 move.l d7,d3
 jsr capture_profile.grow
 movem.l (sp)+,d1-d3
 move.w (sp)+,ccr
 move.w ccr,-(sp)
 movem.l d0,-(sp)
 move.l 4(a4),d0
 jsr capture_profile.retainedRelease
 movem.l (sp)+,d0
 move.w (sp)+,ccr
 move.w ccr,-(sp)
 movem.l d0-d5/a0-a2,-(sp)
 move.l d7,d1
 move.l d6,d4
 move.l #8,d5
 move.l #1,d0
 move.l #48,d2
 move.l #20,d3
 lea Table,a0
 lea Temp,a1
 lea Order,a2
 jsr capture_profile.snapshot
 movem.l (sp)+,d0-d5/a0-a2
 move.w (sp)+,ccr
 move.w ccr,-(sp)
 movem.l a0,-(sp)
 movea.l a6,a0
 jsr capture_profile.report
 movem.l (sp)+,a0
 move.w (sp)+,ccr
"#;
    let mut defines = GATES.map(str::to_string).to_vec();
    defines.push("OPFORGE_PREPARATION_PROGRESS".into());
    assert_eq!(
        assemble_wrapper(SITES, true, &defines),
        assemble_wrapper(expected, true, &defines)
    );
}

fn capture_tree() -> Vec<(&'static str, String)> {
    let mut entry = String::from(
        ".module app\n.cpu 68020\n.use provider (EMIT)\n.include \"capture_telemetry.i\"\n\
         cap_table=0\ncap_temp=0\ncap_order=0\n\
         .CAPTURE_SNAPSHOT CAPTURE_ACQUISITION, cap_table, 0, cap_temp, cap_order, 28, 16, 0, 12\n\
         .include \"templates/local.inc\"\n",
    );
    for value in 0..24 {
        entry.push_str(&format!(".EMIT {value}\n"));
    }
    entry.push_str(".for item in {2,4}\n.EMIT item\n.endfor\n.REPEAT 3\n.endmodule\n");
    vec![
        ("entry.asm", entry),
        ("provider.asm", ".module provider\n.cpu 68020\n.pub\nEMIT .macro value\n.byte .value\n.endmacro\n.endmodule\n".into()),
        ("templates/local.inc", "REPEAT .macro times\n.for .times\n.include \"leaf.inc\"\n.endfor\n.endmacro\n".into()),
        ("templates/leaf.inc", ".byte 37\n".into()),
        // Guest source has no debug defines or observer module. Native must
        // still bind the disabled call's boundary identity at module close.
        ("capture_telemetry.i", fs::read_to_string(workspace_root().join(INCLUDE)).unwrap()),
    ]
}

#[test]
fn native_capture_telemetry_source_has_complete_live_rust_oracle() {
    let dir = scratch();
    let mut expected = (0..24).collect::<Vec<u8>>();
    expected.extend([2, 4, 37, 37, 37]);
    assert_eq!(
        super::native_active_includes::live_oracle(&dir.0, &capture_tree()),
        expected
    );

    // Rust control for a known native acquisition gap: `.block` used as a
    // macro formal can be misclassified as a structural directive even in
    // an inactive body. The capture wrapper uses `arena` to avoid that gap;
    // this empty Rust result does not claim native support for the old form.
    let gap_dir = scratch();
    let gap_files = [("entry.asm", String::from(
        ".module app\n.cpu 68020\nPROBE .macro block\n.ifdef NEVER_CAPTURE_BODY\n lea .block,a0\n.endif\n.endmacro\n.PROBE 0\n.endmodule\n"
    ))];
    assert!(super::native_active_includes::live_oracle(&gap_dir.0, &gap_files).is_empty());
}

fn capture_rows(stdout: &str) -> std::collections::BTreeMap<u32, [u32; 3]> {
    let mut rows = std::collections::BTreeMap::new();
    for line in stdout
        .lines()
        .filter(|line| line.starts_with("progress p="))
    {
        let fields = line
            .split_whitespace()
            .skip(1)
            .map(|field| {
                let (key, value) = field.split_once('=').expect("progress field");
                (
                    key,
                    u32::from_str_radix(value, 16).expect("fixed hexadecimal progress value"),
                )
            })
            .collect::<std::collections::BTreeMap<_, _>>();
        let phase = fields["p"];
        if matches!(phase, 400..=406 | 416..=420 | 424..=428 | 432..=436) {
            assert!(
                rows.insert(phase, [fields["f"], fields["l"], fields["r"]])
                    .is_none(),
                "duplicate capture row {phase}"
            );
            eprintln!("CAPTURE_TELEMETRY {line}");
        }
    }
    assert_eq!(rows.len(), 22, "all terminal capture rows must be present");
    rows
}

fn assert_capture_rows(stdout: &str) -> std::collections::BTreeMap<u32, [u32; 3]> {
    let rows = capture_rows(stdout);
    assert_eq!(
        rows[&400],
        [1, 0, 7],
        "schema, errors and completed boundary mask"
    );
    let [records, headers, tokens] = rows[&401];
    let [lexemes, plans, padding] = rows[&402];
    assert!(records > 0 && tokens > 0 && lexemes > 0 && plans > 0);
    assert_eq!(headers, records.checked_mul(64).unwrap());
    assert_eq!(tokens % 20, 0);
    let composed = [headers, tokens, lexemes, plans, padding]
        .into_iter()
        .try_fold(0u32, u32::checked_add)
        .expect("bounded record composition");
    assert_eq!(
        composed, rows[&404][1],
        "composition equals actual append copies"
    );
    let [source_bytes, constructed, cleared] = rows[&403];
    assert!(source_bytes > 0 && constructed >= records);
    assert!(
        cleared >= constructed.checked_mul(2).unwrap()
            && cleared <= constructed.checked_mul(8).unwrap(),
        "constructor clears only its reserved word and two alignment gaps"
    );
    assert!(rows[&404][0] > 0 && rows[&404][2] > 0 && rows[&405][0] > 0);
    assert!(
        rows[&405][1] >= rows[&406][0],
        "global grow calls cover retained capture growth"
    );
    assert!(
        rows[&405][2] >= rows[&406][1],
        "global copies cover retained capture growth"
    );
    for base in [416, 424] {
        let [regions, used, capacity] = rows[&base];
        let [slack, temporary_used, temporary_capacity] = rows[&(base + 1)];
        let [table_used, table_capacity, order_used] = rows[&(base + 2)];
        let [order_capacity, temporary_slack, table_slack] = rows[&(base + 3)];
        let [order_slack, reserved0, reserved1] = rows[&(base + 4)];
        assert!(regions >= 2 && capacity >= used);
        if base == 416 {
            // Expanded active includes can acquire additional physical sources
            // during replay, after the initial acquisition snapshot.
            assert!(used > 0 && used <= composed);
        } else {
            assert_eq!(used, composed, "final replay owns every appended record");
        }
        assert_eq!(capacity - used, slack);
        for (used, capacity, slack) in [
            (temporary_used, temporary_capacity, temporary_slack),
            (table_used, table_capacity, table_slack),
            (order_used, order_capacity, order_slack),
        ] {
            assert!(capacity >= used);
            assert_eq!(capacity - used, slack);
        }
        assert_eq!([reserved0, reserved1], [0, 0]);
        assert!(
            rows[&406][2] >= capacity,
            "retained transient peak covers snapshot capacity"
        );
    }
    for phase in 432..=436 {
        assert_eq!(rows[&phase], [0; 3], "released ownership at phase {phase}");
    }
    rows
}

fn assert_hotspot_counter_relationships(
    profile: &serde_json::Value,
    capture: &std::collections::BTreeMap<u32, [u32; 3]>,
) {
    let counter = |name: &str| {
        profile["work_counters"][name]
            .as_u64()
            .unwrap_or_else(|| panic!("missing hotspot work counter {name}"))
    };
    let calls = |slot: usize| {
        profile["hotspots"][slot]["calls"]
            .as_u64()
            .unwrap_or_else(|| panic!("missing hotspot call count for slot {slot}"))
    };
    let process_calls = calls(12);
    let lookup_calls = calls(15);
    let capture_empty_records = counter("CAPTURE_EMPTY_RECORDS");
    let lookup_namespace_rejects = counter("LOOKUP_NAMESPACE_REJECTS");
    let lookup_length_rejects = counter("LOOKUP_LENGTH_REJECTS");
    let lookup_name_compares = counter("LOOKUP_NAME_COMPARES");
    let lookup_compare_bytes = counter("LOOKUP_COMPARE_BYTES");
    let lookup_hash_bytes = counter("LOOKUP_HASH_BYTES");
    let lookup_chain_probes = counter("LOOKUP_CHAIN_PROBES");

    assert_eq!(counter("PROCESS_RECORDS"), process_calls);
    assert_eq!(
        counter("LOOKUP_HITS") + counter("LOOKUP_MISSES"),
        lookup_calls
    );
    assert_eq!(calls(30), lookup_calls);
    assert_eq!(calls(31), lookup_calls);
    assert_eq!(
        lookup_chain_probes,
        lookup_namespace_rejects + lookup_length_rejects + lookup_name_compares
    );
    assert!(lookup_compare_bytes >= lookup_name_compares);
    assert!(lookup_hash_bytes >= lookup_calls);
    assert!(
        capture_empty_records > 0,
        "fixture should capture blank/comment records"
    );
    assert!(counter("CAPTURE_EMPTY_BYTES") >= capture_empty_records * 64);
    assert!(counter("CAPTURE_EMPTY_BYTES") <= u64::from(capture[&404][1]));
    assert!(capture_empty_records <= u64::from(capture[&401][0]));
    assert!(counter("REPLAY_EMPTY_RECORDS") <= calls(5));
    assert!(counter("PROCESS_EMPTY_INPUTS") <= process_calls);
    assert!(counter("PROCESS_EMPTY_OUTPUTS") <= process_calls);
    assert!(counter("SOURCE_CONSUMED") <= calls(16));
    assert!(counter("CONDITION_CONSUMED") <= calls(18));
}

#[test]
#[ignore = "requires configured FS-UAE; fresh complete capture source parity and accounting"]
fn native_capture_telemetry_compact_cli_fs_uae() {
    let root = workspace_root();
    let dir = scratch();
    let files = capture_tree();
    let oracle = super::native_active_includes::live_oracle(&dir.0, &files);
    let hotspots = std::env::var("OPFORGE_CAPTURE_HOTSPOTS").as_deref() == Ok("1");
    let reference_image = std::env::var_os("OPFORGE_CAPTURE_REFERENCE_IMAGE").map(PathBuf::from);
    let image = if let Some(path) = &reference_image {
        fs::read(path).expect("read explicitly selected capture reference executable")
    } else {
        let build = build_native_packages(
            &engine::build_default_asm_registry(),
            &dir.0.join("native"),
            &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
            &EmbedSelection::Targets(vec!["68020".into()]),
        )
        .unwrap();
        let mut defines = vec![
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_MEMORY_TELEMETRY",
            "OPFORGE_CAPTURE_TELEMETRY",
            "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
            "OPFORGE_PREPARATION_PROGRESS",
        ];
        if hotspots {
            defines.push("OPFORGE_HOTSPOT_TELEMETRY");
        }
        super::compact_cli_input::assemble_cli_with_defines(&root, &build, &defines)
    };
    eprintln!(
        "CAPTURE_CASE reference_image={reference_image:?} oracle_bytes={}",
        oracle.len()
    );
    let run = run_capture_source_case(
        &root,
        &image,
        "capture-accounting-multifile-macro-replay",
        &files,
        &oracle,
    )
    .expect("fresh capture CLI exact-artifact proof");
    if reference_image.is_none() || hotspots {
        let capture = assert_capture_rows(&run.stdout);
        let memory = super::native_package_loading_performance::memory(
            &run.captured_artifacts[&PathBuf::from("Work/memory.bin")],
        );
        if hotspots {
            let stdout = dir.0.join("hotspot.stdout");
            fs::write(&stdout, &run.stdout).unwrap();
            let decoded = std::process::Command::new("python3")
                .arg(root.join("scripts/performance/hotspot_telemetry.py"))
                .arg(stdout)
                .env("PYTHONDONTWRITEBYTECODE", "1")
                .output()
                .expect("run maintained hotspot decoder on fresh guest stdout");
            assert!(
                decoded.status.success(),
                "{}",
                String::from_utf8_lossy(&decoded.stderr)
            );
            let value: serde_json::Value = serde_json::from_slice(&decoded.stdout).unwrap();
            let profile = &value["hotspot_summary"];
            assert!(profile.is_object(), "hotspot observer must report");
            assert_eq!(profile["eclock_hz"], memory["eclock_hz"]);
            assert_hotspot_counter_relationships(profile, &capture);
            eprintln!("HOTSPOT_NATIVE_PROFILE {profile}");
        }
    }
}

fn run_capture_source_case(
    root: &Path,
    image: &[u8],
    name: &str,
    files: &[(&str, String)],
    oracle: &[u8],
) -> Result<FsUaeSmokeRun, String> {
    let guest_files = files
        .iter()
        .map(|(relative_path, source)| OpforgeNativeCliGuestFile {
            relative_path,
            bytes: source.as_bytes(),
        })
        .collect::<Vec<_>>();
    let case = OpforgeNativeCliParityCase {
        name,
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(files[0].1.as_bytes()),
        command_template: Some("Work:entry.asm --bin Work:out.bin"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &guest_files,
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/out.bin",
            rust_oracle: oracle,
        },
    };
    let FsUaeSmokeOutcome::Completed { mut runs } =
        run_prebuilt_compact_cli_case_from_env(root, &case, image)?
    else {
        return Err("real native execution required".into());
    };
    if runs.len() != 1 {
        return Err(format!(
            "{name}: expected one guest run, got {}",
            runs.len()
        ));
    }
    let run = runs.remove(0);
    if !run.protocol_completed || !run.success || run.exit_code != Some(0) {
        return Err(format!(
            "{name}: incomplete fresh proof or failed guest exit {:?}",
            run.exit_code
        ));
    }
    if run
        .captured_artifacts
        .get(&PathBuf::from("Work/out.bin"))
        .map(Vec::as_slice)
        != Some(oracle)
    {
        return Err(format!(
            "{name}: complete artifact differs from this case's live Rust oracle"
        ));
    }
    Ok(run)
}

fn observer_probe_source() -> String {
    // Real observer code, not a mirror. The progress stub has the maintained
    // passive bridge ABI; the CLI proof above exercises the real DOS bridge.
    let calls = [
        ("", "clear"),
        ("", "growBegin"),
        ("moveq #0,d1\nmoveq #0,d2\nmove.l #256,d3", "grow"),
        ("", "growEnd"),
        (
            "moveq #2,d0\nmoveq #1,d1\nmoveq #4,d2\nmoveq #4,d3",
            "construction",
        ),
        (
            "moveq #1,d1\nmoveq #4,d2\nmoveq #4,d3\nmoveq #7,d4\nmove.l #92,d5\nmoveq #64,d6",
            "append",
        ),
        ("moveq #92,d0", "materialize"),
        ("", "replayBegin"),
        ("moveq #92,d0", "materialize"),
        ("", "growBegin"),
        ("moveq #92,d1\nmove.l #256,d2\nmove.l #512,d3", "grow"),
        ("", "growEnd"),
        (
            "moveq #0,d0\nmoveq #1,d1\nmoveq #12,d2\nmoveq #0,d3\nmoveq #2,d4\nmoveq #8,d5",
            "snapshot",
        ),
        (
            "moveq #1,d0\nmoveq #1,d1\nmoveq #12,d2\nmoveq #0,d3\nmoveq #2,d4\nmoveq #8,d5",
            "snapshot",
        ),
        ("move.l #512,d0", "retainedRelease"),
        (
            "moveq #2,d0\nmoveq #1,d1\nmoveq #12,d2\nmoveq #0,d3\nmoveq #2,d4\nmoveq #8,d5",
            "snapshot",
        ),
        ("", "report"),
    ];
    let mut calls = calls
        .into_iter()
        .map(|(setup, name)| (setup, format!("jsr observer.{name}"), None))
        .collect::<Vec<_>>();
    // The clear macros preserve everything except their documented counter.
    // Compare against the expected D4 while retaining all other canaries.
    calls.extend([
        ("", ".CAPTURE_CONSTRUCTION_CLEAR_BEGIN d4".into(), Some(2)),
        (
            "moveq #3,d5",
            ".CAPTURE_CONSTRUCTION_CLEAR_GAP d4, d5".into(),
            Some(0x505 + 3),
        ),
    ]);
    let mut checks = String::new();
    for flags in [0, 31] {
        for (index, (setup, action, expected_d4)) in calls.iter().enumerate() {
            let setup = setup.replace('\n', "\n ");
            let expected_counter = expected_d4
                .map(|value| format!("move.l #{value},Before+16"))
                .unwrap_or_default();
            // Nonzero canaries include every address register. A0-A2 point to
            // valid Block views, with one retained record and logical order.
            checks.push_str(&format!(
                r#"
 movem.l Seeds,d0-d7/a0-a6
 {setup}
 movem.l d0-d7/a0-a6,Before
 {expected_counter}
 move.l sp,BeforeStack
 move.w #{flags},ccr
 {action}
 move.w ccr,AfterCCR
 movem.l d0-d7/a0-a6,After
 cmpa.l BeforeStack,sp
 bne.w bad
 move.w AfterCCR,d0
 andi.w #31,d0
 cmpi.w #{flags},d0
 bne.w bad
 lea Before,a0
 lea After,a1
 moveq #14,d0
compare_{flags}_{index}
 move.l (a0)+,d1
 cmp.l (a1)+,d1
 bne.w bad
 dbra d0,compare_{flags}_{index}
"#
            ));
        }
    }
    let observer = fs::read_to_string(workspace_root().join(OBSERVER)).unwrap();
    format!(
        r#".module probe
.cpu 68020
.use debug.amigaos.capture_profile as observer
.include "capture_telemetry.i"
.section entry,kind=code
start .block
 movem.l d1-d7/a0-a6,-(sp)
 bsr.w exercise
 movem.l (sp)+,d1-d7/a0-a6
 rts
.bend
exercise .block
{checks}
 moveq #0,d0
 rts
bad
 moveq #20,d0
 rts
.bend
.endsection
.section data,kind=data
Seeds .long $101,$202,$303,$404,$505,$606,$707,$808
 .long Table,Temp,Order,Record,Descriptor,Payload,OrderBytes
Table .long Descriptor,12,12
Descriptor .long Payload,512,92
Temp .long Record,128,92
Order .long OrderBytes,16,0
Record .long $42435031,92,1,4,7,0,1,0,4,0,0,0,0,0,0,0
 .fill byte,64,0
Payload .fill byte,512,0
OrderBytes .fill byte,16,0
.endsection
.section bss,kind=bss
Before .res long,15
After .res long,15
BeforeStack .res long,1
AfterCCR .res word,1
.endsection
.output "build/capture-probe",format=hunk,sections=entry,code,data,bss
.endmodule
{observer}
.module debug.amigaos.memory_profile
.cpu 68020
.pub
.section code,kind=code
progress .block
 rts
.bend
.endsection
.endmodule
.end
"#
    )
}

fn build_observer_probe(dir: &Path) -> (String, Vec<u8>) {
    let source = observer_probe_source();
    let path = dir.join("capture-probe.asm");
    fs::write(&path, &source).unwrap();
    fs::copy(
        workspace_root().join(INCLUDE),
        dir.join("capture_telemetry.i"),
    )
    .unwrap();
    let mut defines = GATES.map(str::to_string).to_vec();
    defines.push("OPFORGE_PREPARATION_PROGRESS".into());
    let image = assemble_source(&path, dir, &defines, "build/capture-probe");
    (source, image)
}

#[test]
fn native_capture_telemetry_actual_observer_probe_host_assembles() {
    let dir = scratch();
    let (_, image) = build_observer_probe(&dir.0);
    assert_eq!(
        &image[..4],
        &0x3f3u32.to_be_bytes(),
        "executable Hunk probe"
    );
}

#[test]
#[ignore = "requires configured FS-UAE; actual capture observer registers, CCR and stack"]
fn native_capture_telemetry_actual_observer_preservation_fs_uae() {
    let dir = scratch();
    let (source, image) = build_observer_probe(&dir.0);
    let case = OpforgeNativeCliParityCase {
        name: "capture-observer-register-ccr-stack-probe",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::SuccessfulExitWithoutArtifacts(&["Work/out.bin"]),
    };
    let FsUaeSmokeOutcome::Completed { runs } =
        run_prebuilt_compact_cli_case_from_env(&workspace_root(), &case, &image)
            .expect("fresh actual capture observer preservation probe")
    else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed && runs[0].success);
    assert_eq!(
        runs[0].exit_code,
        Some(0),
        "all actual observer CCR, registers and stack checks passed"
    );
}
