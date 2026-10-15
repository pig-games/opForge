//! The probe executes the maintained observer, with passive deterministic bridge stubs.
use super::native_iterable_loops::scratch;
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

const OBSERVER: &str = "native/motorola68000/amigaos/debug/opforge_hotspot_profile.asm";
const GATES: [&str; 3] = [
    "OPFORGE_DEBUG_CONTRACTS",
    "OPFORGE_MEMORY_TELEMETRY",
    "OPFORGE_HOTSPOT_TELEMETRY",
];
const PREDICATES: &str = " .HOTSPOT_COUNT_IF_ZERO d0,0,#1\n .HOTSPOT_COUNT_IF_EQ d0,#7,1,#1\n .HOTSPOT_COUNT_IF_EQ_BYTE d0,#7,2,#1\n .HOTSPOT_COUNT_IF_NE_WORD d0,d6,3,#1\n";
const CALL_SITES: &str = " .HOTSPOT_CALL #0,passive\n .HOTSPOT_BSR #1,passive\n";
const ORIGINAL_CALLS: &str = " jsr passive\n bsr.w passive\n";
const SITES: &str =
    " .HOTSPOT_CLEAR\n .HOTSPOT_ENTER #0\n .HOTSPOT_LEAVE #0\n .HOTSPOT_COUNT HOTSPOT_LOOKUP_HASH_BYTES,#1\n .HOTSPOT_REPORT a6\n";

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

fn assemble(source: &str, defines: &[String], output: &str) -> Vec<u8> {
    let dir = scratch();
    let path = dir.0.join("hotspot.asm");
    fs::write(&path, source).unwrap();
    crate::tests::native_telemetry_macros::stage_telemetry_fixture_files(&dir.0);
    let mut args = vec![
        "opForge".into(),
        path.to_string_lossy().into_owned(),
        "-I".into(),
        dir.0.to_string_lossy().into_owned(),
    ];
    for define in defines {
        args.extend(["--define".into(), define.clone()]);
    }
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.0.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "hotspot fixture: {}; diagnostics: {:?}",
            error.summary(),
            error.diagnostics()
        ),
        error => panic!("hotspot fixture: {error:?}"),
    });
    fs::read(dir.0.join(output)).unwrap()
}

fn wrapper(sites: &str, stub: bool) -> String {
    let stub = if stub {
        ".module debug.amigaos.hotspot_profile\n.cpu 68020\n.pub\nARITHMETIC_OVERFLOW = 32\n.section code,kind=code\nclear .block\n rts\n.bend\nenter .block\n rts\n.bend\nleave .block\n rts\n.bend\nreport .block\n rts\n.bend\n.endsection\n.section code,kind=code\nErrors .long 0\nStopped .long 0\nCounters .long 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n.endsection\n.endmodule\n"
    } else {
        ""
    };
    format!(".module test\n.cpu 68020\n.region ram,0,$ffff\n.include \"hotspot_telemetry.i\"\n.section code,kind=code\nstart .block\n moveq #7,d0\n{sites} rts\n.bend\npassive .block\n rts\n.bend\n.endsection\n.place code in ram\n.output \"build/wrapper\",format=bin,sections=code\n.endmodule\n{stub}.end\n")
}

#[test]
fn native_hotspot_telemetry_missing_gates_emit_zero_bytes() {
    for mask in 0..7 {
        let defines = GATES
            .iter()
            .enumerate()
            .filter(|(bit, _)| mask & (1 << bit) != 0)
            .map(|(_, gate)| (*gate).into())
            .collect::<Vec<String>>();
        assert_eq!(
            assemble(
                &format!(
                    "{}{}\n.end\n",
                    wrapper(&format!("{SITES}{PREDICATES}{CALL_SITES}"), false)
                        .trim_end_matches(".end\n"),
                    fs::read_to_string(workspace_root().join(OBSERVER)).unwrap()
                ),
                &defines,
                "build/wrapper"
            ),
            assemble(&wrapper(ORIGINAL_CALLS, false), &defines, "build/wrapper"),
            "gate mask {mask} must emit no import or instructions"
        );
    }
    let defines = GATES.map(str::to_string);
    assert_eq!(
        assemble(
            &wrapper(" .HOTSPOT_REPORT a6\n", true),
            &defines,
            "build/wrapper"
        ),
        assemble(&wrapper("", true), &defines, "build/wrapper"),
        "report additionally requires preparation progress"
    );
}

#[test]
fn native_hotspot_telemetry_wrappers_preserve_ccr_before_argument_setup() {
    let expected = " move.w ccr,-(sp)\n jsr hotspot_profile.clear\n move.w (sp)+,ccr\n move.w ccr,-(sp)\n move.l d0,-(sp)\n move.l #0,d0\n jsr hotspot_profile.enter\n move.l (sp)+,d0\n move.w (sp)+,ccr\n move.w ccr,-(sp)\n move.l d0,-(sp)\n move.l #0,d0\n jsr hotspot_profile.leave\n move.l (sp)+,d0\n move.w (sp)+,ccr\n move.w ccr,-(sp)\n move.l d0,-(sp)\n move.l #1,d0\n tst.l hotspot_profile.Stopped\n bne.w count_done\n add.l d0,hotspot_profile.Counters+28\n bcc.w count_done\n move.l #$ffffffff,hotspot_profile.Counters+28\n ori.l #32,hotspot_profile.Errors\n move.l #1,hotspot_profile.Stopped\ncount_done\n move.l (sp)+,d0\n move.w (sp)+,ccr\n move.w ccr,-(sp)\n move.l a0,-(sp)\n movea.l a6,a0\n jsr hotspot_profile.report\n movea.l (sp)+,a0\n move.w (sp)+,ccr\n";
    let mut defines = GATES.map(str::to_string).to_vec();
    defines.push("OPFORGE_PREPARATION_PROGRESS".into());
    assert_eq!(
        assemble(&wrapper(SITES, true), &defines, "build/wrapper"),
        assemble(&wrapper(expected, true), &defines, "build/wrapper")
    );
}

// Expected observations are explicit scenario facts, not a second profiler.
// Fields: invocation count, completed own samples, timing windows, inclusive, exclusive.
type Slot = [u32; 5];
struct Probe {
    code: String,
    rows: Vec<[u32; 4]>,
    checks: usize,
    period: u32,
    counters: [u32; 16],
    seed: u32,
}
impl Probe {
    fn checked(&mut self, action: &str, flags: u32) {
        let serial = self.checks;
        self.checks += 1;
        self.code.push_str(&format!(
            r#"
 movem.l d0-d7/a0-a6,Before
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
compare_{serial}
 move.l (a0)+,d1
 cmp.l (a1)+,d1
 bne.w bad
 dbra d0,compare_{serial}
 movem.l Before,d0-d7/a0-a6
"#
        ));
    }
    fn clear(&mut self) {
        self.checked(".HOTSPOT_CLEAR", 0);
        self.counters = [0; 16];
    }
    fn enter(&mut self, slot: u32) {
        self.checked(&format!(".HOTSPOT_ENTER #{slot}"), 31);
    }
    fn leave(&mut self, slot: u32) {
        self.checked(&format!(".HOTSPOT_LEAVE #{slot}"), 0);
    }
    fn warm(&mut self, slot: u32, count: u32) {
        if count == 0 {
            return;
        }
        let label = format!("warm_{}", self.checks);
        self.code
            .push_str(&format!(" move.l #{},d7\n{label}\n", count - 1));
        self.enter(slot);
        self.leave(slot);
        self.code.push_str(&format!(" dbra d7,{label}\n"));
    }
    fn report(
        &mut self,
        errors: u32,
        frequency: u32,
        depth: [u32; 2],
        stopped: u32,
        slots: [Slot; 32],
    ) {
        self.checked(".HOTSPOT_REPORT a6", 31);
        self.rows.extend([
            [500, 1, errors, self.period],
            [501, frequency, depth[0], depth[1]],
            [502, 32, self.seed, stopped],
        ]);
        for (id, [calls, samples, windows, inclusive, exclusive]) in slots.into_iter().enumerate() {
            let id = id as u32;
            self.rows.extend([
                [512 + 3 * id, id, calls, samples],
                [513 + 3 * id, windows, 0, inclusive],
                [514 + 3 * id, 0, exclusive, 0],
            ]);
        }
        for (id, value) in self.counters.into_iter().enumerate() {
            self.rows.push([608 + id as u32, id as u32, value, 0]);
        }
    }
}

fn probe_source(period: u32, seed: u32) -> String {
    let mut p = Probe {
        code: String::new(),
        rows: Vec::new(),
        checks: 0,
        counters: [0; 16],
        period,
        seed,
    };
    let warm0 = seed.wrapping_neg() & (period - 1);
    // All public macros execute with both extremes of CCR; nonzero canaries
    // cover all 15 registers and each check verifies SP before touching it.
    for flags in [0, 31] {
        p.checked(".HOTSPOT_CLEAR", flags);
        p.warm(0, warm0);
        p.checked(".HOTSPOT_ENTER #0", flags);
        p.checked(".HOTSPOT_LEAVE #0", flags);
        let mut slots = [[0; 5]; 32];
        slots[0] = [warm0 + 1, 1, 1, 1000, 1000];
        p.report(0, 1000, [1, 0], 0, slots);
        // Report preservation with CCR=0 as well as the report method's 31.
        if flags == 0 {
            p.checked(".HOTSPOT_REPORT a6", 0);
            let rows = p.rows[p.rows.len() - 115..].to_vec();
            p.rows.extend(rows);
        }
    }
    // Only the direct forced child's complete wall interval is needed to
    // subtract its deeper descendants from the sampled parent's inclusive time.
    p.clear();
    p.warm(0, warm0);
    p.enter(0);
    p.enter(1);
    p.enter(2);
    p.leave(2);
    p.leave(1);
    p.leave(0);
    let mut slots = [[0; 5]; 32];
    slots[0] = [warm0 + 1, 1, 1, 3000, 2000];
    slots[1] = [1, 0, 1, 0, 0];
    slots[2] = [1, 0, 0, 0, 0];
    p.report(0, 1000, [3, 0], 0, slots);
    p.clear();
    p.warm(0, warm0);
    p.enter(0);
    p.enter(0);
    p.leave(0);
    p.leave(0);
    let mut slots = [[0; 5]; 32];
    slots[0] = [warm0 + 2, 1, 2, 3000, 2000];
    p.report(0, 1000, [2, 0], 0, slots);
    // A child selected by its own phase remains sampled under an unsampled parent.
    let warm2 = (34 + seed).wrapping_neg() & (period - 1);
    p.clear();
    p.warm(2, warm2);
    p.enter(3);
    p.enter(2);
    p.leave(2);
    p.leave(3);
    let mut slots = [[0; 5]; 32];
    slots[2] = [warm2 + 1, 1, 1, 1000, 1000];
    slots[3] = [1, 0, 0, 0, 0];
    p.report(0, 1000, [2, 0], 0, slots);
    // Every identity runs across two full periods plus a tail. This count
    // oracle checks phase offsets independently of observer implementation.
    p.clear();
    let calls = 2 * period + 19;
    let mut slots = [[0; 5]; 32];
    for id in 0..32u32 {
        p.warm(id, calls);
        let samples = (0..calls)
            .filter(|index| (index + 17 * id + seed) & (period - 1) == 0)
            .count() as u32;
        slots[id as usize] = [calls, samples, samples, 1000 * samples, 1000 * samples];
    }
    p.report(0, 1000, [1, 0], 0, slots);
    // A sampled timestamp crosses the low-word carry without a negative duration.
    p.clear();
    p.warm(0, warm0);
    p.code
        .push_str(" clr.l bridge.Clock\n move.l #$fffff830,bridge.Clock+4\n");
    p.enter(0);
    p.leave(0);
    let mut slots = [[0; 5]; 32];
    slots[0] = [warm0 + 1, 1, 1, 1000, 1000];
    p.report(0, 1000, [1, 0], 0, slots);
    // Failure freezes this observer; later production macro calls still return
    // passively, retaining unfinished depth rather than inventing completions.
    p.clear();
    p.warm(0, warm0);
    p.enter(0);
    p.leave(1);
    p.enter(2);
    p.leave(0);
    let mut slots = [[0; 5]; 32];
    slots[0] = [warm0 + 1, 0, 0, 0, 0];
    p.report(4 + 128, 1000, [1, 1], 1, slots);
    p.clear();
    p.warm(0, warm0);
    for _ in 0..33 {
        p.enter(0);
    }
    for _ in 0..33 {
        p.leave(0);
    }
    let mut slots = [[0; 5]; 32];
    slots[0] = [warm0 + 33, 0, 0, 0, 0];
    p.report(2 + 128, 1000, [32, 32], 1, slots);
    // Exact counters use immediate, register and memory operands without clocks.
    // All IDs exercise positive updates and full passive ABI preservation.
    p.clear();
    for id in 0..16 {
        p.checked(&format!(".HOTSPOT_COUNT {id},#{}", id + 1), 0);
        p.checked(&format!(".HOTSPOT_COUNT {id},#2"), 31);
        p.counters[id] = id as u32 + 3;
    }
    p.checked(".HOTSPOT_COUNT HOTSPOT_LOOKUP_HASH_BYTES,d0", 31);
    p.counters[7] += 0x101;
    p.checked(".HOTSPOT_COUNT HOTSPOT_LOOKUP_COMPARE_BYTES,Seeds", 0);
    p.counters[10] += 0x101;
    p.report(0, 0, [0, 0], 0, [[0; 5]; 32]);
    p.clear();
    p.checked(".HOTSPOT_COUNT_IF_ZERO d0,0,#1", 31);
    p.checked(".HOTSPOT_COUNT_IF_ZERO bridge.Failed,0,#2", 0);
    p.counters[0] = 2;
    p.checked(".HOTSPOT_COUNT_IF_EQ d0,#$101,1,#3", 31);
    p.checked(".HOTSPOT_COUNT_IF_EQ d0,#0,1,#7", 0);
    p.counters[1] = 3;
    p.checked(".HOTSPOT_COUNT_IF_EQ_BYTE d0,#1,2,#4", 31);
    p.checked(".HOTSPOT_COUNT_IF_EQ_BYTE d0,#2,2,#7", 0);
    p.counters[2] = 4;
    p.checked(".HOTSPOT_COUNT_IF_NE_WORD d0,d6,3,#1", 31);
    p.checked(".HOTSPOT_COUNT_IF_NE_WORD d0,d0,3,#1", 0);
    p.counters[3] = 1;
    // Passive callees retain the canaries/flags across measured calls.
    // d0 carries the canary used by every checked() action.
    p.checked(".HOTSPOT_CALL #0,bridge.passive", 31);
    p.checked(".HOTSPOT_BSR #1,passive", 0);
    let sample0 = u32::from(seed & (period - 1) == 0);
    let sample1 = u32::from((17 + seed) & (period - 1) == 0);
    let mut slots = [[0; 5]; 32];
    slots[0] = [1, sample0, sample0, 1000 * sample0, 1000 * sample0];
    slots[1] = [1, sample1, sample1, 1000 * sample1, 1000 * sample1];
    p.report(
        0,
        if sample0 + sample1 > 0 { 1000 } else { 0 },
        [1, 0],
        0,
        slots,
    );
    // Natural u32 overflow saturates the failing counter and freezes every
    // observer update. No direct writes seed implementation-owned storage.
    p.clear();
    p.checked(".HOTSPOT_COUNT HOTSPOT_PROCESS_RECORDS,#$ffffffff", 31);
    p.checked(".HOTSPOT_COUNT HOTSPOT_PROCESS_RECORDS,#1", 0);
    p.checked(".HOTSPOT_COUNT HOTSPOT_LOOKUP_HITS,#1", 31);
    p.enter(0);
    p.leave(0);
    p.counters[15] = u32::MAX;
    p.report(32, 0, [0, 0], 1, [[0; 5]; 32]);
    // Clear recovers both sampled observer and work counters after failure.
    p.clear();
    p.report(0, 0, [0, 0], 0, [[0; 5]; 32]);
    let table = p
        .rows
        .iter()
        .map(|row| format!(" .long {},{},{},{}\n", row[0], row[1], row[2], row[3]))
        .collect::<String>();
    let observer = fs::read_to_string(workspace_root().join(OBSERVER)).unwrap();
    format!(
        r#".module probe
.cpu 68020
.use debug.amigaos.memory_profile as bridge
.include "hotspot_telemetry.i"
.section entry,kind=code
start .block
 movem.l d1-d7/a0-a6,-(sp)
 lea Expected,a0
 move.l a0,bridge.Cursor
 movem.l Seeds,d0-d7/a0-a6
 bsr.w exercise
 movem.l (sp)+,d1-d7/a0-a6
 rts
.bend
exercise .block
{code}
 tst.l bridge.Failed
 bne.w bad
 cmpi.l #{row_count},bridge.Rows
 bne.w bad
 moveq #0,d0
 rts
bad
 moveq #20,d0
 rts
.bend
passive .block
 rts
.bend
.endsection
.section data,kind=data
Seeds .long $101,$202,$303,$404,$505,$606,$707,$808
 .long Before,After,Seeds,Expected,BeforeStack,AfterCCR,Expected
Expected
{table}.endsection
.section bss,kind=bss
Before .res long,15
After .res long,15
BeforeStack .res long,1
AfterCCR .res word,1
.endsection
.output "build/probe",format=hunk,sections=entry,code,data,bss
.endmodule
{observer}
.module debug.amigaos.memory_profile
.cpu 68020
.pub
.section code,kind=code
passive .block
 rts
.bend
; Deterministic passive E-clock: advance 1000 ticks/read, frequency1000.
readClock .block
 move.w ccr,-(sp)
 movem.l d1,-(sp)
 addi.l #1000,Clock+4
 moveq #0,d1
 move.l Clock,d0
 addx.l d1,d0
 move.l d0,Clock
 move.l Clock,(a0)
 move.l Clock+4,4(a0)
 move.l #1000,d0
 movem.l (sp)+,d1
 move.w (sp)+,ccr
 rts
.bend
; Compare every row, including reserved fields. No expected state lives in
; profiler storage; the actual implementation owns all counters/timing.
progress .block
 move.w ccr,-(sp)
 movem.l d0-d3/a1,-(sp)
 movea.l Cursor,a1
 cmpi.l #{row_count},Rows
 bhs.w bad
 cmp.l (a1)+,d0
 bne.w bad
 cmp.l (a1)+,d1
 bne.w bad
 cmp.l (a1)+,d2
 bne.w bad
 cmp.l (a1)+,d3
 bne.w bad
 move.l a1,Cursor
 addq.l #1,Rows
 bra.w done
bad
 move.l #1,Failed
done
 movem.l (sp)+,d0-d3/a1
 move.w (sp)+,ccr
 rts
.bend
.endsection
.section bss,kind=bss
Clock .res long,2
Cursor .res long,1
Rows .res long,1
Failed .res long,1
.endsection
.endmodule
.end
"#,
        code = p.code,
        row_count = p.rows.len()
    )
}

fn build_probe(period: u32, seed: u32) -> (String, Vec<u8>) {
    let source = probe_source(period, seed);
    let mut defines = GATES.map(str::to_string).to_vec();
    defines.extend([
        "OPFORGE_PREPARATION_PROGRESS".into(),
        format!("OPFORGE_HOTSPOT_SAMPLE_PERIOD={period}"),
        format!("OPFORGE_HOTSPOT_PHASE_SEED={seed}"),
    ]);
    let image = assemble(&source, &defines, "build/probe");
    (source, image)
}

#[test]
fn native_hotspot_telemetry_actual_observer_probe_host_assembles() {
    for (period, seed) in [(64, 0), (128, 0), (256, 7)] {
        let (_, image) = build_probe(period, seed);
        assert_eq!(
            &image[..4],
            &0x3f3u32.to_be_bytes(),
            "executable Hunk probe, period{period}/seed{seed}"
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; actual observer sampling, errors, registers, CCR and stack"]
fn native_hotspot_telemetry_actual_observer_fs_uae() {
    for (period, seed) in [(64, 0), (128, 0), (256, 7)] {
        let (source, image) = build_probe(period, seed);
        let name = format!("hotspot-observer-period{period}-seed{seed}");
        let case = OpforgeNativeCliParityCase {
            name: &name,
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
                .expect("fresh actual hotspot observer probe")
        else {
            panic!("real native execution required")
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].protocol_completed && runs[0].success);
        assert_eq!(
            runs[0].exit_code,
            Some(0),
            "all actual observer rows and passive ABI checks, period{period}/seed{seed}"
        );
    }
}
