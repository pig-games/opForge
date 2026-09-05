# Step17 / A-test-throughput: assertion-preserving test optimization

Active AGENTS.md remains binding. This implements the bounded experiment in
plan amendment 2428fe26, after Step16 c64a8d4c. The full candidate gate and exact inventory comparison pass. The warm repetition also passes. The retention decision is to keep the profile;
final workflow/compliance and the focused commit remain pending.

## Change and purpose

The only production/configuration change is Cargo's test profile:
`opt-level = 1`, `debug-assertions = true`, `overflow-checks = true`.
Release and development profiles, tests, features, oracles and gate commands
are unchanged. The purpose is faster repeated validation while completing
Phase A; no Rust release or native 68020 runtime speedup is claimed here.

Host assembly caching remains deferred. Each timed test still assembles every
input and uses fresh output directories. The candidate build cache is isolated
at `target/step17-opt1`; the original unoptimized cache is preserved.

## Focused measurements

The existing host-only test is
`tests::native_platform_profile_harness_and_cli_assemble`, invoked with
`--exact --nocapture --test-threads=1`. It assembles two roots under each of five
existing define compositions: ten native configurations per run. Both control
runs completed before the one candidate profile was applied; no other benchmark
or gate ran concurrently with these timings.

| Whole command | Run 1 | Run 2 | Median |
|---|---:|---:|---:|
| Unoptimized control | 162.942063s | 162.724017s | 162.833040s |
| Optimized candidate | 35.769016s | 35.193120s | 35.481068s |

The observed focused command improvement is **78.2102%**. Candidate warm
variation is about 1.62%, below the plan's 10% minimum acceptance threshold;
the improvement exceeds that threshold. This is focused evidence, not a
whole-gate result.

Control compile preparation reused its existing cache in 0.042693s. The
candidate's cold assembler-test compilation took 35.879959s; preparing the
remaining non-LSP workspace tests took another 35.349042s, for 71.229001s total.
These cold preparation costs are separate from the warm timings. The complete
candidate gate may incur additional build work, which is included in its
reported whole duration rather than hidden from payback.

## Correctness and check preservation

All four focused runs passed the identical selected test (one pass, 1,590
filtered out). Across every configuration, both candidate runs produce exactly
the control executable, listing and hex bytes. All ten combinations are also
stable between the two runs within each profile. This is host assembly evidence
(Level C), not native guest execution or a new Level D claim.

Verbose compiler records show optimization level 1 and enabled debug assertions
for the six relevant core crates, the assembler test harness, and 21 additional
workspace test compilation commands. No overflow-check disabling override is
present. The manifest explicitly enables overflow checks. A temporary Rust
probe compiled with the actual assembler test's effective check flags passes
three tests: debug-assertion cfg is enabled, a failed debug assertion panics,
and runtime integer overflow panics. Probe flags preserve the compiler's actual
absence of an explicit overflow override; the behavior is tested rather than
inferred solely from that absence.

The complete gate comparison retains package/suite identity, every test name
and status, and all result summaries. The control contains 3,030 result records
across 34 Cargo suites and 38 summaries. Four extra child records belong to the
existing FFI release-panic suite. The initial parser's unique-summary assumption
was rejected on those child records and corrected to preserve and reconcile all
child and parent results; no tests or evidence rows were removed.

## Full validation and disposition

The same-source Step16 control passed the non-LSP Rust gate in 1,889.188706s,
including 1,591 assembler tests in 1,819.78s. The candidate complete non-LSP gate passed in 278.294873s, including 1,591
assembler tests in 208.20s. Every package/test/result record matches the control:
3,030 records, 34 Cargo suites and 38 summaries, including all child records.
The observed whole-gate reduction is 85.2691%; the
assembler suite reduction is 88.5591%.

The candidate's first complete pass, including its 71.229001s cold preparation,
cost 349.523874s versus the warm control gate's 1,889.188706s: a conservative
first-pass saving of 1,539.664832s (25m 40s). Thus build preparation is already
repaid on the first gate; no speculative number of future iterations is needed.
This comparison explicitly includes the candidate's extra cold preparation.
The required additional warm non-LSP suite passed in 227.927479s, with the
identical complete inventory. Its assembler suite took 207.04s, versus 208.20s
in the candidate gate (0.56% variation). The warm-suite command omits the other
quality-gate checks, so its whole duration is not presented as a second whole
gate measurement. No universal timing guarantee or native product gain follows.

Retention decision: keep the three test-profile settings. All measured
performance, identity, safety-check, output and repeatability conditions pass.
Final workflow and independent compliance are required before the focused
commit; the plan receipt records that approval.

LSP clippy/tests remain deferred by the approved `--defer-lsp` mode. Native guest
checks unavailable in this host gate remain environment skips, exactly as in
the control; a passing Rust harness result does not turn a skipped guest into
native parity evidence. All B10, aggregate, diagnostic and other Phase A debt
remains open, with full native qualification still required.

The raw identity, compiler commands, timing logs, check probes, output hashes
and comparison helpers are retained under
`target/workflow-logs/step17-test-throughput` in
`/Users/erik/Code/Retro/opForge-wt-native-statement-live-performance`.
The comparison JSON indexes the stable receipts and states their proof limits.

#### Progress log

- Production code changed: three explicit Cargo test-profile settings.
- Behavior now implemented: optimized test execution with debug assertions and overflow checks retained.
- Validation status: focused timings, exact host artifacts and runtime check probes pass; full non-LSP gate and exact inventory pass; warm repetition and exact inventories pass; final workflow/compliance pending.
- Unresolved issue: final workflow/compliance; other Phase A debt remains open.
- Next concrete implementation step: complete final compliance and commit the retained profile before Step18.
