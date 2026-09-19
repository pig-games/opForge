# Iterative VM and native runtime reset

Status: the [binary-source experiment](prepared-source-experiment.md) now completes
both native mixed8 workloads, with roughly 8–11× observed gains over the optimized
text path for its subset. The next agreed approach is the
[compact native runtime](prepared-source-experiment.md#next-implementation-compact-native-runtime): remove legacy state dependencies,
own preparation/execution memory, and qualify bounded cases within 2 MiB.
R1's earlier prepared-CTBL comparison showed no meaningful speed gain; further
isolated CTBL tuning is not the current direction. Full native language coverage
and product qualification remain incomplete.
The active [AGENTS.md](../../AGENTS.md) and [workflow](../workflow/README.md)
remain binding. This plan captures the current discussion, not instructions from
historical plans. Only the next iteration is detailed; later outcomes are
directions to refine from evidence.

## Product and technical direction

- Self-assemble on a 68020 Amiga with AmigaOS 3.1+ and 2 MB installed RAM in
  at most 15 minutes, preferably substantially faster. Count source loading,
  preparation, layout and output through a runnable executable. Available
  application memory must allow for the OS and required resident components.
  Choose a baseline clock, memory configuration and storage before hardware
  qualification; maximum-speed emulator timings are not hardware calibration.
- Use Rust as the semantic reference and rapid laboratory for general VM efficiency.
  Native representations follow explicit resource constraints rather than a literal
  translation of Rust abstractions. Compare bounded native work early; host elapsed
  time alone cannot rank native costs.
- Optimize shared execution mechanisms for all supported assembly targets, not
  one CPU/family's encoding path. The 68020 product requirement describes the
  execution platform. Use several assembly families as evidence of generality;
  do not equate representative coverage with measured gains on every target.
- Canonical packages remain authoritative. Derive runtime representations from
  them for selected assembly-target capabilities and execution-platform resource
  profiles when measurements justify preparation. These are independent dimensions.
  Optimizing canonical programs does not require another format; prototype derived
  representations in memory before introducing persistent runtime packages.
- Specialized fragments implement package-defined operations; they do not become
  a second manually maintained set of CPU semantics. Bind through validated
  program structure or explicit package bindings, never benchmark identity.
- Preserve independent reference execution and existing correctness evidence.
  Native confirmation follows the [parity contract](../../agents/rules/native-rust-parity-porting.md).
  The reference need not be resident alongside a runtime package on the Amiga.
- Compact source, executable size, package size and working memory are separate
  concerns. Runtime preparation must earn its cost in both time and space.

## Approved execution strategy: controlled replacement

The existing native implementation is a runnable reference and source of reusable
components; its internal architecture is not the destination. Default to replacing
an owned responsibility with a compact implementation, rather than extending the
current structure with isolated optimizations. Correctness repairs remain appropriate
when they unblock an agreed replacement or its reference comparison.

- Preserve the CLI and complete small assembly cases as the observable boundary.
  Integrate one useful replacement at a time; avoid a large unfinished second product.
- For each replacement, identify the Rust semantic boundary, package authority,
  owned state, preparation lifetime and invalidation. Reuse proven I/O, arithmetic,
  output and VM primitives only where their contracts fit the intended design.
- Distinguish immutable package/source preparation from pass-dependent values,
  symbols, CPU state, layout and fixups. No cache may assume repeated inputs merely
  because program bytes or source locations repeat.
- Define which old responsibility disappears. Comparison paths may coexist during
  an experiment; qualified integration deletes the superseded path. Do not grow
  another selection monolith or keep adapter layers merely to preserve old internals.
- Before 1.0, migrate the affected contract atomically and remove obsolete execution
  versions. Retain mismatch rejection; do not launch an unrelated global version purge.
- Write new assembly with opForge language features that improve readability:
  structs/named fields, lists, compile-time loops and reusable macros rather than
  repeated low-level scaffolding. Names inside a module stay short and do not
  repeat its qualification. Inspect the generated size and runtime behavior.
- Add conditional telemetry at meaningful boundaries in new or adapted runtime
  code. Put the conditionals and preservation in reusable macros over the approved
  counter framework; gate calls, counter updates, storage
  and telemetry-only dependencies at build time so release builds contain none of
  them. Record preparation/reuse, bytes/rows examined, dispatch and memory where
  relevant. Keep records bounded, preserve registers/CCR/state, and distinguish
  instrumented work attribution from uninstrumented release timing. Verify disabled
  build output rather than assuming a runtime-disabled flag has zero overhead.
- Judge each slice by complete-case correctness, work avoided, elapsed time,
  executable/package size, memory requirements and clarity. Record unmeasured
  quantities explicitly; an incomplete checkpoint is not an integration claim.

### R1 — Select and begin the first replacement

Authorized outcome: a compact first replacement participating in complete small
assemblies, with an explicit removed responsibility and a recoverable reference.
Use one short attribution pass through existing approved native counters on the
completed 8-block comparison cases to choose the boundary. This preparation is
bounded within R1, not a separate audit or a new instrumentation framework.

Initial candidates are package preparation and repeated selection/program lookup.
Confirm the actual boundary before choosing; counters quantify work, not cycle
cost. Prefer a shared mechanism with evidence across assembly targets. Implement
or model its portable semantics against Rust before any native-specific lowering.
Record the selected ownership, invalidation, resource budget and validation below
once the evidence is available. Stop and discuss if the boundary requires a larger
semantic migration than fits one reviewable slice; do not disguise an unfinished
architecture as a micro-optimization.

### R1 setup baseline: reusable telemetry and selected boundary

The user refined R1 to establish reusable, compile-time-gated telemetry as code is
adapted. Runtime enter/opcode/leave calls in the expression frontend and evaluator
now use `debug/telemetry_macros.i`; service/candidate forms are ready for the first
replacement. The include owns conditional imports, argument setup and register/CCR
preservation. Disabled builds are tested against removed sites, including absence
of the observer dependency. Assembly language features are chosen for clarity;
module-local names must not repeat their module's qualification.

Two complete 8-block native cases pass exact-output parity with decoded, correlated,
non-overflowing runtime counters. These are instrumented observations, not release
speed measurements. The final attribution batch took 59.96 seconds; preliminary
combined/runtime-only checks took 75.14/76.37 seconds (211.47 seconds total).
The preliminary runtime-only cases proved output parity but did not yet decode
counter records; the final batch supplies the actual attribution. Results remain
in ignored `build/runtime-replacement-macro-counters/summary.json`.

| Native 8-block source | TKVM ops | PRVM ops | EXVM ops | ExprVM ops | Encode calls | Package / pass-one / layout / final ticks |
| --- | ---: | ---: | ---: | ---: | ---: | --- |
| m6502 | 2,170 | 2,121 | 208 | 232 | 120 | 4 / 69 / 69 / 70 |
| m68020 | 1,978 | 1,577 | 248 | 248 | 120 | 4 / 104 / 105 / 107 |

Ticks are the instrumented guest's 50 Hz phase clock, on the uncalibrated 68040
emulator. Counted VM instructions are not machine instructions and omit substantial
helper/lookup work. Both cases visit 66 retained statements three times. The
cost is concentrated in repeated assembly passes rather than recorded package
setup. This supports investigating work repeated per encode; it does not measure
CTBL's share of cycles or prove redundant state-dependent evaluation.

The selected first substantive replacement is **a prepared CTBL view and active
pipeline owner binding**, following Rust's `decode_compact_tabl_chunk` and
`RuntimeModelCore::from_chunks`/prepared map lookup. Native currently reconstructs
strings and walks the program table during individual lookups. In the measured
canonical package, CTBL is 52,396 bytes: 16 owners, 1,585 strings, 1,157 programs
and 3,459 rows. A four-byte-per-program offset directory alone would need 4,628
bytes, excluding other metadata and allocation overhead; this is a design input,
not an allocated buffer or proof of fitting 2 MB.

Preparation owns validated offsets/counts and a bounded program directory. Package
reload invalidates it; successful pipeline selection commits owner binding, while
failed selection must preserve the previous binding. Request operands, symbols and
pass state stay outside immutable preparation. Preserve owner precedence and
malformed-data behavior against Rust. The intended removal is per-request CTBL
structural decoding/program-table traversal and the narrow zero-shape memo. Keep
prefix-compressed strings initially rather than copying the complete string table.
No CTBL replacement code is integrated at this checkpoint. Do not choose arbitrary
fixed capacities or claim complete Rust validation equivalence before testing them.

The initial all-counter build failed final-pass expression compilation on both
otherwise passing workloads. Runtime-only telemetry preserves behavior. The cause
of the combined-counter failure remains unresolved; neither the macro conversion
nor speculative callback changes are claimed to fix it. Do not use that combination
as parity/performance evidence. This is recorded as a focused instrumentation gap,
not a reason to expand R1 into a global debug rewrite.

Validation: two focused host macro/source-contract tests and both real-native
macro-enabled comparisons pass. All 136 workflow tests, native formatting (247
files), workflow links, ownership inventory, CPU boundary and fresh-proof guards
pass. Full workspace qualification remains outside this checkpoint.


### R1 experiment: prepared CTBL and committed owner binding

The replacement prepares CTBL at package load, validates native structural bounds,
row indices and strict key ordering, and allocates a direct program-pointer directory.
Successful pipeline selection binds owners; failed selection keeps the previous
binding. Reload invalidates and frees the directory; failed preparation frees any
pending allocation; the CLI and affected harnesses release it at shutdown. Per-lookup
owner discovery, program-table traversal and the zero-shape memo are removed.
Compressed-string reconstruction remains in lookup. The canonical CTBL format is
unchanged; no persistent runtime-package format or target-specific semantics were added.

The isolated review baseline is `a4debf56`. The comparison built that commit's native
source snapshot and the candidate with identical package, source and expected output
bytes. Both unprofiled cases and both runtime-counter cases pass fresh real-native
exact-output comparison with their live Rust oracles (Level D).

| 8-block source target | Baseline native interval | Prepared CTBL interval | CTBL lookups | Strings reconstructed | Binary-search rows examined |
|---|---:|---:|---:|---:|---:|
| m6502 | 4.773 s | 4.815 s | 120 | 190,200 | 1,392 |
| m68020 | 6.886 s | 6.918 s | 24 | 38,040 | 552 |

Each instrumented case prepares once, visits 1,157 program entries during preparation,
and records a 4,628-byte directory allocation. VM/service totals remain consistent
with the setup baseline. Both cases perform 120 encode calls: CTBL therefore covers
all of those calls for this 6502 workload but only one fifth for this 68020 workload.
Every CTBL lookup still reconstructs the entire 1,585-string dictionary. These counts
identify repeated work and uneven coverage; they do not establish cycle attribution.

The unprofiled native executable grows from 563,220 to 563,768 bytes (+548).
The directory adds 4,628 allocated bytes, excluding allocator overhead and fixed state;
preparation also needs a 52-byte local frame before saved registers. The prior 40-byte
memo is removed. Neither executable size nor these local figures measure total native
peak RAM or establish feasibility on the proposed 2 MB machine. The unchanged package
is 368,635 bytes. The emulator remains uncalibrated and configured with a 68040 and
substantially more RAM than the product target.

The baseline, candidate and counter batches took 77.52, 57.06 and 56.17 seconds,
respectively (190.75 seconds total); every measurement invocation retained its
60-second cap. Build/startup troubleshooting was separate and made this iteration
longer than intended. Receipts remain in ignored `build/ctbl-before`, `build/ctbl-after`
and `build/ctbl-profile`. One sample per configuration does **not** establish a
regression or improvement at these small timing differences.

Using named struct layouts exposed a host Hunk bug: struct offsets and constants
derived from them were mistaken for relocatable addresses. The scoped constant
classification now handles both dotted identifiers and member expressions; a focused
Hunk regression checks the stack displacement and retains a real address relocation.
The separate documented struct-name-as-size expression gap remains recorded in the
workflow notebook; native frame sizes are derived from final named fields.

Native validation still differs from Rust for UTF-8 and duplicate owner/string/program
payloads; the existing chunk locator also treats a zero-length CTBL as absent.
Existing native limits remain: 16-bit program lengths/row counts and the
4,096-byte reconstruction scratch capacity. This is not a claim of complete malformed
package equivalence. The default-off telemetry extension uses the existing 192-byte
OFVE record at schema 2, with no retained schema-1 decoder. Disabled macro expansions
have been checked to emit no bytes or observer imports.

Validation also passes the real-native lifecycle batch (29 commands, including
failed selection, reload, malformed program-index rejection and recovery), 14 host
struct tests, nine host Hunk checks, disabled-telemetry byte equivalence, enabled
full-CLI assembly, assembler-library Clippy, formatting of 250 native files, and
`make workflow-gate` including all 136 workflow tests. The lifecycle harness needed
the shared debug include path after telemetry became a runtime dependency; its
fresh challenge/oracle/exit checks remain unchanged. The Hunk repair is committed
separately as `332e3960` so the native experiment can be reverted without losing it.
No full-workspace, real-hardware, allocation-failure or complete malformed-package
qualification is claimed.

**Decision for review:** this is a recoverable implementation experiment, not a
performance success or authorization to continue adding CTBL metadata. Review whether
to retain the preparation boundary as a foundation or revert its directory cost.
The next useful design question is shared name/program lookup preparation across
execution routes: removing repeated compressed-string work could help, but a CTBL-only
change has limited reach in the measured 68020 case. Agree that next slice and its
memory/coverage tradeoff before implementing it.

## Iteration size and interaction

Proposed starting budget: 30–60 minutes of active work and approximately 15,000
total agent tokens per iteration, including briefing, delegation and verification.
These are planning ceilings, not measured performance promises or mandatory
spending. Report available usage honestly; do not invent exact token accounting.
Report long compile/emulator waits separately and include them in elapsed time.
Recalibrate these budgets with Erik after the first two iterations.

Measurements must fit inside the iteration, not become hours-long background
work. Proposed starting limits: 60 seconds wall time per measurement invocation
and five minutes for the whole measurement batch, including setup, warmups,
repetitions and attribution runs. Enforce both limits in the measurement tool;
terminate and clean up owned child processes on timeout. Count emulator startup
in the wall-time budget even when reporting guest execution time separately.
Tune workload size downward when needed, not the timeout upward. If useful
coverage cannot fit, report that limitation and change the experiment with Erik.

Do not run the current native self-host test for this work: it does not finish
in a useful time. Neither Rust nor native full-self-host profiling is an initial
deliverable or an automatic option in the new comparison command. Full
self-assembly remains a later product acceptance target for the replacement,
selected with Erik only after bounded workloads give credible feasibility evidence.

Each iteration answers one consequential question and ends with something Erik
can run and inspect: an executable comparison, working assembly path, or a
measured replacement. Inspection alone is bounded preparation within an iteration.
Do not consume several iterations on an audit, framework or report before behavior.

Before starting, briefly state the hypothesis, demonstrable outcome, comparison
and stop condition. Within that scope work autonomously. If the budget is at risk,
reduce to a still-useful demonstration or preserve an honest checkpoint and
discuss the obstacle; do not silently extend the experiment. A failed hypothesis
can be a useful result, but an incomplete prototype is not integration-ready.

At the end, provide the exact run command, relevant results and limitations, a
compact diff explanation and the proposed next outcome. Pause for Erik's review
between iterations in this interactive workstream. This is not a commit gate:
use recovery commits whenever useful, and qualify integration independently.

Reuse deterministic scripts and existing profiling. Delegate only separable work
whose savings exceed coordination and verification costs. Run focused checks
during development and broader checks at affected integration boundaries; avoid
unchanged full-suite reruns. Keep one living plan, no per-iteration sidecars.

## Work tracker

This table tracks outcomes, not individual edits or commits. Update the current
row in place with the run command, concise result and relevant commit when work
finishes. Proposed means awaiting agreement on scope, not queued for automatic
execution. W1–W3 and the bounded comparisons are reviewed baseline work. The
M1/M2 implementation is complete. M3 compiles expressions once and passes its
bounded native cases, but the measured expression workload is about 5–10% slower.
M4 folds constants correctly but its measured release totals do not improve on
M3. Review preparation cost before widening coverage.

| Work | Status | Reviewable result | Depends on |
| --- | --- | --- | --- |
| W1 — Measure shared package-VM work across families | Complete; reviewed | Runnable cross-family baseline and verified VM attribution; brief results below | Authorized in conversation |
| W2 — Reduce shared runtime-model setup cost | Complete; reviewed | Compact selector string validation uses a temporary index; comparative results below | W1; authorized in conversation |
| W3 — Measure VM work and repetition | Complete; reviewed | Per-engine/pass/program dispatch and repetition counts at all three workload sizes | W2; authorized in conversation |
| R1 — First native replacement experiment | Measured; no meaningful timing gain | Prepared CTBL comparison and resource accounting below | Retained reference implementation |
| Binary-source experiment | Implemented and measured | Complete mixed8 parity on both targets; observed 8–11× combined-path gains | `a378ec48` |
| M1 / M2 — Compact native runtime | Complete | Shared interpreters, streaming preparation and released lexical storage; both mixed8/mixed32 targets complete on a 68020 / 2 MiB guest | Binary-source experiment |
| M3 — Prepared numeric expressions | Measured; review tradeoff | Shared execution and multiplication work within 2 MiB; expression-replay32 takes 2.18/4.69 s versus M2 1.98/4.48 s | M2; no CLI migration |
| M4 — Preparation-time constant folding | Retained for size savings | Expression payloads shrink 31.7%; no demonstrated total-time gain | M3; unchanged format, limits and coverage |
| M5 — Compact runtime expressions | Measured; ready for review | Expression bytes shrink another 54.4%, packed records 25–27%; release totals broadly unchanged, linked code +428 B | M4; unchanged semantic coverage |
| M6 — Preparation-cost attribution | Measured | Tokenization/binding dominate (~72–74%); expression preparation ~8–11%; release image unchanged | Indexed binding proposed next; not started |
| M7 — Indexed name binding | Measured | Release totals −11% / −19%; temporary capacity +32 KiB, retained memory unchanged | M6; same 2 MiB guest and semantic limits |
| M8 — Tokenizer attribution | Measured | 83–84% of conditional branches untaken; deferred target decoding gives modest 1–3% elapsed reduction, image −8 B | M7; VM counts/semantics and owned allocation unchanged |

## W1 agreement: focused package-VM baseline

**Hypothesis:** shared instruction selection and operand processing repeat work
across statements or passes that can be removed by preparation or binding across
families. W1 tests this hypothesis; it does not presume dispatch is the dominant
cost or prioritize whichever family has the largest absolute runtime.

**Concrete workload:** a deterministic shared workload shape with representative
MOS 6502, Z80 and Motorola 68000-family source variants, using 8, 32 and 128
independently labelled blocks. Each block uses approximately six to eight
statements exercising operand selection, a local forward branch, a symbol-derived
expression and a data value. Use each family's appropriate immediate/register/
memory forms; do not force identical instruction counts or encodings to imply
equal work. Vary operands and constants without making branch distances grow
with the whole file. Reuse forms already covered by existing fixtures.

These are representative probes of shared mechanisms, not target-specific
optimization tracks or an exhaustive CPU matrix. Report the executed programs
and shared operations, and compare each case against its own baseline. This
iteration covers selection/operand and pass work, not module loading, macros or
full self-hosting. Include a small expected-failure companion for each family.

**Files and entrypoint:** implement `scripts/performance/vm_efficiency.py` and
focused tests under `scripts/performance/tests/`, with a short usage section in
the performance documentation. Keep workload generation with the runner unless
a hand-written fixture is clearer. Reuse existing profiling and package-loading
APIs; do not create a generic benchmark framework or duplicate the corpus runner.
The entrypoint and focused tests are implemented. See the
[measurement guide](../performance/vm-efficiency.md) for evidence limits and output:

```sh
python3 scripts/performance/vm_efficiency.py selection --blocks 8,32,128
```

### W1 result for review

The 8/32/128-block batch completed in 2.14 seconds (27.94 seconds build/setup on
the qualifying run). All nine cases matched independently calculated bytes on
every warmup, sample and attribution invocation; all three negative cases passed.
Eight runner tests cover byte contracts, VM attribution, environment isolation,
wrong-output rejection, crash rejection and invocation/batch timeout behavior.
The actual middle-size workloads contain 259 source lines and 160 instructions.
Each family recorded 259 VM parses and 320 VM encodes across two passes.

Uninstrumented medians were approximately 38–53 ms across sizes/families. Separate
32-block profiles attributed 19–21 ms to one assembler model-bootstrap call
versus 0.8–1.1 ms to instruction encoding across both passes. That favors investigating
shared model preparation before specialized instruction fragments. It does not
yet isolate generation, decoding or indexing within bootstrap, nor establish
native payoff. W2 should first distinguish avoidable model construction from
necessary preparation, then implement one cheaper path within its own budget.

The canonical package was 368,579 bytes. Peak memory was not measured; the guide
explains the limited storage sensitivity estimate. Z80 used supported `MVI r,n`
because VM-only `LD r,n` failed in the smoke test; that syntax gap remains open.
No native or full-self-host run was performed. Results live under ignored build
directories and can be regenerated; this paragraph is the retained decision.

Build once as a separately reported setup step, then reuse that identified
executable for all runs. The runner must not silently rebuild on every sample.
Retain source, binary output and a compact machine-readable summary only in a
user-selected output directory or ignored build directory for inspection; no
committed measurement ledger. Use one warmup and three uninstrumented samples per
size and family, plus one attribution run per family at the middle size, all
within the same five-minute batch deadline, not five minutes per family.
If variance prevents a useful timing conclusion, report work counts and the
uncertainty rather than adding unbounded repetitions. Native measurements are
not part of W1.

**Acceptance:** the command completes within the agreed measurement caps; the
successful cases produce independently checked output through the actual
package-VM paths; the negative companions yield the expected failures; the summary
separates setup/execution and shows which repeated operation is worth addressing
next across families (or why none is established). Show per-family work counts
and timings; an aggregate improvement must not hide a regression. Report the
chosen reference boundaries and
prove that a host family-handler bypass did not supply the measured VM result.
Test timeout/batch-budget failure behavior as well as workload correctness.
The tool's source, generated assembly, output and concise result are Erik's
review surface. No runtime-package or accelerator implementation is required.

**Budget and review:** use the proposed 30–60 minute / approximately 15k-token
iteration budget and the 60-second invocation / five-minute batch limits above.
Reduce block counts if necessary while preserving the mechanism and documenting
the change. Retain representative family coverage rather than silently reverting
to a single target. If execution-path attribution or an independent comparison cannot be
established within budget, stop with a precise blocker; do not claim W1 done.
Review the result with Erik before choosing W2's optimization and success target.

### W1 measurement details

Compare baseline and candidate on identical workload sizes and inputs. Record
what the workload represents and what it omits. Completed small cases establish
their own correctness and cost, not full-product throughput. Prefix/abort probes
may localize cost but must be labelled incomplete; a timeout is a failed
measurement, never a speed sample or justification for a longer automatic retry.

Reuse current prepared-package and prepared-line mechanisms. Trace only the
dominant path needed to distinguish remaining decoding, lookup, parsing, dispatch,
copying and repeated pass work. Confirm which Rust execution mode actually uses
package VMs; family-handler bypasses must not be misreported as VM performance.
Add counters only where existing observations cannot answer that question.

The command should identify source/package/build/environment, separate preparation
from execution, and summarize phase times plus relevant work counts. Run timing
without detailed tracing; collect attribution separately. Report available memory
measurements and their limits, plus a first native footprint estimate distinguishing
fixed package/runtime data, per-statement/symbol state and temporary storage.
Do not equate Rust heap size with native memory consumption.

## W2 result for review

**Question:** can shared model setup be reduced without changing canonical packages
or adding a persistent runtime cache? The assembler already retains its model
between passes. A register-validation index showed no useful timing gain and was
discarded. Temporary decode probes identified compact selector decoding as the
largest decoder cost; those probes were removed after localization.

The retained change replaces repeated linear duplicate-string scans in the compact
selector decoder with a sorted index of wire positions. Prefix reconstruction,
string references, exact case-sensitive uniqueness and first-error order remain
unchanged. It supports every existing compact selector version. Comparisons become
O(N log N); arbitrary unsorted tables can still require O(N²) index moves. The
current generated table is sorted, so insertion appends. No CPU semantics changed.

Run the same W1 command to inspect the candidate:

```sh
python3 scripts/performance/vm_efficiency.py selection --blocks 8,32,128
cargo test -p package --lib --locked
```

Two reference/candidate comparisons used identical package, source and output
hashes. Each complete measurement batch took about two seconds; repeat build/setup
was 36 seconds for reference and 41 seconds for candidate. The repeat's 32-block
measurements were:

| Family | Whole-process median, reference → candidate | Model-bootstrap attribution, reference → candidate |
| --- | --- | --- |
| 6502 | 39.83 → 35.93 ms | 20.04 → 17.31 ms |
| Z80 | 40.80 → 36.77 ms | 20.35 → 17.44 ms |
| 68000 | 41.14 → 38.96 ms | 20.08 → 17.08 ms |

Bootstrap decreased across all three families in both comparisons, about 10–15%.
Whole-process results are noisier: the first 8-block 68000 median rose from
37.61 to 39.39 ms; the repeat fell from 40.40 to 38.31 ms. These small fixed-order
samples support the setup improvement, not a universal throughput guarantee.
All nine positive cases and three negative companions passed in both comparisons.
All 101 package tests passed, including new randomized uniqueness comparisons,
wire-order/case coverage, prefix reconstruction and error-precedence checks.

The package remains 368,579 bytes and the measured host executable 4,291,808 bytes.
The 2,958-string table needs 23,664 bytes of temporary indices on this 64-bit host
(11,832 with 32-bit indices), freed before selector decoding. This is additional
scratch capacity, not measured peak memory or a native implementation claim.
String contents are not duplicated by the index. No native/self-host run occurred.
Comparative artifacts remain under ignored `build/w2-*` directories.

**Next proposed outcome:** continue simplifying shared preparation before adding
specialized fragments. Semantic programs are currently decoded once for compact
operand-record validation and again for retention; selector plans also expand
compact fields into strings. Measure one of these paths and remove one justified
piece of repeated work while accounting for preparation memory. Agree the next
slice after reviewing W2; W3's acceleration direction remains conditional.

## W3 result: VM work, repetition and tokenizer equivalence

Opt-in aggregate counters separate bytecode dispatch, decoded operations, repeated
invocations and repeated positions within calls. Timing samples disable collection.
Every case runs automatic and forced-generic tokenizer modes with the same binary,
package and source. The [guide](../performance/vm-efficiency.md) defines coverage,
units, mode selection and exclusions. This is not a whole-machine instruction count.

```sh
python3 scripts/performance/vm_efficiency.py selection --blocks 8,32,128
cargo test -p vm --lib tokenizer_fast_equivalence --locked
```

At 32 blocks (160 assembled instructions and 259 source lines):

| Family | Automatic operations | Forced-generic operations | Generic operations/instruction | Tokenizer dispatches added |
| --- | ---: | ---: | ---: | ---: |
| 6502 | 6,476 | 14,998 | 93.74 | 8,522 |
| Z80 | 6,732 | 17,302 | 108.14 | 10,570 |
| 68000 | 7,020 | 18,070 | 112.94 | 11,050 |

Both modes include 320 decoded steps for the 68000 case; the rest are dispatched
bytecode operations. Automatic tokenization uses an existing Rust fast path whose
logical budget counts exactly match the added generic dispatches on all nine
paired workloads. Those logical counts never enter automatic-mode dispatch totals.
Observed growth is linear at 8/32/128 blocks. Generic mode disables only this
specialization: helper internals, decoding, declarative state and other Rust work
still remain outside these counts, so neither mode establishes native feasibility.

Generic tokenizer execution is the largest measured instruction contribution.
At the middle size it visits 16 distinct bytecode positions over 259 invocations,
with 4,870/6,790/7,398 repeated visits within invocations respectively. The shared
statement parser executes 4,166 operations over 258 invocations and 24 positions,
with no within-invocation repeats; its repetition is across statements. Parsing
is already reused for pass 2. These cases do not exercise the instrumented EXVM
expression-parser executor, which limits generality.

There is also repeated selection work: 68000 descriptor selection attempts 480
candidates per pass, rejects 352 and produces 128; 6502 attempts 192, produces
160 and returns 32 intermediate candidate errors. Z80 uses a different selection
route, so these counters do not quantify its candidate work. Repeated bytecode or
candidate discovery does not itself prove equivalent inputs/state or redundancy.

The paired measurement batch took 4.42 seconds (0.34 seconds cached setup; the
preceding rebuild took 31.69 seconds). All eighteen positive cases and six negative
companions passed; paired sources, outputs and negative diagnostics match. The
small host timing samples show no consistent fast-path advantage; they are not a
native speed estimate. Fast/generic tokenizer equivalence tests passed for full
tokens/spans and exact diagnostics across three families, explicit edge inputs,
128 deterministic fuzz lines and reduced token/lexeme/step budgets. Successful
logical-step and generic-dispatch correspondence is checked explicitly.

Other qualification: 410 existing VM tests, 27 expression tests, three collector
tests, one actual-executor count test and ten runner tests; affected-core Clippy,
formatting and engineering guards. The VM-only CLI retains existing unused-code
warnings. No full-workspace or native qualification is claimed. Measurement
artifacts remain in ignored `build/w3-*` directories.

## Rust/native comparison and reset decision

The shared-data repair is the baseline for this comparison. Its focused correctness
results and remaining expression/reservation gaps are recorded in the
[boundary plan](package-execution-boundaries.md). The reusable
[comparison command](../performance/vm-efficiency.md#minimal-rustnative-comparison)
adds no production runtime changes. Results below are from the current working
code, including the uncommitted shared-data repair, with one identical 368,635-byte
canonical package. Raw receipts and hashes remain in ignored
`build/runtime-comparison-initial/summary.json` and
`build/runtime-comparison-reduced/summary.json`.

| Source target | Blocks / instructions | Rust auto | Rust generic tokenizer | Native guest interval |
|---|---:|---:|---:|---:|
| m6502 | 8 / 40 | 37.32 ms | 30.68 ms | 4.81 s |
| m6502 | 32 / 160 | 43.90 ms | 34.75 ms | 16.05 s |
| m68020 | 8 / 40 | 38.48 ms | 32.00 ms | 6.73 s |
| m68020 | 32 / 160 | 39.90 ms | 38.72 ms | No completed result (deadline) |
| m68020 | 16 / 80 | 1885.60 ms | 1658.42 ms | 13.41 s |

The initial batch took 158.29 seconds; preparation built the release CLI and test
executable in 66.60 seconds separately. The larger native m68020 case hit the
35-second emulator wait deadline, which includes boot: this supplies neither
completed parity nor a 35-second lower bound on assembly time. It was reduced
rather than extending the deadline. The reduced batch took 60.48 seconds;
combined measurement execution took 218.77 seconds. Rust timings are medians of three unprofiled
fresh-process samples; the reduced batch jumped to 1.66–1.89 seconds despite
the same binary/package, versus 31–44 ms in the initial batch. The cause was
not investigated in this bounded slice, so cross-batch Rust timing comparisons
and a tokenizer speed claim are not supported; each native success is one host-observed guest START/DONE
interval with fresh protocol, explicit zero exit and exact output equality
(Level D). Rust output also matches independently constructed workload bytes.
Native samples exclude boot, but include command loading and output overhead.

The emulator is configured as an A4000/68040 with 2 MiB chip, 8 MiB fast RAM and
an additional runner-forced 64 MiB Zorro III RAM (plus RTG configuration). It is
not clock-calibrated and is not the proposed 68020/2 MB machine. No native peak
RAM, native VM-opcode count or total compatibility-code footprint was measured.
Do not extrapolate these cases to full self-assembly or report a hardware speed
ratio. The Rust executable is 4,324,880 bytes on this host; that is not native
executable size or application working memory.

### What this changes in the reset

Four times the 6502 workload raised native time from 4.81 to 16.05 seconds:
there is substantial cost beyond fixed startup. Rust's initial tens-of-milliseconds
process samples and the later timing instability show no reliable
automatic-tokenizer advantage and cannot
rank native bottlenecks. At 32 blocks, forced-generic Rust dispatched 14,998
counted VM steps for 6502 and 19,545 for m68020 (about 94 and 122 per assembled
instruction, including data/labels and both passes). These are nonuniform work
units, excluding many helper internals; they are not native work counts.

Keep the reset's Rust semantic reference and general VM-first direction, but use
bounded native evidence early. Neither indiscriminate native tuning nor a runtime
package compiler follows from these timings. The next proposed slice is one
bounded attribution comparison on the completed small workloads using the
existing approved native runtime counters, alongside Rust counters. Identify
whether repeated parsing, program execution, lookup/decoding or service traffic
is the useful common target. Then select one general prepare-once/reuse or
program-optimization experiment, measuring work avoided, complete-case time,
representation size and correctness. This is now the authorized preparation for R1; it does not authorize a new
instrumentation framework or a prolonged inventory.

### Canonical optimization and version overhead

Optimizing canonical bytecode programs is a valid first layer: eliminate redundant
operations or unreachable paths, simplify branching and improve generated program
structure while preserving state, errors, source spans and budget behavior.
That does not require a second package format. Decoded operations, resolved
references, fused handlers or platform lowering form a derived runtime
representation; prototype in memory before deciding a persistent format is useful.
Neither approach should create independently hand-maintained target semantics.

Version numbers must be interpreted per contract. TKVM, PRVM, EXVM and EXPR are
different machines; SEMV tags also distinguish different program kinds. An exact
current-version package-load check is useful validation, not legacy execution.
Real multiple-version paths remain in expression parsing/evaluation, scalar value
and operand-record execution. Some encoding/fixup variants share executors and
pay selection/decoding cost per program rather than per opcode.

The concrete hot-loop example is native `exprvm_runtime.asm:evalLoop`: four
68000 instructions clear/load the selected version, compare with V2 and branch
on every expression opcode, followed by separate V1/V2 dispatch paths. Rust's
`eval_portable_expr_program` selects its V1/V2 evaluator once per evaluation.
Removing obsolete contracts can save handlers, compiler/decoder paths and this
recurring native selection work. Exact linked bytes and elapsed-time savings
remain unmeasured; they require a controlled latest-only A/B migration, not
source-line counts. Keep mismatch rejection and migrate generators plus both
executors atomically; do not treat all older-numbered program kinds as obsolete.

## Longer-term direction

After the first qualified replacement, grow coverage through bounded complete workloads, then qualify full
self-assembly only when feasibility evidence justifies it and Erik selects that
step. Integration removes superseded code and updates the affected technical and
user documentation. No native rewrite, global semantic audit, version purge or
multi-platform package compiler is a prerequisite for W1. Later outcomes may need
several iterations; each must still end with a useful implementation to inspect.

## Constraints on experimentation and integration

- Derive size and emission behavior from shared semantics if separating them.
  Exercise unresolved symbols, relaxation and changing state; do not maintain
  independent encoders that merely agree on easy fixtures.
- Choose optimizations and accelerator boundaries by shared operations or
  validated program structure. Family-specific fixture syntax is expected;
  family-name dispatch or hand-coded target semantics in generic runtime paths
  is not. Extend representative coverage when a new program shape matters, and
  run relevant cross-target correctness checks at integration. Gains need not be
  identical across families; report regressions and unmeasured targets honestly.
- A runtime package is reproducible derived data. Validate its canonical input
  identity, representation version and required executor capabilities. An initial
  in-memory prototype need not invent a persistent file format. Keep one supported
  current version per affected contract. Before 1.0 there is no bytecode backward
  compatibility requirement: migrate package generation and Rust/native consumers
  together and remove superseded versions. Version tags validate compatibility;
  they do not require keeping old executors.
- Avoid requiring complete canonical and prepared representations simultaneously
  in the target's RAM. Include preparation peak and generic fallback in the budget.
- Do not add responsibilities to the selection-service monolith. Each replacement
  names its owned state, dependencies and the old responsibility it removes.
  Temporary parallel paths are experimental; integration removes replaced code
  rather than accumulating delegates and permanent experiment switches.
- Judge gains on complete workloads including preparation. A fast microbenchmark,
  dispatch reduction or timeout escape is not product success. Here "complete"
  means the selected focused case completes assembly, not that every iteration
  assembles the whole product. Keep correctness cases with different shapes to
  detect workload-specific assumptions.
- Stop, revise or discard experiments that do not justify their complexity or
  memory. Generated/AOT code is another hypothesis, not an automatic fallback plan.
- Update this plan in place after review. Move durable decisions into maintained
  references and delete the completed plan; Git is the historical record.
