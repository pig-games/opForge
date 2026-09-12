# Iterative VM and native runtime reset

Status: W3 implemented and locally validated; awaiting review before further optimization.
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
- First improve general VM execution efficiency in Rust: eliminate repeated
  work, identify hotspots and provide explicit acceleration points. Native
  platform optimization follows, with early native feasibility checks.
- Optimize shared execution mechanisms for all supported assembly targets, not
  one CPU/family's encoding path. The 68020 product requirement describes the
  execution platform. Use several assembly families as evidence of generality;
  do not equate representative coverage with measured gains on every target.
- Canonical packages remain authoritative. Derive runtime representations from
  them for selected assembly-target capabilities and execution-platform resource
  profiles. These are independent dimensions. Start with one experimental runtime
  representation, not a C64/Amiga/modern-host implementation matrix.
- Specialized fragments implement package-defined operations; they do not become
  a second manually maintained set of CPU semantics. Bind through validated
  program structure or explicit package bindings, never benchmark identity.
- Preserve independent reference execution and existing correctness evidence.
  Native confirmation follows the [parity contract](../../agents/rules/native-rust-parity-porting.md).
  The reference need not be resident alongside a runtime package on the Amiga.
- Compact source, executable size, package size and working memory are separate
  concerns. Runtime preparation must earn its cost in both time and space.

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
execution. W1–W3 were authorized in conversation; later work is not yet authorized.

| Work | Status | Reviewable result | Depends on |
| --- | --- | --- | --- |
| W1 — Measure shared package-VM work across families | Complete; reviewed | Runnable cross-family baseline and verified VM attribution; brief results below | Authorized in conversation |
| W2 — Reduce shared runtime-model setup cost | Complete; reviewed | Compact selector string validation uses a temporary index; comparative results below | W1; authorized in conversation |
| W3 — Measure VM work and repetition | Complete; review pending | Per-engine/pass/program dispatch and repetition counts at all three workload sizes | W2; authorized in conversation |
| W4 — Test the compact representation on native 68020 | Direction only | Bounded complete native assembly with fresh parity and resource measurements | A useful Rust representation; move earlier if native feasibility is the largest uncertainty |

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

**Next decision for review:** B1 of the short
[package-execution boundary plan](package-execution-boundaries.md) now exposes
target-semantic callback attempts and includes the corresponding native source
scan. Review its findings and select one shared operation before further
optimization. Afterwards, candidates include reducing generic
tokenizer dispatch through package-derived preparation and reusing prepared
candidate discovery across passes. The tokenizer loop is a demonstrated generic hotspot, but copying
its hand-maintained Rust fast path into native would create another correspondence
to maintain. Any specialization needs the canonical generic path and explicit
equivalence checks; any selection reuse must preserve changing values, instability
and layout and reuse the existing prepared-route mechanism where possible. Do not
select a native strategy from modern-host timing alone.

## Longer-term direction

After W4, grow coverage through bounded complete workloads, then qualify full
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
  current version per affected contract; migrate only when the chosen work needs it.
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
