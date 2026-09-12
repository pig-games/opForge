# Iterative VM and native runtime reset

Status: proposed execution plan for discussion; implementation has not started.
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
execution. No implementation work has started.

| Work | Status | Reviewable result | Depends on |
| --- | --- | --- | --- |
| W1 — Measure a focused package-VM assembly path | Proposed: scope below | One runnable bounded benchmark, inspectable source/output, verified execution path and one ranked optimization recommendation | Agreement on W1 |
| W2 — Eliminate the largest justified repeated operation | Direction only | Same complete workload on reference and improved Rust paths, with correctness/work/time/space comparison | W1 evidence and agreement on the exact change |
| W3 — Prove one acceleration boundary if it is justified | Direction only | Generic and specialized execution selectable for comparison, including unsupported/error cases | Measured hotspot; may be replaced by further VM simplification |
| W4 — Test the compact representation on native 68020 | Direction only | Bounded complete native assembly with fresh parity and resource measurements | A useful Rust representation; move earlier if native feasibility is the largest uncertainty |

## W1 agreement: focused package-VM baseline

**Hypothesis:** varied instruction selection and operand processing repeat work
across statements or passes that can be removed by preparation or binding. W1
tests this hypothesis; it does not presume that dispatch is the dominant cost.

**Concrete workload:** one deterministic 68020 assembly source generator with
8, 32 and 128 independently labelled blocks. Each block uses approximately six
to eight statements mixing immediate, register and memory operands, a local
forward branch, a symbol-derived expression and a data value. Vary registers and
constants without making branch distances grow with the whole file. Use valid
forms already covered by existing fixtures. This targets selection/operand and
pass work; module loading, macros, broad CPU coverage and full self-hosting are
deliberately not claims of this case. Add one small expected-failure companion
to check that the chosen execution route retains diagnostic behavior.

**Files and entrypoint:** implement `scripts/performance/vm_efficiency.py` and
focused tests under `scripts/performance/tests/`, with a short usage section in
the performance documentation. Keep workload generation with the runner unless
a hand-written fixture is clearer. Reuse existing profiling and package-loading
APIs; do not create a generic benchmark framework or duplicate the corpus runner.
These paths and the command below are proposed deliverables, not existing tools:

```sh
python3 scripts/performance/vm_efficiency.py selection --blocks 8,32,128
```

Build once as a separately reported setup step, then reuse that identified
executable for all runs. The runner must not silently rebuild on every sample.
Retain source, binary output and a compact machine-readable summary only in a
user-selected output directory or ignored build directory for inspection; no
committed measurement ledger. Use one warmup and three uninstrumented samples per
size, plus one attribution run at the middle size, all within the batch deadline.
If variance prevents a useful timing conclusion, report work counts and the
uncertainty rather than adding unbounded repetitions. Native measurements are
not part of W1.

**Acceptance:** the command completes within the agreed measurement caps; the
successful cases produce independently checked output through the actual
package-VM path; the negative companion yields the expected failure; the summary
separates setup/execution and shows which repeated operation is worth addressing
next (or why none is established). Report the chosen reference boundary and
prove that a host family-handler bypass did not supply the measured VM result.
Test timeout/batch-budget failure behavior as well as workload correctness.
The tool's source, generated assembly, output and concise result are Erik's
review surface. No runtime-package or accelerator implementation is required.

**Budget and review:** use the proposed 30–60 minute / approximately 15k-token
iteration budget and the 60-second invocation / five-minute batch limits above.
Reduce block counts if necessary while preserving the mechanism and documenting
the change. If execution-path attribution or an independent comparison cannot be
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
