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

## Next iteration: a runnable efficiency baseline

Question: which repeated VM work offers the strongest opportunity within the
Amiga's memory budget, and can we measure it without a new profiling framework?

Deliver a small repository-local comparison entrypoint, built on existing
performance tools and Rust profiling, with a documented quick command that Erik
can run. Its default demonstration must be short; full-workload profiling is an
explicit mode. Exact invocation and initial results replace this paragraph when
implemented; no command is claimed to exist yet.

Use the actual native self-host source/package as the primary workload. Add one
small, semantically complete workload exercising repeated instructions, forward
references/layout and a state change. Prefix/abort probes may localize cost but
must be labelled incomplete and must not claim full correctness or throughput.

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

Done when the quick command completes, checks actual assembly output against the
reference, and exposes enough evidence to select one optimization with an explicit
correctness boundary and expected whole-workload benefit. Record only the brief
decision and reproducible command here. If profiling cannot identify a useful
candidate within budget, report exactly what is missing; do not start cleanup or
invent an optimization to satisfy the plan.

No native rewrite, global semantic audit, version purge, general accelerator
framework or multi-platform package compiler belongs in this iteration.

## Subsequent outcomes, refined one at a time

| Outcome | Implementation Erik can inspect and run | Evidence needed before extending it |
| --- | --- | --- |
| One cheaper VM path | Rust assembles a complete focused workload through a derived runtime representation, compared with canonical/reference execution | Less repeated work, matching artifacts and relevant diagnostics/state; preparation, execution and memory costs reported |
| One useful acceleration point | The same path can use one specialized fragment or generic VM execution through a defined boundary | Equal results and failure behavior, material benefit attributable to the fragment; unsupported cases retain generic execution |
| An early native feasibility slice | The compact representation runs a complete focused assembly on the 68020 | Fresh native proof, measured native time and memory, updated estimate against the 2 MB/15-minute target |
| Growing useful coverage | Broader source workloads use the improved path; bounded additional iterations add only evidence-backed mechanisms | End-to-end gains, controlled invalidation across state/layout changes, and manageable code/data growth |
| A maintained native product path | Complete self-assembly with the replacement integrated and superseded implementation removed | Correct executable/artifacts, hardware-budget qualification, relevant broad checks and current technical/user documentation |

These rows are not five promised single-iteration tasks. Split large outcomes into
complete testable workloads at the next planning boundary. Choose acceleration,
preparation or size/emission separation based on measured cost; do not force every
mechanism into the design. Reach native feasibility before broad coverage work.

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
  dispatch reduction or timeout escape is not product success. Keep correctness
  cases beyond the self-host workload to detect workload-specific assumptions.
- Stop, revise or discard experiments that do not justify their complexity or
  memory. Generated/AOT code is another hypothesis, not an automatic fallback plan.
- Update this plan in place after review. Move durable decisions into maintained
  references and delete the completed plan; Git is the historical record.
