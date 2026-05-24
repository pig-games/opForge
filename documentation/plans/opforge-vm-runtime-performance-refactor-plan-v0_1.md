<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# Plan: opForge VM Runtime Performance Refactor

## Metadata

- Source: user request, 2026-05-24: reevaluate the Rust-side opForge VM/package
  performance approach and preserve the larger-win optimization path in a
  traceable plan.
- Mode: implementation
- Owner: Codex
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.
- Artifact type: plan
- Relevant skill: `skills/opforge-plan-authoring/SKILL.md`
- Workflow wrapper or validator: `scripts/workflow/start_artifact.py` via
  `scripts/workflow/new_artifact_from_template.sh`; validate with
  `scripts/workflow/check_plan_checkboxes.py`,
  `scripts/workflow/check_workflow_artifact_bundle.py`, and before committing
  implementation slices `scripts/workflow/run_plan_workflow.sh`.
- Validation status: plan workflow complete; see sidecar quality-gate file for
  latest command evidence.

## Goal

Create an execution-ready optimization roadmap that keeps follow-up work aimed
at large performance wins in the VM/package implementation paths instead of
drifting back into small allocation-only improvements.

The plan targets the remaining high-cost shape after the first optimization
series:

- Runtime parse replay has been reduced substantially by successful parse reuse.
- Pass1 stabilization still reruns almost the whole expanded program.
- Repetition traversal still rediscovers control structure and execution shape.
- Cached ASTs still route through semantic dispatch and instruction binding on
  every execution.
- The phase profiler currently blends stabilization `pass_num = 2` work into
  `pass2.*` buckets, which can mislead later prioritization.
- No-listing CLI assemblies still format listing text into `std::io::sink()`,
  which is a major product-path cost even when no listing file is requested.

Primary objective: introduce persistent prepared-line execution state as the
foundation for later bound-route reuse and incremental stabilization, without
changing assembler semantics, diagnostics, listing output, or package/CPU
architecture boundaries.

## Version Impact

- Affected component(s): `asm`, `engine`, `vm`, CLI assembly execution,
  profiling output, and internal plan/workflow artifacts.
- Impact class: patch
- Owned contract: assembler semantics and output compatibility remain owned by
  existing tests, golden/reference checks, and full AmigaOS workload byte
  comparisons.
- Rationale: The planned changes alter execution structure and profiling only;
  there is no intended syntax, diagnostics, listing, binary output, package, or
  public API contract change unless an item explicitly updates this section
  before implementation.

## Work Items

- [x] Item 1: Repair performance profiling attribution before deeper refactors
  - Source requirement or finding IDs: user request to pursue bigger wins;
    observation that stabilization calls `run_layout_pass(..., pass_num = 2,
    ...)`, so current `pass2.parse_line_ast`, `pass2.line_route`,
    `pass2.expr_eval`, and `pass2.symbol_lookup` buckets include stabilization
    retry work.
  - Expected files: `crates/opforge-asm/src/phase_profile.rs`,
    `crates/opforge-asm/src/engine.rs`, and narrowly related profiler call
    sites.
  - Full quality gates: `cargo fmt --all`; `cargo check -p asm`;
    `cargo check -p engine`; focused profiler tests if present or added;
    `scripts/workflow/run_rust_quality_gate.sh`; full native AmigaOS workload
    with `OPFORGE_PROFILE_PHASES=1 OPFORGE_PROFILE_EXECUTION_PATHS=1`.
  - Plan-compliance review evidence: `scripts/workflow/run_plan_workflow.sh`
    for this plan must return `PASS` before committing the item.
  - Commit outcome: completed in one profiler-only commit; assembly output is
    unchanged. `scripts/workflow/run_rust_quality_gate.sh` still fails on the
    known pre-existing CPU-boundary findings, accepted by maintainer for this
    profiler-only item.
  - Definition of done: profiler output distinguishes initial pass1,
    stabilization retry, and final pass2 parse/route/expression/symbol/listing
    work; profiling remains opt-in and disabled by default; full workload output
    is byte-for-byte identical to the pre-item output.

- [ ] Item 2: Introduce persistent `PreparedLine` IR without changing execution
  - Source requirement or finding IDs: user request to prioritize VM/package
    paths and avoid repeated reconstruction from source text; current successful
    parse cache still clones cached ASTs into `AsmLine::process_ast` for every
    execution.
  - Expected files: `crates/opforge-asm/src/line.rs`,
    `crates/opforge-asm/src/engine.rs`, possibly new
    `crates/opforge-asm/src/prepared_line.rs`, and focused tests in
    `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: `cargo fmt --all`; `cargo check -p asm`;
    `cargo check -p engine`; relevant VM/package parser and repetition tests;
    `cargo test -p asm qualified_use_reachability_perf_regression_multi_module_fixture`;
    `scripts/workflow/run_rust_quality_gate.sh`; full native AmigaOS workload
    with listing byte comparison before/after.
  - Plan-compliance review evidence: `scripts/workflow/run_plan_workflow.sh`
    for this plan must return `PASS` before committing the item.
  - Commit outcome: one commit that creates and threads a prepared-line data
    structure while preserving the existing `process_ast` behavior as the sole
    semantic executor.
  - Definition of done: expanded top-level lines can be prepared once with
    source text, line number, cached AST, end span/token, route/cache identity,
    and safe trace/lockstep policy; execution results, diagnostics, and listing
    output remain unchanged; no CPU-specific Rust logic is added.

- [ ] Item 3: Convert repetition traversal to a prepared block tree
  - Source requirement or finding IDs: current repetition execution repeatedly
    scans for matching `.endfor` / `.endwhile` and reparses or reclassifies line
    shape during each traversal; previous caching made parse cheaper but did
    not remove structural rediscovery.
  - Expected files: `crates/opforge-asm/src/repetition.rs`,
    `crates/opforge-asm/src/repetition_driver.rs`,
    `crates/opforge-asm/src/engine.rs`, prepared-line module from Item 2, and
    focused repetition tests.
  - Full quality gates: `cargo fmt --all`; `cargo check -p asm`;
    `cargo check -p engine`; `cargo test -p asm repetition_`; relevant
    VM/package repetition tests; `scripts/workflow/run_rust_quality_gate.sh`;
    full native AmigaOS workload with listing byte comparison before/after.
  - Plan-compliance review evidence: `scripts/workflow/run_plan_workflow.sh`
    for this plan must return `PASS` before committing the item.
  - Commit outcome: one commit that executes from a prepared repetition tree but
    still delegates regular-line semantics to existing `AsmLine` processing.
  - Definition of done: matching loop boundaries and regular-line/control-line
    classification are computed once per prepared source; loop iteration
    diagnostics and pass1/pass2 iteration consistency checks are unchanged.

- [ ] Item 4: Add non-cloning bound instruction route plans
  - Source requirement or finding IDs: current route work repeatedly performs
    directive checks, family operand parsing, dialect mapping, and instruction
    binding; a previous cloned trait-object style cache regressed, so this item
    must design a lightweight key/borrow-based bound route instead.
  - Expected files: `crates/opforge-asm/src/asmline_instruction.rs`,
    `crates/opforge-asm/src/line.rs`, prepared-line module, and possibly
    registry-facing helper types if a borrow-safe key is needed.
  - Full quality gates: `cargo fmt --all`; `cargo check -p asm`;
    `cargo check -p engine`; relevant VM/package instruction route tests;
    `cargo test -p asm qualified_use_reachability_perf_regression_multi_module_fixture`;
    `scripts/workflow/run_rust_quality_gate.sh`; full native AmigaOS workload
    with listing byte comparison before/after and cache-disabled comparison if
    a kill switch is added.
  - Plan-compliance review evidence: `scripts/workflow/run_plan_workflow.sh`
    for this plan must return `PASS` before committing the item.
  - Commit outcome: one commit that caches or stores only stable instruction
    route decisions and operands in a form cheaper than recomputation.
  - Definition of done: hot instruction lines can reuse family/dialect route
    decisions without cloning large trait objects or changing VM encode
    semantics; fallback and diagnostics match the current implementation.

- [ ] Item 5: Short-circuit listing formatting when no listing output is
  requested
  - Source requirement or finding IDs: measurement on 2026-05-24 showed a CLI
    assembly without `-l` still spent roughly `842ms` in
    `pass2.listing_generation` because the engine writes formatted listing text
    to `std::io::sink()`.
  - Expected files: `crates/opforge-engine/src/lib.rs`,
    `crates/opforge-asm/src/engine.rs`, listing writer abstractions if needed,
    and focused engine/CLI tests for no-listing behavior.
  - Full quality gates: `cargo fmt --all`; `cargo check -p asm`;
    `cargo check -p engine`; `cargo check -p cli`; focused no-listing CLI or
    engine tests; `scripts/workflow/run_rust_quality_gate.sh`; full native
    AmigaOS workload with and without `-l`.
  - Plan-compliance review evidence: `scripts/workflow/run_plan_workflow.sh`
    for this plan must return `PASS` before committing the item.
  - Commit outcome: one commit that bypasses normal listing line/footer
    formatting on success when no listing path exists, while preserving
    diagnostics and requested listing output.
  - Definition of done: no-listing assemblies avoid per-line listing formatting;
    requested listing output remains byte-for-byte identical; diagnostic
    reporting for failing assemblies remains intact.

- [ ] Item 6: Prototype incremental or reduced stabilization execution
  - Source requirement or finding IDs: pass1 stabilization retries remain a
    major cost because each retry reruns nearly the whole expanded program with
    fresh `AsmLine` state.
  - Expected files: `crates/opforge-asm/src/engine.rs`, prepared-line module,
    layout snapshot/state helpers, and targeted layout stabilization tests.
  - Full quality gates: `cargo fmt --all`; `cargo check -p asm`;
    `cargo check -p engine`; existing auto-sizing / layout stabilization tests;
    `cargo test -p asm qualified_use_reachability_perf_regression_multi_module_fixture`;
    `scripts/workflow/run_rust_quality_gate.sh`; full native AmigaOS workload
    with listing byte comparison before/after.
  - Plan-compliance review evidence: `scripts/workflow/run_plan_workflow.sh`
    for this plan must return `PASS` before committing the item.
  - Commit outcome: one commit containing a narrow, safely disabled or
    conservatively enabled stabilization reuse prototype.
  - Definition of done: either a measured reduction in stabilization retry work
    with unchanged output, or a documented rollback/no-go result that preserves
    the prepared-line foundation for later work.

- [ ] Item 7: Reassess product-build feature stripping for tooling traces
  - Source requirement or finding IDs: original optimization request asked to
    verify whether lockstep/runtime traces/LSP/tooling metadata remain on
    production hot paths and can be feature-gated later.
  - Expected files: `crates/opforge-asm/src/line.rs`,
    `crates/opforge-engine/src/lib.rs`, feature declarations if needed, and
    focused trace/lockstep tests.
  - Full quality gates: `cargo fmt --all`; `cargo check -p asm`;
    `cargo check -p engine`; relevant trace/lockstep tests;
    `scripts/workflow/run_rust_quality_gate.sh`; full native AmigaOS workload
    with listing byte comparison before/after.
  - Plan-compliance review evidence: `scripts/workflow/run_plan_workflow.sh`
    for this plan must return `PASS` before committing the item.
  - Commit outcome: one commit only if there is measurable hot-path value;
    otherwise close the item with evidence and no code change.
  - Definition of done: production CLI defaults remain trace-light; editor,
    LSP, lockstep, and diagnostic tooling behavior remains available where
    explicitly requested; any feature gates are opt-in/opt-out documented in
    code and tests.

## Blocking Rules

- no commit before all quality gates pass
- `plan-compliance-reviewer` must return `PASS` before commit
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- archive completed plans with `scripts/workflow/archive_completed_plan.sh`
- if `scripts/workflow/run_rust_quality_gate.sh` fails only on known
  pre-existing CPU boundary findings, record the failure and do not claim the
  item is fully validated until the maintainer accepts that exception for the
  specific commit
- do not add CPU/family/dialect/register/addressing-mode/instruction-specific
  Rust logic to generic VM, Native VM, workflow, or CLI paths
- do not remove existing profilers; all new profiling remains opt-in and
  disabled by default
- do not change assembler output, listing output, diagnostics, or package
  semantics without updating this plan and receiving fresh plan-compliance
  approval
