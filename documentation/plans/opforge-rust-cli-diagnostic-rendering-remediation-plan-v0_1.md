<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# Plan: opForge Rust CLI Diagnostic Rendering Remediation v0.1

## Metadata

- Source: User-supplied “opForge Rust CLI error-reporting analysis” (2026-07-21), verified against the current Rust CLI diagnostic boundary in `crates/opforge-cli/src/lib.rs` and `crates/opforge-cli-core/src/run.rs`.
- Mode: remediation
- Owner: Codex
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.
- Artifact type: Rust CLI diagnostic-rendering remediation plan.
- Workflow: `skills/opforge-plan-authoring/SKILL.md`; `scripts/workflow/run_plan_workflow.sh`.
- Validation status: Pending — this plan must pass the plan workflow before activation.

## Goal

Make every failed Rust CLI assembly invocation render the terminal cause exactly
once through the configured diagnostics channel, unless `--no-error` explicitly
suppresses diagnostics. A fatal `AsmRunError` summary must never disappear just
because its attached diagnostic list is empty or contains only unrelated
warnings.

The completed CLI contract is:

- A nonzero assembly exit emits at least one error record, except under
  `--no-error`.
- Existing source diagnostics remain visible and retain their configured classic,
  contextual, or JSON representation.
- The fatal summary is emitted only when no equivalent error diagnostic already
  represents it; warnings must never hide the terminal cause.
- Diagnostic-sink write failure has a last-resort stderr message unless output
  was intentionally disabled.
- JSON-mode failures emit at least one valid JSON error/fatal record.

This plan intentionally does not add duplicate source-local diagnostics to every
engine or module-error constructor. Those improvements may follow later; the
central renderer is the correctness boundary for all present and future fatal
errors.

## Version Impact

- Affected component(s): `opforge-cli`, `opforge-cli-core`, CLI integration-test support, and CLI user-facing diagnostic output.
- Impact class: patch
- Owned contract: fatal assembly, workflow, I/O, and post-processing failures have deterministic human- and machine-readable output.
- Rationale: `AsmRunError` already preserves both a fatal `AsmError` and zero or more source diagnostics, but the CLI renders only the latter. This leaves ordinary missing-input, folder, module, output, and workflow failures silent.

## Current-State Evidence

- `AsmRunError` retains `error`, `diagnostics`, and `source_lines`; its display and summary APIs expose the fatal cause in `crates/opforge-asm/src/error.rs`.
- `process_failed_assembly_run` in `crates/opforge-cli/src/lib.rs` prepares and emits only `error.diagnostics()`, then never renders `error.error()` or `error.summary()`.
- `input_base_from_path`, folder resolution, module-resolution, output, and workflow conversion paths can construct an `AsmRunError` with an empty diagnostic vector.
- `DiagnosticsSink::emit_line` ignores `writeln!` failure and `run_main` discards the `Result` from failure reporting.
- `run_with_cli_with_context` validates twice: once in `run_main` and once inside the runner.
- Existing tests exercise report-processing helpers but do not enforce the executable stderr/stdout contract.

## Constraints

- Keep the correction in the Rust CLI/reporting boundary; do not add CPU, family, dialect, mnemonic, or addressing-mode behavior to generic code.
- Preserve `--no-error` as intentional suppression and `--error FILE` as intentional redirection. A write failure to the redirected sink is not intentional suppression and requires a stderr fallback.
- Preserve one rendered copy of source-located diagnostics. A fatal summary may be added only when it is not already represented by an error diagnostic; a warning never represents a terminal failure.
- Preserve current successful-run warning, fixit, listing, formatter, and output behavior unless a failure-path contract explicitly requires a change.
- Introduce no new external test dependency unless the existing workspace test facilities cannot launch the binary; prefer the existing Cargo executable-test mechanism and temporary fixtures.
- One checkbox item is active at a time. Each item ends with exactly one focused commit after all named gates and a `plan-compliance-reviewer` PASS. No plan-driven commit may precede those checks.

## Milestones and Dependency Graph

```text
M1 Authoritative fatal renderer
  -> M2 Fallible sink and top-level fallback
  -> M3 Preserve workflow fatal metadata and JSON contract
  -> M4 Remove duplicate validation boundary
  -> M5 Executable-level regression matrix and closure review
```

M1 is the minimum user-visible correction. M2 through M4 make the guarantee
reliable for reporting and workflow failures; M5 proves it at the executable
boundary. No broad constructor-by-constructor diagnostic migration is part of
this plan.

## Work Items

- [x] Item 1: add the authoritative terminal `AsmRunError` renderer
  - Source requirement or finding IDs: CLI-DIAG-001 (fatal summary is never rendered); CLI-DIAG-002 (empty diagnostic vectors cause silent input/folder/module/output failures); CLI-DIAG-003 (warnings can mask the terminal failure).
  - Invariant: for an `AsmRunError`, render all eligible attached diagnostics, then render one synthetic fatal error record only if no emitted error diagnostic represents `error.summary()`. The synthetic record carries the fatal error kind/code, summary, known input path, and available source context; warning diagnostics never suppress it.
  - Expected files: `crates/opforge-cli/src/lib.rs` and its focused unit tests; only add shared API changes if the renderer cannot build a `Diagnostic` from the existing public `AsmError`/diagnostic model.
  - Implementation boundary: replace the terminal portion of `process_failed_assembly_run` with an `emit_asm_run_error`-style helper. Use the existing diagnostic formatting path for classic/contextual output; define the fatal JSON object/record at this one boundary so JSON never has zero records on an assembly failure.
  - Required focused tests: zero-diagnostic fatal error; fatal error plus unrelated warning; matching error diagnostic without duplicate fatal summary; fileless fatal error with input fallback; classic/contextual and JSON formatting assertions. Test captured sink contents, not only helper return values.
  - Full quality gates: `cargo test -p opforge-cli`; focused CLI test filters introduced by this item; `scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`.
  - Plan-compliance review evidence: PASS (2026-07-21): diff is limited to the central terminal-rendering contract and focused regressions; no constructor-by-constructor behavior expansion.
  - Commit outcome: one Rust CLI renderer commit with production code written before its focused tests.
  - Definition of done: Complete — CLI-DIAG-001 through CLI-DIAG-003 are fully closed for `AsmRunError` paths, including an empty diagnostics vector and a misleading warning-only vector. Evidence: `cargo test -p cli`, `scripts/workflow/run_rust_quality_gate.sh`, and `make workflow-gate` (2026-07-21).

- [x] Item 2: make diagnostic emission fallible and preserve a last-resort failure path
  - Source requirement or finding IDs: CLI-DIAG-004 (sink write errors are discarded); CLI-DIAG-005 (`run_main` discards reporting/post-processing failures).
  - Invariant: each diagnostics-sink write returns `io::Result`; reporting functions propagate write/fixit/report failures; `run_main` reports a concise direct stderr fallback with both the sink failure and original fatal summary when output was not deliberately disabled.
  - Expected files: `crates/opforge-cli/src/lib.rs` and its tests; no engine changes.
  - Required behavior: `--no-error` stays silent by design. For a failed `--error FILE` sink, do not recursively attempt that sink; write one direct stderr fallback. Successful assembly followed by fixit/report-processing failure must also produce an explanation before exit 1.
  - Required focused tests: custom failing writer returns an error; failing redirected diagnostics sink produces stderr fallback; failure reporting result reaches top level; `--no-error` remains non-emitting; successful assembly plus post-processing failure is visible.
  - Full quality gates: `cargo test -p opforge-cli`; `scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`.
  - Plan-compliance review evidence: PASS (2026-07-21) for fallible reporting and explicit fallback only.
  - Commit outcome: one focused CLI I/O/error-propagation commit.
  - Definition of done: Complete — CLI-DIAG-004 and CLI-DIAG-005 fully close without changing intentional diagnostic suppression. Evidence: failing-writer and `--no-error` focused tests; `cargo test -p cli`, `scripts/workflow/run_rust_quality_gate.sh`, and `make workflow-gate` (2026-07-21).

- [x] Item 3: retain workflow failure identity through the CLI boundary
  - Source requirement or finding IDs: CLI-DIAG-006 (workflow failures are flattened into empty-diagnostic `AsmRunError` values); CLI-DIAG-007 (JSON error output is incomplete).
  - Invariant: non-assembler `AssemblerWorkflowError` variants retain a stable category/code and summary until terminal rendering. Every workflow failure produces at least one human-readable error and, under `--format json`, one valid fatal JSON object containing type, code, kind, message, and input when known.
  - Expected files: `crates/opforge-cli-core/src/run.rs`, `crates/opforge-cli/src/lib.rs`, and focused tests in the owning crates.
  - Design constraint: prefer extending `CliRunError` with a small CLI-owned fatal/workflow payload over manufacturing source diagnostics in the engine. Keep source diagnostics separate from workflow-fatal metadata.
  - Required focused tests: invalid argument, invalid request, I/O, and internal workflow variants each preserve category/summary; JSON serializes stable fields; normal assembler errors retain Item 1’s no-duplicate behavior.
  - Full quality gates: `cargo test -p opforge-cli-core`; `cargo test -p opforge-cli`; `scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`.
  - Plan-compliance review evidence: PASS (2026-07-21) for workflow metadata preservation and JSON schema behavior.
  - Commit outcome: one CLI-core/CLI workflow-error contract commit.
  - Definition of done: Complete — CLI-DIAG-006 and CLI-DIAG-007 fully close for every current workflow-error variant. Evidence: `cargo test -p cli-core`, `cargo test -p cli`, `scripts/workflow/run_rust_quality_gate.sh`, and `make workflow-gate` (2026-07-21).

- [ ] Item 4: pass validated CLI configuration through the runner once
  - Source requirement or finding IDs: CLI-DIAG-008 (duplicate CLI validation permits a second failure after `run_main` has already validated configuration).
  - Invariant: the executable validates exactly once for an assembly invocation, and the runner consumes the resulting `CliConfig` or a deliberately named validated-run entrypoint. Any validation failure is rendered through the same selected output contract where practical.
  - Expected files: `crates/opforge-cli-core/src/run.rs`, `crates/opforge-cli-core/src/lib.rs` if re-export changes are needed, `crates/opforge-cli/src/lib.rs`, and focused tests.
  - Required focused tests: validation is not re-run by the normal main-to-runner path; existing direct caller API remains coherent or is migrated in the same focused commit; validation failure output remains nonempty outside intentional suppression.
  - Full quality gates: `cargo test -p opforge-cli-core`; `cargo test -p opforge-cli`; `scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`.
  - Plan-compliance review evidence: PASS for the validation-boundary change only.
  - Commit outcome: one narrow validated-configuration handoff commit.
  - Definition of done: CLI-DIAG-008 fully closes and no second validation can enter a diagnostic-loss path.

- [ ] Item 5: add executable-level CLI diagnostic contract coverage and complete the remediation
  - Source requirement or finding IDs: CLI-DIAG-009 (no black-box CLI output contract); acceptance evidence for CLI-DIAG-001 through CLI-DIAG-008.
  - Invariant: each tested nonzero executable invocation has nonempty diagnostic output and identifies the terminal cause, unless `--no-error` was supplied. Tests assert exit status and public stdout/stderr/JSON, rather than internal helper state.
  - Expected files: CLI integration-test target(s), temporary input/output fixtures, Cargo manifest only if an existing workspace facility cannot launch `CARGO_BIN_EXE_opforge`, and no production semantic files beyond test-support seams strictly needed for capture.
  - Required matrix: missing `.asm` input with path and “not found”; unsupported extension with accepted-extension guidance; directory with zero/multiple `main.*`; ambiguous module with module name/candidates; output-open/permission failure with output path; failed diagnostics-file write with stderr fallback; `--quiet` fatal visibility; JSON fatal parseability; multi-input terminal input naming; and intentionally silent `--no-error`.
  - Proof discipline: use deterministic temporary paths and a portable failing-output mechanism; if permission denial is not portable, use an existing injectable writer or an OS-neutral invalid target and document the exact guarantee tested. Avoid asserting color/control-sequence layout.
  - Full quality gates: complete integration matrix; `cargo test -p opforge-cli`; `cargo test -p opforge-cli-core`; `scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`; `scripts/workflow/run_plan_workflow.sh` against this plan.
  - Plan-compliance review evidence: PASS confirming every CLI-DIAG finding has direct executable evidence and no broad constructor churn entered the scope.
  - Commit outcome: one executable-contract-test commit, followed only by a documentation/plan checkbox update if required by the repository workflow.
  - Definition of done: CLI-DIAG-009 fully closes; all nine findings have passing evidence; the plan can be archived with `scripts/workflow/archive_completed_plan.sh` only after the implementation commits and completion evidence exist.

## Blocking Rules

- no commit before all quality gates pass
- `plan-compliance-reviewer` must return `PASS` before commit
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- if a renderer change would duplicate a source diagnostic, stop and refine the representation/deduplication predicate before committing
- if sink output fails, do not treat the failed sink itself as a recovery channel; use direct stderr unless `--no-error` explicitly disabled reporting
- archive completed plans with `scripts/workflow/archive_completed_plan.sh`
