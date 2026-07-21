<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# Plan: opForge Rust CLI Diagnostic Context Rendering v0.1

## Metadata

- Source: User request on 2026-07-21 for rustc-style CLI source context; `documentation/libopforge-diagnostics-and-fixits-guide.md`; the completed Rust CLI diagnostic-rendering remediation plan.
- Mode: remediation
- Owner: Codex
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.
- Artifact type: Rust CLI diagnostic rendering follow-on plan.
- Workflow: `skills/opforge-plan-authoring/SKILL.md`; `scripts/workflow/run_plan_workflow.sh`.
- Validation status: Pending — requires plan-quality-reviewer PASS before activation.

## Goal

Render source-backed CLI diagnostics with a bounded compiler-style context: a
file/line/column header, surrounding source lines, a precise primary
caret/range, and preserved related-span, note, help, and fixit information.

Use the existing generic `Diagnostic` and source-map-derived fields; do not
discard structured metadata or add CPU/family/dialect behavior to formatting.

## Version Impact

- Affected component(s): `opforge-types` diagnostic text rendering, Rust CLI classic/default presentation, diagnostic tests, and the diagnostics guide.
- Impact class: patch
- Owned contract: source-located tokenizer, parser, semantic, preprocessing, and assembler diagnostics show bounded local context and an accurate line/column marker; infrastructure-only diagnostics do not fabricate source context.
- Rationale: `Diagnostic` already carries file, source, column bounds, related spans, notes, help, fixits, and remapped source locations, but `build_context_lines()` currently renders one source line only.

## Current-State Evidence

- `Diagnostic::format_with_context` already renders headers, primary source, related spans, notes, help, and fixits in `crates/opforge-types/src/diagnostics.rs`.
- `build_context_lines` returns one source line plus an inline caret; it has no context window, separate caret row, or range-width marker.
- `crates/opforge-cli/src/lib.rs` delegates text output to that shared model, while JSON already exposes line, column, related spans, notes, help, and fixits.
- The diagnostics guide defines those structured fields and source-map navigation as the host-facing contract.

## Constraints

- Keep rendering generic and architecture-neutral.
- Default to two preceding and two following lines, clipped at file boundaries; multiple noncontiguous span windows use an explicit omission marker.
- Do not use ANSI color as the sole position indicator; color-disabled output is the canonical test surface. Handle tabs and UTF-8 consistently with the existing source-position contract.
- Preserve source-map remapping, multi-file related spans, existing JSON field meanings, and intentional no-source behavior.
- One active item at a time. Each item ends in exactly one focused commit after full gates and `plan-compliance-reviewer` PASS.

## Milestones and Dependency Graph

```text
M1 Context-window primitive -> M2 related spans/guidance -> M3 executable CLI proofs -> M4 documentation and closure
```

## Work Items

- [x] Item 1: implement the bounded source-context rendering primitive
  - Source requirement or finding IDs: CLI-CONTEXT-001 (single-line context); CLI-CONTEXT-002 (non-actionable line/column marker).
  - Invariant: a source-backed diagnostic emits up to two preceding and two following lines, a distinct caret row, and a visible `column..col_end` range marker. File boundaries, EOF, unavailable source, tabs, Unicode, and invalid columns have deterministic fallback behavior.
  - Expected files: `crates/opforge-types/src/diagnostics.rs`, its focused unit tests, and narrow CLI/public re-exports in `crates/opforge-cli/src/lib.rs`, `crates/opforge-asm/src/error.rs`, and `crates/opforge-lib/src/lib.rs`.
  - Full quality gates: focused `cargo test -p types`; `scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`.
  - Plan-compliance review evidence: PASS — implementation is limited to the shared source-window/layout primitive and narrow public/classic wiring; focused `cargo test -p types`, Rust quality gate, and workflow gate passed.
  - Commit outcome: one shared diagnostic-rendering primitive commit.
  - Definition of done: CLI-CONTEXT-001 and CLI-CONTEXT-002 fully close with deterministic bounded output.

- [x] Item 2: render related spans and preserve structured guidance in text layout
  - Source requirement or finding IDs: CLI-CONTEXT-003 (related-span context is not actionable); CLI-CONTEXT-004 (notes/help/fixits must survive richer text output).
  - Invariant: primary context remains visually dominant; every related span renders its own file/line/column identity and bounded context when available, followed by its label. Notes, help, and fixits preserve order; JSON stays schema-compatible.
  - Expected files: `crates/opforge-types/src/diagnostics.rs` and focused diagnostic tests.
  - Full quality gates: focused `cargo test -p types`; `cargo test -p cli`; `scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`.
  - Plan-compliance review evidence: PASS — text rendering adds file-qualified related-span locations and same-file context only; related spans, notes, help, fixits, and JSON fields retain their existing meaning. Focused `types` and `cli` tests, Rust quality gate, and workflow gate passed.
  - Commit outcome: one related-span/guidance rendering commit.
  - Definition of done: CLI-CONTEXT-003 and CLI-CONTEXT-004 fully close without changing diagnostic meaning or JSON fields.

- [ ] Item 3: prove CLI default and classic context with real source failures
  - Source requirement or finding IDs: CLI-CONTEXT-005 (no executable proof of multi-line source presentation); CLI-DIAG-009 follow-on coverage.
  - Invariant: `opforge` default and `--diagnostics-style classic` output show shared bounded context for representative tokenizer, parser, and semantic failures. JSON remains structured; `--no-error` remains intentionally silent.
  - Expected files: `crates/opforge-cli/tests/diagnostic_contract.rs` and minimal test fixtures only.
  - Full quality gates: `cargo test -p cli --test diagnostic_contract`; `cargo test -p cli`; `cargo test -p types`; `scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`.
  - Plan-compliance review evidence: PASS confirming executable public assertions and no fixture-only product branches.
  - Commit outcome: one executable contract-test/integration commit.
  - Definition of done: CLI-CONTEXT-005 fully closes with source-backed executable evidence for line, column, range, and bounded context.

- [ ] Item 4: document the rendering contract and close the plan
  - Source requirement or finding IDs: CLI-CONTEXT-006 (published guide lacks text-rendering contract); closure evidence for CLI-CONTEXT-001 through CLI-CONTEXT-005.
  - Invariant: the diagnostics guide distinguishes bounded human text context from structured JSON and documents source maps, spans, related spans, notes, help, fixits, and graceful no-source behavior.
  - Expected files: `documentation/libopforge-diagnostics-and-fixits-guide.md` and plan bookkeeping only.
  - Full quality gates: `cargo test -p types`; `cargo test -p cli --test diagnostic_contract`; `scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`; `scripts/workflow/run_plan_workflow.sh`.
  - Plan-compliance review evidence: PASS for documentation/closure scope only.
  - Commit outcome: one documentation and completion-evidence commit, then archive the completed plan.
  - Definition of done: CLI-CONTEXT-006 fully closes; the guide and executable behavior state the same rendering contract.

## Blocking Rules

- no commit before all quality gates pass
- `plan-compliance-reviewer` must return `PASS` before commit
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- do not use color as the only caret/range indicator or test oracle
- do not fabricate source context for infrastructure/workflow failures without a valid source span
- preserve JSON field meanings and source-map-derived locations; an incompatible schema change blocks the item pending explicit user direction
- archive completed plans with `scripts/workflow/archive_completed_plan.sh`
