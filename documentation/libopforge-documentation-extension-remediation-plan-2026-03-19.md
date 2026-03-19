# libopforge Documentation Extension Review Remediation Plan 2026-03-19

## Metadata

- Source: `documentation/libopforge-documentation-extension-review-2026-03-19.md`
- Mode: `remediation`
- Owner: GitHub Copilot

## Objective

Implement the documentation and rustdoc changes needed to close `RVW-2026-03-19-001`, `RVW-2026-03-19-002`, and `RVW-2026-03-19-003` without widening scope beyond the review's fix directions, while preserving the branch-local requirement that each work item lands as its own fully validated commit.

## Plan Activation

- This plan does not become active until `plan-quality-reviewer` returns `PASS` against this plan, the active worktree `AGENTS.md`, the source review artifact, and the scoped user request.
- `plan-compliance-reviewer` is an execution-phase gate only. It must be run for each concrete work-item slice with the targeted checkbox, changed files, consumer path, executed validation results, temporary debt state, and updated plan state.
- No review finding covered by this plan may be marked fixed or closed until `finding-closure-reviewer` returns `PASS` on a closure artifact for the claimed finding status.

## Activation Checklist

- [x] Activation 1: `plan-quality-reviewer` returned `PASS` for this plan on 2026-03-19 against the active worktree `AGENTS.md`, the source review artifact, and the scoped user request.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at all times during plan execution.
- Execute only one work item at a time.
- Each work item must end in exactly one new commit before the next item begins.
- Full quality gates are required before each commit.
- `plan-compliance-reviewer` must return `PASS` before each commit.
- `finding-closure-reviewer` must return `PASS` before any covered finding is marked fixed or closed.
- Do not widen scope into README restructuring, FFI onboarding expansion, or generalized documentation-link automation in this remediation pass unless a narrower change proves insufficient to land one of the reviewed fixes.

## Work Items

- [x] Item 1
  - Source requirement or finding IDs: `RVW-2026-03-19-003` (expected closure: full); `RVW-2026-03-19-001` (expected closure: partial)
  - Expected files: `documentation/libopforge-developer-guide.md`; `documentation/libopforge-specification.md`
  - Validation: `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && rg -n "libopforge-api-aesthetics-improvement-plan-v0_1" documentation README.md crates`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo test --locked -p libopforge`
  - Full quality gates: `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo fmt --all`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo clippy --workspace -- -D warnings`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo audit`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo test --locked --workspace`
  - Plan-compliance review evidence: `plan-compliance-reviewer` `PASS` for Item 1 against this plan, the active worktree `AGENTS.md`, the user request, and the staged diff before commit
  - Commit outcome: remove the stale aesthetics-plan references and turn the developer-guide roadmap section into a live, branch-maintained navigation section that names the four companion guides and their decision boundaries
  - Definition of done: no maintained host-facing document still points at `documentation/libopforge-api-aesthetics-improvement-plan-v0_1.md`, the developer guide contains a current roadmap or decision matrix for the companion docs, and the staged diff is limited to the review-directed guide and specification cleanup

- [x] Item 2
  - Source requirement or finding IDs: `RVW-2026-03-19-001` (expected closure: partial)
  - Expected files: `documentation/libopforge-embedding-cookbook.md`; `documentation/libopforge-execution-modes-and-lockstep-guide.md`; `documentation/libopforge-developer-guide.md`; `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs` only if an example anchor or wording update is strictly required
  - Validation: `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo test --locked -p libopforge`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && rg -n "Embedding Cookbook|Execution Modes and Lockstep" documentation/libopforge-developer-guide.md documentation/libopforge-embedding-cookbook.md documentation/libopforge-execution-modes-and-lockstep-guide.md`
  - Full quality gates: `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo fmt --all`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo clippy --workspace -- -D warnings`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo audit`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo test --locked --workspace`
  - Plan-compliance review evidence: `plan-compliance-reviewer` `PASS` for Item 2 against this plan, the active worktree `AGENTS.md`, the user request, and the staged diff before commit
  - Commit outcome: publish the host-integration and runtime-selection companion guides and wire the developer guide to them with stable task-based entry links
  - Definition of done: the documentation set contains a usable Embedding Cookbook and Execution Modes and Lockstep guide, the developer guide routes readers to those documents for task-specific detail, and the new content stays within the review's host-facing documentation scope

- [x] Item 3
  - Source requirement or finding IDs: `RVW-2026-03-19-001` (expected closure: full)
  - Expected files: `documentation/libopforge-cpu-family-extension-guide.md`; `documentation/libopforge-diagnostics-and-fixits-guide.md`; `documentation/libopforge-developer-guide.md`
  - Validation: `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo test --locked -p libopforge`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && rg -n "CPU/Family Extension Guide|Diagnostics and Fixits|Embedding Cookbook|Execution Modes and Lockstep" documentation/libopforge-developer-guide.md documentation/libopforge-*.md`
  - Full quality gates: `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo fmt --all`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo clippy --workspace -- -D warnings`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo audit`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo test --locked --workspace`
  - Plan-compliance review evidence: `plan-compliance-reviewer` `PASS` for Item 3 against this plan, the active worktree `AGENTS.md`, the user request, and the staged diff before commit
  - Commit outcome: publish the extension and diagnostics companion guides, then reduce the main developer guide to an index plus high-level decision matrix that points readers into all four companion documents
  - Definition of done: all four promised companion guides exist under `documentation/`, the developer guide no longer carries the detailed task guidance moved into those companion docs, and `RVW-2026-03-19-001` has closure-ready documentation structure without unrelated manual or README expansion

- [x] Item 4
  - Source requirement or finding IDs: `RVW-2026-03-19-002` (expected closure: partial)
  - Expected files: `crates/opforge-lib/src/lib.rs`; `documentation/libopforge-developer-guide.md` only if a stable example anchor or cross-reference label must be added for the new rustdoc links
  - Validation: `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo doc --locked -p libopforge --no-deps`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo test --locked -p libopforge`
  - Full quality gates: `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo fmt --all`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo clippy --workspace -- -D warnings`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo audit`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo test --locked --workspace`
  - Plan-compliance review evidence: `plan-compliance-reviewer` `PASS` for Item 4 against this plan, the active worktree `AGENTS.md`, the user request, and the staged diff before commit
  - Commit outcome: add a focused rustdoc pass for the `processing::*` public routing helpers and the `AssemblerBuilder` setter surface using short "when to use this" guidance and stable guide-example links
  - Definition of done: the `processing` routing helpers and `AssemblerBuilder` public methods have inline rustdoc that explains selection intent, default behavior, and the relevant guide example path without changing runtime behavior or widening to unrelated public surfaces

- [ ] Item 5
  - Source requirement or finding IDs: `RVW-2026-03-19-002` (expected closure: full)
  - Expected files: `crates/opforge-lib/src/lib.rs`; `documentation/libopforge-developer-guide.md` only if the existing examples need stable section labels to support the final rustdoc links
  - Validation: `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo doc --locked -p libopforge --no-deps`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo test --locked -p libopforge`
  - Full quality gates: `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo fmt --all`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo clippy --workspace -- -D warnings`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo audit`; `cd /Users/erik/Code/Retro/opForge/worktrees/libopforge-lib && cargo test --locked --workspace`
  - Plan-compliance review evidence: `plan-compliance-reviewer` `PASS` for Item 5 against this plan, the active worktree `AGENTS.md`, the user request, and the staged diff before commit
  - Commit outcome: complete the second rustdoc pass for `AssemblerSessionBuilder` and the remaining high-traffic builder and session helpers that define the public happy path
  - Definition of done: `AssemblerSessionBuilder` and the remaining public builder or session helper methods named in the review have inline rustdoc that explains when to use them, what defaults or invariants they rely on, and how they map to the long-form developer-guide examples, leaving `RVW-2026-03-19-002` closure-ready

## Milestones

- [x] Milestone 1: stale documentation references are removed and the developer-guide roadmap is current
- [x] Milestone 2: the Embedding Cookbook and Execution Modes and Lockstep guide are published and linked from the main guide
- [x] Milestone 3: all four companion guides exist and the developer guide has been reduced to index and decision-matrix duties
- [ ] Milestone 4: the processing and builder rustdoc pass is complete and ready for closure review

## Closure Evidence

- After Item 1 lands, record closure evidence for `RVW-2026-03-19-003` before marking that finding fixed.
- After Item 3 lands, record closure evidence for `RVW-2026-03-19-001` before marking that finding fixed.
- After Item 4 lands, `RVW-2026-03-19-002` may be recorded as partial only; do not claim full closure until Item 5 validation and closure review both pass.
- After Item 5 lands, record closure evidence for `RVW-2026-03-19-002` before marking that finding fixed.

## Blocking Rules

- no plan activation before `plan-quality-reviewer` returns `PASS`
- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- no finding may be marked fixed or closed before `finding-closure-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping