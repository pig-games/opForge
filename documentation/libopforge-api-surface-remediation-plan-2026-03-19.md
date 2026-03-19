# libopforge API Surface Review Remediation Plan 2026-03-19

## Metadata

- Source: `documentation/libopforge-api-surface-review-2026-03-19.md`
- Mode: `remediation`
- Owner: GitHub Copilot

## Objective

Implement the remediation changes needed to support closure of `RVW-2026-03-19-001` and `RVW-2026-03-19-002` without widening scope beyond the review's decided fix directions, while preserving the branch-local workflow requirement that each remediation item lands as its own validated commit.

## Plan Activation

- This plan does not become active until `plan-quality-reviewer` returns `PASS` against this plan, the active worktree `AGENTS.md`, the source review artifact, and the scoped user request.
- `plan-compliance-reviewer` is an execution-phase gate only. It must be run against one concrete work-item slice with the targeted checkbox, changed files, relevant consumer path, executed validation results, temporary debt state, and updated plan/progress state.
- No review finding covered by this plan may be marked fixed or closed until `finding-closure-reviewer` returns `PASS` on a closure artifact for the claimed finding status.

## Activation Checklist

- [x] Activation 1: `plan-quality-reviewer` returned `PASS` for this tracked plan on 2026-03-19 against the active worktree `AGENTS.md`, the source review artifact, and the scoped user request.
- [x] Activation 2: this plan-correction slice landed as its own bookkeeping commit before Item 1 began.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at all times during plan execution.
- Do not treat this plan artifact by itself as sufficient evidence for `plan-compliance-reviewer`; that gate applies per execution slice after a concrete work item produces code or documentation changes and validation evidence.
- Execute only one work item at a time.
- Each work item must end in exactly one new commit before the next item begins.
- Full quality gates are required before each commit.
- `plan-quality-reviewer` must return `PASS` before this plan becomes active.
- `plan-compliance-reviewer` must return `PASS` before each commit.
- `finding-closure-reviewer` must return `PASS` before either review finding is marked fixed or closed.
- Do not widen scope to add a downstream integration-test suite or README compile-backed validation in this remediation pass; those are testing-gap follow-ups, not part of the two review findings.

## Work Items

- [ ] Item 1
  - Source requirement or finding IDs: `RVW-2026-03-19-001` (expected closure: full)
  - Expected files: `crates/opforge-lib/src/lib.rs`; `.github/workflows/cargo-build-matrix.yml`
  - Validation: `cargo test --locked -p libopforge --features vm-runtime-only`; `cargo test --locked -p libopforge`
  - Full quality gates: `cargo test --locked -p libopforge --features vm-runtime-only`; `cargo test --locked -p libopforge`; `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --locked --workspace`
  - Plan-compliance review evidence: `plan-compliance-reviewer` `PASS` for Item 1 against this plan, the active worktree `AGENTS.md`, the user request, and the staged diff before commit
  - Commit outcome: repair the `vm-runtime-only` facade test path to use the facade-visible runtime-model construction and add an always-on `libopforge` `vm-runtime-only` test lane in the existing cargo build matrix
  - Definition of done: the failing processing test no longer depends on `::engine::editor_default_runtime_model()`, `cargo test --locked -p libopforge --features vm-runtime-only` passes locally, the CI matrix exercises that lane explicitly, and the staged diff remains limited to the review-directed public-crate validation fix

- [ ] Item 2
  - Source requirement or finding IDs: `RVW-2026-03-19-002` (expected closure: partial)
  - Expected files: `crates/opforge-lib/src/lib.rs`; `documentation/libopforge-developer-guide.md` only if a missing anchor or stable section link is strictly required to support the new rustdoc cross-references
  - Validation: `cargo doc --locked -p libopforge --no-deps`; `cargo test --locked -p libopforge`
  - Full quality gates: `cargo doc --locked -p libopforge --no-deps`; `cargo test --locked -p libopforge`; `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --locked --workspace`
  - Plan-compliance review evidence: `plan-compliance-reviewer` `PASS` for Item 2 against this plan, the active worktree `AGENTS.md`, the user request, and the staged diff before commit
  - Commit outcome: add consistent rustdoc to the grouped and option-centric public facade types so docs.rs and IDE hover explain when to use owned versus borrowed configuration paths and what default behavior each option set carries
  - Definition of done: `OwnedSourceOptions`, `OwnedExecutionOptions`, `OwnedOutputOptions`, `OwnedAssemblerConfig`, `PrepareOptions`, `AssembleOptions`, `SourceOptions`, `ExecutionOptions`, `OutputOptions`, `DiagnosticsOptions`, and `AssemblerConfig` all have inline rustdoc that covers intended use, default behavior, and guide cross-reference points without changing runtime behavior

- [ ] Item 3
  - Source requirement or finding IDs: `RVW-2026-03-19-002` (expected closure: full)
  - Expected files: `crates/opforge-lib/src/lib.rs`
  - Validation: `cargo doc --locked -p libopforge --no-deps`; `cargo test --locked -p libopforge`
  - Full quality gates: `cargo doc --locked -p libopforge --no-deps`; `cargo test --locked -p libopforge`; `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --locked --workspace`
  - Plan-compliance review evidence: `plan-compliance-reviewer` `PASS` for Item 3 against this plan, the active worktree `AGENTS.md`, the user request, and the staged diff before commit
  - Commit outcome: document the high-traffic builder, session, assembler, and prepared-assembly entrypoints and methods so the facade's main ergonomic path is understandable directly from the public API surface
  - Definition of done: `AssemblerBuilder`, `AssemblerSessionBuilder`, `Assembler`, `AssemblerSession`, `PreparedAssembly`, and `PreparedAssemblySession`, plus the main `new`, `builder`, `with_config`, `build`, `prepare`, `assemble`, and `check` methods that define the public happy path, have inline rustdoc that explains when to use each path, what `check()` suppresses, and how output-base derivation and prepared-session reuse behave from the stable facade contract

## Milestones

- [ ] Milestone 1: `RVW-2026-03-19-001` is fully remediated and the `vm-runtime-only` public-crate lane is green locally and in CI configuration
- [ ] Milestone 2: the option and grouped-config portion of `RVW-2026-03-19-002` is documented and landed as its own commit
- [ ] Milestone 3: the builder and prepared-session portion of `RVW-2026-03-19-002` is documented and the finding is ready for closure evidence

## Closure Evidence

- After Item 1 lands, record closure evidence for `RVW-2026-03-19-001` with a finding-closure artifact before marking the finding fixed.
- After Item 3 lands, record closure evidence for `RVW-2026-03-19-002` with a finding-closure artifact before marking the finding fully fixed.
- If Item 2 lands before Item 3, record `RVW-2026-03-19-002` as partial only; do not claim full closure until Item 3 validation and closure review both pass.

## Blocking Rules

- no plan activation before `plan-quality-reviewer` returns `PASS`
- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- no finding may be marked fixed or closed before `finding-closure-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping