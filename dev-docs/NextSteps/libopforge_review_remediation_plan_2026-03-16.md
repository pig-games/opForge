# libopforge review remediation plan - 2026-03-16

## Metadata

- Source: `dev-docs/reviews/review_test.md` at commit `b8a3245`
- Mode: remediation
- Owner: GitHub Copilot

## Objective

Close review findings `RVW-2026-03-16-001` through `RVW-2026-03-16-003` without widening scope beyond the reviewed host-facing surfaces. The work must restore the documented FFI panic contract, harden the FFI ABI release-platform check, and narrow the `libopforge` facade so the LSP depends only on a deliberate stable surface.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at all times during plan execution.
- One active work item at a time; no partial parallel advancement across milestones.
- Each work item ends in exactly one new commit before the next item starts.
- No fixture or reference regeneration is allowed unless a behavior change intentionally requires it.
- Full quality gates and `plan-compliance-reviewer` `PASS` evidence are required before each commit.
- Scope is limited to the findings in `review_test.md`; no opportunistic facade redesign or unrelated CI cleanup.

## Work Items

- [x] Item 1: Route the supported FFI shipping path through `release-ffi` only.
  - Source requirement or finding IDs: `RVW-2026-03-16-001` (expected full closure).
  - Expected files: `Makefile`, `README.md`, and any FFI packaging check touched to keep the documented release path honest.
  - Validation: `cargo build -p ffi --profile release-ffi --locked --lib`; `cargo test -p ffi --locked`; `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and executed validation results.
  - Commit outcome: the top-level supported build and release flow no longer treats workspace `--release` as a valid way to ship the shared library, and the intended FFI build path is explicitly the unwind-capable `release-ffi` profile.
  - Definition of done: no documented or automated shipping path for the shared library produces the public FFI artifact via the workspace `panic = "abort"` release profile.

- [x] Item 2: Make the ABI contract test enforce header compilation on every release platform.
  - Source requirement or finding IDs: `RVW-2026-03-16-002` (expected full closure).
  - Expected files: `crates/opforge-ffi/tests/abi_contract.rs`, `.github/workflows/cargo-build-matrix.yml`, and `crates/opforge-ffi/Cargo.toml` only if compiler-path support needs a new test dependency.
  - Validation: `cargo test -p ffi exported_header_matches_rust_abi_contract --locked`; `cargo test -p ffi --locked`; `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and executed validation results.
  - Commit outcome: the ABI contract test resolves a Windows-supported compiler path where needed, fails instead of silently skipping when no supported release-platform compiler path exists, and the release-matrix workflow still runs that test after the `release-ffi` build step.
  - Definition of done: the public header compile check has no silent skip path on Windows-class release runners, and the CI lane that publishes FFI artifacts still executes that contract test explicitly.

- [x] Item 3: Move the LSP to public APIs from the owning crates.
  - Source requirement or finding IDs: `RVW-2026-03-16-003` (expected partial closure).
  - Expected files: `crates/opforge-lsp/src/lib.rs`, `crates/opforge-lsp/src/cpu_context.rs`, and any narrow set of LSP call sites that currently depend on `libopforge` family-module re-exports.
  - Validation: `cargo test --locked -p lsp`; `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and executed validation results.
  - Commit outcome: the LSP stops depending on `libopforge::families::*` and `libopforge::z80::*` and instead imports the needed CPU identifiers or related data from public APIs exposed by the crates that own those concepts.
  - Definition of done: no LSP code path depends on family-module compatibility re-exports from `libopforge`, and the replacement imports come from documented public APIs in the underlying crates rather than a new `libopforge` convenience surface.

- [x] Item 4: Remove the broad compatibility re-exports from `libopforge` once the LSP migration is complete.
  - Source requirement or finding IDs: `RVW-2026-03-16-003` (expected full closure, completing Item 3).
  - Expected files: `crates/opforge-lib/src/lib.rs` and focused tests in `crates/opforge-lib` only if public contract coverage needs adjustment after the re-export removal.
  - Validation: `cargo test --locked -p libopforge`; `cargo test --locked -p lsp`; `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and executed validation results.
  - Commit outcome: the transitional family-module re-exports are deleted from `libopforge`, leaving the facade aligned with its documented `libopforge::registry`-first boundary rather than acting as a compatibility barrel for internal family modules.
  - Definition of done: `libopforge` no longer exports those family module trees publicly, and the branch still passes with the LSP using public APIs from the owning crates.

- [x] Item 5: Run final remediation closure and traceability pass.
  - Source requirement or finding IDs: `RVW-2026-03-16-001`, `RVW-2026-03-16-002`, `RVW-2026-03-16-003` (expected closure confirmation only).
  - Expected files: this plan for checkbox updates and any review-closure artifact required by the active workflow.
  - Validation: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`; rerun `python3 scripts/workflow/check_plan_checkboxes.py dev-docs/NextSteps/libopforge_review_remediation_plan_2026-03-16.md` after updating progress bookkeeping.
  - Full quality gates: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`.
  - Plan-compliance review evidence: final `PASS` from `plan-compliance-reviewer` confirming the last active checkbox, changed files, validation evidence, and progress bookkeeping are all consistent.
  - Commit outcome: all findings have explicit closure evidence, the plan state is current, and the branch is ready for follow-up review or merge.
  - Definition of done: every review finding is traceably mapped to completed work, all required validations are green, and plan bookkeeping reflects the true completion state.

## Milestones

- [x] Milestone 1: FFI release contract restored (`Item 1` complete and committed).
- [x] Milestone 2: ABI release-platform enforcement restored (`Item 2` complete and committed).
- [x] Milestone 3: LSP migrated to owning-crate public APIs and `libopforge` compatibility re-exports removed (`Item 3` and `Item 4` complete and committed).
- [x] Milestone 4: Remediation closure evidence complete (`Item 5` complete and committed).

## Closure Evidence

- `RVW-2026-03-16-001` closed by commit `2182c64` (`Route shipped FFI builds through release-ffi.`).
- `RVW-2026-03-16-002` closed by commit `9050b18` (`Enforce ABI header compilation on release platforms.`).
- `RVW-2026-03-16-003` partial closure landed in commit `d0aa77c` (`Move LSP CPU context to owning crate APIs.`).
- `RVW-2026-03-16-003` full closure completed in commit `10f0fe0` (`Remove libopforge family compatibility reexports.`).
- Final validation for the remediation branch: `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`, `cargo audit`, `cargo test --workspace --locked`, and `python3 scripts/workflow/check_plan_checkboxes.py dev-docs/NextSteps/libopforge_review_remediation_plan_2026-03-16.md`.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping