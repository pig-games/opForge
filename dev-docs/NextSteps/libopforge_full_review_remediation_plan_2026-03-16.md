# libopforge full review remediation plan - 2026-03-16

## Metadata

- Source: `dev-docs/reviews/libopforge_full_review_2026-03-16.md` at commit `cba00ee`
- Mode: remediation
- Owner: Codex

## Objective

Close review finding `RVW-2026-03-16-004` by ensuring the shipped `release-ffi`
shared library is dynamically loaded and exercised on every release OS before
artifacts are packaged or published. Scope is limited to the remaining
cross-platform FFI release-validation gap from the latest full review.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- One active work item at a time; no overlapping implementation across items.
- Each work item ends in exactly one new commit before the next item starts.
- Scope is limited to `RVW-2026-03-16-004`; previously closed findings
  `RVW-2026-03-16-001` through `RVW-2026-03-16-003` must not be reopened except
  for narrowly required regression prevention.
- Keep the first implementation slice host-path focused: reuse the existing
  `release-ffi` smoke coverage where possible instead of redesigning the FFI
  test harness.
- Full quality gates and `plan-compliance-reviewer` `PASS` evidence are
  required before each commit.

## Work Items

- [ ] Item 1: Make the release-ffi smoke test portable across release platforms.
  - Source requirement or finding IDs: `RVW-2026-03-16-004` (expected partial closure).
  - Expected files: `crates/opforge-ffi/tests/release_panic_boundary.rs` and `crates/opforge-ffi/Cargo.toml` only if the existing smoke test needs narrowly scoped support changes for macOS or Windows loading.
  - Validation: local pre-commit validation with `cargo test --locked -p ffi release_profile_catches_forced_ffi_panic` and `cargo test --locked -p ffi`; post-push CI evidence from the updated GitHub Actions job definition or run logs showing the smoke test is configured to execute on Ubuntu, macOS, and Windows runners
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results for the portable smoke-test slice.
  - Commit outcome: the existing release-profile dynamic-load and panic-boundary smoke test can run unchanged on Linux, macOS, and Windows release runners, with any platform-specific library naming or child-process assumptions handled inside the current focused test path.
  - Definition of done: the FFI smoke test no longer depends on Linux-only assumptions, local focused validation is green, and CI configuration or run evidence shows the same focused test path is ready to execute on Ubuntu, macOS, and Windows without adding a second parallel test path.

- [ ] Item 2: Gate every release OS on the release-ffi load-and-call smoke test.
  - Source requirement or finding IDs: `RVW-2026-03-16-004` (expected full closure).
  - Expected files: `.github/workflows/cargo-build-matrix.yml`, `.github/workflows/release-binaries.yml`, and `crates/opforge-ffi/tests/release_panic_boundary.rs` only if the workflow promotion needs a narrow invocation rename or filter update; `crates/opforge-lsp/tests/lsp_client_integration.rs` only if a narrowly scoped existing CI flake must be stabilized so the required Item 2 quality-gate and cross-platform evidence can be collected without reopening unrelated product scope.
  - Validation: local pre-commit validation with `cargo test --locked -p ffi release_profile_catches_forced_ffi_panic`, `cargo test --locked -p ffi exported_header_matches_rust_abi_contract`, `cargo test --locked -p ffi`, and `cargo test --locked -p lsp --test lsp_client_integration overlapping_validations_publish_only_newest_version_results`; required post-push CI evidence from GitHub Actions showing the release-ffi dynamic-load smoke test passed on Ubuntu, macOS, and Windows and that the release-packaging workflow is blocked on those per-platform results before artifact upload
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`
  - Plan-compliance review evidence: `PASS` from `plan-compliance-reviewer` against `AGENTS.md`, this plan, the active checkbox, changed files, and the executed validation results proving the promoted smoke test is the required gate for each release OS.
  - Commit outcome: the OS build matrix runs the `release-ffi` dynamic-load smoke test on Ubuntu, macOS, and Windows, and the release-packaging workflow depends on those per-platform smoke results before uploading artifacts.
  - Definition of done: local focused validation is green, GitHub Actions evidence shows the built shared library was loaded and exercised successfully on Ubuntu, macOS, and Windows, and no release artifact can be packaged or published unless those same-OS smoke-test jobs pass first.

- [ ] Item 3: Capture closure evidence and finish remediation bookkeeping.
  - Source requirement or finding IDs: `RVW-2026-03-16-004` (expected closure confirmation only).
  - Expected files: this plan for checkbox updates and a finding-closure report artifact for `RVW-2026-03-16-004` required by the active workflow.
  - Validation: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`; `python3 scripts/workflow/check_plan_checkboxes.py dev-docs/NextSteps/libopforge_full_review_remediation_plan_2026-03-16.md`; `finding-closure-reviewer` `PASS` for the `RVW-2026-03-16-004` closure artifact
  - Full quality gates: `cargo fmt --all --check`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --workspace --locked`
  - Plan-compliance review evidence: final `PASS` from `plan-compliance-reviewer` confirming the active checkbox, validation evidence, changed files, and progress bookkeeping are all consistent with the completed remediation state.
  - Commit outcome: the remaining review finding has explicit closure traceability in a completed closure artifact, `finding-closure-reviewer` has returned `PASS`, and plan progress is current.
  - Definition of done: `RVW-2026-03-16-004` is traceably mapped to completed work, the required validations and `finding-closure-reviewer` gate are green, and plan bookkeeping matches the actual completed remediation state.

## Milestones

- [ ] Milestone 1: Portable release-ffi smoke coverage is ready for cross-platform CI promotion (`Item 1` complete and committed).
- [ ] Milestone 2: Every release OS is gated on load-and-call smoke coverage before packaging (`Item 2` complete and committed).
- [ ] Milestone 3: Closure evidence and remediation bookkeeping are complete (`Item 3` complete and committed).

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- no finding may be marked fixed before `finding-closure-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
