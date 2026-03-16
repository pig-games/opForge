# Closure Report

## Scope

Closure report for review findings `RVW-2026-03-16-001` through
`RVW-2026-03-16-004` from
`dev-docs/reviews/libopforge_full_review_2026-03-16.md` on branch
`feature/libopforge-lib`.

## Implementation Summary

- `RVW-2026-03-16-001` was addressed by replacing the fragile LSP file-URI
  path conversions with platform-correct handling for Unix paths, Windows
  drive-letter paths, and UNC paths, then adding round-trip coverage for the
  affected helper surface.
- `RVW-2026-03-16-002` was addressed by rooting validation overlays and module
  lookup against configured or inferred workspace roots, rebasing relative
  include and module paths before CLI validation, and extending integration
  coverage for sibling-directory edits and workspace-rooted definition lookup.
- `RVW-2026-03-16-003` was addressed by retaining pending validation requests
  when the worker cap is saturated and replaying the newest pending URI after
  worker completion instead of dropping the request permanently.
- `RVW-2026-03-16-004` was addressed by making source-graph bootstrap scanning
  conditional-aware and by limiting root `.use` discovery to the selected root
  module block rather than the full expanded line list.

## Finding Closure Map

### RVW-2026-03-16-001

- Status: fixed
- Implementation slice: uncommitted working-tree remediation on `2026-03-16`
- Files:
  - `crates/opforge-lsp/src/session.rs`
  - `crates/opforge-lsp/tests/lsp_client_integration.rs`
- Closure evidence: `uri_to_path()` and `path_to_file_uri()` now preserve
  Windows drive-letter paths and UNC authorities, and focused tests
  `windows_drive_file_uri_roundtrip_smoke` and `unc_file_uri_roundtrip_smoke`
  prove the original failure mode no longer reproduces at the helper layer.

### RVW-2026-03-16-002

- Status: fixed
- Implementation slice: uncommitted working-tree remediation on `2026-03-16`
- Files:
  - `crates/opforge-lsp/src/session.rs`
  - `crates/opforge-lsp/src/validation_runner.rs`
  - `crates/opforge-lsp/src/workspace_index.rs`
  - `crates/opforge-lsp/tests/lsp_client_integration.rs`
- Closure evidence: overlays now choose a workspace-scoped root, relative
  `include_paths` and `module_paths` are rebased before validation, and the
  integration tests
  `overlay_uses_workspace_root_and_rebased_module_paths_for_sibling_files` and
  `definition_resolves_module_target_via_workspace_rooted_relative_module_path`
  prove sibling unsaved edits and relative module roots resolve through the
  intended workspace scope.

### RVW-2026-03-16-003

- Status: fixed
- Implementation slice: uncommitted working-tree remediation on `2026-03-16`
- Files:
  - `crates/opforge-lsp/src/session.rs`
  - `crates/opforge-lsp/tests/lsp_client_integration.rs`
- Closure evidence: pending validation URIs are now retained when
  `MAX_CONCURRENT_VALIDATIONS` is hit and are automatically rescheduled after
  worker completion, and the integration test
  `validation_backpressure_replays_latest_request_after_capacity_returns`
  proves the latest version is eventually validated instead of being dropped.

### RVW-2026-03-16-004

- Status: fixed
- Implementation slice: uncommitted working-tree remediation on `2026-03-16`
- Files:
  - `crates/opforge-engine/src/source_graph.rs`
  - `crates/opforge-engine/src/source_graph_tests.rs`
  - `crates/opforge-engine/src/lib.rs`
- Closure evidence: bootstrap scanning now tracks active conditional state and
  scopes root-module dependency discovery to the selected root module, and the
  focused tests
  `root_module_id_ignores_inactive_conditional_modules`,
  `load_module_graph_ignores_use_directives_in_inactive_conditionals`, and
  `load_module_graph_scans_root_uses_only_from_selected_root_module` prove the
  reviewed dead-branch and wrong-root failure modes no longer drive graph
  loading.

## Validation Evidence

- `cargo fmt --all --check` PASS
- `cargo clippy -- -D warnings` PASS
- `cargo audit` PASS with the existing allowed `RUSTSEC-2025-0026` warning for `registry`
- `cargo test --locked -p lsp` PASS
- `cargo test --locked -p engine` PASS
- `cargo test --locked` PASS
- `python3 scripts/workflow/check_plan_checkboxes.py dev-docs/NextSteps/libopforge_full_review_remediation_plan_2026-03-16.md` PASS

## Closure Gates

- `Finding Closure Reviewer`: PASS
- `Plan Compliance Reviewer`: PASS

## Residual Risk

- The targeted Windows and UNC regressions are now covered by focused URI and
  integration tests, but this closure pass still did not run the LSP inside a
  live Windows editor host.
- The active plan's commit-per-item workflow has not yet been satisfied because
  the remediation work is still uncommitted in the current worktree. That is a
  workflow-compliance gap, not a reopened product-code defect in findings
  `RVW-2026-03-16-001` through `RVW-2026-03-16-004`.

## Notes

- `Finding Closure Reviewer` result: `PASS: The claimed fixes match the
  working-tree code and tests and the listed quality gates completed
  successfully, so the closure report accurately reflects the original
  findings' resolution.`
- `Plan Compliance Reviewer` result: `PASS: The current diff is scoped to
  Items 1-4 plus the closure-report portion of Item 5, the closure report lists
  the fixes and evidence per finding, and the validation suite plus
  plan-checkbox check all passed, so the slice is ready for the Item 5 commit.`
