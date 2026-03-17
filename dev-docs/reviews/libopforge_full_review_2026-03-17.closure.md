# Finding Closure Report

## Source Review

- Review: `dev-docs/reviews/libopforge_full_review_2026-03-17.md`
- Findings in scope: `RVW-2026-03-17-004`, `RVW-2026-03-17-005`
- Closure date: `2026-03-17`

## Finding Status

### RVW-2026-03-17-004

- Status: Closed
- Implemented by:
  - `06171f0` Pin manual release uploads to requested tags.
  - `4a4025f` Derive release packaging tags from the checkout.
- Closure rationale: upload-mode `workflow_dispatch` runs now resolve and check out `refs/tags/<tag>` before any build step, and the workflow reuses the resolved checkout-derived tag for package naming, archive naming, layout verification, and `softprops/action-gh-release`. The build source, packaged artifact names, and uploaded release tag are now derived from the same resolved tag identity.
- Validation evidence:
  - Workflow-semantic inspection via `rg -n "resolve_checkout_ref|resolve_release_tag|steps\.resolve_checkout_ref\.outputs\.checkout_ref|steps\.resolve_release_tag\.outputs\.tag_name|refs/tags/" .github/workflows/release-binaries.yml` confirmed:
    - upload-mode manual runs set `checkout_ref="refs/tags/${INPUT_TAG_NAME}"`
    - checkout uses `${{ steps.resolve_checkout_ref.outputs.checkout_ref }}`
    - release tag resolution derives `tag_name` from `CHECKOUT_REF#refs/tags/`
    - packaging, archive naming, and `action-gh-release` all consume `${{ steps.resolve_release_tag.outputs.tag_name }}`
  - `cargo test --locked -p ffi release_profile_loads_and_assembles_smoke`
  - `cargo fmt --all --check`
  - `cargo clippy --workspace -- -D warnings`
  - `cargo audit`
  - `cargo test --workspace --locked`

### RVW-2026-03-17-005

- Status: Closed
- Implemented by:
  - `4a5618c` Rebase validator roots from the source workspace.
  - `6a768f0` Stage validation overlay include dependencies.
- Closure rationale: validator config roots are now interpreted from the original workspace root, while in-workspace targets continue to map back into the overlay and external relative roots stay rooted on disk. Overlay construction now stages the active file, reachable `.use` module files, and recursively discovered ordinary include files without widening to unrelated workspace files, so validation sees the same dependency set as normal assembly.
- Validation evidence:
  - `cargo test --locked -p lsp`
  - `cargo fmt --all --check`
  - `cargo clippy --workspace -- -D warnings`
  - `cargo audit`
  - `cargo test --workspace --locked`

## Final Validation

- `python3 scripts/workflow/check_plan_checkboxes.py dev-docs/NextSteps/libopforge_full_review_remediation_plan_2026-03-17.md`
- `cargo fmt --all --check`
- `cargo clippy --workspace -- -D warnings`
- `cargo audit`
- `cargo test --workspace --locked`

## Residual Notes

- No additional findings were reopened while executing this remediation plan.
- The release workflow and LSP overlay fixes were landed as four slice-scoped commits to preserve traceability from finding to implementation.