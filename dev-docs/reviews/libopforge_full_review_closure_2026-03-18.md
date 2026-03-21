# Finding Closure Report

## Finding

- ID: `RVW-2026-03-18-001`
- Original summary: The public `libopforge::processing::route_module_item_line` helper exposed orchestration and runtime-model failures as `ParseError` instead of the neutral `EngineError` and `ProcessorError` contract promised for the processing boundary.

## Claimed Fix

- Plan item: `Item 1` in `dev-docs/NextSteps/libopforge_full_review_remediation_plan_2026-03-18.md`
- Implementation slice or commit: `d1a94b3` (`Repair module-item processing contract`)
- Changed files: `crates/opforge-engine/src/processing.rs`, `crates/opforge-engine/src/lib.rs`, `crates/opforge-lib/src/lib.rs`

## Validation Evidence

- Command or check: `cargo test --locked -p engine`
- Result: PASS; focused engine tests prove the module-item helper returns neutral processor invalid-request errors for runtime-model and unsupported-return failures.
- Command or check: `cargo test --locked -p libopforge`
- Result: PASS; facade tests prove the default helper now preserves the `EngineError` split and the model-backed module-item helper is published.
- Command or check: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --locked --workspace`
- Result: PASS with the accepted baseline advisory warning `RUSTSEC-2025-0026` for `registry`.

## Closure Status

- Status: fixed
- Residual risk: none beyond the accepted `cargo audit` baseline warning, which does not change this finding's behavior.

## Notes

- The stable facade now keeps processor/orchestration failures out of the parse-error path for named public module-item routing helpers.

---

## Finding

- ID: `RVW-2026-03-18-002`
- Original summary: Core-owned routed failures lost their specific `CoreErrorKind` classification after crossing the processing boundary because engine-side routing flattened them to `ParseError`.

## Claimed Fix

- Plan item: `Item 2` in `dev-docs/NextSteps/libopforge_full_review_remediation_plan_2026-03-18.md`
- Implementation slice or commit: `b2c642c` (`Preserve routed core error kinds`)
- Changed files: `crates/opforge-engine/src/processing.rs`, `crates/opforge-engine/src/lib.rs`, `crates/opforge-lib/src/lib.rs`, `crates/opforge-asm/src/tests.rs`

## Validation Evidence

- Command or check: `cargo test --locked -p engine`
- Result: PASS; focused engine tests prove routed conditional and `.use` failures retain their structured concern kind before leaving the engine boundary.
- Command or check: `cargo test --locked -p libopforge`
- Result: PASS; public facade tests prove routed conditional failures surface `CoreErrorKind::Conditional` and routed invalid `.use` failures surface `CoreErrorKind::Use`.
- Command or check: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --locked --workspace`
- Result: PASS with the accepted baseline advisory warning `RUSTSEC-2025-0026` for `registry`.

## Closure Status

- Status: fixed
- Residual risk: none beyond the accepted `cargo audit` baseline warning, which does not change routed core classification.

## Notes

- The processing boundary now preserves the public concern taxonomy promised by `CoreErrorKind` for routed core failures.

---

## Finding

- ID: `RVW-2026-03-18-003`
- Original summary: The developer guide still documented `AsmRunError` as the top-level high-level Rust assembly failure type even though the shipped facade returns `AssemblerWorkflowError`.

## Claimed Fix

- Plan item: `Item 3` in `dev-docs/NextSteps/libopforge_full_review_remediation_plan_2026-03-18.md`
- Implementation slice or commit: `689db4f` (`Sync developer guide workflow error contract`)
- Changed files: `documentation/libopforge-developer-guide.md`, `crates/opforge-lib/src/lib.rs`

## Validation Evidence

- Command or check: `cargo test --locked -p libopforge`
- Result: PASS; the guide assertion test proves the documentation now names `AssemblerWorkflowError` as the top-level failure contract and describes `AsmRunError` only as the `AssemblerWorkflowError::Assemble` payload.
- Command or check: `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`; `cargo audit`; `cargo test --locked --workspace`
- Result: PASS with the accepted baseline advisory warning `RUSTSEC-2025-0026` for `registry`.

## Closure Status

- Status: fixed
- Residual risk: none beyond the accepted `cargo audit` baseline warning, which does not affect this documentation contract.

## Notes

- The shipped developer guide now matches the stable Rust facade's current high-level workflow error surface.

---

## Workflow Gate Evidence

- Checkbox validator: PASS via `python3 scripts/workflow/check_plan_checkboxes.py dev-docs/NextSteps/libopforge_full_review_remediation_plan_2026-03-18.md`
- Finding Closure Reviewer: PASS for `RVW-2026-03-18-001` through `RVW-2026-03-18-003` at the finding level based on the mapped remediation commits and focused validation evidence
- Plan-compliance-reviewer: earlier Item 4 preflight PASS recorded during closure-bookkeeping validation; final staged-diff recheck remains required before commit
- Final quality gates: PASS via `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`, `cargo audit` with accepted baseline warning `RUSTSEC-2025-0026`, and `cargo test --locked --workspace` on the clean rerun after one flaky LSP timeout