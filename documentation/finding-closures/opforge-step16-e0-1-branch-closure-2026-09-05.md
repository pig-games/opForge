<!-- workflow-provenance: skill=opforge-review-closure; entrypoint=run_closure_workflow.sh -->

# Finding Closure Report: E0-1 resolved forward branch targets

Active AGENTS.md remains binding. Source: Step16 / A-branch and E0-1 in the
Step09 early-failure ledger. Performance contribution: restored correctness
needed to qualify earlier gains; no new runtime speedup is claimed.

## Finding

- ID: RVW-2026-09-05-001
- Original ledger ID: E0-1 (same finding; formal review registration alias).
- Original summary: Four completed native branch cases reject forward BEQ/BRA in pass two with OPC-NCLI026 after successful pass one; the earliest divergent boundary was unproven.

Formal registration: `documentation/reviews/opforge-step16-branch-finding-registration-2026-09-05.md`.

## Claimed Fix

- Plan item: Step16 / Item A-branch
- Implementation slice or commit: Minimal rel8 resolved-value repair on parent27cc06cc; focused commit pending.
- Changed files: native/motorola68000/amigaos/tkpkg/tkpkg_operand_runtime.asm; crates/opforge-asm/src/tests/native_branch_failures.rs; child-module registration, corrected rel8 source contract in crates/opforge-asm/src/tests.rs and required source/inventory snapshots.

Rust `execution_model/selector_encoding.rs::encode_expr_relative` evaluates the
available target and checks `target-(PC+instruction_length)`. Native's later-pass
instability check instead discarded that value when the defining row had not yet
refreshed. Removing those four instructions preserves the existing pass-one
placeholder, expression errors, signed range checks and output encoding. The
original public label is retained. No instrumentation or CPU-specific semantic
logic is added.

## Validation Evidence

- Command or check: Known-good invocation (the explicitly enabled native confirmation command): OPFORGE_BRANCH_CASE=first-run / all, with OPFORGE_FS_UAE_SMOKE=1, OPFORGE_FS_UAE_BIN=/Applications/FS-UAE.app/Contents/MacOS/fs-uae, OPFORGE_FS_UAE_CONFIG_TEMPLATE=/tmp/opforge-performance.fs-uae, OPFORGE_FS_UAE_ARGS={fsuae_config}, OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=300000 and RUST_TEST_THREADS=1; cargo test --locked -p asm native_recorded_branch_failures_fs_uae -- --nocapture --test-threads=1.
- Result: Single unchanged reproduction returned fresh guest exit1 and the original BEQ OPC-NCLI026. After correction, all four original cases returned fresh guest exit0 and exact in-memory live Rust artifact equality; host confirmation exit0 in730.070850s. The label-only correction produced byte-identical native images. Full non-LSP Rust passed (1,591 assembler tests; all remaining included packages passed), as did the staged native and workflow gates. Independent finding-closure reviewer plan_review passed E0-1 on 2026-09-05.

The first full non-LSP Rust gate terminated with 1,590 passing tests and one
failed source contract that still required the removed instability rejection.
The corrected Level B contract requires evaluation-success handling and rejects
any later-pass instability guard while retaining displacement and range checks.
Both focused contracts pass. The replacement full gate passed with 1,591
assembler tests in 1,819.78s (1,889.188706s whole gate); the original failed
receipt is preserved. Refreshed staged native and workflow
checks pass. These test-only changes do not alter the confirmed native image.

Detailed identities, log hashes, source hashes, proof limits and the initial
public-label snapshot correction are preserved in
`documentation/performance/results/opforge-step16-branch-evidence-2026-09-05.json`
and `opforge-step16-branch-correction-2026-09-05.md`.

The exact confirmed cases are first-run artifact contract, 65c02_simple,
65c02_allmodes and source-cpu-package-aliases, preserving each original source,
command, CPU/package and live Rust oracle. The runner attempts every case even
on failure and requires fresh challenge-bound start/completion, explicit exit
and exact artifact equality before removing all guest case files. Source audit
is not native execution proof; completed confirmations are Level D.

## Closure Status

- Status: fixed
- Residual risk: All required code gates and independent E0-1 closure review pass. B10, the independent aggregate timeout, wrong negative diagnostic, other native failures and all A-close qualification remain open.
- Closure rationale: The specific rejection is source-explained and all four recorded cases now pass, and independent finding-closure review now approves E0-1 as fixed. This report does not close a whole parity group or Phase A.
