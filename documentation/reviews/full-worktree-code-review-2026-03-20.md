# Review Report

## Scope

Full worktree review of the current `libopforge-lib` checkout, non-differential, with emphasis on maintainability, non-DRY code, and places where verbose wrapper code can be significantly simplified without changing behavior.

## Findings

### RVW-2026-03-20-001

- Severity: medium
- File: `crates/opforge-ffi/src/lib.rs:2303`, `crates/opforge-ffi/src/lib.rs:3077`, `crates/opforge-ffi/src/lib.rs:3300`
- Issue: The FFI layer is dominated by hand-written boilerplate entrypoints and accessors. `crates/opforge-ffi/src/lib.rs` is 8,218 lines long and exposes 183 `extern "C"` functions, with many near-identical wrappers for null checks, index checks, field reads, and report destruction.
- Why it matters: This is a public ABI surface, so every new field or behavior change has to be replicated consistently across many small functions. That raises drift risk, makes review harder, and turns routine surface evolution into copy-paste maintenance work where inconsistencies are easy to miss.
- Fix direction (one direction only; resolve competing options before finalizing): Introduce a small internal generation layer for the FFI surface, such as declarative macros or generic helper builders, so report accessors and paired assemble/check entrypoints are derived from shared descriptors instead of hand-expanded per function.

### RVW-2026-03-20-002

- Severity: medium
- File: `crates/opforge-lib/src/lib.rs:1900`, `crates/opforge-lib/src/lib.rs:2194`
- Issue: `AssemblerBuilder<'a>` and `AssemblerSessionBuilder` reimplement almost the same fluent API, documentation, and behavior twice, differing mostly in ownership shape (`&str`/`&Path` versus `String`/`PathBuf`/`Arc`).
- Why it matters: This is host-facing API surface. Any new option, validation rule, default, or documentation tweak now has to be made twice, which makes API drift between borrowed and owned entrypoints much more likely and keeps the public facade file much larger than it needs to be.
- Fix direction (one direction only; resolve competing options before finalizing): Extract the shared builder behavior behind one internal config-mutating layer, then keep the borrowed and owned builders as thin type-specific frontends that delegate to that shared implementation.

### RVW-2026-03-20-003

- Severity: low
- File: `crates/opforge-lib/src/lib.rs:568`
- Issue: The workflow error section duplicates the same `code`/`summary` storage and `new`/`code`/`summary`/`Display`/`Error` implementations across `InvalidArgumentError`, `InvalidRequestError`, `HostIoError`, and `InternalErrorReport`.
- Why it matters: The duplication adds a lot of surface area without adding meaning. If these wrappers ever need another common field or trait implementation, four nearly identical types must be kept in lockstep, which is pure maintenance overhead inside an already very large facade module.
- Fix direction (one direction only; resolve competing options before finalizing): Replace the repeated wrappers with one shared private detail type plus thin newtype aliases or a small macro that generates the identical impl blocks from a single definition.

### RVW-2026-03-20-004

- Severity: low
- File: `crates/opforge-cli-core/src/cli.rs:60`
- Issue: The build-profile banner strings are maintained in three parallel `cfg` ladders for `BUILD_PROFILE_SUMMARY`, `LONG_VERSION`, and `HELP_BUILD_PROFILE`, each repeating the same profile text.
- Why it matters: This is a straightforward drift trap. Changing a profile label or adding a new profile combination requires touching three separate constant families, and the duplication makes the top of the CLI module much noisier than the underlying behavior warrants.
- Fix direction (one direction only; resolve competing options before finalizing): Define one canonical `BUILD_PROFILE_SUMMARY` per `cfg` case and derive the longer strings from it so the profile matrix is authored once.

### RVW-2026-03-20-005

- Severity: low
- File: `crates/opforge-lsp/src/session.rs:212`, `crates/opforge-lsp/src/session.rs:233`
- Issue: `handle_did_open` and `handle_did_change` repeat the same document-state construction, derived-state refresh, workspace-index refresh, and validation dispatch flow, with only the text source and validation mode differing.
- Why it matters: Notification-path duplication tends to drift over time. Any future change to how documents are normalized, indexed, or scheduled for validation has to be remembered in both handlers, which is an avoidable maintenance hazard in a central LSP session type.
- Fix direction (one direction only; resolve competing options before finalizing): Extract a shared helper that upserts a document from `(uri, version, text, force_validate)` and let each notification handler only perform its own minimal parameter decoding.

## Testing Gaps

- There is good behavioral test coverage overall (`cargo test -q` passed), but there is little structural protection against wrapper-surface duplication itself. The highest-maintenance areas are public facade/FFI code where consistency currently depends on manual edits rather than shared generation.
- `cargo clippy -q --workspace --all-targets` was nearly clean and surfaced only one small test-only simplification in `crates/opforge-lsp/tests/lsp_client_integration.rs:1074`, so the main risks here are maintainability and drift, not obvious lint-level defects.

## Residual Risks

- This review sampled the full worktree with emphasis on production Rust crates and the highest-surface-area modules, but it did not do a literal line-by-line audit of all 120k+ Rust LOC, example fixtures, or generated-like opcode/data tables.
- Large data-heavy files in CPU tables and examples were intentionally deprioritized because their size appears to come mainly from domain tables and fixtures rather than avoidable wrapper logic.

## Brief Summary

The codebase is behaviorally healthy and well tested, but the public wrapper layers are carrying a lot of manual repetition. The clearest simplification wins are in the FFI surface, the duplicated borrowed/owned libopforge builders, the repeated workflow error wrappers, the CLI build-profile constants, and the duplicated LSP document-upsert flow.
