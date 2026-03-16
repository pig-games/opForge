# Closure Report

## Scope

Closure report for review findings `RVW-2026-03-16-001` through `RVW-2026-03-16-003` from `dev-docs/reviews/review_test.md` on branch `feature/libopforge-lib`.

## Implementation Summary

- `RVW-2026-03-16-001` was addressed by routing the supported shipped FFI path through `release-ffi` in the top-level build/package flow and by documenting that host integrations must not ship the workspace `--release` FFI artifact.
- `RVW-2026-03-16-002` was addressed by making the ABI contract test resolve the platform C compiler through the `cc` toolchain path, including MSVC-class environments, and by removing the silent skip behavior when no supported compiler is available.
- `RVW-2026-03-16-003` was addressed in two steps: the LSP was first moved to the owning `families` crate public APIs for CPU identifiers, then the broad `libopforge` family-module compatibility re-exports were removed.

## Finding Closure Map

### RVW-2026-03-16-001

- Status: closed
- Commit: `2182c64` (`Route shipped FFI builds through release-ffi.`)
- Files:
  - `Makefile`
  - `README.md`
  - `documentation/libopforge-developer-guide.md`
- Closure evidence: the supported Makefile/package flow now builds the shared library with `cargo build -p ffi --profile release-ffi --locked --lib`, `test-ffi-packaging` checks `target/release-ffi`, and the host-facing docs explicitly reject shipping the workspace `target/release` FFI artifact.

### RVW-2026-03-16-002

- Status: closed
- Commit: `9050b18` (`Enforce ABI header compilation on release platforms.`)
- Files:
  - `Cargo.lock`
  - `crates/opforge-ffi/Cargo.toml`
  - `crates/opforge-ffi/tests/abi_contract.rs`
- Closure evidence: the ABI contract test now uses `cc::Build::try_get_compiler()` with explicit host/target configuration, emits MSVC-compatible compile flags when needed, and fails immediately instead of skipping when no supported compiler is available.

### RVW-2026-03-16-003

- Status: closed
- Commits:
  - `d0aa77c` (`Move LSP CPU context to owning crate APIs.`)
  - `10f0fe0` (`Remove libopforge family compatibility reexports.`)
- Files:
  - `Cargo.lock`
  - `crates/opforge-lsp/Cargo.toml`
  - `crates/opforge-lsp/src/lib.rs`
  - `crates/opforge-lsp/src/cpu_context.rs`
  - `crates/opforge-lib/src/lib.rs`
- Closure evidence: the LSP no longer consumes `libopforge::families::*` or `libopforge::z80::*`, instead taking CPU identifiers from the owning `families` crate public APIs, and the `libopforge` compatibility re-export trees have been deleted.

## Validation Evidence

- `cargo fmt --all` PASS
- `cargo clippy --workspace -- -D warnings` PASS
- `cargo audit` PASS
- `cargo test --workspace --locked` PASS
- `python3 scripts/workflow/check_plan_checkboxes.py dev-docs/NextSteps/libopforge_review_remediation_plan_2026-03-16.md` PASS

## Closure Gates

- `Finding Closure Reviewer` PASS
- `Artifact Traceability Reviewer` PASS

## Residual Risk

- No additional open findings remain from `review_test.md`.
- The review assumptions about live Windows and macOS GitHub-hosted runner environments were not re-executed here, but the branch now enforces the intended contract through the shipped test logic and release workflows.