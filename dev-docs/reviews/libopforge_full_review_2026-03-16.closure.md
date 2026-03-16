# Finding Closure Report

## Finding

- ID: `RVW-2026-03-16-004`
- Original summary: Published macOS and Windows `release-ffi` artifacts were
  built and packaged without any equivalent shared-library load-and-call smoke
  test outside the Linux-only quality gate, so platform-specific loader or
  exported-symbol regressions could ship while CI stayed green.

## Claimed Fix

- Plan item: `Item 3` of `dev-docs/NextSteps/libopforge_full_review_remediation_plan_2026-03-16.md`, closing the implementation from `Item 1` and `Item 2`
- Implementation slice or commit: `78cc090`, `46a23d3`, `006d944`, `a997a7c`,
  `0c374d3`, `d9009b0`
- Changed files:
  - `.github/workflows/cargo-build-matrix.yml`
  - `.github/workflows/release-binaries.yml`
  - `crates/opforge-ffi/tests/release_panic_boundary.rs`
  - `crates/opforge-lsp/tests/lsp_client_integration.rs`

## Validation Evidence

- Command or check: `cargo fmt --all --check`
- Result: PASS on commit `d9009b0`
- Command or check: `cargo clippy --workspace -- -D warnings`
- Result: PASS on commit `d9009b0`
- Command or check: `cargo audit`
- Result: PASS on commit `d9009b0` with the existing allowed `RUSTSEC-2025-0026` warning for `registry`
- Command or check: `cargo test --workspace --locked`
- Result: PASS on commit `d9009b0`
- Command or check: `cargo test --locked -p ffi release_profile_catches_forced_ffi_panic`
- Result: PASS on commit `d9009b0`
- Command or check: `cargo test --locked -p ffi exported_header_matches_rust_abi_contract`
- Result: PASS on commit `d9009b0`
- Command or check: `cargo test --locked -p ffi`
- Result: PASS on commit `d9009b0`
- Command or check: `cargo test --locked -p lsp --test lsp_client_integration overlapping_validations_publish_only_newest_version_results`
- Result: PASS on commit `d9009b0`
- Command or check: GitHub Actions `Quality Gate` run `23165250323`
- Result: PASS on commit `d9009b0`
- Command or check: GitHub Actions `Cargo Build Matrix` run `23165250263`
- Result: `Run release-ffi panic boundary smoke test` and `Run FFI ABI contract test` passed on Ubuntu, macOS, and Windows for commit `d9009b0`
- Command or check: GitHub Actions `Release Binaries` verify-only run `23165258036`
- Result: `Run shipped release-ffi smoke test`, `Package release artifacts`, `Verify release package layout`, and `Archive release artifacts` passed on Ubuntu, macOS, and Windows for commit `d9009b0`; `Upload assets to release` remained downstream and skipped after a later checksum failure

## Closure Status

- Status: fixed
- Residual risk: The verify-only release workflow still fails later at
  `Generate checksums` on all three OS jobs in run `23165258036`. That is a
  separate release-packaging defect, but it does not reopen
  `RVW-2026-03-16-004` because the original missing same-OS shared-library
  smoke coverage is now present and passing before artifact upload.

## Notes

- Manual `finding-closure-reviewer` readback against `AGENTS.md`, the original
  finding, this closure report, the implementation slice, and the validation
  evidence: `PASS`. The finding ID matches exactly, the implementation touches
  the relevant release-validation path, and the evidence now directly covers the
  original failure mode on Ubuntu, macOS, and Windows.
