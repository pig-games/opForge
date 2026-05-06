# Finding Closure Report

## Finding

- ID: RVW-2026-05-06-003
- Original summary: The native AmigaOS target tree mixed deliverable runtime
  modules with debug, smoke, and sample harness entrypoints as first-class
  sibling components.

## Claimed Fix

- Plan item: Item 4, "Move non-deliverable harnesses into a clearly named native
  test-harness subtree," in
  `documentation/plans/opforge-native-amigaos-deliverable-remediation-plan-2026-05-06.md`.
- Implementation slice or commit: pending Item 4 commit.
- Changed files:
  - `native/motorola68000/amigaos/test-harnesses/tkpkg/tkpkg_debug_cli.asm`
  - `native/motorola68000/amigaos/test-harnesses/prvm/prvm_debug_cli.asm`
  - `native/motorola68000/amigaos/test-harnesses/prvm/prvm_smoke.asm`
  - `native/motorola68000/amigaos/test-harnesses/prvm/prvm_line_iterator_smoke.asm`
  - `native/motorola68000/amigaos/test-harnesses/tokvm/tokvm_test_input.asm`
  - `native/README.md`
  - `crates/opforge-asm/src/tests.rs`
  - `crates/opforge-asm/src/fs_uae_smoke.rs`
- Fix summary: the five non-deliverable debug, smoke, and sample entrypoints
  named in the review were moved out of the production-looking `prvm`, `tkpkg`,
  and `tokvm` runtime module directories into
  `native/motorola68000/amigaos/test-harnesses/`. Host-side assembly tests and
  FS-UAE smoke staging now resolve those harness paths explicitly, while module
  and include roots keep the moved harnesses dependent on the production runtime
  modules and package fixtures.

## Validation Evidence

- Command or check: `cargo fmt --all && cargo test -p asm motorola68020_tkpkg_smoke_debug_cli_example_assembles_native_pipeline_smoke_path -- --nocapture`
- Result: PASS.
- Command or check: `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
- Result: PASS.
- Command or check: `cargo test -p asm motorola68020_prvm_ -- --nocapture`
- Result: PASS.
- Command or check: `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`
- Result: PASS.
- Command or check: `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_hunk_smoke -- --nocapture`
- Result: PASS.
- Command or check: `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_opforge_native_cli_ -- --nocapture`
- Result: PASS.
- Command or check: `scripts/workflow/run_rust_quality_gate.sh`
- Result: PASS. The captured log ends with `PASS: Rust quality gate complete.`

## Closure Status

- Status: fixed
- Residual risk: The native tree still contains planned production remediation
  for output-mode behavior in Item 5, so Milestone 3 is not complete yet. That
  does not affect this finding because the harness and runtime layout boundary
  is now explicit.
- Closure rationale: Production runtime modules remain under
  `opforge-cli`, `opcore`, `prvm`, `tkpkg`, and `tokvm`, while the named debug,
  smoke, and sample entrypoints now live under a clearly non-deliverable
  `test-harnesses` subtree. Documentation, host tests, and FS-UAE staging all
  point at the new locations, and the validation evidence proves the moved
  harnesses still assemble and run through the expected native smoke paths.

## Notes

- `tkpkg_debug_cli.asm` continues to load `tkpkg_debug_cli_package.opasm` from
  the production `tkpkg` fixture directory through an explicit include root; the
  package fixture itself was not moved because it remains part of the runtime
  package fixture surface.