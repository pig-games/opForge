# Finding Closure Report

## Finding

- ID: RVW-2026-05-06-002
- Original summary: The native CLI hard-coded instruction acceptance,
  addressing-mode selection, operand payload construction, and PC advancement
  for a small mnemonic set before calling the package-backed encoder service,
  making the CLI a second selector implementation.

## Claimed Fix

- Plan item: Item 3, "Cut the CLI over to the native runtime stage and delete
  CLI-owned selector logic," in
  `documentation/plans/opforge-native-amigaos-deliverable-remediation-plan-2026-05-06.md`.
- Implementation slice or commit: pending Item 3 commit.
- Changed files:
  - `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`
  - `native/motorola68000/amigaos/opasm/opasm_selector_stage.asm`
  - `crates/opforge-asm/src/tests.rs`
  - `crates/opforge-asm/src/fs_uae_smoke.rs`
- Fix summary: the CLI now stages parsed mnemonic and operand text plus label
  metadata into a selector-stage context and calls
  `opasm_selector_stage_build_encode_request_v1`. The CLI-side label
  pre-resolution helpers were removed. The opasm runtime stage now owns operand
  value resolution through `opcore_expr_eval_operand_v1`, encode-request payload
  construction, and instruction-size decisions for the supported native subset.
  The CLI keeps only orchestration duties and forwards the resulting request to
  the tkpkg package-backed encoder service.

## Validation Evidence

- Command or check: `cargo test -p asm motorola68020_opasm_selector_stage_module_owns_native_subset_policy -- --nocapture`
- Result: PASS.
- Command or check: `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`
- Result: PASS.
- Command or check: `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
- Result: PASS.
- Command or check: `cargo test -p asm motorola68020_prvm_ -- --nocapture`
- Result: PASS.
- Command or check: `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`
- Result: PASS.
- Command or check: `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_opforge_native_cli_ -- --nocapture`
- Result: PASS.
- Command or check: `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_hunk_smoke -- --nocapture`
- Result: PASS.
- Command or check: `scripts/workflow/run_rust_quality_gate.sh`
- Result: PASS.

## Closure Status

- Status: fixed
- Residual risk: The native opasm runtime stage still covers the initial
  supported 6502 subset rather than a complete target surface. That limitation
  is existing native-subset scope; the closed finding was the CLI-owned duplicate
  selector policy for that surface.
- Closure rationale: The CLI no longer contains the helper that pre-resolved
  label operands into fixed hex text and source-lock coverage asserts those
  helpers remain absent. The opasm runtime stage receives the parsed statement
  data and label metadata, resolves operands internally, builds the encode
  request payload, and provides instruction-size decisions. Host tests and
  FS-UAE tests prove forward-label `jmp` output still matches the Rust VM bytes
  and that unresolved labels are still rejected diagnostically.

## Notes

- The FS-UAE hunk smoke needed the same PRVM module root for `tkpkg_debug_cli`
  that host-side assembly already uses; that staging fix is included so the
  emulator gate exercises the intended native dependency graph.