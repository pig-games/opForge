# Finding Closure Report

## Finding

- ID: `RVW-2026-04-23-001`
- Original summary: `tokvmOpcodeJump` checked `D0 + 4` without first deriving `D0` from the current bytecode program counter, so malformed or short tokenizer bytecode could validate against dispatch state instead of the current `A0 - A3` offset.

## Claimed Fix

- Plan item: post-plan review remediation for the package-backed native tokenizer runtime implementation.
- Implementation slice or commit: `4f1879a7` (`Stabilize m68k native tokenizer parity`)
- Changed files:
  - `examples/motorola68000/amigaos/tokvm/tokvm_tokenizer_vm.asm`
  - `crates/opforge-asm/src/tests.rs`

## Validation Evidence

- Command or check: implementation diff inspection for `4f1879a7`
- Result: PASS; `tokvmOpcodeJump` now derives the operand-end offset from `A0 - A3`, adds the four-byte inline operand width, and compares that computed bytecode offset against `D7` before reading the jump operand.
- Command or check: `cargo test -p asm motorola68020_tokvm_ -- --nocapture`
- Result: PASS; 24 focused native tokvm surface and behavior tests passed after the remediation, including `motorola68020_tokvm_interpreter_locks_jump_bounds_and_hex_escape_emit`.
- Command or check: `cargo test -p vm motorola68000_tokenizer_vm_staged_corpus_matches_host_for_example_lines -- --nocapture`
- Result: PASS; the Rust VM staged tokenizer corpus continued to match the host reference behavior.
- Command or check: `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_tkpkg_native_motorola68000_family_corpus_matches_vm_authoritative_rows -- --nocapture`
- Result: PASS before closure documentation; the full FS-UAE native tokenizer corpus gate passed in 697.81s after the fix. It was not rerun for this closure-only slice because no relevant code changed after that gate.

## Closure Status

- Status: `fixed`
- Residual risk: low; the restored bounds check is locked by a focused source-surface assertion, while runtime malformed-bytecode cases are still a useful later negative-test expansion.

## Notes

- The fixed path now matches the other PC-relative jump handlers' offset-derivation pattern before validating inline operands.
- This closure records existing validation evidence only; no production or test code changed during closure.
