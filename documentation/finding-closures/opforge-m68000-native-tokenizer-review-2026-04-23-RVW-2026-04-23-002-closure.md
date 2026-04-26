# Finding Closure Report

## Finding

- ID: `RVW-2026-04-23-002`
- Original summary: the `tkpkg` smoke assertion still required the old fixed `m68020` `setPipelineRequest` block even though the debug CLI now emits CPU-selectable pipeline payloads.

## Claimed Fix

- Plan item: post-plan review remediation for the package-backed native tokenizer runtime implementation.
- Implementation slice or commit: `4f1879a7` (`Stabilize m68k native tokenizer parity`)
- Changed files:
  - `crates/opforge-asm/src/tests.rs`
  - `examples/motorola68000/amigaos/tkpkg/tkpkg_debug_cli.asm`

## Validation Evidence

- Command or check: implementation diff inspection for `4f1879a7`
- Result: PASS; the smoke assertion now locks the conditional `setPipelineRequest` shape, including `TKPKG_DEBUG_PIPELINE_M68000`, `TKPKG_DEBUG_PIPELINE_M68080`, and the default `m68020` payload.
- Command or check: `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
- Result: PASS; 19 focused `tkpkg` tests passed after the assertion refresh.
- Command or check: `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_tkpkg_native_motorola68000_family_corpus_matches_vm_authoritative_rows -- --nocapture`
- Result: PASS before closure documentation; the full FS-UAE native tokenizer corpus gate passed in 697.81s after the fix. It was not rerun for this closure-only slice because no relevant code changed after that gate.

## Closure Status

- Status: `fixed`
- Residual risk: low; the refreshed assertion covers the CPU-selectable shape that broke the focused test, and the FS-UAE corpus gate exercised the native debug CLI path with CPU-selected pipelines.

## Notes

- The assertion now matches the intended CPU-selectable contract instead of preserving the obsolete fixed `m68020` block.
- This closure records existing validation evidence only; no production or test code changed during closure.
