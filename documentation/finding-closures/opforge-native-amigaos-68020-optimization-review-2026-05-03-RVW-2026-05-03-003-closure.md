# Finding Closure Report

## Finding

- ID: `RVW-2026-05-03-003`
- Original summary: fixed locator state in the native tkpkg resolver path was cleared byte-by-byte instead of being treated as aligned four-byte records plus, where needed, one trailing owner byte.

## Claimed Fix

- Plan item: Item 2 - aligned locator-record longword clear/copy optimizations.
- Implementation slice or commit: pre-commit item-2 native tkpkg optimization slice on `main`
- Changed files:
  - `examples/motorola68000/amigaos/tkpkg/tkpkg_token_policy.asm`
  - `examples/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm`
  - `examples/motorola68000/amigaos/tkpkg/tkpkg_buffers.asm`
  - `examples/reference/motorola68000/amigaos/opforge/opforge_cli.hunk`
  - `examples/reference/motorola68000/amigaos/opforge/opforge_cli.lst`
  - `examples/reference/motorola68000/amigaos/tkpkg/tkpkg_debug_cli.hunk`
  - `examples/reference/motorola68000/amigaos/tkpkg/tkpkg_debug_cli.lst`

## Validation Evidence

- Command or check: implementation diff inspection for the reconstructed item-2 slice
- Result: PASS; `tkpkg_token_policy_find_owner_v1` now uses `CLR.L (A3)+` followed by `CLR.B (A3)` for the four-byte pending token-policy locator plus trailing owner tag, while `tkpkgPipelineNoDialect` and `tkpkgPipelineClearOptionalLocator` now clear fixed locator records with `CLR.L (A3)`.
- Command or check: alignment justification audit of the shared locator storage in `tkpkg_buffers.asm`
- Result: PASS; `.align 2` now explicitly realigns `activeTokenPolicyOffsetLo`, `activeTokenizerVmOffsetLo`, `pendingFamilyOffsetLo`, and `pendingTokenizerVmOffsetLo`, and `pendingTokenPolicyOffsetLo` remains naturally aligned because it follows the four-byte `pendingCanonicalDialect` locator record.
- Command or check: `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
- Result: PASS; the focused native tkpkg suite stayed green after replacing the byte-wise locator clears with longword clears.
- Command or check: `cargo test -p vm motorola68000_tokenizer_vm_staged_corpus_matches_host_for_example_lines -- --nocapture`
- Result: PASS; staged Motorola 68000 tokenizer VM parity remained aligned with host behavior after the locator-state changes.
- Command or check: `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_tkpkg_native_motorola68000_family_corpus_matches_vm_authoritative_rows -- --nocapture`
- Result: PASS; the FS-UAE native parity gate stayed green for the package-backed tokenizer path after the longword locator-clear changes.
- Command or check: `opForge_UPDATE_REFERENCE=1 cargo test -p asm examples_match_reference_outputs -- --nocapture`
- Result: PASS; the expected native example payload and listing references were refreshed for the intentional assembly output drift caused by the optimized locator operations.

## Closure Status

- Status: fixed
- Residual risk: low; the change is limited to fixed-size locator-state reset paths and is covered by focused asm, VM, FS-UAE, and reference-output validation.
- Closure rationale: the reviewed byte-wise locator-clear inefficiency no longer exists in the affected native tkpkg paths, and the aligned storage needed for the longword operations is now explicit in the shared buffer layout.

## Notes

- The refreshed `opforge_cli` and `tkpkg_debug_cli` references capture the expected payload and listing drift from the item-2 locator optimizations.