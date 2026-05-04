# Finding Closure Report

## Finding

- ID: `RVW-2026-05-03-006`
- Original summary: the native tkpkg resolver paths used repeated `LEA 0(Ax),Ay` address-register copies where plain `MOVEA.L` copies were sufficient.

## Claimed Fix

- Plan item: Item 1 - scan-path skip and address-copy optimizations.
- Implementation slice or commit: pre-commit item-1 native tkpkg optimization slice on `main`
- Changed files:
  - `examples/motorola68000/amigaos/tkpkg/tkpkg_token_policy.asm`
  - `examples/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm`
  - `crates/opforge-asm/src/tests.rs`
  - `examples/reference/motorola68000/amigaos/opforge/opforge_cli.hunk`
  - `examples/reference/motorola68000/amigaos/opforge/opforge_cli.lst`
  - `examples/reference/motorola68000/amigaos/tkpkg/tkpkg_debug_cli.hunk`
  - `examples/reference/motorola68000/amigaos/tkpkg/tkpkg_debug_cli.lst`

## Validation Evidence

- Command or check: implementation diff inspection for the item-1 slice
- Result: PASS; the zero-displacement `LEA 0(Ax),Ay` address copies in the native tkpkg token-policy and pipeline scan paths were replaced with `MOVEA.L` register copies, and the focused source-surface assertions in `crates/opforge-asm/src/tests.rs` were updated to lock the new form.
- Command or check: `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
- Result: PASS; the focused native tkpkg suite stayed green after the address-register copy tightening.
- Command or check: `cargo test -p vm motorola68000_tokenizer_vm_staged_corpus_matches_host_for_example_lines -- --nocapture`
- Result: PASS; staged Motorola 68000 tokenizer VM parity remained aligned with host behavior.
- Command or check: `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_tkpkg_native_motorola68000_family_corpus_matches_vm_authoritative_rows -- --nocapture`
- Result: PASS; the FS-UAE native parity gate stayed green for the package-backed tokenizer path after the `MOVEA.L` substitutions.
- Command or check: `scripts/workflow/run_rust_quality_gate.sh`
- Result: PASS; the canonical Rust quality gate completed successfully after the related example references were refreshed.

## Closure Status

- Status: fixed
- Residual risk: low; these are semantics-preserving address-register copies in hot scan paths, and both source-surface and runtime validation stayed green.
- Closure rationale: the reviewed `LEA 0(Ax),Ay` copy idiom has been removed from the targeted native tkpkg resolver paths and replaced with the intended `MOVEA.L` form without changing runtime outcomes.

## Notes

- This closure covers only the reviewed native tkpkg resolver locations in the current slice; item-2 and item-3 work remain pending in the plan.