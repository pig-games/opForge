# Finding Closure Report

## Finding

- ID: RVW-2026-05-06-004
- Original summary: The native AmigaOS CLI parsed `--bin` and `--hunk` into the
  same output path state and later wrote that path through the flat-byte writer,
  making `--hunk` misleading and format selection implicit.

## Claimed Fix

- Plan item: Item 5, "Split native output format state and make `--hunk`
  deterministic and honest," in
  `documentation/plans/opforge-native-amigaos-deliverable-remediation-plan-2026-05-06.md`.
- Implementation slice or commit: pending Item 5 commit.
- Changed files:
  - `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`
  - `crates/opforge-asm/src/tests.rs`
  - `crates/opforge-asm/src/fs_uae_smoke.rs`
- Fix summary: the native CLI now records explicit output format state with
  separate `bin` and `hunk` path buffers. `--bin` is the only format routed into
  `opforge_native_cli_write_flat_output`, and `--hunk` returns deterministic
  `OPC-NCLI028` not-implemented diagnostics before assembly/output emission can
  write flat bytes. Host source-lock coverage and FS-UAE failure-path coverage
  distinguish the two modes.

## Validation Evidence

- Command or check: `cargo test -p asm motorola68020_opforge_native_cli_surface_locks_rust_subset_flag_names -- --nocapture`
- Result: PASS.
- Command or check: `cargo test -p asm motorola68020_opforge_native_cli_6502_small_assembly_contract_matches_rust_vm_bytes -- --nocapture`
- Result: PASS.
- Command or check: `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`
- Result: PASS.
- Command or check: `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_hunk_smoke -- --nocapture`
- Result: PASS.
- Command or check: `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_opforge_native_cli_ -- --nocapture`
- Result: PASS.
- Command or check: `scripts/workflow/run_rust_quality_gate.sh`
- Result: PASS. The captured log at `target/item5-rust-quality-gate.log` ends
  with `PASS: Rust quality gate complete.`

## Closure Status

- Status: fixed
- Residual risk: Native Amiga Hunk output remains intentionally unimplemented.
  That is now a truthful CLI contract rather than an implicit flat-output alias;
  a real Hunk writer remains deferred to future output-artifact work.
- Closure rationale: `--bin` and `--hunk` no longer share a path variable or an
  implicit flat-writer route. The parser stores explicit format state, the flat
  writer opens only the bin path, and the hunk path exits with a stable
  not-implemented diagnostic. Host and FS-UAE tests prove successful bin output
  still works while hunk output cannot silently produce flat bytes.

## Notes

- The default FS-UAE native CLI smoke now uses the supported `m6502` package and
  bin output path so the default success case exercises the truthful flat-output
  mode. The separate hunk failure case locks the deferred Hunk behavior.