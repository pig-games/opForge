# opForge Native AmigaOS 6502 First-Run Plan Handover

## Status

- Active plan:
  `documentation/plans/opforge-native-amigaos-6502-full-assembly-first-run-plan-v0_1.md`.
- Completed slices:
  - Item 1, first-run 6502 parity matrix and artifact contract.
  - Item 2, native `opasm` two-pass engine and assembly-session ownership.
- Not completed: Items 3-18 remain open. The next implementation step is Item
  3, completing package-backed parse records for directives and operands.
- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times.

## Completed Changes

- Added `examples/mos6502/6502_first_run_artifact_contract.asm`.
- Added Rust reference artifacts:
  - `examples/reference/mos6502/6502_first_run_artifact_contract.hex`
  - `examples/reference/mos6502/6502_first_run_artifact_contract.lst`
- Added focused Rust contract test:
  `motorola68020_opforge_native_cli_first_run_artifact_contract_locks_rust_outputs`.
- Updated the active plan with the first-run acceptance matrix and marked Item 1
  and Milestone 1 complete.
- Added `native/motorola68000/amigaos/opasm/opasm_engine.asm`.
- Moved the transitional two-pass loop and assembly-session storage into native
  `opasm.amigaos.engine`.
- Updated the native CLI to call `opasm_engine_run_two_pass_v1` and import
  opasm-owned statement, label, image, source-record, PC/origin, and pass state.
- Left the CLI callback adapter in place for the existing smoke semantics; this
  is the bridge that Item 3 should replace with richer package-backed parse
  records.
- Updated the active plan to mark Item 2 and Milestone 2 complete.

## Locked Artifact Contract

- `.bin` bytes for `$0800..$0814`:
  `A9 42 8D 02 02 F0 05 D0 F7 A2 10 E8 AA 0C 08 03 08 4F 4B FF FF`.
- `.prg` bytes: little-endian load address prefix `00 08`, followed by the
  `.bin` payload.
- `.hex` payload:
  `:15080000A9428D0202F005D0F7A210E8AA0C0803084F4BFFFFB0`,
  followed by EOF.
- `.lst` payload: reference listing in
  `examples/reference/mos6502/6502_first_run_artifact_contract.lst`.

## Validation Completed

- `cargo test -p asm motorola68020_opforge_native_cli_first_run_artifact_contract_locks_rust_outputs -- --nocapture`
- `cargo test -p asm examples_match_reference_outputs -- --nocapture`
- `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`
- `cargo test -p asm motorola68020_opasm_engine_module_owns_two_pass_loop -- --nocapture`
- `cargo test -p asm motorola68020_opforge_native_cli_two_pass_engine_surface_tracks_forward_label_layout -- --nocapture`
- `cargo test -p asm motorola68020_opforge_native_cli_shell_assembles_with_stage_stub -- --nocapture`
- `cargo test -p asm motorola68020_prvm_ -- --nocapture`
- `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
- `scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-native-amigaos-6502-full-assembly-first-run-plan-v0_1.md`
- `make workflow-gate`
- `scripts/workflow/run_rust_quality_gate.sh`

All listed validations passed. `cargo audit` reported the existing allowed
warnings for `registry` and `rand`, then the Rust quality gate completed with
`PASS`.

## Next Slice

- Target Item 3 only.
- Expected files for Item 3 remain:
  `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`,
  `native/motorola68000/amigaos/prvm/*`,
  `native/motorola68000/amigaos/opasm/*`, and focused parser parity coverage in
  `crates/opforge-asm/src/tests.rs`.
- First concrete code change should expand the package-backed parse record shape
  enough for native `opasm` to receive directive-ready and operand-ready rows
  without adding CLI-side re-parsing.
- Before committing Item 3, rerun the item-specific focused tests, the full
  Rust quality gate, and the plan-compliance gate.
