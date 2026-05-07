# opForge Native AmigaOS 6502 First-Run Plan Handover

## Status

- Active plan:
  `documentation/plans/opforge-native-amigaos-6502-full-assembly-first-run-plan-v0_1.md`.
- Completed slice: Item 1, first-run 6502 parity matrix and artifact contract.
- Not completed: Items 2-18 remain open. The next implementation step is Item 2,
  moving remaining assembly-engine ownership out of the native CLI and into
  native `opasm`.
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
- `scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-native-amigaos-6502-full-assembly-first-run-plan-v0_1.md`
- `make workflow-gate`
- `scripts/workflow/run_rust_quality_gate.sh`

All listed validations passed. `cargo audit` reported the existing allowed
warnings for `registry` and `rand`, then the Rust quality gate completed with
`PASS`.

## Next Slice

- Target Item 2 only.
- Expected files for Item 2 remain:
  `native/motorola68000/amigaos/opasm/*`,
  `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`, and focused
  coverage in `crates/opforge-asm/src/tests.rs`.
- First concrete code change should be to introduce or wire a native `opasm`
  engine entrypoint that owns pass/session/image state, then call it from the
  CLI without expanding selector or directive coverage in the same slice.
- Before committing Item 2, rerun the item-specific focused tests, the full
  Rust quality gate, and the plan-compliance gate.
