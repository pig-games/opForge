# Finding Closure Report

## Finding

- ID: `RVW-2026-04-05-001`
- Original summary: `encode_ammx_vea()` rejected much of the PRM-defined AMMX vector-effective-address matrix, including displacement, indexed, PC-relative, absolute, and immediate forms.

## Claimed Fix

- Plan item: `WI-4`
- Implementation slice or commit: `1930d01` (`Expanded the 68080 AMMX vector effective-address matrix.`)
- Changed files: `crates/opforge-families/src/m68080/handler.rs`, `crates/opforge-asm/src/tests.rs`, `examples/motorola68000/68080_ammx_addressing_matrix.asm`, `examples/reference/motorola68000/68080_ammx_addressing_matrix.hex`, `examples/reference/motorola68000/68080_ammx_addressing_matrix.lst`, `examples/reference/motorola68000/68080_ammx_shape_error.err`

## Validation Evidence

- Command or check: `cargo test --workspace 68080`
- Result: `PASS`, including the expanded `m68080_ammx_full_extension_matrix` coverage.
- Command or check: `make reference-test`
- Result: `PASS`, confirming the checked AMMX addressing example and refreshed references assemble as expected.
- Command or check: `make test-external-oracle`
- Result: `PASS`, confirming the updated AMMX surface remained consistent with the external oracle validation set used for this remediation slice.

## Closure Status

- Status: `fixed`
- Residual risk: The encoded AMMX matrix is now covered by targeted tests and checked examples; no additional open closure risk for this finding remains.

## Notes

- This finding was closed entirely by WI-4.

## Finding

- ID: `RVW-2026-04-05-002`
- Original summary: `LOADI` and `STOREI` reused the generic AMMX load/store path and therefore encoded the wrong selector model, while AMMX vector-effective-address support was also narrower than the PRM contract.

## Claimed Fix

- Plan item: `WI-3` and `WI-4`
- Implementation slice or commit: `9227a58` (`Implemented the 68080 LOADI and STOREI selector model.`), `1930d01` (`Expanded the 68080 AMMX vector effective-address matrix.`)
- Changed files: `crates/opforge-families/src/m68080/handler.rs`, `crates/opforge-asm/src/tests.rs`, `examples/motorola68000/68080_ammx_addressing_matrix.asm`, `examples/reference/motorola68000/68080_ammx_addressing_matrix.hex`, `examples/reference/motorola68000/68080_ammx_addressing_matrix.lst`, `examples/reference/motorola68000/68080_ammx_shape_error.err`

## Validation Evidence

- Command or check: `cargo test --workspace 68080`
- Result: `PASS`, including selector-carrier, selector-diagnostic, and expanded VEA coverage.
- Command or check: `make reference-test`
- Result: `PASS`, confirming the refreshed `68080_ammx_addressing_matrix` example and references for selector-aware and expanded addressing forms.
- Command or check: `make test-external-oracle`
- Result: `PASS`, confirming the updated LOADI/STOREI and AMMX addressing surface against the external oracle fixture set used during WI-3 and WI-4.

## Closure Status

- Status: `fixed`
- Residual risk: The original selector-model and AMMX-addressing mismatches cited by the review no longer reproduce under the targeted coverage and refreshed reference surface.

## Notes

- This finding required both the selector-specific WI-3 slice and the broader VEA WI-4 slice before it was fully closed.

## Finding

- ID: `RVW-2026-04-05-003`
- Original summary: the 68080 FPU path only recognized m68020-style one- and two-operand `FP0`-`FP7` forms and could not assemble PRM-defined banked `E0-E23` three-operand syntax.

## Claimed Fix

- Plan item: `WI-5` and `WI-6`
- Implementation slice or commit: `79a4801` (`Added the 68080 banked FPU parser substrate.`), `7253692` (`Implemented the 68080 banked three-operand FPU encode path.`)
- Changed files: `crates/opforge-families/src/m68k/handler.rs`, `crates/opforge-families/src/m68080/handler.rs`, `crates/opforge-asm/src/tests.rs`, `examples/motorola68000/68080_fpu_surface.asm`, `examples/reference/motorola68000/68080_fpu_surface.hex`, `examples/reference/motorola68000/68080_fpu_surface.lst`

## Validation Evidence

- Command or check: `cargo test -p families parses_banked_68080_fpu_data_register_operands -- --nocapture`
- Result: `PASS`, proving the parser substrate accepts banked `E0-E23` FPU operands for 68080 FPU mnemonics.
- Command or check: `cargo test -p asm m68080_banked_fpu_three_operand_forms_encode -- --nocapture`
- Result: `PASS`, proving the PRM-listed banked arithmetic group covered by this slice now assembles through concrete bytes, including `FMUL.W E4,FP3,E5`, `FCMP.W E4,FP3,E5`, `FSCALE E4,FP3,E5`, and `FREM E4,FP3,E5`.
- Command or check: `cargo test --workspace 68080`
- Result: `PASS`, confirming the focused FPU banked-form coverage and the wider 68080 slice remain green.
- Command or check: `make reference-test`
- Result: `PASS`, confirming the checked `68080_fpu_surface` example and refreshed references now include banked three-operand forms.

## Closure Status

- Status: `fixed`
- Residual risk: The original review gap is closed for the PRM-listed banked arithmetic forms now covered by targeted tests and the checked 68080 FPU reference surface.

## Notes

- WI-5 closed the parser/register-substrate half of the finding and WI-6 closed the encoding half.

## Finding

- ID: `RVW-2026-04-05-004`
- Original summary: `ADDIW`, `CMPIW`, and `MOVIW` rejected high-bit 16-bit literals even though the PRM defines those operands as raw word patterns.

## Claimed Fix

- Plan item: `WI-2`
- Implementation slice or commit: `084502e` (`Accepted raw 16-bit word patterns for 68080 word immediates.`)
- Changed files: `crates/opforge-families/src/m68080/handler.rs`, `crates/opforge-asm/src/tests.rs`, `examples/motorola68000/68080_integer_addressing_matrix.asm`, `examples/reference/motorola68000/68080_integer_addressing_matrix.hex`, `examples/reference/motorola68000/68080_integer_addressing_matrix.lst`

## Validation Evidence

- Command or check: `cargo test --workspace 68080`
- Result: `PASS`, including the high-bit literal coverage added for the 68080 word-immediate path.
- Command or check: `make reference-test`
- Result: `PASS`, confirming the refreshed integer-addressing example and references for raw 16-bit word-pattern literals.

## Closure Status

- Status: `fixed`
- Residual risk: The original high-bit literal rejection no longer reproduces under the targeted test and checked example coverage.

## Notes

- This finding was closed entirely by WI-2.
- Post-closure PRM sanity follow-up: `EXTUB`/`EXTUW` required a separate opcode-base correction and refreshed integer reference outputs, but `PERM` still intentionally keeps `SS=01`. Under the current PRM BANK-prefix interpretation, `SS` encodes the total prefixed instruction length; the assembled `PERM` forms remain 6-byte bundles (`71 xx 4C C0 imm16`), so `01` is still the expected field value rather than `00`.

## Finding

- ID: `RVW-2026-04-05-005`
- Original summary: the 68080 runtime state defaulted Apollo mode to on, rejected `.apollo off`, and therefore accepted Apollo-gated Line-A instructions without an explicit enable.

## Claimed Fix

- Plan item: `WI-1`
- Implementation slice or commit: `332fcf2` (`Corrected the 68080 Apollo state and gating contract.`)
- Changed files: `crates/opforge-families/src/m68k/state.rs`, `crates/opforge-asm/src/tests.rs`, `examples/motorola68000/68080_apollo_gate_error.asm`, `examples/reference/motorola68000/68080_apollo_gate_error.err`, `examples/ab/motorola68000/vasm/68080/documented_divergence/68080_mov3q_core.asm`, `examples/ab/motorola68000/vasm/68080/documented_divergence/68080_moviw_core.asm`, `examples/motorola68000/68080_full_additional_surface.asm`, `examples/reference/motorola68000/68080_full_additional_surface.lst`

## Validation Evidence

- Command or check: `cargo test --workspace 68080`
- Result: `PASS`, including the new Apollo default-off and directive-handling coverage.
- Command or check: `make reference-test`
- Result: `PASS`, confirming the refreshed Apollo-gating example and reference diagnostic.
- Command or check: `make test-external-oracle`
- Result: `PASS`, confirming the updated Apollo-gating behavior against the external oracle fixture set used for WI-1.

## Closure Status

- Status: `fixed`
- Residual risk: The original Apollo default-state and explicit-disable mismatch no longer reproduces under the targeted tests and checked example coverage.

## Notes

- This finding was closed entirely by WI-1.