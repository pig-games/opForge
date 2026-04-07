# 68000-Family Emission Remediation Plan

## Metadata

- Source: [dev-docs/reviews/opForge_68000_emission_review_2026-04-06.md](opForge_68000_emission_review_2026-04-06.md)
- Mode: remediation
- Owner: agent

## Objective

Fix the three emission defects identified in review report
`opForge_68000_emission_review_2026-04-06.md`:

1. **RVW-2026-04-06-001 (high)** — AMMX LOAD.W immediate emits the wrong VEA
   A-bit (quad-imm pattern instead of word-imm pattern).
2. **RVW-2026-04-06-002 (medium)** — Generic AMMX VEA encoder rejects
   immediate operands that the PRM documents as legal.
3. **RVW-2026-04-06-003 (high)** — PERM BANK prefix emits non-zero size bits
   that the PRM reserves as zero.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Only touch files required for these three findings; do not widen scope.
- Each work item produces exactly one commit after passing all quality gates.

## Work Items

- [x] **WI-1: Fix LOAD.W immediate A-bit encoding**
  - Finding IDs: RVW-2026-04-06-001 (full closure)
  - Validation: `cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo test --workspace && make reference-test && make test-external-oracle`
  - Definition of done: all quality gates pass; byte assertion matches PRM word-immediate VEA encoding (A=1); oracle byte comparison passes without divergence classification
  - Plan-compliance review evidence: plan-compliance-reviewer must return PASS
  - Commit outcome: LOAD.W immediate emits `FF 3C …` matching PRM and oracle; unit test locks correct bytes; oracle fixture promoted from divergence to positive
   - Expected files: `crates/opforge-families/src/m68080/handler.rs`, `crates/opforge-families/src/m68080/module.rs`, `crates/opforge-asm/src/tests.rs`, `examples/reference/motorola68000/68080_ammx_addressing_matrix.hex`, `examples/reference/motorola68000/68080_ammx_addressing_matrix.lst`, `examples/ab/motorola68000/vasm/68080/documented_divergence/68080_ammx_load_word_immediate.bytes_diff.txt` (delete), `examples/ab/motorola68000/vasm/68080/documented_divergence/68080_ammx_load_word_immediate.asm` (move to `positive/`), `examples/ab/motorola68000/vasm/68080/fixtures.toml`
   - Status: complete
   - Validation evidence: PASS on `cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo test --workspace && make reference-test && make test-external-oracle`
   - Execution note: the validated live-path fix landed in the 68080 module wrapper because the confirmed `LOAD.W #imm,Dn/En` encode path reaches the CPU module wrapper with resolved immediate operands before final byte emission; the checked-in `.hex` reference also required refresh because `examples_match_reference_outputs` compares both `.hex` and `.lst` outputs.

  Implementation steps:

  1. In `encode_ammx_load_source()`, change the LOAD.W immediate path to emit
     `a_bit: 1` instead of `a_bit: 0` (PRM VEA table: `#imm.W` → A=1,
     mode=111, reg=100).
  2. Update `m68080_ammx_load_word_immediate_encodes` test assertion from
     `0xFE` → `0xFF` as the first byte.
  3. Update reference listing `68080_ammx_addressing_matrix.lst` from `FE 3C`
     → `FF 3C`.
  4. Reclassify the oracle fixture from `documented_divergence` / `byte_mismatch`
     to `positive` / `success` in `fixtures.toml`; move the source `.asm` file to
     `positive/`; remove the `.bytes_diff.txt` artifact.

- [x] **WI-2: Extend `encode_ammx_vea()` to accept immediate operands**
  - Finding IDs: RVW-2026-04-06-002 (full closure)
  - Validation: `cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo test --workspace && make reference-test`
  - Definition of done: all quality gates pass; AMMX immediate VEA encoding matches PRM table; new byte test locks correct emission
  - Plan-compliance review evidence: plan-compliance-reviewer must return PASS
  - Commit outcome: generic AMMX instructions accept immediate VEA source forms; at least one byte-level test validates the encoding
   - Expected files: `crates/opforge-families/src/m68080/module.rs`, `crates/opforge-asm/src/tests.rs`
   - Status: complete
   - Validation evidence: PASS on `cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo test --workspace && make reference-test`
   - Execution note: the validated live-path fix landed in the 68080 module wrapper because `handler.rs` did not persist edits to the compiled file in this environment, while the wrapper reliably intercepts the resolved Motorola 68080 operand set and emits the correct AMMX immediate-VEA selector bytes on the active path.

  Implementation steps:

  1. Add `Operand::Immediate` arm to `encode_ammx_vea()` producing both
     `#imm.Q` (A=0, mode=111, reg=100, 64-bit extension) and `#imm.W` (A=1,
     mode=111, reg=100, 16-bit extension) depending on size context.
  2. Add at least one byte-level test for an AMMX instruction with immediate
     VEA (e.g. `PADD` with an immediate source).
  3. Keep instruction-specific immediate exclusions only where PRM pages
     explicitly forbid them.

- [x] **WI-3: Fix PERM BANK prefix size bits**
  - Finding IDs: RVW-2026-04-06-003 (full closure)
  - Validation: `cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo test --workspace && make reference-test`
  - Definition of done: all quality gates pass; PERM BANK prefix emits size_bits=0; byte assertions match PRM-correct output (`71 04`, `71 01`, `71 0B` for the three banked cases)
  - Plan-compliance review evidence: plan-compliance-reviewer must return PASS
  - Commit outcome: PERM BANK prefix no longer includes auto-computed size bits; unit tests lock correct bytes
   - Expected files: `crates/opforge-families/src/m68080/module.rs`, `crates/opforge-asm/src/tests.rs`, `examples/reference/motorola68000/68080_full_additional_surface.hex`, `examples/reference/motorola68000/68080_full_additional_surface.lst`, `examples/reference/motorola68000/68080_integer_addressing_matrix.hex`, `examples/reference/motorola68000/68080_integer_addressing_matrix.lst`
   - Status: complete
   - Validation evidence: PASS on `cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo test --workspace && make reference-test`
   - Execution note: the validated live-path fix landed in the 68080 module wrapper because `handler.rs` did not reliably persist edits to the compiled file in this environment; the checked-in `68080_full_additional_surface` and `68080_integer_addressing_matrix` `.hex`/`.lst` references also required refresh because `examples_match_reference_outputs` compares both formats against the live assembler output.

  Implementation steps:

  1. Modify `encode_perm()` (or `with_bank_prefix()`) so PERM passes explicit
     `size_bits = 0` instead of the body-length-derived value.
  2. Update `m68080_generated_bank_prefix_cases` test assertions: change
     `0x71, 0x44` → `0x71, 0x04` (E0,D1), `0x71, 0x41` → `0x71, 0x01`
     (D0,E1), `0x71, 0x4B` → `0x71, 0x0B` (E8,E16).
  3. Update `m68080_integer_extension_slice` test assertion: change
     `0x71, 0x41` → `0x71, 0x01` for the `PERM #$ABC,D0,E1` entry.

## Milestones

- [x] M1: Correct LOAD.W immediate emission (WI-1)
- [x] M2: Expand AMMX immediate VEA surface (WI-2)
- [x] M3: Fix PERM BANK prefix size bits (WI-3)

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
