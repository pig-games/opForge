# opForge Motorola 68000 ISA Remediation Plan (v0.1)

## Metadata

- Source: `documentation/reviews/68000ISAReview-2026-03-27.md`; explicit user instruction on 2026-03-27 to turn the approved review into a remediation plan
- Mode: `remediation`
- Owner: implementation agent

## Objective

Close the three validated baseline-68000 ISA and effective-address findings from
`documentation/reviews/68000ISAReview-2026-03-27.md` in ordered, commit-sized
slices without widening scope beyond those review findings and their directly
required regression coverage and fixture updates.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Scope is limited to closing `RVW-2026-03-27-001`, `RVW-2026-03-27-002`, and
  `RVW-2026-03-27-003` from the approved review artifact.
- Do not widen scope to post-68000 CPUs, broad documentation rewrites, alias
  expansion, VM rollout work, or unrelated cleanup.
- Work must stay inside the existing crate-based architecture, with family-wide
  68000 behavior in `crates/opforge-families/src/m68k/*` and regression
  coverage in the existing assembler/example workflows.
- Any example or reference-artifact changes must follow the repository's normal
  fail-before-update reference workflow.
- Only one work item may be active at a time, and each item must end in its own
  commit before the next item begins.

## Work Items

- [x] `M68K-R01`: Reject illegal `CMP Dn,<ea>` forms and remove the silent
      misassembly path.
  - Source requirement or finding IDs: `RVW-2026-03-27-001`
  - Expected closure: full closure of `RVW-2026-03-27-001`
  - Definition of done: `CMP` no longer shares the memory-destination branch
    that accepts `Dn,<memory alterable ea>`, invalid baseline forms diagnose
    deterministically, and existing legal `CMP` encodings remain unchanged
  - Validation: focused family and assembler tests proving legal
    `CMP <ea>,Dn` encodings remain correct while `CMP.B/W/L Dn,<ea>` now emit
    deterministic legality diagnostics; `cargo fmt --all`;
    `cargo clippy --workspace -- -D warnings`; `cargo audit`; `make test`
  - Expected files: `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: focused family and asm regression tests,
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` against
    `agents/plan-compliance-reviewer.md` citing `M68K-R01` before commit
  - Commit outcome: one commit that removes the illegal `CMP Dn,<ea>` encode
    path and lands the regression tests that prove rejection

- [x] `M68K-R02`: Fix PC-relative displacement handling for scalar symbol expressions.
  - Source requirement or finding IDs: `RVW-2026-03-27-002`
  - Expected closure: full closure of `RVW-2026-03-27-002`
  - Definition of done: scalar symbols in PC-relative forms encode as literal
    displacements, relocatable targets still rebase from `PC + 2`, and the new
    regression coverage protects both paths
  - Validation: focused family and assembler tests covering `.const` and `.set`
    scalar symbols in `d16(PC)` and `d8(PC,Xn)` forms, plus label-based
    PC-relative forms to prove relocatable targets still rebase correctly;
    `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`;
    `cargo audit`; `make test`
  - Expected files: `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: focused family and asm regression tests,
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` against
    `agents/plan-compliance-reviewer.md` citing `M68K-R02` before commit
  - Commit outcome: one commit that fixes scalar-symbol PC-relative encoding and
    lands targeted regression coverage for scalar versus relocatable references

- [x] `M68K-R03`: Restore legal PC-relative data-addressing for read-only `CMPI` and `BTST`, and sync the affected fixtures.
  - Source requirement or finding IDs: `RVW-2026-03-27-003`
  - Expected closure: full closure of `RVW-2026-03-27-003`
  - Definition of done: legal PC-relative `CMPI` and `BTST` cases assemble,
    alterable-only bit-modify forms stay rejected, and shipped fixtures reflect
    the corrected legality surface
  - Validation: focused family and assembler legality tests proving
    `CMPI.W #imm,d16(PC)` and `BTST #imm,d16(PC)` assemble on baseline 68000,
    while `BCHG/BCLR/BSET` remain alterable-only; fail-before-update reference
    compare if example fixtures change; reference update; clean compare;
    `cargo fmt --all`; `cargo clippy --workspace -- -D warnings`;
    `cargo audit`; `make test`
  - Expected files: `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-asm/src/tests.rs`, `examples/68000_btst_pc_relative_error.asm`,
    `examples/reference/68000_btst_pc_relative_error.err`, `examples/*`,
    `examples/reference/*`
  - Full quality gates: focused family and asm legality tests,
    fail-before-update reference compare when fixtures change, reference update,
    clean compare, `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` against
    `agents/plan-compliance-reviewer.md` citing `M68K-R03` before commit
  - Commit outcome: one commit that restores the baseline-68000 PC-relative
    data-addressing cases for `CMPI` and `BTST` and leaves shipped fixtures
    truthful about the final legality surface

## Milestones

- [x] Milestone 1: silent code-generation defects closed (`M68K-R01`,
      `M68K-R02`)
- [x] Milestone 2: remaining baseline PC-relative legality gap closed and
      fixtures synchronized (`M68K-R03`)

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- if the source review changes materially during execution, update this plan and
  re-run plan validation before continuing
