# opForge Motorola 68000 Family 68080 Extension Implementation Plan (v0.1)

## Metadata

- Source: specification at documentation/opForge-m68000-family-68080-extension-spec-v0_1.md
- Mode: implementation
- Owner: GitHub Copilot (GPT-5.3-Codex)

## Objective

Implement the bounded 68080 extension surface defined in the source
specification, preserving existing 68000-68040 behavior, and delivering
deterministic legality, diagnostics, and fixture-backed validation for the
first 68080 integer and AMMX slices.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Scope is limited to the source specification requirement IDs
  `REQ-68080-001` through `REQ-68080-009` and acceptance criteria
  `AC-68080-001` through `AC-68080-008`.
- No work item may silently widen scope beyond the source specification.
- One active work item at a time; each work item ends in exactly one new
  commit before the next work item starts.

## Work Items

- [x] Work Item 1: Freeze 68080 syntax and directive decisions required to
      remove implementation ambiguity.
  - Source requirement or finding IDs: `Q-68080-001`, `Q-68080-002`,
    `Q-68080-003`, `Q-68080-004`, `REQ-68080-003`, `REQ-68080-005`
  - Validation: python3 scripts/workflow/check_spec_artifact.py documentation/opForge-m68000-family-68080-extension-spec-v0_1.md
  - Definition of done: no unresolved blocker remains that would change parser
    contract, directive naming, or capability contract during implementation.
  - Expected files: documentation/opForge-m68000-family-68080-extension-spec-v0_1.md,
    documentation/plans/opForge-m68000-family-68080-extension-implementation-plan-v0_1.md
  - Full quality gates: bash scripts/workflow/run_spec_workflow.sh documentation/opForge-m68000-family-68080-extension-spec-v0_1.md "freeze 68080 syntax/directive decisions"
  - Plan-compliance review evidence: `PASS` from plan-compliance-reviewer citing
    explicit closure/update of open questions and no scope widening.
  - Commit outcome: one commit updating source spec and this plan with frozen
    decisions or explicit deferred items.

- [x] Work Item 2: Add `m68080` CPU identity, aliases, capability reporting,
      and CPU gating skeleton without enabling new mnemonics.
  - Source requirement or finding IDs: `REQ-68080-001`, `REQ-68080-002`,
    `REQ-68080-008`, `AC-68080-001`, `AC-68080-002`, `AC-68080-004`
  - Validation: cargo test -p opforge-asm cpu_aliases_and_support
  - Definition of done: `.cpu 68080` resolves; capability output includes
    `m68080`; no existing CPU alias/behavior regresses.
  - Expected files: crates/opforge-families/src/lib.rs,
    crates/opforge-families/src/m68080.rs,
    crates/opforge-families/src/m68080/module.rs,
    crates/opforge-families/src/m68080/handler.rs,
    crates/opforge-engine/src/lib.rs,
    crates/opforge-asm/src/tests.rs
  - Full quality gates: cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo test --workspace
  - Plan-compliance review evidence: `PASS` from plan-compliance-reviewer with
    traceability to `REQ-68080-001/002/008` and regression safety for 68000-68040.
  - Commit outcome: one commit that adds CPU registration/alias surfaces and
    tests proving discoverability and non-regression.

- [x] Work Item 3: Extend register and operand model for E/B namespaces and add
      Apollo mode runtime gating state.
  - Source requirement or finding IDs: `REQ-68080-004`, `REQ-68080-005`,
    `REQ-68080-008`, `AC-68080-005`, `AC-68080-006`
  - Validation: cargo test -p opforge-asm m68k_register_and_runtime_directive_slices
  - Definition of done: E/B registers are legal only on `m68080`; Apollo mode
    gate exists and produces deterministic disabled-mode diagnostics.
  - Expected files: crates/opforge-families/src/m68k/module.rs,
    crates/opforge-families/src/m68k/operand.rs,
    crates/opforge-families/src/m68k/handler.rs,
    crates/opforge-families/src/m68k/state.rs,
    crates/opforge-asm/src/tests.rs
  - Full quality gates: cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo test --workspace
  - Plan-compliance review evidence: `PASS` from plan-compliance-reviewer
    confirming deterministic non-68080 rejection of E/B registers and explicit
    Apollo-gating diagnostics.
  - Commit outcome: one commit that lands parser/runtime-state changes and
    focused positive/negative tests.

- [x] Work Item 4: Implement bounded 68080 integer extension encoding slice.
  - Source requirement or finding IDs: `REQ-68080-006`, `REQ-68080-008`,
    `REQ-68080-009`, `AC-68080-003`, `AC-68080-004`, `AC-68080-005`
  - Validation: cargo test -p opforge-asm m68080_integer_slice
  - Definition of done: representative in-scope integer mnemonics assemble on
    `m68080` and fail deterministically on unsupported CPUs or disabled Apollo mode.
  - Expected files: crates/opforge-families/src/m68k/table.rs,
    crates/opforge-families/src/m68080/handler.rs,
    crates/opforge-asm/src/tests.rs,
    examples/motorola68000/68080_integer_slice.asm,
    examples/reference/motorola68000/68080_integer_slice.lst,
    examples/reference/motorola68000/68080_integer_slice.hex
  - Full quality gates: cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo test --workspace
  - Plan-compliance review evidence: `PASS` from plan-compliance-reviewer with
    evidence that only in-spec integer forms were enabled.
  - Commit outcome: one commit with integer-slice encoding plus fixtures and
    rejection diagnostics for non-68080 modes.

- [x] Work Item 5: Implement bounded AMMX extension encoding slice and operand
      shape enforcement.
  - Source requirement or finding IDs: `REQ-68080-007`, `REQ-68080-008`,
    `REQ-68080-009`, `AC-68080-007`
  - Validation: cargo test -p opforge-asm m68080_ammx_slice
  - Definition of done: in-scope AMMX representative forms pass with expected
    bytes; invalid pair/alignment/shape forms fail deterministically.
  - Expected files: crates/opforge-families/src/m68k/table.rs,
    crates/opforge-families/src/m68080/handler.rs,
    crates/opforge-asm/src/tests.rs,
    examples/motorola68000/68080_ammx_slice.asm,
    examples/reference/motorola68000/68080_ammx_slice.lst,
    examples/reference/motorola68000/68080_ammx_slice.hex
  - Full quality gates: cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo test --workspace
  - Plan-compliance review evidence: `PASS` from plan-compliance-reviewer with
    evidence of legal-shape success and illegal-shape deterministic failures.
  - Commit outcome: one commit that enables bounded AMMX instruction families
    and adds fixtures/tests.

- [x] Work Item 6: Diagnostic normalization, documentation sync, and final
      quality hardening.
  - Source requirement or finding IDs: `REQ-68080-009`, `AC-68080-002`,
    `AC-68080-008`
  - Validation: cargo test -p opforge-asm normalization
  - Definition of done: diagnostic classes remain stable, docs are honest about
    shipped 68080 surface, and full workspace validation is green.
  - Expected files: crates/opforge-asm/src/normalization.rs,
    crates/opforge-asm/src/tests.rs,
    documentation/opForge-m68000-family-68080-extension-spec-v0_1.md,
    documentation/plans/opForge-m68000-family-68080-extension-implementation-plan-v0_1.md,
    README.md (if capability matrix excerpt is present)
  - Full quality gates: cargo fmt --all && cargo clippy --all-targets --all-features -- -D warnings && cargo test --workspace
  - Plan-compliance review evidence: `PASS` from plan-compliance-reviewer with
    explicit confirmation that docs/diagnostics match shipped behavior.
  - Commit outcome: one final commit that lands normalization updates,
    documentation alignment, and evidence updates.

## Milestones

- [x] Milestone 1: Source ambiguity resolved and CPU identity scaffold landed
      (Work Items 1-2).
- [x] Milestone 2: Register/gating substrate and integer extension slice landed
      (Work Items 3-4).
- [x] Milestone 3: AMMX slice, diagnostics, and final hardening landed
      (Work Items 5-6).

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping