# opForge Motorola 68000 Family Implementation Plan (v0.1, 68000 baseline)

## Metadata

- Source: `documentation/opForge-m68000-family-68000-cpu-extension-spec-v0_1.md`
- Mode: `implementation`
- Owner: implementation agent

## Objective
Implement the `motorola68000` / `m68000` baseline defined in the source spec as
ordered, commit-sized slices that fit the current crate-based workspace. The
family namespace for this work is `m68k`; the baseline CPU namespace is
`m68000`.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Scope is limited to baseline `m68000` support and the canonical
  `motorola68k` dialect described by the source spec.
- Work must use the current crate layout (`crates/opforge-*`) rather than the
  older monolithic `src/` layout.
- Do not widen scope to later 68000-family CPUs, compatibility dialects, or
  authoritative VM rollout tables unless the source spec is updated first.
- Any formatter or VM work in this plan is limited to keeping existing builtin
  registration and registry-derived hierarchy-package behavior coherent.

## Work Items

- [x] `M68K-001`: Add the `motorola68000` family stack and make `.cpu 68000`
      discoverable in the current registry surfaces.
  - Source requirement or finding IDs: `REQ-M68K-001`, `REQ-M68K-004`,
    `REQ-M68K-005`, `AC-M68K-001`
  - Validation: focused alias-resolution, `cpusupport`, capabilities, and
    registry-derived hierarchy-package smoke tests plus `cargo fmt`,
    `cargo clippy -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: `.cpu 68000`, `.cpu m68000`, and `.cpu mc68000`
    resolve; default dialect metadata is present; and registry-derived
    discovery/hierarchy-package paths remain green
  - Expected files: `crates/opforge-families/src/lib.rs`,
    `crates/opforge-families/src/m68k.rs`,
    `crates/opforge-families/src/m68k/module.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68k/operand.rs`,
    `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68000.rs`,
    `crates/opforge-families/src/m68000/module.rs`,
    `crates/opforge-families/src/m68000/handler.rs`,
    `crates/opforge-asm/src/engine.rs`, `crates/opforge-asm/src/tests.rs`,
    `crates/opforge-engine/src/lib.rs`
  - Full quality gates: `cargo fmt`, `cargo clippy -- -D warnings`,
    `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-001` before commit
  - Commit outcome: one commit that adds the new family stack and makes
    `motorola68000` / `m68000` discoverable without claiming encode
    completeness yet

- [x] `M68K-002`: Implement canonical 68000 operand parsing and baseline CPU
      metadata.
  - Source requirement or finding IDs: `REQ-M68K-002`, `REQ-M68K-003`,
    `REQ-M68K-005`, `AC-M68K-002`, `AC-M68K-004`
  - Validation: operand parser unit tests, CPU metadata tests, and focused
    `.word` or `.emit word` endianness checks plus `cargo fmt`,
    `cargo clippy -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: all baseline operand families from the spec parse
    deterministically, and `m68000` reports the required 24-bit big-endian
    target metadata
  - Expected files: `crates/opforge-families/src/m68k/operand.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68k/module.rs`,
    `crates/opforge-families/src/m68000/module.rs`,
    `crates/opforge-families/src/m68000/handler.rs`,
    `crates/opforge-core/src/parser.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: `cargo fmt`, `cargo clippy -- -D warnings`,
    `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-002` before commit
  - Commit outcome: one commit that resolves the v0.1 operand families into
    structured family operands and exposes 68000 CPU target metadata

- [x] `M68K-003`: Implement the first native encode slice for movement and
      addressing instructions.
  - Source requirement or finding IDs: `REQ-M68K-002`, `REQ-M68K-003`,
    `AC-M68K-002`
  - Validation: focused encode tests for `MOVE`, `MOVEA`, `LEA`, `PEA`, `JMP`,
    and `JSR` plus negative legality checks and the full required gates
  - Definition of done: representative movement/addressing programs assemble to
    expected bytes, and illegal effective-address combinations fail
    deterministically
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68000/handler.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: `cargo fmt`, `cargo clippy -- -D warnings`,
    `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-003` before commit
  - Commit outcome: one commit that makes a small but useful 68000 program
    assemble through the live native path

- [x] `M68K-004`: Implement the remaining baseline instruction classes and
      deterministic diagnostics.
  - Source requirement or finding IDs: `REQ-M68K-002`, `REQ-M68K-003`,
    `AC-M68K-002`, `AC-M68K-003`
  - Validation: arithmetic, control-flow, shift, quick-form, immediate-range,
    and branch-boundary tests plus deterministic diagnostic assertions and the
    full required gates
  - Definition of done: arithmetic, control-flow, shift, and quick-form slices
    assemble correctly, and size/branch/immediate diagnostics are deterministic
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68000/handler.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: `cargo fmt`, `cargo clippy -- -D warnings`,
    `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-004` before commit
  - Commit outcome: one commit that completes the native baseline instruction
    set promised by the spec

- [x] `M68K-005`: Add integration coverage, examples, and minimal pipeline
      coherence work for the new family.
  - Source requirement or finding IDs: `REQ-M68K-001`, `REQ-M68K-004`,
    `REQ-M68K-005`, `AC-M68K-001`, `AC-M68K-004`, `AC-M68K-005`
  - Validation: example assembly runs, formatter registry smoke coverage,
    registry-derived hierarchy-package smoke coverage, reference comparison, and
    the full required gates
  - Definition of done: example/reference artifacts are stable, formatter and
    engine registration paths stay coherent, and the new family does not regress
    existing stacks
  - Expected files: `crates/opforge-asm/src/tests.rs`,
    `crates/opforge-engine/src/lib.rs`,
    `crates/opforge-formatter/src/builtin_hooks.rs`,
    `crates/opforge-formatter/src/hook_registry.rs`,
    `crates/opforge-formatter/src/fixture_tests.rs`,
    `examples/68000_basic_moves.asm`, `examples/68000_branching.asm`,
    `examples/68000_effective_addresses.asm`,
    `examples/68000_arithmetic_sizes.asm`, `examples/reference/*`
  - Full quality gates: `cargo fmt`, `cargo clippy -- -D warnings`,
    `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-005` before commit
  - Commit outcome: one commit that makes 68000 support visible across the
    assembler, engine, formatter, and example/reference surfaces without
    claiming authoritative VM rollout

- [x] `M68K-006`: Sync documentation to shipped 68000 behavior and close the
      v0.1 delivery slice.
  - Source requirement or finding IDs: `REQ-M68K-004`, `AC-M68K-006`
  - Validation: documentation smoke review against real examples plus
    `cargo fmt`, `cargo clippy -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: README and manual coverage match implemented behavior,
    and documentation does not over-claim later CPUs or authoritative VM parity
  - Expected files: `README.md`, `documentation/opForge-reference-manual.md`,
    and the next release-notes draft file only if release-note workflow
    requires it
  - Full quality gates: `cargo fmt`, `cargo clippy -- -D warnings`,
    `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-006` before commit
  - Commit outcome: one commit that syncs user-facing docs to the actual shipped
    `m68000` baseline and nothing broader

## Milestones

- [x] Milestone 1: registration and operand foundation complete (`M68K-001`,
      `M68K-002`)
- [x] Milestone 2: native encoding baseline complete (`M68K-003`, `M68K-004`)
- [x] Milestone 3: integration, examples, and documentation complete
      (`M68K-005`, `M68K-006`)

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- if the source spec changes materially during execution, update this plan and
  re-run plan validation before continuing
