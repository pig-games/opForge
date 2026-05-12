# opForge Amiga Hunk Full-Support Implementation Plan v0.2

## Metadata

- Source: `documentation/opForge-amiga-hunk-full-support-spec-v0_2.md`
- Mode: `implementation`
- Owner: Codex

## Objective

Take opForge from the current working Hunk executable subset to practical
full-support regular AmigaDOS executable generation: generic fixup capture,
broader data and instruction relocation support, removal of the assigned-base
requirement for valid relocatable Hunk executables after explicit placement has
succeeded, preservation of emitted-segment indexing semantics, and targeted
notation improvements for the first-slice unambiguous symbolic forms defined by
the source spec.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- This plan is a follow-on to the already-landed v0.1 Hunk work and must
  preserve current runnable Hunk examples while extending support.
- One active work item at a time.
- Each work item or phase must end in exactly one new commit before the next
  item begins.
- Full quality gates are mandatory before each commit.
- `plan-compliance-reviewer` must pass before each plan-driven commit.
- Scope must remain limited to regular executable Hunk support plus the
  notation improvements explicitly covered by the source spec.
- The existing `.output` placement gate remains in scope; this plan removes the
  writer-side assigned-base prerequisite for supported relocatable Hunk
  emission but does not make unplaced sections legal.
- Object-file output, overlays, debug hunks, symbol hunks, and Workbench
  startup handling remain out of scope for this plan.
- FS-UAE remains opt-in validation, not a default required dependency.

## Work Items

- [x] Work item 1: introduce a generic fixup model and preserve the current Hunk subset through that seam
  - Source requirement or finding IDs: spec `Goals` generic fixup model; spec `Invariants / Constraints` separation of fixup capture vs Hunk rendering; spec `Acceptance Criteria` generic fixup consumption
  - Definition of done:
    - the model carries source section, offset, relocation kind, target identity, and encoded addend information
    - the current working `helloworld`, `writefile`, and existing focused Hunk tests remain green
    - deterministic empty selected non-BSS omission and emitted-segment `HUNK_RELOC32` indexing behavior remain preserved through the new seam
    - no new notation behavior is introduced in this item
  - Validation:
    - add focused tests proving the current relocation-free and `HUNK_RELOC32` example paths still work through the new generic fixup seam
    - keep or add a focused byte-level test proving deterministic empty non-BSS omission still produces the same emitted segment list and relocation target indices
    - run `cargo test -p asm linker_output_hunk_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-vm/src/output_model.rs`
    - `crates/opforge-asm/src/engine.rs`
    - `crates/opforge-asm/src/line.rs`
    - `crates/opforge-asm/src/asmline_instruction.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to replacing the relocation contract shape without widening supported source forms yet
  - Commit outcome:
    - Hunk output consumes a generic fixup model instead of treating Hunk-specific relocation vectors as the long-term primary contract

- [x] Work item 2: broaden data-side fixup production for ordinary symbol-bearing longword data
  - Source requirement or finding IDs: spec `Goals` broader data relocation support; spec `Behavioral Contract` data relocation contract; spec `Acceptance Criteria` `.long label` and `.long label + constant`
  - Definition of done:
    - supported longword data expressions emit correct addends and `HUNK_RELOC32` records
    - unsupported data-expression shapes fail explicitly rather than collapsing to constants
  - Validation:
    - add focused tests for `.long label`, `.long label + constant`, and multi-entry longword pointer tables
    - run `cargo test -p asm linker_output_hunk_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-asm/src/line.rs`
    - `crates/opforge-asm/src/tests.rs`
    - optionally one focused Amiga or Hunk example if needed for coverage
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to ordinary longword data fixups
  - Commit outcome:
    - ordinary symbol-bearing longword data participates naturally in executable Hunk relocation output

- [x] Work item 3: broaden instruction-side fixup capture across a wider common m68k absolute-long subset
  - Source requirement or finding IDs: spec `Goals` broader instruction relocation support; spec `Behavioral Contract` instruction relocation contract; spec `Boundary Cases` multiple relocations and unsupported forms
  - Definition of done:
    - the first instruction-family matrix is limited to `LEA symbol.L,An`, `PEA symbol.L`, `MOVE.L symbol.L,Dn`, `MOVE.L Dn,symbol.L`, `MOVE.L symbol.L,d16(An)`, and `MOVE.L d16(PC),symbol.L`
    - that first matrix emits correct section-relative addends and `HUNK_RELOC32` records
    - unsupported instruction forms still fail explicitly and deterministically
    - all other instruction families remain out of scope for this item
    - ambiguous bare absolute-symbol instruction forms remain explicit-only in this item
  - Validation:
    - add focused tests for `LEA symbol.L,An`, `PEA symbol.L`, `MOVE.L symbol.L,Dn`, `MOVE.L Dn,symbol.L`, `MOVE.L symbol.L,d16(An)`, and `MOVE.L d16(PC),symbol.L`
    - add focused negative tests proving ambiguous bare absolute-symbol instruction forms still require explicit notation during this item
    - run `cargo test -p asm linker_output_hunk_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-asm/src/asmline_instruction.rs`
    - `crates/opforge-families/src/m68k/` files only if operand metadata or family help is required
    - `crates/opforge-asm/src/tests.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to broader instruction fixup capture and addend rewriting
  - Commit outcome:
    - a named first matrix of common AmigaOS-style explicit absolute-address instructions is no longer limited to a tiny hand-recognized relocation subset

- [x] Work item 4: remove the assigned-base requirement for valid relocatable Hunk executable emission after explicit placement succeeds
  - Source requirement or finding IDs: spec `Goals` support for explicitly placed selected sections without pre-assigned final bases; spec `Behavioral Contract` Hunk executable contract; spec `Boundary Cases` explicitly placed sections without assigned final bases; spec `Acceptance Criteria` emitted segment list indexing
  - Definition of done:
    - explicitly placed supported selected sections can emit executable Hunk output without pre-assigned final absolute load addresses
    - the existing `.output` placement gate remains intact
    - section order, deterministic empty non-BSS omission, emitted-segment relocation indexing, and code-first validation remain intact
    - unsupported or incomplete fixup metadata still fails deterministically
  - Validation:
    - add focused tests proving `format=hunk` succeeds for supported explicitly placed CODE/DATA/BSS section sets without assigned final bases and still rejects non-code-first output
    - add byte-level tests proving deterministic empty-section omission and `HUNK_RELOC32` target indices remain defined against the emitted segment list after the assigned-base prerequisite is removed
    - run `cargo test -p asm linker_output_hunk_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-vm/src/output_hunk.rs`
    - `crates/opforge-asm/src/engine.rs`
    - `crates/opforge-vm/src/output_model.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to Hunk executable input requirements and writer behavior
  - Commit outcome:
    - valid relocatable Hunk executables no longer depend on artificially assigned final section bases once explicit placement has already succeeded

- [x] Work item 5: land tightly enumerated notation improvements for the spec-defined first slice only
  - Source requirement or finding IDs: spec `Goals` notation improvements; spec `Behavioral Contract` notation improvement contract; spec `Resolved v0.2 Scope Decisions`; spec `Acceptance Criteria` covered first-slice notation forms
  - Definition of done:
    - covered first-slice forms are limited to already-unambiguous immediate bare-symbol long forms and longword data forms
    - covered first-slice forms resolve to the canonical relocatable long encoding
    - ambiguous bare absolute-symbol instruction forms continue to require explicit notation and emit clear diagnostics
    - no silent word-sized downgrade occurs for covered relocatable symbolic forms
  - Validation:
    - add focused tests for covered first-slice forms such as `MOVE.L #label,D1`, `.long label`, and `.long label + constant`
    - add focused diagnostics tests proving `LEA label,A1`, `PEA label`, and `MOVE.L D0,label` remain explicit-only until a later spec widens the notation contract
    - run `cargo test -p asm linker_output_hunk_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-families/src/m68k/` parser or resolver files
    - `crates/opforge-asm/src/asmline_instruction.rs`
    - `crates/opforge-asm/src/line.rs`
    - `crates/opforge-asm/src/tests.rs`
    - AmigaOS example files only if a working example becomes cleaner because of covered first-slice notation that is actually in scope
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to explicitly enumerated notation improvements and related diagnostics
  - Commit outcome:
    - the spec-defined first slice of symbolic source reads more naturally without widening into ambiguous bare absolute-symbol instruction forms

- [x] Work item 6: refresh examples, references, and opt-in emulator validation for the broader Hunk surface
  - Source requirement or finding IDs: spec `Goals` preserve working examples; spec `Acceptance Criteria` runnable examples and opt-in validation; spec `Validation Expectations`
  - Definition of done:
    - AmigaOS examples and references reflect the newer relocation support and only the notation support actually landed by prior items
    - default validation stays green without FS-UAE while the opt-in emulator path continues to work when configured
  - Validation:
    - update example/reference fixtures only after the production behavior is landed
    - run `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`
    - run `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - run the opt-in FS-UAE smoke path when the environment is configured
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `examples/motorola68000/amigaos/` example files as needed
    - `examples/reference/motorola68000/amigaos/` reference files
    - `crates/opforge-asm/src/tests.rs`
    - relevant docs only where behavior or supported notation changed
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to example and validation refresh after the core behavior is already landed
  - Commit outcome:
    - the broader Hunk support is visible in checked-in examples and validated by both default and opt-in workflows

## Milestones

- [x] Milestone 1: generic fixup groundwork is landed without regressing the current working Hunk subset (`Work item 1`)
- [x] Milestone 2: ordinary data and broader instruction relocations flow through the generic model (`Work item 2` and `Work item 3`)
- [x] Milestone 3: explicitly placed relocatable Hunk executables no longer require assigned final bases (`Work item 4`)
- [x] Milestone 4: tightly scoped notation improvements and refreshed AmigaOS examples make the feature practical to author against without widening spec scope (`Work item 5` and `Work item 6`)

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- do not widen this plan into Hunk object-file output, overlays, debug hunks, symbol hunks, or Workbench startup support
- do not regress the currently working AmigaOS Hunk example programs while broadening support
