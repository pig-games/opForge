# opForge Amiga Hunk Executable Completeness Implementation Plan v0.3

## Metadata

- Source: `documentation/opForge-amiga-hunk-executable-completeness-spec-v0_3.md`
- Mode: `implementation`
- Owner: Codex

## Objective

Take the completed `v0.2` Hunk executable subset to executable completeness:
broader natural bare-symbol notation, removal of the mandatory explicit
placement gate for supported Hunk executables, and broader executable relocation
coverage for the explicit `vasmm68k_mot`-anchored Hunk-executable compatibility
matrix defined in the `v0.3` spec.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- This plan starts from the completed `v0.2` Hunk executable baseline and must
  preserve the current working AmigaOS examples while extending support.
- One active work item at a time.
- Each work item or phase must end in exactly one new commit before the next
  item begins.
- Full quality gates are mandatory before each commit.
- `plan-compliance-reviewer` must pass before each plan-driven commit.
- Scope is limited to executable Hunk completeness only.
- Scope is limited to Hunk executable authoring behavior; it must not broaden
  into general M68k instruction-set expansion work unless a narrowly required
  parser or resolver change is needed to expose an already-supported Hunk-safe
  symbolic form.
- The spec-defined `v0.3` compatibility matrix is the complete scope target for
  this plan; additional bare-symbol executable forms are deferred unless a
  later spec revision adds them explicitly.
- For the unplaced executable path covered by this plan, emitted segment order
  must be determined solely by user-declared `sections=...`, with unused
  `.region` declarations having no effect on output semantics.
- Hunk object-file output, symbol hunks, debug hunks, overlays, memory-type
  expansion, and other non-executable phases remain out of scope and are to be
  spec’d/planned later.
- FS-UAE remains opt-in validation, not a default required dependency.

## Work Items

- [x] Work item 1: land the first bare-symbol executable notation slice from the frozen matrix
  - Source requirement or finding IDs: spec `Goals` notation completeness; spec `Behavioral Contract` A; spec `Acceptance Criteria` covered bare-symbol executable forms
  - Definition of done:
    - the spec-defined `v0.3` Hunk-executable compatibility matrix remains the binding implementation target and is not redefined by plan execution
    - the first notation-owned subset is limited to `LEA label,A1`, `PEA label`, `MOVE.L label,Dn`, `MOVE.L Dn,label`, `MOVEA.L label,An`, `JMP label`, and `JSR label`
    - each notation-owned covered form is accepted without explicit `.L` and resolves to one canonical relocatable long encoding
    - still-ambiguous forms remain explicit-only with deterministic diagnostics
  - Validation:
    - add focused tests for the notation-owned subset: `LEA label,A1`, `PEA label`, `MOVE.L label,Dn`, `MOVE.L Dn,label`, `MOVEA.L label,A0`, `JMP label`, and `JSR label`
    - add focused negative tests for any remaining ambiguous forms
    - run `cargo test -p asm linker_output_hunk_ -- --nocapture`
    - run `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`
    - run `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-asm/src/asmline_instruction.rs`
    - `crates/opforge-asm/src/line.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to implementing the spec-defined `v0.3` notation matrix
  - Commit outcome:
    - natural bare-symbol executable source becomes a first-class supported path for the notation-owned `v0.3` subset, without treating general ISA support or extra matrix expansion as the work objective

- [x] Work item 2: remove the mandatory region and explicit placement gate for supported Hunk executables
  - Source requirement or finding IDs: spec `Goals` no mandatory explicit placement gate; spec `Behavioral Contract` B; spec `Boundary Cases` unplaced selected executable sections; spec `Acceptance Criteria` unplaced ordering semantics
  - Validation: focused unplaced-emission tests plus full quality gates for the `sections=...`-driven executable path
  - Definition of done:
    - `format=hunk` succeeds for supported selected sections without `.region`, `.place`, or `.pack`
    - explicit placement remains supported and behaviorally consistent
    - optional `.region` declarations that are not used for explicit `.place` or `.pack` do not become hidden prerequisites or semantic modifiers for the covered subset
    - for the unplaced executable path, emitted segment order is determined solely by `sections=...`
    - the unplaced executable path continues to reject any `sections=...` order whose first emitted segment is not code
    - unsupported fixups under the unplaced executable path continue to fail explicitly
    - emitted target indexing stays deterministic under that unplaced ordering rule and the existing omission rule for empty selected non-BSS sections
  - Validation details:
    - add focused tests for unplaced `code,data` Hunk executable output with no `.region`
    - add focused tests for `.region` declared but unused inputs proving they behave the same as inputs with no `.region`
    - add focused tests proving emitted segment order follows `sections=...` exactly for the unplaced executable path
    - add a focused negative test proving the unplaced path still rejects any `sections=...` order whose first emitted segment is not code
    - add a focused negative test proving unsupported fixups still fail explicitly under the unplaced executable path
    - add focused tests proving explicit placement and unplaced emission agree for the covered subset
    - add a focused test with an empty selected non-BSS section and at least one relocation proving deterministic omission and stable emitted target indices under the unplaced path
    - run `cargo test -p asm linker_output_hunk_ -- --nocapture`
    - run `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`
    - run `cargo test -p asm examples_match_reference_outputs -- --nocapture`
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
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to removing the Hunk executable region or placement prerequisite
  - Commit outcome:
    - Hunk executable output no longer depends on `.region`, `.place`, or `.pack` for the supported subset, and the unplaced path has binding `sections=...`-driven ordering semantics

- [x] Work item 3: broaden executable relocation coverage against the explicit compatibility matrix
  - Source requirement or finding IDs: spec `Goals` broader relocation coverage; spec `Behavioral Contract` C; spec `Acceptance Criteria` broader symbolic instruction and data forms; spec `Acceptance Criteria` explicit Hunk-executable compatibility matrix
  - Definition of done:
    - the relocation-owned remainder of the frozen `v0.3` matrix is limited to `.long label`, `.long label+const`, and `MOVE.L #label,Dn`
    - those relocation-owned remaining entries emit correct addends and Hunk relocations
    - unsupported relocation kinds or expression shapes still fail deterministically
    - multi-section multi-fixup executable examples no longer depend on ad hoc special cases
  - Validation:
    - add focused tests for `.long label`, `.long label+const`, and `MOVE.L #label,Dn` across data-expression addend and relocation coverage
    - add focused negative tests for relocation kinds or shapes still outside the executable subset
    - run `cargo test -p asm linker_output_hunk_ -- --nocapture`
    - run `cargo test -p asm motorola68000_family_example_programs_assemble_in_reference_workflow -- --nocapture`
    - run `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-asm/src/asmline_instruction.rs`
    - `crates/opforge-asm/src/line.rs`
    - `crates/opforge-vm/src/output_hunk.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to broader executable relocation coverage
  - Commit outcome:
    - the executable Hunk support story covers the relocation-owned remaining explicit compatibility-matrix forms without widening past the frozen `v0.3` matrix

- [x] Work item 4: refresh examples, references, and opt-in emulator validation for executable completeness
  - Source requirement or finding IDs: spec `Goals` preserve and simplify examples where support permits; spec `Acceptance Criteria`; spec `Validation Expectations`
  - Definition of done:
    - AmigaOS examples reflect the broader notation or placement support actually landed by prior items
    - references are refreshed to match the new executable-complete subset
    - default validation remains green and the opt-in emulator path still proves the examples runnable
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
    - `examples/motorola68000/amigaos/`
    - `examples/reference/motorola68000/amigaos/`
    - `crates/opforge-asm/src/tests.rs`
    - docs only where supported behavior changed
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to example and validation refresh after executable-complete behavior is already landed
  - Commit outcome:
    - the executable-complete Hunk support is visible in checked-in examples and emulator-validated where configured

## Milestones

- [x] Milestone 1: the notation-owned bare-symbol subset from the frozen `v0.3` Hunk-executable compatibility matrix lands (`Work item 1`)
- [x] Milestone 2: Hunk executable output no longer depends on `.region` or explicit placement for the supported subset, and the unplaced path follows binding `sections=...` ordering (`Work item 2`)
- [x] Milestone 3: broader executable relocation coverage lands across the relocation-owned remaining `v0.3` matrix forms (`Work item 3`)
- [x] Milestone 4: examples and validation reflect executable completeness without widening into later Hunk phases (`Work item 4`)

## To Be Spec’d / Planned Later

- Hunk object-file output
- symbol hunks
- debug hunks
- overlay support
- memory-type customization beyond the executable default
- richer non-executable relocation-kind expansion beyond what executable completeness truly needs

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- do not widen this plan into non-executable Hunk feature phases that are explicitly deferred
- do not regress the currently working AmigaOS Hunk example programs while broadening executable completeness
