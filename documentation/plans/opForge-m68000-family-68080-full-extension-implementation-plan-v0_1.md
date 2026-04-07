# opForge Motorola 68000 Family 68080 Full Extension Implementation Plan (v0.1)

## Metadata

- Source: `documentation/opForge-m68000-family-68080-full-extension-spec-v0_1.md`
- Mode: implementation
- Owner: GitHub Copilot (GPT-5.4)
- Status: complete; `plan-quality-reviewer` passed before implementation

## Objective

Implement the full missing 68080 assembler surface required by the source
specification, with no optional or low-priority omissions, while preserving
existing 68000-68040 behavior and enforcing deterministic legality,
diagnostics, fixtures, and workflow gates.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Scope includes all mandatory requirements `REQ-68080-FULL-001` through
  `REQ-68080-FULL-011` and acceptance criteria `AC-68080-FULL-001` through
  `AC-68080-FULL-011`.
- Every missing instruction or alias named by the specification must land in
  one of the work items below; nothing in scope is optional, deferred, or
  low-priority.
- Existing 68000-68040 positive/reference behavior must remain unchanged unless
  a separate bug fix is explicitly required.
- Parse ownership stays in the m68k layer; CPU legality and feature gating stay
  in the m68080 layer.
- One active work item at a time; each work item or phase ends in exactly one
  new commit before the next work item starts.

## Execution Rules From AGENTS.md

- Production code first for each slice. Do not spend a slice on cleanup,
  renames, formatting-only changes, or test-harness expansion unless the work
  item explicitly requires it to unblock the requested behavior.
- Stay inside the smallest viable vertical slice. Each work item must advance a
  working part of the 68080 feature set rather than broad preparatory churn.
- Tests are supporting work, not the main work. Add the minimum targeted
  validation needed to prove the newly landed behavior, then run the full
  quality gates.
- Refactoring is allowed only when it directly enables the listed slice. If a
  slice requires refactoring, the implementation notes for that slice must
  explain why a narrower change was insufficient.
- If blocked, stop peripheral work and record the blocker precisely: exact
  cause, exact file or interface, what was attempted, what decision is needed,
  and the smallest next step once resolved.
- Because this is a larger multi-round effort, milestone boundaries should also
  collect traceability evidence, and `artifact-traceability-reviewer` should be
  used if slice-to-plan linkage becomes hard to inspect.

## Execution Protocol

1. Run `plan-quality-reviewer` on this plan and the source specification.
   Implementation does not start until that review returns `PASS`.
2. Select the next unchecked work item only after the previous one is fully
   committed and its checkbox state is updated.
3. Implement only the current slice and update the plan checkboxes as required
   bookkeeping.
4. Run the full quality gates for the slice:
   `cargo fmt --all`, `cargo clippy --all-targets --all-features -- -D warnings`,
   `cargo audit`, and `cargo test --workspace`.
5. Run any additional mandatory slice-specific validation listed in the work
   item, including `make reference-test` and `make test-external-oracle` when
   examples, references, or oracle fixtures change.
6. Run `plan-compliance-reviewer` with the active `AGENTS.md`, this plan, the
   current slice summary, changed files, and validation evidence.
7. If `plan-compliance-reviewer` returns `PASS`, create exactly one new commit
   for that work item, update the checkbox state, and then continue to the next
   item.
8. If any quality gate or `plan-compliance-reviewer` review fails, do not
   commit, do not advance to the next item, and resolve the blocker first.
9. If the same gate or review loop fails three times for one item, stop and ask
   the user to resolve the blockage before continuing.

## Work Items

- [x] WI-1: Activate the 68080 capability and directive substrate.
  - Source requirement or finding IDs: `REQ-68080-FULL-001`,
    `REQ-68080-FULL-003`, `REQ-68080-FULL-005`, `REQ-68080-FULL-006`,
    `REQ-68080-FULL-007`; `AC-68080-FULL-001`, `AC-68080-FULL-002`,
    `AC-68080-FULL-006`
  - Validation: `cargo test --workspace 68080`
  - Definition of done: 68080 capability reporting is complete, `.apollo` and
    `.fpu 68080` behave per spec, `.apollo off` emits the required deterministic
    strict-mode or unsupported-mode result, and non-68080 CPUs reject 68080-only
    surface entry points deterministically.
  - Coverage in this slice: canonical `m68080` CPU identity, full capability
    metadata for integer/AMMX/FPU surfaces, `.apollo` acceptance on 68080,
    deterministic `.apollo off` behavior on 68080, deterministic `.apollo`
    rejection on earlier CPUs, `.fpu 68080` legality, deterministic illegal
    target-pairing diagnostics, and baseline E/B-register gating.
  - Expected files: `crates/opforge-families/src/m68k/state.rs`,
    `crates/opforge-families/src/m68k/module.rs`,
    `crates/opforge-families/src/m68080/module.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming only capability,
    directive, and CPU-gating substrate changes landed for this slice.
  - Commit outcome: one commit enabling the full 68080 state and directive
    substrate required by all later slices.

- [x] WI-2: Land the AMMX alias and saturated arithmetic slice.
  - Source requirement or finding IDs: `REQ-68080-FULL-001`,
    `REQ-68080-FULL-004`, `REQ-68080-FULL-009`, `REQ-68080-FULL-011`;
    `AC-68080-FULL-004`, `AC-68080-FULL-009`
  - Validation: `cargo test --workspace 68080`
  - Definition of done: all listed mnemonics parse and encode correctly on
    68080, reject on earlier CPUs, and the dotless aliases no longer depend on
    dotted size-suffix parsing.
  - Coverage in this slice: `PADDB`, `PADDW`, `PADDUSB`, `PADDUSW`, `PSUBB`,
    `PSUBW`, `PSUBUSB`, `PSUBUSW`, and `PAVGB`, including first-class dotless
    lookup entries in `m68080_base_kind()` and correct saturated opcodes in the
    PADD/PSUB encoders.
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68080/handler.rs`,
    `crates/opforge-asm/src/tests.rs`,
    `examples/motorola68000/68080_full_additional_surface.asm`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming this commit is limited
    to alias registration and the saturated/average AMMX arithmetic path.
  - Commit outcome: one commit adding the required alias substrate and
    saturated/average AMMX arithmetic coverage.

- [x] WI-3: Land the fixed-opcode AMMX comparison and shift slice.
  - Source requirement or finding IDs: `REQ-68080-FULL-001`,
    `REQ-68080-FULL-004`, `REQ-68080-FULL-009`; `AC-68080-FULL-004`
  - Validation: `cargo test --workspace 68080`
  - Definition of done: every instruction listed in this work item assembles on
    68080 with the spec bytes and produces deterministic unsupported-CPU
    diagnostics elsewhere.
  - Coverage in this slice: `PMAXSB`, `PMAXUB`, `PMAXSW`, `PMAXUW`, `PMINSB`,
    `PMINUB`, `PMINSW`, `PMINUW`, `LSLQ`, and `LSRQ`.
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68080/handler.rs`,
    `crates/opforge-asm/src/tests.rs`,
    `examples/motorola68000/68080_ammx_addressing_matrix.asm`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming this commit stays inside
    the fixed-opcode AMMX compare/min-max/shift matrix.
  - Commit outcome: one commit adding the remaining fixed-opcode AMMX families.

- [x] WI-4: Land the constrained AMMX pair and group slice.
  - Source requirement or finding IDs: `REQ-68080-FULL-001`,
    `REQ-68080-FULL-004`, `REQ-68080-FULL-009`, `REQ-68080-FULL-010`,
    `REQ-68080-FULL-011`; `AC-68080-FULL-004`, `AC-68080-FULL-009`,
    `AC-68080-FULL-010`
  - Validation: `cargo test --workspace 68080`
  - Definition of done: all listed instructions encode correctly on legal forms
    and emit stable, deterministic diagnostics for pair/group/alignment
    violations, including the required `UNPACK1632` destination-pair checks.
  - Coverage in this slice: `BFLYB`, `BFLYW`, `C2P`, `MINTERM`, `TRANSHI`,
    `TRANSLO`, and `UNPACK1632` register-pair constraint validation, including
    dotless BFLY aliases, destination-pair even-alignment, source-group mod-4
    alignment, and deterministic pair/group-shape errors.
  - Expected files: `crates/opforge-families/src/m68k/operand.rs`,
    `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68080/handler.rs`,
    `crates/opforge-asm/src/tests.rs`,
    `examples/motorola68000/68080_ammx_shape_error.asm`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming this slice is limited to
    pair/group operand parsing, encoding, and legality enforcement.
  - Commit outcome: one commit landing the constrained AMMX pair/group matrix.

- [x] WI-5: Land the special-form AMMX memory slice.
  - Source requirement or finding IDs: `REQ-68080-FULL-001`,
    `REQ-68080-FULL-004`, `REQ-68080-FULL-009`, `REQ-68080-FULL-010`;
    `AC-68080-FULL-004`, `AC-68080-FULL-010`
  - Validation: `cargo test --workspace 68080`
  - Definition of done: all required STOREM, STOREM3, and TEX forms parse,
    encode, and fail deterministically on illegal shapes or invalid mode values.
  - Coverage in this slice: `STOREM`, `STOREM3`, and all required `TEX`
    forms: `TEX8.512`, `TEX16.256`, `TEX24.64`, and `TEX.B`, including nested
    operand parsing, mode-range checks for `STOREM3`, and deterministic malformed
    texture-operand diagnostics.
  - Expected files: `crates/opforge-families/src/m68k/operand.rs`,
    `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68080/handler.rs`,
    `crates/opforge-asm/src/tests.rs`,
    `examples/motorola68000/68080_ammx_addressing_matrix.asm`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming this slice is restricted
    to STOREM, STOREM3, and TEX special-form handling.
  - Commit outcome: one commit landing the special-form AMMX memory families.

- [x] WI-6: Land the B-register integer matrix.
  - Source requirement or finding IDs: `REQ-68080-FULL-001`,
    `REQ-68080-FULL-004`, `REQ-68080-FULL-007`, `REQ-68080-FULL-009`;
    `AC-68080-FULL-003`, `AC-68080-FULL-006`, `AC-68080-FULL-011`
  - Validation: `cargo test --workspace 68080`
  - Definition of done: every listed B-register instruction assembles on 68080,
    rejects on non-68080 CPUs, and preserves parse-vs-legality ownership.
  - Coverage in this slice: `ADDQ Bn`, `SUBQ Bn`, `MOVE Bn,<ea>`, `MOVEA <ea>,Bn`,
    `CMP Bn,Dn`, and `LEA` forms involving B registers, including the required
    68080-only legality restrictions for B-register sources and destinations.
  - Expected files: `crates/opforge-families/src/m68k/operand.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68080/handler.rs`,
    `crates/opforge-asm/src/tests.rs`,
    `examples/motorola68000/68080_integer_addressing_matrix.asm`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming this slice only advances
    the B-register integer matrix and its negative legality coverage.
  - Commit outcome: one commit enabling the full B-register integer instruction
    surface.

- [x] WI-7: Land the remaining non-branch integer extension slice.
  - Source requirement or finding IDs: `REQ-68080-FULL-001`,
    `REQ-68080-FULL-004`, `REQ-68080-FULL-005`, `REQ-68080-FULL-007`;
    `AC-68080-FULL-003`, `AC-68080-FULL-006`
  - Validation: `cargo test --workspace 68080`
  - Definition of done: each listed instruction or behavior delta encodes or
    gates correctly under 68080 and does not regress legacy CPU behavior.
  - Coverage in this slice: `CLR.Q`, `EXTUB`, `EXTUW`, `PERM`, `BANK`, 68080
    `MOVEC` control-register extensions, 68080 `MOVE SR` privilege behavior,
    and 68080 `MOVE16` alignment-relaxation behavior.
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68080/handler.rs`,
    `crates/opforge-asm/src/tests.rs`,
    `examples/motorola68000/68080_full_additional_surface.asm`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming this slice is limited to
    the remaining non-branch integer extensions and their 68080-specific
    legality rules.
  - Commit outcome: one commit landing the remaining non-branch integer surface.

- [x] WI-8: Land the branch-extension slice.
  - Source requirement or finding IDs: `REQ-68080-FULL-001`,
    `REQ-68080-FULL-004`, `REQ-68080-FULL-007`, `REQ-68080-FULL-010`;
    `AC-68080-FULL-003`, `AC-68080-FULL-006`, `AC-68080-FULL-008`
  - Validation: `cargo test --workspace 68080`
  - Definition of done: every listed branch-extension form assembles on 68080,
    rejects deterministically elsewhere, and reports stable diagnostics for
    illegal displacement cases.
  - Coverage in this slice: `DBcc.L`, all required `Bcc.S+` forms, `BRA.S+`,
    and `BSR.S+`, including the odd-bit long/extended-short signaling rule and
    deterministic misalignment diagnostics.
  - Expected files: `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68080/handler.rs`,
    `crates/opforge-asm/src/tests.rs`,
    `examples/motorola68000/68080_full_additional_surface.asm`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming this slice is restricted
    to branch extensions and their diagnostics.
  - Commit outcome: one commit landing the long-counter and extended-short
    branch matrix.

- [x] WI-9: Land the 68080 FPU slice.
  - Source requirement or finding IDs: `REQ-68080-FULL-002`,
    `REQ-68080-FULL-004`, `REQ-68080-FULL-006`, `REQ-68080-FULL-007`,
    `REQ-68080-FULL-010`; `AC-68080-FULL-005`, `AC-68080-FULL-006`,
    `AC-68080-FULL-008`
  - Validation: `cargo test --workspace 68080`
  - Definition of done: all required 68080 FPU forms encode correctly under
    legal `.cpu 68080` plus `.fpu 68080` state and reject illegal target
    pairings or operand shapes deterministically.
  - Coverage in this slice: `FDBcc.L`, `FMOVE.D Dn,FPn`, `FMOVE.D FPn,Dn`,
    `FMOVEM` Apollo extended-format behavior, and the required verification or
    correction of the existing 68080 FPU helper forms `FLOADI`, `FSTOREI`,
    `FMOVERZ`, and `FMOVEURZ` so that spec `FPU-01` through `FPU-04` all pass.
  - Expected files: `crates/opforge-families/src/m68k/state.rs`,
    `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68080/handler.rs`,
    `crates/opforge-asm/src/tests.rs`,
    `examples/motorola68000/68080_fpu_surface.asm`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`
  - Plan-compliance review evidence: `PASS` confirming this slice is limited to
    the 68080 FPU matrix and legal `.fpu 68080` behavior.
  - Commit outcome: one commit completing the 68080 FPU encoding and gating
    surface.

- [x] WI-10: Land the cross-CPU regression and diagnostic-normalization slice.
  - Source requirement or finding IDs: `REQ-68080-FULL-003`,
    `REQ-68080-FULL-007`, `REQ-68080-FULL-010`; `AC-68080-FULL-002`,
    `AC-68080-FULL-006`, `AC-68080-FULL-008`, `AC-68080-FULL-010`,
    `AC-68080-FULL-011`
  - Validation: `cargo test --workspace 68080` and `make reference-test`
  - Definition of done: the new 68080 surface is fully fenced off from earlier
    CPUs, diagnostics normalize into stable existing classes, and legacy
    reference outputs remain unchanged.
  - Coverage in this slice: explicit non-68080 negative matrices for all new
    mnemonics and E/B-register forms, deterministic normalization-class checks
    for new diagnostics, and proof that existing 68000-68040 positive/reference
    outputs remain unchanged.
  - Expected files: `crates/opforge-asm/src/normalization.rs`,
    `crates/opforge-asm/src/tests.rs`,
    `examples/reference/motorola68000/**/*`,
    `README.md`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`,
    `make reference-test`
  - Plan-compliance review evidence: `PASS` confirming the slice only advances
    regression-preservation and diagnostic-normalization coverage.
  - Commit outcome: one commit locking in the cross-CPU rejection matrix and
    diagnostic-stability coverage.

- [x] WI-11: Land the examples, references, oracle fixtures, and final
      traceability slice.
  - Source requirement or finding IDs: `REQ-68080-FULL-008`; `AC-68080-FULL-003`,
    `AC-68080-FULL-004`, `AC-68080-FULL-005`, `AC-68080-FULL-007`
  - Validation: `cargo test --workspace 68080`, `make reference-test`, and
    `make test-external-oracle`
  - Definition of done: every mandatory missing instruction family has example,
    reference, and fixture coverage appropriate to oracle support, and the plan
    state reflects completion accurately.
  - Coverage in this slice: comprehensive example/reference coverage for every
    newly added integer, AMMX, and FPU family; AB fixtures for success, error,
    and documented-divergence cases; documented divergence metadata wherever the
    external oracle lacks full 68080 support; final traceability evidence back
    to the specification inventory.
  - Expected files: `examples/motorola68000/68080_*.asm`,
    `examples/reference/motorola68000/68080_*`,
    `examples/ab/motorola68000/vasm/68080/fixtures.toml`,
    `examples/ab/motorola68000/vasm/68080/**/*.asm`,
    `documentation/plans/opForge-m68000-family-68080-full-extension-implementation-plan-v0_1.md`
  - Full quality gates: `cargo fmt --all`, `cargo clippy --all-targets
    --all-features -- -D warnings`, `cargo audit`, `cargo test --workspace`,
    `make reference-test`, `make test-external-oracle`
  - Plan-compliance review evidence: `PASS` confirming fixture/reference/oracle
    changes are fully traceable to the completed instruction inventory.
  - Commit outcome: one commit delivering the final example/reference/oracle
    matrix for the full 68080 extension surface.
  - Final traceability evidence: integer and branch coverage is locked by
    `examples/motorola68000/68080_integer_addressing_matrix.asm`,
    `examples/motorola68000/68080_full_additional_surface.asm`, and the
    documented-divergence fixtures `68080-bregister-core-divergence` and
    `68080-branch-core-divergence`; FPU coverage is locked by
    `examples/motorola68000/68080_fpu_surface.asm` plus the documented
    divergences `68080-fpu-core-divergence` and
    `68080-fpu-explicit-literal-divergence`; AMMX coverage remains locked by
    the existing `68080_ammx_*` examples, references, and oracle fixtures.
  - Final validation evidence: `make reference-test` and
    `make test-external-oracle` both passed after the full 68080 example and
    fixture matrix was expanded.

## Milestones

- [x] Milestone 1: 68080 substrate and fixed-opcode AMMX surface complete
      (WI-1 through WI-3).
- [x] Milestone 2: constrained and special-form AMMX plus B-register and
      remaining integer surface complete (WI-4 through WI-7).
- [x] Milestone 3: branch and FPU surface complete (WI-8 through WI-9).
- [x] Milestone 4: regression preservation, references, oracle fixtures, and
      final traceability complete (WI-10 through WI-11).

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no plan execution before `plan-quality-reviewer` returns `PASS`
- no commit before all full quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- if full quality gates and `plan-compliance-reviewer` both pass, commit the
  slice and only then continue to the next work item
- no advancing to the next item on failed validation or failed compliance review
- checkbox updates are mandatory bookkeeping
- no scope widening beyond the source specification without first updating and
  re-reviewing the plan
- if this implementation later claims review findings are fixed, no finding may
  be marked fixed until the required finding-closure gate passes
