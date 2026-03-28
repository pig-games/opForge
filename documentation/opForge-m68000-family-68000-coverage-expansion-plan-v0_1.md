# opForge Motorola 68000 Coverage Expansion Plan (v0.1 follow-on)

## Metadata

- Source: explicit user instruction on 2026-03-26 to extend support for all
  remaining baseline 68000 instructions and addressing modes, including
  sufficient examples and reference fixtures, from the current repository
  state; plus explicit user instruction on 2026-03-26 to update the plan for
  the remaining true-68000 alias gaps identified from the external
  `allmodesnew.asm` comparison
- Mode: `implementation`
- Owner: implementation agent

## Objective
Complete the remaining baseline Motorola 68000 native-assembler coverage that
is still missing after the original `v0.1` delivery plan closed, using small
commit-sized slices that preserve the active family/CPU layering and example
/reference workflow. The only remaining scope in this follow-on plan is the
small baseline alias gap that still exists after the canonical coverage and
first idiomatic-alias slices landed.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Scope is limited to remaining baseline `m68000` instruction and addressing
  coverage from the current repository state; do not widen to post-68000 CPUs,
  compatibility dialects, or authoritative VM rollout work.
- For the remaining open work, scope is limited to true baseline-68000 alias
  forms only. Do not widen to 68020-style scaled-index modes, memory-indirect
  forms, base-suppression forms, or ambiguous absolute-width inference.
- Work must stay inside the existing crate-based architecture, with family-wide
  behavior in `crates/opforge-families/src/m68k/*` and CPU identity/metadata in
  `crates/opforge-families/src/m68000/*`.
- Example and reference updates must follow the repository's normal
  fail-before-update reference workflow.
- Only one work item may be active at a time, and each item must end in its own
  commit before the next item begins.

## Work Items

- [x] `M68K-X01`: Add `CHK`, `MULS`, `MULU`, `DIVS`, and `DIVU` with deterministic size and effective-address diagnostics.
  - Source requirement or finding IDs: explicit user instruction on 2026-03-26; remaining baseline arithmetic coverage from current repo state
  - Definition of done: the five opcodes assemble with correct baseline sizing rules, deterministic invalid-form diagnostics, and committed example/reference coverage
  - Validation: focused family encode tests, focused assembler byte/diagnostic
    tests, example/reference compare workflow for the new example, `cargo fmt`
    / `cargo clippy` / `cargo audit` / `make test`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-asm/src/tests.rs`,
    `examples/68000_multiply_divide_check.asm`, `examples/reference/*`
  - Full quality gates: focused family/asm tests, fail-before-update reference
    compare, reference update, clean compare, `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` against
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-X01` before commit
  - Commit outcome: one commit that lands `CHK`/`MUL*`/`DIV*` support plus its
    example/reference coverage

- [x] `M68K-X02`: Add `ADDX`, `SUBX`, `ABCD`, `SBCD`, and `CMPM`.
  - Source requirement or finding IDs: explicit user instruction on 2026-03-26; remaining baseline arithmetic/BCD coverage from current repo state
  - Definition of done: legal register and predecrement forms encode correctly, illegal operand shapes diagnose deterministically, and examples/reference fixtures cover the supported paths
  - Validation: focused family encode tests, focused assembler byte/diagnostic
    tests, example/reference workflow plus `cargo fmt` / `cargo clippy` /
    `cargo audit` / `make test`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-asm/src/tests.rs`, `examples/*`, `examples/reference/*`
  - Full quality gates: focused family/asm tests, fail-before-update reference
    compare when fixtures change, reference update, clean compare,
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` against
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-X02` before commit
  - Commit outcome: one commit that lands the remaining X/BCD arithmetic slice

- [x] `M68K-X03`: Add `ROXL`, `ROXR`, and the remaining legal memory-form shift/rotate encodings.
  - Source requirement or finding IDs: explicit user instruction on 2026-03-26; remaining baseline shift/rotate coverage from current repo state
  - Definition of done: register and legal memory forms encode correctly, and unsupported size or operand combinations diagnose deterministically
  - Validation: focused family encode tests, focused assembler byte/diagnostic
    tests, example/reference workflow plus `cargo fmt` / `cargo clippy` /
    `cargo audit` / `make test`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-asm/src/tests.rs`, `examples/*`, `examples/reference/*`
  - Full quality gates: focused family/asm tests, fail-before-update reference
    compare when fixtures change, reference update, clean compare,
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` against
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-X03` before commit
  - Commit outcome: one commit that completes the remaining baseline
    shift/rotate slice

- [x] `M68K-X04`: Add `MOVEM` and `MOVEP`, including any parser support needed for register lists.
  - Source requirement or finding IDs: explicit user instruction on 2026-03-26; remaining baseline transfer/addressing coverage from current repo state
  - Definition of done: register lists parse deterministically, legal `MOVEM` and `MOVEP` forms encode correctly, and illegal list/addressing combinations diagnose deterministically
  - Validation: focused parser/family/asm tests, example/reference workflow,
    and `cargo fmt` / `cargo clippy` / `cargo audit` / `make test`
  - Expected files: `crates/opforge-core/src/parser.rs`,
    `crates/opforge-families/src/m68k/operand.rs`,
    `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-asm/src/tests.rs`, `examples/*`, `examples/reference/*`
  - Full quality gates: focused parser/family/asm tests, fail-before-update
    reference compare when fixtures change, reference update, clean compare,
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` against
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-X04` before commit
  - Commit outcome: one commit that lands the remaining register-list and
    peripheral-transfer slice

- [x] `M68K-X05`: Expand examples/reference coverage for all shipped canonical addressing families and the new instruction groups.
  - Source requirement or finding IDs: explicit user instruction on 2026-03-26; remaining baseline addressing/example coverage from current repo state
  - Definition of done: the corpus demonstrates every canonical addressing family accepted by the 68000 parser plus the new instruction groups, and the reference workflow is green
  - Validation: focused example assembly tests, full reference workflow, and
    `cargo fmt` / `cargo clippy` / `cargo audit` / `make test`
  - Expected files: `crates/opforge-asm/src/tests.rs`, `examples/*`,
    `examples/reference/*`, and docs only if shipped-behavior claims need a
    narrow sync to stay truthful
  - Full quality gates: focused asm tests, fail-before-update reference compare,
    reference update, clean compare, `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` against
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-X05` before commit
  - Commit outcome: one commit that closes the remaining baseline coverage gap
    with honest examples/reference artifacts

- [x] `M68K-X06`: Add the remaining identity-scale baseline alias spellings for indexed effective-address forms.
  - Source requirement or finding IDs: explicit user instruction on 2026-03-26 to compare against `allmodesnew.asm`; `REQ-M68K-002`, `REQ-M68K-003`, `AC-M68K-007`
  - Definition of done: baseline indexed aliases with explicit `*1` identity scale assemble identically to their existing canonical forms for address-register and PC-relative indexed modes, including zero-displacement variants where they map to existing baseline forms
  - Validation: focused parser/family/asm alias-parity tests, negative tests proving `*2`/`*4`/`*8` and later-form syntax stay rejected, plus `cargo fmt` / `cargo clippy` / `cargo audit` / `make test`
  - Expected files: `crates/opforge-core/src/parser.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-asm/src/tests.rs`, and examples/reference files only if a
    narrow alias fixture update is needed to keep shipped behavior visible
  - Full quality gates: focused parser/family/asm tests, fail-before-update
    reference compare if fixtures change, reference update, clean compare,
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` against
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-X06` before commit
  - Commit outcome: one commit that closes the remaining baseline `*1`
    identity-scale alias gap without widening to post-68000 scaling

- [x] `M68K-X07`: Add the remaining zero-displacement PC-relative shorthand alias and sync examples/spec wording.
  - Source requirement or finding IDs: explicit user instruction on 2026-03-26 to compare against `allmodesnew.asm`; `REQ-M68K-002`, `REQ-M68K-004`, `AC-M68K-005`, `AC-M68K-007`
  - Definition of done: `(PC)` assembles identically to the existing zero-displacement PC-relative form, diagnostics stay deterministic for unsupported non-68000 shorthand forms, and the example/reference corpus shows the supported final alias set honestly
  - Validation: focused alias-parity and diagnostic tests, example/reference workflow if fixtures change, `python3 scripts/workflow/check_plan_checkboxes.py`, `cargo fmt` / `cargo clippy` / `cargo audit` / `make test`
  - Expected files: `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-asm/src/tests.rs`, `examples/*`, `examples/reference/*`,
    and the 68000 spec/docs only if shipped alias claims need a narrow sync
  - Full quality gates: focused asm tests, fail-before-update reference
    compare when fixtures change, reference update, clean compare,
    `python3 scripts/workflow/check_plan_checkboxes.py`,
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` against
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-X07` before commit
  - Commit outcome: one commit that closes the remaining strict-68000 alias
    follow-up and leaves the docs/examples truthful about the final supported
    alias set

## Milestones

- [x] Milestone 1: remaining word-sized math slice complete (`M68K-X01`)
- [x] Milestone 2: remaining arithmetic and shift/rotate slice complete
      (`M68K-X02`, `M68K-X03`)
- [x] Milestone 3: remaining transfer/addressing and corpus coverage complete
      (`M68K-X04`, `M68K-X05`)
- [x] Milestone 4: remaining strict-68000 alias parity complete (`M68K-X06`,
      `M68K-X07`)

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- if the user materially changes the target scope again, update this plan and
  re-run plan validation before continuing
