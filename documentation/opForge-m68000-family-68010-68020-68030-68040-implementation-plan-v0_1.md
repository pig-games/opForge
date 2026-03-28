# opForge Motorola 68000 Family 68010/68020/68030/68040 Implementation Plan (v0.1, post-spec execution)

## Metadata

- Source: explicit user instruction dated `2026-03-28`,
  `documentation/opForge-m68000-family-68000-cpu-extension-spec-v0_1.md`,
  `documentation/opForge-m68000-family-68010-68020-68030-68040-cpu-extension-spec-v0_1.md`,
  `documentation/opForge-m68000-family-68000-implementation-plan-v0_1.md`, and
  `documentation/opForge-m68000-family-68000-coverage-expansion-plan-v0_1.md`
- Mode: `implementation`
- Owner: implementation agent

## Objective
Extend the shipped `m68k` family beyond the current `m68000` baseline by
adding staged, source-backed support for the full non-MMU, non-FPU integer
instruction-set surfaces of `m68010`, `m68020`, `m68030`, and `m68040`
without regressing the existing `m68000` surface or over-claiming excluded
system-programming features.

## Activation Preconditions

- This plan is not active until the later-CPU source artifact at
  `documentation/opForge-m68000-family-68010-68020-68030-68040-cpu-extension-spec-v0_1.md`
  passes the spec-quality gate.
- If the prerequisite later-CPU spec changes the CPU-delta matrix,
  control-register matrix, accepted-addressing model, or `m68040`
  restriction set materially, update this plan and re-run plan-quality
  validation before implementation begins.
- No execution may begin until `agents/plan-quality-reviewer.agent.md` returns
  `PASS` for this plan after the prerequisite spec exists.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Work must use the current crate layout (`crates/opforge-*`) rather than the
  older monolithic `src/` layout.
- Scope is limited to the non-MMU, non-FPU integer surfaces explicitly adopted
  by the prerequisite later-CPU source spec for `m68010`, `m68020`, `m68030`,
  and `m68040`.
- Do not silently widen scope to `68EC020`, `68060`, `CPU32`, MMU, PMMU, FPU,
  coprocessor, cache-control, or compatibility-dialect syntax beyond the
  prerequisite source spec.
- `m68010` must remain a baseline-addressing CPU slice; `68020+`
  full-extension addressing work must not be backported implicitly to `m68010`
  or `m68000`.
- Family-common parsing, operand modeling, alias normalization, and shared
  encode behavior should stay in `crates/opforge-families/src/m68k/*` when
  genuinely shared; CPU-specific enablement and legality must remain explicit
  per CPU so that `m68000`, `m68010`, `m68020`, `m68030`, and `m68040`
  continue to diagnose unsupported forms deterministically.
- Example and reference updates must follow the repository’s fail-before-update,
  update, and clean-compare workflow.
- Each work item must end in exactly one new commit before the next item starts,
  and no item may advance on failed validation.

## Source Traceability Keys

- `SRC-M68KLINEAGE-USER`: the user requested a concrete implementation plan for
  the `68010`, `68020`, `68030`, and `68040` lineage under the existing
  `motorola68000` family on `2026-03-28`
- `SRC-M68KLINEAGE-BASELINE`: the shipped `m68000` source contract and live
  family layout remain authoritative unless widened explicitly by the later-CPU
  source spec
- `SRC-M68KLINEAGE-SPEC`: the later-CPU source spec is the execution authority
  for CPU ids, aliases, control-register matrices, addressing boundaries,
  instruction-family scope, and `m68040` restrictions
- `SRC-M68KLINEAGE-HONESTY`: examples, docs, capabilities, and diagnostics must
  match the actually shipped later-family surface without over-claiming
  excluded system-programming space

## Work Items

- [x] `M68K-LINEAGE-001`: Add `m68010`, `m68020`, `m68030`, and `m68040`
      registry wiring, CPU metadata, and family capability hooks without
      enabling new later-family syntax or encodes yet.
  - Validation: focused CPU-resolution, capabilities, and metadata tests plus
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, and `make test`
  - Definition of done: `.cpu 68010`, `.cpu m68010`, `.cpu mc68010`,
    `.cpu 68020`, `.cpu m68020`, `.cpu mc68020`, `.cpu 68030`, `.cpu m68030`,
    `.cpu mc68030`, `.cpu 68040`, `.cpu m68040`, and `.cpu mc68040` resolve;
    per-CPU metadata exists; and the family layer can distinguish per-CPU
    legality without regressing `m68000`
  - Source requirement or finding IDs: `REQ-M68KLINEAGE-001`,
    `REQ-M68KLINEAGE-002`, `REQ-M68KLINEAGE-003`, `AC-M68KLINEAGE-001`,
    `AC-M68KLINEAGE-002`
  - Expected files: `crates/opforge-families/src/lib.rs`,
    `crates/opforge-families/src/m68k.rs`,
    `crates/opforge-families/src/m68k/module.rs`,
    `crates/opforge-families/src/m68010.rs`,
    `crates/opforge-families/src/m68010/module.rs`,
    `crates/opforge-families/src/m68020.rs`,
    `crates/opforge-families/src/m68020/module.rs`,
    `crates/opforge-families/src/m68030.rs`,
    `crates/opforge-families/src/m68030/module.rs`,
    `crates/opforge-families/src/m68040.rs`,
    `crates/opforge-families/src/m68040/module.rs`,
    `crates/opforge-asm/src/engine.rs`, `crates/opforge-asm/src/tests.rs`,
    and any registry or capabilities files required by the current workspace
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-001`
    before commit
  - Commit outcome: one commit that makes the new CPUs discoverable and
    testable as separate targets while leaving their live ISA surfaces narrow

- [x] `M68K-LINEAGE-002`: Implement the full `m68010` non-MMU, non-FPU integer
      delta on top of the baseline `68000` addressing model.
  - Validation: focused `m68010` encode or legality tests for `BKPT`,
    `MOVEC`, `MOVES`, `MOVE.W CCR,<ea>`, and `RTD`, negative tests showing
    `m68000` rejection of `68010`-only forms, plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: the source-spec `m68010` delta assembles through the
    live family path, `m68010` remains on the baseline effective-address model,
    and unsupported `68010` forms still fail deterministically on `m68000`
  - Source requirement or finding IDs: `REQ-M68KLINEAGE-004`,
    `REQ-M68KLINEAGE-005`, `AC-M68KLINEAGE-003`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68010/handler.rs`,
    `crates/opforge-families/src/m68010/module.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-002`
    before commit
  - Commit outcome: one commit that establishes the complete source-backed
    `m68010` CPU delta before any `68020+` addressing work begins

- [x] `M68K-LINEAGE-003`: Extend the parser and operand model for core
  `68020+` full-extension addressing, excluding memory-indirect encode
  enablement.
  - Definition of done: the accepted core `68020+` operand families parse
    deterministically into structured family operands, later-only forms are
    rejected on `m68000` and `m68010`, and malformed suppression or width forms
    diagnose deterministically
  - Validation: focused parser and operand-model tests covering scaled index,
    base displacement, PC-relative full extension, base suppression, and index
    suppression plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Source requirement or finding IDs: `REQ-M68KLINEAGE-006`,
    `REQ-M68KLINEAGE-009`, `AC-M68KLINEAGE-004`
  - Expected files: `crates/opforge-core/src/parser.rs`,
    `crates/opforge-families/src/m68k/operand.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68k/module.rs`,
    `crates/opforge-families/src/m68020/module.rs`,
    `crates/opforge-families/src/m68030/module.rs`,
    `crates/opforge-families/src/m68040/module.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-003`
    before commit
  - Commit outcome: one commit that makes the core later-family full-extension
    syntax structurally available without yet claiming the full memory-indirect
    surface

- [x] `M68K-LINEAGE-004`: Extend the parser and operand model for the full
      accepted `68020+` memory-indirect, outer-displacement, alias-normalized,
      and omission-driven full-extension addressing surface.
  - Validation: focused parser and operand-model tests covering accepted
    preindexed, postindexed, outer-displacement, and alias-normalization cases
    plus `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, and `make test`
  - Definition of done: the accepted indirect and alias families normalize to
    structured family operands, canonical-versus-alias parity is demonstrated,
    and unsupported later-family forms still diagnose deterministically on
    earlier CPUs
  - Source requirement or finding IDs: `REQ-M68KLINEAGE-006`,
    `REQ-M68KLINEAGE-009`, `AC-M68KLINEAGE-004`, `AC-M68KLINEAGE-005`
  - Expected files: `crates/opforge-core/src/parser.rs`,
    `crates/opforge-families/src/m68k/operand.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68k/module.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-004`
    before commit
  - Commit outcome: one commit that completes the full `68020+` addressing
    syntax surface before broader instruction-family enablement begins

- [ ] `M68K-LINEAGE-005`: Enable the shared baseline instruction families and
      later `MOVES` operand roles on the shipped `68020+` addressing model.
  - Validation: focused encode or legality tests proving the intended shared
    baseline mnemonics and later `MOVES` forms now accept the shipped `68020+`
    addressing surface where legal, plus negative tests showing `m68000` and
    `m68010` rejection of those addressing forms, plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: the source-spec shared instruction surface and `MOVES`
    use the full shipped `68020+` addressing model where legal, and unsupported
    instruction or addressing combinations still fail deterministically on
    earlier CPUs
  - Source requirement or finding IDs: `REQ-M68KLINEAGE-006`,
    `REQ-M68KLINEAGE-009`, `AC-M68KLINEAGE-004`, `AC-M68KLINEAGE-005`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68020/handler.rs`,
    `crates/opforge-asm/src/tests.rs`, and any narrowly required family support
    files
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-005`
    before commit
  - Commit outcome: one commit that makes the full later-family addressing
    model usable by the carried-forward instruction surface before grouped
    `m68020` delta enablement begins

- [ ] `M68K-LINEAGE-006`: Implement the first grouped `m68020` instruction
      delta slice: `MOVEC` register-matrix expansion, long branches, `LINK.L`,
      `EXTB.L`, and long integer multiply or divide families.
  - Validation: focused `m68020` encode or legality tests for the grouped
    families, plus negative tests showing `m68000` and `m68010` rejection,
    plus `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, and `make test`
  - Definition of done: the first grouped `m68020` instruction families
    assemble through the live family path with the source-spec legality rules,
    including the non-MMU `MOVEC` control-register matrix, while earlier CPUs
    still reject later-only forms deterministically
  - Source requirement or finding IDs: `REQ-M68KLINEAGE-006`,
    `REQ-M68KLINEAGE-010`, `AC-M68KLINEAGE-006`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68020/handler.rs`,
    `crates/opforge-families/src/m68020/module.rs`,
    `crates/opforge-asm/src/tests.rs`, and any narrowly required family support
    files
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-006`
    before commit
  - Commit outcome: one commit that establishes the first substantial
    `m68020` later-family instruction group instead of leaving `m68020`
    parser-only

- [ ] `M68K-LINEAGE-007`: Implement the second grouped `m68020` instruction
      delta slice: `CAS`, `CAS2`, `CHK2`, `CMP2`, the bit-field family,
      `PACK`, `UNPK`, `TRAPcc`, `CALLM`, and `RTM`.
  - Validation: focused `m68020` encode or legality tests for the grouped
    families, plus negative tests showing earlier-CPU rejection, plus
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, and `make test`
  - Definition of done: the second grouped `m68020` instruction families
    assemble through the live family path with deterministic legality and
    diagnostics, and unsupported later instructions still fail deterministically
    on earlier CPUs
  - Source requirement or finding IDs: `REQ-M68KLINEAGE-006`,
    `REQ-M68KLINEAGE-010`, `AC-M68KLINEAGE-006`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68020/handler.rs`,
    `crates/opforge-families/src/m68020/module.rs`,
    `crates/opforge-asm/src/tests.rs`, and any narrowly required family support
    files
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-007`
    before commit
  - Commit outcome: one commit that completes the in-scope `m68020`
    later-family instruction surface before pure carry-forward CPUs are enabled

- [ ] `M68K-LINEAGE-008`: Implement the `m68030` CPU enablement slice defined by
      the source-backed delta matrix while keeping excluded MMU or coprocessor
      space rejected.
  - Validation: focused `m68030` CPU-selection and legality tests proving the
    intended carry-forward relative to `m68020` and deterministic rejection of
    excluded system surfaces, plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: `m68030` accepts exactly the source-spec non-MMU,
    non-FPU carry-forward surface, shared behavior stays centralized where
    appropriate, and excluded MMU or coprocessor forms keep failing with
    deterministic diagnostics
  - Source requirement or finding IDs: `REQ-M68KLINEAGE-007`,
    `REQ-M68KLINEAGE-010`, `AC-M68KLINEAGE-007`
  - Expected files: `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68030/handler.rs`,
    `crates/opforge-families/src/m68030/module.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-008`
    before commit
  - Commit outcome: one commit that makes `m68030` behavior explicit and
    testable rather than merely aliasing `m68020` informally

- [ ] `M68K-LINEAGE-009`: Implement the `m68040` CPU enablement slice,
      including `MOVE16` and the explicit `m68040` removal or restriction set.
  - Validation: focused `m68040` legality tests for `MOVE16`, supported and
    unsupported later-family instruction families, deterministic diagnostic
    checks for `CALLM`, `RTM`, and `MOVEC CAAR` rejection, plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: `m68040` accepts the source-spec carried-forward
    integer surface, accepts `MOVE16`, rejects explicit `m68040` restrictions
    deterministically, and keeps documented differences from `m68020` or
    `m68030` explicit
  - Source requirement or finding IDs: `REQ-M68KLINEAGE-008`,
    `REQ-M68KLINEAGE-010`, `AC-M68KLINEAGE-008`
  - Expected files: `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68040/handler.rs`,
    `crates/opforge-families/src/m68040/module.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-009`
    before commit
  - Commit outcome: one commit that makes `m68040` a real target with explicit
    additions and removals, not a vague family alias

- [ ] `M68K-LINEAGE-010`: Add example, reference, and integration coverage for
      the shipped `m68010` and `m68020` surfaces, including full `68020+`
      addressing and the grouped later instruction families.
  - Validation: focused example-assembly smoke tests, fail-before-update
    reference comparison, reference update, clean reference comparison,
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, and `make test`
  - Definition of done: `m68010` and `m68020` each have representative examples
    and synchronized `.hex` or `.lst` references, their shipped addressing and
    instruction deltas are visible in the example corpus, and existing `m68000`
    references remain stable except where an intentional shared-output change is
    required
  - Source requirement or finding IDs: `REQ-M68KLINEAGE-011`,
    `REQ-M68KLINEAGE-012`, `AC-M68KLINEAGE-010`
  - Expected files: `crates/opforge-asm/src/tests.rs`, `examples/68010_*.asm`,
    `examples/68020_*.asm`, and the corresponding `examples/reference/*`
    artifacts
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-010`
    before commit
  - Commit outcome: one commit that makes the first two later-family CPU
    surfaces observable through stable shipped examples and references

- [ ] `M68K-LINEAGE-011`: Add example, reference, and integration coverage for
      the shipped `m68030` and `m68040` surfaces, including `m68040`
      restrictions and `MOVE16`.
  - Validation: focused example-assembly smoke tests, fail-before-update
    reference comparison, reference update, clean reference comparison,
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, and `make test`
  - Definition of done: `m68030` and `m68040` each have representative examples
    and synchronized `.hex` or `.lst` references, their shipped deltas are
    visible in the example corpus, and earlier-family reference outputs remain
    stable except where an intentional shared-output change is required
  - Source requirement or finding IDs: `REQ-M68KLINEAGE-011`,
    `REQ-M68KLINEAGE-012`, `AC-M68KLINEAGE-010`
  - Expected files: `crates/opforge-asm/src/tests.rs`, `examples/68030_*.asm`,
    `examples/68040_*.asm`, and the corresponding `examples/reference/*`
    artifacts
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-011`
    before commit
  - Commit outcome: one commit that makes the later shipped CPU surfaces
    observable through stable examples and references without bundling all four
    CPUs into one fixture sweep

- [ ] `M68K-LINEAGE-012`: Sync user-facing documentation, capabilities
      reporting, and release-facing notes to the shipped `m68010`, `m68020`,
      `m68030`, and `m68040` behavior.
  - Validation: documentation smoke review against real examples and CPU
    selection behavior plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: README, manual, and capabilities surfaces describe the
    actually shipped later-family behavior without over-claiming MMU, PMMU,
    FPU, coprocessor, or cache-control scope
  - Source requirement or finding IDs: `REQ-M68KLINEAGE-011`,
    `REQ-M68KLINEAGE-012`, `REQ-M68KLINEAGE-013`, `AC-M68KLINEAGE-011`
  - Expected files: `README.md`,
    `documentation/opForge-reference-manual.md`, and release-notes artifacts
    only if the repository workflow requires them
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-012`
    before commit
  - Commit outcome: one commit that aligns user-facing docs with the actual
    shipped later-family CPU surface and closes the delivery slice

## Milestones

- [x] Milestone 0: prerequisite later-CPU spec artifact landed and
      spec-quality-approved before implementation execution starts
- [x] Milestone 1: CPU registration foundation complete (`M68K-LINEAGE-001`)
- [x] Milestone 2: `m68010` delta complete (`M68K-LINEAGE-002`)
- [ ] Milestone 3: full `68020+` parsing and addressing complete
      (`M68K-LINEAGE-003`, `M68K-LINEAGE-004`, `M68K-LINEAGE-005`)
- [ ] Milestone 4: grouped `m68020` instruction families complete
      (`M68K-LINEAGE-006`, `M68K-LINEAGE-007`)
- [ ] Milestone 5: `m68030` carry-forward and `m68040` restriction slices
      complete (`M68K-LINEAGE-008`, `M68K-LINEAGE-009`)
- [ ] Milestone 6: examples, references, and documentation complete
      (`M68K-LINEAGE-010`, `M68K-LINEAGE-011`, `M68K-LINEAGE-012`)

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no execution of this plan before the prerequisite later-CPU spec exists and
  the spec-quality gate returns `PASS`
- no plan execution before `plan-quality-reviewer` returns `PASS`
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- if the prerequisite later-CPU spec or its instruction, control-register,
  or addressing matrix changes materially during execution, update this plan
  and re-run plan validation before continuing
