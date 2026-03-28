# opForge Motorola 68000 Family 68010/68020/68030/68040 Implementation Plan (v0.1)

## Metadata

- Source: explicit user instruction dated `2026-03-28`,
  `documentation/opForge-m68000-family-68000-cpu-extension-spec-v0_1.md`,
  `documentation/opForge-m68000-family-68000-implementation-plan-v0_1.md`, and
  `documentation/opForge-m68000-family-68000-coverage-expansion-plan-v0_1.md`
- Mode: `implementation`
- Owner: implementation agent

## Objective
Extend the shipped `m68k` family beyond the current `m68000` baseline by
adding staged, source-backed support for `m68010`, `m68020`, `m68030`, and
`m68040` without regressing the existing 68000 surface or over-claiming
later-family features that are not yet implemented.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- No plan execution may begin until `agents/plan-quality-reviewer.agent.md`
  returns `PASS` for this plan.
- Work must use the current crate layout (`crates/opforge-*`) rather than the
  older monolithic `src/` layout.
- Scope is limited to the integer or core assembler-visible `68010`, `68020`,
  `68030`, and `68040` CPU surfaces that are explicitly backed by the future
  source spec for this effort.
- Do not silently widen scope to `68EC020`, `68060`, `CPU32`,
  coprocessor/FPU/PMMU instruction sets, cache/MMU configuration directives, or
  compatibility-dialect syntax beyond what the source spec explicitly adopts.
- `m68010` must remain a baseline-addressing CPU slice unless the source spec
  explicitly says otherwise; `68020+` full-extension addressing work must not be
  backported implicitly to `m68010` or `m68000`.
- Family-common parsing, operand modeling, and encode behavior should stay in
  `crates/opforge-families/src/m68k/*` when genuinely shared; CPU-specific
  enablement and legality must remain explicit per CPU so that `m68000`,
  `m68010`, `m68020`, `m68030`, and `m68040` continue to diagnose unsupported
  forms deterministically.
- Example and reference updates must follow the repository’s fail-before-update,
  update, and clean-compare workflow.
- Each work item must end in exactly one new commit before the next item starts,
  and no item may advance on failed validation.

## Source Traceability Keys

- `SRC-M68KLINEAGE-USER`: the user requested a concrete plan for adding the
  `68010`, `68020`, `68030`, and `68040` lineage to the `68000` family on
  `2026-03-28`
- `SRC-M68KLINEAGE-BASE-FAMILY`: the current shipped `m68k` family layout and
  `m68000` baseline remain the foundation for later CPU work
- `SRC-M68KLINEAGE-BASE-SCOPE`: the current source spec explicitly excludes
  later CPUs and later-family addressing forms until separately specified
- `SRC-M68KLINEAGE-BASE-HONESTY`: examples, docs, capabilities, and diagnostics
  must match the actual shipped CPU surface without over-claiming parity

## Work Items

- [ ] `M68K-LINEAGE-001`: Author and land the source-backed
      68010/68020/68030/68040 family extension spec and instruction or
      addressing delta matrix that will govern implementation.
  - Validation: spec self-review against authoritative Motorola or NXP source
    citations, `spec-quality-reviewer` `PASS`, `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: a new spec artifact exists that distinguishes shared
    behavior from CPU-specific deltas, names the accepted 68010 and 68020+
    instruction or addressing groups, and explicitly records out-of-scope FPU,
    MMU, and coprocessor space
  - Source requirement or finding IDs: `SRC-M68KLINEAGE-USER`,
    `SRC-M68KLINEAGE-BASE-FAMILY`, `SRC-M68KLINEAGE-BASE-SCOPE`,
    `SRC-M68KLINEAGE-BASE-HONESTY`
  - Expected files:
    `documentation/opForge-m68000-family-68010-68020-68030-68040-cpu-extension-spec-v0_1.md`,
    this plan if traceability or scope wording must be tightened, and only the
    smallest related documentation cross-links needed to keep the artifact set
    coherent
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-001`
    before commit
  - Commit outcome: one commit that lands the execution source of truth for the
    later-CPU family work without making production-code claims yet

- [ ] `M68K-LINEAGE-002`: Add `m68010`, `m68020`, `m68030`, and `m68040`
      registry wiring, CPU metadata, and family capability hooks without
      enabling new syntax or instruction encodes yet.
  - Validation: focused CPU-resolution, capabilities, and metadata tests plus
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, and `make test`
  - Definition of done: `.cpu 68010`, `.cpu m68010`, `.cpu mc68010`,
    `.cpu 68020`, `.cpu m68020`, `.cpu mc68020`, `.cpu 68030`, `.cpu m68030`,
    `.cpu mc68030`, `.cpu 68040`, `.cpu m68040`, and `.cpu mc68040` resolve;
    per-CPU metadata exists; and the family layer can distinguish per-CPU
    legality without regressing `m68000`
  - Source requirement or finding IDs: `SRC-M68KLINEAGE-USER`,
    `SRC-M68KLINEAGE-BASE-FAMILY`, and the requirement IDs defined by
    `M68K-LINEAGE-001`
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
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-002`
    before commit
  - Commit outcome: one commit that makes the new CPUs discoverable and
    testable as separate targets while leaving their live ISA surfaces
    intentionally narrow

- [ ] `M68K-LINEAGE-003`: Implement the `68010` instruction and legality delta
      on top of the baseline 68000 addressing model.
  - Validation: focused `m68010` encode or legality tests for each source-spec
    `68010` instruction family, negative tests showing `m68000` rejection of
    `68010`-only forms, plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: the source-spec `68010` delta assembles through the
    live family path, `m68010` remains on the baseline effective-address model,
    and unsupported `68010` forms still fail deterministically on `m68000`
  - Source requirement or finding IDs: `SRC-M68KLINEAGE-USER`,
    `SRC-M68KLINEAGE-BASE-FAMILY`, and the `68010` requirement IDs defined by
    `M68K-LINEAGE-001`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68010/handler.rs`,
    `crates/opforge-families/src/m68010/module.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-003`
    before commit
  - Commit outcome: one commit that establishes a useful, live `m68010`
    surface before any 68020+ syntax work begins

- [ ] `M68K-LINEAGE-004`: Extend the parser and effective-address model for the
      68020+ full extension-word forms and any accepted idiomatic aliases
      defined by the source spec.
  - Validation: focused parser and operand-model tests covering scaled index,
    base displacement, outer displacement, memory indirect, and PC-relative
    full-extension forms plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: the accepted 68020+ operand families parse
    deterministically into structured family operands, later-only forms are
    rejected on `m68000` and `m68010`, and alias spellings normalize to the
    same internal representation as their canonical forms
  - Source requirement or finding IDs: `SRC-M68KLINEAGE-BASE-SCOPE`,
    `SRC-M68KLINEAGE-BASE-HONESTY`, and the addressing requirement IDs defined
    by `M68K-LINEAGE-001`
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
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-004`
    before commit
  - Commit outcome: one commit that makes the later-family addressing syntax
    structurally available without yet claiming the full later-family
    instruction delta

- [ ] `M68K-LINEAGE-005`: Implement the `68020` instruction and legality delta
      on top of the new addressing model.
  - Validation: focused `m68020` encode or legality tests for each source-spec
    instruction family, negative tests showing `m68000` and `m68010` rejection
    of `68020` forms, plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: the source-spec `68020` instruction delta assembles
    through the live family path, full-extension addressing works where legal,
    and unsupported later instructions still fail deterministically on earlier
    CPUs
  - Source requirement or finding IDs: `SRC-M68KLINEAGE-USER`,
    `SRC-M68KLINEAGE-BASE-FAMILY`, and the `68020` requirement IDs defined by
    `M68K-LINEAGE-001`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68020/handler.rs`,
    `crates/opforge-families/src/m68020/module.rs`,
    `crates/opforge-asm/src/tests.rs`, and any narrowly required family support
    files
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-005`
    before commit
  - Commit outcome: one commit that establishes a useful, live `m68020`
    surface instead of parser-only support

- [ ] `M68K-LINEAGE-006`: Implement the `68030` CPU enablement slice from the
      source-backed delta matrix while keeping non-goal MMU or coprocessor
      space rejected.
  - Validation: focused `m68030` CPU-selection and legality tests proving the
    intended delta relative to `m68020` and `m68040` plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: `m68030` accepts exactly the source-spec core surface,
    shared behavior stays centralized when appropriate, and out-of-scope MMU or
    coprocessor forms keep failing with deterministic diagnostics
  - Source requirement or finding IDs: `SRC-M68KLINEAGE-USER`,
    `SRC-M68KLINEAGE-BASE-HONESTY`, and the `68030` requirement IDs defined by
    `M68K-LINEAGE-001`
  - Expected files: `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68030/handler.rs`,
    `crates/opforge-families/src/m68030/module.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-006`
    before commit
  - Commit outcome: one commit that makes `m68030` behavior explicit and
    testable rather than merely aliasing `m68020` informally

- [ ] `M68K-LINEAGE-007`: Implement the `68040` CPU enablement and
      incompatibility slice from the source-backed delta matrix.
  - Validation: focused `m68040` legality tests for supported and unsupported
    instruction families, deterministic diagnostic checks for removed or
    restricted forms, plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: `m68040` accepts the source-spec core surface, rejects
    excluded legacy or later-family forms deterministically, and documents any
    deliberate differences from `m68020` or `m68030`
  - Source requirement or finding IDs: `SRC-M68KLINEAGE-USER`,
    `SRC-M68KLINEAGE-BASE-HONESTY`, and the `68040` requirement IDs defined by
    `M68K-LINEAGE-001`
  - Expected files: `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68040/handler.rs`,
    `crates/opforge-families/src/m68040/module.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-007`
    before commit
  - Commit outcome: one commit that makes `m68040` a real target with explicit
    differences, not a vague family alias

- [ ] `M68K-LINEAGE-008`: Add example, reference, and integration coverage for
      the shipped `68010`, `68020`, `68030`, and `68040` surfaces.
  - Validation: focused example-assembly smoke tests, fail-before-update
    reference comparison, reference update, clean reference comparison,
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, and `make test`
  - Definition of done: each newly shipped CPU has representative examples and
    synchronized `.hex` or `.lst` references, the addressing and instruction
    deltas are visible in the example corpus, and existing 68000 references
    remain stable except where an intentional shared-output change is required
  - Source requirement or finding IDs: `SRC-M68KLINEAGE-BASE-HONESTY` and the
    example or acceptance IDs defined by `M68K-LINEAGE-001`
  - Expected files: `crates/opforge-asm/src/tests.rs`, `examples/68010_*.asm`,
    `examples/68020_*.asm`, `examples/68030_*.asm`, `examples/68040_*.asm`,
    and the corresponding `examples/reference/*` artifacts
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-008`
    before commit
  - Commit outcome: one commit that makes the later-family support observable
    through stable shipped examples and references

- [ ] `M68K-LINEAGE-009`: Sync user-facing documentation, capabilities
      reporting, and release-facing notes to the shipped `68010`, `68020`,
      `68030`, and `68040` behavior.
  - Validation: documentation smoke review against real examples and CPU
    selection behavior plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: README, manual, and capabilities surfaces describe the
    actually shipped `m68010`, `m68020`, `m68030`, and `m68040` behavior
    without over-claiming unsupported coprocessor or system-programming scope
  - Source requirement or finding IDs: `SRC-M68KLINEAGE-BASE-HONESTY` and the
    documentation or acceptance IDs defined by `M68K-LINEAGE-001`
  - Expected files: `README.md`,
    `documentation/opForge-reference-manual.md`, and release-notes artifacts
    only if the repository workflow requires them
  - Full quality gates: `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, `make test`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68K-LINEAGE-009`
    before commit
  - Commit outcome: one commit that aligns user-facing docs with the actual
    shipped later-family CPU surface and closes the delivery slice

## Milestones

- [ ] Milestone 1: source spec and CPU registration foundation complete
      (`M68K-LINEAGE-001`, `M68K-LINEAGE-002`)
- [ ] Milestone 2: 68010 delta complete (`M68K-LINEAGE-003`)
- [ ] Milestone 3: 68020 parsing, addressing, and core ISA enablement complete
      (`M68K-LINEAGE-004`, `M68K-LINEAGE-005`)
- [ ] Milestone 4: 68030 and 68040 CPU deltas complete
      (`M68K-LINEAGE-006`, `M68K-LINEAGE-007`)
- [ ] Milestone 5: examples, references, and documentation complete
      (`M68K-LINEAGE-008`, `M68K-LINEAGE-009`)

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no plan execution before `plan-quality-reviewer` returns `PASS`
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- if the source spec or instruction or addressing matrix changes materially
  during execution, update this plan and re-run plan validation before
  continuing
