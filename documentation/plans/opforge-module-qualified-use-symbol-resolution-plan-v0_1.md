<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# opForge Qualified Module Use Symbol Resolution Plan v0.1

## Metadata

- Source: User request on 2026-05-12 to plan `.use opasm.amigaos.engine`
  qualified symbol notation such as `jsr engine.sessionPass`, with module-local
  source symbols de-prefixed and library symbols internally mangled by full
  module path plus symbol name.
- Mode: implementation
- Owner: implementation agent executing under `AGENTS.md`

## Objective

Extend opForge `.use` and symbol resolution so a module import can expose public
runtime symbols through a qualifier derived from either an explicit alias or the
final module path segment. The first target behavior is:

```asm
.module opasm.amigaos.engine
    .pub
sessionPass:
    rts
.endmodule

.module main
    .use opasm.amigaos.engine
    jsr engine.sessionPass
.endmodule
```

The implementation must preserve the existing selective import behavior,
visibility rules, and CPU-family operand semantics while making qualified
imported runtime symbols usable across all supported CPU families.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- This plan must not become active until `plan-quality-reviewer` or
  `plan-quality-orchestrator` returns `PASS`.
- Execute one work item at a time; do not begin the next item until the current
  item is validated, reviewed by `plan-compliance-reviewer`, committed, and its
  checkbox state is updated.
- Keep the first implementation slice narrow: host/Rust `.use` and runtime
  symbol resolution only, not the native AmigaOS `.use` port.
- Preserve existing `.use module (item)`, `.use module (item as alias)`, and
  wildcard import semantics.
- Preserve `.pub` / `.priv` visibility enforcement for imported runtime
  symbols.
- Do not redesign tokenization unless a focused failing test proves there is no
  narrower parser or resolver change.
- Do not change CPU instruction encoding semantics beyond recognizing qualified
  imported symbols as symbol operands.
- Do not install, import, add, recommend, vendor, execute, or otherwise touch
  `litellm`.

## Version Impact

- Affected component(s): opcore `.use` parsing/import data, `opforge-asm`
  symbol resolution, CPU-family operand parsing for dotted imported symbol
  operands, documentation for module imports.
- Impact class: minor
- Owned contract: `.use` module imports and public runtime symbol resolution,
  including explicit aliases, implicit final-segment qualifiers, selective
  imports, wildcard imports, and visibility diagnostics.
- Rationale: modules become much easier to consume when source can import a
  module once and then reference public entrypoints through a stable short
  qualifier, while the assembler retains full module-path symbol names
  internally for collision-free library chunk linking.

## Work Items

- [ ] Item 1: Add implicit qualifier semantics for bare `.use module.path`
  - Source requirement or finding IDs: User requirement that `.use
    opasm.amigaos.engine` allows later `engine.symbol` references without
    requiring source-level prefixes.
  - Expected files: `crates/opforge-core/src/imports.rs`,
    `crates/opforge-types/src/symbol.rs`, focused tests in
    `crates/opforge-asm/src/tests.rs` or `crates/opforge-types/src/symbol.rs`.
  - Full quality gates: focused Cargo tests for import alias/default-alias
    behavior; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 1 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that records an implicit qualifier for
    bare `.use module.path`, detects collisions with explicit or implicit
    aliases, and leaves existing selective imports unchanged.
  - Definition of done: `.use opasm.amigaos.engine` resolves the qualifier
    `engine` in import metadata, `.use opasm.amigaos.engine as eng` still
    resolves only `eng`, and duplicate qualifier cases produce deterministic
    `.use` diagnostics.

- [ ] Item 2: Centralize qualified imported runtime symbol resolution
  - Source requirement or finding IDs: User requirement that library chunks can
    use full module path plus symbol name for internal mangling while source can
    reference imported symbols through the import qualifier.
  - Expected files: `crates/opforge-types/src/symbol.rs`,
    `crates/opforge-asm/src/line.rs`, `crates/opforge-asm/src/asmline_eval.rs`,
    and focused tests in `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: focused Cargo tests for qualified imported constants,
    labels, scalar/value symbols, private-symbol rejection, and full module path
    lookup; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 2 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that provides one shared qualified import
    resolver and routes scoped symbol lookup, scalar/value lookup, and VM bridge
    symbol evaluation through it.
  - Definition of done: `engine.sessionPass`,
    `opasm.amigaos.engine.sessionPass`, selective `sessionPass`, and explicit
    alias `eng.sessionPass` resolve consistently where applicable, with
    `.pub` / `.priv` visibility preserved.

- [ ] Item 3: Make M68K operand parsing accept qualified imported symbol operands
  - Source requirement or finding IDs: User observed `jsr engine.sessionPass`
    currently fails as an unrecognized symbol or strange addressing mode,
    depending on CPU family.
  - Expected files: `crates/opforge-asm/src/line.rs`,
    `crates/opforge-families/src/m68k/handler/operand_parsing.rs`, and focused
    M68K tests in `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: focused M68K tests for `jsr engine.sessionPass`, branch
    or data references where relevant, existing absolute-size suffix tests such
    as `label.W` / `label.L`, and `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 3 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that lets qualified imported symbols
    reach M68K encoding as symbol operands without regressing `.W` / `.L`,
    indexed register, register-pair, or special-register parsing.
  - Definition of done: a M68K module can import `opasm.amigaos.engine` and
    assemble `jsr engine.sessionPass`, while existing M68K dotted operand
    fixtures continue to pass.

- [ ] Item 4: Prove qualified imported symbol operands across supported CPU families
  - Source requirement or finding IDs: User requirement that the notation work
    for all CPU families, not just Motorola 68000.
  - Expected files: `crates/opforge-asm/src/tests.rs` and the minimum family
    parsing files required only if a focused family test exposes a real blocker.
  - Full quality gates: focused tests for representative call/jump or absolute
    reference forms on Intel 8080/8085/Z80, MOS 6502-family targets, Motorola
    6809/HD6309, Motorola 68000-family targets, and
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 4 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that either confirms the shared resolver
    is sufficient for non-M68K families or applies the smallest family-specific
    fixes for qualified imported symbol operands.
  - Definition of done: every supported family has at least one focused test
    proving a qualified imported public label can be used in an instruction or
    family-appropriate absolute reference.

- [ ] Item 5: Document qualified `.use` runtime symbol notation
  - Source requirement or finding IDs: User requirement for more usable module
    imports and library chunks with de-prefixed source symbols.
  - Expected files: `documentation/opForge-reference-manual.md`, examples under
    `examples/opcore/` or CPU-specific examples only if needed, and focused
    reference artifacts only for examples explicitly added or changed.
  - Full quality gates: documentation/example focused checks, any required
    `examples_match_reference_outputs` run for touched examples, and
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, the Item 5 slice
    summary, changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit documenting explicit aliases, implicit
    final-segment qualifiers, selective imports, full module path references,
    visibility behavior, and internal module-path symbol naming.
  - Definition of done: the reference manual explains that bare `.use a.b.c`
    exposes public runtime symbols under `c.*`, shows `jsr engine.sessionPass`,
    and clearly distinguishes qualified imports from selective unqualified
    imports.

## Milestones

- [ ] Milestone 1: Import metadata supports explicit and implicit qualifiers.
- [ ] Milestone 2: Shared symbol resolution handles qualified imported runtime
  symbols consistently.
- [ ] Milestone 3: M68K accepts `engine.sessionPass` without regressing dotted
  operand syntax.
- [ ] Milestone 4: All supported CPU families have focused qualified import
  coverage.
- [ ] Milestone 5: User-facing documentation matches the implemented behavior.

## Completion Archive

- When every checkbox in this plan is complete and the plan is no longer the
  active execution artifact, archive it with
  `scripts/workflow/archive_completed_plan.sh`.
- The archived filename must move to `documentation/plans/completed/` and end
  in `-completed-YYYY-MM-DDTHHMMSSZ.md`.
- Move the companion quality-gate sidecar with the same timestamped basename.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
