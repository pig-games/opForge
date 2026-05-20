<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# Plan: opForge Qualified Module Use, Section Mapping, and Selective Inclusion Implementation v0.1

## Metadata

- Source: `documentation/opForge-module-qualified-use-section-mapping-spec-v0_1.md`,
  derived from
  `documentation/plans/opforge-module-qualified-use-symbol-resolution-plan-v0_1.md`
  and
  `documentation/architecture/opforge-module-qualified-use-section-mapping-concept-spec-v0_1.md`.
- Mode: implementation
- Owner: implementation agent executing under `AGENTS.md`
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.

## Goal

Implement the concrete v0.1 module composition contract in narrow,
commit-sized slices: qualified `.use` namespace binding, shared imported-symbol
resolution, CPU-family operand acceptance, selected-root metadata,
logical-section map metadata, selective reachable-unit inclusion, integrated
executable output-policy integration, unsupported library/object diagnostics,
and user-facing documentation.

The first implementation path is host/Rust opForge. Native AmigaOS assembly
ports are explicitly out of scope until the Rust behavior is complete.

## Version Impact

- Affected component(s): opcore `.use` parsing/import metadata,
  `opforge-types` symbol identity and visibility helpers, `opforge-asm` line
  parsing/resolution/evaluation, CPU-family operand parsers, section model,
  module graph/linking/output policy code, examples, and reference
  documentation.
- Impact class: minor
- Owned contract: qualified module imports, selected root imports, logical
  section maps, dependency-driven reachable-unit inclusion, integrated
  executable packaging policy, and unsupported library/object diagnostics.
- Rationale: reusable modules need concise qualified source references,
  consumer-owned layout, and selective inclusion so native opForge, AmigaOS, C64
  OS, and future target libraries can share modules without long global symbol
  names or duplicated source architectures.

## Work Items

- [ ] Item 1: Finalize import namespace metadata
  - Source requirement or finding IDs: Spec goals for explicit aliases, implicit
    final-segment qualifiers, direct selective imports, duplicate diagnostics,
    and `.pub` / `.priv` preservation.
  - Expected files: `crates/opforge-core/src/imports.rs`,
    `crates/opforge-types/src/symbol.rs`, focused tests in
    `crates/opforge-core` and/or `crates/opforge-types`.
  - Full quality gates: focused Cargo tests for import alias/default-alias and
    duplicate diagnostics; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, Item 1 summary,
    changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that records explicit and implicit
    qualified namespace bindings, detects collisions deterministically, and
    leaves existing direct selective import metadata unchanged.
  - Definition of done: `.use opasm.amigaos.engine` binds `engine`,
    `.use opasm.amigaos.engine as eng` binds only `eng`, direct selective imports
    still bind selected names directly, and duplicate qualifiers diagnose
    deterministically.

- [ ] Item 2: Centralize qualified imported symbol resolution
  - Source requirement or finding IDs: Spec behavioral contract for alias,
    implicit qualifier, full module-path, selective, wildcard, and private-symbol
    lookup forms.
  - Expected files: `crates/opforge-types/src/symbol.rs`,
    `crates/opforge-asm/src/line.rs`,
    `crates/opforge-asm/src/asmline_eval.rs`, focused tests in
    `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: focused Cargo tests for public label/constant/value
    resolution, full module path lookup, explicit alias lookup, implicit
    qualifier lookup, direct selective lookup, deterministic unresolved
    diagnostics when full-path lookup has no imported module-path match,
    deterministic ambiguity diagnostics when more than one full-path split is
    possible, and private-symbol rejection; `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, Item 2 summary,
    changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that provides one shared imported-symbol
    resolver and routes scoped symbol lookup, scalar/value lookup, and VM bridge
    symbol evaluation through it.
  - Definition of done: `engine.sessionPass`,
    `opasm.amigaos.engine.sessionPass`, `eng.sessionPass`, direct `sessionPass`
    where selectively imported, and wildcard-compatible forms resolve
    consistently while private symbols remain inaccessible.

- [ ] Item 3: Unblock M68K qualified imported symbol operands
  - Source requirement or finding IDs: Spec acceptance criterion that public
    qualified imported symbols work as operands, with prior observed
    `jsr engine.sessionPass` failure as the first concrete M68K case.
  - Expected files: `crates/opforge-asm/src/line.rs`,
    `crates/opforge-families/src/m68k/handler/operand_parsing.rs`, and focused
    M68K tests in `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: focused M68K tests for `jsr engine.sessionPass`,
    absolute symbol references where relevant, existing M68K dotted syntax such
    as `label.W` / `label.L`, indexed registers, register pairs, and
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, Item 3 summary,
    changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that lets qualified imported symbols
    reach M68K encoders as symbol operands without regressing dotted suffix,
    indexed register, register-pair, or special-register parsing.
  - Definition of done: M68K can assemble a public qualified imported label in a
    call/absolute operand and all existing dotted M68K operand fixtures still
    pass.

- [ ] Item 4: Prove cross-family qualified symbol operands
  - Source requirement or finding IDs: Spec acceptance criterion that public
    qualified imported symbols work as operands for every supported CPU family.
  - Expected files: focused tests in `crates/opforge-asm/src/tests.rs`; family
    operand parsing files only if focused tests expose blockers.
  - Full quality gates: focused tests for representative call/jump or absolute
    reference forms on Intel 8080/8085/Z80, MOS 6502-family targets, Motorola
    6809/HD6309, Motorola 68000-family targets, and
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, Item 4 summary,
    changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that either proves the shared resolver is
    sufficient for non-M68K families or applies the smallest family-specific
    fixes exposed by the focused tests.
  - Definition of done: every supported CPU family has focused proof that a
    qualified imported public symbol can be used in at least one
    family-appropriate operand form.

- [ ] Item 5: Store selected-root metadata
  - Source requirement or finding IDs: Spec goals for `.use module (symbol)` and
    `.use module (symbol) as alias` selecting roots independently from direct
    source references.
  - Expected files: module/import metadata in `crates/opforge-core`, assembler
    module graph or linking code in `crates/opforge-asm`, focused tests near the
    existing module/use tests.
  - Full quality gates: focused tests proving selected-root metadata is recorded
    for direct and qualified selected imports, qualified selected roots remain
    qualified by default, direct selective imports remain direct, and no
    reachability or output inclusion is attempted in this slice;
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, Item 5 summary,
    changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that records selected roots in import or
    module metadata without changing binary inclusion yet.
  - Definition of done: root-composition source can express
    `.use opforge.cli.entry (start)` and the selected-root set is available to a
    later reachability pass.

- [ ] Item 6: Parse and validate logical section contracts and maps
  - Source requirement or finding IDs: Spec behavioral contract for
    `.section ..., logical`, `.use ... map { logical -> concrete }`, missing-map
    diagnostics, and kind/capability compatibility.
  - Expected files: opcore parser/import/section metadata files,
    `crates/opforge-types` section types, assembler diagnostics, focused tests
    for logical section and map syntax.
  - Full quality gates: focused Cargo tests for logical section declarations,
    `.use` clause ordering with maps, deterministic rejection of reordered or
    duplicate `.use` clauses, deterministic rejection of per-item aliases or
    wildcard selections combined with a module qualifier, map parsing, duplicate
    map entries, unknown logical sections, undeclared concrete map-target
    diagnostics that name both the missing target section and the relevant import
    map entry, explicit no-default-map metadata, compatible
    code/data/bss/zp maps, incompatible kind diagnostics where both sides are
    known, and
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, Item 6 summary,
    changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that stores logical section contracts and
    validates import-side map syntax and static compatibility before output
    emission.
  - Definition of done: reusable modules can declare logical sections, consumers
    can map them to concrete sections, `.use` accepts only the v0.1 clause
    ordering, undeclared concrete map-target diagnostics name both the missing
    target section and the relevant import map entry, and invalid static maps
    fail with deterministic diagnostics. Reachability-backed missing-map
    diagnostics are deferred to Item 7.

- [ ] Item 7: Build reachable-unit inclusion over the module graph
  - Source requirement or finding IDs: Spec invariant that module availability is
    separate from binary inclusion and acceptance criteria for dependency-driven
    selective inclusion, selected roots, v0.1 unit granularity, and explicit
    missing-map diagnostics.
  - Expected files: assembler/module graph construction, symbol reference
    tracking, unit/reachability types in `crates/opforge-asm` and
    `crates/opforge-types`, focused graph tests.
  - Full quality gates: focused tests for referenced qualified symbols including
    their dependencies, selected roots including dependencies, unreferenced public
    exports excluded from the reachable set, deterministic handling of cycles,
    private references rejected, v0.1 top-level-symbol unit boundaries, missing
    explicit map diagnostics for reachable logical sections, and
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, Item 7 summary,
    changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that computes reachable units from
    selected roots and symbol references and associates reachable units with
    resolved section-map placement.
  - Definition of done: the assembler computes a deterministic reachable unit set
    from selected roots and references, excludes public but unused material from
    that set, and reports missing explicit maps for reachable logical sections.

- [ ] Item 8: Integrate executable output policy and unsupported library diagnostics
  - Source requirement or finding IDs: Spec output-policy contract for integrated
    executables, unresolved symbols, and explicit diagnostics for unsupported
    library/object output policies in v0.1.
  - Expected files: output format policy code for generic assembler output and
    existing host/Rust executable paths, hunk output code where applicable, and
    focused tests for executable policy plus unsupported library/object requests.
  - Full quality gates: focused tests for integrated executable unresolved-symbol
    errors, reachable-unit emission through section maps, unreferenced public
    exports excluded from emitted integrated output, explicit diagnostics
    for hunklib/C64 OS library/object policy requests that are not implemented
    in v0.1, and `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, Item 8 summary,
    changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit that applies reachable-unit and
    section-map results during integrated executable output emission and rejects
    unsupported library/object policies explicitly.
  - Definition of done: supported integrated executable output emits only
    reachable mapped units, and unsupported library/object output requests fail
    with deterministic diagnostics instead of silently changing policy.

- [ ] Item 9: Document the qualified module composition model
  - Source requirement or finding IDs: Spec goals and acceptance criteria for
    user-facing qualified imports, selected roots, section mapping, selective
    inclusion, and output policy.
  - Expected files: `documentation/opForge-reference-manual.md`, examples under
    `examples/` only if needed, and reference artifacts only for changed
    examples.
  - Full quality gates: documentation/example focused checks, required reference
    output checks for any changed examples, and
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: before commit, run
    `plan-compliance-reviewer` with `AGENTS.md`, this plan, Item 9 summary,
    changed files, and validation output; require `PASS`.
  - Commit outcome: exactly one commit documenting explicit aliases, implicit
    qualifiers, direct selective imports, selected roots, logical section maps,
    selective inclusion, output policies, and visibility behavior.
  - Definition of done: the reference manual explains the complete v0.1 model
    with examples for `jsr engine.sessionPass`, root selection, and section
    mapping.

## Blocking Rules

- no commit before all quality gates pass
- `plan-compliance-reviewer` must return `PASS` before commit
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- archive completed plans with `scripts/workflow/archive_completed_plan.sh`
- do not install, import, add, recommend, vendor, execute, or otherwise touch
  `litellm`
- keep CPU/family/dialect-specific semantics out of generic VM, workflow, CLI,
  and shared type paths
