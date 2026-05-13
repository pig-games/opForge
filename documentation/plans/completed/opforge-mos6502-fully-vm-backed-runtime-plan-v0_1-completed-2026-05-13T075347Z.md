<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# MOS 6502 Fully VM-Backed Runtime Migration Plan

## Metadata

- Source: user request: make MOS 6502 fully VM-backed and unmistakable for native 68020 implementation agents
- Mode: migration
- Owner: implementation agent

## Objective

Move the MOS 6502 instruction assembly path from "VM-backed but Rust
family-assisted" to "fully VM-backed by default" for MOS 6502 family source
lines.

For this plan, "fully VM-backed" means:

- source operands are classified into VM selector input shapes without calling
  `families::mos6502::MOS6502FamilyHandler` or matching
  `families::mos6502::FamilyOperand` in the live MOS 6502 VM path
- candidate selection is driven by package/model selector data and generic VM
  selector machinery, not by MOS 6502 family-specific Rust helpers
- final byte emission remains the existing VM program execution path keyed by
  scoped owner, mnemonic, and mode key
- compatibility with existing MOS 6502 outputs is proven by focused tests and
  the full Rust quality gate

This plan is intentionally MOS 6502-only. Agents implementing AmigaOS/68020 or
other native targets must treat this as a boundary marker: do not copy,
reintroduce, or extend CPU/family-specific Rust operand classification into new
VM-backed paths. New target work should add package/model data and generic VM
selector behavior instead.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Keep scope to MOS 6502 family instruction source-line assembly. Do not
  migrate Intel8080/Z80, M6800/M6809/HD6309, M65816-specific behavior, or
  Motorola 68000/68020 behavior in this plan.
- Preserve the current public default of `ExecutionMode::Vm` for assembly APIs.
  This plan changes the implementation behind the MOS 6502 VM path, not the
  public execution-mode API.
- Do not remove non-VM Rust fallback code until the fully VM-backed MOS 6502
  path is tested and the plan-compliance reviewer passes for that work item.
- Do not add new MOS 6502 CPU/family-specific Rust selector logic as a
  shortcut. Any new MOS 6502 mode coverage required by this plan belongs in
  package/model selector descriptors, generic selector-input conversion, or
  generic VM operand-plan handling.
- Keep every work item commit-sized. Each work item must end in exactly one new
  commit before the next begins.

## Version Impact

- Affected component(s): `opforge-vm`, `opforge-asm`, MOS 6502 runtime model
  package generation, MOS 6502 VM runtime tests
- Impact class: minor
- Owned contract: MOS 6502 instruction assembly under `ExecutionMode::Vm`
- Rationale: The current MOS 6502 instruction path is VM-backed for final encode
  but still uses Rust family operand parsing and `FamilyOperand` shape mapping
  before candidates reach the VM. This migration removes that dependency for
  the default MOS 6502 VM path so future native-target agents can follow a
  clearly package/VM-backed pattern.

## Work Items

- [x] Item 1: Freeze the MOS 6502 VM-backed boundary in tests
  - Source requirement or finding IDs: user request; scan finding that
    `encode_instruction_from_exprs` currently reaches
    `MOS6502FamilyHandler::parse_operands` and `FamilyOperand` shape matching.
  - Expected files: `crates/opforge-vm/src/runtime_tests.rs`,
    possibly `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: focused MOS 6502 VM runtime tests; then
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: record `plan-compliance-reviewer` `PASS`
    before committing the item.
  - Commit outcome: one commit adding explicit regression coverage that proves
    MOS 6502 VM assembly behavior remains the target while the implementation
    is migrated.
  - Definition of done: tests name the intended contract in plain terms:
    MOS 6502 VM instruction source lines must assemble through VM selector and
    VM encode behavior, and future work must not satisfy the tests by adding
    new MOS 6502 Rust candidate-selection shortcuts.

- [x] Item 2: Introduce a generic selector-input model for VM source operands
  - Source requirement or finding IDs: current `SelectorInput` is populated from
    `families::mos6502::FamilyOperand`; the replacement must be package/VM
    facing.
  - Expected files: `crates/opforge-vm/src/execution_model/selector_bridge.rs`,
    `crates/opforge-vm/src/execution_model/selector_encoding.rs`,
    `crates/opforge-vm/src/execution_model.rs`.
  - Full quality gates: focused selector-input unit tests; then
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: record `plan-compliance-reviewer` `PASS`
    before committing the item.
  - Commit outcome: one commit adding a CPU-neutral selector input structure
    and generic conversion helpers that do not import or match MOS 6502 family
    operand types.
  - Definition of done: generic selector input can represent the MOS 6502
    shapes currently used by package selectors: implied, accumulator,
    immediate, direct, direct indexed forms, indirect forms, pair direct,
    forced forms, and the existing long/stack shapes without depending on
    `families::mos6502`.

- [x] Item 3: Move MOS 6502 source operand classification into VM-owned logic
  - Source requirement or finding IDs: source operands currently pass through
    `MOS6502FamilyHandler::parse_operands` before VM selector lookup.
  - Expected files: `crates/opforge-vm/src/execution_model/selector_bridge.rs`,
    optionally a new small VM-owned helper module under
    `crates/opforge-vm/src/execution_model/`.
  - Full quality gates: focused tests for each migrated MOS 6502 shape; then
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: record `plan-compliance-reviewer` `PASS`
    before committing the item.
  - Commit outcome: one commit replacing live MOS 6502 VM-path calls to
    `MOS6502FamilyHandler::parse_operands` with VM-owned classification from
    portable `Expr` operands to selector input.
  - Definition of done: the MOS 6502 resolver no longer imports
    `MOS6502FamilyHandler` or `families::mos6502::FamilyOperand` for live
    source-expression candidate selection.

- [x] Item 4: Drive MOS 6502 candidate selection only from package selector data
  - Source requirement or finding IDs: package `ModeSelectorDescriptor` already
    carries `shape_key`, `mode_key`, `operand_plan`, `priority`,
    `unstable_widen`, and `width_rank`; the MOS 6502 path must rely on this
    data rather than Rust family mode selection.
  - Expected files: `crates/opforge-vm/src/execution_model/selector_bridge.rs`,
    `crates/opforge-vm/src/execution_model/selector_encoding.rs`,
    `crates/opforge-vm/src/runtime_model_core.rs`,
    `crates/opforge-vm/src/builder.rs` only if selector metadata gaps are
    found.
  - Full quality gates: focused tests for zero-page/absolute widening, branch
    offsets, forced addressing, and candidate priority; then
    `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: record `plan-compliance-reviewer` `PASS`
    before committing the item.
  - Commit outcome: one commit making MOS 6502 candidate selection data-driven
    through the runtime model.
  - Definition of done: changing MOS 6502 selector behavior requires changing
    package/model selector data or generic selector evaluation, not adding a
    MOS 6502-specific Rust branch.

- [x] Item 5: Seal the default MOS 6502 VM path against Rust-family fallback
  - Source requirement or finding IDs: MOS 6502 `ExecutionMode::Vm` must fail
    loudly on missing VM/package coverage instead of silently depending on the
    native family handler for candidate selection.
  - Expected files: `crates/opforge-asm/src/asmline_instruction.rs`,
    `crates/opforge-vm/src/execution_model/encoding_bridge.rs`,
    `crates/opforge-vm/src/runtime_tests.rs`,
    possibly `crates/opforge-asm/src/tests.rs`.
  - Full quality gates: focused MOS 6502 assembly tests through public VM
    execution APIs; then `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: record `plan-compliance-reviewer` `PASS`
    before committing the item.
  - Commit outcome: one commit ensuring MOS 6502 default VM assembly does not
    call Rust family candidate selection on the live path.
  - Definition of done: a missing MOS 6502 VM selector/program produces a VM
    runtime diagnostic, not a native-family fallback; all existing supported
    MOS 6502 examples that are in scope still assemble with matching bytes.

- [x] Item 6: Add guardrails for future native target agents
  - Source requirement or finding IDs: user request that AmigaOS/68020 native
    implementation agents can clearly see what to implement without being
    tricked into reintroducing CPU/family-specific code.
  - Expected files: targeted source comments or test names only where useful,
    likely `crates/opforge-vm/src/execution_model/selector_bridge.rs`,
    `crates/opforge-vm/src/runtime_tests.rs`, and a short note in the closure
    report for this plan when executed.
  - Full quality gates: `scripts/workflow/run_rust_quality_gate.sh`.
  - Plan-compliance review evidence: record `plan-compliance-reviewer` `PASS`
    before committing the item.
  - Commit outcome: one commit adding durable guardrails that identify MOS 6502
    as the completed fully VM-backed pattern and reject family-specific Rust
    shortcuts in the tested path.
  - Definition of done: future agents can inspect the tests/source guardrail
    and see that fully VM-backed means package/model selector data plus generic
    VM selector logic, not target-specific Rust operand resolvers.

## Milestones

- [x] Milestone 1: MOS 6502 VM-backed contract is pinned by tests.
- [x] Milestone 2: MOS 6502 selector input no longer depends on
  `families::mos6502` operand enums.
- [x] Milestone 3: MOS 6502 candidate selection is package/model driven.
- [x] Milestone 4: MOS 6502 default VM assembly path is sealed against
  native-family fallback.
- [x] Milestone 5: future-target guardrails are in place for native
  AmigaOS/68020 work.

## Execution Evidence

- Implementation landed as one vertical migration slice after user approval to
  proceed with formal close-out.
- Items 1 and 6: `crates/opforge-vm/src/runtime_tests.rs` now pins VM selector
  source operand behavior and includes a guardrail rejecting
  `families::mos6502`, `MOS6502FamilyHandler`, and `FamilyOperand` shortcuts in
  the VM selector path.
- Items 2 and 3: `crates/opforge-vm/src/execution_model/selector_bridge.rs`,
  `selector_encoding.rs`, and `execution_model.rs` now use a VM-owned selector
  input model and force enum built from source `Expr` operands.
- Item 4: MOS 6502 candidate selection now flows through existing
  package/model selector descriptors and generic selector evaluation, with no
  MOS 6502 family parser in the live VM expression path.
- Item 5: `crates/opforge-asm/src/asmline_instruction.rs` routes `m6502` and
  `m65c02` source-line instructions through VM expression encoding before host
  family operand parsing, and missing selector coverage reports
  `VM runtime selector missing for <MNEMONIC>`.
- M65816-specific behavior remains intentionally outside the public VM-first
  assembler branch for this plan scope.
- Validation evidence:
  - `bash scripts/workflow/run_plan_workflow.sh --check-once --mode migration
    documentation/plans/opforge-mos6502-fully-vm-backed-runtime-plan-v0_1.md
    "user request: make MOS 6502 fully VM-backed and unmistakable for native
    68020 implementation agents"` passed.
  - Focused MOS 6502 VM/ASM regression tests passed, including VM expression
    selector tests, missing-selector diagnostics, parity corpus checks, and
    65816 guard regression checks.
  - `scripts/workflow/run_rust_quality_gate_summary.sh` passed and executed the
    canonical Rust quality gate.
- Plan-compliance review evidence:
  - `agents/plan-compliance-reviewer.agent.md` returned `PASS` for the
    completed migration slice. The reviewed scope is limited to MOS 6502 VM
    selector/source-line migration, focused test guardrails, completed plan
    bookkeeping, and archiving; unrelated pre-existing dirty native 68000 files
    are excluded from the commit.
- Commit outcome:
  - one focused local commit records the completed MOS 6502 fully VM-backed
    runtime migration and archived plan evidence.

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
