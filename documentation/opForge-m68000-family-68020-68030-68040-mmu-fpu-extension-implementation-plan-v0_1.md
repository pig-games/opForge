# opForge Motorola 68000 Family 68020/68030/68040 MMU and FPU Extension Implementation Plan (v0.1)

## Metadata

- Source: explicit user instruction dated `2026-03-29` and
  `documentation/opForge-m68000-family-68020-68030-68040-mmu-fpu-extension-spec-v0_1.md`
- Mode: `implementation`
- Owner: implementation agent

## Objective

Implement the assembler-facing Motorola 68020/68030/68040 MMU and FPU extension
surface defined by the source specification without widening opForge into VM
execution semantics, full PMMU support, or non-listed CPU variants.

## Activation Preconditions

- This plan is not active until
  `documentation/opForge-m68000-family-68020-68030-68040-mmu-fpu-extension-spec-v0_1.md`
  passes the spec-quality gate.
- No execution may begin until the branch-local plan-quality gate passes for
  this plan. Prefer `agents/plan-quality-orchestrator.agent.md`; at minimum,
  `agents/plan-quality-reviewer.agent.md` must return `PASS`.
- If the source specification changes materially in the `.fpu` host matrix,
  MMU support matrix, FPU instruction matrix, diagnostics contract, or scope
  boundary, update this plan and re-run the plan-quality gate before execution
  resumes.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Use the current workspace crate layout (`crates/opforge-*`) and the current
  Motorola 68000 family implementation paths; do not create parallel stacks.
- Scope is limited to assembler-facing parsing, legality, encoding,
  diagnostics, capability reporting, examples, and documentation for:
  `PFLUSH`, `.fpu none|68881|68882|68040`, the `MC68881`/`MC68882` assembler
  surface on `m68020`/`m68030`, and the integrated `m68040` FPU surface.
- Do not silently widen scope to VM execution, numeric emulation, software
  assist packages, full PMMU instruction families, `68LC040`, `68EC040`,
  `68EC030`, `68060`, or any non-listed CPU variant.
- Existing integer-only `m68000` through `m68040` behavior must remain stable
  when no FPU selector is active.
- `.cpu` alone must not enable FPU instructions; all FPU legality must remain
  explicitly tied to `.fpu`.
- Existing MMU-related `MOVEC` register legality must remain preserved exactly
  where currently shipped.
- One active work item at a time.
- Each work item must be commit-sized and end in exactly one new commit before
  the next item starts.
- No plan-driven commit is allowed until all quality gates pass and
  `agents/plan-compliance-reviewer.agent.md` returns `PASS` for the active
  slice.
- If plan-quality or plan-compliance correction loops fail three times for the
  same slice, stop and ask the user to resolve the blockage before continuing.
- The repository supply-chain safety rule remains binding: do not add, invoke,
  recommend, or otherwise touch `litellm` anywhere in this work.

## Execution Decisions

- Sequence the narrow MMU slice before broad FPU bring-up: implement `PFLUSH`
  on `m68030` first, then widen to `m68040` in the next slice.
- Render capability reporting as selector-driven FPU support plus a distinct
  minimal-MMU note so the optional FPU surface and CPU-gated MMU surface do not
  blur together.
- For execution traceability, treat conversion coverage as format-converting
  `FMOVE` forms plus `FINT` and `FINTRZ`; do not rely on vague "conversion"
  wording during slice review.
- For execution traceability, split the remaining extended FPU inventory into:
  trig or hyperbolic families (`FSIN`, `FCOS`, `FSINCOS`, `FTAN`, `FASIN`,
  `FACOS`, `FATAN`, `FSINH`, `FCOSH`, `FTANH`, `FATANH`) and exponent, log,
  extract, scale, and remainder families (`FETOX`, `FETOXM1`, `FTENTOX`,
  `FTWOTOX`, `FLOGN`, `FLOGNP1`, `FLOG10`, `FLOG2`, `FGETEXP`, `FGETMAN`,
  `FSCALE`, `FMOD`, `FREM`).
- Keep shared parsing and encoding logic in `crates/opforge-families/src/m68k/*`
  only when it is genuinely shared; keep CPU legality explicit in the per-CPU
  handlers so diagnostics remain deterministic.

## Source Traceability Keys

- `SRC-M68KMF-USER`: the user requested an implementation plan artifact that
  explicitly references the project workflow rules, small commits, quality
  gates, and `plan-compliance-reviewer` requirements
- `SRC-M68KMF-SPEC`: the source specification is the execution authority for the
  `.fpu` target matrix, MMU scope, FPU scope, diagnostics expectations,
  documentation honesty, and assembler-only boundary
- `SRC-M68KMF-STABILITY`: existing integer-only `m68000` through `m68040`
  behavior and the shipped MMU-related `MOVEC` behavior must not regress
- `SRC-M68KMF-HONESTY`: capability reporting, examples, references, and manuals
  must describe only the actually shipped MMU/FPU surface

## Work Items

- [x] `M68KMF-001`: Add `.fpu` directive infrastructure, runtime state, host
      pairing diagnostics, and explicit preservation coverage for the existing
      family id, dialect id, and shipped integer CPU identities.
  - Validation: focused registry and runtime-directive tests for `motorola68000`,
    `motorola68k`, shipped `m68000` through `m68040` CPU identities, and legal
    or illegal `.cpu` + `.fpu` pairings, plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: `.fpu none|68881|68882|68040` parses through the live
    directive path, legal CPU/FPU pairings are accepted, illegal pairings name
    both the host CPU and requested FPU target, the family id and dialect id are
    unchanged, the shipped integer CPU identities remain unchanged, and no FPU
    mnemonic or register becomes legal solely because `.cpu` changed
  - Source requirement or finding IDs: `REQ-M68KMF-001`, `REQ-M68KMF-004`,
    `REQ-M68KMF-005`, `REQ-M68KMF-006`, `REQ-M68KMF-010`,
    `AC-M68KMF-001`, `AC-M68KMF-002`, `AC-M68KMF-006`
  - Expected files: `crates/opforge-asm/src/line.rs`,
    `crates/opforge-asm/src/state.rs`,
    `crates/opforge-registry/src/registry.rs`,
    `crates/opforge-families/src/m68k/module.rs`, and any narrowly required
    diagnostic or state support files
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68KMF-001` before
    commit
  - Commit outcome: one commit that makes `.fpu` selection explicit,
    deterministic, and opt-in while preserving the existing family identity and
    integer-only surface

- [x] `M68KMF-002`: Preserve and lock down the shipped MMU-related `MOVEC`
      surface with focused regression coverage before adding new MMU behavior.
  - Validation: focused regression tests for shipped MMU-related `MOVEC`
    registers where currently supported, focused negative tests on earlier CPUs,
    plus `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, and `make test`
  - Definition of done: focused regression tests cover the shipped MMU-related
    `MOVEC` registers where currently supported, earlier CPUs continue to reject
    unsupported MMU control-register access deterministically, and no behavior
    widens beyond the already-shipped surface
  - Source requirement or finding IDs: `REQ-M68KMF-002`, `REQ-M68KMF-006`,
    `REQ-M68KMF-010`, `AC-M68KMF-004`, `AC-M68KMF-006`
  - Expected files: `crates/opforge-families/src/m68040/handler.rs`,
    `crates/opforge-families/src/m68k/operand.rs`,
    `crates/opforge-asm/src/tests.rs`, and any example or fixture file required
    to prove current MMU-related `MOVEC` stability
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68KMF-002` before
    commit
  - Commit outcome: one commit that proves the existing MMU-related `MOVEC`
    register surface remains live and non-regressing before `PFLUSH` lands

- [x] `M68KMF-003`: Implement the first new MMU slice by adding `PFLUSH` support
      on `m68030` and explicit rejection on `m68020` and earlier CPUs.
  - Validation: focused legality or encoding tests for `PFLUSH` on `m68030`,
    focused rejection tests on `m68020`, `m68010`, and `m68000`, plus
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, and `make test`
  - Definition of done: `PFLUSH` assembles on `m68030`, fails on `m68020` with
    CPU-unsupported diagnostics, remains rejected on earlier CPUs, and the broad
    PMMU families stay out of scope and explicitly illegal
  - Source requirement or finding IDs: `REQ-M68KMF-003`, `REQ-M68KMF-006`,
    `REQ-M68KMF-009`, `REQ-M68KMF-010`, `AC-M68KMF-003`, `AC-M68KMF-006`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68000/handler.rs`,
    `crates/opforge-families/src/m68010/handler.rs`,
    `crates/opforge-families/src/m68020/handler.rs`,
    `crates/opforge-families/src/m68030/handler.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68KMF-003` before
    commit
  - Commit outcome: one commit that establishes the first narrow new MMU
    compatibility slice without bundling in `m68040` or broader PMMU families

- [x] `M68KMF-004`: Widen the MMU slice to `m68040` and finish the intentional
      MMU boundary by locking negative coverage for out-of-scope PMMU families.
  - Validation: focused legality or encoding tests for `PFLUSH` on `m68040`,
    focused negative tests for `PMOVE`, `PLOAD`, `PTEST`, `PBcc`, `PDBcc`,
    `PScc`, `PTRAPcc`, `PVALID`, `PSAVE`, and `PRESTORE`, plus
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, and `make test`
  - Definition of done: `PFLUSH` also assembles on `m68040`, the MMU support
    matrix matches the spec exactly, and the excluded PMMU families all fail as
    intentionally out of scope with deterministic diagnostics
  - Source requirement or finding IDs: `REQ-M68KMF-002`, `REQ-M68KMF-003`,
    `REQ-M68KMF-006`, `REQ-M68KMF-009`, `AC-M68KMF-003`, `AC-M68KMF-004`,
    `AC-M68KMF-006`
  - Expected files: `crates/opforge-families/src/m68040/handler.rs`,
    `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-asm/src/tests.rs`, and any narrowly required MMU example or
    reference artifacts
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68KMF-004` before
    commit
  - Commit outcome: one commit that completes the spec MMU matrix while keeping
    the shipped MMU scope deliberately minimal

- [x] `M68KMF-005`: Add FPU register and mnemonic recognition with legality gates
      so FPU-disabled diagnostics work before the encode surface ships.
  - Validation: focused parser and legality tests for `FP0`-`FP7`, `FPCR`,
    `FPSR`, `FPIAR`, FPU mnemonics, and FPU-disabled or incompatible-target
    diagnostics, plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: `FP0`-`FP7`, `FPCR`, `FPSR`, and `FPIAR` are recognized
    only when a legal `.fpu` target is active, FPU mnemonics route into the live
    legality path, and disabled or incompatible configurations fail with explicit
    FPU-focused diagnostics rather than unknown-token failures
  - Source requirement or finding IDs: `REQ-M68KMF-004`, `REQ-M68KMF-005`,
    `REQ-M68KMF-006`, `REQ-M68KMF-007`, `REQ-M68KMF-008`, `REQ-M68KMF-010`,
    `AC-M68KMF-002`, `AC-M68KMF-005`, `AC-M68KMF-006`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/operand.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68020/handler.rs`,
    `crates/opforge-families/src/m68030/handler.rs`,
    `crates/opforge-families/src/m68040/handler.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68KMF-005` before
    commit
  - Commit outcome: one commit that makes the FPU surface parseable and
    diagnosable without prematurely claiming any encode family

- [x] `M68KMF-006`: Implement the external-FPU move and register-transfer slice
      for `m68020` and `m68030` under `.fpu 68881` and `.fpu 68882`.
  - Validation: focused encode or legality tests for `FMOVE`, `FMOVEM`, and FPU
    control-register transfers on `m68020` and `m68030` with `.fpu 68881` or
    `.fpu 68882`, focused negative tests for integer-only configurations, plus
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, and `make test`
  - Definition of done: the move and register-transfer families assemble on the
    external-FPU hosts, `68881` and `68882` behave identically at the
    assembler-visible surface, and the same forms remain illegal when the FPU is
    disabled
  - Source requirement or finding IDs: `REQ-M68KMF-006`, `REQ-M68KMF-007`,
    `REQ-M68KMF-010`, `AC-M68KMF-005`, `AC-M68KMF-006`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68020/handler.rs`,
    `crates/opforge-families/src/m68030/handler.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68KMF-006` before
    commit
  - Commit outcome: one commit that proves a narrow working external-FPU encode
    path before broader arithmetic families are enabled

- [x] `M68KMF-007`: Implement the external-FPU arithmetic, compare/test, and named conversion slice for `m68020` and `m68030`.
  - Validation: focused encode or legality tests for `FADD`, `FSUB`, `FMUL`, `FDIV`, `FSQRT`, `FABS`, `FNEG`, `FCMP`, `FTST`, `FINT`, `FINTRZ`, and format-converting `FMOVE` forms on external-FPU hosts, plus `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: `FADD`, `FSUB`, `FMUL`, `FDIV`, `FSQRT`, `FABS`, `FNEG`, `FCMP`, `FTST`, `FINT`, `FINTRZ`, and format-converting `FMOVE` forms assemble on legal external-FPU hosts and remain unavailable on integer-only configurations
  - Source requirement or finding IDs: `REQ-M68KMF-006`, `REQ-M68KMF-007`,
    `REQ-M68KMF-010`, `AC-M68KMF-005`, `AC-M68KMF-006`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68020/handler.rs`,
    `crates/opforge-families/src/m68030/handler.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68KMF-007` before
    commit
  - Commit outcome: one commit that completes the core external-FPU arithmetic
    slice without yet widening to the integrated `m68040` host

- [x] `M68KMF-008`: Widen the shipped core FPU surface to the integrated
      `m68040` host under `.fpu 68040`.
  - Validation: focused encode or legality tests for the shipped move,
    arithmetic, compare/test, and conversion families on `m68040` with
    `.fpu 68040`, plus focused negative tests for `.fpu 68881` and `.fpu 68882`
    on `m68040`, plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: the shipped core FPU surface assembles on `m68040` only
    when `.fpu 68040` is active, remains disabled under `.fpu none`, and the
    illegal external-FPU target pairings on `m68040` fail deterministically
  - Source requirement or finding IDs: `REQ-M68KMF-006`, `REQ-M68KMF-008`,
    `REQ-M68KMF-010`, `AC-M68KMF-002`, `AC-M68KMF-005`, `AC-M68KMF-006`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68040/handler.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68KMF-008` before
    commit
  - Commit outcome: one commit that widens the proven core FPU encode path to
    the integrated `m68040` target without bundling the remaining FPU families

- [x] `M68KMF-009`: Implement the floating-point conditional families and
      `FSAVE`/`FRESTORE` on all legal FPU targets.
  - Validation: focused encode or legality tests for `FBcc`, `FDBcc`, `FScc`,
    `FTRAPcc`, `FSAVE`, and `FRESTORE` on `.fpu 68881`, `.fpu 68882`, and
    `.fpu 68040`, plus `cargo fmt --all`,
    `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: the floating-point conditional and save or restore
    families assemble on each legal FPU target and continue to diagnose clearly
    when the FPU target is absent or incompatible
  - Source requirement or finding IDs: `REQ-M68KMF-006`, `REQ-M68KMF-007`,
    `REQ-M68KMF-008`, `REQ-M68KMF-010`, `AC-M68KMF-005`, `AC-M68KMF-006`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68020/handler.rs`,
    `crates/opforge-families/src/m68030/handler.rs`,
    `crates/opforge-families/src/m68040/handler.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68KMF-009` before
    commit
  - Commit outcome: one commit that lands the condition-code and state-frame FPU
    slice after the core movement and arithmetic surface is already proven

- [ ] `M68KMF-010`: Implement the trig and hyperbolic FPU slice on all legal FPU targets.
  - Validation: focused encode or legality tests for `FSIN`, `FCOS`, `FSINCOS`, `FTAN`, `FASIN`, `FACOS`, `FATAN`, `FSINH`, `FCOSH`, `FTANH`, and `FATANH` on `.fpu 68881`, `.fpu 68882`, and `.fpu 68040`, plus `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: `FSIN`, `FCOS`, `FSINCOS`, `FTAN`, `FASIN`, `FACOS`, `FATAN`, `FSINH`, `FCOSH`, `FTANH`, and `FATANH` assemble on each legal FPU target, and the implementation remains explicit about assembler-only support
  - Source requirement or finding IDs: `REQ-M68KMF-006`, `REQ-M68KMF-007`,
    `REQ-M68KMF-008`, `REQ-M68KMF-009`, `REQ-M68KMF-010`, `AC-M68KMF-005`,
    `AC-M68KMF-006`, `AC-M68KMF-007`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68020/handler.rs`,
    `crates/opforge-families/src/m68030/handler.rs`,
    `crates/opforge-families/src/m68040/handler.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68KMF-010` before
    commit
  - Commit outcome: one commit that lands the named trigonometric and
    hyperbolic FPU families without bundling the remaining extended-math set

- [ ] `M68KMF-011`: Implement the exponent, logarithm, extract, scale, and remainder FPU slice on all legal FPU targets.
  - Validation: focused encode or legality tests for `FETOX`, `FETOXM1`, `FTENTOX`, `FTWOTOX`, `FLOGN`, `FLOGNP1`, `FLOG10`, `FLOG2`, `FGETEXP`, `FGETMAN`, `FSCALE`, `FMOD`, and `FREM` on `.fpu 68881`, `.fpu 68882`, and `.fpu 68040`, plus `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`, `cargo audit`, and `make test`
  - Definition of done: `FETOX`, `FETOXM1`, `FTENTOX`, `FTWOTOX`, `FLOGN`, `FLOGNP1`, `FLOG10`, `FLOG2`, `FGETEXP`, `FGETMAN`, `FSCALE`, `FMOD`, and `FREM` assemble on each legal FPU target, and the implementation remains explicit about assembler-only support
  - Source requirement or finding IDs: `REQ-M68KMF-006`, `REQ-M68KMF-007`,
    `REQ-M68KMF-008`, `REQ-M68KMF-009`, `REQ-M68KMF-010`, `AC-M68KMF-005`,
    `AC-M68KMF-006`, `AC-M68KMF-007`
  - Expected files: `crates/opforge-families/src/m68k/table.rs`,
    `crates/opforge-families/src/m68k/handler.rs`,
    `crates/opforge-families/src/m68020/handler.rs`,
    `crates/opforge-families/src/m68030/handler.rs`,
    `crates/opforge-families/src/m68040/handler.rs`,
    `crates/opforge-asm/src/tests.rs`
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68KMF-011` before
    commit
  - Commit outcome: one commit that completes the named extended-math FPU
    inventory without widening into runtime semantics

- [ ] `M68KMF-012`: Ship capability reporting, examples, references, and
      user-facing documentation that reflect the minimal MMU scope and optional
      FPU scope honestly.
  - Validation: focused capability-output checks, example-assembly smoke tests,
    fail-before-update and clean-after-update reference comparison, plus
    `cargo fmt --all`, `cargo clippy --workspace -- -D warnings`,
    `cargo audit`, and `make test`
  - Definition of done: capability reporting shows the selector-driven FPU
    surface and narrow MMU notes accurately, example fixtures cover at least one
    `m68030` or `m68040` `PFLUSH` case, one external-FPU case, and one
    `m68040` integrated-FPU case, user-facing docs state the assembler-only
    boundary explicitly, and shipped references match the actual behavior
  - Source requirement or finding IDs: `REQ-M68KMF-009`, `REQ-M68KMF-010`,
    `AC-M68KMF-003`, `AC-M68KMF-005`, `AC-M68KMF-006`, `AC-M68KMF-007`
  - Expected files: `README.md`, `documentation/opForge-reference-manual.md`,
    `documentation/` artifacts required by current workflow,
    `examples/motorola68000/*`, `examples/reference/motorola68000/*`,
    `crates/opforge-cli-core/src/*`, and any capability-reporting support files
  - Plan-compliance review evidence: `PASS` from
    `agents/plan-compliance-reviewer.agent.md` citing `M68KMF-012` before
    commit
  - Commit outcome: one commit that makes the shipped MMU/FPU surface observable
    through capabilities, docs, examples, and stable references without
    overstating runtime support

## Milestones

- [x] Milestone 0: prerequisite spec remains spec-quality-approved and this plan
      passes the branch-local plan-quality gate before execution starts
- [ ] Milestone 1: selector and stability foundation complete
      (`M68KMF-001`, `M68KMF-002`)
- [ ] Milestone 2: minimal MMU scope complete (`M68KMF-003`, `M68KMF-004`)
- [x] Milestone 3: FPU parsing and legality foundation complete (`M68KMF-005`)
- [x] Milestone 4: external-FPU core movement and arithmetic complete
      (`M68KMF-006`, `M68KMF-007`)
- [x] Milestone 5: integrated `m68040` core FPU surface complete (`M68KMF-008`)
- [x] Milestone 6: FPU conditionals and save or restore complete (`M68KMF-009`)
- [ ] Milestone 7: remaining transcendental and extended-math surface complete
      (`M68KMF-010`, `M68KMF-011`)
- [ ] Milestone 8: capabilities, examples, references, and docs complete
      (`M68KMF-012`)

## Blocking Rules

- the active worktree `AGENTS.md` workflow and execution rules must be followed
  throughout execution
- no plan execution before the source spec remains valid and the plan-quality
  gate returns `PASS`
- no commit before all quality gates pass
- no commit before `agents/plan-compliance-reviewer.agent.md` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no next item may start while the current item is failing validation or is
  blocked without a plan update
- checkbox updates are mandatory bookkeeping
- if any review, quality-gate, or compliance issue reopens scope or breaks
  traceability, update the plan before continuing
- no review finding may be claimed fixed during execution unless the
  branch-local finding-closure gate passes for that finding
