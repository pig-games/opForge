# opForge Motorola68000 Family Expression VM Authoritative Rollout Plan v0.1

## Metadata

- Source: user request on 2026-05-08 to continue after the completed
  `documentation/plans/opforge-expression-vm-rust-seam-retirement-migration-plan-v0_1.md`
  by auditing the remaining intentional non-authoritative expression seams and
  turning that audit into the next execution-ready rollout plan; derived from a
  live audit of `crates/opforge-vm/src/rollout.rs`,
  `crates/opforge-vm/src/vm_opcore.rs`,
  `crates/opforge-asm/src/asmline_eval.rs`, the existing Motorola 68000-family
  staged-parity tests, and the active family contracts in
  `documentation/opForge-m68000-family-68010-68020-68030-68040-cpu-extension-spec-v0_1.md`,
  `documentation/opForge-m68000-family-68080-extension-spec-v0_1.md`, and
  `documentation/opForge-m68000-family-68080-full-extension-spec-v0_1.md`.
- Mode: migration
- Owner: opForge implementation agent

## Objective

Promote the `motorola68000` family from staged expression-parser and
expression-eval verification to an authoritative VM-backed expression path
without reopening the completed covered-family migration work or broadening
into unrelated runtime/package rollout.

For this plan, the family scope explicitly includes `m68000`, `m68010`,
`m68020`, `m68030`, `m68040`, and the full `m68080` surface already defined by
the 68080 extension specifications. The plan covers expression parser/eval
behavior for covered expression subranges that appear inside those family
forms; it does not redefine the non-expression instruction/encode ownership of
the later-CPU and 68080 surfaces.

The target end state is:

- covered Motorola 68000-family expression parsing on the assembler hot path
  uses the VM parser contract by default rather than host parser compatibility
- covered Motorola 68000-family expression evaluation uses contract-aware VM
  eval by default for both instruction and non-instruction callers where
  covered semantics already exist
- full `m68080` integer, AMMX, Apollo-gated, and `fpu 68080` surfaces remain
  in scope for this family rollout wherever they contain covered expression
  subranges; the rollout must not silently carve 68080 back down to a smaller
  representative slice
- rollout controls for staged/host override behavior remain explicit,
  deterministic, and test-covered
- permanent host-owned expression boundaries remain explicit and narrow:
  repetition-side-table member/index semantics, string-encoding registry
  ownership, provisional pass-1 unresolved-symbol placeholder evaluation, and
  compatibility-only out-of-scope nodes such as calls/placeholders until a
  later plan promotes them
- unknown families and any family not explicitly promoted by this plan remain
  in staged verification by default

## Current Implementation Facts

- `crates/opforge-vm/src/rollout.rs` marks `motorola68000` as
  `StagedVerification` for runtime/package rollout, expression eval rollout,
  and expression parser rollout, while `mos6502`, `intel8080`, and
  `motorola6800` are already authoritative.
- `crates/opforge-vm/src/vm_opcore.rs` resolves assembler expression-parser
  rollout through `portable_expr_parser_runtime_enabled_for_family(...)`, but
  currently passes empty opt-in/force-host lists, so the assembler path has no
  explicit parser-side rollout override surface.
- `crates/opforge-asm/src/asmline_eval.rs` only takes the default VM eval path
  when the family runtime/package rollout is authoritative and expression eval
  is enabled, so staged families such as `motorola68000` still fall back to
  host AST evaluation for non-instruction callers.
- `crates/opforge-asm/src/asmline_instruction.rs` already has stricter VM expr
  resolution hooks for instruction encoding paths, so some Motorola 68000-
  family expression work can already exercise VM-backed resolution even while
  the family runtime default remains staged.
- Existing Motorola 68000-family staged evidence includes
  `motorola68000_vm_runtime_operand_expr_parse_survives_core_parser_failpoint`
  in `crates/opforge-asm/src/tests.rs` and
  `motorola68000_tokenizer_vm_staged_corpus_matches_host_for_example_lines` in
  `crates/opforge-vm/src/runtime_tests.rs`, but there is no dedicated plan
  artifact yet for authoritative expression promotion.
- The assembler test suite already contains broad `m68080_...` coverage for the
  full shipped 68080 integer, AMMX, Apollo-gated, and FPU surfaces, but the
  family expression-rollout plan did not previously state that those full 68080
  forms must remain inside the expression migration boundary.
- The permanent host carveouts documented in
  `documentation/vm-boundary-protocol-v1.md` remain binding and must not be
  widened silently by this rollout.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Execute one work item at a time, and end each work item in exactly one new
  commit before starting the next item.
- Do not widen this plan into full `motorola68000` runtime/package rollout
  unless a narrower expression-only change proves impossible; any such
  dependency must be stated explicitly in the work item that needs it.
- Keep PRVM/opasm authoritative for Motorola 68000-family operand-shape
  parsing, including wrapped, postfix, tuple, predecrement, postincrement, and
  other addressing-form ownership that is intentionally outside pure
  expression VM coverage.
- Preserve the full 68080 family contracts from
  `documentation/opForge-m68000-family-68080-extension-spec-v0_1.md` and
  `documentation/opForge-m68000-family-68080-full-extension-spec-v0_1.md`.
  This plan must not narrow the 68080 surface back to a representative subset
  just because expression rollout work is the immediate focus.
- No silent fallback: once a Motorola 68000-family expression slice is marked
  authoritative by this plan, covered parser/eval failures must fail
  deterministically rather than delegating back to generic host parsing or host
  AST evaluation.
- Preserve explicit permanent host-owned semantics for repetition-side-table
  member/index access, string-encoding registry ownership, and provisional
  pass-1 unresolved-symbol placeholder rules unless a later scoped plan says
  otherwise.
- Do not promote call expressions, placeholder nodes, or other currently
  compatibility-only value nodes into authoritative VM ownership under this
  plan.
- Unknown families remain staged verification by default after this plan.
- The plan must not become active until `Plan Quality Reviewer` or
  `Plan Quality Orchestrator` returns `PASS`.

## Source Requirement IDs

- `SR-M68K-EXPR-ROLLOUT-SCOPE`: the plan must promote only the
  `motorola68000` family expression parser/eval boundary, including full
  `m68080`, and must not reopen completed covered-family work.
- `SR-M68K-EXPR-GATE-INDEPENDENCE`: Motorola 68000-family expression rollout
  controls must be explicit enough to stage parity work without accidental
  dependence on unrelated runtime/package defaults.
- `SR-M68K-EXPR-PARSER-AUTHORITY`: covered Motorola 68000-family expression
  parsing must use the VM parser path by default once promoted.
- `SR-M68K-EXPR-EVAL-AUTHORITY`: covered Motorola 68000-family expression
  evaluation must use VM-backed contract-aware evaluation by default once
  promoted.
- `SR-M68K-68080-FULL-SURFACE`: full `m68080` integer, AMMX, Apollo-gated,
  and `fpu 68080` expression-bearing forms remain in scope for this family
  rollout wherever they contain covered expression subranges.
- `SR-M68K-HOST-BOUNDARIES`: permanent host-owned semantics must stay narrow,
  explicit, and tested.
- `SR-M68K-OVERRIDE-DISCIPLINE`: any opt-in or force-host escape hatch must be
  explicit, deterministic, and must not recreate silent fallback.
- `SR-M68K-DETERMINISM`: diagnostics, budgets, unresolved-symbol handling, and
  repeated-run behavior must remain deterministic.

## Version Impact

- Affected component(s): `crates/opforge-vm`, `crates/opforge-asm`,
  Motorola 68000-family runtime/assembler tests including full `m68080`
  coverage, and expression/runtime boundary documentation.
- Impact class: staged-family rollout migration for the Motorola 68000
  family expression parser/evaluator hot path.
- Owned contract: `motorola68000` family expression parsing and evaluation at
  the assembler/runtime boundary.
- Rationale: the covered-family seam-retirement work is complete, and the
  remaining intentional expression compatibility seam is the staged Motorola
  68000-family parser/eval path, including full `m68080`, plus its explicit
  host escape hatches.

## Architecture Direction For This Plan

This plan resolves the remaining staged Motorola 68000-family expression
boundary in one direction:

- expression parser/eval rollout must become explicit and independently
  testable for `motorola68000`
- the full `m68080` extension specs remain authoritative for which family
  lines, forms, and directives belong in the parity and promotion evidence for
  this rollout
- parser promotion and eval promotion should happen in separate slices so each
  step can prove parity and deterministic-failure behavior before changing the
  default mode
- the plan should prefer narrow rollout-control changes over broad runtime
  package promotion whenever expression-authoritative behavior can be achieved
  without reopening unrelated family-runtime work
- permanent host carveouts stay explicit and documented rather than implicit
  through compatibility fallback

If later work wants full `motorola68000` runtime/package authority, broader
family rollout, or promotion of compatibility-only value nodes, that must be
handled in a separate plan after this narrower expression rollout lands.

## Work Items

- [x] Item 1 - Surface and decouple Motorola 68000-family expression rollout controls
  - Source requirement or finding IDs: `SR-M68K-EXPR-GATE-INDEPENDENCE`,
    `SR-M68K-OVERRIDE-DISCIPLINE`, `SR-M68K-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/rollout.rs`
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-asm/src/runtime_config.rs`
    - `crates/opforge-asm/src/line.rs`
    - `crates/opforge-asm/src/asmline_eval.rs`
    - focused Motorola 68000-family rollout-control tests in
      `crates/opforge-vm` and `crates/opforge-asm`
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` filters covering parser/eval rollout-control
      behavior for `motorola68000`
    - focused `cargo test -p asm` filters covering assembler-side
      opt-in/force-host behavior for `motorola68000`
    - focused `cargo test -p asm m68080_ -- --nocapture` filters covering the
      full 68080 surface, including `.apollo`, `.fpu`, and 68080-only
      expression-bearing forms
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the rollout-control decoupling slice
    summary, changed files, and validation logs before committing.
  - Commit outcome: one commit that makes Motorola 68000-family expression
    parser/eval rollout independently controllable for parity work without
    silently tying authoritative expression behavior to unrelated
    runtime/package defaults or excluding full `m68080` coverage.
  - Definition of done:
    - Motorola 68000-family expression parser/eval rollout can be exercised
      under explicit staged controls before default promotion
    - no new implicit host fallback surface is introduced
    - full `m68080` surfaces still have explicit rollout and regression
      coverage where their lines contain covered expression subranges
    - runtime/package family default remains staged unless a narrower change is
      impossible and explicitly justified

- [x] Item 2 - Expand Motorola 68000-family staged parser parity and rejection coverage
  - Source requirement or finding IDs: `SR-M68K-EXPR-PARSER-AUTHORITY`,
    `SR-M68K-HOST-BOUNDARIES`, `SR-M68K-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-asm/src/tests.rs`
    - any focused Motorola 68000-family expression corpus helpers needed by
      those tests, including full `m68080` expression-bearing sources
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm motorola68000_tokenizer_vm_staged_corpus_matches_host_for_example_lines -- --nocapture`
    - focused `cargo test -p vm` filters for Motorola 68000 EXVM parser parity
      and deterministic unsupported-form behavior
    - `cargo test -p asm motorola68000_vm_runtime_operand_expr_parse_survives_core_parser_failpoint -- --nocapture`
    - focused `cargo test -p asm` Motorola 68000 parser-path filters covering
      out-of-scope nodes and failpoint behavior
    - focused `cargo test -p asm m68080_ -- --nocapture` filters covering full
      68080 expression-bearing integer, AMMX, Apollo-gated, and FPU lines
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the staged-parser-parity slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: one commit that proves the Motorola 68000-family staged
    parser VM path matches current host behavior for the audited expression
    corpus, including full `m68080` expression-bearing lines, and reports
    deterministic failures for still-out-of-scope forms.
  - Definition of done:
    - Motorola 68000-family expression parser parity evidence exists for the
      audited expression corpus
    - full `m68080` expression-bearing integer, AMMX, Apollo-gated, and
      `fpu 68080` lines are represented in the readiness evidence rather than
      left implicit outside the family rollout claim
    - call expressions, placeholders, and non-expression operand-shape forms
      remain explicit non-goals with deterministic diagnostics
    - promotion readiness gaps are visible in tests rather than implicit in
      rollout tables

- [x] Item 3 - Promote Motorola 68000-family expression parser rollout to authoritative
  - Source requirement or finding IDs: `SR-M68K-EXPR-PARSER-AUTHORITY`,
    `SR-M68K-OVERRIDE-DISCIPLINE`, `SR-M68K-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/rollout.rs`
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` Motorola 68000 parser-rollout filters
    - focused `cargo test -p asm` Motorola 68000 parser integration filters
    - focused `cargo test -p asm m68080_ -- --nocapture` filters covering full
      68080 parser-promotion behavior for expression-bearing forms
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the parser-promotion slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: one commit that flips the Motorola 68000-family expression
    parser rollout from staged verification to authoritative for covered
    expression ranges, including full `m68080`, without reopening PRVM/opasm
    operand-shape ownership.
  - Definition of done:
    - covered Motorola 68000-family expression parsing no longer relies on host
      parser compatibility by default on the assembler hot path
    - full `m68080` expression-bearing forms participate in the authoritative
      family parser claim under their existing family and CPU legality rules
    - unsupported compatibility-only nodes fail deterministically rather than
      silently falling back to host parsing
    - documented override behavior remains explicit and test-covered

- [x] Item 4 - Expand Motorola 68000-family evaluation parity across instruction and non-instruction callers
  - Source requirement or finding IDs: `SR-M68K-EXPR-EVAL-AUTHORITY`,
    `SR-M68K-EXPR-GATE-INDEPENDENCE`, `SR-M68K-HOST-BOUNDARIES`,
    `SR-M68K-DETERMINISM`.
  - Expected files:
    - `crates/opforge-asm/src/asmline_eval.rs`
    - `crates/opforge-asm/src/asmline_instruction.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p asm` Motorola 68000 filters covering instruction
      expressions, data/layout/output directives, and unresolved/unstable symbol
      behavior under staged VM eval controls
    - focused `cargo test -p vm` Motorola 68000 eval-path filters covering
      budgets, scalar-boundary failures, and pass1/pass2 behavior
    - focused `cargo test -p asm m68080_ -- --nocapture` filters covering full
      68080 integer, AMMX, Apollo-gated, and `fpu 68080` expression-bearing
      callers under staged VM eval controls
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the eval-parity slice summary, changed
    files, and validation logs before committing.
  - Commit outcome: one commit that proves VM eval parity and deterministic
    failure behavior for the audited Motorola 68000-family instruction and
    non-instruction callers, including full `m68080` expression-bearing forms,
    before default promotion.
  - Definition of done:
    - Motorola 68000-family VM eval parity evidence exists for covered
      instruction and non-instruction callers
    - full `m68080` expression-bearing integer, AMMX, Apollo-gated, and
      `fpu 68080` lines are included in the eval-readiness evidence
    - pass1/pass2 unresolved, unstable, and finalized symbol handling remains
      deterministic
    - permanent host carveouts remain explicit rather than hidden in generic
      host AST fallback

- [x] Item 5 - Promote Motorola 68000-family expression eval rollout to authoritative
  - Source requirement or finding IDs: `SR-M68K-EXPR-EVAL-AUTHORITY`,
    `SR-M68K-HOST-BOUNDARIES`, `SR-M68K-OVERRIDE-DISCIPLINE`,
    `SR-M68K-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/rollout.rs`
    - `crates/opforge-asm/src/asmline_eval.rs`
    - `crates/opforge-asm/src/asmline_instruction.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p asm` Motorola 68000 eval-promotion filters
    - focused `cargo test -p vm` Motorola 68000 eval-promotion filters
    - focused `cargo test -p asm m68080_ -- --nocapture` filters covering full
      68080 expression-bearing promotion behavior
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the eval-promotion slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: one commit that makes Motorola 68000-family VM expression
    eval the default behavior for covered semantics, including full `m68080`,
    while preserving explicit `FORCE_HOST`, pass-1 placeholder,
    string-encoding, and repetition-side-table boundaries.
  - Definition of done:
    - covered Motorola 68000-family expression callers use VM eval by default
      where covered semantics already exist
    - full `m68080` expression-bearing forms remain inside that authoritative
      family claim rather than relying on a separate compatibility lane
    - pass-2 failures no longer silently retry generic host AST evaluation
    - compatibility-only value nodes and permanent host carveouts remain
      explicit and test-covered

- [x] Item 6 - Refresh boundary docs and close the Motorola 68000-family rollout plan
  - Source requirement or finding IDs: `SR-M68K-EXPR-ROLLOUT-SCOPE`,
    `SR-M68K-HOST-BOUNDARIES`, `SR-M68K-OVERRIDE-DISCIPLINE`,
    `SR-M68K-DETERMINISM`.
  - Expected files:
    - `documentation/vm-boundary-protocol-v1.md`
    - `documentation/opforge-assembler-vm-path-guide-v0_1.md`
    - `documentation/libopforge-developer-guide.md`
    - this plan file for checkbox bookkeeping
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p asm` Motorola 68000 parser/eval promotion filters
    - focused `cargo test -p vm` Motorola 68000 parser/eval promotion filters
    - focused `cargo test -p asm m68080_ -- --nocapture` filters covering the
      final full-68080 family expression boundary state
    - `scripts/workflow/run_rust_quality_gate.sh`
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-motorola68000-expression-vm-authoritative-rollout-plan-v0_1.md`
    - `make workflow-gate`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the documentation-and-closeout slice
    summary, changed files, and validation logs before committing.
  - Commit outcome: one commit that updates the boundary docs to describe the
    new Motorola 68000-family authoritative expression defaults, explicitly
    including full `m68080`, preserves the remaining host carveouts, and
    records final plan bookkeeping.
  - Definition of done:
    - documentation matches the live Motorola 68000-family expression rollout
      state, including full `m68080`
    - the remaining permanent host boundaries and override controls are explicit
    - final gates for the rollout are recorded and green

## Milestones

- [x] Milestone 1 - Motorola 68000-family expression rollout controls are
  explicit and independently testable, including full `m68080`
- [x] Milestone 2 - Motorola 68000-family expression parser promotion
  readiness is proven and the parser path becomes authoritative
- [x] Milestone 3 - Motorola 68000-family expression eval promotion readiness
  is proven and the eval path becomes authoritative
- [x] Milestone 4 - Boundary docs and rollout bookkeeping match the final state

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping