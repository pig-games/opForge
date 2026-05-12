<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->

# Direct EXVM v2 to EXPR Program Emission Follow-Through Plan

## Metadata

- Source: explicit user instruction on 2026-05-11, "update the plan to the
  full scope of recently committed exvm/pr updates," using the prior version
  of this plan, the current implementation in `crates/opforge-vm`,
  `crates/opforge-core`, `crates/opforge-asm`, and
  `native/motorola68000/amigaos/`, plus the recently committed EXVM/PRVM
  slices `e17e2bef`, `a158e2f0`, `e81fed09`, `a87cbbfd`, `32dcc622`,
  `0f91a924`, `acf6dcd4`, `b10ab5d0`, `5ea7e9fb`, `ce5520e1`, `b8ba74c1`, and
  `9c337d9e`, and the governed follow-on plans
  `documentation/plans/opforge-expression-vm-rust-seam-retirement-migration-plan-v0_1.md`
  and
  `documentation/plans/opforge-motorola68000-expression-vm-authoritative-rollout-plan-v0_1.md`.
- Mode: migration
- Owner: opForge implementation agent

## Objective

Record the full scope of the recently committed EXVM/PRVM follow-through work,
and narrow the remaining open work to the specific direct-runtime gap that is
still not landed.

The committed path on `main` now reaches portable EXPR execution through
direct lowering helpers for the covered authoritative grammar and through
authoritative EXVM/EXPR rollout on the assembler hot path, including Motorola
68000-family rollout and native PRVM expression-request plumbing.

The remaining architectural target for this plan is still:

```text
tokens
  -> EXVM v2 parser bytecode
  -> PortableExprProgram
  -> EXPR evaluator
```

but the current implementation does not yet achieve that target directly inside
the EXVM v2 runtime. Instead, it currently uses a hybrid shape:

```text
tokens
  -> EXVM v2 parser bytecode
  -> Rust Expr
  -> direct lowering helpers in vm_opcore for covered forms
     or compatibility compile_core_expr_to_portable_program(...)
  -> PortableExprProgram
  -> EXPR evaluator
```

This updated plan therefore serves two purposes:

- capture the now-committed EXVM/PRVM scope that has already landed on `main`
- define the remaining work needed to move direct `PortableExprProgram`
  emission into the EXVM v2 runtime itself rather than relying on post-parse
  `Expr` lowering helpers in `vm_opcore`

## Current Implementation Facts

- `crates/opforge-vm/src/vm_opcore.rs` now attempts direct portable program
  lowering before compatibility compilation through four landed helpers:
  `try_compile_direct_leaf_expression_program_for_assembler(...)`,
  `try_compile_direct_scalar_expression_program_for_assembler(...)`,
  `try_compile_direct_structural_expression_program_for_assembler(...)`, and
  `try_compile_direct_member_index_expression_program_for_assembler(...)`.
- `crates/opforge-vm/src/vm_opcore.rs` exposes
  `parse_expression_program_for_assembler(...)` and
  `compile_expression_program_with_parser_vm_opt_in_for_assembler(...)`, so
  portable program-returning entry points already exist on the assembler side.
- `crates/opforge-vm/src/exvm_v2_runtime.rs` still executes EXVM v2 by
  building Rust `Expr` nodes directly in the opcode handlers for
  `BuildIdentifier`, `BuildNumber`, `BuildCurrentAddress`, `BuildUnary`,
  `BuildBinary`, `BuildTernary`, and the structural builders. The backend
  abstraction proposed by the original plan has not landed yet.
- `crates/opforge-core/src/expr_vm.rs` now supports the direct lowering slices
  that were committed on top of the original scalar-only plan, including the
  covered scalar, structural, and member/index forms already promoted by the
  seam-retirement work.
- `crates/opforge-asm/src/asmline_conditionals.rs`,
  `crates/opforge-asm/src/line.rs`,
  `crates/opforge-asm/src/asmline_directives_text.rs`,
  `crates/opforge-asm/src/repetition.rs`, and
  `crates/opforge-asm/src/asmline_eval.rs` have already been migrated off the
  generic host AST-eval default for the covered authoritative path, and the
  covered-family fallback behavior has already been retired.
- `crates/opforge-vm/src/rollout.rs` now marks `motorola68000` authoritative
  for both expression parser and expression eval rollout.
- `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm` now carries the
  native PRVM expression-request/resume and statement-expression metadata flow,
  including `PRVM_STATUS_EXPR_REQUEST`, `PRVM_ROUTE_EXPR_REQUEST_SIZE`,
  `PRVM_RESULT_OPERAND_EXPR_SLOT`, and persisted statement-expression metadata
  load/store helpers.
- The remaining open seam is therefore narrower than the original plan: the
  covered authoritative path works, but EXVM v2 itself still produces `Expr`
  internally before the landed direct-lowering helpers take over.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- The plan must not become active until `Plan Quality Reviewer` or
  `Plan Quality Orchestrator` returns `PASS`.
- Execute one work item at a time, and end each work item in exactly one new
  commit before starting the next item.
- Full Rust quality gates are mandatory before each Rust-touching commit; use
  `scripts/workflow/run_rust_quality_gate.sh` as the canonical full gate and
  add focused tests for the active slice.
- Keep the existing Rust `Expr` backend working throughout the remaining open
  work; it remains the compatibility, debugging, and reference backend until
  runtime-backend emission fully replaces the post-parse helper seam.
- Treat the recently committed scope as in-bounds for this updated plan:
  covered scalar lowering, structural lowering, member/index lowering with the
  explicit repetition carveout, caller migration, covered fallback retirement,
  Motorola 68000-family authoritative parser/eval rollout, and native PRVM
  expression-request/metadata plumbing.
- Do not widen this plan into native EXVM execution, native EXPR execution,
  call-expression promotion, placeholder promotion, or new non-expression
  operand-shape ownership.
- Keep the explicit permanent host boundaries unchanged unless a later scoped
  plan promotes them: repetition-side-table member/index semantics,
  string-encoding registry ownership, and compatibility-only out-of-scope
  nodes.
- No silent semantic drift: the remaining direct-runtime emission work must
  preserve the behavior already locked by the committed EXVM/EXPR, assembler,
  Motorola 68000 rollout, and native PRVM tests.

## Source Requirement IDs

- `SR-COMMITTED-DIRECT-LOWERING`: the plan must reflect the already committed
  direct lowering slices for covered leaf, scalar, structural, and member/index
  forms.
- `SR-COMMITTED-CALLER-MIGRATION`: the plan must reflect the already committed
  caller migrations and covered fallback retirement on the assembler hot path.
- `SR-COMMITTED-M68K-ROLLOUT`: the plan must reflect the already committed
  Motorola 68000-family authoritative parser/eval rollout state.
- `SR-COMMITTED-NATIVE-PRVM`: the plan must reflect the already committed
  native PRVM expression-request/resume and statement-expression metadata flow.
- `SR-EXVM-BACKEND-ABSTRACTION`: EXVM parser control flow must still be split
  from output representation inside `exvm_v2_runtime.rs` itself.
- `SR-RUNTIME-DIRECT-EMISSION`: the remaining work must let EXVM v2 emit
  `PortableExprProgram` directly for the already-covered authoritative forms,
  not only through post-parse helper lowering.
- `SR-COMPAT-AST-BACKEND`: the Rust `Expr` output path must remain available as
  a compatibility/debugging backend.
- `SR-EQUIVALENCE-LOCK`: the final runtime-backend emission path must preserve
  the behavior locked by the existing EXVM/EXPR, assembler, Motorola 68000,
  and native PRVM parity evidence.
- `SR-DOC-PORTABILITY`: documentation must continue to state that
  `PortableExprProgram` is the canonical portable tape and that native hosts are
  not required to reproduce Rust AST lowering when the direct path is fully
  available.

## Version Impact

- Affected component(s): `crates/opforge-vm`, `crates/opforge-core`,
  `crates/opforge-asm`, Motorola 68000-family rollout controls/tests, native
  PRVM AmigaOS harness code, and expression/runtime boundary documentation.
- Impact class: migration follow-through that records committed authoritative
  EXVM/EXPR and PRVM work, then narrows the remaining runtime gap.
- Owned contract: EXVM v2 parser-runtime output behavior and its handoff into
  portable EXPR evaluation across the covered authoritative assembler and PRVM
  surfaces.
- Rationale: the direct path is mostly realized downstream of EXVM v2 itself,
  but the runtime still hard-codes `Expr` construction internally.

## Architecture Direction For This Plan

This updated plan resolves the architecture in one direction:

- treat the recently committed EXVM/PRVM work as landed scope, not as future
  speculation
- keep the downstream direct-lowering, caller-migration, and rollout gains in
  place
- move the remaining direct-portable emission work upward into the EXVM v2
  runtime so build events can target multiple backends directly
- once runtime-backend emission exists for the already-covered forms, collapse
  or narrow the now-redundant `try_compile_direct_*_expression_program_for_assembler(...)`
  seam in `vm_opcore`
- keep byte-for-byte program identity as a desirable property for simple cases,
  but treat evaluation/result parity and deterministic diagnostics as the
  required contract

## Work Items

- [x] Item 1 - Land direct covered lowering below the EXVM runtime seam
  - Source requirement or finding IDs: `SR-COMMITTED-DIRECT-LOWERING`.
  - Expected files:
    - `crates/opforge-core/src/expr_vm.rs`
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Land direct leaf, scalar, structural, and member/index lowering helpers
      below the EXVM runtime seam so covered authoritative forms can reach
      portable EXPR execution without generic AST compilation on the hot path.
    - Preserve the explicit repetition-side-table member/index carveout.
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` EXVM/EXPR contract filters
    - focused `cargo test -p opcore --lib -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: recorded in the committed follow-on plan
    artifacts that tracked the executed slices.
  - Commit outcome: completed by `e17e2bef`, `a158e2f0`, `e81fed09`, and
    `a87cbbfd`.
  - Definition of done:
    - covered leaf, scalar, structural, and member/index forms have direct
      portable lowering on the assembler path
    - the EXVM runtime still produces `Expr`, but the downstream lowering seam
      is narrower and authoritative for covered forms

- [x] Item 2 - Migrate covered assembler callers and retire covered fallback behavior
  - Source requirement or finding IDs: `SR-COMMITTED-CALLER-MIGRATION`.
  - Expected files:
    - `crates/opforge-asm/src/asmline_conditionals.rs`
    - `crates/opforge-asm/src/line.rs`
    - `crates/opforge-asm/src/asmline_directives_text.rs`
    - `crates/opforge-asm/src/repetition.rs`
    - `crates/opforge-asm/src/asmline_eval.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `documentation/vm-boundary-protocol-v1.md`
    - `documentation/opforge-assembler-vm-path-guide-v0_1.md`
  - Implementation slice:
    - Move covered conditionals, assignments, text directives, and repetition
      callers onto contract-aware VM-backed evaluation helpers.
    - Retire covered-family host fallback behavior while preserving explicit
      permanent host boundaries.
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p asm` caller-path filters
    - focused `cargo test -p vm` integration filters
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: recorded in the committed follow-on plan
    artifacts that tracked the executed slices.
  - Commit outcome: completed by `32dcc622`, `0f91a924`, and `acf6dcd4`.
  - Definition of done:
    - covered callers no longer rely on generic host AST evaluation or covered
      fallback by default
    - permanent host carveouts remain explicit and documented

- [x] Item 3 - Promote authoritative EXVM/EXPR rollout and native PRVM bridge follow-through
  - Source requirement or finding IDs: `SR-COMMITTED-M68K-ROLLOUT`,
    `SR-COMMITTED-NATIVE-PRVM`.
  - Expected files:
    - `crates/opforge-vm/src/rollout.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `native/motorola68000/amigaos/prvm/prvm_interpreter.asm`
    - `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`
  - Implementation slice:
    - Promote Motorola 68000-family parser/eval rollout to authoritative.
    - Resume native PRVM expression requests and persist statement-expression
      metadata through the native CLI harness.
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` Motorola 68000 rollout filters
    - focused `cargo test -p asm` Motorola 68000 and native PRVM filters
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: recorded in the committed follow-on plan
    artifacts that tracked the executed slices.
  - Commit outcome: completed by `b10ab5d0`, `5ea7e9fb`, `ce5520e1`,
    `b8ba74c1`, and `9c337d9e`.
  - Definition of done:
    - `motorola68000` parser/eval hot paths are authoritative
    - native PRVM expression-request/resume and statement-expression metadata
      persistence are in place

- [x] Item 4 - Refactor EXVM v2 output behind a runtime backend abstraction
  - Source requirement or finding IDs: `SR-EXVM-BACKEND-ABSTRACTION`,
    `SR-COMPAT-AST-BACKEND`.
  - Expected files:
    - `crates/opforge-vm/src/exvm_v2_runtime.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Introduce an internal EXVM output abstraction inside
      `exvm_v2_runtime.rs` so parser control flow and output representation are
      no longer coupled.
    - Move the current Rust `Expr` construction behavior into a dedicated AST
      backend without changing parse-to-`Expr` compatibility behavior.
    - Include the already-covered structural and member/index builders in the
      abstraction surface, not only the original scalar opcodes.
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm exvm_ -- --nocapture`
    - focused `cargo test -p vm motorola68000_tokenizer_vm_staged_corpus_matches_host_for_example_lines -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `Plan Compliance Reviewer` with the
    active `AGENTS.md`, this plan path, the runtime-backend-abstraction slice
    summary, changed files, and validation logs before committing.
  - Commit outcome: one commit that isolates EXVM parser control flow from the
    emitted representation while preserving the Rust `Expr` compatibility path.
  - Execution status:
    - implementation landed in `crates/opforge-vm/src/exvm_v2_runtime.rs` and
      `crates/opforge-vm/src/runtime_tests.rs` on 2026-05-11
    - focused `cargo test -p vm exvm_ -- --nocapture` passed
    - focused `cargo test -p vm motorola68000_tokenizer_vm_staged_corpus_matches_host_for_example_lines -- --nocapture` passed
    - `scripts/workflow/run_rust_quality_gate_summary.sh` passed and executed
      the canonical Rust quality gate
    - `Plan Compliance Reviewer` returned PASS for this slice on 2026-05-11
    - bookkeeping closed and ready for the single focused Item 4 commit
  - Definition of done:
    - EXVM v2 runtime no longer hard-codes Rust `Expr` construction in its core
      opcode handlers.
    - Existing public parse-to-`Expr` entry points still behave the same.
    - No direct portable-program backend is required yet for this item to land.

- [x] Item 5 - Add a direct `PortableExprProgram` backend for the already-covered authoritative forms
  - Source requirement or finding IDs: `SR-RUNTIME-DIRECT-EMISSION`,
    `SR-EXVM-BACKEND-ABSTRACTION`, `SR-EQUIVALENCE-LOCK`.
  - Expected files:
    - `crates/opforge-vm/src/exvm_v2_runtime.rs`
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-core/src/expr_vm.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Add a `PortableExprProgram` backend that consumes EXVM build events
      directly for the forms already covered by the committed direct-lowering
      helpers: leaf, scalar, structural, and member/index forms subject to the
      explicit carveouts.
    - Reuse the existing opcode-version, symbol-interning, stack-depth, and
      operator-mapping behavior already locked by the committed helper-based
      lowering work.
    - Keep number parsing behavior aligned with the currently authoritative
      portable path.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p opcore --lib -- --nocapture`
    - focused `cargo test -p vm exvm_ -- --nocapture`
    - focused `cargo test -p vm` filters covering direct program-returning
      assembler paths
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `Plan Compliance Reviewer` with the
    active `AGENTS.md`, this plan path, the runtime-direct-emission slice
    summary, changed files, and validation logs before committing.
  - Commit outcome: one commit that proves EXVM v2 can emit portable EXPR
    programs directly for the already-covered authoritative forms without going
    through post-parse helper lowering first.
  - Execution status:
    - implementation landed in `crates/opforge-core/src/expr_vm.rs`,
      `crates/opforge-vm/src/exvm_v2_runtime.rs`,
      `crates/opforge-vm/src/vm_opcore.rs`, and
      `crates/opforge-vm/src/runtime_tests.rs` on 2026-05-11
    - focused `cargo test -p vm exvm_ -- --nocapture` passed
    - focused `cargo test -p opcore --lib -- --nocapture` passed
    - `scripts/workflow/run_rust_quality_gate_summary.sh` passed and executed
      the canonical Rust quality gate
    - `Plan Compliance Reviewer` returned PASS for the implementation slice and
      required only bookkeeping closure before commit on 2026-05-11
    - bookkeeping closed and ready for the single focused Item 5 commit
  - Definition of done:
    - EXVM build events can drive portable program emission directly.
    - The runtime-backed portable emission path covers the forms already landed
      in `vm_opcore` helper lowering.
    - The Rust `Expr` backend remains available as a compatibility path.

- [x] Item 6 - Route assembler and native PRVM program-returning paths through runtime-backed emission first
  - Source requirement or finding IDs: `SR-RUNTIME-DIRECT-EMISSION`,
    `SR-COMMITTED-M68K-ROLLOUT`, `SR-COMMITTED-NATIVE-PRVM`,
    `SR-EQUIVALENCE-LOCK`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-asm/src/tests.rs`
    - native PRVM bridge tests or fixtures if the current harnesses need to be
      updated to reflect the new preferred seam
  - Implementation slice:
    - Make the runtime-backed direct emission path the first portable-program
      handoff for the assembler entry points that currently rely on the landed
      `try_compile_direct_*` helper chain.
    - Keep compatibility-only shapes explicit rather than silently widening
      runtime-backed authority.
    - Collapse or narrow redundant helper seams in `vm_opcore` only where the
      runtime-backed path has already proven parity.
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm exvm_ -- --nocapture`
    - focused `cargo test -p asm motorola68000_ -- --nocapture`
    - focused native PRVM bridge tests for expression request/resume and
      metadata persistence if that path changes
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `Plan Compliance Reviewer` with the
    active `AGENTS.md`, this plan path, the runtime-handoff unification
    summary,
    changed files, and validation logs before committing.
  - Commit outcome: one commit that makes runtime-backed portable emission the
    preferred handoff for the already-authoritative covered paths.
  - Execution status:
    - implementation landed in `crates/opforge-vm/src/vm_opcore.rs` on
      2026-05-11
    - EXVM v2 plus EXPR v2 assembler program-returning paths now prefer the
      runtime-backed portable emission backend before the helper chain
    - compatibility-only string and register leaves remain on the explicit
      AST/direct-leaf fallback path instead of silently widening runtime-backed
      authority
    - focused `cargo test -p vm execution_model_parse_expression_program_v2_ -- --nocapture`
      passed after restoring the explicit compatibility fallback
    - focused `cargo test -p vm exvm_ -- --nocapture` passed
    - focused `cargo test -p asm motorola68000_ -- --nocapture` passed
    - focused `cargo test -p vm native_prvm_abi_decodes_expression_request_with_bounded_range_and_resume_slot -- --nocapture`
      passed
    - `cargo fmt --all --check` passed
    - `scripts/workflow/run_rust_quality_gate.sh` passed on 2026-05-11
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/direct-exvm-v2-to-expr-program-emission-plan.md`
      initially failed because the compliance-pass bookkeeping had not yet been
      recorded
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/direct-exvm-v2-to-expr-program-emission-plan.md`
      passed after normalizing the blocking-rule wording and closing the Item 6
      bookkeeping
    - `Plan Compliance Reviewer` returned PASS for the Item 6 slice on
      2026-05-11 with no further bookkeeping required before commit
  - Definition of done:
    - assembler portable-program entry points prefer runtime-backed emission
      for already-covered authoritative forms
    - Motorola 68000-family authoritative behavior remains unchanged
    - native PRVM expression request/resume behavior remains deterministic

- [x] Item 7 - Refresh parity evidence and documentation for the final runtime-backed seam
  - Source requirement or finding IDs: `SR-EQUIVALENCE-LOCK`,
    `SR-DOC-PORTABILITY`.
  - Expected files:
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `documentation/vm-boundary-protocol-v1.md`
    - `documentation/opforge-assembler-vm-path-guide-v0_1.md`
    - this plan file for checkbox bookkeeping if needed
  - Implementation slice:
    - Add or update parity coverage to prove that the final runtime-backed
      direct emission path matches the already-committed behavior on EXVM/EXPR,
      assembler, Motorola 68000 rollout, and native PRVM paths.
    - Refresh documentation so it describes the final seam accurately instead
      of the current hybrid helper-based handoff.
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm exvm_ -- --nocapture`
    - focused `cargo test -p asm motorola68000_ -- --nocapture`
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/direct-exvm-v2-to-expr-program-emission-plan.md`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `Plan Compliance Reviewer` with the
    active `AGENTS.md`, this plan path, the final-runtime-parity summary,
    changed files, and validation logs before committing.
  - Commit outcome: one commit that records the final runtime-backed direct
    emission seam and its locked portability contract.
  - Execution status:
    - parity coverage refreshed in `crates/opforge-vm/src/runtime_tests.rs` on
      2026-05-11 so the public assembler program-returning path is compared
      directly against the runtime-backed portable backend for the covered
      corpus
    - documentation refreshed in `documentation/vm-boundary-protocol-v1.md`
      and `documentation/opforge-assembler-vm-path-guide-v0_1.md` so the final
      seam is described as `tokens -> EXVM bytecode -> PortableExprProgram ->
      EXPR v2`, with the Rust `Expr` backend remaining compatibility/debug only
    - focused `cargo test -p vm execution_model_parse_expression_program_v2_matches_runtime_backend_for_covered_corpus -- --nocapture`
      passed
    - focused `cargo test -p vm exvm_ -- --nocapture` passed
    - focused `cargo test -p asm motorola68000_ -- --nocapture` passed
    - focused `cargo test -p vm native_prvm_abi_decodes_expression_request_with_bounded_range_and_resume_slot -- --nocapture`
      passed
    - `cargo fmt --all --check` passed
    - `scripts/workflow/run_rust_quality_gate.sh` passed on 2026-05-11
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/direct-exvm-v2-to-expr-program-emission-plan.md`
      passed
    - `Plan Compliance Reviewer` returned PASS for the Item 7 slice on
      2026-05-11 with no further bookkeeping required before commit
  - Definition of done:
    - documentation matches the final runtime-backed seam
    - parity evidence covers the final direct-emission path, not only the
      helper-based hybrid
    - this plan is fully aligned with the committed EXVM/PRVM scope and the
      remaining runtime work is closed out

## Milestones

- [x] Milestone 1 - helper-based direct lowering is authoritative for the
  covered EXVM/EXPR grammar below the runtime seam
- [x] Milestone 2 - covered assembler callers and fallback retirement are
  complete
- [x] Milestone 3 - Motorola 68000-family parser/eval hot paths are
  authoritative
- [x] Milestone 4 - native PRVM expression request/resume and metadata
  persistence are landed
- [x] Milestone 5 - EXVM v2 itself emits portable programs through runtime
  backends for the already-covered authoritative forms
- [x] Milestone 6 - redundant helper seams are collapsed and final docs/parity
  evidence are green

## Success Criteria

- The plan accurately reflects the committed EXVM/PRVM scope already on `main`.
- The remaining unchecked work is limited to the real unfinished gap:
  runtime-backed direct emission inside `exvm_v2_runtime.rs` and cleanup of the
  now-intermediate helper seam.
- Existing EXVM v2 to Rust `Expr` behavior remains available and stable as a
  compatibility backend.
- The final direct path does not require constructing Rust `Expr` for the
  already-covered authoritative forms.
- Motorola 68000-family authoritative behavior and native PRVM
  expression-request/metadata behavior remain unchanged by the final seam move.
- Documentation states that `PortableExprProgram` is the canonical portable
  representation and that native hosts need not reproduce Rust AST lowering.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- plan-compliance-reviewer passes before commit
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- do not widen this plan into richer EXPR v2 shapes or broader caller migration
  without a separate scoped follow-up plan
