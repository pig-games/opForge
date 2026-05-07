# opForge Expression VM v2 Authoritative Migration Plan v0.1

## Metadata

- Source: user request on 2026-05-07 to determine whether an unfinished plan
  already exists for moving most opForge expression processing into VMs;
  derived from the completed
  `documentation/plans/opforge-expression-vm-math-expression-implementation-plan-v0_1.md`
  and the draft
  `documentation/opForge-extended-expression-vm-instruction-set-spec-v0_1.md`.
- Mode: migration
- Owner: opForge implementation agent

## Source Artifact And Current Gap

The earlier expression-parser plan is complete, but it stopped at the `EXVM`
surface boundary rather than finishing the underlying migration into explicit
VM-owned expression execution.

Current implementation facts that justify a new plan:

- `crates/opforge-package/src/package.rs` defines `ExvmOpcode` with only
  `End`, `ParseExpression`, `EmitDiag`, and `Fail`.
- `crates/opforge-vm/src/vm_opcore.rs` executes `ParseExpression` by calling
  `parse_exvm_scalar_expression_tokens(...)`.
- `crates/opforge-vm/src/runtime_expr_parser.rs` still contains the
  authoritative recursive-descent expression parser and still constructs Rust
  `Expr` nodes directly for covered grammar.
- `crates/opforge-core/src/expr_vm.rs` still compiles a Rust `Expr` AST into
  scalar-only `EXPR v1` bytecode through
  `compile_core_expr_to_portable_program(...)`.
- As a result, the repository has a finished parser-surface plan, but no still-
  active plan for the remaining migration from Rust parser/compiler control
  flow into explicit VM contracts.

This plan covers that remaining migration.

## Objective

Move covered opcore expression processing from Rust-side parser/compiler control
flow into explicit, versioned VM contracts.

The target end state is:

- `EXVM v2` runs package/runtime-defined parser bytecode rather than the opaque
  `ParseExpression` escape hatch.
- `EXPR v2` evaluates versioned VM-native expression programs with typed values
  rather than relying on Rust `Expr` to `EXPR v1` compilation as the
  authoritative path.
- Covered token slices can be parsed and lowered without Rust `Expr` as the
  authoritative intermediate for authoritative families.
- `RuntimeExpressionParser` and
  `compile_core_expr_to_portable_program(...)` remain compatibility paths only
  until the authoritative rollout is complete, then are retired from covered
  family execution.
- PRVM/opasm continues to own operand boundary detection and CPU-family operand
  shapes.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- This plan continues the expression migration after the completed
  parser-surface plan. It must not reopen already-closed rename or guardrail
  work except where new versioned `EXVM v2` or `EXPR v2` contract surfaces make
  an extension necessary.
- Keep macro expansion, module graph orchestration, preprocess, line routing,
  pass scheduling, and artifact emission host-owned.
- Keep operand splitting and CPU-family operand wrappers in PRVM/opasm; do not
  move whole-line parsing or operand boundary detection into `EXVM`.
- Keep call expressions and placeholder nodes out of scope unless a later plan
  explicitly adds them.
- Native AmigaOS or m68k assembly ports are not part of this plan. They should
  consume the stabilized Rust-side contract in a later follow-up plan rather
  than co-evolve inside the same execution slice.
- No silent fallback: for any grammar slice marked authoritative by this plan,
  version mismatch, invalid opcode, or unsupported covered operation is a hard
  error rather than host delegation.
- The plan must not become active until `plan-quality-reviewer` or
  `plan-quality-orchestrator` returns `PASS`.
- Execute one work item at a time; each work item ends in exactly one new
  commit before the next item starts.

## Source Requirement IDs

- `SR-EXPR-VM-AUTHORITY`: move covered expression processing from Rust-side
  control flow into explicit VM contracts.
- `SR-EXVM-BYTECODE`: covered expression parsing must execute parser bytecode
  rather than an opaque `ParseExpression` delegation seam.
- `SR-EXPR-V2`: expression evaluation must support a typed, versioned VM
  contract that goes beyond scalar-only `EXPR v1`.
- `SR-NO-AST-AUTHORITY`: covered authoritative VM paths must not require Rust
  `Expr` as the authoritative intermediate.
- `SR-PRVM-BOUNDARY`: operand boundary detection and CPU-family operand shapes
  remain outside opcore expression VM ownership.
- `SR-DETERMINISM`: budgets, diagnostics, and failure behavior remain
  deterministic.
- `SR-ROLLOUT-DISCIPLINE`: compatibility fallbacks remain explicit and
  version-gated until full parity is proven.

## Version Impact

- Affected component(s): `crates/opforge-package`, `crates/opforge-vm`,
  `crates/opforge-core`, focused assembler/runtime expression tests, and
  expression/runtime documentation.
- Impact class: architectural migration with runtime-visible contract and
  version-surface changes.
- Owned contract: opcore expression parser and evaluator VM behavior for
  bounded assembler expression token ranges.
- Rationale: the current repository exposes `EXVM` as an authoritative surface,
  but the underlying covered parser/compiler behavior still lives mostly in
  Rust control flow.

## Architecture Direction For This Plan

This plan chooses one execution direction to avoid ambiguity:

- `EXVM` remains a distinct parser contract in this plan. It does not collapse
  into whole-line parsing or general host orchestration.
- `EXPR v2` becomes the typed evaluator contract that authoritative parser
  output targets for scalar or shape-preserving execution.
- Covered parser work must stop using opaque `ParseExpression` delegation.
- Covered evaluator work must stop using Rust `Expr` to `EXPR v1` compilation as
  the authoritative path.

If later work decides to collapse parser and evaluator contracts into one
cleaner opcore boundary, that must happen in a separate follow-up plan after
this migration lands and proves parity.

## Work Items

- [x] Item 1 - Add versioned `EXVM v2` and `EXPR v2` skeletons at the live seams
  - Source requirement or finding IDs: `SR-EXVM-BYTECODE`, `SR-EXPR-V2`,
    `SR-DETERMINISM`, `SR-ROLLOUT-DISCIPLINE`, `SR-NO-AST-AUTHORITY`.
  - Expected files:
    - `crates/opforge-package/src/package.rs`
    - `crates/opforge-package/src/package/tests.rs`
    - `crates/opforge-core/src/expr_vm.rs` or a new focused `expr_vm_v2` module
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Introduce version identifiers, opcode enums, budget structs, diagnostic
      maps, and typed runtime value scaffolding for `EXVM v2` and `EXPR v2`
      without changing the authoritative parser or evaluator path yet.
    - Preserve explicit legacy support for `EXVM v1` and `EXPR v1` so rollout
      can remain staged and testable.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p opforge-package exvm -- --nocapture`
    - `cargo test -p opforge-package expr -- --nocapture`
    - `cargo test -p vm exvm -- --nocapture`
    - `cargo test -p opforge-core expr_vm -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the v2-skeleton slice summary, changed
    files, and validation logs before committing.
  - Commit outcome: One commit that adds versioned parser/evaluator runtime
    skeletons while keeping legacy behavior intact.
  - Definition of done:
    - Package metadata can represent both legacy and v2 expression contracts.
    - Runtime code deterministically rejects unknown versions and invalid v2
      opcodes.
    - Legacy `EXVM v1` and `EXPR v1` behavior is still available for staged
      rollout.

- [x] Item 2 - Implement minimal `EXVM v2` execution for leaf and grouping expressions
  - Source requirement or finding IDs: `SR-EXPR-VM-AUTHORITY`,
    `SR-EXVM-BYTECODE`, `SR-DETERMINISM`, `SR-NO-AST-AUTHORITY`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs` or a new focused
      `exvm_v2_runtime.rs` module
    - `crates/opforge-vm/src/runtime_tests.rs`
    - package/runtime program descriptors required for authoritative families
  - Implementation slice:
    - Implement the token cursor, control flow, and output-stack operations
      needed for one narrow happy path: numeric literals, identifiers, current
      address, and parenthesized grouping.
    - Run that slice through real `EXVM v2` bytecode instead of the opaque
      `ParseExpression` escape hatch.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm_scalar_leaf -- --nocapture` or the nearest focused
      leaf/grouping `EXVM` filter
    - `cargo test -p vm exvm -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the leaf-and-grouping slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that proves `EXVM v2` can execute a real parser
    bytecode slice for basic expression leaves and grouping.
  - Definition of done:
    - The leaf/grouping path no longer uses the opaque `ParseExpression`
      delegation seam.
    - One narrow authoritative `EXVM v2` path is working end to end.

- [x] Item 3 - Add unary and arithmetic parsing to `EXVM v2`
  - Source requirement or finding IDs: `SR-EXPR-VM-AUTHORITY`,
    `SR-EXVM-BYTECODE`, `SR-NO-AST-AUTHORITY`, `SR-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs` or a new focused
      `exvm_v2_runtime.rs` module
    - `crates/opforge-vm/src/runtime_tests.rs`
    - package/runtime program descriptors required for authoritative families
  - Implementation slice:
    - Add parser-program support for unary operators and core arithmetic only:
      `+`, `-`, bit-not, logic-not, low-byte, high-byte, `+`, `-`, `*`, `/`,
      `%`, and `**`.
    - Keep the slice narrow enough that one focused arithmetic happy path can
      demonstrate the bytecode-backed parser before broader operator coverage is
      added.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm_scalar -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the unary-and-arithmetic slice
    summary, changed files, and validation logs before committing.
  - Commit outcome: One commit that extends the real `EXVM v2` parser path to
    unary and arithmetic grammar only.
  - Definition of done:
    - Unary and arithmetic parsing execute real `EXVM v2` bytecode.
    - The implementation remains a narrow vertical slice with one focused
      arithmetic validation target.

- [x] Item 4 - Add shift, comparison, and bitwise or logical parsing to `EXVM v2`
  - Source requirement or finding IDs: `SR-EXPR-VM-AUTHORITY`,
    `SR-EXVM-BYTECODE`, `SR-NO-AST-AUTHORITY`, `SR-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs` or the focused `EXVM v2`
      runtime module introduced earlier
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Add shifts, ordered comparisons, equality, and bitwise or logical
      operators to the bytecode-backed parser path.
    - Do not widen into ternary or aggregate value nodes in this item.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm_operator -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the scalar-operator slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that adds the remaining non-ternary scalar
    operators to the bytecode-backed parser path.
  - Definition of done:
    - Covered shift, comparison, and bitwise or logical parsing execute real
      `EXVM v2` bytecode.
    - This item does not widen into aggregate grammar.

- [ ] Item 5 - Add ternary parsing to `EXVM v2`
  - Source requirement or finding IDs: `SR-EXPR-VM-AUTHORITY`,
    `SR-EXVM-BYTECODE`, `SR-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs` or the focused `EXVM v2`
      runtime module introduced earlier
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Add `?:` parsing to the bytecode-backed parser path while preserving the
      current malformed-branch and missing-colon diagnostics.
    - Keep calls and placeholders out of scope.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm_ternary -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the ternary-parser slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that makes ternary parsing part of the real
    `EXVM v2` path.
  - Definition of done:
    - Ternary parsing executes real `EXVM v2` bytecode.
    - Out-of-scope calls and placeholders remain explicit.

- [ ] Item 6 - Add range and list parsing to `EXVM v2`
  - Source requirement or finding IDs: `SR-EXPR-VM-AUTHORITY`,
    `SR-NO-AST-AUTHORITY`, `SR-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs` or the focused `EXVM v2`
      runtime module introduced earlier
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Add range expression and list literal parsing to the real bytecode-backed
      parser path.
    - Keep the slice limited to these two aggregate forms.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm_range_list -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the range-and-list slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that adds real bytecode-backed parser support for
    ranges and lists.
  - Definition of done:
    - Range and list parsing execute real `EXVM v2` bytecode.
    - Aggregate parsing scope remains narrow and testable.

- [ ] Item 7 - Add struct, member, and index parsing to `EXVM v2`
  - Source requirement or finding IDs: `SR-EXPR-VM-AUTHORITY`,
    `SR-NO-AST-AUTHORITY`, `SR-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs` or the focused `EXVM v2`
      runtime module introduced earlier
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Add struct literals, member access, and index access to the real
      bytecode-backed parser path.
    - Do not mix out-of-scope failure cleanup into this item.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm_struct_access -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the struct-access slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that adds struct, member, and index parsing to
    the real `EXVM v2` path.
  - Definition of done:
    - Struct, member, and index parsing execute real `EXVM v2` bytecode.
    - The slice stays limited to these three access or aggregate forms.

- [ ] Item 8 - Lock explicit out-of-scope failures and retire legacy parser authority for covered grammar
  - Source requirement or finding IDs: `SR-EXPR-VM-AUTHORITY`,
    `SR-NO-AST-AUTHORITY`, `SR-PRVM-BOUNDARY`, `SR-ROLLOUT-DISCIPLINE`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `documentation/opforge-assembler-vm-path-guide-v0_1.md`
  - Implementation slice:
    - Make call expressions and placeholder nodes deterministic out-of-scope
      failures on the authoritative v2 parser path.
    - Retire `RuntimeExpressionParser` from the authoritative covered grammar
      surface once Items 2 through 7 are complete.
    - Keep operand-shape ownership explicitly in PRVM/opasm.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm_remaining_value_out_of_scope -- --nocapture`
    - `cargo test -p vm runtime_expression_parser -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the parser-retirement slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that removes legacy parser authority from the
    covered grammar and hardens out-of-scope behavior.
  - Definition of done:
    - Authoritative covered parser grammar no longer depends on
      `RuntimeExpressionParser`.
    - Out-of-scope nodes fail deterministically rather than drifting back into
      opaque host parsing.
    - Operand-shape ownership remains in PRVM/opasm.

- [ ] Item 9 - Add `EXPR v2` scalar evaluation for leaf, unary, and arithmetic forms
  - Source requirement or finding IDs: `SR-EXPR-V2`, `SR-NO-AST-AUTHORITY`,
    `SR-DETERMINISM`, `SR-ROLLOUT-DISCIPLINE`.
  - Expected files:
    - `crates/opforge-core/src/expr_vm.rs` or a focused `expr_vm_v2` module
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-core` expression VM tests
  - Implementation slice:
    - Implement `EXPR v2` scalar evaluator support for leaf, unary, and core
      arithmetic forms first.
    - Add authoritative lowering from covered parser output into `EXPR v2`
      programs for that narrow subset without routing through Rust `Expr` to
      `EXPR v1` compilation.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p opforge-core expr_vm -- --nocapture`
    - `cargo test -p vm exvm_scalar -- --nocapture`
    - `cargo test -p vm runtime_expression_eval -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the scalar-eval-core slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that makes covered scalar evaluation use
    `EXPR v2` for the narrow core scalar subset.
  - Definition of done:
    - Covered core scalar evaluation no longer depends on
      `compile_core_expr_to_portable_program(...)` as the authoritative path.
    - `EXPR v2` preserves deterministic evaluation, diagnostics, and symbol/
      current-address behavior.
    - Legacy `EXPR v1` remains explicit and version-gated for broader grammar.

- [ ] Item 10 - Extend `EXPR v2` scalar evaluation to the full covered scalar grammar
  - Source requirement or finding IDs: `SR-EXPR-V2`, `SR-NO-AST-AUTHORITY`,
    `SR-DETERMINISM`, `SR-ROLLOUT-DISCIPLINE`.
  - Expected files:
    - `crates/opforge-core/src/expr_vm.rs` or the focused `expr_vm_v2` module
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-core` expression VM tests
  - Implementation slice:
    - Extend the `EXPR v2` scalar path to shifts, comparisons, bitwise or
      logical operators, and ternary expressions.
    - Cut authoritative scalar families over from `EXPR v1` once the full
      scalar grammar is proven under `EXPR v2`.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p opforge-core expr_vm -- --nocapture`
    - `cargo test -p vm exvm_operator -- --nocapture`
    - `cargo test -p vm exvm_ternary -- --nocapture`
    - `cargo test -p vm runtime_expression_eval -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the scalar-eval-full slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that makes `EXPR v2` authoritative for the full
    covered scalar grammar.
  - Definition of done:
    - Covered scalar families no longer depend on legacy `EXPR v1` authority.
    - Full scalar grammar uses `EXPR v2` with deterministic parity.

- [ ] Item 11 - Add `EXPR v2` structural values and explicit scalar-boundary enforcement
  - Source requirement or finding IDs: `SR-EXPR-V2`, `SR-PRVM-BOUNDARY`,
    `SR-DETERMINISM`.
  - Expected files:
    - `crates/opforge-core/src/expr_vm.rs` or the focused `expr_vm_v2` module
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - any focused value-model tests needed for structural results
  - Implementation slice:
    - Extend `EXPR v2` to carry the structural value kinds required by the
      currently in-scope generic expression grammar.
    - Add explicit `RequireScalar`-style boundary checks so scalar-only callers
      fail deterministically when a structural result is not reducible.
    - Keep operand wrappers and CPU-family operand shapes outside scope unless a
      later plan changes the opcore/opasm boundary.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p opforge-core expr_vm -- --nocapture`
    - `cargo test -p vm exvm_range_list -- --nocapture`
    - `cargo test -p vm exvm_struct_access -- --nocapture`
    - `cargo test -p vm runtime_expression_generic_value_nodes_parse_but_reject_scalar_vm_compile -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the structural-value slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that adds typed structural `EXPR v2` value
    handling with explicit scalar-boundary enforcement.
  - Definition of done:
    - Structural results no longer require Rust `Expr` ownership for covered
      authoritative paths.
    - Scalar-only callers receive deterministic diagnostics when reduction is
      invalid.
    - Boundary ownership between opcore and opasm remains explicit.

- [ ] Item 12 - Refresh documentation, rollout evidence, and final quality gates
  - Source requirement or finding IDs: `SR-ROLLOUT-DISCIPLINE`,
    `SR-DETERMINISM`, `SR-EXPR-VM-AUTHORITY`.
  - Expected files:
    - `documentation/libopforge-developer-guide.md`
    - `documentation/vm-boundary-protocol-v1.md`
    - `documentation/opforge-assembler-vm-path-guide-v0_1.md`
    - any expression/runtime references changed by the migration
    - this plan, for final checkbox state only
  - Implementation slice:
    - Update developer and runtime-boundary documentation so the authoritative
      expression path, version rules, host responsibilities, and fallback
      controls match the landed v2 implementation.
    - Run the full Rust quality gate and record the results in final execution
      status.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test --locked`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `scripts/workflow/run_rust_quality_gate.sh`
    - `make workflow-gate` or the relevant `check_*.py` validators if this plan
      or other workflow artifacts are updated during execution
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-expression-vm-v2-authoritative-migration-plan-v0_1.md`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the documentation-and-final-gate slice
    summary, changed files, and validation logs before committing.
  - Commit outcome: One commit that leaves the repo with aligned documentation
    and recorded quality-gate evidence for the completed migration.
  - Definition of done:
    - Runtime and developer documentation describe the landed v2 path rather
      than the legacy parser-surface-only state.
    - The final quality gate passes and is recorded in execution status.
    - The plan checkboxes reflect the true completion state.

## Milestones

- [x] Milestone 1 - v2 parser and evaluator skeletons exist at the live seams
  - Items required: 1.
  - Evidence: package/runtime can represent v2 surfaces without changing legacy
    authority yet.

- [ ] Milestone 2 - Scalar parsing is executed by real `EXVM v2` bytecode
  - Items required: 2-5.
  - Evidence: scalar leaf, operator, and ternary parsing no longer use opaque
    `ParseExpression` delegation.

- [ ] Milestone 3 - Covered parser grammar no longer depends on legacy Rust
  parser authority
  - Items required: 6-8.
  - Evidence: aggregate and access grammar is bytecode-backed; out-of-scope
    behavior stays explicit; legacy parser authority is retired from covered
    grammar.

- [ ] Milestone 4 - Covered evaluation no longer depends on Rust `Expr` to
  `EXPR v1` authority
  - Items required: 9-11.
  - Evidence: scalar and structural covered evaluation run through versioned
    `EXPR v2` with explicit scalar-boundary checks.

- [ ] Milestone 5 - Covered families use the full v2 expression path end to end
  - Items required: 12.
  - Evidence: documentation plus quality-gate evidence are aligned with the
    landed v2 path.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping