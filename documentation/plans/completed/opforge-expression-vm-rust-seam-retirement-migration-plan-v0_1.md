# opForge Expression VM Rust Seam Retirement Migration Plan v0.1

## Metadata

- Source: user request on 2026-05-08 to turn the remaining non-VM Rust
  expression seams into a concrete follow-up migration plan, derived from the
  completed
  `documentation/plans/opforge-expression-vm-v2-authoritative-migration-plan-v0_1.md`
  and a live code audit of the covered assembler path.
- Mode: migration
- Owner: opForge implementation agent

## Objective

Retire the remaining Rust-hosted expression seams on the covered authoritative
assembler path so that covered families no longer depend on Rust recursive-
descent parsing, Rust `Expr` as the authoritative lowering boundary, or direct
host scalar evaluation except at explicitly defined permanent host-owned
boundaries.

For this plan, covered authoritative families means the families/dialects for
which the resolved expression parser and evaluator contracts already opt into
the current EXVM/EXPR runtime path.

The target end state is:

- covered EXVM parsing does not delegate covered grammar to
  `RuntimeExpressionParser` or `Parser::parse_expr_from_tokens(...)`
- covered evaluation does not require Rust `Expr` as the authoritative
  intermediate before EXPR execution
- assembler call sites use contract-aware scalar or value evaluation entry
  points rather than calling `eval_expr_ast(...)` directly, except where the
  host must still own semantics
- permanent host-owned semantics remain explicit, narrow, and test-covered
- non-authoritative families keep an explicit compatibility path until a later,
  separate rollout plan widens family coverage

## Current Implementation Facts

- `crates/opforge-vm/src/vm_opcore.rs` still executes the covered EXVM parse
  path through `parse_exvm_scalar_expression_tokens(...)` and uses
  `RuntimeExpressionParser::new(...).parse_expr_from_tokens()` for the
  compatibility path.
- `crates/opforge-vm/src/vm_opcore.rs` still falls back to
  `Parser::parse_expr_from_tokens(...)` when no parser-VM contract is active.
- `crates/opforge-vm/src/vm_opcore.rs` still compiles parsed Rust `Expr`
  values through `compile_core_expr_to_portable_program_with_opcode_version(...)`
  before portable EXPR evaluation.
- `crates/opforge-asm/src/asmline_eval.rs` still contains the authoritative
  host scalar evaluator `eval_expr_ast(...)`, and multiple assembler flows call
  it directly.
- Direct host scalar evaluation currently appears in conditionals, text
  encoding directives, repetition control, and assignment flows.
- The narrow repetition-side-table member/index carveout remains host-owned by
  design, and string-literal encoding still crosses back through host context.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Execute one work item at a time, and end each work item in exactly one new
  commit before starting the next item.
- Keep macro expansion, pass scheduling, module orchestration, line routing,
  artifact emission, text-encoding registry ownership, and repetition side
  tables host-owned.
- Keep CPU-family operand shape parsing in PRVM/opasm. This plan must not
  reopen operand-shape ownership work that was intentionally left outside
  opcore expression VM ownership.
- No silent fallback for covered authoritative families: once a slice is marked
  authoritative, unsupported covered grammar or invalid runtime state must
  fail deterministically rather than delegating back to host parsing or host
  scalar evaluation.
- Do not widen this plan into full-family rollout. Non-authoritative families
  may retain an explicit compatibility path until a later follow-up plan.
- Do not widen this plan into placeholder or general call-expression support.
  If those features are promoted into covered VM ownership later, that must be
  done under a separate scoped plan.
- The plan must not become active until `Plan Quality Reviewer` or
  `Plan Quality Orchestrator` returns `PASS`.

## Source Requirement IDs

- `SR-COVERED-NO-RUST-PARSER-AUTHORITY`: covered EXVM parsing must not rely on
  Rust recursive-descent parser authority.
- `SR-COVERED-NO-RUST-EXPR-AUTHORITY`: covered evaluation must not require Rust
  `Expr` as the authoritative lowering boundary.
- `SR-HOST-CALLER-MIGRATION`: assembler and directive call sites must use
  contract-aware scalar/value evaluation entry points rather than direct host
  AST evaluation wherever covered semantics already exist.
- `SR-EXPLICIT-HOST-BOUNDARIES`: any remaining host-owned behavior must be
  explicit, narrow, documented, and tested.
- `SR-COMPATIBILITY-DISCIPLINE`: legacy compatibility paths must remain
  isolated, version-gated, and unavailable to covered authoritative families.
- `SR-DETERMINISM`: diagnostics, budgets, and failure behavior must remain
  deterministic.

## Version Impact

- Affected component(s): `crates/opforge-vm`, `crates/opforge-core`,
  `crates/opforge-asm`, focused package/runtime expression tests, and
  expression/runtime boundary documentation.
- Impact class: architectural migration that narrows compatibility seams and
  changes the authoritative covered execution path.
- Owned contract: covered opcore expression parsing and evaluation behavior at
  the assembler/runtime boundary.
- Rationale: the completed EXVM/EXPR v2 migration established the authoritative
  contract surface, but the live covered path still depends on Rust parser,
  lowering, and host-evaluator seams that should either be retired or made
  explicitly permanent.

## Architecture Direction For This Plan

This plan resolves the remaining ambiguity in one direction:

- covered EXVM parsing should produce a VM-owned lowering target without using
  Rust parser authority as the live contract
- covered evaluation should consume that VM-owned lowering target without
  making Rust `Expr` the authoritative intermediate
- direct assembler users of scalar/value expression results should call one
  contract-aware API layer rather than selecting between VM and host AST logic
  ad hoc
- permanent host-owned semantics stay host-owned, but only through explicit,
  named hooks or boundary helpers rather than implicit fallback

If later work wants a broader IR redesign, host service ABI, or family rollout
expansion, that must be handled in a separate plan after this narrower seam-
retirement work lands.

## Work Items

- [x] Item 1 - Fence and instrument the remaining compatibility seams
  - Source requirement or finding IDs: `SR-COMPATIBILITY-DISCIPLINE`,
    `SR-DETERMINISM`, `SR-COVERED-NO-RUST-PARSER-AUTHORITY`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs` or a new focused
      compatibility module
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `documentation/vm-boundary-protocol-v1.md` if boundary wording must be
      tightened immediately
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` filters covering parser-contract and
      compatibility-path behavior
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the compatibility-fencing slice
    summary, changed files, and validation logs before committing.
  - Commit outcome: one commit that isolates legacy parser compatibility behind
    clearly named helpers and proves covered authoritative families do not
    silently fall through to generic host parsing.
  - Definition of done:
    - covered-family tests can detect and fail unintended parser fallback
    - compatibility parsing remains available only through explicit, non-
      authoritative entry points
    - the live code makes the remaining parser seam visible instead of implicit

- [x] Item 2 - Introduce direct VM-owned lowering for one narrow covered slice
  - Source requirement or finding IDs:
    `SR-COVERED-NO-RUST-PARSER-AUTHORITY`,
    `SR-COVERED-NO-RUST-EXPR-AUTHORITY`, `SR-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-core/src/expr_vm.rs` or a new focused lowering module
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` filters for direct EXVM-to-EXPR lowering on a
      covered single-token leaf path plus one narrow non-leaf seam guard
    - focused `cargo test -p opforge-core expr_vm -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the direct-leaf vertical-slice
    summary, changed files, and validation logs before committing.
  - Commit outcome: one commit that proves at least one covered grammar slice
    can parse and lower into portable EXPR execution without Rust `Expr` as the
    authoritative intermediate, initially limited to covered single-token leaf
    expressions.
  - Definition of done:
    - one narrow covered single-token leaf path reaches EXPR execution through
      a VM-owned lowering target
    - the leaf slice does not rely on `RuntimeExpressionParser` or
      `compile_core_expr_to_portable_program_with_opcode_version(...)` for its
      authoritative behavior
    - non-leaf grammar continues to use the legacy lowering seam until Item 3

 - [x] Item 3 - Expand direct lowering to the audited covered scalar grammar
  - Source requirement or finding IDs:
    `SR-COVERED-NO-RUST-PARSER-AUTHORITY`,
    `SR-COVERED-NO-RUST-EXPR-AUTHORITY`, `SR-COMPATIBILITY-DISCIPLINE`,
    `SR-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-core/src/expr_vm.rs` or the direct-lowering module from
      Item 2
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` EXVM/EXPR contract filters for covered scalar
      grammar
    - focused `cargo test -p opforge-core expr_vm -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the scalar-grammar direct-lowering
    summary, changed files, and validation logs before committing.
  - Commit outcome: one commit that makes direct VM-owned lowering the
    authoritative covered-family path for the audited scalar grammar only:
    numbers, identifiers/register-symbols, `$`, strings, grouping, unary,
    arithmetic, shifts, comparisons, bitwise/logical operators, and ternary.
  - Definition of done:
    - covered authoritative families no longer depend on Rust `Expr` lowering
      for the audited scalar grammar
    - list, range, struct, member/index, placeholder, and call-expression work
      is not silently widened into this item

- [x] Item 4 - Add direct lowering for audited structural constructors
  - Source requirement or finding IDs:
    `SR-COVERED-NO-RUST-PARSER-AUTHORITY`,
    `SR-COVERED-NO-RUST-EXPR-AUTHORITY`, `SR-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-core/src/expr_vm.rs` or the direct-lowering module from
      earlier items
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` filters for list/range/struct-literal direct
      lowering
    - focused `cargo test -p opforge-core expr_vm -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the structural-constructor slice
    summary, changed files, and validation logs before committing.
  - Commit outcome: one commit that extends direct VM-owned lowering to the
    already-audited non-call structural constructor forms: list, range, and
    struct-literal creation.
  - Definition of done:
    - covered structural constructor forms no longer require Rust `Expr`
      lowering as the authoritative boundary
    - member access, index access, placeholder, and call expressions remain out
      of scope for this item

- [x] Item 5 - Add direct lowering for audited member/index access excluding permanent host carveouts
  - Source requirement or finding IDs:
    `SR-COVERED-NO-RUST-PARSER-AUTHORITY`,
    `SR-COVERED-NO-RUST-EXPR-AUTHORITY`, `SR-EXPLICIT-HOST-BOUNDARIES`,
    `SR-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-core/src/expr_vm.rs` or the direct-lowering module from
      earlier items
    - `crates/opforge-vm/src/runtime_tests.rs`
    - focused assembler/runtime integration tests for the repetition carveout
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` filters for member/index direct lowering
    - focused `cargo test -p opforge-core expr_vm -- --nocapture`
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the member-index direct-lowering slice
    summary, changed files, and validation logs before committing.
  - Commit outcome: one commit that makes direct VM-owned lowering
    authoritative for audited member/index access while preserving the explicit
    repetition-side-table `repeatLabel[index].field` host carveout.
  - Definition of done:
    - audited member/index forms no longer depend on Rust `Expr` lowering on
      covered authoritative families
    - the repetition-side-table carveout remains explicit and test-covered
    - placeholder and call-expression work is still deferred

- [x] Item 6 - Migrate conditionals and assignment callers off direct host AST evaluation
  - Source requirement or finding IDs: `SR-HOST-CALLER-MIGRATION`,
    `SR-EXPLICIT-HOST-BOUNDARIES`, `SR-DETERMINISM`.
  - Expected files:
    - `crates/opforge-asm/src/asmline_eval.rs`
    - `crates/opforge-asm/src/asmline_conditionals.rs`
    - `crates/opforge-asm/src/line.rs`
    - focused assembler/runtime tests that cover the migrated callers
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p asm` or nearest filters for conditional and
      assignment evaluation
    - focused `cargo test -p vm` integration filters covering assembler-side
      contract usage
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the conditional-and-assignment caller
    migration summary, changed files, and validation logs before committing.
  - Commit outcome: one commit that replaces direct `eval_expr_ast(...)`
    callers in conditionals and assignment flows with contract-aware
    scalar/value evaluation helpers where covered semantics already exist.
  - Definition of done:
    - conditional and assignment flows no longer reach host AST evaluation by
      default on covered authoritative families
    - any remaining host-owned evaluation path in these callers is explicit and
      justified

- [x] Item 7 - Migrate text and repetition callers while preserving permanent host boundaries
  - Source requirement or finding IDs: `SR-HOST-CALLER-MIGRATION`,
    `SR-EXPLICIT-HOST-BOUNDARIES`, `SR-DETERMINISM`.
  - Expected files:
    - `crates/opforge-asm/src/asmline_eval.rs`
    - `crates/opforge-asm/src/asmline_directives_text.rs`
    - `crates/opforge-asm/src/repetition.rs`
    - focused assembler/runtime tests for text directives and repetition
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p asm` or nearest filters for text directives and
      repetition evaluation
    - focused `cargo test -p vm` integration filters covering explicit host
      boundary helpers
    - `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the text-and-repetition caller
    migration summary, changed files, and validation logs before committing.
  - Commit outcome: one commit that moves covered text/repetition callers onto
    contract-aware evaluation helpers while preserving text-encoding ownership
    and repetition-side-table semantics as explicit permanent host boundaries.
  - Definition of done:
    - text directive and repetition flows no longer rely on generic host AST
      evaluation for covered semantics
    - the remaining permanent host boundaries are explicit, narrow, and
      regression-tested

- [x] Item 8 - Retire covered-family fallback behavior and refresh the boundary docs
  - Source requirement or finding IDs: `SR-EXPLICIT-HOST-BOUNDARIES`,
    `SR-COMPATIBILITY-DISCIPLINE`, `SR-DETERMINISM`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-asm/src/asmline_eval.rs`
    - `documentation/libopforge-developer-guide.md`
    - `documentation/vm-boundary-protocol-v1.md`
    - `documentation/opforge-assembler-vm-path-guide-v0_1.md`
    - this plan file for checkbox bookkeeping
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test --locked`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `scripts/workflow/run_rust_quality_gate.sh`
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-expression-vm-rust-seam-retirement-migration-plan-v0_1.md`
    - `make workflow-gate`
  - Plan-compliance review evidence: run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the fallback-retirement and docs slice
    summary, changed files, and validation logs before committing.
  - Commit outcome: one commit that removes covered-family host fallback,
    updates the documented permanent host carveouts, and records final plan
    bookkeeping.
  - Definition of done:
    - covered authoritative families fail deterministically instead of silently
      using host parser or host scalar-evaluator fallback
    - documentation matches the actual remaining permanent host boundaries
    - final gates for the migration are recorded and green

## Milestones

- [x] Milestone 1 - Compatibility seams are fenced and observable
- [x] Milestone 2 - Direct VM-owned scalar lowering is authoritative for
  covered grammar
- [x] Milestone 3 - Direct VM-owned structural lowering is authoritative for
  the audited non-call grammar
- [x] Milestone 4 - Covered assembler call sites use contract-aware evaluation
  APIs
- [x] Milestone 5 - Only explicit permanent host boundaries remain documented

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping