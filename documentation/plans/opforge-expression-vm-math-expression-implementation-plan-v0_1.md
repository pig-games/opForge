# opForge Expression VM Expression Grammar Implementation Plan v0.1

## Metadata

- Source: User request on 2026-05-01 to expand the full VM-based parsing plan to
  move full expression handling into the VM, with the explicit nuance that this
  excludes asm operand shapes and covers purely mathematical expression parsing.
  Follow-up naming request: rename the expression parser VM package contract from
  `EXPP` to `EXVM` so the contract naming matches the existing VM chunk style.
  Follow-up scope adjustment: structs, ranges, and lists are part of the covered
  `EXVM` expression grammar.
- Mode: implementation
- Owner: opForge implementation agent

## Objective

Move opcore expression parsing from the current host-side
`RuntimeExpressionParser` implementation into an authoritative expression parser
VM contract named `EXVM`, while preserving the PRVM/opasm typed sub-call boundary
and keeping CPU-family asm operand-shape interpretation outside the expression VM.

The target end state is:

- package/runtime naming uses `EXVM` for the expression parser VM contract;
- `EXVM` parses the covered expression grammar into the existing
  opcore `Expr` AST shapes;
- PRVM uses typed expression sub-calls to `EXVM` for operand expression token
  ranges;
- m68k, 68080, and other CPU-family operand wrappers remain owned by the
  opasm/operand-shape layer;
- `DelegateCore` is removed or rejected for covered expression
  grammar once parity is proven.

For this plan, "covered expression grammar" means literal and symbol
leaves, parenthesized groups, unary operators, binary arithmetic/bitwise/logical
operators, comparisons, shifts, ternary conditionals, range expressions, list
literals, struct literals, member access, and index access. CPU-family operand
syntax remains outside this grammar even when operand syntax contains covered
expression sub-ranges.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- This plan is expression-parser-only. It must not move macro expansion, module
  graph orchestration, symbol resolution, instruction selection, instruction
  encoding, output generation, or full assembler pass control into `EXVM`.
- `EXVM` parses operand-shape-free expression grammar only. If token interpretation
  depends on CPU-family operand grammar, addressing modes, instruction-family
  syntax, or operand wrappers, it belongs to PRVM/opasm operand-shape handling,
  not to `EXVM`.
- Out of scope for `EXVM`: m68k full-extension/memory-indirect addressing,
  postincrement/predecrement operands, register-pair operands, bitfield suffix
  operands, `#` immediate operand wrapping, 68080 AMMX operand-group syntax,
  postfix tuple addressing forms, and any CPU-family-specific operand shape.
- Also out of scope for this plan unless a later plan explicitly adds them:
  placeholder nodes and function/call expressions.
- PRVM may pass explicit token ranges to `EXVM` through the typed sub-call
  protocol, but PRVM bytecode must not embed `EXVM` bytecode and `EXVM` bytecode
  must not embed PRVM bytecode.
- Existing AST compatibility is mandatory. Covered expression inputs must
  produce the same `Expr` shape, spans, and diagnostics as the current expression
  parser unless a work item explicitly records and validates an intentional
  behavior change.
- Determinism is mandatory: explicit token, step, stack, and output budgets must
  exist before `EXVM` becomes authoritative for any covered grammar slice.
- Compatibility with already-released `EXPP` package chunks is not required
  unless Work item 1 discovers an active runtime artifact or fixture that cannot
  be regenerated safely. If such an artifact exists, Work item 1 must stop and
  update this plan before implementing compatibility.
- Work item 1 is a required behavior-neutral prerequisite because the package
  chunk/schema name is serialized into VM runtime packages and referenced across
  package codec, registry, runtime, and tests. Stabilizing `EXVM` first prevents
  later behavior slices from creating a mixed `EXPP`/`EXVM` contract surface or
  burying package identity changes inside parser behavior commits.
- Fixture/reference regeneration is allowed only for intentional package-format
  or parser-output changes introduced by this plan. It must not be used to hide
  unexpected regressions.

## Opcode Byte Assignment And Reserved Space

`EXVM` must use the same fixed-byte opcode range discipline as the previous VM
implementation plans. Work item 1 may keep the existing legacy expression parser
opcode byte values during the behavior-neutral `EXPP` to `EXVM` rename, but any
new or expanded `EXVM` opcode set introduced by later work items must reserve
the same byte families and future-growth space:

| Range        | Family                                      |
|--------------|---------------------------------------------|
| `0x00-0x0F`  | Control flow and VM entry/exit operations   |
| `0x10-0x1F`  | Token inspection and expression predicates  |
| `0x20-0x2F`  | Token movement and cursor operations        |
| `0x30-0x3F`  | Value, literal, and symbol loading          |
| `0x40-0x4F`  | Expression parser helpers                   |
| `0x50-0x5F`  | Reserved cross-contract or host-service boundary |
| `0x60-0x6F`  | `Expr` AST builders                         |
| `0x70-0x7F`  | Diagnostics and deterministic failure       |
| `0x80-0xFF`  | Reserved                                    |

`0x00` remains `End`. Unassigned bytes inside an active range and every byte in
the reserved `0x80-0xFF` range must fail with a typed invalid-`EXVM`-opcode
diagnostic. Implementation slices must not consume reserved bytes to avoid a
local opcode shortage; if a slice needs more opcode space than the table allows,
execution must stop and this plan must be updated before implementation
continues.

## Work Items

- [x] Item 1 - Rename expression parser package contract from `EXPP` to `EXVM`
  - Source requirement or finding IDs: User naming request, 2026-05-01.
  - Validation: See Full quality gates for this item.
  - Definition of done: See item-specific Definition of done below.
  - Expected files:
    - `crates/opforge-package/src/package.rs`
    - `crates/opforge-package/src/package/codec.rs`
    - `crates/opforge-package/src/package/codec/scoped_schema.rs`
    - `crates/opforge-package/src/package/tests.rs`
    - `crates/opforge-vm/src/builder.rs`
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_model_core.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-vm/src/execution_model/tests.rs`
    - `crates/opforge-asm/src/tests.rs`
    - documentation references that describe the expression parser VM chunk name
  - Implementation slice:
    - Treat this as a behavior-neutral technical prerequisite for all later
      `EXVM` parser behavior work.
    - Rename the package chunk constant and codec helpers from `EXPP` to `EXVM`.
    - Rename public constants, diagnostics, test labels, and user-facing messages
      only where they refer to the expression parser VM contract name.
    - Keep opcode values and AST behavior unchanged.
    - Regenerate package/reference artifacts only if the renamed chunk changes
      expected serialized package bytes by design.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p opforge-package exvm -- --nocapture` or the nearest focused
      package codec test if the exact filter differs after implementation
    - `cargo test -p vm expression_parser_vm -- --nocapture` or the nearest
      focused VM expression contract filter
    - `cargo test -p asm expression_parser_vm -- --nocapture` or the nearest
      focused asm contract filter
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the rename-only slice summary, changed
    files, and validation logs before committing.
  - Commit outcome: One commit that performs only the `EXPP` to `EXVM` naming
    migration and any required reference updates.
  - Definition of done:
    - No active source/runtime/package references use `EXPP` for the current
      expression parser VM contract except historical notes that explicitly say
      they are historical.
    - Package encode/decode tests prove `EXVM` round-trips.
    - The rename does not consume new opcode bytes or change the fixed-byte
      allocation map for later `EXVM` behavior work.
    - Expression parsing behavior is unchanged.

- [x] Item 2 - Lock the `EXVM` expression contract and guardrail corpus
  - Source requirement or finding IDs: User scope requirement that expression
    handling is VM-owned but excludes asm operand shapes.
  - Validation: See Full quality gates for this item.
  - Definition of done: See item-specific Definition of done below.
  - Expected files:
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-package/src/package.rs`
    - `documentation/opforge-assembler-vm-path-guide-v0_1.md`
    - this plan, for progress evidence only
  - Implementation slice:
    - Add or extend focused parity fixtures that compare the current expression
      parser behavior for literals, identifiers, unary operators, binary
      precedence, grouping, ternary expressions, range expressions, list
      literals, struct literals, member access, index access, and malformed
      covered expressions.
    - Add negative guardrail fixtures proving operand shapes remain outside the
      expression parser: m68k postfix tuples, postincrement/predecrement,
      full-extension/memory-indirect brackets, register pairs, bitfield suffixes,
      and immediate operand wrappers.
    - Add explicit out-of-scope fixtures for placeholder nodes and function/call
      expressions so they cannot drift into this plan silently.
    - Document `EXVM` as the expression parser VM contract and explicitly state
      that operand-shape parsing remains outside it.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm runtime_expression_parser -- --nocapture`
    - `cargo test -p vm execution_model_parse_expression_for_assembler -- --nocapture`
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the contract/corpus slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that locks the `EXVM` contract expectations and
    guardrail tests without changing parser behavior.
  - Definition of done:
    - The covered expression corpus represents current behavior before VM parser
      internals change.
    - Operand-shape negative tests fail if `EXVM` starts accepting CPU-family
      addressing syntax as mathematical expressions.

- [x] Item 3 - Add the `EXVM` interpreter skeleton with deterministic budgets
  - Source requirement or finding IDs: Need VM-owned expression parsing rather
    than direct `RuntimeExpressionParser` delegation.
  - Validation: See Full quality gates for this item.
  - Definition of done: See item-specific Definition of done below.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs` or a new focused `EXVM`
      parser module if that is the smaller production change
    - `crates/opforge-package/src/package.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Introduce the `EXVM` execution entrypoint, interpreter state, cursor,
      value/output stack representation, and explicit token/step/stack budgets.
    - Establish the fixed-byte `EXVM` opcode map using the same range families
      and `0x80-0xFF` reserved space as previous VM implementation plans.
    - Support only contract validation, `End`, deterministic failure, and one
      minimal parse path needed by Item 4.
    - Keep `DelegateCore` available only behind an explicit compatibility path
      for not-yet-covered grammar; do not use it for covered Item 4 grammar.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm -- --nocapture`
    - `cargo test -p vm execution_model_parse_expression_for_assembler -- --nocapture`
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the interpreter-skeleton slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that adds the deterministic `EXVM` interpreter
    skeleton without widening expression grammar coverage beyond the first
    planned happy path.
  - Definition of done:
    - `EXVM` has a concrete runtime execution path with explicit budgets.
    - Unassigned and reserved opcode bytes fail with a typed invalid-`EXVM` opcode
      diagnostic.
    - Unsupported covered-mode bytecode fails deterministically.
    - Existing expression parsing still passes through the old behavior unless
      the Item 4 happy path is explicitly selected.

- [x] Item 4 - Implement scalar literals, identifiers, grouping, unary operators, and core arithmetic
  - Source requirement or finding IDs: First production slice of VM-owned
    mathematical expression parsing.
  - Validation: See Full quality gates for this item.
  - Definition of done: See item-specific Definition of done below.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs` or the new `EXVM` parser
      module introduced in Item 3
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Parse number/string/char literals as applicable to current mathematical
      expression behavior, identifiers/symbol references, parenthesized groups,
      unary `+`, unary `-`, bit-not, logic-not, low-byte, high-byte, and binary
      `+`, `-`, `*`, `/`, `%`, and `**` with existing precedence and
      associativity.
    - Route the focused happy-path corpus through `EXVM` with the core parser
      failpoint enabled to prove the VM path owns the covered grammar.
    - Keep operand-shape guardrail tests active.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm_scalar -- --nocapture` or the nearest focused
      scalar/arithmetic `EXVM` test filter
    - `cargo test -p vm runtime_expression_parser_rejects_postfix_indirect_tuple_for_68k_addressing -- --nocapture`
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the scalar-expression slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that makes `EXVM` authoritative for scalar and
    core arithmetic expressions only.
  - Definition of done:
    - Covered scalar/arithmetic expressions parse through `EXVM` without
      delegating to the core parser.
    - AST shape and diagnostics match the locked corpus.
    - Operand-shape guardrail tests remain green.

- [x] Item 5 - Add shifts, comparisons, bitwise operators, and logical operators
  - Source requirement or finding IDs: Broaden pure mathematical operator
    coverage under `EXVM`.
  - Validation: See Full quality gates for this item.
  - Definition of done: See item-specific Definition of done below.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs` or the `EXVM` parser module
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Add `<<`, `>>`, equality, inequality, ordered comparisons, bitwise `&`,
      `|`, `^`, logical `&&`, `||`, and logical xor with existing precedence,
      associativity, spans, and diagnostics.
    - Expand the core-parser-failpoint tests to prove these operators are parsed
      by `EXVM`.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm_operator -- --nocapture` or the nearest focused
      `EXVM` operator coverage filter
    - `cargo test -p vm runtime_expression_parser -- --nocapture`
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the operator-coverage slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that adds the remaining scalar mathematical
    operators to `EXVM`.
  - Definition of done:
    - Covered operator expressions parse through `EXVM` without direct core
      parser delegation.
    - Operator precedence, associativity, AST shape, and diagnostics match the
      locked corpus.

- [x] Item 6 - Add ternary mathematical conditional expressions
  - Source requirement or finding IDs: Complete the in-scope mathematical
    expression operator grammar without adding generic value-expression nodes.
  - Validation: See Full quality gates for this item.
  - Definition of done: See item-specific Definition of done below.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs` or the `EXVM` parser module
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Add `?:` parsing for mathematical conditionals where the condition,
      then-branch, and else-branch are all covered mathematical expressions.
    - Preserve the current missing-colon and malformed-branch diagnostics.
    - Keep call and placeholder expressions out of the covered `EXVM` grammar.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm_ternary -- --nocapture` or the nearest focused
      ternary `EXVM` test filter
    - `cargo test -p vm exvm -- --nocapture`
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the ternary-expression slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that adds ternary conditionals to the covered
    mathematical `EXVM` grammar.
  - Definition of done:
    - Ternary mathematical expressions parse through `EXVM` without direct core
      parser delegation.
    - Call and placeholder expressions remain out of covered `EXVM` grammar.

- [x] Item 7 - Add range and list expression nodes
  - Source requirement or finding IDs: User follow-up request that ranges and
    lists are part of the covered `EXVM` expression grammar.
  - Validation: See Full quality gates for this item.
  - Definition of done: See item-specific Definition of done below.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs` or the `EXVM` parser module
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Add range expression parsing, including inclusive/exclusive ranges and
      optional step expressions, using covered expression grammar for each range
      component.
    - Add list literal parsing using covered expression grammar for each element.
    - Preserve AST shape, spans, and diagnostics for malformed ranges and lists.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm_range_list -- --nocapture` or the nearest focused
      range/list `EXVM` test filter
    - `cargo test -p vm runtime_expression_generic_value_nodes_parse_but_reject_scalar_vm_compile -- --nocapture`
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the range/list slice summary, changed
    files, and validation logs before committing.
  - Commit outcome: One commit that adds range and list literals to covered
    `EXVM` grammar.
  - Definition of done:
    - Range and list expressions parse through `EXVM` without direct core parser
      delegation.
    - Scalar `EXPR` compilation still rejects non-scalar range/list nodes with
      the existing diagnostics.

- [x] Item 8 - Add struct literals, member access, and index access
  - Source requirement or finding IDs: User follow-up request that structs are
    part of the covered `EXVM` expression grammar; member/index access are the
    expression-level access forms needed for struct and list values.
  - Validation: See Full quality gates for this item.
  - Definition of done: See item-specific Definition of done below.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_expr_parser.rs` or the `EXVM` parser module
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation slice:
    - Add struct literal parsing with covered expression grammar for field
      values.
    - Add member access and index access as expression-level postfix forms.
    - Preserve AST shape, spans, and diagnostics for malformed struct literals,
      member access, and index access.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm_struct_access -- --nocapture` or the nearest
      focused struct/member/index `EXVM` test filter
    - `cargo test -p vm runtime_expression_parser_parses_struct_literal_expression -- --nocapture`
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the struct/access slice summary, changed
    files, and validation logs before committing.
  - Commit outcome: One commit that adds struct literals, member access, and
    index access to covered `EXVM` grammar.
  - Definition of done:
    - Struct literals, member access, and index access parse through `EXVM`
      without direct core parser delegation.
    - Scalar `EXPR` compilation still rejects non-scalar struct/access nodes
      with the existing diagnostics.

- [x] Item 9 - Lock explicit handling for remaining out-of-scope value nodes
  - Source requirement or finding IDs: Calls and placeholders are not requested
    for this plan and must not drift in during execution.
  - Validation: See Full quality gates for this item.
  - Definition of done: See item-specific Definition of done below.
  - Expected files:
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `documentation/opforge-assembler-vm-path-guide-v0_1.md`
  - Implementation slice:
    - Add strict-mode tests proving placeholder nodes and function/call
      expressions are not part of the covered `EXVM` grammar for this plan.
    - Preserve any existing non-strict compatibility behavior only behind an
      explicit out-of-scope compatibility path, or return deterministic
      unsupported-feature diagnostics if strict mode is active.
    - Do not implement parsing for calls or placeholders in this plan.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm_remaining_value_out_of_scope -- --nocapture` or the
      nearest focused remaining-value out-of-scope filter
    - `cargo test -p vm runtime_expression_parser_parses_call_with_list_and_placeholder_args -- --nocapture`
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the remaining out-of-scope value-node
    summary, changed files, and validation logs before committing.
  - Commit outcome: One commit that locks calls and placeholders out of the
    covered `EXVM` grammar without broadening parser behavior.
  - Definition of done:
    - Calls and placeholders have explicit strict-mode tests and outcomes.
    - Later work cannot silently classify calls or placeholders as covered
      expression grammar.

- [ ] Item 10 - Route PRVM expression sub-calls through authoritative `EXVM` coverage
  - Source requirement or finding IDs: Full VM-based parsing path must use the
    typed PRVM to `EXVM` expression boundary.
  - Validation: See Full quality gates for this item.
  - Definition of done: See item-specific Definition of done below.
  - Expected files:
    - `crates/opforge-vm/src/vm_opasm.rs`
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_model_core.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Implementation slice:
    - Ensure `ParseOperandExprRange` calls enter `EXVM` for covered expression
      token ranges.
    - Preserve opasm operand-shape adapters around the `EXVM` sub-call for
      immediate wrappers, m68k addressing, bitfield suffixes, register pairs,
      and other CPU-family operand syntax.
    - Add focused m68k and non-m68k tests that combine operand shapes with
      inner mathematical expressions.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture`
    - `cargo test -p vm exvm -- --nocapture`
    - `cargo test -p asm motorola68000 -- --nocapture` or the nearest focused
      m68k operand-boundary filter
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the PRVM-subcall routing slice summary,
    changed files, and validation logs before committing.
  - Commit outcome: One commit that makes PRVM expression sub-calls use the
    authoritative `EXVM` path for covered expression grammar.
  - Definition of done:
    - Covered expression token ranges from PRVM are parsed by `EXVM`.
    - CPU-family operand-shape parsing remains outside `EXVM`.
    - Previously fixed m68k operand-boundary examples stay green.

- [ ] Item 11 - Remove `DelegateCore` for covered `EXVM` grammar and harden strict mode
  - Source requirement or finding IDs: End-state requirement that VM-owned
    expression parsing does not silently fall back to direct core parsing for
    covered grammar.
  - Validation: See Full quality gates for this item.
  - Definition of done: See item-specific Definition of done below.
  - Expected files:
    - `crates/opforge-package/src/package.rs`
    - `crates/opforge-vm/src/vm_opcore.rs`
    - `crates/opforge-vm/src/runtime_model_core.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Implementation slice:
    - Reject or remove `DelegateCore` for all covered mathematical expression
      grammar in strict VM execution mode.
    - Preserve an explicit compatibility or unsupported-feature diagnostic only
      for cases Item 9 classifies as out of scope.
    - Add tests that enable the core parser failpoint and prove all covered
      grammar still passes through `EXVM`.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo test -p vm exvm -- --nocapture`
    - `cargo test -p vm execution_model_parse_expression_for_assembler_certified_path_bypasses_core_parser_failpoint -- --nocapture`
    - `cargo test -p asm expression -- --nocapture` or the nearest focused asm
      expression contract filter
    - `cargo test --locked`
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, the strict-mode slice summary, changed
    files, and validation logs before committing.
  - Commit outcome: One commit that removes silent host parser fallback for
    covered `EXVM` grammar.
  - Definition of done:
    - Covered expression grammar remains green with direct core parser failpoints
      enabled.
    - Unsupported/out-of-scope cases fail with deterministic diagnostics.

- [ ] Item 12 - Refresh documentation and run full quality gates
  - Source requirement or finding IDs: Final documentation and validation for
    the `EXVM` expression grammar implementation plan.
  - Validation: See Full quality gates for this item.
  - Definition of done: See item-specific Definition of done below.
  - Expected files:
    - `documentation/opforge-assembler-vm-path-guide-v0_1.md`
    - `documentation/vm-boundary-protocol-v1.md`
    - `documentation/opForge-reference-manual.md`
    - this plan, for progress evidence only
    - release notes only if a new release tag is being created; otherwise do not
      update release notes
  - Implementation slice:
    - Update current VM path documentation to describe `EXVM` as the expression
      parser VM and `EXPR` as the portable expression evaluator VM.
    - Document the PRVM to `EXVM` boundary and the operand-shape exclusion.
    - Record final validation evidence in this plan.
  - Full quality gates:
    - `cargo fmt --all --check`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo test --locked`
    - `cargo test --workspace`
    - `cargo audit --no-fetch`
    - focused FS-UAE/native evidence only if this plan introduces native `EXVM`
      execution; otherwise explicitly record that no native path changed
  - Plan-compliance review evidence: Run `plan-compliance-reviewer` with the
    active `AGENTS.md`, this plan path, final docs/validation summary, changed
    files, and validation logs before committing.
  - Commit outcome: One commit that updates documentation and records final
    validation evidence without introducing new parser behavior.
  - Definition of done:
    - Documentation consistently names `EXVM` and distinguishes it from `EXPR`.
    - The plan has final validation evidence.
    - Full quality gates pass or any pre-existing baseline exception is recorded
      precisely with evidence.

## Milestones

- [x] Milestone 1 - `EXVM` naming established
  - Includes Work item 1.
  - Done when the package/runtime contract name is `EXVM`, tests prove package
    round-trip compatibility for the new chunk name, and expression behavior is
    unchanged.

- [x] Milestone 2 - Contract guardrails locked
  - Includes Work item 2.
  - Done when the expression corpus and operand-shape negative corpus are in
    place before parser ownership changes.

- [x] Milestone 3 - Minimal deterministic `EXVM` parser path exists
  - Includes Work items 3 and 4.
  - Done when `EXVM` owns scalar/core arithmetic parsing without direct core
    parser delegation.

- [x] Milestone 4 - Scalar, conditional, and aggregate grammar is covered
  - Includes Work items 5, 6, 7, and 8.
  - Done when all operator, ternary, range, list, struct, member, and index
    grammar covered by this plan is implemented by `EXVM`.

- [ ] Milestone 5 - Remaining out-of-scope nodes are explicit and PRVM uses `EXVM`
  - Includes Work items 9 and 10.
  - Done when PRVM expression token ranges route through `EXVM` while operand
    shape parsing remains in the opasm layer, and remaining value nodes have
    explicit out-of-scope strict-mode behavior.

- [ ] Milestone 6 - Covered grammar no longer delegates to the core parser
  - Includes Work items 11 and 12.
  - Done when covered grammar passes with core parser failpoints enabled,
    documentation is current, and full validation has been recorded.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- no plan-driven work may start before this plan receives a `PASS` from the
  plan-quality reviewer
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- if any slice discovers that CPU-family operand grammar is required inside
  `EXVM`, stop and update the plan instead of widening scope silently
- if any slice discovers that `EXPP` compatibility must be preserved for active
  artifacts, stop and update Work item 1 before implementing a compatibility
  bridge
