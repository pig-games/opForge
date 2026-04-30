# opForge opcore Expression / opasm Operand Shape Migration Plan v0.1

## Metadata

- Source: user request on 2026-04-30 to identify and migrate non-expression
  shapes currently produced by the opcore expression VM into opasm, based on the
  current implementation rather than assumptions.
- Mode: migration
- Owner: Codex

## Objective

Move assembler operand-shape parsing out of the opcore expression VM and into
opasm, while keeping opcore focused on generic expression parsing and portable
expression evaluation.

The target boundary is:

- opcore expression parsing owns generic scalar expression syntax and expression
  evaluation inputs.
- opasm owns assembler operand wrappers and operand shape syntax.
- CPU/family-specific helpers own operand forms that only make sense for one
  family or dialect.

## Current Implementation Facts

- `crates/opforge-vm/src/vm_opcore.rs` exposes
  `parse_expression_tokens(...)`, which directly calls
  `RuntimeExpressionParser::parse_expr_from_tokens(...)`.
- `crates/opforge-vm/src/runtime_expr_parser.rs` currently parses generic
  operator precedence, literals, identifiers, `$`, strings, unary/binary/
  ternary expressions, ranges, lists, struct literals, calls, member access,
  index access, and several assembler operand shapes.
- `crates/opforge-core/src/parser.rs` defines one shared `Expr` enum that mixes
  generic expression nodes and assembler operand-shape nodes:
  `Register`, `Indirect`, `Immediate`, `IndirectLong`, and `Tuple` are explicitly
  documented as register/immediate/indirect/complex-indirect shapes.
- `crates/opforge-core/src/expr_vm.rs` is the best current signal for the
  portable scalar expression subset: it compiles numbers, identifiers/registers
  as symbols, `$`, unary/binary/ternary expressions, and strings; it rejects
  list, index, member, struct literal, call, placeholder, immediate, tuple, and
  range; it currently strips `Indirect` and `IndirectLong` by compiling their
  inner expression.
- `crates/opforge-vm/src/vm_opasm.rs` already has an opasm operand-shape hook,
  `parse_operand_expr_range(...)`, which first recognizes selected assembler
  operand shapes and then falls back to
  `vm_opcore::parse_expr_with_vm_contract_and_boundary(...)`.
- `vm_opasm.rs` already handles several operand-specific forms before opcore
  fallback: indexed register postfix operands, m68k register pairs, and m68k
  bitfield suffix operands.

## Boundary Classification

### Stay in opcore expression parsing now

- Numeric literals.
- Identifiers as symbols.
- `$` current-address expression.
- String literals, because the portable expression VM has explicit string
  literal support.
- Unary operators, including generic assembler low/high-byte prefix operators
  `<expr` and `>expr`.
- Binary operators and precedence.
- Ternary expressions.
- Parenthesized grouping, but only as grouping: `(expr)` should return `expr`.

### Stay in opcore only if/when evaluation semantics exist

These are generic-looking but currently rejected by the portable expression VM.
They should not be expanded until scalar/value semantics are intentionally
defined:

- `Expr::StructLiteral`.
- `Expr::Member`.
- `Expr::Index`.
- `Expr::Call`.
- `Expr::List`.
- `Expr::Range`.

The migration should not deepen these features. It should only avoid confusing
them with assembler operand shapes.

### Move to opasm as generic assembler operand syntax

These are assembler-wide operand wrappers or operand shapes, not CPU-specific
by themselves:

- `#expr` -> `Expr::Immediate(expr)`.
- Parenthesized operand indirection: `(expr)` -> `Expr::Indirect(expr)` when
  parsing an operand, not when parsing a standalone expression.
- Bracketed operand indirection: `[expr]` -> `Expr::IndirectLong(expr)` when
  parsing an operand, subject to family support.
- Parenthesized comma operand forms currently represented as
  `Expr::Indirect(Expr::Tuple(...))`.
- Bracketed comma operand forms currently represented as
  `Expr::IndirectLong(Expr::Tuple(...))`.

### Move to opasm with CPU/family-specific gates

These forms should be detected in opasm only with CPU/family/mnemonic/dialect
context:

- 680x0 indexed/addressing tuples such as `4(A0,D1.W)` and
  `(,A0,D1.L*4)`.
- 680x0 postincrement/predecrement-like shapes such as `(A0)+` and `-(A7)`.
- 680x0 bitfield suffixes such as `{offset:width}`, which are already partly
  handled by `parse_bitfield_suffix_operand(...)`.
- 680x0 register pair syntax for `CAS2` and long divide mnemonics, which is
  already partly handled by `parse_register_pair_operand(...)`.
- 6502-family indexed/indirect wrappers such as `(expr,X)`, `(expr),Y`, and
  `[expr],Z`.
- Z80/8080-family register and indirect-register operand shapes, including
  dialect-sensitive register naming.
- 6809 indexed addressing syntax.

### Treat as symbols in opcore, registers in opasm

`TokenKind::Register` currently becomes `Expr::Register` in
`RuntimeExpressionParser`. That is CPU-aware token classification leaking into
the generic expression layer. During migration, opcore expression parsing should
either:

- normalize register tokens to identifiers for generic expression parsing, or
- preserve `Expr::Register` only as a compatibility symbol node while ensuring
  opasm, not opcore, decides whether the token is a register operand.

The second path is the safer first slice because `expr_vm.rs` already treats
`Expr::Register` like a symbol for portable expression compilation.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Keep the classic parser and runtime VM parser compatible during migration;
  do not make the VM path diverge in a way that breaks current rollout families.
- Do not widen this plan into AST redesign. The first migration should move
  production behavior to the right layer while keeping the shared `Expr` enum
  stable where possible.
- Do not touch parser VM v2 branch work unless explicitly coordinated. This
  plan should prefer `vm_opasm.rs`, `runtime_expr_parser.rs`, and focused tests
  in the current worktree.
- Each work item is one commit-sized slice and must end in a commit before the
  next item starts.

## Work Items

- [x] Item 1: add opasm generic operand wrappers for `#expr`, `(expr)`, and
  `[expr]`
  - Validation: focused opasm operand parser tests plus normal formatting,
    clippy, and diff checks.
  - Definition of done: opasm recognizes generic operand wrappers before
    falling back to opcore expression parsing.
  - Source requirement or finding IDs: move generic assembler operand wrappers
    out of opcore and into `parse_operand_expr_range(...)`.
  - Expected files:
    - `crates/opforge-vm/src/vm_opasm.rs`
    - focused opasm parser tests
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` opasm operand parser tests
    - `cargo clippy -p vm -- -D warnings`
    - `git diff --check`
  - Plan-compliance review evidence:
    - show that opasm wraps operand slices while nested expression slices are
      still parsed by opcore.
  - Commit outcome:
    - one commit where opasm recognizes immediate, parenthesized indirect, and
      bracketed indirect operands before opcore fallback.
  - Definition of done:
    - opasm owns generic immediate, parenthesized-indirect, and
      bracketed-indirect operand wrappers before opcore narrows its standalone
      expression surface.

- [x] Item 2: move parenthesized/bracketed comma operand tuples into opasm
  - Validation: focused opasm operand tuple tests plus normal formatting,
    clippy, and diff checks.
  - Definition of done: tuple-backed indirect shapes are produced only from
    operand parsing, not standalone opcore expression parsing.
  - Source requirement or finding IDs: current opcore VM produces
    `Expr::Indirect(Expr::Tuple(...))` and
    `Expr::IndirectLong(Expr::Tuple(...))` for comma forms.
  - Expected files:
    - `crates/opforge-vm/src/vm_opasm.rs`
    - focused opasm tuple/indirect operand tests
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` opasm operand tuple tests
    - `cargo clippy -p vm -- -D warnings`
    - `git diff --check`
  - Plan-compliance review evidence:
    - show comma tuple production happens only from operand parsing, not from
      standalone opcore expression parsing.
  - Commit outcome:
    - one commit where opasm builds tuple-backed indirect operands from operand
      contexts.
  - Definition of done:
    - `(a,b)` and `[a,b]` are not generic opcore expression syntax.
    - opasm still preserves the AST shapes expected by selector/lowering code.

- [x] Item 3: gate CPU/family-specific operand shapes in opasm
  - Validation: focused family operand parser tests plus normal formatting,
    clippy, and diff checks.
  - Definition of done: family-only operand syntax is guarded by CPU/family/
    mnemonic/dialect context and does not affect unrelated families.
  - Source requirement or finding IDs: decide carefully whether each migrated
    form is generic assembler syntax or CPU/family-specific.
  - Expected files:
    - `crates/opforge-vm/src/vm_opasm.rs`
    - any focused family helper module if the existing file becomes too dense
    - focused family parser tests
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` family operand parser tests
    - `cargo clippy -p vm -- -D warnings`
    - `git diff --check`
  - Plan-compliance review evidence:
    - show each CPU/family-specific form is guarded by CPU/family/mnemonic/
      dialect context and does not affect unrelated CPU families.
  - Commit outcome:
    - one commit that explicitly gates at least the 680x0 operand forms already
      represented by postfix tuple/postincrement/register-pair/bitfield logic.
  - Definition of done:
    - 680x0-only syntax does not parse as generic opcore expression syntax.
    - existing 680x0 opasm behavior remains covered.

- [ ] Item 4: make opcore VM parentheses generic for standalone expressions
  - Validation: focused opcore expression parser tests plus normal formatting,
    clippy, and diff checks.
  - Definition of done: standalone opcore VM grouping returns the inner generic
    expression and does not construct operand indirection.
  - Source requirement or finding IDs: boundary classification; current
    `runtime_expr_parser.rs` wraps `(expr)` as `Expr::Indirect(expr)`.
  - Expected files:
    - `crates/opforge-vm/src/runtime_expr_parser.rs`
    - focused VM expression parser tests
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` expression parser tests
    - focused opasm operand wrapper tests from Items 1-3
    - `cargo clippy -p vm -- -D warnings`
    - `git diff --check`
  - Plan-compliance review evidence:
    - show opasm already preserves operand wrapper behavior from Items 1-3, so
      this commit changes only standalone opcore expression grouping.
  - Commit outcome:
    - one commit where `vm_opcore::parse_expression_tokens("(1+2)")` produces
      the same scalar expression shape as `1+2`, not `Expr::Indirect`.
  - Definition of done:
    - standalone opcore VM expression grouping is generic.
    - assembler operand behavior remains preserved by opasm-owned parsing from
      the earlier commits.

- [ ] Item 5: narrow remaining opcore VM operand-only expression surface
  - Validation: focused opcore expression parser tests plus normal formatting,
    clippy, and diff checks.
  - Definition of done: standalone opcore expression parsing no longer
    constructs remaining operand-only expression shapes.
  - Source requirement or finding IDs: after opasm owns wrappers, tuples, and
    family-gated operand syntax, opcore VM should reject or stop producing
    non-expression shapes.
  - Expected files:
    - `crates/opforge-vm/src/runtime_expr_parser.rs`
    - focused opcore expression parser tests
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` opcore expression parser tests
    - `cargo clippy -p vm -- -D warnings`
    - `git diff --check`
  - Plan-compliance review evidence:
    - show non-expression tokens such as `#`, operand-only brackets,
      placeholder tuple slots, and postfix indirect syntax are rejected or left
      to opasm, while Items 1-4 preserve operand behavior.
  - Commit outcome:
    - one commit where `RuntimeExpressionParser` no longer constructs
      `Expr::Immediate`, `Expr::Indirect`, `Expr::IndirectLong`, or
      `Expr::Tuple` from standalone opcore expression parsing beyond the
      grouping change already made in Item 4.
  - Definition of done:
    - opcore VM output is aligned with the portable expression VM scalar subset
      except for explicitly deferred generic-value features.

- [ ] Item 6: classify unresolved generic-value nodes and lock diagnostics
  - Validation: focused expression compile/evaluation tests plus normal
    formatting, clippy, and diff checks.
  - Definition of done: generic-looking but non-evaluable nodes have an
    intentional parse/eval diagnostic boundary.
  - Source requirement or finding IDs: `StructLiteral`, `Member`, `Index`,
    `Call`, `List`, and `Range` look generic but are currently rejected by
    portable expression compilation/evaluation.
  - Expected files:
    - `crates/opforge-vm/src/runtime_expr_parser.rs`
    - `crates/opforge-core/src/expr_vm.rs` only if diagnostics need tightening
    - focused expression compile/evaluation tests
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p vm` and/or `cargo test -p opcore` expression VM
      tests
    - `cargo clippy -p vm -- -D warnings`
    - `cargo clippy -p opcore -- -D warnings` if `expr_vm.rs` changes
    - `git diff --check`
  - Plan-compliance review evidence:
    - show no new aggregate semantics were invented during migration.
  - Commit outcome:
    - one commit documenting and testing whether these nodes are accepted at
      parse time but rejected at compile/eval time, or rejected earlier.
  - Definition of done:
    - opcore has an intentional, tested stance for each generic-looking but
      currently non-evaluable node.

- [ ] Item 7: align classic parser or isolate VM-only migration path
  - Validation: focused opcore parser tests, VM parser tests, relevant assembler
    smoke tests, plus normal formatting, clippy, and diff checks.
  - Definition of done: classic parser and VM parser agree on the new
    opcore/opasm boundary or intentionally stage the difference.
  - Source requirement or finding IDs: `crates/opforge-core/src/parser.rs` has
    the same mixed parsing behavior as the runtime expression parser.
  - Expected files:
    - `crates/opforge-core/src/parser.rs`
    - `crates/opforge-vm/src/vm_opasm.rs`
    - focused compatibility tests
  - Full quality gates:
    - `cargo fmt --all --check`
    - focused `cargo test -p opcore` parser tests
    - focused `cargo test -p vm` VM parser tests
    - relevant assembler smoke tests for affected CPU families
    - `cargo clippy -p vm -- -D warnings`
    - `cargo clippy -p opcore -- -D warnings`
    - `git diff --check`
  - Plan-compliance review evidence:
    - show classic parser behavior and VM parser behavior either match under
      the new boundary or the divergence is intentionally scoped behind rollout.
  - Commit outcome:
    - one commit that prevents long-term parser/VM disagreement.
  - Definition of done:
    - the canonical Rust parser path and runtime VM path agree on what is
      opcore expression syntax vs opasm operand syntax.

## Milestones

- [x] Milestone 1: opasm owns generic immediate/indirect/bracketed operand
  wrappers.
- [x] Milestone 2: opasm owns tuple-backed operand shapes.
- [x] Milestone 3: CPU/family-specific operand shapes are gated in opasm.
- [ ] Milestone 4: opcore expression VM no longer treats simple parentheses as
  indirect operand syntax.
- [ ] Milestone 5: opcore expression parser and portable expression evaluator
  have an intentional, tested scalar/value boundary.
- [ ] Milestone 6: classic parser and VM parser boundary behavior are aligned
  or intentionally staged.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping

## Notes For Future Native Work

This migration should make the future native 68020 implementation simpler, not
larger. The native tokenizer and opcore expression VM should only need to
recognize generic tokens and generic expression operators. 68020 addressing
forms should be tested against opasm/CPU operand-shape behavior, not against
opcore expression parsing.
