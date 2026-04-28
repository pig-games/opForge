# opForge PRVM v2 opasm Statement Parser — Rust-First Implementation Spec & Plan v0.1

## Metadata

- Source: User request on 2026-04-26 to consolidate the PRVM v2 design into an
  implementation-ready document; existing draft spec
  `documentation/opForge-extended-parser-vm-instruction-set-spec-v0_1.md`
  (v0.1-draft); engine/processor partition decisions captured in chat with
  open questions Q1–Q9 resolved
- Companion architectural reference:
  `documentation/opforge-assembler-vm-path-guide-v0_1.md`
- Native-port template:
  `documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md`
- Mode: `implementation` (Rust-side only; native 68020 port deferred to a
  follow-up plan triggered by WI-7 of this plan)
- Owner: Codex

## Objective

Replace the current PRVM v1 envelope-delegation parser VM with a fine-grained
PRVM v2 opasm statement parser whose contract mirrors the tokenizer VM
contract: typed stream input, explicit VM-owned token cursor and AST builder
primitives, runtime-mediated cross-contract sub-calls into the opcore
expression parser.

The Rust v2 implementation is the source-of-truth target. The native 68020
port is **out of scope for this plan** and is reserved for a follow-up plan
that may begin only after PRVM v2 has reached behavioral parity with the
combined `PRVM v1 + Rust opasm parsing helpers` surface defined in §3 below.

This plan is opasm-statement-only. It does not move opcore line classification,
opcore-owned directives (conditionals, modules, includes, namespaces, macro
shells), or opcore expression parsing into the opasm contract.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding
  at all times during plan execution.
- Scope is limited to the opasm statement parser surface that runs **after**
  opcore has classified a line and dispatched
  `ProcessingRequestKind::Processor { processor: "asm", kind: "statement" }`.
- The plan must not redefine opcore line routing, must not absorb opcore-owned
  directives, and must not embed expression-parsing logic into the opasm
  contract.
- PRVM v1 is treated as transient scaffolding only: it is deleted incrementally
  as v2 absorbs each line shape, and fully removed in WI-5. Backward
  compatibility with the v1 program format is **not** a goal.
- Cross-contract sub-calls (opasm → opcore expression parser) are mediated
  exclusively by the runtime through typed sub-call opcodes whose semantics
  are fixed by the receiving contract's opcode-version constant. Bytecode
  of one contract must never embed bytecode of another.
- The runtime entry-boundary precondition (opasm v2 only runs when invoked via
  the typed processor request) is enforced at runtime, not just documented.
- Determinism is mandatory: explicit bytecode/token/step/value-stack budgets;
  no hidden reparsing of source text outside declared opcore expression
  sub-calls.
- This plan must not become active until `plan-quality-reviewer` returns
  `PASS`.
- One active work item at a time.
- Each work item ends in exactly one new commit before the next item begins.
- Full quality gates (`cargo fmt`, `cargo clippy --all-targets --all-features
  -- -D warnings`, `cargo audit`, `cargo test --workspace`) are mandatory
  before each commit.
- `plan-compliance-reviewer` must pass before each plan-driven commit.
- No fixture/reference regeneration except where the WI explicitly captures a
  deliberate, expected-by-design behavior change. New unexpected diffs are
  treated as regressions.

## Resolved Design Decisions (formerly Open Questions Q1–Q9)

These resolutions amend the v0.1 draft spec
(`documentation/opForge-extended-parser-vm-instruction-set-spec-v0_1.md`).
Spec authors should re-issue that document at v0.2 reflecting these
resolutions; until then, this plan is the authoritative source.

### Q1 — Opcode byte assignment

Fixed-byte opcode map with reserved gaps for additive future growth:

| Range        | Family                     |
|--------------|----------------------------|
| `0x00–0x0F`  | Control flow               |
| `0x10–0x1F`  | Token inspection           |
| `0x20–0x2F`  | Token movement             |
| `0x30–0x3F`  | Value loading              |
| `0x40–0x4F`  | Statement helpers          |
| `0x50–0x5F`  | Cross-contract sub-calls (opcore expression) |
| `0x60–0x6F`  | AST builders               |
| `0x70–0x7F`  | Diagnostics                |
| `0x80–0xFF`  | Reserved                   |

`0x00` is `End`. Unassigned bytes within an in-use range fail with the typed
"invalid parser opcode" diagnostic.

### Q2 — Rollback semantics

`Checkpoint` snapshots: token cursor index, AST builder state, value-stack
high-water-mark. `Rollback` restores all three. `Commit` discards the
checkpoint. Maximum live checkpoint depth is `4`; exceeding it fails with a
typed "checkpoint depth exceeded" diagnostic.

### Q3 — First-slice ordering

PRVM v2 absorbs line shapes in this order (one shape per work item where the
WI table below maps it):

1. Plain instruction statements with optional labels (`LineAst::Statement`).
2. Data directives that already normalize to `LineAst::Statement`
   (`.byte`/`.db`, `.word`/`.dw`, `.long`, `.text`, `.null`, `.ptext`,
   `.fill`, `.res`, `.ds`, `.align`).
3. Simple positional/control statements (`*=`, `.org`, `name = expr`).
   `.org` is tied to the existing `parse_star_or_org_envelope_from_tokens`
   helper and migrates with `*=`, not with the data-directive shapes.
4. Block-scoped opasm-owned directive heads/tails (`.region`, `.section`/
   `.endsection`, `.encode`/`.endencode`, `.meta`/`.endmeta`,
   `.output`/`.endoutput`).

Deferred to a later plan: `.place`, `.pack`, 65816 runtime-state directives
(`.al`/`.as`/`.xl`/`.xs`/`.assume`/`.databank`/`.dbank`/`.dpage`), and inline
`.meta.output.*` shorthand.

### Q4 — Operand-list opcode shape

Two distinct opcodes, no mega-op:

- `ScanTopLevelCommaBoundaries` — pure opasm-side scan over the typed token
  slice. Produces a boundary table on the value stack.
- `ParseOperandExprRange(start_tok_idx, end_tok_idx)` — typed cross-contract
  sub-call. Resolved by the runtime against the active opcore expression
  parser program over the bounded token sub-slice. Returns a typed
  `Expr` (or `Expr::Error` per Q5) onto the opasm value stack.

The opasm program loops `ParseOperandExprRange` over the boundary table.

### Q5 — Expression error handling

PRVM v2 preserves the existing `Expr::Error` embedding for delegated statement
flows that already expect it (current Rust behavior in
`crates/opforge-asm/src/asmline_instruction.rs`). Normalization into terminal
parser diagnostics is reserved for a v3 follow-up and is **out of scope** for
this plan.

### Q6 — Native entry symbols (reserved, not implemented in this plan)

Reserved symbol names so the future native port plan does not have to
redesign the cross-call ABI:

- `prvm_run_68000` — opasm statement parser (this plan's follow-up).
- `expvm_run_68000` — opcore expression parser (separate future plan).
- `corevm_run_68000` — opcore directive interpreter (separate future plan,
  see Q9).

68020 is the baseline CPU for all three when their native plans are authored.

### Q7 — PRVM v1 lifecycle

PRVM v1 is scaffolding only. Each work item that moves a line shape into v2
deletes the matching v1 envelope opcode and its `parse_*_envelope_from_tokens`
helper in the same commit. WI-5 removes the residual `ParserVmOpcode` v1
enum, `PARSER_VM_OPCODE_VERSION_V1` constant, and any remaining v1 fallback
paths.

### Q8 — Engine/Processor partition (three-VM-contract model)

Three first-class, mutually independent VM contracts:

| Contract              | Owner   | Opcode-version constant                          | Status            |
|-----------------------|---------|--------------------------------------------------|-------------------|
| Expression parser     | opcore  | `EXPR_PARSER_VM_OPCODE_VERSION_*` (existing)     | Authoritative     |
| opcore directives     | opcore  | `OPCORE_DIRECTIVE_VM_OPCODE_VERSION_*` (reserved)| Future plan (Q9)  |
| opasm statement parser| opasm   | `PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT`    | This plan         |

Cross-contract invocation rules:

- An opasm v2 program may not declare or execute expression-parsing opcodes.
- An opcore expression program may not declare or execute statement-parsing
  opcodes.
- Cross-contract sub-calls go through the runtime, never via direct bytecode
  embedding or via opaque "callback into Rust" host hooks.
- Entry-boundary precondition: the opasm v2 executor refuses to run unless
  invoked through `Processor { processor: "asm", kind: "statement" }`. Any
  other invocation fails with the typed cross-boundary diagnostic.

### Q9 — opcore directive surface (deferred)

opcore directives — conditionals (`.if`/`.elif`/`.else`/`.endif`), modules
(`.module`/`.endmodule`), includes (`.include`), namespaces (`.namespace`),
macro shells (`.macro`/`.endmacro`), and any future source-structure
construct — are **out of scope for this plan**. They will continue to be
consumed by the existing Rust opcore-directive code path until a separate
opcore-directive VM contract plan lands. Decision rule for future
classification: *does this directive concern source structure (opcore) or
output emission/assembler state (opasm)?*

The opasm v2 contract must not declare or accept opcore-directive shapes.

## opasm v2 Opcode Inventory (normative)

Mnemonics below define the contract surface; exact byte assignments follow
the Q1 range table. Byte-level finalization happens in WI-1.

- Control flow: `End`, `Jump`, `JumpIfTrue`, `JumpIfFalse`, `Checkpoint`,
  `Rollback`, `Commit`.
- Token inspection: `PeekKind`, `PeekIdentifier`, `PeekOperator`, `IsEol`.
- Token movement: `Advance`, `ConsumeKind`, `ConsumeOperator`.
- Value loading: `LoadIdentifier`, `LoadSpan`, `LoadTokenText`.
- Statement helpers: `ParseOptionalLeadingLabel`,
  `ScanTopLevelCommaBoundaries`, `RequireNoTrailingTokens`.
- Cross-contract sub-calls: `ParseOperandExprRange` (single range; loop in
  the opasm program for full lists).
- AST builders: `BeginStatement`, `SetLabel`, `SetMnemonic`, `PushOperand`,
  `FinishLine`.
- Diagnostics: `EmitDiag`, `EmitDiagIfNoResult`, `Fail`.

## Diagnostics Contract (delta vs draft spec)

Adds the following typed codes on top of the v0.1 draft spec §7:

- `parser.opasm_v2.entry_boundary_violation` — opasm v2 executor invoked
  without the required typed processor request.
- `parser.opasm_v2.forbidden_cross_contract_opcode` — opasm v2 program
  attempted to execute an opcode reserved for a different contract.
- `parser.opasm_v2.unknown_subcall_contract` — runtime could not resolve the
  target opcore expression contract for a cross-contract sub-call.
- `parser.opasm_v2.subcall_version_mismatch` — target opcore expression
  contract version is incompatible with the calling opasm program.
- `parser.opasm_v2.misrouted_opcore_directive` — opcore-directive line shape
  reached opasm v2 instead of the opcore-directive code path.
- `parser.opasm_v2.checkpoint_depth_exceeded` — more than 4 live checkpoints.

## Architectural References (read before starting WI-1)

- PRVM v1 executor: [crates/opforge-vm/src/execution_model/parser_vm.rs](../../crates/opforge-vm/src/execution_model/parser_vm.rs)
- v1 opcode enum: [crates/opforge-package/src/package.rs](../../crates/opforge-package/src/package.rs) (lines defining `ParserVmOpcode` and `from_u8`)
- v1 envelope helpers: [crates/opforge-vm/src/vm_opasm_parse.rs](../../crates/opforge-vm/src/vm_opasm_parse.rs)
- Default v1 program emission: [crates/opforge-vm/src/builder.rs](../../crates/opforge-vm/src/builder.rs)
- Opcore expression sub-contract: [crates/opforge-vm/src/vm_opcore.rs](../../crates/opforge-vm/src/vm_opcore.rs) (`parse_expr_with_vm_contract`)
- Opcore delegation seam: [crates/opforge-core/src/parser_opcore_requests.rs](../../crates/opforge-core/src/parser_opcore_requests.rs)
- Tokenizer VM as port template: [crates/opforge-package/src/package.rs](../../crates/opforge-package/src/package.rs) (`TokenizerVmOpcode`, `TokenizerVmStreamDescriptor`)
- Asmline runtime-expression path (preserves `Expr::Error` per Q5):
  [crates/opforge-asm/src/asmline_instruction.rs](../../crates/opforge-asm/src/asmline_instruction.rs) (lines 314–520)

## Work Items

**Note on `Expected files` lists.** Each WI's `Expected files` section lists
the **minimum anchors** the slice must touch. It is not exhaustive: any
additional file that directly imports, dispatches, or tests a v1 opcode being
deleted in the same commit (notably the live executor at
`crates/opforge-vm/src/execution_model/parser_vm.rs`, runtime tests at
`crates/opforge-vm/src/runtime_tests.rs`, and package tests at
`crates/opforge-package/src/package/tests.rs`) must be updated in the same
commit to keep the tree compiling and the quality gate green. The
`plan-compliance-reviewer` is expected to allow such follow-on edits when
they are mechanically required by the listed deletions.

- [x] **Work item 1**: land the PRVM v2 opcode enum, executor scaffolding, and runtime-mediated cross-contract sub-call mechanism alongside v1
  - Source requirement or finding IDs: Q1, Q2, Q4, Q8 resolutions; entry-boundary precondition from Q8
  - Validation: see detailed validation commands and recorded evidence below.
  - Definition of done:
    - `ParserVmOpcodeV2` enum with the inventory listed in §"opasm v2 Opcode Inventory" and Q1 byte assignments lands in `crates/opforge-package/src/package.rs` next to existing v1 enum
    - `PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT` constant added
    - new v2 executor module (e.g., `crates/opforge-vm/src/execution_model/parser_vm_v2.rs`) implements: opcode dispatch, token cursor, value stack with budgeted depth, AST builder for `LineAst::Statement`, checkpoint/rollback/commit with depth ≤ 4, and the typed `ParseOperandExprRange` cross-contract sub-call routed through the runtime against the active opcore expression program
    - runtime entry-boundary precondition: the v2 executor refuses to run unless invoked through `Processor { processor: "asm", kind: "statement" }`; violation produces `parser.opasm_v2.entry_boundary_violation`
    - typed diagnostics from §"Diagnostics Contract" wired into the executor
    - v1 executor and v1 enum **remain intact and authoritative** for all line shapes after this WI; v2 is dormant (not yet selected by `builder.rs`)
    - encode/decode round-trip tests for v2 opcodes; executor unit tests for cursor/checkpoint/sub-call mechanics using a stub expression contract
  - Validation:
    - `cargo test -p opforge-package parser_vm_v2 -- --nocapture`
    - `cargo test -p opforge-vm execution_model::parser_vm_v2 -- --nocapture`
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test --workspace`
  - Expected files (minimum anchors; see Expected-files note at the top of the Work Items section):
    - `crates/opforge-package/src/package.rs`
    - `crates/opforge-package/src/package/tests.rs`
    - `crates/opforge-vm/src/execution_model/parser_vm_v2.rs` (new)
    - `crates/opforge-vm/src/execution_model/mod.rs`
    - `crates/opforge-vm/src/runtime_model_core.rs` (sub-call dispatcher hook)
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to v2 enum + dormant executor + sub-call plumbing; v1 still owns all live parses
    - 2026-04-27 execution evidence: `cargo fmt --all`, `cargo test -p package parser_vm_v2 -- --nocapture`, `cargo test -p vm execution_model::parser_vm_v2 -- --nocapture`, `cargo test -p vm execution_model_parser_vm_v2_expr_subcall_contract_validation_is_runtime_mediated -- --nocapture`, `cargo clippy --all-targets --all-features -- -D warnings`, and `cargo audit` passed. `cargo test --workspace` is accepted for this WI with the single reproduced baseline failure `asm::tests::examples_match_reference_outputs` waived by the user on 2026-04-27; the failure was reproduced on `main` and is the existing `motorola68000/amigaos/tkpkg/tkpkg_debug_cli` hunk reference mismatch, not a WI-1 regression.
  - Commit outcome:
    - PRVM v2 opcode set, executor, and cross-contract sub-call mechanism exist in the tree but are not yet selected by the default builder; nothing in the assembler pipeline behaves differently yet

- [x] **Work item 2**: move plain instruction statements (`LineAst::Statement` mnemonic + operands) from v1 envelope delegation to v2 bytecode
  - Source requirement or finding IDs: Q3 step 1; Q5 (preserve `Expr::Error`); Q7 (delete v1 helper in same commit)
  - Validation: see detailed validation commands and recorded evidence below.
  - Definition of done:
    - default builder in `crates/opforge-vm/src/builder.rs` emits a v2 program for plain instruction statements: optional leading label → mnemonic → operand-comma scan → per-range opcore expression sub-call loop → `FinishLine`
    - `ParserVmOpcode::ParseStatementEnvelope` and `ParseInstructionEnvelope` removed from v1 enum; matching `parse_statement_envelope_from_tokens` and `parse_instruction_envelope_from_tokens` helpers removed from `vm_opasm_parse.rs`
    - all instruction-statement parses now flow through the v2 executor
    - runtime expression error path continues to embed `Expr::Error` exactly where Rust does today (verify against `asmline_instruction.rs` lines 314–520)
    - existing parser/asm tests for instruction statements pass without fixture regeneration
  - Validation:
    - `cargo test -p opforge-vm vm_opasm -- --nocapture`
    - `cargo test -p opforge-asm asmline_instruction -- --nocapture`
    - `cargo fmt --all` / `cargo clippy --all-targets --all-features -- -D warnings` / `cargo audit` / `cargo test --workspace`
  - Expected files (minimum anchors; see Expected-files note at the top of the Work Items section):
    - `crates/opforge-vm/src/builder.rs`
    - `crates/opforge-vm/src/vm_opasm_parse.rs`
    - `crates/opforge-vm/src/execution_model/parser_vm.rs` (v1 dispatcher loses two opcodes)
    - `crates/opforge-vm/src/execution_model/parser_vm_v2.rs`
    - `crates/opforge-package/src/package.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-package/src/package/tests.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to instruction-statement migration + matched v1 deletion
    - 2026-04-27 execution evidence: `cargo fmt --all`, `cargo test -p package parser_vm_v2 -- --nocapture`, `cargo test -p vm execution_model -- --nocapture`, `cargo test -p vm vm_opasm -- --nocapture`, `cargo test -p asm asmline_instruction -- --nocapture`, `cargo test -p asm vm_native -- --nocapture`, `cargo test -p asm vm_runtime_ -- --nocapture`, `cargo test -p asm tests::motorola68020_tkpkg_smoke_package_fixture_matches_authoritative_registry -- --nocapture`, `cargo clippy --all-targets --all-features -- -D warnings`, and `cargo audit` passed. `cargo test --workspace` is accepted for this WI with `862 passed; 1 failed`, where the only failure is the single reproduced baseline `asm::tests::examples_match_reference_outputs` waived by the user on 2026-04-27. The focused tkpkg smoke package fixture was updated because the intentionally changed default parser package bytes now encode the PRVM v2 instruction-statement program.
  - Commit outcome:
    - instruction statements are parsed by PRVM v2; the two corresponding v1 envelope opcodes and helpers are gone

- [x] **Work item 3**: move data directives (`.byte`/`.db`, `.word`/`.dw`, `.long`, `.text`, `.null`, `.ptext`, `.fill`, `.res`, `.ds`, `.align`) to v2
  - Source requirement or finding IDs: Q3 step 2; Q7
  - Validation: see detailed validation commands and recorded evidence below.
  - Note: `.org` is **not** in this WI; it is tied to `parse_star_or_org_envelope_from_tokens` (see [crates/opforge-vm/src/vm_opasm_parse.rs](../../crates/opforge-vm/src/vm_opasm_parse.rs)) and migrates in WI-4 with `*=`.
  - Definition of done:
    - v2 builder emits programs for the listed data directives
    - all listed directives parse through v2; existing tests pass with only the intentional tkpkg smoke fixture regeneration required by the changed default parser program bytes
    - **`ParserVmOpcode::ParseDotDirectiveEnvelope` and `parse_dot_directive_envelope_from_tokens` remain live** because block-scoped directive heads/tails still need them; their deletion is deferred to WI-4 once those shapes have migrated
    - block-scoped directive heads/tails (`.region`, `.section`, `.encode`, `.meta`, `.output` and their `.end*` counterparts) are **not** included here — they belong to WI-4
    - opcore-owned directives must not appear in this WI; if any test exercises an opcore-owned directive that was incidentally being routed through the dot-directive envelope, that case is rerouted to the opcore code path (not v2)
  - Validation: as WI-2 plus targeted runtime directive coverage in `crates/opforge-asm/src/tests.rs`
  - Expected files (minimum anchors as amended after WI-3 mapping; see Expected-files note at the top of the Work Items section):
    - `crates/opforge-vm/src/builder.rs`
    - `crates/opforge-vm/src/execution_model/parser_vm_v2.rs`
    - `crates/opforge-package/src/package.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-package/src/package/tests.rs`
    - `crates/opforge-asm/src/tests.rs` (runtime lockstep test surface only)
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_debug_cli_package.opasm` (intentional package fixture update)
  - Mapping note:
    - `crates/opforge-vm/src/vm_opasm_parse.rs` was reviewed but intentionally not modified in WI-3 because `parse_dot_directive_envelope_from_tokens` must remain live until WI-4.
    - `crates/opforge-asm/src/asmline_directives_data.rs` was reviewed as the original directive test surface, but the focused WI-3 validation belongs in `crates/opforge-asm/src/tests.rs` because it exercises the runtime/lockstep PRVM v2 parser path directly.
  - Plan-compliance review evidence:
    - 2026-04-27 first `plan-compliance-reviewer` run returned `FAIL` because the initial WI-3 `Expected files` anchors still named `crates/opforge-vm/src/vm_opasm_parse.rs` and `crates/opforge-asm/src/asmline_directives_data.rs`; the behavioral scope was otherwise found compliant. This plan entry was amended before commit to record the actual validated anchor files and why the original two references were not modified.
    - 2026-04-27 execution evidence: `cargo fmt --all`, `cargo test -p package parser_vm -- --nocapture`, `cargo test -p vm parser_vm_v2 -- --nocapture`, `cargo test -p asm vm_runtime_parses_wi3_data_directives_through_v2_parser -- --nocapture`, `cargo test -p asm tests::motorola68020_tkpkg_smoke_package_fixture_matches_authoritative_registry -- --nocapture`, `cargo test -p vm vm_opasm -- --nocapture`, `cargo test -p asm vm_native -- --nocapture`, `cargo test -p asm vm_runtime_ -- --nocapture`, `cargo test -p asm asmline_instruction -- --nocapture`, `cargo clippy --all-targets --all-features -- -D warnings`, and `cargo audit` passed. `cargo test -p vm execution_model_ -- --nocapture` passed (`124 passed`). `cargo test --workspace` is accepted for this WI with `863 passed; 1 failed`, where the only failure is the single reproduced baseline `asm::tests::examples_match_reference_outputs` waived by the user on 2026-04-27.
    - The focused tkpkg smoke package fixture was updated from 3985 bytes to 3997 bytes because the intentionally changed default parser package bytes now encode the PRVM v2 data-directive branch.
    - 2026-04-27 rerun `plan-compliance-reviewer` returned `PASS`; residual risk is limited to relying on the recorded validation matrix for the rerun and the already-waived `asm::tests::examples_match_reference_outputs` baseline failure.
  - Commit outcome: data directives parsed by v2; `ParseDotDirectiveEnvelope` still alive for block-scoped shapes pending WI-4

- [x] **Work item 4**: move `*=`/`.org`, `name = expr`, and block-scoped directive heads/tails to v2; delete `ParseStarOrgEnvelope`, `ParseAssignmentEnvelope`, and `ParseDotDirectiveEnvelope`
  - Source requirement or finding IDs: Q3 steps 3 and 4; Q7
  - Validation: see detailed validation commands and recorded evidence below.
  - Definition of done:
    - v2 builder emits programs for `*=`, `.org` (which today flows through `parse_star_or_org_envelope_from_tokens`), `name = expr`, and the block-scoped directive heads/tails (`.region`, `.section`/`.endsection`, `.encode`/`.endencode`, `.meta`/`.endmeta`, `.output`/`.endoutput`)
    - `ParserVmOpcode::ParseStarOrgEnvelope`, `ParseAssignmentEnvelope`, and `ParseDotDirectiveEnvelope` removed; matching `parse_star_or_org_envelope_from_tokens`, `parse_assignment_envelope_from_tokens`, and `parse_dot_directive_envelope_from_tokens` helpers removed
    - `.place`, `.pack`, 65816 runtime-state directives, and inline `.meta.output.*` remain Rust-routed and explicitly out of scope; assertions in tests pin that boundary
    - all in-scope shapes parse through v2; existing tests pass with only the intentional tkpkg smoke fixture regeneration required by the changed default parser program bytes
  - Validation: as WI-2 plus targeted block-scope tests in `crates/opforge-asm/src/tests.rs`
  - Expected files (minimum anchors as amended after WI-4 implementation; see Expected-files note at the top of the Work Items section):
    - `crates/opforge-vm/src/builder.rs`
    - `crates/opforge-vm/src/vm_opasm_parse.rs`
    - `crates/opforge-vm/src/execution_model/parser_vm.rs` (v1 dispatcher loses three opcodes)
    - `crates/opforge-vm/src/execution_model/parser_vm_v2.rs`
    - `crates/opforge-package/src/package.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
    - `crates/opforge-package/src/package/tests.rs`
    - `crates/opforge-vm/src/execution_model/tests.rs`
    - `crates/opforge-asm/src/tests.rs` (runtime lockstep test surface only)
    - `examples/motorola68000/amigaos/tkpkg/tkpkg_debug_cli_package.opasm` (intentional package fixture update)
  - Mapping note:
    - `crates/opforge-asm/src/asmline_directives_layout.rs` and `crates/opforge-asm/src/asmline_directives_metadata.rs` were reviewed as the directive implementation surfaces, but the focused WI-4 validation belongs in `crates/opforge-asm/src/tests.rs` because it exercises the runtime/lockstep PRVM v2 parser path directly.
    - `.place`, `.pack`, conditionals, 65816 runtime-state directives, and inline `.meta.*`/`.output.*` forms remain Rust-routed through the explicit out-of-scope fallback pending later parser work.
  - Plan-compliance review evidence:
    - 2026-04-27 execution evidence: `cargo fmt --all`, `cargo test -p package parser_vm_v2_opcode_byte_round_trip_is_stable -- --nocapture`, `cargo test -p vm parser_vm_v2 -- --nocapture`, `cargo test -p vm execution_model_ -- --nocapture`, `cargo test -p asm vm_runtime_parses_wi4_control_and_block_shapes_through_v2_parser -- --nocapture`, `cargo test -p asm motorola68020_tkpkg_smoke_package_fixture_matches_authoritative_registry -- --nocapture`, `cargo clippy --all-targets --all-features -- -D warnings`, and `cargo audit` passed. `cargo test --workspace` is accepted for this WI with `864 passed; 1 failed`, where the only failure is the single reproduced baseline `asm::tests::examples_match_reference_outputs` waived by the user on 2026-04-27.
    - The focused tkpkg smoke package fixture was updated from 3997 bytes to 4025 bytes because the intentionally changed default parser package bytes now encode the PRVM v2 assignment and `*=` branches.
    - 2026-04-27 first `plan-compliance-reviewer` run returned `FAIL` because the out-of-scope fallback boundary was only pinned for `.struct`. The boundary test was expanded to cover `.place`, `.pack`, `.al`, `.assume`, `.meta.output.name`, and `.output.bin` with a deliberately failing v2 bytecode program, then `cargo test -p vm parser_vm_v2 -- --nocapture` and `cargo clippy --all-targets --all-features -- -D warnings` passed.
    - 2026-04-27 rerun `plan-compliance-reviewer` returned `PASS`; residual risk is limited to relying on the recorded validation matrix and the already-waived `asm::tests::examples_match_reference_outputs` baseline failure.
  - Commit outcome: assignments, `*=`/`.org`, and block-scoped directive heads/tails parsed by v2; only the residual v1 housekeeping opcode (`EmitDiagIfNoAst`) and the v1 `End` remain pending WI-5

- [x] **Work item 5**: remove residual PRVM v1 surface
  - Source requirement or finding IDs: Q7 final cleanup; Q8 boundary enforcement
  - Validation: see detailed validation commands and recorded evidence below.
  - Definition of done:
    - `ParserVmOpcode` v1 enum removed; `PARSER_VM_OPCODE_VERSION_V1` constant removed
    - `EmitDiagIfNoAst` and any remaining v1-only opcodes either re-homed as v2 opcodes (and renamed under the v2 namespace) or removed
    - all v1 fallback paths in `parser_vm.rs` removed; the file is either deleted or reduced to a thin re-export of the v2 executor
    - `vm_opasm_parse.rs` no longer contains any `parse_*_envelope_from_tokens` helper
    - audit pass: no code path remains that calls into Rust opcore parsing from opasm except via the typed cross-contract sub-call from WI-1
    - `ParserVmOpcode` in `package.rs` is now the v2 enum (rename if needed); only one parser-VM opcode-version constant remains
  - Validation: full quality gate plus a dedicated grep-style test asserting no `_envelope_from_tokens` symbol survives
  - Expected files (minimum anchors; see Expected-files note at the top of the Work Items section):
    - `crates/opforge-vm/src/execution_model/parser_vm.rs` (delete or reduce)
    - `crates/opforge-vm/src/execution_model/parser_vm_v2.rs`
    - `crates/opforge-vm/src/execution_model/mod.rs`
    - `crates/opforge-vm/src/vm_opasm_parse.rs` (slim down)
    - `crates/opforge-package/src/package.rs`
    - `crates/opforge-package/src/package/tests.rs`
    - `crates/opforge-vm/src/builder.rs`
    - `crates/opforge-vm/src/runtime_tests.rs`
  - Implementation notes:
    - removed the opasm PRVM v1 opcode-version constant and promoted the surviving v2 opcode enum to `ParserVmOpcode`; `ParserVmOpcodeV2` remains only as a compatibility type alias for existing v2 call sites
    - reduced `parser_vm.rs` to a v2-only dispatcher that enforces contract/program version equality and rejects non-v2 opasm parser contracts before entering `parser_vm_v2.rs`
    - changed runtime contract validation to accept only `PARSER_VM_OPCODE_VERSION_V2_OPASM_STATEMENT` for opasm statement parsing; expression parser VM v1 remains intentionally untouched as the separate EXPP contract
    - replaced residual v1 test programs with v2 diagnostic/failure programs and added `opasm_parser_source_has_no_retired_envelope_helpers` as the required grep-style source audit
    - refreshed the parser VM guide so it documents PRVM v2 instead of the retired v1 envelope opcodes
  - Validation evidence:
    - `cargo test -p package parser_vm_opcode_byte_round_trip_is_stable -- --nocapture` passed
    - `cargo test -p vm parser_vm_v2 -- --nocapture` passed (`13 passed`)
    - `cargo test -p vm execution_model_ -- --nocapture` passed (`124 passed`)
    - `cargo test -p vm opasm_parser_source_has_no_retired_envelope_helpers -- --nocapture` passed
    - source audit grep for `\bPARSER_VM_OPCODE_VERSION_V1\b|\bEmitDiagIfNoAst\b|parse_[A-Za-z0-9_]*_envelope_from_tokens` under `crates/**/*.rs` returned no matches; expression parser VM v1 matches were intentionally excluded from this opasm cleanup, and `ParserVmOpcode` now names the promoted v2 enum
    - `cargo fmt --all` passed
    - `cargo clippy --all-targets --all-features -- -D warnings` passed
    - `cargo audit` passed with the two already-known allowed warnings (`registry` unmaintained, `rand` advisory through `proptest`)
    - `cargo test --workspace` is accepted for this WI with `864 passed; 1 failed`, where the only failure is the previously waived baseline `asm::tests::examples_match_reference_outputs`
    - expected-file anchors that were not modified in WI-5 were audited as already clean from earlier work items: `vm_opasm_parse.rs` has no retired envelope helpers, `builder.rs` already emits v2 default parser bytecode, `package/tests.rs` now exercises the promoted enum name, and the execution-model module surface remains the existing `execution_model.rs` plus `execution_model/parser_vm.rs`/`parser_vm_v2.rs` split
  - Plan-compliance review evidence: first WI-5 `plan-compliance-reviewer` run returned `FAIL` because the surviving enum was still named `ParserVmOpcodeV2` and the expected-file anchor audit was not explicit; after promoting the enum to `ParserVmOpcode`, retaining `ParserVmOpcodeV2` only as a compatibility alias, and documenting the anchor audit, the rerun returned `PASS`
  - Commit outcome: PRVM v1 is gone, PRVM v2 is the sole opasm-statement parser-VM contract, and the engine/processor partition is enforceable by inspection

- [x] **Work item 6**: Rust v2 parity hardening and authority confirmation
  - Source requirement or finding IDs: Objective ("source-of-truth target"); Q5 expression-error preservation; native-port readiness gate
  - Validation: see detailed validation commands and recorded evidence below.
  - Definition of done:
    - parity test corpus added under `crates/opforge-vm/tests/` covering: instruction statements (operand counts 0/1/2/3+, all asm addressing-mode shapes currently exercised by the test suite), data directives (each shape from WI-3), assignment forms, block-scoped heads/tails, `Expr::Error` preservation cases, malformed-statement diagnostic cases, trailing-token cases, and checkpoint depth boundary; entry-boundary violations are covered by the renamed private executor test because the public opasm adapter always supplies the valid `asm` statement entry request
    - parity is verified by running the full existing assembler test suite (`cargo test --workspace`) with zero fixture regeneration
    - performance is documented but not gated: a one-off micro-benchmark records v2 statement-parse throughput against the prior v1+helpers path; result is recorded in a comment inside the new test module, not a release note
    - cross-contract sub-call counters / assertions confirm zero direct Rust-opcore calls from opasm parsing outside the typed sub-call path
  - Validation:
    - `cargo test -p opforge-vm parser_vm_v2_parity -- --nocapture`
    - full quality gate
  - Expected files:
    - `crates/opforge-vm/tests/parser_vm_v2_parity.rs` (new)
    - small additions to existing test modules where parity gaps are surfaced
  - Implementation notes:
    - added `crates/opforge-vm/tests/parser_vm_v2_parity.rs` as the public runtime parity corpus for migrated statement forms, data directives, assignment forms, block-scoped heads/tails, expression-error preservation, malformed/trailing-token diagnostics, typed expression sub-calls, checkpoint depth, and a non-gated throughput sample
    - renamed the private executor entry-boundary and checkpoint-depth tests so the `parser_vm_v2_parity` filter includes the executor boundary coverage as well as the public integration corpus; entry-boundary coverage intentionally remains private because the public opasm adapter cannot produce the wrong request kind
    - split CPU-special m68k addressing shapes (`BFTST D0{1:3}` and `CAS2.W ...`) into a v2-authority assertion because the generic host parser is no longer a valid oracle for those forms
    - recorded the throughput sample in the new test module comment; the latest local sample printed `768 v2 parses in 25.247625ms; 768 host parses in 1.385917ms`, and it remains informational only
  - Validation evidence:
    - `cargo test -p vm parser_vm_v2_parity -- --nocapture` passed (`2` private executor tests plus `7` integration tests)
    - `cargo fmt --all` passed
    - `cargo clippy --all-targets --all-features -- -D warnings` passed after factoring the integration-test counting-handler return tuple into a type alias
    - `cargo audit` passed with the two already-known allowed warnings (`registry` unmaintained, `rand` advisory through `proptest`)
    - `cargo test --workspace` is accepted for this WI with `864 passed; 1 failed`, where the only failure is the previously waived baseline `asm::tests::examples_match_reference_outputs`
    - no fixtures or reference outputs were regenerated for WI-6
  - Plan-compliance review evidence: first WI-6 `plan-compliance-reviewer` run returned `FAIL` because this evidence line still said `pending` and the plan implied entry-boundary coverage was in the public parity corpus; after documenting that entry-boundary coverage is intentionally satisfied by the renamed private executor test and restoring the public checkpoint-depth test marker, the rerun returned `PASS`
  - Commit outcome: PRVM v2 Rust implementation is hardened as the authoritative reference for opasm statement parsing; ready to be used as the truth source for the future native 68020 port

- [x] **Work item 7**: author the native 68020 PRVM follow-up plan (this plan does **not** implement it)
  - Source requirement or finding IDs: Q6 reserved entry symbol `prvm_run_68000`; user directive that native work begins only after Rust v2 parity
  - Validation: see detailed validation commands and recorded evidence below.
  - Definition of done:
    - new file `documentation/plans/opforge-parser-vm-v2-native-amigaos-68020-implementation-plan-v0_1.md` authored, modeled on the existing tokenizer-native plan
    - the new plan explicitly cites this plan's WI-6 as its trigger and uses the Rust v2 implementation as the source of truth
    - the new plan reserves `prvm_run_68000` as the single-call ABI entry symbol, mirrors the tokenizer single-line-buffer ABI shape, and defines the host-mediated cross-call to the (still Rust-side) opcore expression parser
    - the new plan must explicitly say that the active worktree `AGENTS.md` workflow and execution rules remain binding at all times
    - the new plan passes `plan-quality-reviewer` (or `plan-quality-orchestrator` if treated as high-value) before becoming active
    - **no native code is landed in this WI**; this WI's commit contains only the new plan document and any spec updates required for the new plan to pass its quality gate
  - Validation:
    - `plan-quality-reviewer` (or orchestrator) `PASS` on the new plan
    - full quality gate accepted for this WI when it is green or fails only the previously user-waived `asm::tests::examples_match_reference_outputs` baseline
  - Expected files:
    - `documentation/plans/opforge-parser-vm-v2-native-amigaos-68020-implementation-plan-v0_1.md` (new)
    - optionally, a v0.2 reissue of `documentation/opForge-extended-parser-vm-instruction-set-spec-v0_1.md` reflecting Q1–Q9 resolutions if the plan-quality reviewer requires it
  - Implementation notes:
    - authored the new native PRVM plan as a plan-only follow-up with no native code, no spec reissue, and no fixture/reference regeneration
    - the plan cites completed WI-6 commit `e3699c7c` and treats Rust PRVM v2 plus `parser_vm_v2_parity` as the native implementation oracle
    - the plan reserves `prvm_run_68000`, targets `.cpu 68020` for the first native slices, mirrors the tokenizer single-line caller-owned-buffer style, and defers the CLI/file I/O harness until after ABI and parity work
    - host-mediated opcore expression parsing is planned as a pause/resume protocol over explicit operand token ranges; native expression parsing remains blocked
  - Validation evidence:
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-parser-vm-v2-native-amigaos-68020-implementation-plan-v0_1.md` passed
    - `plan-quality-orchestrator` was attempted first, but could not launch nested configured reviewers in this session and performed no substantive review
    - `plan-quality-reviewer` returned `PASS`; residual risk is limited to exact pause/resume record layout and decode normalization, which are intentionally contained in the new plan's first ABI/spec work item
    - combined quality gate command ran through `cargo test --workspace`; the only failure was the previously user-waived `asm::tests::examples_match_reference_outputs` baseline (`864 passed; 1 failed`)
  - Plan-compliance review evidence: pending WI-7 run before commit
  - Commit outcome: a Rust-truth-source-driven native 68020 implementation plan exists, reviewed and ready to become active as a separate effort; this plan is then complete

## Acceptance Criteria for the Plan as a Whole

- [ ] PRVM v2 is the sole opasm statement parser-VM contract in the Rust tree.
- [ ] PRVM v1 enum, helpers, and fallback paths are gone.
- [ ] The opasm v2 executor never calls into Rust opcore parsing except through the typed `ParseOperandExprRange` cross-contract sub-call.
- [ ] The runtime entry-boundary precondition is enforced; misrouted lines fail with a typed diagnostic.
- [ ] All existing assembler tests pass without fixture regeneration.
- [ ] `Expr::Error` preservation in delegated statement flows matches pre-v2 behavior.
- [ ] A separate native 68020 plan exists, references this plan as its trigger, and has passed plan-quality review.

## Validation Expectations (cumulative)

- v2 opcode encode/decode round-trip tests (WI-1).
- v2 executor unit tests for cursor, checkpoint/rollback/commit, value stack, AST builder, and cross-contract sub-call dispatch (WI-1).
- Per-shape parity tests as each line shape migrates (WI-2 through WI-4).
- Residual-v1-surface absence test (WI-5).
- Full parity corpus + entry-boundary + cross-contract violation tests (WI-6).
- Plan-quality review of the native follow-up plan (WI-7).

## Out of Scope (explicit non-goals)

- Native 68020 implementation of `prvm_run_68000` (reserved for the WI-7 follow-up plan).
- Any opcore-directive VM contract work (Q9 deferral).
- Any opcore-expression VM redesign.
- Migrating `.place`, `.pack`, 65816 runtime-state directives, or inline `.meta.output.*` shorthand into v2.
- `Expr::Error` normalization into terminal parser diagnostics (Q5; reserved for v3).
- Tokenizer VM, encoding VM, or assembler-pass-engine changes.
- Performance gating of v2 vs v1+helpers (measurement only, no gate).
