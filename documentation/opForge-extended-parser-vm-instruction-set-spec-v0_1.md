# opForge Extended opasm Statement Parser VM Specification

**Version:** 0.1-draft
**Date:** March 22, 2026
**Status:** Proposal

## Summary

This specification defines an extended VM instruction set for opasm statement parsing only.

The target concern is the assembler-side statement parser that runs after opcore has already classified a line and delegated the remaining statement work to the `asm` processor domain. The specification does not redefine opcore routing, opcore-owned directive parsing, or expression parsing. Instead, it defines how opasm statement parsing can move from Rust helper control flow into a versioned VM contract while preserving the existing architectural partition:

- opcore owns line-level routing for core concerns and the delegation decision,
- opasm owns delegated assembler statement parsing,
- expression parsing remains an explicit opcore sub-contract used by opasm when operand expressions are needed.

For this specification, the current architecture and Rust implementation are the source of truth for the target boundary. The spec therefore describes the opasm-owned statement surface that exists today in Rust, and then constrains the first `PRVM v2` slice to the narrowest part of that surface that should move into VM first.

## Problem

The current VM parser contract is broader and more mixed than the architecture intends.

- In the native architecture, opcore first processes a line and decides whether it is an opcore-owned concern or whether it should delegate to `ProcessingRequestKind::Processor { processor: "asm", kind: "statement" }` in [crates/opforge-core/src/parser_opcore_requests.rs](/Users/erik/Code/Retro/opForge/crates/opforge-core/src/parser_opcore_requests.rs).
- That delegation boundary is the correct architectural seam for opasm parsing.
- The current VM-backed parser path in [crates/opforge-vm/src/vm_opasm_parse.rs](/Users/erik/Code/Retro/opForge/crates/opforge-vm/src/vm_opasm_parse.rs) still mixes concerns from both sides of that seam by parsing some opcore-owned line forms directly instead of limiting itself to delegated opasm statement work.
- As a result, the earlier spec over-described a mixed parser VM that touched line forms outside opasm ownership.
- That makes the contract harder to reason about, weakens architectural partitioning, and obscures the intended delegation mechanism between opcore and opasm.

The practical consequence is design drift: the specification talks about a general parser VM, but the intended implementation boundary is narrower. The spec must describe only the opasm-owned statement parser surface and its explicit dependency on opcore expression parsing.

## Goals

- [ ] Define a versioned VM contract for opasm statement parsing only.
- [ ] Preserve the opcore-to-opasm delegation seam as the required entry boundary for this VM.
- [ ] Limit required VM-owned parsing behavior to opasm statement concerns such as mnemonic recognition, operand boundary parsing, and statement-shape construction for delegated assembler statements.
- [ ] Keep expression parsing and expression error construction behind an explicit opcore sub-contract used by opasm.
- [ ] Allow `PRVM v2` to break from the current `v1` parser-program format where needed to match the narrower and more correct opasm-first architectural scope.
- [ ] Make VM/native parity testable for delegated opasm statement behavior.
- [ ] Keep Rust responsible for tokenization, package/model resolution, budget enforcement, and orchestration around the delegated parser execution.

## Non-Goals

- [ ] Redefine opcore line classification, module/import parsing, or core directive routing.
- [ ] Move `.use`, `.statement`, `.endstatement`, `.module`, `.endmodule`, or other opcore-owned concerns into opasm scope.
- [ ] Move opcore-owned conditional/directive parsing into this specification unless opcore explicitly delegates those forms to opasm in the future.
- [ ] Replace the tokenizer VM, expression parser VM, or full assembler pass engine with one whole-program parser VM.
- [ ] Redesign the `LineAst` or `Expr` data model.
- [ ] Change existing assembler semantics merely to fit a VM design.

## Invariants / Constraints

1. The contract starts after opcore delegation.
   The extended opasm parser VM runs only for lines that have already crossed the `processor = "asm", kind = "statement"` boundary.

2. opcore remains the routing authority.
   opcore continues to own line classification and line forms already handled by `process_opcore_statement_request(...)` in [crates/opforge-core/src/parser_opcore_requests.rs](/Users/erik/Code/Retro/opForge/crates/opforge-core/src/parser_opcore_requests.rs).

3. Expression parsing remains opcore-owned.
   The opasm parser VM may request expression parsing only through the typed opcore expression contract used today by `parse_expr_with_vm_contract(...)` in [crates/opforge-vm/src/vm_opcore.rs](/Users/erik/Code/Retro/opForge/crates/opforge-vm/src/vm_opcore.rs) and [crates/opforge-vm/src/vm_opasm_parse.rs](/Users/erik/Code/Retro/opForge/crates/opforge-vm/src/vm_opasm_parse.rs).

4. `PRVM v2` is allowed to define a new incompatible parser-program format.
   Backward compatibility with the current `v1` parser-program format is not a goal of this specification. If a new opcode set, encoding layout, or contract shape is the clearest way to realize the narrower opasm-first architecture, this specification allows that break.

5. VM/native parity is required only for delegated opasm statement behavior.
   The parity obligation applies to statement parsing that belongs to opasm scope, not to opcore-owned routing or opcore-owned directives.

6. Determinism is mandatory.
   The VM executes within explicit bytecode, token, step, and stack budgets. No hidden reparsing of source text is allowed outside the declared opcore expression sub-calls.

7. Unsupported cross-boundary behavior must fail explicitly.
   If a `v2` parser program attempts to consume line forms outside delegated opasm scope, execution must fail with a typed parser diagnostic or be rejected by contract validation.

8. Spans remain contract-visible.
   AST nodes and diagnostics produced by the opasm parser VM must preserve token-derived spans with the same fidelity expected by current assembler flows.

9. Current Rust architecture is the source of truth for scope.
   The opasm-owned statement inventory for this specification is determined by the current opcore delegation seam plus the directive/runtime handlers that exist today in Rust. Desired future cleanup does not override current ownership evidence.

## Behavioral Contract

### 1. Entry Boundary

`PRVM v2` in this specification is an opasm statement parser.

It does not define the whole-line parser for opForge. Its logical entrypoint is the delegated assembler statement stage used by opasm, represented today by [crates/opforge-asm/src/opasm.rs](/Users/erik/Code/Retro/opForge/crates/opforge-asm/src/opasm.rs) and the VM-facing helpers in [crates/opforge-vm/src/vm_opasm.rs](/Users/erik/Code/Retro/opForge/crates/opforge-vm/src/vm_opasm.rs).

The required sequence is:

1. tokenize the line,
2. let opcore classify the line and decide whether it is an opasm statement request,
3. only then execute the opasm parser VM for the delegated statement.

This specification does not authorize `PRVM v2` to replace step 2.

### 2. Covered Result Surface

For the first `PRVM v2` slice, required VM result construction is limited to the delegated opasm statement result shape:

- `LineAst::Statement`

Within that result, the VM must be able to populate:

- optional leading label as seen by the delegated assembler statement parser,
- statement mnemonic,
- ordered operand-expression list,
- token-derived spans needed for diagnostics and downstream assembler handling.

This specification does not require the first `PRVM v2` slice to construct:

- `Assignment`
- `Conditional`
- `Use`
- `StatementDef`
- `StatementEnd`

The current Rust implementation still treats `Place` and `Pack` as opasm-owned special cases rather than opcore concerns, but they remain outside the minimum required first slice because they are not yet normalized into the generic delegated `LineAst::Statement` path.

Those forms are outside the required first-slice VM contract unless opcore explicitly delegates them and the VM scope is extended accordingly.

### 3. Covered opasm Responsibilities

For delegated statement lines, the VM owns these decisions:

- recognize the mnemonic token for the delegated statement,
- determine whether operands are present,
- split the operand list at top-level comma boundaries,
- preserve operand ordering,
- identify operand sub-ranges that must be sent to the opcore expression parser,
- produce a final `LineAst::Statement` result with mnemonic and operands,
- emit typed parser diagnostics for malformed delegated statement syntax.

This is the intended opasm responsibility boundary: statement shape and operand boundary parsing, not whole-line concern ownership.

### 4. Expression Delegation

When an operand requires expression parsing, `PRVM v2` must call the opcore expression parser through an explicit typed sub-contract.

Required properties:

- opasm decides the token range for each operand expression,
- opcore parses the expression for that range,
- the returned expression or expression error is inserted into the operand list,
- opasm does not use opaque host callbacks such as "parse the rest of the line".

This preserves the intended architectural split: opasm owns instruction-and-operand statement structure, opcore owns expression parsing.

### 5. Minimum New Opcode Surface (`PRVM v2`)

`PRVM v2` for opasm statement parsing requires at least the following opcode families.

| Opcode Family | Required operations | Meaning |
|---|---|---|
| Control flow | `Jump`, `JumpIfTrue`, `JumpIfFalse`, `Checkpoint`, `Rollback`, `Commit` | Support deterministic statement-shape branching and bounded rollback. |
| Token inspection | `PeekKind`, `PeekIdentifier`, `PeekOperator`, `IsEol` | Inspect the current delegated statement tokens without consuming them. |
| Token movement | `Advance`, `ConsumeKind`, `ConsumeOperator` | Move through the delegated statement token stream. |
| Value loading | `LoadIdentifier`, `LoadSpan`, `LoadTokenText` | Capture mnemonic, label, and span data needed by statement builders. |
| Statement helpers | `ParseOptionalLeadingLabel`, `ScanTopLevelCommaBoundaries`, `RequireNoTrailingTokens` | Normalize delegated statement entry and operand segmentation rules. |
| Expression sub-calls | `ParseOperandExprRange`, `ParseOperandExprList` | Invoke the opcore expression contract for one operand range or a full operand list. |
| AST builders | `BeginStatement`, `SetLabel`, `SetMnemonic`, `PushOperand`, `FinishLine` | Build `LineAst::Statement` directly inside VM execution. |
| Diagnostics | `EmitDiag`, `EmitDiagIfNoResult`, `Fail` | Emit typed parser diagnostics or terminate unsuccessfully. |

The intent is to make opasm statement parsing explicit without broadening the contract into opcore-owned line parsing.

### 6. Current opasm-Owned Statement Inventory and First-Slice Coverage

The current architecture and Rust implementation show that the opasm-owned dot-statement surface is broader than the minimum first `PRVM v2` slice.

Current opasm-owned common directive families in Rust are:

- layout and placement: `.region`, `.section`, `.endsection`, `.place`, `.pack`, and the removed `.dsection` compatibility error path,
- data and code-generation: `.fill`, `.org`, `.align`, `.const`, `.var`, `.set`, `.cpu`, `.encode`, `.endencode`, `.enc`, `.encoding`, `.cdef`, `.tdef`, `.edef`, `.emit`, `.res`, `.byte`, `.db`, `.word`, `.dw`, `.long`, `.text`, `.null`, `.ptext`, `.ds`,
- metadata and output: `.meta`, `.endmeta`, `.output`, `.endoutput`, `.name`, `.version`, `.list`, `.hex`, `.bin`, `.mapfile`, `.exportsections`,
- inline metadata and output variants currently accepted by opasm: `.output.name`, `.output.list`, `.output.hex`, `.output.bin`, `.output.fill`, `.meta.name`, `.meta.version`, `.meta.output.name`, `.meta.output.list`, `.meta.output.hex`, `.meta.output.bin`, `.meta.output.fill`,
- CPU-specific output block heads and tails of the form `.<cpu>` and `.end<cpu>` inside `.output` metadata handling when `<cpu>` resolves through the registry.

Current family-specific runtime directives currently implemented in Rust are also opasm-owned when the active CPU family exposes them. The concrete implemented set verified in the current codebase is the 65816 runtime-state family:

- `.assume`,
- `.al`, `.as`, `.xl`, `.xs`,
- `.databank`, `.dbank`, `.dpage`.

This inventory is grounded in the current Rust routers and runtime hooks in:

- [crates/opforge-asm/src/asmline_directives_layout.rs](/Users/erik/Code/Retro/opForge/crates/opforge-asm/src/asmline_directives_layout.rs),
- [crates/opforge-asm/src/asmline_directives_data.rs](/Users/erik/Code/Retro/opForge/crates/opforge-asm/src/asmline_directives_data.rs),
- [crates/opforge-asm/src/asmline_directives_metadata.rs](/Users/erik/Code/Retro/opForge/crates/opforge-asm/src/asmline_directives_metadata.rs),
- [crates/opforge-asm/src/line.rs](/Users/erik/Code/Retro/opForge/crates/opforge-asm/src/line.rs),
- [crates/opforge-families/src/m65816/state.rs](/Users/erik/Code/Retro/opForge/crates/opforge-families/src/m65816/state.rs).

The first `PRVM v2` slice does not need to cover that whole inventory at once.

The minimum first slice must cover only:

- plain instruction statements with optional labels,
- delegated assembler directives that already normalize to `LineAst::Statement`,
- operand lists that require top-level comma splitting and opcore expression sub-parses.

The first slice does not need to absorb current opasm-owned transitional special cases such as `LineAst::Place` and `LineAst::Pack` immediately, but it must not misclassify them as opcore-owned.

It also does not require support for line forms that opcore handles before delegation.

### 7. Diagnostics Contract

`PRVM v2` must support stable typed diagnostics for at least:

- unsupported parser VM opcode version,
- parser contract/program version mismatch,
- invalid parser opcode,
- missing opcode operand,
- missing mnemonic in a delegated statement where one is required,
- malformed operand separation,
- unexpected trailing tokens in delegated statement scope,
- parser VM termination without producing a delegated statement result,
- attempt to use out-of-scope line-form handling through this contract.

Diagnostics must remain attributable to token-derived spans.

### 8. Host Boundary

The host/runtime boundary for this specification is limited to:

- token slice and end-of-line metadata,
- parser contract metadata and diagnostic catalog,
- explicit opcore expression-parser sub-calls over bounded operand token ranges,
- final `LineAst::Statement` delivery and diagnostic delivery.

The host boundary must not expose opaque callbacks such as:

- "classify this line for me",
- "parse any opcore directive for me",
- "parse this whole line with the native parser".

The purpose of `PRVM v2` here is to formalize delegated opasm statement parsing, not to erase the opcore/opasm partition.

## Boundary Cases

1. Non-delegated line reaches the opasm parser VM
   Required behavior: reject or fail explicitly because the entry boundary was violated.

2. Unsupported runtime version
   Required behavior: reject before execution with a typed parser diagnostic.

3. Checkpointed branch fails after partial token consumption
   Required behavior: rollback restores cursor and builder state exactly to the checkpointed state.

4. Operand expression parse fails
   Required behavior: preserve current delegated statement behavior by embedding the opcore expression error only where that behavior is already expected for opasm statement handling.

5. Trailing tokens remain after operand parsing completes
   Required behavior: emit a typed delegated-statement parser diagnostic.

6. Program terminates without `End`
   Required behavior: fail with a typed parser diagnostic.

7. Program reaches `End` with no statement result
   Required behavior: fail with a typed parser diagnostic.

## Acceptance Criteria

- [ ] The specification defines `PRVM v2` as an opasm statement parser only, not as a whole-line parser.
- [ ] The specification makes the opcore-to-opasm delegation boundary explicit and binding.
- [ ] The specification treats the current Rust architecture and implementation as the source of truth for the present opasm-owned statement inventory.
- [ ] The specification limits required VM-owned result construction to delegated `LineAst::Statement` output.
- [ ] The specification defines opasm-owned responsibilities around mnemonic parsing, operand boundary parsing, and delegated statement construction.
- [ ] The specification distinguishes the current opasm-owned directive inventory from the narrower first `PRVM v2` implementation slice.
- [ ] The specification explicitly states that backward compatibility with the current `PRVM v1` format is not a goal.
- [ ] The specification explicitly preserves opcore ownership of line routing and expression parsing.
- [ ] The specification defines a minimum `v2` opcode surface for delegated statement parsing, operand segmentation, expression sub-calls, and diagnostics.
- [ ] Boundary behavior is explicit for entry-boundary violations, operand parse failures, trailing-token errors, unsupported versions, and end-without-result failures.
- [ ] Validation expectations are concrete enough to derive implementation and parity tests without embedding an implementation plan.

## Validation Expectations

An implementation derived from this specification is expected to prove behavior through:

- opcode encode/decode tests for `PRVM v2`,
- parser VM runtime tests for delegated statement cursor control, checkpoints, rollback, and statement builders,
- parity tests comparing native delegated opasm statement parsing with VM-authoritative delegated statement parsing,
- operand-boundary tests for top-level comma splitting and expression sub-call range selection,
- diagnostics tests asserting stable parser codes and correct spans for delegated statement failures,
- budget tests for oversized programs, malformed opcode payloads, and excessive rollback depth,
- integration tests showing that opcore still performs routing while opasm VM parsing handles only delegated statement scope.

## Open Questions

1. Should `PRVM v2` assign fixed opcode bytes in the initial specification, or should symbolic semantics be frozen first and byte assignment deferred until package-format review?
2. Should rollback restore only cursor and statement-builder state, or also restore temporary value-stack data captured since the checkpoint?
3. Which opasm-owned directives from the current Rust inventory should be included in the first `v2` slice after instruction statements, and in what order?
4. Should the operand-list contract expose one generic `ParseOperandExprList` opcode, or separate primitives for comma scanning and per-range expression calls?
5. For delegated statement flows that currently embed `Expr::Error`, should that remain part of the long-term contract or be normalized into terminal parser diagnostics in a later revision?