# opForge Extended Expression VM Instruction Set Specification

**Version:** 0.1-draft
**Date:** March 23, 2026
**Status:** Proposal

## Summary

This specification defines a clean extended expression VM contract for opForge's `opcore` domain.

The target concern is expression parsing and expression evaluation after a caller has already identified a bounded expression token range. For assembler statements, that means `opasm` still owns mnemonic parsing and operand boundary detection, then delegates each operand expression range to `opcore`. This specification does not redefine tokenization, whole-line routing, or delegated statement parsing. Instead, it defines how `opcore` expression behavior can move from Rust-side AST control flow into a versioned VM contract while preserving the current architectural partition visible in the Rust implementation:

- `opcore` owns expression grammar, expression diagnostics, expression shape construction, and expression evaluation semantics,
- `opasm` owns delegated assembler statement structure and operand boundary selection,
- Rust runtime code remains the coordinator for model resolution, budget enforcement, and contextual host callbacks.

For this specification, the current architecture and Rust implementation are the source of truth for the boundary. This spec therefore prioritizes a clean `v2` instruction set over backward compatibility with the current `v1` bytecode shape.

## Problem

The current expression pipeline is split in a way that limits VM coverage and blurs ownership boundaries.

- The VM-facing expression entrypoint in [crates/opforge-vm/src/vm_opcore.rs](../crates/opforge-vm/src/vm_opcore.rs) is already the opcore-owned seam used by assembler flows, including delegated operand parsing from [crates/opforge-vm/src/vm_opasm_parse.rs](../crates/opforge-vm/src/vm_opasm_parse.rs).
- In the current Rust implementation, `opasm` does not own expression grammar. It selects operand token ranges, then calls the typed opcore expression contract through `parse_expr_with_vm_contract(...)`.
- The current portable evaluator in [crates/opforge-core/src/expr_vm.rs](../crates/opforge-core/src/expr_vm.rs) only supports a narrow scalar instruction set: literals, current address, symbol lookup, unary and binary operators, ternary selection, and string literals.
- The core expression AST in [crates/opforge-core/src/parser.rs](../crates/opforge-core/src/parser.rs) already includes richer shapes such as `Immediate`, `Tuple`, `List`, `Index`, `Member`, `StructLiteral`, `Call`, and `Range`, but the portable compiler currently rejects most of them with unsupported-feature diagnostics.
- The earlier expression VM spec described `v2` mainly as an extension of `v1`, which over-constrained the design before the opcore boundary and responsibility split were stated explicitly.

The practical consequence is twofold:

- extending expression coverage still requires Rust parser/compiler work instead of extending a clean VM-owned opcore contract,
- the spec does not yet say clearly which expression concerns belong inside opcore and which concerns must remain delegated to contextual host services.

## Goals

- [ ] Define a versioned opcore-owned expression VM contract for parsing and evaluating bounded expression token ranges.
- [ ] Make the opasm-to-opcore delegation seam explicit and binding: opasm selects operand token ranges, opcore owns expression semantics.
- [ ] Define a clean `v2` instruction set and value model even if that requires breaking compatibility with the current `EXPR v1` bytecode layout or opcode numbering.
- [ ] Make the VM path capable of producing portable expression programs directly from tokens for covered forms, without requiring a Rust AST round-trip on that path.
- [ ] Introduce a typed VM value model that can carry both scalar values and structural operand-expression shapes needed later by operand interpretation and instruction selection.
- [ ] Make explicit which expression concerns opcore must handle itself and which concerns are delegated through typed host callbacks.
- [ ] Keep diagnostics deterministic and VM/native parity enforceable for covered opcore expression behavior.
- [ ] Keep Rust responsible for orchestration, budget enforcement, model/package resolution, and fallback behavior when the extended VM path is unavailable.

## Non-Goals

- [ ] Preserve backward compatibility with the current `EXPR v1` bytecode encoding, opcode numbering, or stack model when that would make `v2` less coherent.
- [ ] Move tokenization, whole-line routing, or operand boundary parsing into this specification.
- [ ] Make `opasm` authoritative for expression grammar or expression diagnostics.
- [ ] Replace the tokenizer VM, parser VM, or overall assembler pass engine with one whole-program VM.
- [ ] Remove the existing Rust expression parser or evaluator in the same slice that introduces this contract.
- [ ] Redesign macro expansion, preprocessing, module graph construction, or directive parsing as part of this specification.
- [ ] Define full user-extensible function execution for arbitrary compile-time code.
- [ ] Guarantee that every existing `Expr` variant becomes scalar-evaluable in pass 1 or pass 2.
- [ ] Change established language-level scalar operator semantics merely to fit a new bytecode encoding.

## Invariants / Constraints

1. `opcore` remains the expression authority.
   Expression grammar, expression parsing, expression value construction, and expression diagnostics remain opcore-owned concerns.

2. The contract starts at a bounded expression token range.
   `EXPR v2` does not decide where an expression begins or ends within a whole source line. For assembler flows, `opasm` continues to own operand boundary detection before calling the opcore expression contract.

3. Current Rust architecture is the source of truth for scope.
   The ownership boundary described by [crates/opforge-vm/src/vm_opcore.rs](../crates/opforge-vm/src/vm_opcore.rs) and [crates/opforge-vm/src/vm_opasm_parse.rs](../crates/opforge-vm/src/vm_opasm_parse.rs) is binding for this specification.

4. A clean `v2` may be incompatible with `v1`.
   Backward compatibility with the current expression VM bytecode is not mandatory. A runtime may continue supporting `v1` as a legacy contract, but this specification allows `v2` to use a new opcode set, stack model, payload encoding, and validation rules if that yields a cleaner opcore-owned design.

5. VM/native parity is mandatory for covered opcore behavior.
   For any expression form declared covered by this specification, the VM path and native Rust fallback must agree on parse success vs failure, primary diagnostic code, diagnostic severity, and resulting semantic value or structural shape.

6. Determinism is mandatory.
   Expression parsing and evaluation remain bounded by explicit token, bytecode, stack, and step budgets. No instruction may require unbounded recursion, unbounded allocation, or hidden reparsing of source text outside the declared contract.

7. Delegated callbacks must be typed and minimal.
   The host boundary may expose contextual queries such as symbol lookup, current-address lookup, stability/finalization checks, contextual string evaluation, builtin dispatch, and host-owned member/index resolution. It must not expose opaque callbacks such as "parse this expression for me" or "evaluate this AST for me."

8. Structural values are first-class in `v2`.
   The extended VM is not limited to a single `i64` stack element type. It must support tagged values so operand-shape information can remain in VM-owned form when a later consumer needs it.

9. Unsupported behavior must fail explicitly.
   When a required `v2` operation cannot be performed because a capability is missing or a boundary is violated, the VM path must emit a typed diagnostic rather than silently delegating to ad hoc Rust logic.

10. Spans remain contract-visible.
   Expression results and expression diagnostics produced through the VM path must preserve token-derived spans with the same fidelity expected by current assembler flows.

## Behavioral Contract

### 1. Entry Boundary

`EXPR v2` in this specification is an opcore expression contract.

Its logical entrypoint is a bounded token slice that a caller has already identified as one expression candidate. In assembler flows, the required sequence is:

1. tokenize the line,
2. let opcore classify the line and let opasm parse delegated statement structure,
3. let opasm identify each operand expression range,
4. call the opcore expression contract for each operand range.

This specification does not authorize `EXPR v2` to replace operand splitting, statement parsing, or whole-line routing.

### 2. Concern Partition

This specification makes the opcore boundary explicit.

#### opcore-owned concerns

For covered expression forms, opcore owns these responsibilities:

- consume a bounded token slice according to expression grammar,
- enforce precedence, associativity, and grouping rules,
- distinguish identifiers, register references, wrappers, and structural expression shapes,
- construct expression values or expression-shape results,
- compile the covered expression directly into portable expression bytecode when the VM path is authoritative,
- execute pure expression semantics on VM-native values,
- enforce scalar-versus-shape-preserving conversion rules,
- choose and emit typed expression diagnostics for syntax errors, stack/type errors, malformed payloads, and invalid scalar conversions,
- preserve and report token-derived spans.

These are the concerns that belong inside opcore even when the implementation is VM-backed.

#### Delegated concerns

The following concerns remain delegated through explicit typed host callbacks or runtime orchestration:

- symbol lookup by name or interned ID,
- current-address lookup,
- symbol stability/finalization lookup,
- contextual string literal evaluation when the active assembler/runtime context defines the mapping,
- builtin dispatch when the builtin is family-specific, package-specific, or otherwise not frozen into opcore semantics,
- member or index resolution when the base value is host-owned or its semantics depend on runtime-owned domain objects,
- package/model resolution, version negotiation, and budget enforcement.

If an operation can be completed using only VM-native values and frozen opcore semantics, opcore should handle it itself rather than delegating it back to the host.

### 3. Value Model

Expression VM `v2` introduces a tagged stack value model.

The runtime-visible value kinds are:

- `Int(i64)`
- `String(Vec<u8>)`
- `SymbolRef(symbol_id)`
- `RegisterRef(name_id)`
- `Immediate(inner)`
- `Indirect(inner)`
- `IndirectLong(inner)`
- `Tuple(items...)`
- `List(items...)`
- `Range { start, end, step, inclusive }`
- `StructLiteral { type_name_id, fields... }`
- `Placeholder`

The VM may internally materialize optimized forms, but these are the semantic kinds exposed by the contract.

`v2` evaluation supports two result modes:

- `Scalar`: the final value must be reducible to `Int` or to another explicitly allowed scalar result for that caller contract.
- `ShapePreserving`: the final value may remain structural so later assembler stages can inspect it without reconstructing Rust AST nodes.

Any caller that requests `Scalar` and receives a non-scalar result must receive a typed diagnostic.

### 4. Versioning and Compatibility

- `v1` may remain available as a legacy scalar evaluator contract.
- `v2` is not required to be a bytecode-compatible extension of `v1`.
- A runtime that does not support `v2` must reject a `v2` program before execution.
- A package that declares `v2` expression parsing or evaluation support must also declare the required host capability flags for any delegated operations it emits.
- If a runtime offers both `v1` and `v2`, version selection must be explicit. `v2` must not silently reinterpret `v1` payloads.

The design priority is a coherent `v2` contract, not preservation of legacy opcode layout.

### 5. Minimum Opcode Surface (`EXPR v2`)

This specification freezes semantic operations, not opcode bytes. `v2` may reuse, renumber, split, or merge current `v1` opcodes as needed for a cleaner contract.

The minimum required semantic surface is:

| Opcode Family | Required operations | Meaning |
|---|---|---|
| Program control | `End`, `Fail` | End execution successfully or terminate with failure. |
| Diagnostics | `EmitDiag` | Emit a typed expression diagnostic slot from inside the VM. |
| Scalar loading | `PushLiteral`, `PushCurrentAddress`, `PushSymbol`, `PushStringLiteral` | Load scalar and environment-derived values. |
| Scalar ops | `ApplyUnary`, `ApplyBinary`, `SelectTernary` | Preserve established scalar operator semantics. |
| Shape loading | `PushRegisterRef`, `PushPlaceholder` | Preserve non-scalar identity needed by later consumers. |
| Shape wrappers | `WrapImmediate`, `WrapIndirect`, `WrapIndirectLong` | Preserve operand-addressing intent rather than erasing it during compilation. |
| Shape builders | `BuildTuple`, `BuildList`, `BuildRange`, `BuildStructLiteral` | Build structural values without forcing scalar reduction. |
| Access/call ops | `GetMember`, `IndexValue`, `CallBuiltin` | Evaluate member access, indexing, and builtin calls under the opcore/delegated boundary rules. |
| Conversion/validation | `RequireScalar` | Enforce the explicit conversion boundary between shape-preserving results and scalar-only callers. |

Semantic rules:

- `PushRegisterRef` does not perform symbol lookup. It preserves register identity as distinct from ordinary identifiers.
- `WrapImmediate`, `WrapIndirect`, and `WrapIndirectLong` preserve addressing intent.
- `BuildTuple` and `BuildList` are shape-preserving operations. They do not imply scalar evaluation.
- `BuildRange` preserves optional step and inclusive or exclusive form. It does not implicitly expand the range.
- `GetMember` and `IndexValue` should operate directly on VM-native structural values when possible. Delegation is reserved for host-owned semantics.
- `CallBuiltin` should call into the host only for builtins not frozen into opcore semantics.
- `RequireScalar` is the explicit conversion boundary between operand-shape evaluation and scalar-only consumers.
- `EmitDiag` must use the VM diagnostic catalog rather than free-form message conventions.

### 6. Parser-to-Program Contract

For covered forms, the opcore expression VM path produces `EXPR v2` bytecode directly from the bounded token slice.

Covered forms in this specification are:

- numeric literals,
- string literals,
- identifiers,
- register references,
- current address (`$`),
- unary operators already supported today,
- binary operators already supported today,
- ternary expressions,
- immediate expressions (`#expr`),
- indirect expressions (`(expr)`),
- long indirect expressions (`[expr]`),
- tuples,
- lists,
- range expressions,
- member access,
- index access,
- struct literals,
- builtin-call expressions under the typed capability contract.

For those forms, the VM path must not require constructing a Rust `Expr` merely to compile the portable program.

Native Rust may still build `Expr` in fallback mode, but the VM path is authoritative for covered forms once the corresponding runtime contract is enabled.

This specification does not require the current `EXVM v1` expression-parser contract shape to remain intact. Expression parsing and expression bytecode generation may remain separate versioned boundaries or may collapse into one cleaner opcore-owned contract if that proves simpler.

### 7. Delegated Capability Contract

The host/runtime boundary for `EXPR v2` is limited to these operations:

- resolve symbol by name or interned ID,
- resolve current address,
- resolve whether a symbol is stable or finalized,
- evaluate string literal semantics in the active expression context,
- dispatch builtin calls that are explicitly delegated out of opcore,
- resolve member access for host-owned base kinds,
- resolve index access for host-owned base kinds.

The host boundary must not expose opaque callbacks such as:

- "parse this expression for me",
- "evaluate this AST for me",
- "reparse the original source text",
- "decide where the expression ends for me".

If a `v2` program uses `CallBuiltin`, `GetMember`, or `IndexValue` in a way that requires host participation and the capability is missing, execution must fail with a typed expression diagnostic.

### 8. Diagnostics Contract

The expression VM continues using the `ope...` namespace for expression-runtime failures.

This specification requires the catalog to distinguish at least these scenarios:

- unsupported opcode version,
- invalid stack type for operation,
- invalid scalar conversion request,
- missing delegated capability,
- unknown builtin name,
- invalid member access,
- invalid index access,
- malformed structural payload,
- boundary violation where a caller asks the expression contract to perform out-of-scope parsing or delegation.

Diagnostics must carry spans sourced from the expression tokens or from emitted program metadata. VM-originated failures must not rely on message-prefix conventions alone.

### 9. Relationship to Existing Scalar Semantics

The goal of this specification is to keep established language-level scalar semantics where they already define user-visible behavior, while allowing `v2` bytecode to change shape.

The following behaviors remain the semantic baseline for covered scalar operations unless intentionally revised in a later spec:

- unary and binary operator precedence,
- current-address resolution semantics,
- symbol lookup semantics and unstable-symbol reporting,
- ternary truthiness semantics,
- contextual string-literal evaluation behavior,
- budget enforcement for program bytes, symbol count, stack depth, and evaluation steps.

This means semantic continuity is preferred, but bytecode compatibility is not mandatory.

## Boundary Cases

1. Unsupported runtime version
   A package emits `EXPR v2`, but the runtime only supports `v1`.
   Required behavior: reject before execution with a typed unsupported-version diagnostic.

2. Caller passes an out-of-scope token range
   Example: `opasm` attempts to hand the expression VM a whole statement tail rather than one bounded expression range.
   Required behavior: fail with a typed boundary-violation or malformed-expression diagnostic rather than asking the host to recover heuristically.

3. Structural value in scalar-only context
   Example: a list, tuple, range, or struct literal reaches a caller that requested `Scalar` mode.
   Required behavior: fail through `RequireScalar` with a typed diagnostic. Do not guess a scalar projection.

4. Immediate and indirect wrappers used outside operand-shape consumers
   Required behavior: preserve the wrapper in `ShapePreserving` mode and reject in `Scalar` mode unless a later explicit conversion rule exists.

5. Builtin call without registered capability
   Required behavior: emit a typed capability-missing diagnostic. Do not fall back to a free-form Rust call path.

6. Member or index access on a VM-native structural base
   Required behavior: if the semantics are fully defined for VM-native values, opcore handles the operation without host help. Host delegation is not allowed just to avoid implementing VM-native semantics.

7. Member or index access on an incompatible base value
   Required behavior: emit a typed expression diagnostic that distinguishes "operation unsupported for this type" from "unknown field/index".

8. Placeholder outside an allowed deferred-shape context
   Required behavior: emit a typed diagnostic rather than treating placeholder as zero or empty.

9. Range values
   Required behavior: preserve range structure. Do not auto-expand to a list during ordinary expression evaluation.

10. Register references vs identifiers
    Required behavior: register-reference operations must preserve the distinction so later operand interpretation can tell whether a token was a register or a symbol-like identifier.

## Acceptance Criteria

- [ ] The specification defines `EXPR v2` as an opcore-owned expression contract over bounded token ranges, not as a whole-line parser.
- [ ] The specification makes the opasm-to-opcore delegation boundary explicit and binding.
- [ ] The specification treats the current Rust architecture and implementation as the source of truth for the opcore expression boundary.
- [ ] The specification explicitly states that backward compatibility with the current `v1` bytecode is not required.
- [ ] The specification defines the semantic value kinds carried by `v2`, including scalar and structural forms.
- [ ] The specification defines the minimum semantic opcode surface needed to cover immediate, indirect, long indirect, tuple, list, range, member, index, struct literal, placeholder, register reference, and builtin call behavior.
- [ ] The specification explicitly separates opcore-owned expression concerns from delegated contextual concerns.
- [ ] The VM path for covered forms is defined to compile tokens directly into portable expression bytecode without requiring a Rust AST round-trip.
- [ ] The host/VM boundary is constrained to typed callbacks and explicitly forbids opaque parse-or-evaluate delegation.
- [ ] Boundary behavior is explicit for out-of-scope token ranges, scalar-conversion failures, missing capabilities, incompatible base types, unsupported versions, and placeholder misuse.
- [ ] Validation expectations are concrete enough to derive parity and regression tests without embedding an implementation plan.

## Validation Expectations

The implementation derived from this specification is expected to prove behavior through:

- opcode encode and decode tests for `EXPR v2`,
- VM runtime tests for each new semantic operation and for mixed scalar and structural stacks,
- parity tests comparing native Rust fallback with VM-authoritative opcore parsing and evaluation for covered forms,
- delegated-boundary tests showing that opasm passes bounded operand ranges while opcore owns expression parsing and expression diagnostics,
- diagnostic tests asserting stable `ope...` codes and correct spans for VM-originated failures,
- budget enforcement tests for deep structural values, argument counts, malformed payloads, and boundary violations,
- assembler-path integration tests showing that covered operand-expression forms no longer require Rust AST construction on the authoritative VM path.

## Open Questions

1. Should builtin-call coverage in `v2` be split into a fixed opcore builtin catalog plus a separately delegated family-specific builtin layer?
2. Which member and index operations should be required to execute fully on VM-native structural values in the first `v2` slice?
3. Should struct-literal and member-access support be included in the first runtime slice, or should `v2` reserve those operations while initially enabling only immediate, indirect, tuple, list, range, and index coverage?
4. Should `Range` remain shape-preserving only, or does any caller contract need a standardized VM-side expansion rule later?
5. Should expression parsing remain a distinct `EXVM` contract that emits `EXPR v2` programs, or should parsing and bytecode generation collapse into one versioned opcore-owned boundary?