# Native Debug Contracts

Native debug contracts are executable documentation of Rust-derived invariants
at a named 68000 boundary. Each call site must identify its Rust reference,
native boundary, condition, failure meaning, allowed build mode, and stability.

Contract IDs are stable 16-bit values in
`native/motorola68000/amigaos/debug/debug_contract_ids.asm`. IDs are grouped by
expression request, buffer, result, statement, pass, selector, and encoder
contracts. Renaming a symbolic constant does not authorize reusing its numeric
ID for a different invariant.

Use this comment form:

```asm
; CONTRACT_EXPR_REQ_001
; Rust reference: <file and function>
; Native boundary: <module and routine>
; Rule: the 1-based, end-exclusive span is inside request text.
; Failure means: native request construction produced a malformed window.
; Allowed only in: OPFORGE_DEBUG_CONTRACTS builds.
; Stability: stable.
	.DEBUG_ASSERT_SPAN_IN_TEXT contracts.CONTRACT_EXPR_REQ_001
```

Stability is `stable`, `transitional`, or `diagnostic-only`. Transitional
contracts state removal criteria. Diagnostic-only contracts cannot support a
parity completion claim.

An assertion failure appends `EVENT_ASSERT_FAIL` with the contract ID, predicate
routine ID, and four numeric arguments. Host tooling decodes those fields; call
sites do not emit free-form text.
