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

## Contract catalog

### `CONTRACT_EXPR_REQ_001`
Rust reference: VM expression request construction; Native boundary: expression request bridge; Condition: span is inside request text; Failure meaning: malformed source window; Stability: stable

### `CONTRACT_EXPR_REQ_002`
Rust reference: VM expression request construction; Native boundary: expression request bridge; Condition: full operand end equals length plus one; Failure meaning: truncated operand window; Stability: stable

### `CONTRACT_EXPR_REQ_003`
Rust reference: VM expression request construction; Native boundary: expression request bridge; Condition: non-empty text has a non-null pointer; Failure meaning: invalid request storage; Stability: stable

### `CONTRACT_EXPR_REQ_004`
Rust reference: VM expression request construction; Native boundary: expression request bridge; Condition: request kind matches the boundary; Failure meaning: request routing mismatch; Stability: stable

### `CONTRACT_BUF_001`
Rust reference: native request ABI; Native boundary: request/error storage; Condition: request and last-error buffers do not overlap; Failure meaning: diagnostic corruption risk; Stability: stable

### `CONTRACT_BUF_002`
Rust reference: native extension ABI; Native boundary: extension/IO storage; Condition: extension and IO buffers do not overlap; Failure meaning: request corruption risk; Stability: stable

### `CONTRACT_BUF_003`
Rust reference: native result ABI; Native boundary: result/token storage; Condition: result and token scratch buffers do not overlap; Failure meaning: result corruption risk; Stability: stable

### `CONTRACT_RESULT_001`
Rust reference: VM result-slot contract; Native boundary: expression result bridge; Condition: result pointer is valid; Failure meaning: invalid result target; Stability: stable

### `CONTRACT_RESULT_002`
Rust reference: VM result-slot contract; Native boundary: expression result bridge; Condition: slot index matches the request; Failure meaning: result/request mismatch; Stability: stable

### `CONTRACT_RESULT_003`
Rust reference: VM result-slot contract; Native boundary: expression result bridge; Condition: result target is not scratch memory; Failure meaning: unstable result lifetime; Stability: stable

### `CONTRACT_STMT_001`
Rust reference: portable statement model; Native boundary: statement store; Condition: statement index is valid; Failure meaning: invalid statement access; Stability: stable

### `CONTRACT_STMT_002`
Rust reference: portable statement model; Native boundary: statement store; Condition: statement has source metadata; Failure meaning: source attribution loss; Stability: stable

### `CONTRACT_STMT_003`
Rust reference: portable statement model; Native boundary: statement store; Condition: operand bounds are inside the source line; Failure meaning: malformed operand window; Stability: stable

### `CONTRACT_STMT_004`
Rust reference: portable statement model; Native boundary: statement store; Condition: label-only statement has no mnemonic or operand; Failure meaning: statement shape mismatch; Stability: stable

### `CONTRACT_STMT_005`
Rust reference: portable statement model; Native boundary: directive store; Condition: directive kind matches the statement; Failure meaning: directive routing mismatch; Stability: stable

### `CONTRACT_PASS_001`
Rust reference: assembler pass model; Native boundary: native session; Condition: pass is one or two; Failure meaning: invalid pass state; Stability: stable

### `CONTRACT_PASS_002`
Rust reference: assembler pass model; Native boundary: native symbol session; Condition: pass-two label count agrees with pass one; Failure meaning: unstable symbol layout; Stability: stable

### `CONTRACT_PASS_003`
Rust reference: assembler diagnostics; Native boundary: pass-two parser; Condition: unresolved label is not trailing text; Failure meaning: diagnostic category mismatch; Stability: stable

### `CONTRACT_PASS_004`
Rust reference: assembler sizing; Native boundary: pass-one encoder; Condition: unresolved label may use placeholder sizing; Failure meaning: premature pass-one rejection; Stability: stable

### `CONTRACT_SEL_001`
Rust reference: package selector; Native boundary: native selector; Condition: selected shape comes from package/parser data; Failure meaning: host-specific selection leaked in; Stability: stable

### `CONTRACT_SEL_002`
Rust reference: package selector; Native boundary: native selector; Condition: bare direct operand shape is preserved; Failure meaning: operand shape drift; Stability: stable

### `CONTRACT_SEL_003`
Rust reference: package selector; Native boundary: native selector; Condition: selected and package candidate kinds match; Failure meaning: candidate mismatch; Stability: stable

### `CONTRACT_ENC_001`
Rust reference: package encoder; Native boundary: native encoder; Condition: encoded byte count matches selected plan; Failure meaning: output length mismatch; Stability: stable
