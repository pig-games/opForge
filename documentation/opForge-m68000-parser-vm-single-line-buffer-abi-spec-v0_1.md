# opForge Native AmigaOS 68020-Baseline Parser VM Single-Line Buffer ABI Spec v0.1

## Summary

This specification defines the first native buffer ABI for the AmigaOS-native
`68020` baseline implementation of the opasm statement parser VM v2.

The native entry symbol is `prvm_run_68000`. The symbol name follows the
existing tokenizer-native naming family while the first implementation target
remains `.cpu 68020`.

The scope is deliberately narrow: one newline-free source line, caller-owned
source/token/lexeme input buffers, caller-owned result/diagnostic/resume
buffers, and deterministic status returns that let host-side tests compare the
decoded native result against the Rust `parse_statement_line_with_model` path.

## Problem

The Rust PRVM v2 implementation is now the behavioral authority for delegated
opasm statement parsing, but the native AmigaOS path cannot begin safely until
the memory and host-bridge contract is explicit.

Without this ABI, the first `prvm_run_68000` assembly slice could choose ad hoc
record layouts for tokens, parser results, diagnostics, or expression sub-call
state. That would make host-side parity tests fragile and would blur the
required boundary between native opasm statement parsing and Rust/opcore-owned
expression parsing.

This specification provides the narrow native contract that must exist before
native parser assembly or native parser fixtures land.

## Goals

- [ ] Define the first single-line native `prvm_run_68000` ABI for delegated
  opasm statement parsing.
- [ ] Reserve `prvm_run_68000` as the only native PRVM entry symbol for this
  contract.
- [ ] Define `.cpu 68020` as the first native implementation baseline while
  keeping the `68000` symbol-name convention.
- [ ] Define a caller-owned request frame that references source, token, lexeme,
  parser-program, result, diagnostic, resume, expression-request, and
  expression-result buffers.
- [ ] Define deterministic return registers and status codes for success,
  diagnostics, invalid input, output overflow, newline rejection, entry-boundary
  violation, and expression pause/resume.
- [ ] Define the result/event, diagnostic, expression-request, expression-result,
  and resume-state records needed by host-side decoders.
- [ ] Keep expression parsing Rust/opcore-owned by defining only an explicit
  token-range pause/resume sub-call protocol.

## Non-Goals

- [ ] Do not define a native expression parser.
- [ ] Do not define native opcore line routing or whole-file parsing.
- [ ] Do not define macro expansion, module graph execution, symbol resolution,
  instruction selection, or instruction encoding.
- [ ] Do not define the final AmigaOS CLI/file I/O harness UX.
- [ ] Do not define a stable serialized form for every Rust `LineAst` or `Expr`
  variant.
- [ ] Do not require emulator execution as a default local or CI quality gate.

## Invariants / Constraints

The active worktree `AGENTS.md` workflow and execution rules remain binding for
any plan or implementation derived from this specification.

This ABI is for the first AmigaOS-native `68020` baseline only. Spec-derived
native PRVM interpreter code must target `.cpu 68020` unless a later
specification revises that baseline explicitly.

`prvm_run_68000` is the only reserved native parser VM entry symbol for this
v0.1 contract. Additional native PRVM entry symbols require a later spec
revision.

This ABI starts after opcore has classified the line and delegated it as
`ProcessingRequestKind::Processor { processor: "asm", kind: "statement" }`.
Native PRVM code must not classify raw opForge lines or decide opcore/opasm
ownership.

This ABI is single-line only. The source byte range in the request frame must
not contain `0x0A` or `0x0D`. Whole-file parsing, refillable streams, and
line-splitting remain out of scope.

The primary parser input is the caller-owned token record array plus the
source/lexeme buffers needed for spans, text, diagnostics, and report rendering.
Native PRVM must not re-tokenize raw source bytes.

All integer fields stored in memory use Motorola 68000 big-endian byte order.

Spans use the same one-based, half-open column convention as the portable Rust
tokenizer path: `col_start` is the first byte column and `col_end` is one past
the last byte column.

Token ranges are zero-based token indexes into the input token record array and
are half-open: `start_token` is inclusive and `end_token` is exclusive.

The native interpreter remains generic to parser VM bytecode and package data.
CPU-family statement semantics must live in package-provided PRVM bytecode and
data, not in native interpreter branches.

Expression parsing remains Rust/opcore-owned. Native PRVM may request an
expression parse only by returning the ABI-defined expression-request status and
record. It must never parse the expression locally.

## Behavioral Contract

### Call Shape

The first native call shape is:

- `A0`: pointer to a `PRVM_REQUEST_FRAME_V1` record
- `D0`: size of the request frame in bytes

Return-register contract:

- `D0`: signed PRVM status code
- `D1`: result record count on success, expression slot index on expression
  request, or committed diagnostic count on diagnostic failure
- `D2`: status-specific cursor, token index, or source-byte offset as defined
  in the status table below
- `D3`: result buffer bytes committed on success, resume-state bytes committed
  on expression request, or diagnostic buffer bytes committed on diagnostic
  failure

Register preservation contract for `prvm_run_68000` v0.1:

- caller-saved: `D0-D3`, `A0-A3`
- callee-preserved: `D4-D7`, `A4-A6`

The request frame carries all additional buffer addresses and capacities so the
ABI can grow without consuming more call registers.

### Request Frame

`PRVM_REQUEST_FRAME_V1` is 112 bytes.

| Offset | Size | Field | Meaning |
|---|---:|---|---|
| `0` | 4 | `magic` | ASCII `OPRP` (`0x4F505250`). |
| `4` | 2 | `abi_version` | Must be `1`. |
| `6` | 2 | `frame_size` | Must be at least `112`. |
| `8` | 2 | `call_mode` | `0` start, `1` resume after expression result. |
| `10` | 2 | `entry_kind` | Must be `1` for delegated opasm statement. |
| `12` | 4 | `line_num` | Source line number for spans and diagnostics. |
| `16` | 4 | `source_ptr` | Pointer to newline-free source bytes. |
| `20` | 4 | `source_len` | Source byte length. |
| `24` | 4 | `token_ptr` | Pointer to input token records. |
| `28` | 4 | `token_count` | Number of complete input token records. |
| `32` | 2 | `token_record_size` | Must be `20` for v0.1. |
| `34` | 2 | `reserved0` | Must be `0`; ignored by readers. |
| `36` | 4 | `lexeme_ptr` | Pointer to token lexeme bytes. |
| `40` | 4 | `lexeme_len` | Valid lexeme byte length. |
| `44` | 4 | `program_ptr` | Pointer to parser VM bytecode. |
| `48` | 4 | `program_len` | Parser VM bytecode length in bytes. |
| `52` | 4 | `result_ptr` | Pointer to result/event output records. |
| `56` | 4 | `result_capacity` | Result buffer capacity in bytes. |
| `60` | 4 | `diagnostic_ptr` | Pointer to diagnostic records. |
| `64` | 4 | `diagnostic_capacity` | Diagnostic buffer capacity in bytes. |
| `68` | 4 | `resume_ptr` | Pointer to resume-state bytes. |
| `72` | 4 | `resume_capacity` | Resume-state buffer capacity in bytes. |
| `76` | 4 | `expr_request_ptr` | Pointer to one expression-request record. |
| `80` | 4 | `expr_request_size` | Must be at least `32`. |
| `84` | 4 | `expr_result_ptr` | Pointer to expression-result slot records. |
| `88` | 4 | `expr_result_count` | Number of expression-result slot records. |
| `92` | 4 | `parser_contract_version` | Expected PRVM opcode/contract version. |
| `96` | 4 | `step_budget` | Maximum native interpreter steps for this call. |
| `100` | 4 | `flags` | Must be `0` for v0.1. |
| `104` | 4 | `reserved1` | Must be `0`; ignored by readers. |
| `108` | 4 | `reserved2` | Must be `0`; ignored by readers. |

Pointer fields may be `0` only when their matching length or capacity is `0`
and the field is not required for the requested operation. Required nonzero
pointers for `call_mode = 0` are `source_ptr`, `token_ptr` when `token_count >
0`, `program_ptr`, `result_ptr`, `diagnostic_ptr`, `resume_ptr`,
`expr_request_ptr`, and `expr_result_ptr` when `expr_result_count > 0`.

`entry_kind` values:

- `1`: delegated opasm statement request

All other values are entry-boundary violations in v0.1.

`call_mode` values:

- `0`: start a new PRVM parse from token cursor `0`
- `1`: resume from the state saved in `resume_ptr` after the host has filled
  the requested expression-result slot

All other values are invalid runtime contract input.

### Status Codes

The v0.1 PRVM status codes and return-register meanings are:

| Code | Name | Meaning | `D1` | `D2` | `D3` |
|---:|---|---|---|---|---|
| `0` | `PRVM_STATUS_OK` | A final parser result was emitted. | Result record count. | Final token cursor. | Result bytes committed. |
| `1` | `PRVM_STATUS_EXPR_REQUEST` | Native paused and requests Rust/opcore expression parsing. | Requested expression slot index. | Current token cursor. | Resume-state bytes committed. |
| `2` | `PRVM_STATUS_NEWLINE_UNSUPPORTED` | Source contains `0x0A` or `0x0D`. | `0`. | Source-byte offset of first newline. | `0`. |
| `3` | `PRVM_STATUS_ENTRY_BOUNDARY` | `entry_kind` is not delegated opasm statement. | `0`. | `0`. | `0`. |
| `4` | `PRVM_STATUS_INVALID_ARGUMENT` | Request frame, pointer, capacity, or mode is invalid. | `0`. | `0`. | `0`. |
| `5` | `PRVM_STATUS_INVALID_TOKEN` | Token record or lexeme reference is malformed. | `0`. | Offending token index, or `0` if unavailable. | `0`. |
| `6` | `PRVM_STATUS_INVALID_PROGRAM` | Parser contract or bytecode is invalid for v0.1. | `0`. | Current token cursor, or `0` before execution. | `0`. |
| `7` | `PRVM_STATUS_OUTPUT_OVERFLOW` | Result, diagnostic, expression-request, or resume capacity was exceeded. | Fully committed record count for the buffer being written, or `0` for expression-request/resume overflow. | Current token cursor. | Fully committed bytes for the buffer being written, or `0` for expression-request/resume overflow. |
| `8` | `PRVM_STATUS_DIAGNOSTIC` | Parser failed with one or more committed diagnostics. | Diagnostic record count. | Current token cursor. | Diagnostic bytes committed. |
| `9` | `PRVM_STATUS_UNSUPPORTED_OPCODE` | Bytecode uses an opcode outside the implemented native slice. | `0`. | Current token cursor. | `0`. |
| `10` | `PRVM_STATUS_INVALID_RESUME` | Resume state is absent, malformed, or for a different request. | `0`. | Resume cursor if recoverable, otherwise `0`. | `0`. |
| `11` | `PRVM_STATUS_EXPR_RESULT_INVALID` | Requested expression slot is missing or invalid on resume. | Requested expression slot index if recoverable, otherwise `0`. | Resume cursor if recoverable, otherwise `0`. | `0`. |
| `12` | `PRVM_STATUS_BUDGET_EXCEEDED` | Step, stack, or checkpoint budget was exceeded. | `0`. | Current token cursor. | `0`. |

On success, `D1` is the number of committed result records and `D3` is the
number of result-buffer bytes committed.

On `PRVM_STATUS_EXPR_REQUEST`, `D1` is the requested expression slot index,
`D2` is the current token cursor, and `D3` is the number of resume-state bytes
committed.

On diagnostic failure, `D1` is the number of committed diagnostic records and
`D3` is the number of diagnostic-buffer bytes committed.

On all failures, native PRVM must leave only fully committed records readable.
Partially written result, diagnostic, expression-request, or resume records are
not valid output.

For hard failures with `D1 == 0` and `D3 == 0`, host decoders must ignore
result, diagnostic, expression-request, and resume buffers unless the status
description above explicitly says committed records are readable.

### Token Input Records

The PRVM input token record format is the tokenizer ABI's 20-byte token record.
The request frame `token_record_size` must be `20` for v0.1.

| Offset | Size | Field | Meaning |
|---|---:|---|---|
| `0` | 2 | `kind_code` | Token kind code from the tokenizer ABI table. |
| `2` | 2 | `reserved` | Must be `0`; ignored by readers. |
| `4` | 4 | `col_start` | One-based start column. |
| `8` | 4 | `col_end` | One-based exclusive end column. |
| `12` | 4 | `lexeme_offset` | Zero-based byte offset into `lexeme_ptr`. |
| `16` | 4 | `lexeme_len` | Lexeme byte length. |

Native PRVM must validate that every token lexeme range is within
`lexeme_ptr[0..lexeme_len)` before it reads token text.

Token `kind_code` values are inherited from
`documentation/opForge-m68000-tokenizer-vm-single-line-buffer-abi-spec-v0_1.md`.
Codes outside that table are invalid for this parser ABI.

### Result/Event Records

The result buffer is an array of 32-byte `PRVM_RESULT_RECORD_V1` records. The
host decoder reads only the first `D1` records on success.

| Offset | Size | Field | Meaning |
|---|---:|---|---|
| `0` | 2 | `record_kind` | Result record kind. |
| `2` | 2 | `flags` | Must be `0` in v0.1 unless specified by kind. |
| `4` | 4 | `span_line` | Source line number for this record. |
| `8` | 4 | `col_start` | One-based start column, or `0` when not applicable. |
| `12` | 4 | `col_end` | One-based exclusive end column, or `0` when not applicable. |
| `16` | 4 | `arg0` | Kind-specific value. |
| `20` | 4 | `arg1` | Kind-specific value. |
| `24` | 4 | `arg2` | Kind-specific value. |
| `28` | 4 | `arg3` | Kind-specific value. |

Result `record_kind` values:

| Code | Name | Meaning |
|---:|---|---|
| `1` | `BEGIN_STATEMENT` | Begin a decoded `LineAst::Statement`. |
| `2` | `LABEL_TEXT` | Optional leading label text. |
| `3` | `MNEMONIC_TEXT` | Statement mnemonic text. |
| `4` | `OPERAND_EXPR_SLOT` | Operand expression reference by expression slot. |
| `5` | `FINISH_LINE` | Finalize the decoded statement result. |
| `6` | `EMPTY_LINE` | Decode as `LineAst::Empty` for an empty delegated token stream. |

For `LABEL_TEXT` and `MNEMONIC_TEXT`, `arg0` is `lexeme_offset` and `arg1` is
`lexeme_len` into the request lexeme buffer.

For `OPERAND_EXPR_SLOT`, `arg0` is `operand_index`, `arg1` is
`expr_slot_index`, `arg2` is the inclusive operand `start_token`, and `arg3` is
the exclusive operand `end_token`.

The v0.1 final successful statement event sequence is:

1. `BEGIN_STATEMENT`
2. optional `LABEL_TEXT`
3. exactly one `MNEMONIC_TEXT`
4. zero or more `OPERAND_EXPR_SLOT` records in operand order
5. `FINISH_LINE`

Host decoders must reject out-of-order or duplicate structural records as a
malformed native result rather than guessing a Rust AST.

### Diagnostic Records

The diagnostic buffer is an array of 32-byte `PRVM_DIAGNOSTIC_RECORD_V1`
records. The host decoder reads only the first `D1` records when status is
`PRVM_STATUS_DIAGNOSTIC`.

| Offset | Size | Field | Meaning |
|---|---:|---|---|
| `0` | 2 | `diag_code` | Stable diagnostic code from the table below. |
| `2` | 2 | `flags` | Must be `0` in v0.1. |
| `4` | 4 | `span_line` | Diagnostic line number. |
| `8` | 4 | `col_start` | One-based start column. |
| `12` | 4 | `col_end` | One-based exclusive end column. |
| `16` | 4 | `token_index` | Token index associated with the diagnostic, or `0xFFFFFFFF`. |
| `20` | 4 | `message_offset` | Lexeme-buffer offset for optional message text, or `0xFFFFFFFF`. |
| `24` | 4 | `message_len` | Message byte length, or `0`. |
| `28` | 4 | `reserved` | Must be `0`; ignored by readers. |

Diagnostic codes:

| Code | Meaning |
|---:|---|
| `1` | missing mnemonic |
| `2` | malformed operand separator |
| `3` | unexpected trailing token |
| `4` | parser ended without result |
| `5` | parser VM contract or opcode version mismatch |
| `6` | parser checkpoint depth exceeded |
| `7` | forbidden cross-contract opcode |
| `8` | misrouted opcore directive |
| `9` | invalid parser opcode or operand |
| `10` | unsupported native parser opcode in the current slice |

The diagnostic code table is intentionally compact. The host may map these
codes onto Rust-side diagnostic strings for parity reporting, but the native ABI
must preserve span fields and any message text it emits.

### Expression Pause/Resume Protocol

Native PRVM requests Rust/opcore expression parsing by writing exactly one
32-byte `PRVM_EXPR_REQUEST_V1` record and returning
`PRVM_STATUS_EXPR_REQUEST`.

| Offset | Size | Field | Meaning |
|---|---:|---|---|
| `0` | 2 | `request_version` | Must be `1`. |
| `2` | 2 | `flags` | Must be `0` in v0.1. |
| `4` | 4 | `operand_index` | Zero-based operand index in the statement. |
| `8` | 4 | `expr_slot_index` | Expression-result slot the host must fill. |
| `12` | 4 | `start_token` | Inclusive token index for Rust/opcore expression parsing. |
| `16` | 4 | `end_token` | Exclusive token index for Rust/opcore expression parsing. |
| `20` | 4 | `boundary_line` | Boundary span line used when range is empty. |
| `24` | 4 | `boundary_col_start` | Boundary start column. |
| `28` | 4 | `boundary_col_end` | Boundary exclusive end column. |

The host bridge must parse exactly `tokens[start_token..end_token]` through the
existing Rust/opcore expression parser. It must not widen the range to the rest
of the line or reclassify the line.

Expression-result slots are 32-byte `PRVM_EXPR_RESULT_SLOT_V1` records in the
buffer referenced by `expr_result_ptr`.

| Offset | Size | Field | Meaning |
|---|---:|---|---|
| `0` | 2 | `slot_state` | `0` empty, `1` ready expression, `2` ready expression error. |
| `2` | 2 | `flags` | Must be `0` in v0.1. |
| `4` | 4 | `expr_slot_index` | Slot index; must match the requested slot. |
| `8` | 4 | `span_line` | Expression or expression-error span line. |
| `12` | 4 | `col_start` | One-based start column. |
| `16` | 4 | `col_end` | One-based exclusive end column. |
| `20` | 4 | `host_expr_handle` | Host-owned opaque handle/index for decoded Rust `Expr`. |
| `24` | 4 | `reserved0` | Must be `0xFFFFFFFF` in v0.1. |
| `28` | 4 | `reserved1` | Must be `0` in v0.1. |

The native interpreter treats `host_expr_handle` as opaque. Its only native
responsibility is to verify that the requested slot is ready on resume and to
emit `OPERAND_EXPR_SLOT` records that preserve operand order and slot indexes.
The host decoder replaces those slot references with the Rust `Expr` values it
already owns.

For both `slot_state = 1` and `slot_state = 2`, `host_expr_handle` is the sole
authoritative expression payload. For `slot_state = 2`, the handle identifies a
host-owned Rust `Expr::Error` or equivalent normalized expression-error object;
the native ABI does not serialize expression-error message text in v0.1.

`PRVM_STATUS_EXPR_REQUEST` requires native PRVM to commit enough resume state to
continue deterministically after the host fills the expression-result slot.
Resume state is native-owned opaque bytes, but v0.1 requires the first 16 bytes
to be a stable header:

| Offset | Size | Field | Meaning |
|---|---:|---|---|
| `0` | 4 | `magic` | ASCII `PRRS` (`0x50525253`). |
| `4` | 2 | `resume_version` | Must be `1`. |
| `6` | 2 | `header_size` | Must be at least `16`. |
| `8` | 4 | `continuation_id` | Native continuation identifier. |
| `12` | 4 | `requested_expr_slot` | Slot index requested before pause. |

The host must pass the same request frame back with `call_mode = 1`, the same
source/token/lexeme/program buffers, the same resume buffer contents, and the
requested expression-result slot set to ready.

## Boundary Cases

Unsupported newline input:

- If any source byte is `0x0A` or `0x0D`, `prvm_run_68000` must return
  `PRVM_STATUS_NEWLINE_UNSUPPORTED` before committing result or diagnostic
  records.

Non-delegated entry request:

- If `entry_kind != 1`, `prvm_run_68000` must return
  `PRVM_STATUS_ENTRY_BOUNDARY` and must not execute parser bytecode.

Invalid request frame:

- Invalid magic, undersized frame, unsupported ABI version, impossible pointer
  and capacity combinations, unsupported `call_mode`, or nonzero reserved fields
  must return `PRVM_STATUS_INVALID_ARGUMENT`.

Malformed token records:

- Invalid token kind codes, decreasing spans, or lexeme ranges outside the
  lexeme buffer must return `PRVM_STATUS_INVALID_TOKEN` before token text is
  consumed.

Output overflow:

- If the next complete result, diagnostic, expression-request, or resume record
  cannot fit, native PRVM must return `PRVM_STATUS_OUTPUT_OVERFLOW` and expose
  only earlier fully committed records.

Expression request with empty range:

- Empty operand ranges are allowed only when Rust PRVM v2 would preserve an
  expression error for that operand. The expression request must still carry a
  bounded half-open token range and boundary span.

Expression result missing on resume:

- If `call_mode = 1` but the requested expression slot is not ready, has a
  mismatched slot index, or has nonconforming reserved fields, native PRVM must
  return `PRVM_STATUS_EXPR_RESULT_INVALID`.

Invalid resume state:

- Missing `PRRS` magic, unsupported resume version, mismatched continuation, or
  state that does not match the current request must return
  `PRVM_STATUS_INVALID_RESUME`.

Unsupported native opcode during early implementation slices:

- Until native opcode coverage reaches the full Rust PRVM v2 authority corpus,
  unsupported but otherwise valid parser opcodes must return
  `PRVM_STATUS_UNSUPPORTED_OPCODE` rather than silently diverging.

## Acceptance Criteria

- [ ] The ABI reserves only `prvm_run_68000` for native PRVM v0.1.
- [ ] The ABI states `.cpu 68020` as the first native implementation baseline.
- [ ] The call contract uses a caller-owned request frame and deterministic
  return registers.
- [ ] The request frame references source, token, lexeme, parser-program,
  result, diagnostic, resume, expression-request, and expression-result buffers.
- [ ] Token input records are compatible with the tokenizer single-line buffer
  ABI.
- [ ] Result/event records are sufficient to decode a `LineAst::Statement` with
  optional label, mnemonic, ordered operand expression slots, spans, and an
  explicit finish event.
- [ ] Diagnostic records carry stable codes and token-derived spans.
- [ ] Expression sub-calls use explicit half-open token ranges and named
  expression-result slots.
- [ ] Newline-containing source and non-delegated entry requests are explicit
  failure cases.
- [ ] The spec does not authorize native expression parsing, native opcore
  routing, native instruction encoding, or a full assembler pass.

## Validation Expectations

This specification must pass:

- `python3 scripts/workflow/check_spec_artifact.py documentation/opForge-m68000-parser-vm-single-line-buffer-abi-spec-v0_1.md`
- spec quality review before it is treated as an implementation authority
- `plan-compliance-reviewer` before the Work item 1 commit

Implementation work derived from this specification must also run the relevant
full quality gate for its work item, including `cargo fmt --all`, `cargo clippy
--all-targets --all-features -- -D warnings`, `cargo audit`, and `cargo test
--workspace`, subject only to explicitly recorded user-waived baseline failures.

The first host-side ABI tests derived from this spec should decode:

- one success result with label, mnemonic, and operand expression slot records,
- one diagnostic result with span and diagnostic code,
- one expression request with bounded operand token range and resume state,
- one expression-result resume path,
- newline rejection,
- entry-boundary rejection.

## Open Questions

No open questions block v0.1. Later work items may revise this specification if
host-side decode tests prove that additional result record kinds or expression
slot metadata are required for Rust PRVM v2 parity.