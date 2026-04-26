# opForge Native AmigaOS 68020-Baseline Tokenizer VM Single-Line Buffer ABI Spec v0.1

## Summary

This specification defines the minimal authoritative native buffer ABI for the
first `tokvm_run_68000` implementation slice on the AmigaOS-native `68020`
baseline.

The scope is intentionally narrow: one newline-free source line, one token
record array in caller-owned memory, one lexeme scratch buffer in caller-owned
memory, and a deterministic return-register contract that lets host-side tests
and the AmigaOS tokenizer harness render the existing `OPFORGE-TOKVM 1` text
report without inventing a private native layout.

## Problem

The current AmigaOS tokenizer harness specification defines the call registers
for `tokvm_run_68000` and the textual `OPFORGE-TOKVM 1` report shape, but it
does not define the in-memory token record layout or the native return-status
contract for the first assembly-side tokenizer interpreter.

Without that contract, the first native AmigaOS 68020-baseline slice can
assemble and run while still choosing an ad hoc token layout that host-side
validations and the CLI harness cannot verify against a stable authority.

## Goals

- [ ] Define the first single-line-only native `tokvm_run_68000` buffer ABI.
- [ ] Define `68020` as the baseline CPU for the first native AmigaOS tokenizer
  interpreter slice while keeping the existing `tokvm_run_68000` symbol name.
- [ ] Define a stable token record layout for the caller-owned token output
  buffer passed in `A1`.
- [ ] Define deterministic return-register semantics for tokenizer status,
  emitted token count, final cursor, and lexeme scratch usage.
- [ ] Define a stable token kind code table and report-rendering names for the
  first native slice.
- [ ] Keep the native ABI aligned with the current portable tokenizer span and
  lexeme semantics used by the Rust VM path.

## Non-Goals

- [ ] Do not define CLI parsing, file I/O, or the AmigaOS harness process
  return-code policy.
- [ ] Do not define multi-line or whole-file tokenization in one native call.
- [ ] Do not define refillable native input streams.
- [ ] Do not define parser VM, expression VM, or encoder VM native ABIs.
- [ ] Do not standardize the final source-buffer, token-buffer, or lexeme-buffer
  capacities for executables that use this ABI.

## Invariants / Constraints

The active worktree `AGENTS.md` workflow and execution rules remain binding for
any plan or implementation derived from this specification.

This ABI is for the first AmigaOS-native `68020` baseline only. Spec-derived
native tokenizer interpreter and harness code must target `.cpu 68020` unless
a later specification revises that baseline explicitly. Illustrative AmigaOS
examples outside that native tokenizer implementation slice are not required to
adopt the same baseline.

This ABI is single-line only. The input byte range passed in `A0` and `D0`
must not contain `0x0A` or `0x0D`. Multi-line iteration, line splitting, and
whole-file refill behavior remain out of scope for this v0.1 contract.

The token buffer in `A1` is an array of fixed-size records. The capacity in
`D1` counts complete records, not bytes and not a header-plus-records block.

The lexeme scratch buffer in `A2` is a byte-addressed payload area. Token
records refer into it by byte offset and byte length.

All integer fields stored in memory use Motorola 68000 big-endian byte order.

Column values use the same one-based, half-open span convention as the current
portable tokenizer path: `col_start` is the first byte column of the token and
`col_end` is one past the last byte column of the token.

The native interpreter remains generic to tokenizer VM opcodes and package
data. This ABI must not be used as a back door for package-specific tokenizer
semantics in interpreter control flow.

Where one byte sequence can carry more than one later semantic meaning, the
tokenizer ABI records the lexical form only. Later parser or expression stages
remain responsible for semantic interpretation.

## Behavioral Contract

The first native call shape remains:

- `A0`: source line bytes
- `D0`: source line byte length
- `A1`: token output buffer base address
- `D1`: token output capacity in records
- `A2`: lexeme scratch buffer base address
- `D2`: lexeme scratch capacity in bytes
- `A3`: tokenizer VM bytecode program
- `D3`: tokenizer VM bytecode length in bytes

Return-register contract for v0.1:

- `D0`: signed tokenizer status code
- `D1`: emitted token count in complete records
- `D2`: final cursor as a zero-based byte offset into the source line
- `D3`: lexeme scratch bytes committed into `A2`

Register preservation contract for `tokvm_run_68000` v0.1:

- caller-saved: `D0-D3`, `A0-A3`
- callee-preserved: `D4-D7`, `A4-A6`

### Status Codes

The v0.1 tokenizer status codes are:

- `0`: success
- `1`: newline unsupported for this single-line ABI
- `2`: token output capacity exceeded before the next token could be committed
- `3`: lexeme scratch capacity exceeded before the next token could be committed
- `4`: tokenizer VM signaled failure or diagnostic emission
- `5`: invalid argument or malformed runtime contract input
- `6`: invalid tokenizer VM program for the active contract

On success, `D2` must equal the input byte length from the call-site value of
`D0`.

On failure, `D1` and `D3` report only fully committed output. The interpreter
must not leave partially committed token records or partially committed lexeme
payload for the failing token.

On failure, `D2` must report the zero-based source-byte offset where the next
token could not be committed or where validation stopped. The universal v0.1
rule is:

- status `1`: offset of the first newline byte
- status `2`: start offset of the token that could not fit in the token record buffer
- status `3`: start offset of the token whose lexeme could not fit in scratch
- status `4`: current tokenizer cursor at the point the VM failure becomes observable
- status `5`: `0` when argument validation fails before token inspection
- status `6`: current tokenizer cursor at the point an invalid program or kind-code condition becomes observable

For newline rejection, the interpreter must not emit any tokens. `D1` and `D3`
must both be `0`, and `D2` must identify the zero-based offset of the first
newline byte.

### Token Record Layout

Each token record occupies 20 bytes in the `A1` buffer.

| Offset | Size | Field | Meaning |
|---|---:|---|---|
| `0` | 2 | `kind_code` | Token kind code from the table below. |
| `2` | 2 | `reserved` | Must be written as `0` in v0.1 and ignored by readers. |
| `4` | 4 | `col_start` | One-based start column. |
| `8` | 4 | `col_end` | One-based exclusive end column. |
| `12` | 4 | `lexeme_offset` | Zero-based byte offset into the `A2` lexeme scratch buffer. |
| `16` | 4 | `lexeme_len` | Lexeme byte length in `A2`. |

The `A2` scratch buffer stores token lexeme payloads densely in token order.
`D3` returns the total committed byte count. Readers must treat the valid
scratch region as `A2[0..D3)`.

For identifiers, registers, and numbers, the stored lexeme bytes must match the
post-policy token text that the portable tokenizer path would expose after
applying the active token-case rule.

For strings, the stored lexeme bytes are the raw token bytes used by the
portable tokenizer kind payload.

For punctuation and operators, the stored lexeme bytes are the exact token text
bytes shown in the kind table below.

### Token Kind Codes and Report Names

The first native slice uses these stable `kind_code` values and `KIND` report
names:

| Code | Report `KIND` | Lexeme text form |
|---|---|---|
| `0` | `identifier` | identifier bytes |
| `1` | `register` | register bytes |
| `2` | `number` | normalized number text |
| `3` | `string` | raw string token bytes |
| `4` | `comma` | `,` |
| `5` | `colon` | `:` |
| `6` | `dollar` | `$` |
| `7` | `dot` | `.` |
| `8` | `hash` | `#` |
| `9` | `question` | `?` |
| `10` | `open_bracket` | `[` |
| `11` | `close_bracket` | `]` |
| `12` | `open_brace` | `{` |
| `13` | `close_brace` | `}` |
| `14` | `open_paren` | `(` |
| `15` | `close_paren` | `)` |
| `16` | `op_range` | `..` |
| `17` | `op_range_inclusive` | `..=` |
| `18` | `op_plus` | `+` |
| `19` | `op_minus` | `-` |
| `20` | `op_multiply` | `*` |
| `21` | `op_caret` | `^` |
| `22` | `op_divide` | `/` |
| `23` | `op_mod` | `%` |
| `24` | `op_shl` | `<<` |
| `25` | `op_shr` | `>>` |
| `26` | `op_bit_not` | `~` |
| `27` | `op_logic_not` | `!` |
| `28` | `op_bit_and` | `&` |
| `29` | `op_bit_or` | `|` |
| `30` | `op_logic_and` | `&&` |
| `31` | `op_logic_or` | `||` |
| `32` | `op_logic_xor` | `^^` |
| `33` | `op_eq` | `==` |
| `34` | `op_ne` | `!=` |
| `35` | `op_ge` | `>=` |
| `36` | `op_gt` | `>` |
| `37` | `op_le` | `<=` |
| `38` | `op_lt` | `<` |

Codes outside this table are invalid for v0.1.

### Mapping to `OPFORGE-TOKVM 1`

The AmigaOS tokenizer harness renders the native output buffers as follows:

- `STATUS` comes from return `D0`
- `TOKENS` comes from return `D1`
- `CURSOR` comes from return `D2`
- each `TOKEN <index> ...` line is built from record `<index>` in `A1`
- `LEN` is the record `lexeme_len`
- `LEXHEX` is the uppercase hexadecimal encoding of
  `A2[lexeme_offset..lexeme_offset + lexeme_len)`

The harness must use the report `KIND` names from the table above instead of
inventing a second name mapping.

## Boundary Cases

Null pointers or impossible capacity combinations:

- if the caller provides a null buffer pointer with a nonzero matching
  capacity, the interpreter must return status `5`

Zero token capacity:

- if a non-empty line would emit at least one token and `D1 == 0`, the
  interpreter must return status `2` before committing output

Zero lexeme capacity:

- if a token requires lexeme bytes and `D2 == 0`, the interpreter must return
  status `3` before committing output

Unsupported newline input:

- if any source byte is `0x0A` or `0x0D`, the interpreter must return status
  `1` with no emitted tokens

Token overflow:

- if the next token would exceed the record capacity, the interpreter must
  return status `2` with all earlier committed tokens still readable

Lexeme overflow:

- if the next token would exceed the scratch capacity, the interpreter must
  return status `3` with all earlier committed tokens still readable

VM failure or emitted diagnostic:

- if the tokenizer VM resolves to a failure/diagnostic path, the interpreter
  must return status `4`, preserve only the last fully committed token set, and
  report in `D2` the current tokenizer cursor when the failure becomes observable

Invalid token kind code for the contract:

- if the native tokenizer reaches a token kind outside the v0.1 table, it must
  return status `6` and report in `D2` the cursor position for the invalid token

## Acceptance Criteria

- [ ] A spec-derived implementation can serialize token records into `A1`
  without a header block and lexeme bytes into `A2` using the 20-byte record
  layout defined here.
- [ ] A spec-derived implementation returns deterministic values in `D0`, `D1`,
  `D2`, and `D3` for success and failure cases.
- [ ] A host-side test can decode the native buffers and render the exact
  `STATUS`, `TOKENS`, `CURSOR`, and `TOKEN ... LEXHEX ...` lines required by
  `OPFORGE-TOKVM 1`.
- [ ] Newline-containing input is rejected with status `1` and zero emitted
  tokens.
- [ ] Token and lexeme overflow failures do not expose partially committed
  output for the failing token.
- [ ] The `KIND` names used by the harness are the canonical names defined by
  this specification.
- [ ] The ABI remains single-line only and does not imply whole-file tokenization
  in one native call.

## Validation Expectations

Spec validation:

- `python3 scripts/workflow/check_spec_artifact.py documentation/opForge-m68000-tokenizer-vm-single-line-buffer-abi-spec-v0_1.md`

Expected implementation validation derived from this spec:

- one focused host-side decode test for a successful single-line token buffer
- one focused host-side decode test for newline rejection with status `1`
- one focused host-side decode test for token capacity overflow
- one focused host-side decode test for lexeme scratch overflow
- one focused report-rendering test that converts fixed `A1` and `A2` payloads
  into the exact `OPFORGE-TOKVM 1` line set

## Open Questions

No open questions remain for the v0.1 single-line native buffer ABI. Multi-line
iteration, final executable buffer sizes, and harness CLI policy remain defined
by separate specs or later work.