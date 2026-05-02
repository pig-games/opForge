# Native VM Pipeline Report and Handoff Specification v0.1

## Summary

This specification defines the temporary native AmigaOS `opforge_cli` report
and handoff records used while the native VM front end is being brought online.
The report starts with `OPFORGE-NATIVE 1` and gives Rust tests, FS-UAE smokes,
and later native stages one stable stream to observe host bootstrap,
tokenization, parser, expression, diagnostic, and emitter-boundary behavior.

The contract is intentionally narrow. It is a front-end report and handoff
format, not a native object format and not a replacement for the eventual
emitter plan. Records are textual, line-oriented, bounded, and shaped so that a
68020 implementation can write them with fixed buffers and simple decimal or
hex formatting.

## Problem

The native CLI now has three independently useful pieces: package-backed
tokenization, transitional host-owned `.module` / `.use` recognition, and
one-level host-owned `.include` expansion. The next parser and expression VM
slices need a stable way to report what each stage produced without coupling
tests to ad hoc strings or making the native emitter stub look like a real
assembler backend.

Without this boundary, Work item 5 would have to invent parser result records as
part of the first `PRVM` integration, while also deciding how include source
locations, module/use summaries, expression requests, diagnostics, and emitter
handoff markers fit together. That would make the first `PRVM` slice larger
than necessary and would risk moving host bootstrap responsibilities into the
VM path by accident.

The source contracts this report composes are:

- `documentation/vm-boundary-protocol-v1.md`
- `documentation/opForge-native-amigaos-module-use-port-spec-v0_1.md`
- `documentation/opForge-native-include-preprocessor-input-spec-v0_1.md`
- `examples/motorola68000/amigaos/opforge/opforge_cli.asm`

## Goals

- [ ] Define the line-oriented `OPFORGE-NATIVE 1` report records for host
      bootstrap, tokenizer, parser, expression, diagnostic, and emitter-boundary
      stages.
- [ ] Define module/use summary records that can represent module ids, import
      module ids, import aliases, selected item names, selected item aliases,
      and parser-record-only selected-item aliases.
- [ ] Define include/input-expansion records that preserve parent file, child
      file, include depth, and logical source location mapping.
- [ ] Define statement-result and expression-result records that are bounded
      and easy to emit from 68020 assembly.
- [ ] Define diagnostic records that preserve native `OPC-NCLI` codes and
      logical source locations for later Rust decoding.
- [ ] Define a temporary emitter-boundary marker that proves the front end has
      reached the handoff point without claiming object generation support.

## Non-Goals

- [ ] Do not define Hunk, relocation, listing, symbol table, map, or object
      emission behavior in this report contract.
- [ ] Do not move `.include`, `.module`, `.use`, module graph loading, macro
      expansion, or pass orchestration into `TKVM`, `PRVM`, or `EXVM`.
- [ ] Do not define new tokenizer, parser, or expression bytecode opcodes.
- [ ] Do not require binary report encoding for this roadmap slice.
- [ ] Do not require full Rust module graph, macro expansion, or preprocessor
      parity before the first parser VM smoke.
- [ ] Do not make this report a stable external CLI API; it is a temporary
      native front-end contract until the emitter plan supersedes it.

## Invariants / Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding
  for work derived from this specification.
- `OPFORGE-NATIVE 1` is the report version header. A decoder must reject any
  other major report header unless it explicitly supports that version.
- Records are newline-terminated ASCII text. Numeric fields are unsigned decimal
  unless the field name says `_HEX`.
- Every record begins with an uppercase record tag. Tags are case-sensitive.
- Fixed fields precede free text. If a record has a text payload, the payload
  starts after the last fixed field and one separating space.
- String payloads may contain spaces. First native slices should avoid tabs and
  control characters in payloads; later binary-safe escaping can be added under
  a new report minor version.
- File ids, module ids, import ids, statement ids, expression ids, and diagnostic
  ids are one-based within one CLI invocation. `0` means absent or not
  applicable when the record explicitly allows it.
- Logical source locations use the expanded input mapping after include
  processing: `file-id`, one-based `line`, and one-based `column` where known.
- Host-owned bootstrap records must be emitted before VM parser/expression
  result records that depend on them.
- `PRVM` owns statement and operand-shape parsing after host bootstrap has
  prepared the input stream. `EXVM` owns only bounded mathematical expression
  token ranges requested by `PRVM`.
- The native emitter remains a stub in this roadmap. Reaching the emitter
  boundary is observable; generating final artifacts is out of scope.

## Behavioral Contract

### Report Envelope

A successful native report begins with the existing header:

```text
OPFORGE-NATIVE 1
```

The header is followed by zero or more records. A stage starts with `STAGE` and
ends with either `STATUS` or `DIAG`. Stage order for the first complete front-end
pipeline is:

1. `bootstrap`
2. `include`
3. `tokenizer`
4. `module-use`
5. `parser`
6. `expression`
7. `emitter-boundary`

The report may omit stages that are not reached after a terminal diagnostic.
The native CLI may also emit a subset while a stage is still unimplemented, but
it must use deterministic diagnostics rather than ambiguous free-form failure
text.

Canonical stage/status records:

```text
STAGE <stage-name>
STATUS <stage-name>-ok
STATUS <stage-name>-skipped <reason-code>
```

The current compatibility aliases remain valid for slices already landed:

```text
STATUS tokenizer-ok
STATUS include-ok
STATUS parser-module-use-ok
```

Later implementation slices may emit both aliases and canonical records during a
transition. Tests should prefer canonical records once the producing slice lands.

### Source And Include Records

Source records describe the logical line stream consumed by `TKVM` and later
`PRVM`.

```text
SRC-FILE <file-id> <path>
SRC-LINE <line-id> <file-id> <line> <text-len> <text>
SRC-MAP <line-id> <file-id> <line> <parent-file-id> <parent-line> <include-depth>
```

`SRC-LINE` text is bounded by `text-len`; the textual report still writes the
text after the last space for human inspection. Native implementations may cap
`text-len` to the same source-line buffer capacity used by the tokenizer.

Include records preserve the Work item 3 report while adding ids that later
stages can join against:

```text
INCLUDE-ROOT <root-id> <path>
INCLUDE-FILE <file-id> <path>
INCLUDE-ENTER <depth> <parent-file-id> <parent-line> <child-file-id> <target>
INCLUDE-LINE <file-id> <line> <path>
INCLUDE-LEAVE <depth> <child-file-id>
```

The existing first-slice `INCLUDE-ROOT 1 <path>`, `INCLUDE-FILE 1 <path>`, and
`INCLUDE-LINE <depth> <line> <path>` forms are accepted as compatibility forms
until a later implementation emits the full id-bearing form. Decoders must treat
the compatibility form as an observation record, not as proof that full source
mapping is available.

### Module/Use Summary Records

Module/use records summarize host-owned bootstrap state. They do not replace
`PRVM` parser records for `.module`, `.endmodule`, or `.use` syntax; they report
the host state that later macro, parser, expression, and emitter stages may
need.

```text
MOD-ROOT <module-id>
MOD-DEF <module-id> <file-id> <line> <depth> <name-len> <name>
MOD-END <module-id> <file-id> <line> <depth>
MOD-PATH <path-index> <path>
USE-IMPORT <import-id> <owner-module-id> <import-module-id> <file-id> <line> <alias-len> <alias>
USE-SELECT <import-id> <item-index> <item-len> <item> <alias-len> <alias> <flags>
USE-WILDCARD <import-id> <flags>
```

`alias-len` is `0` when no alias is present. The text payload is omitted when
its length is `0`.

`MOD-PATH` records preserve module root order. Index `0` is the implicit input
root; later indexes are repeatable `-M` / `--module-path` roots in command-line
order.

`USE-SELECT` flags are decimal bit flags:

| Bit | Name | Meaning |
|---:|---|---|
| 0 | `parser-alias-only` | Selected-item alias came from parser syntax but is not semantically applied by the current module import injection path. |
| 1 | `public-only` | Selected item must resolve through a public export. |
| 2 | `reserved` | Reserved for the first native macro/import expansion slice. |

The first table-backed module/use slice may emit only records it can prove from
native table state. Transitional print-only records such as `MODULE <name>` and
`USE <name>` remain compatibility observations and must not be treated as module
tables by later stages.

### Tokenizer Records

The tokenizer stage may continue to print existing tokenizer rows. The handoff
record that later stages should use is:

```text
TOK-LINE <line-id> <file-id> <line> <token-count> <status-code>
TOK-REC <line-id> <token-index> <kind-id> <span-start> <span-end> <text-len> <text>
```

`span-start` and `span-end` are one-based inclusive columns in the logical source
line when known. Token text is bounded by `text-len`; empty token text is allowed
only for token kinds that explicitly do not carry source spelling.

### Statement Records

`PRVM` statement output is reported through bounded records. Work item 5 should
start with one no-expression statement shape and may emit a subset of these
records.

```text
STMT-RESULT <stmt-id> <line-id> <file-id> <line> <kind-id> <flags>
STMT-MNEMONIC <stmt-id> <text-len> <text>
STMT-OPERAND <stmt-id> <operand-index> <shape-id> <span-start> <span-end> <flags>
STMT-EXPR-REQ <stmt-id> <expr-id> <operand-index> <span-start> <span-end> <token-start> <token-count>
```

Statement flags are decimal bit flags:

| Bit | Name | Meaning |
|---:|---|---|
| 0 | `has-label` | The statement has a parsed label field. |
| 1 | `has-operands` | At least one operand record follows. |
| 2 | `needs-expression` | One or more `STMT-EXPR-REQ` records follow. |
| 3 | `directive` | The statement is an assembler directive rather than an instruction. |

`shape-id` is the parser VM's stable operand-shape id for the active contract.
The textual report does not define the shape table itself; it only carries the
id returned by `PRVM` for native/Rust parity tests.

### Expression Records

`EXVM` records describe parsing of bounded mathematical expression token ranges
requested by `PRVM`. They do not include CPU-family operand wrappers.

```text
EXPR-RESULT <expr-id> <stmt-id> <status-code> <value-kind> <span-start> <span-end> <flags>
EXPR-SCALAR <expr-id> <value-hex>
EXPR-SHAPE <expr-id> <shape-kind> <payload-len> <payload>
```

`EXPR-SCALAR` uses an uppercase hexadecimal unsigned representation of the
native scalar payload. Signed interpretation remains the responsibility of the
consumer and the active expression contract.

Expression flags are decimal bit flags:

| Bit | Name | Meaning |
|---:|---|---|
| 0 | `relocatable` | The result depends on a symbol or address that may change between passes. |
| 1 | `final` | The value is known-final for the current pass. |
| 2 | `structural` | The result carries a non-scalar structural expression shape. |

### Diagnostic Records

Diagnostics use the same native diagnostic code family already visible in the
CLI, with structured location fields added before the text payload.

```text
DIAG <diag-id> <severity-id> <code> <file-id> <line> <column> <text-len> <text>
DIAG-NOTE <diag-id> <note-index> <file-id> <line> <column> <text-len> <text>
```

Severity ids are:

| Id | Severity |
|---:|---|
| 1 | error |
| 2 | warning |
| 3 | note |

Existing native error lines such as `ERROR OPC-NCLI014: native include expansion
failed` remain compatibility output. A producing stage that has enough location
state should additionally emit `DIAG` so Rust-side decoders do not need to parse
human text.

### Emitter Boundary Records

The native CLI must make the end of the front-end pipeline explicit while Hunk
emission remains out of scope:

```text
EMIT-BOUNDARY <status-code> <statement-count> <expression-count> <diag-count>
STATUS emitter-boundary-ok
```

If the native emitter is still unavailable, the compatibility diagnostic remains
valid:

```text
ERROR OPC-NCLI009: native emitter VM not implemented
```

The presence of `EMIT-BOUNDARY` means the front end has produced all records it
can for the current slice. It does not mean bytes, relocations, hunks, listings,
or symbols have been emitted.

## Boundary Cases

- A report with no `OPFORGE-NATIVE 1` header is not a native pipeline report.
- A terminal diagnostic may stop the report before later stages appear.
- A skipped stage must emit either `STATUS <stage-name>-skipped <reason-code>`
  or a `DIAG` record naming the unsupported stage.
- Compatibility include records without full ids are accepted only as
  observations; later source-map consumers must require `SRC-*` or id-bearing
  include records before remapping diagnostics.
- Transitional `MODULE <name>` and `USE <name>` records are not module/import
  table records.
- Selected-item aliases from `.use module (item as alias)` must be preserved in
  `USE-SELECT` with `parser-alias-only` until a later module-import slice gives
  them semantic import behavior.
- A parser statement with no expression operands may emit `STMT-RESULT` and
  `STMT-MNEMONIC` without any `STMT-EXPR-REQ` records.
- An expression result may not appear unless a corresponding `STMT-EXPR-REQ`
  record introduced the `expr-id`, except in standalone expression VM tests that
  explicitly declare a synthetic statement id of `0`.
- Diagnostics without known source location must use `file-id = 0`, `line = 0`,
  and `column = 0` rather than inventing a location.
- Text payload length overflow is a deterministic diagnostic; payload text must
  not be silently truncated while the record claims the original length.
- Unknown record tags are ignored by forward-compatible decoders only after the
  version header is accepted; required consumers for a slice may still fail if a
  needed tag is absent.

## Acceptance Criteria

- [ ] The spec defines every stage name and status family needed before native
      `PRVM` CLI integration begins.
- [ ] The spec includes module/use table summary records for module ids, import
      module ids, import aliases, selected item names, selected item aliases,
      and parser-record-only selected-item aliases.
- [ ] The spec includes include/input-expansion records for parent file, child
      file, include depth, and logical source-location mapping.
- [ ] The spec defines bounded statement-result records suitable for the first
      no-expression `PRVM` smoke.
- [ ] The spec defines bounded expression-result records suitable for later
      `PRVM` to `EXVM` handoff and result reporting.
- [ ] The spec defines structured diagnostic records while preserving existing
      `OPC-NCLI` compatibility error text.
- [ ] The spec defines an emitter-boundary marker and explicitly keeps object
      generation out of this roadmap slice.
- [ ] The roadmap Work item 4 and Milestone 2 can be marked complete after the
      spec validator, roadmap validator, full quality gates, and compliance
      review pass.

## Validation Expectations

Minimum validation for this spec-only slice:

- `python3 scripts/workflow/check_spec_artifact.py documentation/opForge-native-vm-pipeline-report-v0_1.md`
- `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-portable-vm-68020-amigaos-roadmap-v0_1.md`
- `cargo fmt --all --check`
- `cargo clippy --all-targets --all-features -- -D warnings`
- `cargo audit --no-fetch`
- `cargo test --workspace`
- `cargo test --locked`
- plan-compliance review before commit

Additional validation for later implementation slices:

- focused native CLI tests that lock newly emitted record tags;
- reference regeneration only when report output intentionally changes;
- opt-in FS-UAE native CLI smoke when native runtime behavior changes;
- Rust-side decoder tests before any consumer depends on these records as data
  rather than text observations.

## Open Questions

- None for Work item 4. The record names and fields above are sufficient to
  unblock the first no-expression `PRVM` CLI integration slice. Later emitter or
  binary-report work may supersede this temporary front-end report with a new
  versioned contract.