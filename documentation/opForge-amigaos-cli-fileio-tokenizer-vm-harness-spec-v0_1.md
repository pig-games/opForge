# opForge AmigaOS CLI File I/O Tokenizer VM Harness Spec v0.1

## Summary

This specification defines a reusable AmigaOS CLI and File I/O support module
for the draft AmigaOS-native 68020-baseline tokenizer VM example, plus a
tokenizer-specific CLI harness contract that can produce readable, parsable
results under FS-UAE.

The intended outcome is an assembly-side support layer that future Hunk
executables can reuse for small "assemble, run in AmigaOS, capture output"
tests. This is a specification only; it does not implement the module.

## Problem

The current AmigaOS-native tokenizer VM draft describes an interpreter ABI, but
it does not yet define how an AmigaOS executable would:

- receive CLI arguments
- read an input file or source fragment
- write a stable test result file or stream
- report a process return code
- share AmigaDOS file operations with future assembly-fragment tests

Without that contract, a future Hunk output writer may be able to emit an
executable, but FS-UAE tests would still lack a reusable way to provide input
and collect deterministic output.

## Goals

- [ ] Define a reusable `amigaos_cli_fileio` `.module` contract for AmigaOS
  Shell-launched test executables.
- [ ] Define a tokenizer-specific CLI harness contract that can import
  `amigaos_cli_fileio` and call the 68000 tokenizer VM entry point.
- [ ] Define a stable text result format that host-side tests can parse after
  FS-UAE runs the executable.
- [ ] Keep the low-level File I/O module reusable for future assembly-fragment
  test executables, not only tokenizer VM tests.
- [ ] Define failure behavior and return-code mapping for CLI, file, VM, and
  output-format errors.
- [ ] Keep Workbench startup, requesters, buffered I/O, and interactive UI out
  of the first harness contract.
- [ ] Keep the first native AmigaOS tokenizer harness on a `68020` CPU
  baseline.
- [ ] Keep the active worktree `AGENTS.md` workflow and execution rules binding
  for any plan derived from this specification.

## Non-Goals

- [ ] Do not implement the AmigaOS support module in this specification.
- [ ] Do not implement Hunk output in this specification.
- [ ] Do not define a full C runtime, startup object, or Workbench tool ABI.
- [ ] Do not require FS-UAE for default workspace tests.
- [ ] Do not add a general AmigaOS SDK binding layer.
- [ ] Do not implement a full AmigaDOS `ReadArgs()` parser in the first
  tokenizer harness.
- [ ] Do not require the tokenizer VM to fully match the Rust tokenizer before
  the CLI/File I/O harness can exist.

## Invariants / Constraints

The low-level module must be reusable. It must not know about token records,
tokenizer VM bytecode, source lexing policy, or opForge test expectations.

The tokenizer harness must own tokenizer-specific concerns:

- choosing the VM program and buffers
- parsing command-line arguments into input and output paths
- invoking `tokvm_run_68000`
- rendering tokenizer status and token records into the result format

The low-level `amigaos_cli_fileio` module must own AmigaOS integration
concerns:

- opening and closing `dos.library` if needed by the final implementation
- calling AmigaDOS file operations through the platform calling convention
- exposing simple read, write, close, stdout/stderr, and error-code helpers
- preserving a documented register contract for callers

The first target is a CLI/Shell-launched executable. AmigaOS documentation
distinguishes Shell/CLI startup from Workbench startup; Workbench launch has a
different startup message flow and is out of scope for this first contract.

The first native AmigaOS tokenizer harness baseline CPU is `68020`. The
existing `tokvm_run_68000` symbol name remains the v0.1 ABI entry label, but
spec-derived native tokenizer harness code must target `.cpu 68020` unless a
later specification changes that baseline explicitly. Illustrative AmigaOS
examples outside that native tokenizer implementation slice are not required to
adopt the same baseline.

The result artifact must be plain ASCII text so host-side tests can parse it
without an Amiga-specific binary decoder.

Reference material used by this spec:

- AmigaOS Program Startup documentation, especially the distinction between
  CLI/Shell and Workbench startup:
  https://wiki.amigaos.net/wiki/Program_Startup
- AmigaOS Basic Input and Output Programming documentation for `Read()` and
  `Write()` behavior:
  https://wiki.amigaos.net/wiki/Basic_Input_and_Output_Programming
- AmigaOS 3 `dos.library` autodocs for `Close()` and `IoErr()` behavior:
  https://developer.amigaos3.net/autodocs/dos.library/Close.html
  https://developer.amigaos3.net/autodocs/dos.library/IoErr.html

## Behavioral Contract

The reusable module name should be:

- `amigaos_cli_fileio`

The tokenizer-specific harness module should be separate:

- `tokvm_amigaos_cli_harness`

The low-level module must expose callable routines with a stable assembly ABI.
The exact labels may change during implementation, but the module must cover
these capabilities:

- initialize AmigaOS CLI/File I/O support
- shut down any resources acquired by initialization
- return the inherited or opened standard output handle
- return the inherited or opened standard error handle when available
- open an existing input file for reading
- create or replace an output file for writing
- read a byte range from an open handle into a caller-provided buffer
- write a byte range from a caller-provided buffer to an open handle
- close a handle that this module opened
- return the most recent AmigaDOS I/O error value

The low-level module must document register preservation. The preferred
contract is:

- D0 returns primary status or byte count
- D1 may return secondary status, such as AmigaDOS error value
- A0/A1 are argument and scratch registers
- D2-D7/A2-A6 are preserved unless an individual routine documents otherwise

The tokenizer harness must support a minimal command form:

```text
tokvm <input-path> <output-path>
```

The first command parser may split on ASCII whitespace and may reject quoted or
escaped paths. If quoted paths are not supported, the harness must reject them
with a deterministic usage result instead of parsing them incorrectly.

The tokenizer harness must read the input file into a fixed caller-owned source
buffer. If the file is larger than the buffer, the harness must fail with a
deterministic "input too large" status rather than silently truncating.

The tokenizer harness must call `tokvm_run_68000` using the ABI already
documented in the draft tokenizer VM:

- A0 source line bytes
- D0 source line byte length
- A1 token output buffer
- D1 token output capacity in records
- A2 lexeme scratch buffer
- D2 lexeme scratch capacity in bytes
- A3 tokenizer VM bytecode program
- D3 tokenizer VM bytecode length in bytes

The tokenizer harness must write a stable text report. The v0.1 report format
is line-oriented:

```text
OPFORGE-TOKVM 1
STATUS <signed-decimal>
TOKENS <unsigned-decimal>
CURSOR <unsigned-decimal>
TOKEN <index> KIND <kind> START <col-start> END <col-end> LEN <len> LEXHEX <hex-bytes>
END
```

There must be one `TOKEN ...` line per emitted token. `LEXHEX` encodes the
lexeme bytes as uppercase hexadecimal without separators. This avoids escaping
rules for punctuation, tabs, quotes, and non-printable bytes.

The harness process return code must be deterministic:

- `0` for successful harness execution and tokenizer `STATUS 0`
- nonzero for usage, file I/O, buffer-limit, output-rendering, or VM failure

When the harness returns nonzero, it should still attempt to write a result
report if an output handle is available. Host-side tests should prefer the
report file when present and fall back to the process return code when absent.

FS-UAE integration must be a separate test-harness concern. The assembly module
must not know whether it is being run by FS-UAE, a real Amiga, or another
emulator. It only reads files, writes files, and returns a code.

## Boundary Cases

Missing arguments:

- `tokvm` without both input and output paths returns a usage failure and writes
  a usage message to stdout or stderr if a stream is available.

Unsupported quoting:

- if the first implementation does not support quoted paths, input such as
  `"RAM Disk:input.txt"` must fail with a deterministic usage error.

Input file open failure:

- the harness returns a file-open failure and records the AmigaDOS error value
  when available.

Output file open failure:

- the harness returns a file-open failure and writes to stdout or stderr when
  available.

Input too large:

- the harness must reject input larger than its configured source buffer.

Token output overflow:

- the harness must preserve the tokenizer VM status and include it in the
  report format if the output file is available.

Partial file write:

- if a write call reports fewer bytes than requested, the File I/O module must
  report failure rather than pretending the report was fully written.

Close failure:

- close failure must be reflected in the final harness status when it affects
  output reliability, but closing must not be retried blindly.

Workbench launch:

- Workbench startup is out of scope and may return a deterministic unsupported
  startup status if detected.

FS-UAE unavailable:

- default tests must not fail. FS-UAE execution checks must be opt-in or gated
  by an environment variable.

## Acceptance Criteria

- [ ] A spec-derived implementation can add `amigaos_cli_fileio` without
  modifying tokenizer VM interpreter logic.
- [ ] A spec-derived implementation can add `tokvm_amigaos_cli_harness` that
  imports the low-level File I/O module and calls `tokvm_run_68000`.
- [ ] A spec-derived implementation targets `.cpu 68020` as the baseline CPU
  for the first native AmigaOS tokenizer harness slice.
- [ ] The harness supports the command shape `tokvm <input-path> <output-path>`.
- [ ] The harness writes the v0.1 `OPFORGE-TOKVM 1` report format for one
  successful tokenizer VM run.
- [ ] The report format is parseable by a host test using line splitting and
  whitespace-separated fields.
- [ ] Lexeme bytes are represented with `LEXHEX` and do not require string
  escaping.
- [ ] Usage failure, input-open failure, output-open failure, input-too-large,
  VM failure, and partial-write failure produce deterministic nonzero return
  codes.
- [ ] The low-level File I/O module can be reused by a future non-tokenizer test
  executable without depending on tokenizer record layout.

## Validation Expectations

Spec validation:

- `python3 scripts/workflow/check_spec_artifact.py documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md`

Expected implementation validation derived from this spec:

- assemble the tokenizer VM draft with the new module imported
- unit or golden validation that the report renderer emits the exact
  `OPFORGE-TOKVM 1` format for a fixed token record buffer
- a negative validation for missing arguments
- a negative validation for input too large
- an opt-in FS-UAE test that mounts an input/output directory, runs the Hunk
  executable, and parses the generated report file

## Open Questions

- Should the first argument parser support quoted AmigaDOS paths, or should
  quoted paths stay out of scope until the first CLI harness works?
- Should the result target default to stdout when `<output-path>` is omitted,
  or should an explicit output path remain mandatory for deterministic FS-UAE
  file capture?
- What source-buffer, lexeme-buffer, and token-buffer sizes should the first
  executable reserve?
- Should the tokenizer harness process one file as one source buffer, or should
  it process line-by-line to better match the existing tokenizer VM line model?
- Should FS-UAE tests capture only a report file, or also capture stdout/stderr
  for diagnostics?
