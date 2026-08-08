# Native Rust-to-68000 Parity Porting Rule Pack

Load this rule pack when porting Rust VM or CLI behavior to native
68000/AmigaOS, fixing native behavior expected to match Rust, or adding native
parser, expression, selector, encoder, output, source, or session behavior.

Also load:

- `agents/rules/native-68000.md` when changing 68000 assembly
- `agents/rules/native-parity-failure-triage.md` when investigating a failure
- `agents/rules/native-68000-safe-instrumentation.md` before instrumenting
- `agents/rules/fs-uae.md` when running FS-UAE tests

## Boundary contract

Before editing production code, record:

```text
Slice name:
Rust reference files/functions:
Native target files/functions:
Boundary type:
Contract:
Expected native inputs:
Expected native outputs:
Known non-equivalences:
Proof-level tests:
Fast proof:
FS-UAE proof:
```

Boundary types include source reader, tokenizer, parser, statement store,
expression request, EXVM result, selector, encoder, and output.

Do not invent native behavior when Rust reference behavior exists. Native
divergence is limited to memory layout, calling convention, register pressure,
fixed-buffer constraints, AmigaOS host I/O, and 68000 control-flow
representation. Document the divergence and preserve Rust semantics.

## Find the first divergence

Compare boundaries in this order:

1. Source line read
2. Tokenization
3. Parser or portable AST/statement shape
4. Native statement/session record
5. Expression request envelope
6. EXVM/EXPR parse/evaluation result
7. Selector candidate
8. Encoder output
9. Session image bytes
10. Output artifact

Patch only the first divergent boundary. One slice corrects one named invariant.

## Evidence levels

Classify every test or observation:

| Level | Evidence |
|---|---|
| A | Pure Rust semantic oracle |
| B | Rust-side package/native harness contract |
| C | Host-side native request-shape simulator |
| D | Real native 68000/AmigaOS execution through FS-UAE |
| E | Temporary localization or debug probe |

Every test summary must state:

```text
This test proves:
This test does not prove:
```

Levels A-C can provide fast boundary proof but cannot replace a required Level D
confirmation. Level E never proves production behavior.

## Singular Level D parity proof contract

There is one authoritative rule for using FS-UAE to prove native parity. The
actual test case is the CPU, exact source bytes, command surface, package bytes,
and the Rust oracle held directly by that case in memory. A stored evidence file,
display name, manifest alias, or output filename must never select or resolve the
Rust oracle.

A positive Level D parity result exists only when all of these are true in the
same run:

1. The host removed all prior capture and output files before launch.
2. The guest returned the exact start and done messages for a fresh per-run challenge
   bound to a fingerprint of the actual test case.
3. The guest wrote an explicit exit code of exactly zero.
4. The expected output exists and is byte-for-byte equal to the Rust oracle
   carried by that test case.
5. Every on-disk case input, output, marker, log, and derived evidence artifact is
   removed before the runner returns, whether the run passes, fails, times out,
   crashes, or unwinds.
6. A failed case must not prevent later cases from executing. The serial test
   coordinator recovers from a poisoned lock, and a case counts only when that
   case itself reaches the emulator proof contract.

There is no fallback success condition and no optional confirmation. Launcher
success, marker existence without exact contents, partial output, a previous
green record, a diagnostic probe, or a caller-side comparison cannot promote a
run to Level D parity. Negative cases use the same fresh completion protocol and
must additionally return a nonzero guest exit with the required diagnostic.
Outside byte parity, launcher success never substitutes for guest completion and
an explicit guest exit, and the same ephemeral artifact cleanup remains mandatory.
No test result is valid unless its fresh guest protocol completed and supplied an
explicit exit code, including tests that expect failure.
