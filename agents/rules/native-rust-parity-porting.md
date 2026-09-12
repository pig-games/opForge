# Native Rust-to-68000 Parity Porting Rule Pack

Load this rule pack when porting Rust VM or CLI behavior to native
68000/AmigaOS, fixing native behavior expected to match Rust, or adding native
parser, expression, selector, encoder, output, source, or session behavior.

Also load:

- `agents/rules/native-68000.md` when changing 68000 assembly
- `agents/rules/native-parity-failure-triage.md` when investigating a failure
- `agents/rules/native-68000-safe-instrumentation.md` before instrumenting
- `agents/rules/fs-uae.md` when running FS-UAE tests

## Reference boundaries and debugging

Identify the Rust reference and native boundary, their inputs, outputs and known
non-equivalences. Record only the detail needed to understand and test the change.
Preserve existing Rust/native semantic equivalence; representation and host-specific
implementation may differ. Investigate the first divergence through source loading,
parsing, state, selection, encoding and output as appropriate. This is a debugging
technique, not a restriction to one file or one boundary per change.

## Evidence limits

Existing harnesses use these labels:

| Level | Evidence |
|---|---|
| A | Rust semantic oracle |
| B | Rust-side package/native harness contract |
| C | Host-side native request-shape simulator |
| D | Real native execution through FS-UAE |
| E | Localization or debug probe |

Explain material limits of the evidence used. Host-side checks cannot replace
required real-native confirmation; probes cannot establish production parity.
Routine reporting does not need a separate form for every observation.

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
