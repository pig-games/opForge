# Frozen corpus v1: native diagnostic-entry status

This is a historical status ledger, not reusable Level D evidence or a native
timing baseline. It supports the user-approved Item 2 diagnostic entry exception.
The actual source, public command, and package still supply each new run's live
Rust oracle. No row in this file may select an oracle or satisfy a proof gate.

- Inputs: `opforge-corpus-v1-manifest.json`, SHA-256
  `fece2121b487b37e1217b4854b74308366399938e26520e06d124ed63559aed9`.
- Package: explicit 368,278-byte package, SHA-256
  `46a56a5bd436b012c596c65d1f7d85fe6cd8fadbd702362955804415e00c0d41`.
- Source provenance: current generator inputs; the earlier nested-include
  B08/B10 candidates are excluded from this frozen revision.
- Observation provenance: interactive test output from 2026-09-04. Guest case
  trees were removed by the fail-closed runner. Raw guest case files are not
  retained or reconstructed. Native executable digests were not retained by
  these attempts; no binary-level performance comparison is supported.
- Environment: FS-UAE 3.1.66 on the local macOS host, ordinary explicit-package
  native CLI, counters off, actual CPU 68020. B01 used the initial template
  without explicit CPU speed. Later cases used `uae_cpu_speed=max`, JIT off,
  inherited A4000 boot configuration and runner-supplied 64 MiB Zorro III RAM.
  The host deadline was 120,000 ms after guest start; it is not elapsed assembly
  time. No physical A6000 run or full native self-assembly is represented.

| Case | Current diagnostic-entry status | Last relevant observation | Remaining proof |
|---|---|---|---|
| B01 | incomplete validation | Initial guest completed with zero exit and nine exact `EA` bytes | Repeat with pinned speed and current strict empty-stream adapter |
| B02 | incomplete execution | Post-start timeout; no completion, partial streams empty | Actual completion, zero exit, artifacts and diagnostics |
| B03 | incomplete execution | Post-start timeout; no completion, partial streams empty | Actual completion, zero exit, artifacts and diagnostics |
| B04 | incomplete validation | Initial guest completed with zero exit and exact BIN | Repeat with current strict empty-stream adapter |
| B05 | incomplete execution | Post-start timeout; no completion, partial streams empty | Actual completion, zero exit, artifacts and diagnostics |
| B06 | incomplete validation | Initial guest completed with zero exit and exact BIN | Repeat with current strict empty-stream adapter |
| B07 | incomplete validation | Initial guest completed with zero exit and exact BIN | Repeat with current strict empty-stream adapter |
| B08 | failed diagnostics | Corrected sibling-include guest completed with zero exit and exact BIN, but stdout was nonempty | Capture unexpected text, resolve divergence, fresh full proof |
| B09 | failed assembly | Fresh completion with exit 1; `OPC-NCLI022` unresolved native label followed by `OPC-NCLI020` | Localize divergence; fresh exact eleven-artifact proof |
| B10 | incomplete execution | Corrected 256-group sibling-include guest timed out; partial streams empty | Actual completion, zero exit, all three artifacts and diagnostics |

Every case remains ineligible for a successful native timing comparison under
the current strict contract. The four earlier byte-equality receipts remain
bounded supporting observations, not a claim that the final adapter has passed.
The failing mechanisms have not been removed from the frozen sources. Initial
eight-case and corrected two-case cohorts attempted later cases after failures;
their 1,179.10 s and 299.56 s total host durations include compilation, boot and
teardown and are deliberately not entered as assembly timings.

Next: Item 0f may collect bounded, fresh-identity Level E counter snapshots or
symbolized PC samples for these exact inputs, explicitly marking aborted or
timed-out assemblies incomplete. Fresh completed cases still require the full
Level D contract. No parity finding, optimization threshold, Phase 0 closure,
or terminal self-host gate is satisfied by this ledger.
