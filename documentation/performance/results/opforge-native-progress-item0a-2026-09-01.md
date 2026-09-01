# Native Progress Item 0a Result — 2026-09-01

## Outcome

Item 0a adds an observation-only native progress bridge. The bridge emits no
code or storage in an ordinary release build. Debug-contract builds expose a
fixed 128-byte record with explicit active/complete/incomplete state, coarse
phase ticks, pass/layout position, current and completed statement, and
statement visits. Heartbeat and diagnostic abort remain off by default.

The fetched checkpoint did not pass its mandatory native redundant-test guard:
14 already-redundant `tst` instructions followed flag-setting instructions.
The guard's repository fixer removed those no-op tests as a separate preflight
repair. That makes the cleaned release Hunk 28 bytes smaller than the untouched
checkpoint; the progress bridge's footprint is therefore measured against the
cleaned release, not hidden inside that repair.

This result does not claim a performance improvement or revive the parked
self-host proof. An incomplete record is localization evidence only.

## Build measurements

All four builds used the same local Rust opForge executable and exact native
module/include paths. The three Item 0a builds were run sequentially without a
concurrent quality gate. Times are single wall-clock observations from the host
runner and are not native runtime benchmarks.

| Mode | Hunk bytes | Delta vs checkpoint | Delta vs cleaned release | Host build wall time | SHA-256 |
|---|---:|---:|---:|---:|---|
| untouched `68cc693c` | 554,528 | 0 | — | 26.2s | `54596516cc6edebbc8c55aa97075ced84567460a9c151ebfda315b8090d0d26b` |
| Item 0a cleaned release | 554,500 | -28 | 0 | 27.49s | `17a2b25571799adfb840439ade1304c3d442723a9eaef178158a3d5f1d64d8ab` |
| counters only | 556,172 | +1,644 | +1,672 | 28.16s | `db6945067c74104a4346649639059497cde7141d940cc75dc1e4c0834fff84bd` |
| counters + 4096 heartbeat | 556,188 | +1,660 | +1,688 | 28.31s | `c13f4ce21ebc1f7de76a38544a10684677a56b06f47c83220bac07444243f931` |

The heartbeat setup costs 16 Hunk bytes beyond counters-only. The record is 128
bytes and its two private tick words add 8 bytes of BSS. Single host-build times
span 27.49–28.31s and do not establish a meaningful build-time difference.

One fresh FS-UAE exact-artifact fixture was run in release and counters-only
modes. End-to-end test wall times were 48.46s and 48.55s respectively: +0.09s,
or +0.19%. Both runs completed the fresh guest protocol, supplied explicit zero
guest exit, and matched the same live Rust oracle byte-for-byte. This single
observation includes Hunk assembly and emulator startup and is only a bounded
perturbation check, not a vintage-hardware runtime estimate.

Native runtime overhead on the multi-hour workload is intentionally not
claimed from emulator startup timing. Counters-only performs one bounded
fixed-record routine call per statement-loop visit without a timer call or I/O;
that routine updates several fixed fields. The optional heartbeat adds a
`DateStamp` sample and one bounded event only at its visit quantum. It must stay
off for the first counters-only profile; a disabled/counters/heartbeat runtime
comparison is part of Item 0f before optimization selection.

## Validation evidence

- Host record decoder: three Level-C test groups pass, including fail-closed rejection of
  contradictory, stateless, nonzero-complete, active, incomplete, unknown-flag,
  and unknown-overflow records.
- Native source/harness: Level-B/C source contract and focused assembly pass,
  including locked visit/phase-tick saturation branches.
- Focused guest record contract: fresh FS-UAE protocol completed with explicit
  guest exit zero after deterministic phase, tick, heartbeat, visit-limit,
  visit saturation, phase-tick saturation, overflow-bit, and incomplete-terminal
  checks.
- Real CLI parity: a separate fresh counters-enabled CLI run completed with
  explicit guest exit zero and produced bytes exactly equal to its live Rust
  oracle. Heartbeat and diagnostic abort were disabled.
- Release isolation: the progress module and call sites emit no release bytes.
  The measured 28-byte checkpoint delta is exactly the separately identified
  14-instruction mandatory redundant-test cleanup.

The first focused guest attempt exposed arithmetic absolute BSS addressing that
did not locate the record in the loaded composition. The implementation was
changed to an explicit pointer ABI and base-relative accesses; the subsequent
fresh run passed. No failed run is counted as proof.
