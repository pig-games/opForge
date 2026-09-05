# Item 0f: common-boundary observer controls

Level E controlled-abort diagnostics, not complete assembly or Level D parity.
Active `AGENTS.md` and the performance plan remain binding. This is a bounded
continuation inside Item 0f, not an optimization item or its closure report.

## Boundary and comparison contract

Use the full frozen B03 case (257 lines, 3,083 bytes), with the existing
one-statement-visit abort. That boundary follows the complete frontend and
allows a same-work comparison within the unchanged 120-second coordinator
ceiling. B03 is its own frozen case, not a reduced fixture claiming B10 semantic
coverage. It does not exercise B10's module scan or later backend passes.

Run sequentially in `all / all-no-io / all-no-io / all` order. Each uses
`production_corpus.py diagnose --case B03 --abort-visits 1 --control-mode console`
with its explicit `--diagnostic-profile`. Console support is enabled but never
entered: no pause, resume, register or memory commands. The sampler observes
the fresh START and exact derived DONE plus explicit guest exit before cleanup,
polling every 100 milliseconds within its 115-second control window.

The emulator is FS-UAE 3.1.66, 68020/max-speed/JIT-disabled, using the same
configuration as the corrected B10 pair. Configurations may differ only in the
ephemeral Work mount. Neither hardware MIPS nor native tick-to-wall-time
calibration is inferred. Native observer binaries remain the hashes recorded
in the I/O-counter calibration report; only the I/O counter switch differs.

`scripts/performance/compare_native_controls.py` independently decodes all five
raw records and rejects disagreement with stored profiles. It requires frozen
case/corpus/package identity, exact prelaunch source inventories, unique valid
guest challenges, completed controlled aborts, explicit matching exits, no
overflow, no debugger commands, bounded finite elapsed observations and completed
cleanup. It also checks common guest command, emulator version/configuration,
driver/sampler hashes, define sets differing only in the I/O kill switch and
stable native binary/symbol identity within each mode.

All decoded non-I/O work must match, including all bulk-memory fields, not just
a selected VM count. Only run identity and clocks are removed. Disabled I/O
fields are unavailable, not evidence of zero I/O. Raw receipts remain diagnostic
and `comparison_eligible=false`; the derived comparison is explicitly
`controlled-abort-only`, never a promotion into the successful corpus ledger.

## Results

Raw receipts, in execution order:

1. `opforge-b03-boundary-all-r1-2026-09-04.json`
2. `opforge-b03-boundary-noio-r1-2026-09-04.json`
3. `opforge-b03-boundary-noio-r2-2026-09-04.json`
4. `opforge-b03-boundary-all-r2-2026-09-04.json`

The derived `opforge-b03-boundary-comparison-2026-09-04.json` includes each raw
receipt's SHA-256 and the complete shared-work projection. Its shared-work
SHA-256 is `0e330ed5fd40861bef349cf67769d18505cf4ee9733a6eab8a0a76348b268546`.

| Mode | Run 1 seconds | Run 2 seconds | Mean seconds | Frontend ticks |
|---|---:|---:|---:|---|
| all | 76.533833 | 76.598390 | 76.566111 | 332 / 333 |
| all-no-io | 76.397049 | 76.329328 | 76.363189 | 320 / 320 |

The mean START-to-DONE difference is **0.202923 seconds**, with both no-I/O
observations below both all-counter observations. This is a small consistent
direction in this four-run experiment, not a precise overhead estimate. The
within-mode ranges are about 65 and 68 milliseconds, below the 100-millisecond
host polling interval. Raw guest phase ticks are recorded separately and are
not converted into wall time or physical-machine performance.

Every run reports the same 257 built statements, one statement visit, 257
tokenizer invocations / 12,830 opcodes, 516 parser invocations / 8,742 opcodes,
524 clears / 43,250,219 completed bytes, and 1,803 copies / 125,915 completed
bytes. All remaining shared decoded work agrees too. The all-counter runs each
report 3,084 source reads returning 3,083 bytes, 257 logical lines and zero
module candidates. All five records are correlated, incomplete and nonoverflowing.

All four guests complete their fresh diagnostic protocols with explicit exit 1
and `OPC-NCLI020` at the requested abort boundary; each diagnostic host test
exits zero and reports `capture_ok=true`. This is expected controlled-abort
capture success, not successful assembly. `complete`, `parity_passed` and
`comparison_eligible` remain false. The four ephemeral guest trees were removed.

## Measurement decision

Use `all-no-io` provisionally for subsequent compute/bulk attribution, and retain
separate `all` runs when structural I/O counts are needed. It preserves the
shared B03 work and modestly reduces observed elapsed time here. Together with
the corrected B10 progress pair, this supports avoiding per-read counting in
the next compute probe; it does not establish an overhead correction factor.
Do not subtract this B03 difference from B10 timings or extrapolate its size.

The next bounded implementation slice is repeated full-B10 low-I/O-observer
sampling with the existing 60/100-second same-guest capture, preserving exact
source inventory and all other counter groups. That is still attribution, not
a buffered-reader, startup-clear or VM optimization. Backend attribution,
counter-free calibration and Item 0f's ranked report remain unfinished.

Follow-up outcome: `opforge-native-b10-repeatability-2026-09-04.md` records one
additional usable B10 pair, one early-only capture and a retry with no frame.
The repeat check remains incomplete; debugger-entry acknowledgement must be
localized before further identical probes. No missing snapshot is native-stall
evidence.

## Interpretation limits

Two observations per mode are a small repeat check, not a statistically robust
overhead distribution. START-to-DONE includes executable loading, initialization,
record export and abort handling. Those fixed costs dilute a frontend-local
difference. A difference near the 100-millisecond polling interval does not
establish a precise observer percentage. Mode-dependent code layout and host
variance remain confounders. No counter-free runtime, full B03/B10 runtime,
backend time share, physical-A6000 performance or product speedup follows.

## Validation and status

- Level A/B: nine focused synthetic comparison/rejection tests pass. They cover
  frozen outer identities, challenge grammar/uniqueness, raw-profile disagreement,
  shared-work changes, harness/guest-command/emulator identity, observer defines,
  invalid timing/exit/inventory and native/configuration mismatches.
- `make workflow-gate` passes all 125 host tests and the native formatter check
  (238 files, none changed). The intentional negative-fixture architecture error
  printed within the unit suite is expected; the suite and real guard pass.
  Staged-only checks see an empty index, not a prepared commit.
- The plan bundle with its pending-gate allowance and whitespace checks pass.
- Reviewer `platform_coverage_review` (Hume) independently verifies all four raw
  receipts and the derived comparison: interim PASS for this diagnostic slice.
- Item 0f, the full Rust gate and the complete native FS-UAE wrapper remain open.
  This slice changes only host comparison tooling, tests and evidence/plan text;
  no native algorithm code, staged files, commit or push.
