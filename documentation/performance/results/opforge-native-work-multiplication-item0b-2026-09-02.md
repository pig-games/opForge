# Native Work Multiplication Item 0b Result — 2026-09-02

## Outcome

Item 0b adds an independently gated, observation-only `OFWM` companion to the
Item 0a progress bridge. The fixed 128-byte record correlates to `OFPR` by run
ID and counts pass-one, layout, and final-emission statement visits; layout
rounds and change reasons; flow rows, redirect direction, and maximum spans;
retained module/endmodule/use/generic classifications; and convergence/final
image-byte work. Every counter group saturates at `0xffffffff` and exposes a
defined overflow bit.

The companion performs no timer calls, console output, or file I/O. It emits no
code or storage in release or Item 0a progress-only builds. This result does
not claim a performance improvement or identify a hotspot. B01-B10 corpus
sampling and counter slopes belong to Items 2 and 0f. The full self-host remains
a terminal correctness/scalability proof and is not a profiling workload.

## Build measurements

All builds used the same local Rust opForge executable and native composition.
Times are single sequential host wall-clock observations and are not native
runtime benchmarks.

| Mode | Hunk bytes | Delta vs release | Delta vs progress-only | Host build wall time | SHA-256 |
|---|---:|---:|---:|---:|---|
| Item 0a cleaned release | 554,500 | 0 | -1,672 | 28.10s | `17a2b25571799adfb840439ade1304c3d442723a9eaef178158a3d5f1d64d8ab` |
| Item 0a progress-only | 556,172 | +1,672 | 0 | 28.23s | `db6945067c74104a4346649639059497cde7141d940cc75dc1e4c0834fff84bd` |
| Item 0b progress + work | 556,964 | +2,464 | +792 | 28.61s | `e14979a252214ba077ebaca0ded257352431dd16cbac19f2df6627b02b42d54e` |

The release and progress-only sizes and digests exactly match the Item 0a
ledger. The work group therefore adds 792 Hunk bytes over progress-only,
including its 128-byte `OFWM` BSS record. The single build observations do not
establish a meaningful build-time difference.

The same tiny exact-artifact fixture was then run once in three adjacent
FS-UAE modes. End-to-end wall times were 47.95s release, 48.32s progress-only,
and 48.57s progress plus work. Progress-only was +0.37s/+0.77% over release;
work was +0.25s/+0.52% over progress-only and +0.62s/+1.29% over release. Every
run completed the fresh guest protocol, supplied explicit zero guest exit, and
matched its live Rust oracle byte-for-byte.

These single observations include host assembly and emulator startup. They are
a bounded perturbation check only, not a vintage-hardware overhead estimate and
not evidence about the multi-hour workload. The B01-B10 profile will separately
record disabled/progress/work measurements before any optimization is selected.

## Counter semantics

- Statement classifications are counted per visit, so their totals expose
  pass multiplication rather than unique source rows.
- Flow rows count every callback-selected next row. Forward/backward redirect
  totals count only actual redirects; maximum spans are statement-index deltas.
- Label-value and placement/extent changes are separate layout-change reasons.
- Convergence image bytes accumulate completed non-final layout passes; final
  image bytes accumulate completed final emissions.
- Complete, incomplete, and active states mirror the correlated `OFPR`
  lifecycle. A complete record with nonzero exit, or any uncorrelated record,
  is rejected as proof.

## Validation evidence

- Host decoder: five tests pass, including full field decoding and fail-closed
  rejection of malformed, contradictory, unknown, incomplete, and
  uncorrelated records.
- Native source/harness: Level-B/C contracts and focused assembly pass. The
  deterministic oracle covers every field and drives all five counter groups
  through saturation.
- Focused guest counter contract: a fresh FS-UAE run completed with explicit
  guest exit zero after deterministic pass, flow, layout, classification,
  image-byte, terminal-state, and overflow checks.
- Real CLI parity: fresh release, progress-only, and work-enabled runs each
  completed with explicit guest exit zero and exact equality to their live
  Rust oracle.
- Release isolation: release and progress-only Hunk digests remain exactly the
  Item 0a values, so the independently gated work group perturbs neither mode.
- Full regression: the final single-thread canonical Rust gate passed all
  1,567 assembly-library tests, remaining workspace tests, the serial LSP
  integration suite, and 55 documentation tests.

## Failed-run triage retained as non-proof

The first work-counter guest harness attempt exited 33 because the harness
reused A1 for the event buffer and later treated it as the work-record pointer.
The harness now reacquires the public `OFWM` pointer before terminal checks; a
fresh run passed. The failed run proves nothing about the implementation.

The first real CLI work-enabled attempt reached pass two but returned the image
byte count as the engine status. Level-E subgroup runs localized the change to
the passive pass-end site. That site now restores the already-proven zero
success value after recording the byte count, and a structural contract locks
the invariant. Fresh focused and whole-CLI Level-D runs then passed. The failed
run and Level-E localization probes are not parity proof.

The first full Rust quality-gate run reported three deterministic failures. The
shared harness had not guarded its new `OFWM` oracle in Item 0a progress-only
mode; the guard was added and the focused harness build passed. The native
product inventory also correctly detected the new source/export surface and
was updated to the measured 1,626,750 loadable source bytes, 6,571 public
declarations, and 128,224 packed declaration-name bytes. Those remain below the
2,097,152-byte, 8,192-declaration, and 262,144-name-byte capacities. Focused
reruns passed; the failed gate is not completion evidence.

A normal-concurrency full-gate retry then passed the complete 1,567-test
assembly library but two unrelated LSP integration cases timed out under load.
Both passed immediately in focused serial reruns. The final canonical gate was
therefore run with `RUST_TEST_THREADS=1` and passed in full; the parallel retry
is retained only as flake triage and is not the completion receipt.

No counter result from this slice ranks a hotspot. Item 0f must use the frozen
bounded corpus and numeric acceptance rules before the plan may choose or
reorder an optimization.
