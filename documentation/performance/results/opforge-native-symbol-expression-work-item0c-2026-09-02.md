# Native Symbol and Expression Work Item 0c Result — 2026-09-02

## Outcome

Item 0c adds an independently gated, observation-only `OFSE` companion to the
native progress bridge. The fixed 256-byte record correlates with `OFPR` and
keeps exact, scoped, imported, and final-component lookup classes separate. It
records calls, hits, misses, ambiguity, expression lifecycle, and phase/pass
identity. Optional detail mode adds actual label candidates, compared byte
positions, exact-hash probe distribution and maxima, maximum insertion-chain
depth, and expression-snapshot scan work.

The bridge does not add an index, cache, intern table, prepared expression, or
changed lookup path. It performs no clock calls, console output, or file I/O.
This result does not identify a hotspot or justify an optimization. Dynamic
B01-B10 slopes and ranking remain Items 2 and 0f work; the full native
self-host remains excluded as a profiling workload.

## Build measurements

All four builds used the same local Rust opForge executable and native
composition. Times are single sequential host wall-clock observations, not
native runtime benchmarks.

| Mode | Hunk bytes | Delta vs release | Delta vs Item 0b work-only | Host build wall time | SHA-256 |
|---|---:|---:|---:|---:|---|
| Release | 554,500 | 0 | -2,472 | 26s | `17a2b25571799adfb840439ade1304c3d442723a9eaef178158a3d5f1d64d8ab` |
| Item 0b work-only, same-tool archived source | 556,972 | +2,472 | 0 | 26s | `88265708e33ae7c2598996d724fe917edf7e584f33a6dc07bd8dd242b4902b19` |
| Item 0c aggregate | 558,064 | +3,564 | +1,092 | 27s | `d0d5bc7d3cdcd2b3df73a4142e0da5a009d141630eab44ee7c9f969b3906f9a8` |
| Item 0c detail | 558,432 | +3,932 | +1,460 | 27s | `ae423af0513ca7a48743aff2830d658b23d8c2979c336fcc6aceee50039d110f` |

The archived Item 0b source and current source produced byte-identical work-only
Hunks with the same current assembler. This same-tool isolation is authoritative
for Item 0c. Its 556,972-byte value differs by eight bytes from the earlier Item
0b ledger's separately measured invocation, so this ledger does not falsely
claim equality to that older number. Aggregate mode adds 1,092 Hunk bytes over
the same-tool Item 0b baseline; detail adds another 368. Data storage is 256 BSS
bytes in aggregate mode and 768 total BSS bytes in detail mode, including the
fixed 512-byte chain-depth scratch table.

The same bounded exact-artifact CLI fixture was run once in adjacent FS-UAE
modes after the final imported-callback coverage correction. Detail completed
in 46.22s and progress-only in 46.35s. Both runs used a fresh challenge, reached
guest completion, supplied explicit zero guest exit, and matched the live
in-memory Rust oracle byte-for-byte. The -0.13s/-0.28% difference is ordinary
single-run noise: it detects no meaningful perturbation but is neither a speed
claim nor a vintage-hardware estimate.

## Counter semantics

- Exact lookup candidates and compared bytes are actual hash-chain visits and
  byte positions.
- Scoped calls and qualified candidates describe the logical cascade. Nested
  exact comparisons remain in the exact class, avoiding double counting.
- Imported calls count callback requests, candidates, and outcomes. The opaque
  callback's private comparisons are not observable and imported bytes remain
  zero; later engine comparisons retain their exact or final class.
- Final-component scans count visited label rows and suffix byte positions and
  distinguish ambiguous results from hits and misses.
- Expression bind counts symbol-snapshot resolution. Snapshot candidates and
  bytes reflect that scan; request/parse/compile/evaluate/success/failure remain
  separately visible.
- Aggregate mode retains calls, outcomes, lifecycle, and phase identity while
  all candidates, byte counts, histograms, and maxima remain zero.
- Every group saturates with a defined overflow bit. Complete, incomplete, and
  active state must correlate with `OFPR` or the decoder rejects the record.

## Validation evidence

- Eight host decoder tests pass, including strict `OFSE` correlation and
  fail-closed malformed, incomplete, overflowing, and detail-mode cases.
- Level-B/C source and assembly tests cover all lookup classes, exact/scoped/
  imported/final outcomes, ambiguity, expression success/failure, phase/pass
  totals, detail fields, both terminal states, and all six writable saturation
  groups exercised by the harness.
- A fresh focused FS-UAE harness executed the real counter routines and exited
  zero after the deterministic oracle completed.
- Fresh detail-enabled and progress-only real CLI cases completed the guest
  protocol with zero exit and exact live Rust artifact equality.
- Same-tool archived-source comparison proves that the disabled Item 0c build
  is byte-identical to Item 0b work-only code. The release Hunk also retains its
  554,500-byte digest.

The focused sub-item gates and plan-compliance result are recorded in the plan
item completion evidence. The full repository/native closure gates remain
deferred to Phase 0 closure Item 0f. No counter observation in this slice ranks
a hotspot; that decision remains gated on the frozen bounded corpus and numeric
acceptance rules.
