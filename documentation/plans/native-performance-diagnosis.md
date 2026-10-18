# Native runtime and memory diagnosis

Status, 2026-10-18: bounded prepared-record storage reduces the disk-backed frozen
full-build peak from **10,489,408 to 8,525,376 B (−18.72%)**. Module ranges are read
through bounded views during global finalization and reachability; the contiguous
assembly stream is reconstructed only after frontend scratch retires. Resident
capture remains the default/reference storage policy. Parity remains paused.
This is in-build storage ownership, not persistent `.ofm` objects.
See [ownership](native-lazy-preparation.md) and the
[implemented object boundary](native-fragment-preparation.md).

## Measurement contract

Compare engine changes on identical source/package bytes and commands against a
newly assembled live Rust oracle. Require fresh challenge-bound START/DONE, exit
zero and byte-for-byte complete output. A timeout or launcher success is no proof.
Tracked heap and exact work are measured separately from uninstrumented runtime.
Two-pair results below about 2% / two seconds do not establish a speed change.

The expanded-memory A4000/68040 FS-UAE profile and 64 KiB guest stack run serially.
These are not A6000 measurements or 2 MiB qualification. Heap excludes executable,
stack, OS and untracked allocations; linked reservation is reported separately.
No observer framework or experiment selector was added.

## Bounded prepared-record storage

Expected saving was at least 1 MiB, with a planning estimate of 1–1.5 MiB: avoid
retaining a 2 MiB record allocation alongside frontend scratch. Preserve the
`c76e53f2` executable as the reference. `--capture-dir DIR` now controls both raw
capture and prepared-record backing; use a real disk directory, not RAM-backed `T:`.

A module owns checked dense record ranges in the shared store. Four 32 KiB mutable
payload pages cap residency at 128 KiB; the directory is outside that cap. Records
never cross pages. Leases prevent eviction of borrowed pointers, dirty pages persist
before eviction, and dependency-ordered views cap reads at module boundaries.
Global identities remain a build-local bridge. There are no process pointers in
stored record payloads. Resident mode keeps the contiguous reference path.

Layout, outputs, canonical binding and record remapping keep their existing phase
order across page batches. Reachability retains whole-block decisions in an independent
index. Runtime bindings, typed values, parameters and member identities transfer
before lexical/template scratch retires. Only then is one contiguous ordered stream
rebuilt and omission applied. The assembly engine's no-growth frontier is unchanged.
The superseded disk-assisted growth helper and its probes were removed.

Frozen workload: **214 mapped files / 2,648,242 staged bytes**, SHA-256
`eae5102bd21b3863105a7f550912193dff8e52c8ba2d6ff11b498392b7dca480`.
Every full comparison emits the live Rust oracle's exact **708,216 bytes**,
`fnv1a64:27c800763d556961`.

| Resource | Reference | Final paged storage |
|---|---:|---:|
| Tracked heap peak | 10,489,408 B | 8,525,376 B |
| Allocated/freed capacity, each | 89,224,992 B | 126,781,224 B |
| Prepared live heap | 2,969,088 B | 2,969,088 B |
| Assembly live heap | 4,943,360 B | 4,943,360 B |
| Cleanup live heap | 0 B | 0 B |
| Expression compilations | 69,528 | 69,528 |
| Expression evaluations | 176,850 | 176,850 |
| Source bytes read | 2,816,781 | 2,816,781 |

The **1,964,032 B / 1.873 MiB** peak saving matches replacing the 2 MiB buffer with
128 KiB payload pages plus a 2 KiB directory. It clears the resource gate. Allocation
traffic rises **42.09%** because faulted pages are allocated and freed; lower peak
residency is not lower allocation work. The candidate reproduced its peak before and
after removal of the obsolete growth helper. Reference memory is the preserved
qualified `c76e53f2` receipt on this exact frozen input. Diagnostics are empty and
all capacities balance; no language work was skipped.

Uninstrumented frozen-input measurements are separate. The first two pairs precede
the final dead-helper cleanup; the last row measures the final executable.

| Release engine | Run A | Run B | Mean |
|---|---:|---:|---:|
| Reference `c76e53f2` | 105.032 s | 104.295 s | 104.664 s |
| Paged storage before cleanup | 104.632 s | 103.283 s | 103.957 s |
| Final paged storage | 102.934 s | 102.983 s | 102.959 s |

Final observed difference: **-1.63% / -1.705 s**.
This is inside timing noise; no release speedup or slowdown is established.
The heap saving is the reason to retain the change. Final release image/linked
reservation is **719,128 / 753,576 B**, versus 715,132 / 749,504 B; the instrumented
image/linked reservation is **733,756 / 769,968 B**.

The maintained disk-backed controls compare source-directed BIN and complete Hunk
outputs against live Rust, independently of self-host workload identity:

| Resource | Small modules reference → final | Macro-heavy reference → final |
|---|---:|---:|
| Tracked heap peak | 608,768 → 608,768 B | 2,098,920 → 2,066,408 B |
| Allocated/freed capacity, each | 1,074,656 → 1,106,920 B | 4,522,712 → 4,523,232 B |
| Instrumented seconds | not resolved → not resolved | 6.073 s → 6.078 s |

The short instrumented times are attribution samples, not release-speed claims.
The module case completes before the host poll resolves a START/DONE interval;
its timing is explicitly unavailable, not zero.
Cleanup is zero and diagnostics are empty. Expression/evaluation counts and source
reads agree with the reference. A small workload need not save heap: page granularity
has a cost, while the large-buffer overlap disappears only on sufficiently large inputs.

## Current qualification and distance to target

The **complete current CLI self-build**, distinct from the frozen comparison,
passes on real FS-UAE execution: **220 staged files / 2,743,248 source/package bytes**,
source manifest `fnv1a64:6734ba67110e7e54`. Native produces the exact **719,128-byte**
Rust/bootstrap executable, `fnv1a64:44c8104a1488ead0`, with fresh protocol completion,
exit zero and no scratch files left. It takes **109.959 s** uninstrumented with the
embedded m68020 package and `--capture-dir Work:`. This changing-source time is not
an optimization comparison or hardware timing.

The disk and resident 14-case module matrices cover configuration, active includes,
templates, diamonds/multiple modules per file, late/reordered maps, forward layout,
logical geometry and invalid parameter sets. The final resident matrix also verifies
normal allocation after removing the old wrapper. The prepared-store component
covers eviction/writeback, pinned-budget rejection, unchanged ownership on failure,
reordered views, empty modules, page/module limits, EOF and repeated cleanup. A block
probe covers deferred omission, exact size validation and opaque layout records.
Host builds/oracles, formatting, CPU architecture boundaries and changed-subsystem
CCR checks pass; advisory existing call-return checks remain. Native no-growth,
instrumentation safety and fresh-proof guards also pass. No broad Rust workspace
gate or A6000 run was made for this slice.

**No 2 MiB fit is claimed.** Frozen disk peak alone is **8.13 MiB / 4.07 times**
the entire target, exceeding it by 6.13 MiB. Adding instrumented linked reservation
reaches at least **8.86 MiB**, before stack/OS/untracked costs. Prepared/assembly live
heap is still 2.83 / 4.71 MiB. These are frozen-input figures, not a new memory
measurement of the larger current-source self-build. Resident peak was last qualified
at 18,175,240 B in the preceding publication slice; it was not remeasured here.

## Maintained decisions and next boundary

- Resident discovery publishes dependency-complete modules and retires completed raw
  captures. Disk capture retains complete-graph scheduling to avoid overlapping growing
  configuration/semantic owners. Both use shared replay and global finalization.
- Raw capture uses 32 KiB shared chunks, dedicated large records and offset handles;
  disk payload is capped at 1 MiB. Finished configuration/discovery retire after seeding.
- Lexical names occupy 64 KiB pages. Symbol buckets scale from 256 up to 16,384;
  the qualified exact-work comparison reduced chain visits 89.7% and compared byte
  pairs 14.3%, at 28,720 extra peak bytes. Fixed 4,096 and stored hashes were removed.
- Telemetry implementation bodies are generically guarded, with no-op signatures for
  release. No test, filename or benchmark identity selects production behavior.
- Identity layouts reuse resident records; disk layouts publish ordering/origin metadata
  without copying payload until the post-frontend materialization boundary.
- Macro-only deferral/token/fragment experiments, raw-include reuse and prepared leaf
  caches were removed: no demonstrated whole-peak/runtime benefit justified retention.

The next object boundary is module-local identity and ownership of definitions,
types, templates and unresolved binding/layout plans. They remain global today.
Prepared storage is now bounded in disk mode, but final assembly still needs the
contiguous record stream. A `.ofm` object must use local IDs, checked offsets and
relocatable fragments/recipes before that stream and module metadata can retire.
Do not return to macro-only audits or treat a final-only cache as an earlier peak saving.

The [bounded module-capacity calculation](native-fragment-preparation.md#capacity-budget-before-module-sealing)
identifies 4.13 MiB of lexical allocation at a frontier within 42,096 B of the
measured peak. After retained interfaces, an identity bridge, the largest active
private module, sealing overlap and a workspace allowance, its planning budget is
1.5–1.9 MiB less at that frontier. This is conditional on closing private bindings
and sequential module retirement; it is not a measured new full-build peak. The
next implementation must demonstrate at least 1 MiB peak saving on the unchanged
input. Sealing/retirement is not implemented by this calculation.

## Evidence and reproduction

Immutable reference/candidate images, frozen cases, current-source export, raw receipts
and decoded results live at `/tmp/opforge-prepared-objects-20261018`. The reference
memory receipt remains at `/tmp/opforge-module-pipeline-20261017/policy-disk`.
These local diagnostic artifacts are outside `target`, not repository dependencies.
Git preserves superseded code and detailed trials.

Use the [FS-UAE environment](../../agents/rules/fs-uae.md) and ignored
`native_full_selfhost_comparison_fs_uae` with `OPFORGE_FULL_COMPARISON_CASE` naming
a fresh [comparison case](../../scripts/performance/run_fs_uae_selfhost_comparison.py).
`capture_mode: "disk"` adds `--capture-dir Work:`; resident/unset retains RAM storage.
Both execute real native code against a freshly built Rust oracle.

Use `native_preparation_benchmark_fs_uae` with `OPFORGE_PREPARATION_IMAGE`,
`OPFORGE_PREPARATION_WORKLOAD=modules|macros`, a fresh
`OPFORGE_PREPARATION_RESULT_DIR` and `OPFORGE_PREPARATION_CAPTURE_MODE=disk`.
[Maintained workloads](../../benchmarks/native-preparation/README.md) include a
four-module project and a four-module, 1,000-call macro workload; opForge465 contains
no macros and is not mislabeled as the macro-heavy case.

`native_prepared_store_probe_save_image` / `OPFORGE_PREPARED_STORE_SAVE_DIR` exports
the component image outside `target`. Existing MEMD/capture events supply separate
heap/work measurements. Fresh expected artifacts never substitute for completion.
