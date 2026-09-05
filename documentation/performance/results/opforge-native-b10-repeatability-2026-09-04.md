# Item 0f: repeated B10 compute snapshots

Level E localization evidence only. Active `AGENTS.md` and the performance plan
remain binding. This report stays inside Item 0f: no native optimization,
completed-assembly timing, parity result or high-level closure is claimed.

## Experiment and audit

Request two repeats of the corrected full frozen B10 case, using the existing
provisional `all-no-io` observer mode and the same 60/100-second snapshot
requests. Compare with the earlier exact-input no-I/O receipt. One repeat lacks
its later debugger frame, so retain it and make one unchanged bounded retry.
These are observations of one mode, not a new randomized all/no-I/O timing
experiment. The baseline was collected earlier; the new runs execute serially
without other emulator runs or heavyweight host gates during measurement.

The invocation uses `production_corpus.py diagnose --case B10 --abort-visits 1
--diagnostic-profile all-no-io --sample-after-seconds 60
--resample-after-seconds 100`, with explicit
`OPFORGE_FS_UAE_CONSOLE_DEBUGGER_AUTOMATE=1` and the existing
`/tmp/opforge-performance-68020-max-20260904.fs-uae` template. The coordinator
ceiling remains 120 seconds after fresh START. No source reduction, timeout
extension, native instrumentation or capture-code modification is involved.

Fixed identities:

- Case SHA-256: `0e8c45ef53ca91471524bbb030720fc4bb4e7601fa30f5b12b0e94d02ec9484c`.
- Corpus SHA-256: `fece2121b487b37e1217b4854b74308366399938e26520e06d124ed63559aed9`.
- Package SHA-256: `46a56a5bd436b012c596c65d1f7d85fe6cd8fadbd702362955804415e00c0d41`.
- Native executable: 562,768 bytes, SHA-256 `c615e29d5ffaf595f6ab6375c0efaf383352abbc6fb037510ab815f89d7a1c4c`.
- Symbol map SHA-256: `f34da7a322ecba3b5e3520528d90a33c9c74a12c92f0852e5ca5d81dd6b9cd1c`.
- Driver SHA-256: `70bf78d5014b9a0756a10d20e14de465e3042827870d9a952581b13fd4ae5d49`.
- Sampler SHA-256: `960c38442a7111d20f834316fec666dfc9bb05c27cdec53f22a260242f15b860`.

Each receipt must match the exact five-file source inventory, guest-command
fingerprint, observer defines, executable/symbol/tool identities and FS-UAE
3.1.66 configuration, apart from its ephemeral Work mount. Each run requires a
distinct fresh START challenge. Split its raw console transcript at the single
bare `g` and reconstruct each pause independently with `snapshot_records`;
strictly decode the five raw records again and require equality with every
stored profile field. Verify the pause-local register frame, re-read getter
pointers and same-run retained locations, correlated active run identity,
zero overflow and `io=false, bulk=true`. Never borrow another run's addresses
or an earlier pause's missing bytes.

Actual frame times are observations, not exact requested stop times. Records
are read milliseconds later while the guest remains paused. The first pause
and resume perturb execution; no calibration removes that cost. Active phase
ticks are checkpoints, not live elapsed durations. Disabled I/O counters are
unavailable observations, not zero I/O. A later ROM PC does not identify its
native caller or provide a symbolized owner.

## Accepted snapshot pairs

| Receipt suffix | Early frame seconds | Later frame seconds | Tokenizer calls / opcodes | Parser calls / opcodes | Total VM opcodes | Copies / completed bytes |
|---|---:|---:|---|---|---:|---|
| baseline | 60.795863 | 100.702199 | 303 / 15,331 | 557 / 5,506 | 20,837 | 2,193 / 113,575 |
| r2 | 60.972717 | 100.738523 | 306 / 15,426 | 562 / 5,559 | 20,985 | 2,215 / 114,628 |

The baseline is `opforge-b10-exact-noio-live60-resume100-2026-09-04.json`,
SHA-256 `1653019faedeca97119bba714a2145a9b346870928a3da0d36260a0c94b83f33`.
The first new repeat is
`opforge-b10-exact-noio-live60-resume100-r2-2026-09-04.json`, SHA-256
`0d0537e23dce91fe2d2f681d22e6475c1200521e9570249b6cb204dfad71b0be`.
Their active run IDs are 3,789,466,346 and 3,789,546,899.

At each early snapshot, the session clear has requested 41,221,928 bytes but has
not returned; 516,003 bytes from preceding clears are already completed. VM
opcodes, copies and engine statement visits are zero. At each accepted later
snapshot, the session clear has completed and the phase is frontend. The
checkpoint total is 2,070 ticks, not the elapsed duration of that open phase.
There are still no engine statement visits, expression evaluator requests or
backend service executions. The statement-total field has not yet published a
completed frontend total; it must not be read as proof of no statements built.

These guests are intentionally stopped without DONE or a guest exit. Their
host tests exit 101 and diagnostic commands exit 1; `capture_ok`, `complete`,
`parity_passed` and `comparison_eligible` remain false. Accepted snapshot pairs
are Level E observations, not successful FS-UAE assemblies.

## Retained missing-snapshot attempt

`opforge-b10-exact-noio-live60-resume100-r3-2026-09-04.json`
(SHA-256 `48a2b9842aa14b8d1e874d1ab967c944b67cf969646d27ffdfd929a5cf80bfe0`)
contains one valid snapshot at 60.762540 host seconds. It is in package/setup,
with the same outstanding 41,221,928-byte session clear and no VM opcodes or
statement visits. Run ID is 3,789,556,746.

The sampler requests resume at 60.768630 seconds and debugger entry at
100.726345 seconds, but the retained console transcript ends at the first `g`.
There is no second activation message, prompt, register frame or memory dump.
The sampler stops at its deadline; the coordinator reports its unchanged
120-second timeout without DONE/exit. `resample_observed=false`, final `pc=null`
and final registers are empty. The initial snapshot remains explicitly within
snapshot 0 and cannot supply later evidence.

This attempt is excluded from all late-work ranges, not discarded or counted
as zero progress. The absent debugger response does not distinguish a lost
entry request, transport problem, emulator issue or native stall. It is an
open observation failure, not a diagnosed native regression or a proven
instrumentation defect. One unchanged retry is the bounded discriminator;
no timeout extension or speculative code change is made.

## Retry without a debugger frame

`opforge-b10-exact-noio-live60-resume100-r4-2026-09-04.json`
(SHA-256 `b436cd0ac62ed07d89a976f74e77a4789b5539f5c7e37f0c83d173cd5497d539`)
matches the fixed identities and exact inventory and observes its fresh guest
START. Debugger entry is requested at 60.711167 seconds, but there is no
activation message, prompt, frame, memory read, command or accepted snapshot.
The automation call returns without a reported exception; that is not evidence
that the emulator received or acted on the key event. The sampler stops at its
deadline with empty final registers, `pc=null`, `sample_observed=false` and
`resample_observed=false`. No phase, run ID or work count is inferred.

Unlike r3's coordinator timeout, r4 reports the sampler deadline and launcher
exit 1, followed by proof rejection because the exact fresh start/done challenge
and guest exit evidence are not all present. Test exit is 101 and diagnostic
command exit is 1; no coordinator timeout is claimed for r4. All
completion/parity flags remain false.
No further retry is made. Both failed attempts stay in the record, so the
intended three usable pairs have **not** been collected: only baseline and r2
support a later-work comparison.

## Findings and stop/go decision

| ID | Hypothesis | Evidence for | Evidence against / limit | Status | Next discriminator |
|---|---|---|---|---|---|
| R1 | B10 advances from session initialization into frontend work | Baseline and r2 independently show the pending session clear at about 61s and its completion plus tokenizer/parser work at about 101s | Two other attempts lack later frames; backend behavior is unobserved | confirmed | Isolate frontend costs after capture reliability is restored |
| R2 | The later work position is tightly repeatable | Usable pair totals are 20,837 and 20,985 VM opcodes; tokenizer calls 303/306 and parser calls 557/562 | Only two usable pairs, slightly different stop times, earlier baseline and observer effects; two missing observations prevent the intended repeat check | open | Resolve missing debugger responses before claiming a repeatability range |
| R3 | Missing frames demonstrate a guest stall | No later frame in r3; no frame at all in r4 | No guest PC/counters or completion at the missing boundary; key-request success is not an emulator acknowledgement | open | Localize debugger activation/foreground delivery and prompt acknowledgement separately from guest execution |

The usable later snapshots differ by 148 VM opcodes, 22 copies and 1,053
completed copied bytes. These are observed work-position differences, not a
rate, elapsed-time improvement, confidence interval or calibrated observer
overhead. The earlier all-counter B10 observation is not another replicate of
this no-I/O mode and is not mixed into the range.

Stop identical B10 retries for now. The concrete next implementation slice is
the host debugger-entry boundary: distinguish targeting/foreground/key delivery
from an acknowledged fresh prompt, and fail explicitly when acknowledgement is
missing. First localize that boundary with focused host checks and one bounded
confirmation; do not add speculative native fixes or relax evidence checks.
If reliable entry still cannot be obtained, record the unresolved transport
boundary before selecting another approved observation mechanism.

Once capture reliability is established, narrow compute attribution inside
the frontend rather than collecting more copies of the same coarse endpoints.
The existing frontend phase interleaves source ingestion and statement building
(`opforge-cli/source_reader.asm`, `opforgeNativeCliTokenizeFrontend`); its single
open-phase checkpoint cannot separate module/read cost from tokenizer/parser
execution and statement retention. Any additional guest timing must use the
approved debug/assert framework and a scoped instrumentation plan, not guessed
hotspot rankings. Item 0f's full attribution report and optimization stop/go
decision remain open.

## Validation and status

- Level A/B host checks: nine capture tests and eleven corpus tests pass. They
  validate host contracts, not the missing emulator acknowledgements.
- Level E: baseline and r2 each have two independently reconstructed/decoded
  snapshots; r3 has one; r4 has none. Missing responses never inherit old data.
- Native code, capture code, counter defines, input corpus and timeout policy
  are unchanged. No product fix or performance improvement is claimed.
- The new guest trees are removed on return; retained JSON contains diagnostic
  observations only and cannot select a parity oracle.
- Plan-bundle validation with pending-gate allowance and whitespace checks pass.
  Full Rust/native closure gates remain pending. No files are staged, committed
  or pushed for this unfinished item.
- Reviewer `platform_coverage_review` (Hume) independently audits the baseline
  and all three new attempts and returns interim PASS for the final evidence
  wording. This approves the bounded report, not Item 0f closure or the missing
  repeatability measurement.
