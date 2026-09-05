# Production-path performance corpus v1

## Current scheduling amendment (2026-09-05)

The active performance plan now assigns broad nonterminal native qualification
to Step 16 / Item A-close. The user subsequently deferred all LSP work until
the final Item LSP-close. Step 08 / Item 0f requires its focused observation
checks and the explicit `run_rust_quality_gate.sh --defer-lsp` gate; it does not
rerun the 51-group native gate or repair LSP. Older scheduling statements below
are historical and superseded by that reviewed plan. All recorded failures,
proof limitations and raw observations remain unchanged. Raw B03/B10 receipt
SHA-256 identities and byte counts are inventoried in
`documentation/performance/results/opforge-item0f-observation-inventory-2026-09-05.json`.


Status (2026-09-04): Item 2 frozen-input/result-protocol review and focused gates
pass under the approved diagnostic entry exception. All ten current cases pass
seven retained Rust release invocations with repeatable artifacts.
Native completion is not established for the full matrix. No performance gain,
physical A6000 runtime, or terminal self-host proof is claimed.

Active `AGENTS.md` remains binding. This document records the implementation of
Item 2 in the Rust-first VM/native performance plan; its completion freezes inputs
and the protocol, not successful native parity.
Plan workflow: `skills/opforge-plan-authoring/SKILL.md`,
`scripts/workflow/run_plan_workflow.sh`, and `make workflow-gate`.

The approved sequencing amendment is commit `8c0f6b60`. It permits diagnostic
entry on failed/incomplete native baselines, not parity or performance acceptance.
The retained, non-oracle records are:

- `results/opforge-corpus-v1-manifest.json`: exact inputs/commands/package,
  corpus digest `fece2121b487b37e1217b4854b74308366399938e26520e06d124ed63559aed9`.
- `results/opforge-corpus-v1-rust-baseline-2026-09-04.json`: fresh seven-run
  release measurements with current Cargo-selected executable provenance,
  compiled at `8c0f6b60`; executable 4,324,912 bytes, SHA-256
  `9f207b431f6a7ecf034da9db0f76de55a857890a3335d3cb7ace525dc0db90a0`.
- `results/opforge-corpus-v1-native-status-2026-09-04.md`: per-case historical
  status, explicit proof gaps, native binary-provenance limitation and no
  accepted native timing comparison.

## Inputs and commands

`scripts/performance/production_corpus.py` deterministically generates ordinary
source files. It does not add benchmark-dependent assembler behavior. The
manifest records each input's UTF-8 byte length and SHA-256, the complete public
CLI argument list, expected artifacts, source composition, and package identity.
The case digest covers full source bytes and the final command before the
manifest replaces source text with compact per-file fingerprints.

| Case | Composition | Independent BIN contract |
|---|---|---|
| B01 | Ten source lines, nine no-operand instructions | Nine `EA` bytes |
| B02 | 1 MiB of comment/whitespace lines, one data byte | `42` |
| B03 | 256 trivial instructions | 256 `EA` bytes |
| B04 | 128 labels and 128 references | Sequential bytes plus little-endian label addresses |
| B05 | Canonical forward-width stability root plus 64 branch groups | Canonical absolute encodings, origin gaps, and short branch bytes |
| B06 | 128 arithmetic expressions referring to a constant | Computed little-endian words |
| B07 | Counted loops enclosing IF/MATCH alternatives | 32 `42` bytes |
| B08 | Main → math → helper modules and two sibling includes | `11 22 07` |
| B09 | Existing linker-region fixture extended with Hunk and metadata output | Full set checked for presence and exact run-to-run bytes |
| B10 | 256 mixed instruction/expression/forward-branch groups, modules, two includes, nested flow, region placement, BIN/PRG/map | Full set checked for presence and exact run-to-run bytes |

B09 requires eleven artifacts, including S-record, HEX, listing, section exports,
map, Hunk, PRG and binary image outputs. It changes the reused fixture's symbolic
`.word` to `.long` because the Hunk path supports the latter relocation. S-record
is requested with the implemented CLI `--srec` option, not an unsupported
`.output format=srec` directive. No golden examples or references are modified.

B08/B10 deliberately use sibling includes: the current native reader rejects
a second include level (`NATIVE_INCLUDE_DEPTH_LIMIT = 1`). An initial nested
candidate failed with fresh nonzero guest exit and `OPC-NCLI014/040`; it was not
accepted as parity. The transitive module dependency remains present. This
corpus does not claim nested-include support.

B10 is a bounded cross-mechanism workload, not a reduced self-host correctness
test. Its original 32 groups were increased to 256 to make it less dominated by
startup. The current region spans `$0800..$1fff`; the BIN is 2,066 bytes. Final
native sizing and the physical A6000 envelope remain unvalidated.

```sh
python3 scripts/performance/production_corpus.py manifest --output /tmp/corpus.json
python3 scripts/performance/production_corpus.py rust --output /tmp/rust-result.json
python3 scripts/performance/production_corpus.py validate --result /tmp/rust-result.json
python3 -m unittest discover -s scripts/workflow/tests -p test_production_corpus.py
```

Output files must not exist; the tool refuses to overwrite them. `--case B01`
selects a smoke subset and `--runs 1` requests one retained sample. Such outputs
explicitly have `comparison_eligible: false`. No arbitrary `--binary` is accepted:
the runner first executes `cargo build --release --locked -p cli --bin opforge`.
The executable is taken from Cargo's compiler-artifact JSON, including configured
target directories, not assumed to live at `target/release/opforge`. It records
the command, Cargo artifact profile, default-feature policy, Cargo.lock digest, compiler and
Cargo versions, build-related environment, executable hash/size, host, HEAD,
generator digest, and package hash/size.

Every measured invocation gets fresh temporary inputs and outputs. One unmeasured
warm-up precedes seven retained runs per case by default. Host filesystem caches
are warm; no cache flush is claimed. The ledger preserves all elapsed nanoseconds,
median, min/max and nearest-rank p95 (the maximum for seven samples). This is a
fixed-order baseline, not a randomized paired optimization comparison. Ambient
host activity can affect timings; structural comparison eligibility is not proof
that variance or observer overhead is negligible.

Exit status must be zero and both Rust output streams empty. All requested files
must be regular, nonsymlink files within the fresh output tree. Repeated artifacts
must match exactly, including the warm-up. B01–B08 also have independent semantic
BIN fingerprints. The result validator rejects missing/duplicate/undeclared
cases, package/command mismatch, incomplete flags, changed diagnostics, invalid
sample counts, derived-statistic mismatch, missing artifacts, malformed digests,
duplicate JSON keys, nonfinite numbers and a boolean masquerading as schema 1.
Result JSON is a measurement record, never a native oracle.

## Native confirmation

The test adapter obtains exact source/command/package bytes and fresh in-memory
Rust artifacts from the generator's stdout-only `native-input` command. It does
not read stored result files or select an oracle by evidence filename. Native
arguments use the same public options with `Work:` paths; the same package bytes
are explicitly supplied. Each case uses the existing fresh challenge/start/done,
explicit exit and byte-for-byte multi-artifact proof. Guest stderr/stdout must
match the empty diagnostic contract separately from launcher teardown messages.
Case trees, including unsuccessful runs, are ephemeral. Post-run validation
panics are caught so later cases are still attempted; a caught panic is failure.

Generate a separate configuration without modifying the configured boot template:

Invocation policy: opt-in-allowed for this user-authorized bounded corpus only.
Keep the environment attached to the single-instance test command below; it is
not a default CI gate or authorization to launch unrelated native tests.

```sh
python3 scripts/performance/production_corpus.py fs-uae-config \
  --template '/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' \
  --output /tmp/opforge-performance.fs-uae

OPFORGE_PERFORMANCE_CORPUS=1 \
OPFORGE_NATIVE_CORPUS_CASES=B01,B10 \
OPFORGE_FS_UAE_SMOKE=1 \
OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' \
OPFORGE_FS_UAE_CONFIG_TEMPLATE=/tmp/opforge-performance.fs-uae \
OPFORGE_FS_UAE_ARGS='{fsuae_config}' \
OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=120000 \
RUST_TEST_THREADS=1 \
cargo test -p asm external_fs_uae_native_production_corpus_parity -- --nocapture --test-threads=1
```

Omit `OPFORGE_NATIVE_CORPUS_CASES` to attempt all ten. Profiling is off by default;
`OPFORGE_NATIVE_CORPUS_PROFILE=all` enables the existing approved counter groups
and fixed terminal exports. Terminal profile decoding requires correlated,
complete, zero-overflow records. Timed-out runs cannot supply terminal proof.
`CORPUS_RESULT` lines carry the command template (including the rendered-by-runner
package placeholder) and package digest. They are reports of that invocation,
never reusable proof inputs.

### Controlled incomplete capture (Item 0f)

The diagnostic entry point uses the existing debug framework's build-time
`OPFORGE_PROGRESS_ABORT_VISITS` control and sealed-record exporter; it does not
truncate the frozen source. Exactly one case and a limit from 1 to 100,000 visits
are required. The ordinary parity test remains separate and unchanged in proof
requirements. Example, with the same explicitly opt-in FS-UAE environment above:

```sh
python3 scripts/performance/production_corpus.py diagnose --case B03 \
  --abort-visits 1 --output /tmp/b03-abort1.json
```

Each command makes one capture (`--runs` is for Rust baselines). The guest keeps
the 120-second post-start ceiling. The result contains the exact case/package
identity, command/defines, guest challenge messages, raw fixed-size records,
decoded correlated counters, host test transcript and capture status. Output
paths must be new. Guest case and decoder scratch trees remain ephemeral.

`capture_ok: true` means a fresh completed protocol returned nonzero, the abort
flag and visit limit matched, and all five incomplete counter groups decoded
without overflow. It **never** means successful assembly: `complete`,
`parity_passed` and `comparison_eligible` remain false. A timeout, skipped guest,
unexpected error, missing record or mismatched counter returns a failed capture.
Raw diagnostic records are retained for report auditing, never as parity oracles.
Native executable digests and observer calibration are still required before
any quantitative performance acceptance; these initial captures are Level E.

The configuration pins actual CPU 68020, `uae_cpu_speed=max`, and
`jit_compiler=0`; boot ROM/device mappings are inherited and printed in the run
configuration. The existing runner replaces the Work mount and supplies 64 MiB
Zorro III memory. FS-UAE documents different CPU-speed defaults for lower and
higher CPU models, so CPU selection alone does not define comparable timing.
[CPU-speed documentation](https://fs-uae.net/docs/options/uae_cpu_speed/) and
[JIT documentation](https://fs-uae.net/docs/options/jit_compiler/).
Max-speed emulation is host-dependent and is not calibrated to the A6000's MIPS.

## Current evidence and open gates

The current consolidated stop/go decision is
`results/opforge-native-item0f-attribution-decision-2026-09-04.md`. The repeated
B10 sampling loop is stopped: startup clearing is localized and frontend
progress is observed, but backend/hardware attribution remains unproven. One
new host-entry confirmation records verified foreground PID and fresh prompts
for both pauses. The user-approved Phase 0 gate runs all 51 nonterminal native
groups; full-product assembly and two-generation self-hosting remain mandatory
at terminal qualification. Both required gates completed and failed; closure
now depends on focused failure classification and remediation, then fresh gate
results and final compliance review—not more identical snapshots.
Neither deferred self-host group is counted as passed.
The final native result is 38/51 groups passed and 13 failed, including 29 exact
300-second timeout events, four completed pass-2 branch rejections and one wrong
negative-case diagnostic. The Rust quality gate also failed at the LSP client
integration suite (34 passed / 14 failed) after all 1,586 assembler tests passed.
Exact results are in `results/opforge-item0f-closure-gates-2026-09-05.md`. No
further B10 sampling is scheduled.

- Level A/B: corpus/schema tests cover negative cases and Cargo-selected paths.
  These do not prove native execution or representative hardware performance.
- Level A: all current ten cases pass seven retained Rust release runs, with
  exact artifact repeatability and the independent small-fixture contracts.
- Level D: initial B01, B04, B06 and B07 guests completed with explicit zero exit and
  exact same-case live Rust BIN equality. B01 used the initial 68020 configuration
  without an explicit speed setting; B04 used max speed. They are not a timing
  comparison. These initial receipts precede the added empty-stream assertion.
- Incomplete/localization only: B02, B03 and B05 exceeded 120,000 ms after guest start
  without completion. Neither supplies parity proof or an elapsed throughput.
- Initial B10 failed at its nested include. The supported sibling-include,
  enlarged candidate subsequently exceeded 120,000 ms without completion;
  captured partial stdout/stderr were empty. It supplies no parity proof.
- Initial B08 failed for the same nested-include restriction. The eight-case
  cohort still attempted every subsequent case and finished in 1,179.10 seconds;
  that total includes host build/guest startup/teardown and is not assembly time.
- Corrected B08 completed with zero guest exit and exact BIN equality, but failed
  the stricter empty-stdout assertion. That failure was caught and B10 was still
  attempted. The two-case run took 299.56 seconds including build/startup/teardown.
  The assertion now reports unexpected stream contents for future localization;
  the failed run did not retain that text. Source inspection finds unconditional
  include-line reporting in `line_processor.asm`/`report.asm`; this is a likely
  explanation, not yet an exact captured-stream match or a corrected invariant.
- B09 completed the guest protocol with nonzero exit and `OPC-NCLI022` (unresolved
  native label), followed by `OPC-NCLI020`. This is a correctness failure, not a
  timeout or successful artifact proof. Its source has not been simplified to
  hide the unresolved path.
- `make workflow-gate` passed (104 tests in the latest run). It initially exposed a
  stale expected owner-import tuple left by Item 0e; the tuple now includes the
  already-approved platform observer, without relaxing the dependency guard.
- Full Rust gate preflight identified exactly two safe redundant `tst.w` checks
  in the platform/runtime counters. They have been removed: the preceding
  `move.w` sets the same NZVC flags and preserves X, so zero-class/service
  branching, registers, memory, stack balance and public saved CCR are unchanged.
  No new instrumentation or production optimization was introduced. The focused
  runtime/platform counter contracts subsequently passed two tests covering five
  fresh guests (88.67 seconds total), with explicit zero exits and same-case
  oracle checks. These are counter proofs, not corpus performance evidence.
- The full Rust gate passed preflight but exposed stale product-staging and
  capacity snapshots. The added profiler files require 99 staged source files
  (previously 96) and a classic-Amiga-safe alias for the 31-character
  `opforge_symbol_expr_profile.asm` filename. The alias maps only the staged
  component to `opforge_symexpr_profile.asm`; canonical source is not renamed.
  The focused filename-map check and host full-product Hunk/S-record/listing
  oracle/staging check pass; native execution was skipped in the latter, so it
  does not prove self-host parity. Capacity snapshots now reflect
  1,663,953 loadable source bytes, 91,795 expanded rows/3,517,576 expanded bytes,
  419 imports, and 6,575 public declarations/128,293 name bytes. All six focused
  capacity tests pass; allocation and line-length limits are unchanged. The
  already-failing full-suite run was stopped after roughly 28 minutes while its
  remaining exhaustive VM table tests were still active. It exited on SIGTERM,
  not successful completion; the full gate remains failed/incomplete.

The independent preliminary reviewer required width-stability coverage, verified
release provenance, exact schema type and independent artifact contracts; those
are implemented. Final independent Item 2 plan-compliance review passed, as did
the actual staged workflow/native gate (104 tests; 238 formatted native files).
The full native wrapper has not run. The user approved incomplete-run diagnosis without accepting these
native failures. Runtime bounds, complete native coverage and counter overhead
remain open for investigation/closure. Item 0f has begun controlled incomplete
captures; no later optimization has begun.

## Focused hypothesis ledger

Source-inventory correction (2026-09-04): profiling flags previously selected an
extra generic main-source alias in the guest harness. Earlier profiled captures
remain observations of that extra-file workload, not exact-frozen-discovery
measurements. The selector is corrected and new console receipts verify every
discoverable `.asm`/`.inc` path/size/hash against the frozen manifest before
launch. See `results/opforge-native-io-counter-calibration-2026-09-04.md`.

For I/O-counter calibration, `diagnose --diagnostic-profile all-no-io` enables
the existing I/O-counter kill switch while retaining bulk and the other counter
groups. The default is `all`; ordinary corpus parity cannot select `all-no-io`.
Decoded enabled-group flags must match the request. Disabled I/O fields are
unavailable observations, not measured zero work.

The full-B03 common-boundary ABBA control (all/no-I/O/no-I/O/all) reaches the
existing one-visit abort with identical shared work. Mean START-to-DONE times
are 76.566 and 76.363 seconds. This small four-run difference supports using
`all-no-io` provisionally for compute/bulk attribution, not an overhead percentage
or B10 timing correction. B03 has no module candidates. Raw receipts and the
fail-closed comparison are documented in
`results/opforge-native-common-boundary-controls-2026-09-04.md`; all remain
Level E controlled-abort evidence, not complete assembly or parity.

Repeated B10 no-I/O sampling subsequently yields one additional usable 60/100s
pair (20,985 VM opcodes at 100.739s versus baseline 20,837 at 100.702s), one
early-only capture and one retry with no debugger frame. The missing responses
are retained, not converted into zero progress or guest stalls. The intended
three usable pairs are not available; debugger-entry acknowledgement must be
localized before more sampling. See
`results/opforge-native-b10-repeatability-2026-09-04.md`. Inputs, executable,
capture tools and the 120-second ceiling are unchanged.

For the user-authorized local console diagnostic, add
`OPFORGE_FS_UAE_CONSOLE_DEBUGGER_AUTOMATE=1` and
`--sample-after-seconds 30` (integer 1–100) to a single-case `diagnose` invocation.
This launches stock FS-UAE on a PTY, waits for the exact fresh START challenge,
then sends Cmd+D only to that launched process. Fixed read-only commands capture
registers/disassembly and, when uniquely bound to the actual Hunk, the existing
five counter records. The coordinator removes the config, symbol map, binary,
guest files and logs on return. The JSON retains bounded Level E observations,
not an executable or reusable oracle. The unchanged proof check rejects missing
guest completion/exit; `sample_observed: true` is not `capture_ok`, parity or a
successful assembly. Live active-record exit fields are not guest exit evidence.

For two snapshots in the same guest, use `--sample-after-seconds 60
--resample-after-seconds 100`. The second delay is absolute from fresh START,
at least five seconds after the first and no greater than 100. Before sending
bare `g` to resume, the sampler requires all five strictly decoded, correlated,
active records without overflow. It retains only that process's verified
mapping, then rechecks getter opcodes/pointers and run identity at the later
pause. Each pause has independent transcript parsing: missing later bytes
cannot be filled from the first stop. Exact completion checks prevent sampling
after guest completion. The coordinator's 120-second bound is unchanged.

Two-stop reports put binding attribution, PCs, times and records in `snapshots`;
they omit top-level counter/binding aliases so a failed second stop cannot
inherit first-stop evidence. `resample_observed` requires two accepted snapshots
and complete cleanup; it does not mean guest completion, parity or an elapsed
assembly measurement. Recorded host intervals include debugger interruption,
and sending `g` alone does not prove execution resumed.

For non-interrupting observation controls, omit the sample delay and use
`--control-mode app`, `--control-mode pty`, or `--control-mode console` instead.
All use the same complete frozen case and visit-abort control. The first uses
the normal macOS app launch path with console disabled; the latter two launch
through a PTY with console disabled/enabled respectively. No control sends
Cmd+D or debugger commands. The host records exact fresh START/DONE and explicit
exit observations before teardown, at 100 ms polling granularity. The existing
coordinator still independently validates protocol, expected nonzero diagnostic
exit and counters. A timing receipt without that valid controlled-abort capture
is not a comparable control. These observations do not measure counter-disabled
overhead, debugger-entry pause cost, completed-workload performance or A6000
throughput. Control waits stop at 115 seconds after START; sampling reserves
15 seconds after the chosen stop for bounded reads/cleanup. The coordinator's
120-second post-start ceiling is unchanged.

If a live sample lands outside the loaded opForge CODE, optional
`--binding-register d6` requests one bounded disassembly at the actual captured
register value. Only a unique match against the current Hunk may locate its
counter getters. The receipt keeps the actual PC and labels this anchor as a
register candidate, not sampled-PC attribution. No fixed runtime address or
previous run's mapping is reused. A fresh completed guest protocol stops the
sampler rather than reading a finished process as though it were still active.

The 2026-09-04 B10 live30 and live60-retry captures identify the session-init
bulk-clear loop, with a 41,221,928-byte request. The latter independently decodes
all five correlated active records: package/setup phase, no statement visits,
no source/package reads or VM opcodes yet. This localizes an early setup cost,
not the entire original timeout or later self-hosting runtime. The first live60
attempt failed in the host Hunk parser before guest launch and remains recorded.
See the Item 0f first-captures report for exact provenance and limitations.

| ID | Hypothesis | Evidence for | Evidence against | Status | Next discriminator |
|---|---|---|---|---|---|
| C1 | Nested include exceeds current native support | Nonzero guest include errors; `PreparePendingInclude` rejects nonzero depth; limit is one | Rust permits the nested input | confirmed | Confirm the documented sibling-include candidate without changing native semantics |
| C2 | The bounded source/statement/branch cases need compute or I/O attribution | Corrected B10 module counts match its root-scan prediction; two usable no-I/O pairs reach frontend with 20,837/20,985 VM opcodes near 100.7s; B03 shared-work observer controls are recorded | Two later attempts miss debugger frames; intended repeat check, B10 overhead, backend work and complete runtime remain unproven; older captures include an unintended alias | open | Localize debugger-entry acknowledgement, then isolate frontend costs with approved instrumentation |
| C3 | B09 diverges at section-symbol/layout resolution | Fresh nonzero exit, `OPC-NCLI022`, Rust emits all eleven artifacts | Earliest internal divergence has not been localized; no fix claim | open | Focused Rust/native boundary comparison of the actual forward section-symbol case |
| C4 | Unconditional include reporting violates B08's empty-stdout contract | Corrected B08 fails only the stdout assertion after exact BIN/zero-exit proof; include processing calls the reporting routine without a debug flag | Actual unexpected text was not retained by that run | open | Fresh focused run capturing the strict assertion's stream text; retain failed parity until resolved |

All initial native receipts above use FS-UAE app version 3.1.66. Later adapter
changes additionally enforce empty guest streams, hand off the exact live Rust
package bytes, reject CPU-model overrides/ambiguous sections, and report a
package-bearing command template. Those stricter checks require fresh receipts.
