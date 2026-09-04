# Production-path performance corpus v1

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

The configuration pins actual CPU 68020, `uae_cpu_speed=max`, and
`jit_compiler=0`; boot ROM/device mappings are inherited and printed in the run
configuration. The existing runner replaces the Work mount and supplies 64 MiB
Zorro III memory. FS-UAE documents different CPU-speed defaults for lower and
higher CPU models, so CPU selection alone does not define comparable timing.
[CPU-speed documentation](https://fs-uae.net/docs/options/uae_cpu_speed/) and
[JIT documentation](https://fs-uae.net/docs/options/jit_compiler/).
Max-speed emulation is host-dependent and is not calibrated to the A6000's MIPS.

## Current evidence and open gates

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
remain open for investigation/closure. Item 0f and later optimizations have not begun.

## Focused hypothesis ledger

| ID | Hypothesis | Evidence for | Evidence against | Status | Next discriminator |
|---|---|---|---|---|---|
| C1 | Nested include exceeds current native support | Nonzero guest include errors; `PreparePendingInclude` rejects nonzero depth; limit is one | Rust permits the nested input | confirmed | Confirm the documented sibling-include candidate without changing native semantics |
| C2 | The bounded source/statement/branch cases need compute or I/O attribution | B02/B03/B05 and enlarged B10 lack completion after 120 seconds; B04/B06/B07 complete | No PC samples or partial counter snapshots yet identify the owner | open | Approved incomplete-run snapshots/PC sampling, or an explicitly longer bounded run |
| C3 | B09 diverges at section-symbol/layout resolution | Fresh nonzero exit, `OPC-NCLI022`, Rust emits all eleven artifacts | Earliest internal divergence has not been localized; no fix claim | open | Focused Rust/native boundary comparison of the actual forward section-symbol case |
| C4 | Unconditional include reporting violates B08's empty-stdout contract | Corrected B08 fails only the stdout assertion after exact BIN/zero-exit proof; include processing calls the reporting routine without a debug flag | Actual unexpected text was not retained by that run | open | Fresh focused run capturing the strict assertion's stream text; retain failed parity until resolved |

All initial native receipts above use FS-UAE app version 3.1.66. Later adapter
changes additionally enforce empty guest streams, hand off the exact live Rust
package bytes, reject CPU-model overrides/ambiguous sections, and report a
package-bearing command template. Those stricter checks require fresh receipts.
