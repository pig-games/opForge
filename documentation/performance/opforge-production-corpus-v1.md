# Production corpus tooling

Maintained input definitions and live measurement commands. Previous optimization
plans and recorded runs are available in Git history; they do not schedule work.

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
