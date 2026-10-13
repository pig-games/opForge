# Native parity

Use this guide when implementing or checking native behavior against Rust.
The [repository workflow](../../documentation/workflow/README.md) determines
scope, checkpoints and validation cadence. This document defines what a native
parity result means; it does not require emulator qualification for every edit.

## Reference comparison

Identify the Rust reference and native boundary, their inputs, outputs and known
non-equivalences. Preserve existing semantic equivalence; representation and
host-specific implementation may differ. Compare the actual case's results,
diagnostics, state or artifacts as relevant. Investigating the first divergence
is useful debugging, not a restriction to one file or one boundary per change.

## Evidence limits

Existing harness labels describe different kinds of evidence, not a sequence
of mandatory approval gates:

| Level | Evidence |
|---|---|
| A | Rust semantic oracle |
| B | Rust-side package/native harness contract |
| C | Host-side native request-shape simulator |
| D | Real native execution through FS-UAE |
| E | Localization or debug probe |

State material limits. Host simulation cannot replace required native execution;
a probe cannot establish parity. A semantically complete reduced case can prove
its own behavior, but cannot claim the coverage of an omitted full workload.

## Native result validity

For a positive artifact-parity case, all of the following must hold in the same run:

- The actual CPU, source bytes, command and package bytes identify the case.
  Its Rust oracle is carried directly by that case in memory. A stored evidence
  filename, display name, manifest alias or output path must not select the oracle.
- Exact guest start and completion responses match a fresh per-run challenge bound
  to that case. Prior capture/output files cannot supply any part of the result.
- The guest reports an explicit zero exit and the required output exists and
  matches that case's Rust oracle byte-for-byte.

An expected-failure case instead requires the same fresh completion protocol,
an explicit nonzero guest exit and the required diagnostic. Distinguish a passing
negative test from successful assembly. Other smoke/diagnostic checks still need
fresh guest completion and an explicit exit before they can claim a valid result.

A timeout, crash, absent or stale response, missing output, mismatch, launcher
success or previous green record never substitutes for the required evidence.
A failed case must not prevent later cases from executing and receiving their
own result; lock-poison fallout is not emulator evidence.

Full self-host qualification belongs to the exact tested configuration: source
manifest, bootstrap executable, package bytes and storage, assembly command,
output configuration and instrumentation. An external-package self-build does
not qualify an embedded-package self-build that exercises `.incbin`. A Rust-built
export is a host-validated candidate until that same configuration executes
natively. State the bundle's own qualification when handing it over; related
component or source-only proofs cannot supply that status. Testing an explicitly
identified candidate on hardware remains useful and does not require a prior
emulator qualification, but its results must be reported as a new hardware case.

## Current runner safeguards

The [FS-UAE runner](../../crates/opforge-asm/src/fs_uae_smoke.rs) clears previous
case outputs before launch, binds the guest challenge to case identity, checks
completion/exit/output and recovers the serial coordinator after a failed case.
Its parity, smoke and diagnostic run trees are ephemeral: case inputs, outputs,
markers, logs and derived evidence are removed before the runner returns on
success, failure, timeout, crash or unwind. This retention policy remains in force;
it is separate from the conditions that make a result valid.

The correctness-only [breadth runner](../../crates/opforge-asm/src/fs_uae_smoke/breadth_runner.rs)
accepts one frozen compact executable and cases with their live Rust oracles.
Each case has an isolated directory assigned as guest `Work:`, including absolute
output paths, and fresh identity-bound START/DONE and exit evidence. The separate
mounted `OPForgeBatch` volume keeps the executable and startup script outside
those case roots. A breadth-only ephemeral copy of the directory system drive
prepends the initial `Work:` assign to `S/user-startup` before the standard smoke
hook. Its generated drive label prevents `Work:` from being an immutable mounted
volume; saved system scripts/configuration and single-case runners stay unchanged.
Completed results, including failed artifact proofs, remain in memory after interruption.
Only unfinished work resumes: the interrupted case receives one isolated retry,
then failure is recorded and every later case is attempted. Capture occurs after
emulator shutdown and before ephemeral cleanup. This API records no performance
timing; existing single-case timing and diagnostic paths remain authoritative. The
API `run_prebuilt_compact_cli_breadth_from_env` owns the shared serial coordinator
lock. Its known-good invocation requires `OPFORGE_FS_UAE_SMOKE=1` plus canonical
generated-config arguments;
callers must not hold the coordinator lock while invoking it. Its host recovery
and isolation tests are in `breadth_runner_tests.rs`. The ignored
`native_compact_breadth_correctness` test supplies two independently assembled
live Rust hex oracles, current packages, and `OPFORGE_CORPUS_IMAGE` to the API.
Run that real-native check with `--ignored --test-threads=1`; host structural
checks do not qualify its guest volume assignments or native execution.

The compact family/opcore corpus audits use this runner when
`OPFORGE_CORPUS_BATCH_SIZE` is explicitly set to `1..64`. Unset retains the
single-case timing/diagnostic path. Batch mode prepares unchanged source trees,
current packages and live Rust oracles before execution, preserves original case
indices and host preparation errors, and records both timing fields as null.
A negative case with a fresh rejection and diagnostic is still marked as
unqualified for exact diagnostic parity; an absent requested artifact is a gap.

The [structural check](../../scripts/workflow/check_native_fs_uae_proof_contract.py)
checks runner safeguards. It is not execution proof. For commands and environment,
use the [FS-UAE guide](fs-uae.md); for investigation, use [failure triage](native-parity-failure-triage.md).
