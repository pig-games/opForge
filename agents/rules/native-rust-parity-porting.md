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

## Current runner safeguards

The [FS-UAE runner](../../crates/opforge-asm/src/fs_uae_smoke.rs) clears previous
case outputs before launch, binds the guest challenge to case identity, checks
completion/exit/output and recovers the serial coordinator after a failed case.
Its parity, smoke and diagnostic run trees are ephemeral: case inputs, outputs,
markers, logs and derived evidence are removed before the runner returns on
success, failure, timeout, crash or unwind. This retention policy remains in force;
it is separate from the conditions that make a result valid.

The [structural check](../../scripts/workflow/check_native_fs_uae_proof_contract.py)
checks runner safeguards. It is not execution proof. For commands and environment,
use the [FS-UAE guide](fs-uae.md); for investigation, use [failure triage](native-parity-failure-triage.md).
