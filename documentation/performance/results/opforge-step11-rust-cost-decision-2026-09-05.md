# Step 11 / Item R0: first Rust cost decision

Status: GO for the bounded Step12 early-return experiment, subject to this
item's workflow/compliance receipt and commit. No runtime gain claimed.
Active AGENTS.md remains binding. Source: active performance plan Item R0.
Base: `9af54933ede079be83ecb54c22fda539b56b8a9b`; branch
`codex/rust-vm-native-performance`; worktree
`/Users/erik/Code/Retro/opForge-wt-rust-vm-native-performance`.

## Performance contribution

This slice selects one bounded Rust optimization experiment; it changes no
shipped Rust behavior. Existing real-CLI phase timers place most B01/B10 cost
inside the first layout pass. Source review found fallback package construction
whose resulting bytes are discarded when an explicit package is supplied.
The proposed saving is avoiding that construction, not all first-pass work.

## Baseline and coverage

Default stripped release, locked dependencies, no feature overrides, exact
frozen production corpus, one warmup and seven fresh-process measured runs per
case. Every run completed with empty streams and identical artifact hashes.
B01 median 141.556166ms, range 134.793500–143.470458ms. B10 median
160.046041ms, range 158.939958–162.067417ms. These are standalone baselines,
not matched optimization comparisons or native runtime results.

Three existing phase/module profiles per case retained exact artifacts. Median
first-layout bucket: B01 118.817ms (90.4% of assembly), B10 129.219ms (83.3%).
Profile wall medians were B01 135.693ms and B10 158.743ms, lower than separate
unprofiled baseline medians; this does not establish negative profiling overhead.
Nested phase buckets must not be added as disjoint owner costs.

The bounded host-sampling session tried one late-attached stripped process and
one pre-armed symbols-retained process. Both returned empty call graphs despite
completed exact artifacts; neither establishes owner presence, absence or cost.
The symbols-only build changed executable identity and size. Its first run was
cold (520.276ms); following runs were 165.391ms and 163.713ms. Those diagnostic
runs are not optimization samples. No further sampler retries are planned.

## Candidate boundary and rollback

`crates/opforge-asm/src/runtime_model.rs`:
`build_execution_model_for_request_with_artifact_path` constructs bundled
fallback bytes before invoking the shared bootstrap. In
`crates/opforge-vm/src/runtime_bootstrap.rs`, an explicit path unconditionally
selects that file with no fallback. The candidate is an early explicit-path
return through the existing shared helper before fallback construction. Leave
the shared VM helper and all no-explicit behavior unchanged.

Existing tests cover explicit valid selection, bundled fallback and feature
variants. Step12 should add focused low-level missing/malformed explicit-input
cases with a valid cwd package available, proving no fallback occurs. Preserve
all artifact/diagnostic semantics. Revert the small early-return diff if the
matched integrated improvement does not exceed max(5%, relative range), or any
correctness regression occurs. No global cache, IR or package format change.

## Targeted owner result and overhead

One temporary timer surrounded only eager fallback construction, used the
existing execution-path profiler, and emitted exactly one owner invocation in
each of six profiled B01/B10 runs. Same timer binary, one warmup per case,
three alternating off/on pairs per case; all 14 runs had exact baseline artifacts.
All off streams were empty. On runs enabled OPFORGE_PROFILE_PHASES and
OPFORGE_PROFILE_EXECUTION_PATHS; diagnostic output is expected only there.

| Case | Owner median/range (ms) | Off wall median (ms) | On wall median (ms) | Median wall difference (ms) |
| --- | ---: | ---: | ---: | ---: |
| B01 | 100.479 / 100.334–104.365 | 140.474833 | 140.742083 | +0.267250 |
| B10 | 101.305 / 101.168–102.253 | 159.312667 | 162.513000 | +3.200333 |

The median of paired B10 differences is +3.322333ms; this differs from the
difference of medians above. These measure the whole enabled profiler's observed
overhead and scheduling variation, not just the timer. Owner time covers this
construction on these explicit-package inputs; it is not an additive exclusive
phase share, a native cost, or an already realized speedup.

## Decision and next implementation

GO: remove the discarded fallback package build only for explicit-package
requests. Roughly 100ms of directly timed work is large enough relative to the
160ms B10 baseline to justify the tiny source change. Step12 must measure
unprofiled matched control/candidate real B01 and B10 runs, require exact
artifacts and empty streams, record executable size and use the plan threshold.
No gain is accepted from this diagnostic measurement alone.

Step12 files: `crates/opforge-asm/src/runtime_model.rs`, focused runtime-model
tests in `crates/opforge-asm/src/tests.rs`, result evidence and plan bookkeeping.
Keep the previous committed helper as the reference/rollback. Eligibility is
exactly explicit path present; no cache or invalidation mechanism is added.
Missing/malformed input must remain fail-closed. No-explicit, artifact, bundled,
unbundled and runtime-only behavior are preserved and checked where applicable.
Mandatory non-LSP Rust gate and independent compliance precede its code commit.

Native disposition pending Step12: this Rust helper also serves host assembly
but its saving for native-test preparation has not been measured. The native CLI
loads its existing package and has no Rust registry-to-package construction to
port directly. Do not insert native work without a measured counterpart; continue
to the planned statement-arena audit after the Step12 transfer decision.

## Reproducibility and validation

`opforge-step11-rust-attribution-evidence-2026-09-05.json` embeds the baseline
receipts, exact raw profiles, both unsuccessful sample graphs, build identities,
probe patch, off/on rows and their original byte lengths/SHA256. The sampler
launch-to-observation interval does not prove total process lifetime.
Original release SHA256:
`9f207b431f6a7ecf034da9db0f76de55a857890a3335d3cb7ace525dc0db90a0`.
Timer release SHA256:
`607cfc19daf8ba90f110c490c15ef74bf72f47c57f69a7a1f22fc00eff0ac241`.
Both are 4,324,912 bytes; size equality is not binary equality.

The temporary source was restored byte-for-byte to HEAD, SHA256
`21ff5d1a2b518d93a428c7098d5eefcd74044cd8e7c14f91f1fd05616d604e1c`.
No Rust source change belongs to this commit. The independent methodology review
approved transient focused output/count/overhead proof and a documentation-only
gate after exact restoration. The target release executable is still the
identified diagnostic binary; Step12 must explicitly rebuild its candidate and
use the separately preserved original executable as control.

Both baseline corpus validators PASS. Workflow and final independent compliance
receipts are recorded in the plan sidecar before commit. All native failure debt
remains with A-close and all LSP work remains Step25. No Phase A closure claimed.
