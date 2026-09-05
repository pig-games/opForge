# Step 12 / Item 10: skip discarded fallback package construction

Status: implementation, matched comparison and required gates PASS; final
compliance/commit receipt follows in the plan sidecar. No Phase A closure claimed.
Active AGENTS.md remains binding. Source: active performance plan Item10 and
Step11 decision at `a162373e`. Branch `codex/rust-vm-native-performance`, worktree
`/Users/erik/Code/Retro/opForge-wt-rust-vm-native-performance`.

## Performance contribution

| Real CLI case | Original median (ms) | Candidate median (ms) | Saved (ms) | Improvement |
| --- | ---: | ---: | ---: | ---: |
| B01 | 140.343917 | 38.200500 | 102.143417 | 72.7808% |
| B10 integrated | 162.452666 | 58.512167 | 103.940499 | 63.9820% |

These are completed unprofiled host CLI invocations, including startup and output,
with the same frozen package/input/output flags. The repeated discarded build is
removed once per eligible bootstrap call. This speeds explicit-package assembly,
including comparable host work used by native validation. No native guest or
whole native-test-cycle improvement was measured in this Rust slice.

## Implementation and eligibility

Only `crates/opforge-asm/src/runtime_model.rs` changes production behavior:
`build_execution_model_for_request_with_artifact_path` handles a present explicit
path through the existing shared bootstrap before constructing fallback bytes.
The shared VM helper still makes that file authoritative. Missing and malformed
explicit files return None; cwd and bundled packages cannot mask the error.

No-explicit requests execute the existing fallback code unchanged. The explicit
path remains eligible regardless of CPU choice; generic code gains no CPU logic.
No cache, invalidation state, new runtime representation or package format is
introduced. The preserved original release and committed helper are the reference
and rollback; reverting this small early return restores the old work.
The diagnostic timer from Step11 is absent from both compared executables.

## Comparison protocol and retention decision

Three alternating candidate/control pairs per case, one unmeasured warmup for
each binary/case, serial runs with fresh temporary input/output trees. All 16
runs completed exit0, empty stdout/stderr and exact baseline artifact hashes.
No profiling flags were set. Candidate/control each contain 4,324,912 bytes;
size is unchanged. Default release opt-level3, LTO, one codegen unit, panic abort,
stripped symbols, locked dependencies and no feature overrides.

Original SHA256:
`9f207b431f6a7ecf034da9db0f76de55a857890a3335d3cb7ace525dc0db90a0`.
Candidate SHA256:
`ac927281c9c4038c573cccba10ffd40f6ad9432553a09b2df9329c51efb67877`.
Control was preserved at Step11 before any diagnostic build; candidate source
diff/build identities are embedded with the driver and raw rows in
`opforge-step12-rust-comparison-evidence-2026-09-05.json`.

B01 candidate range34.029208–39.142000ms and control138.048250–140.585041ms:
relative ranges13.3841% and1.8076%. The plan threshold is max(5%, both relative
ranges), therefore13.3841%, and the72.7808% gain passes. Noise exceeding5% does
not itself fail the policy. B10 candidate58.360084–59.350583ms and
control160.903250–163.840667ms: relative ranges1.6928%/1.8082%, threshold5%;
63.9820% passes. No extra pairs are needed by the bounded protocol.

Retain subject to the mandatory remaining gate and final independent review.
No owner-bucket gain substitutes for this measured integrated B10 improvement.
This is a limited engineering threshold, not a statistical confidence interval.
Memory allocation volume was not separately quantified: the fallback Vec and
its construction temporaries are avoided on this path; no memory ratio is claimed.

## Correctness and feature coverage

The new focused runtime-model test supplies a valid cwd package and a known CPU
with bundled fallback available, then tries missing and malformed explicit files.
Both fail without modifying the valid artifact. Existing valid-explicit and
fallback tests remain. Default runtime-model filter:7passed/0failed. The source
budget test passed with unchanged native capacity/snapshots. Its first exact
filter selected0tests and is explicitly non-proof; the corrected run selected1.

Additional focused feature runs both completed exit0:
`vm-runtime-only` and combined
`vm-runtime-only,vm-runtime-opasm-unbundled,vm-runtime-opasm-artifact`.
These cover the remaining cfg arms and artifact precedence without a broader
feature matrix. Formatting and source diff checks pass. Full non-LSP Rust gate completed
exit0: 1,589 assembler tests passed, plus all remaining non-LSP checks.

## Immediate native transfer disposition

No direct native counterpart: in
`native/motorola68000/amigaos/opforge-cli/package_pipeline.asm`,
`opforgeNativeCliStagePackage` checks the external path first and branches to
file staging. Only the no-explicit branch copies embedded `.incbin` bytes.
It does not construct a package from a registry and discard it. Loading/copying
or validation changes would be separate native candidates requiring their own
attribution and semantic proof. No speculative port is inserted; proceed to
Step13/Item16a's planned native statement-arena audit after this focused commit.

## Gate and review receipt

Initial independent source review PASS for the narrow helper and failure test,
with the two focused feature runs above. Mandatory non-LSP Rust and workflow gates PASS. Final independent
plan-compliance and commit receipt follow in the plan sidecar. Native failure debt remains
owned by A-close; all LSP work remains Step25.

Completed focused/workflow log identities (explicit exit0 unless noted):

- `step12-runtime-model-tests-20260905.log`: 1416 bytes, SHA256 `2491359ac80d1d826ae55d18455e6a4ce2dd31e9de1ad67e2680230dfac48ca7`.
- `step12-source-budget-20260905.log`: 370 bytes, SHA256 `261f9ffcff535c7bf710640e480b17416523428852b627241022447648a23aa7`; zero selected tests, non-proof.
- `step12-source-budget-r2-20260905.log`: 551 bytes, SHA256 `c50b738a4f8f31ade5f98f0b991b3d971135ef0f88e63b116ea3dbbddcfccb37`.
- `step12-runtime-only-20260905.log`: 3101 bytes, SHA256 `4b789cb3cb20b20164a105dc659fdc9fd161734a335fef3b47fc0acca300d85f`.
- `step12-unbundled-artifact-20260905.log`: 2859 bytes, SHA256 `fc26e7f6110f3643a24d231588360988cfd2f988d0229cc2e0b2bda6dc1fb69a`.
- `step12-workflow-20260905.log`: 6363 bytes, SHA256 `530dd4d219776c977b58a9fe9669c1d2ca5f1ed34a26e2d87c5446e7294e6496`.

Final non-LSP Rust gate: explicit exit0, 1969.961467s whole command;
1,589 assembler tests passed in1885.26s. Log `step12-rust-non-lsp-20260905.log`,
345756 bytes, SHA256 `c23842d805e8e0f8b7f4d295511e1ac559b822f219f06995e141e373e3764677`.
LSP clippy/tests remain deferred, not full-workspace qualification.
