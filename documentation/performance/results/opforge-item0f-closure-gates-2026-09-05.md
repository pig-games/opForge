# Item 0f closure-gate receipt

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


This records the completed gates; it is not a passing closure receipt. Active
`AGENTS.md` remains binding. Neither failure set is yet classified as pre-existing
or introduced by Item 0f.

## Native Phase 0 gate

Command mode: `run_native_existing_parity_completion.sh --verify-phase-zero`.
The run used FS-UAE 3.1.66 with the reviewed 68020/max-speed configuration and
attempted all 51 selected nonterminal groups. The two named terminal groups—full
product artifact assembly and two-generation self-hosting—were not run and are
not counted as passed.

Final result: **FAIL — 38 groups passed and 13 failed**. The sum of the 51
per-group test durations is 49,852.97 seconds (13.85 hours). This is gate cost,
not an opForge assembly benchmark or an A6000 calibration.

| Failed group | Group seconds | Rejected case summary |
|---|---:|---|
| `external_fs_uae_opforge_native_cli_schema_binary_parity_matches_live_rust_cli` | 1,461.64 | One 300s timeout; three explicit-exit pass-2 `BEQ`/`BRA` selector failures |
| `external_fs_uae_opforge_native_cli_source_cpu_normalization_matches_live_rust_cli` | 909.75 | One explicit-exit pass-2 `BRA` selector failure |
| `native_opcore_scopes_fs_uae` | 455.76 | One 300s timeout |
| `native_macro_preprocessor_harness_fs_uae_proves_capture_lookup_and_nested_frame_rejection` | 326.30 | One 300s timeout |
| `external_fs_uae_native_m68000_move_control_parity` | 1,200.32 | One 300s timeout |
| `external_fs_uae_native_m68000_remaining_base_parity` | 4,619.06 | Two 300s timeouts; one completed negative case returned the wrong diagnostic |
| `external_fs_uae_native_m68020_later_integer_group_b_parity` | 2,175.77 | One 300s timeout |
| `external_fs_uae_native_m68881_m68882_core_parity` | 3,670.96 | Four 300s timeouts |
| `external_fs_uae_native_m68881_m68882_extended_math_parity` | 1,820.25 | Four 300s timeouts |
| `external_fs_uae_native_m68040_integrated_fpu_parity` | 1,004.27 | One 300s timeout |
| `external_fs_uae_native_m68080_integer_parity` | 2,635.49 | Two 300s timeouts |
| `external_fs_uae_native_m68080_ammx_parity` | 2,761.44 | Two 300s timeouts |
| `external_fs_uae_native_motorola68000_complete_reference_parity` | 9,377.18 | Nine 300s timeouts |

Across the failed groups, the transcript contains 29 exact 300,000ms timeout
events, four explicit guest-exit branch-selection failures, and one completed
negative case whose diagnostic did not match its required oracle. A timeout is
not a native stall diagnosis, and a failed group does not independently promote
its other cases to parity success. The wrapper's final result confirms that all
selected groups were attempted. All FS-UAE processes exited and no
`target/fs-uae-*` run tree remained after completion.

Authoritative local transcript:
`target/workflow-logs/native-phase-zero-20260904.log`, 1,007 lines / 61,476
bytes, SHA-256
`137cab6a8f91a6e4e8a66e2a4603e2311e16964bf374fb78c34a8d350d5975e7`.

## Rust quality gate

Final result: **FAIL**. The `asm` crate's 1,586-test suite passed after the four
focused static-contract expectation corrections, as did the other reported
workspace suites before the final failure. The `opforge-lsp` client integration
suite then reported 34 passed and 14 failed. The failures are request-response
and diagnostic-wait failures; this receipt does not infer whether they are a
shared infrastructure problem, pre-existing behavior, or an Item 0f regression.

Authoritative local transcript: `target/workflow-logs/rust-quality-gate.log`,
3,122 lines / 279,675 bytes, SHA-256
`059e1f74a472044d23b599fe5c1ff442e5ff929ca6c2c44ee539f0d92b13c353`.

## Decision

Both required closure gates completed and failed. Item 0f therefore remains
unchecked and uncommitted, and Item 3 must not begin. The bounded B10 profiling
loop remains stopped: these correctness failures do not justify more identical
performance sampling. The next work is focused failure classification, beginning
with a clean reproduction that distinguishes pre-existing failures from changes
in this item.
