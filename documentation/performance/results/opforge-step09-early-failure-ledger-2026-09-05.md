# Step 09 / Item E0: early optimization correctness boundaries

Active AGENTS.md remains binding. This classification changes no production code
and fixes no native defect. Source: Step 09 of the active performance plan;
workflow: `skills/opforge-plan-authoring/SKILL.md`, `run_plan_workflow.sh`.

## Decision

Proceed to Step 10 / Item 17c after this focused commit. Its scope is exactly the
existing 41,221,928-byte initialization range at the single `clearBytes` call in
`initSessionV1`. Fresh B01 now supplies an unaffected complete CLI oracle. Branch,
negative-diagnostic and module/linker failures remain open and may not supply
successful timing controls. Before an optimization uses an affected case, insert
and prove a separate semantic repair or choose another complete exact oracle.

The retained native failure log already contains completed exact-source failures.
Repeating them now would not establish whether they were introduced by older
changes and is unnecessary for the branch-free B01 experiment. No native files
changed between `8fd49290` and `47009f6c`; this only rules out a native-code change
in that interval, not a harness regression or an earlier defect. Baseline versus
introduced status remains unresolved for every retained failure.

## Hypothesis ledger

| ID | Hypothesis | Evidence for | Evidence against / limit | Status | Next discriminator and owner |
|---|---|---|---|---|---|
| E0-1 | Pass-2 request/selection mismatch causes branch rejection | Four completed BEQ/BRA rejections after pass one | Earliest divergent request is not captured; not relevant to nine-NOP B01 | open | A-triage inserts a focused semantic repair before any affected optimization; A-close requalifies |
| E0-2 | Negative scalar diagnostic path diverges | TRAP #16 completes with exit 1 but malformed-table text | Correct Rust diagnostic is known; malformed text does not identify the causal boundary | open | Compare package/selector/encoder result before renderer; separate repair then A-close |
| E0-3 | Timeout cases are dominated by startup work | Exact-input samples locate a large pending clear | No completed time share; 29 deadlines do not prove a stall or common cause | open | 17c matched B01 control/candidate; A-triage localizes remaining failures; A-close owns all |
| E0-4 | Declared session range is six bytes shorter than emitted arena | Header constant 92 versus emitted 98; total 41,221,928 versus 41,221,934 | Observable impact is not proven | confirmed | 17c preserves this exact range; 16a audits reuse/poison invariants; any correction needs its own reviewed item |
| E0-5 | The short range causes stale output presence | Last six bytes belong to image-presence tail | Both pass-entry paths clear the entire presence map before pass work | open | Focused reset/reuse proof in 16a; A-triage must resolve or explicitly classify impact before A-close |
| E0-6 | B01 can gate same-range startup clearing | Current strict adapter completed exact Rust parity | Single run does not establish a performance baseline or broad correctness | confirmed | 17c primitive memory/ABI proof, matched completed controls/candidates and capacity/early-error confirmation |

## Retained native failures and ownership

Source: `opforge-item0f-closure-gates-2026-09-05.md` and
`target/workflow-logs/native-phase-zero-20260904.log`, SHA-256
`137cab6a8f91a6e4e8a66e2a4603e2311e16964bf374fb78c34a8d350d5975e7`.
All 13 failed groups remain open. A-triage (Step 15) selects separate bounded
repairs; A-close (Step 16) owns complete requalification. The branch and negative
diagnostic rows additionally belong to E0-1 and E0-2 above. Timeout-only rows
belong to E0-3; no speedup is assumed to fix them.

| Failed group | Historical group seconds | Failure classification |
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

Totals: 29 timeout events, four completed branch rejections and one completed
wrong diagnostic; 38/51 groups passed. Group seconds are gate cost, not product
timing. Failed negative diagnostic: exact source `        TRAP #16\n`, Rust
requires `TRAP vector 16 out of range (0-15)`; native produced
`OTR901: encode table malformed` and `OPC-NCLI020`. Existing one-case reproducer:
`external_fs_uae_native_m68000_trap_value_diagnostic_parity`.
Representative branch: `examples/mos6502/6502_first_run_artifact_contract.asm`,
`beq done`, rejected in pass two with `OPC-NCLI026`, `No instruction found for
BEQ`, then `OPC-NCLI020`. The single-source PRG test exists but uses the Item 13
route; it is not identical to the failing schema aggregate's case identity.

## Corpus ownership

Historical source: `opforge-corpus-v1-native-status-2026-09-04.md`. Only B01 is
updated by this slice; later Level E diagnostic snapshots do not promote any row.

| Case | Status | Next owner / proof |
|---|---|---|
| B01 | Fresh strict parity PASS in this slice | 17c matched measurement; A-close |
| B02 | Timeout-only | A-triage; A-close completion |
| B03 | Timeout-only | 16a may investigate; A-triage/A-close; no causal claim |
| B04 | Historical exact BIN, strict repeat outstanding | A-close |
| B05 | Timeout-only | A-triage; A-close |
| B06 | Historical exact BIN, strict repeat outstanding | A-close |
| B07 | Historical exact BIN, strict repeat outstanding | A-close |
| B08 | Exact BIN/exit 0 but nonempty stdout violates strict contract | 14m must first obtain a complete module oracle or insert separate repair; A-triage/A-close |
| B09 | Completed exit 1, unresolved label OPC-NCLI022 then OPC-NCLI020 | Separate repair selected by A-triage; A-close exact eleven-artifact proof |
| B10 | Timeout-only; later snapshots remain Level E | Bounded 17c attempt, 16a/14m evidence input; A-triage/A-close completion rule |

LSP 34 passed / 14 failed remains unclassified under final Step 25 / LSP-close.
It blocks no earlier phase. Non-LSP qualification remains mandatory.

## Exact initialization boundary

`native/motorola68000/amigaos/opasm/opasm_engine.asm` declares a 92-byte header
but emits 98 bytes. The independently checked map in
`opforge-native-observer-controls-2026-09-04.md` places session start at 408,024,
end at 41,629,958. The omitted six bytes are the end of
`OpasmEngineImagePresentBuffer`. `opasmEngineBeginPassOneV1` and
`opasmEngineBeginPassTwoV1` call `clearImagePresentV1`, which clears the full
1 MiB map. This static source audit limits the suspected impact; it does not
prove every lifecycle or poisoned-session invariant. 17c must preserve the
existing byte count and ABI; the discrepancy is not silently repaired there.

## Focused evidence

Run at code revision `47009f6c`, clean before launch; counters off, CPU 68020,
`uae_cpu_speed=max`, `jit_compiler=0`, 120,000ms post-start deadline.

```sh
OPFORGE_PERFORMANCE_CORPUS=1 OPFORGE_NATIVE_CORPUS_CASES=B01 \
OPFORGE_FS_UAE_SMOKE=1 \
OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' \
OPFORGE_FS_UAE_CONFIG_TEMPLATE=/tmp/opforge-performance.fs-uae \
OPFORGE_FS_UAE_ARGS='{fsuae_config}' \
OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=120000 RUST_TEST_THREADS=1 \
cargo test -p asm external_fs_uae_native_production_corpus_parity -- --nocapture --test-threads=1
```

Generate the disposable config with `production_corpus.py fs-uae-config` using
the repository-documented FS-UAE template before the command. The exact rendered
config is recorded in the transcript.

Result: host exit 0; one passed, none failed; 127.46s whole test duration.
`CORPUS_RESULT` records complete=true, guest exit=0, exact `Work/output.bin`,
profile=off. The case uses `.org $0800` and nine NOPs, with no forward symbols,
module discovery, negative diagnostic or linker directives. The runner generates
the exact source/command/package Rust oracle afresh, checks strict streams and
fresh case-bound start/completion, and removes the guest case tree before return.
This is Level D exact CLI parity for B01, backed by the live Level A Rust oracle.
It proves neither primitive ABI/guard-byte behavior nor broader native parity,
real-hardware speed, repeated performance gain or a fix for any failed case.
The whole test duration includes build/boot/teardown and is not assembly timing.

Transcript: `target/workflow-logs/step09-b01-20260905.log`, 1579 bytes,
SHA-256 `d319d89afff34c4656a1861b1789c1326815cdc2cb2996d53e15c0e634b4f458`.
Case SHA-256: `114ecc1e524a209c1756dd4cfec5cbcc010cbe483f17097bc5eefa8b4d45057d`.
Package SHA-256: `46a56a5bd436b012c596c65d1f7d85fe6cd8fadbd702362955804415e00c0d41`.
Corpus SHA-256: `fece2121b487b37e1217b4854b74308366399938e26520e06d124ed63559aed9`.

Workflow and independent compliance results are recorded in the plan sidecar
before the focused commit. No additional Rust gate is needed for this docs-only
slice; Step 08's non-LSP Rust PASS is historical qualification of unchanged code.
