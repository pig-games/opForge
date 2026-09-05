# Step 10 / Item 17c: same-range native session clearing

Status: implementation, focused host proof, three matched B01 pairs and native
capacity/error confirmations complete. B10 remains timeout-only in both builds;
Rust and staged workflow gates have passed; final compliance/commit records follow in the plan sidecar. This is not an acceptance or Phase A closure receipt.
Active AGENTS.md remains binding. Source: the active performance plan Item 17c.

## Performance contribution

The old session clear performs 41,221,928 byte-store/decrement/branch iterations.
At the current four-byte-aligned arena start, the candidate performs 10,305,482
longword-store/decrement/branch iterations, writing the same number of bytes.
This is a 75% reduction in inner-loop iterations, not a measured 75% runtime
reduction. Completed alternating B01 trials decide retention; B10 gets the
unchanged fixed-budget control/candidate attempt. Faster startup is expected to
benefit both the shipped native CLI and subsequent native validation runs.

## Boundary and implementation

Rust reference: zero-filled session semantics and the case's fresh exact public
CLI oracle. Native boundary: `initSessionV1` calls private `clearBytes` at one
site in `native/motorola68000/amigaos/opasm/opasm_engine.asm`.

The default path clears initial alignment bytes, then longwords using a 32-bit
count, then remaining bytes. `OPFORGE_SESSION_CLEAR_BYTE_REFERENCE` retains the
old loop. Both paths return D0=0 and advance A1 by the input length; other
registers and stack balance are preserved. CCR returns Z=1, N/V/C=0, with X
unchanged for zero length and cleared for nonzero length. Existing optional
platform-counter calls remain at the same boundaries; no guest instrumentation
was added. The existing six-byte arena-count discrepancy remains explicitly
unfixed under the Step 09 ledger; this slice still clears exactly 41,221,928 bytes.

## Focused proof and its limits

- Level C: `python3 -m unittest scripts/performance/tests/test_session_clear.py -v`
  passed six source-driven tests for both builds: zero/X, four alignments,
  small lengths/tails, >65,535 bytes, the full session length, sentinel memory,
  D0/A1 outputs, preserved registers, stack and CCR. The model executes actual
  selected helper instructions, rejects unsupported instructions, and only
  accelerates exact large CLR/SUBQ/BNE self-loops. Expected memory comes from an
  independent slice-fill oracle. This does not prove native encoding or timing.
- Level B: completed-timing receipt tests reject missing timing, failed cases
  and multi-case batch durations. The fixture inventory check includes the
  byte-reference flag; it does not select a different source or command.
- Level B: exact native source/import budget checks passed after refreshing
  measured source-size snapshots. Capacities are unchanged. These do not prove
  native execution or startup speed.
- Native formatter checked 238 files, none needing changes. Architecture and
  fail-closed native proof-contract guards passed.
- Independent implementation review by `plan_review` found no correctness or
  measurement-validity blocker; final plan-compliance depends on remaining runs.

## Measurement contract

Use frozen B01/B10, ordinary real CLI, explicit current package and fresh live
Rust artifacts, counters off, actual CPU 68020, speed=max, JIT=0 and the unchanged
120,000ms post-start deadline. `OPFORGE_NATIVE_CORPUS_CLEAR=byte` selects the
reference; `longword` selects the candidate. B01 order is candidate/reference,
reference/candidate, candidate/reference. Each run uses a fresh case-bound guest
protocol, zero exit, exact artifacts and empty streams before a successful row.

The host coordinator observes case START to DONE with monotonic time and the
unchanged 250ms polling interval. This excludes build and boot, includes the
case's launch/CLI/output work, and has polling/scheduling uncertainty. Missing
start timing produces no measurement, not zero. Only one-case completed runs
receive a duration; whole-test cost is recorded separately. Native executable
identity uses the repository's explicitly labeled FNV-1a digest, not a security
hash or parity oracle. Raw transcript SHA-256 identities will be recorded with
final results. Guest trees remain ephemeral; reports never supply a new oracle.

Retention requires at least three complete alternating matched pairs and median
improvement greater than max(5%, the larger relative range). No B10 speed claim
is allowed for an incomplete control or candidate. All results remain provisional
until A-close's full non-LSP/native and B01–B10 requirements pass.

## Completed B01 results

Six fresh runs all passed exact artifacts, strict empty streams, completed guest
protocol and explicit zero exit. Within each mode, case/package/command/profile,
poll boundary and native executable identity matched.

| Build | START-to-DONE seconds | Median | Whole-test median |
|---|---|---:|---:|
| byte | 75.341048, 75.563603, 75.568435 | 75.563603s | 129.255052s |
| longword | 44.869376, 44.678805, 44.874342 | 44.869376s | 98.234943s |

Median runtime improvement is **40.62%**, saving **30.69s**
per B01 invocation at this boundary. The larger relative range is 0.436%;
the required threshold is therefore 5%, which this result exceeds. Whole-test
median cost, including native build/boot/teardown, fell from
129.26s to 98.23s
(24.00% less time). The measured developer-loop benefit is thus smaller than
the runtime percentage but still saves roughly half a minute per focused run.

Exact rows and all six raw test-log SHA-256 identities are in
`opforge-step10-b01-clear-trials-2026-09-05.json`. These receipts cannot supply a
future Rust oracle. On this bounded input, 100 comparable future invocations
would save about 51.2 minutes at the case boundary; the invocation count
is a planning assumption, not a measured workload forecast. Broader product
performance and B10 remain unqualified.

Independent measurement review by `plan_review` recomputed all medians, ranges,
thresholds and savings and matched every one of the six transcript hashes. It
confirmed stable per-build native identity and equal case/package/configuration
across all runs. Result: PASS for this bounded B01 performance claim; final
Step 10 qualification remains pending.

## Bounded B10 attempt

Both the byte reference and longword candidate hit the unchanged 120,000ms
post-start deadline. Neither produced DONE, a guest exit or completed artifacts;
partial stdout/stderr were empty. Both host tests exited 101. This is timeout-only
evidence, not a measured B10 speedup, non-regression result or new semantic error.
The scheduled comparison is complete and will not be repeated in this slice.
B01 retention remains provisional under the plan; A-close still requires B10
completion and exact parity.

| Build | Whole-test cost | Transcript SHA-256 |
|---|---:|---|
| byte | 172.40s | `50eb2301a78a93714356b39cdd4287e432fea7d48e2d5c6965dede156fd85ad6` |
| longword | 173.01s | `6257fabe6a2e0d5cc238254c7c01133b4520d559692eb2bc4947434c6d5011ab` |

Logs: `target/workflow-logs/step10-b10-byte-20260905.log` and
`target/workflow-logs/step10-b10-longword-20260905.log`. Both guest trees were
verified removed. Whole-test durations are gate costs, not assembly timings.

## Early-error confirmation

`external_fs_uae_session_clear_early_error_preserves_diagnostic` passed in
92.86s whole-test time, host exit 0. Its completed fresh negative guest case
returned explicit exit 1 and the required unknown-mnemonic diagnostic after
session initialization. This is focused Level D error-path confirmation, not
a timing result or proof of all diagnostics. Transcript SHA-256:
`f6b731e77f7a85bc4ff650ca228bd8985ba18c4e05f950a765e2b055f8d9a5a8`
(`target/workflow-logs/step10-early-error-20260905.log`).

## Capacity confirmation

`native_statement_capacity_over_512_fs_uae` passed, host exit 0, in 127.99s
whole-test time. It crossed the former 512-row boundary with 513 exact Rust/native
bytes, fresh completed protocol and explicit guest exit 0. This is focused Level D
packed-session confirmation; it does not prove maximum capacity or a speedup on
this input. Transcript SHA-256:
`ca13d29a3d551c7d5df726b018de21102376b98e5404e2b6060748800620ec84`
(`target/workflow-logs/step10-capacity-20260905.log`).

## Staged workflow gate

The first staged workflow gate failed on two bookkeeping mismatches: the engine
source digest and the newly added error test ownership entry. After independently
checking that all 101 routines, four imports, code/BSS sections and 35 diagnostic
paths were unchanged, the digest was refreshed. The new test was appended with a
post-split annotation; every historical name/order remains unchanged. The source
change itself was not altered, and the original failed log is retained.

The second staged workflow gate passed with explicit host exit 0: 134 workflow
tests, 238 native formatter files, native slice/instrumentation/proof/ownership
and architecture checks. Independent gate-hygiene review by `plan_review` passed
these updates and the schema-2 slice metadata. No runtime gain is attributed to
this bookkeeping; it supplies auditable qualification for the B01 result.

| Workflow attempt | Result | Transcript bytes | SHA-256 |
|---|---|---:|---|
| `step10-staged-workflow-20260905.log` | FAIL, bookkeeping | 3992 | `6969dd0b3e9021c7441214498cfc68290d766fc4e1cd92bf0305a7a34957840f` |
| `step10-staged-workflow-r2-20260905.log` | PASS | 6912 | `f0986190476f666a99b41ac54205f3abecbfa60c5f493933411b9d04f69b4855` |

The mandatory non-LSP Rust gate subsequently passed, as recorded below. Final
Step 10 compliance and commit are recorded in the plan sidecar; no full-product
or Phase A qualification is claimed by this report.

## Rust qualification and disposition

`bash scripts/workflow/run_rust_quality_gate.sh --defer-lsp` returned explicit
host exit 0 after 1,982.50s whole-gate time. All 1,588 assembler tests passed in
1,902.81s, as did the other non-LSP packages, clippy, formatter, architecture,
audit and C/FFI checks. LSP remains deferred, so this is not full-workspace
qualification. The gate did not launch new native confirmations; native-named
tests in this run supply host/oracle evidence only.

Transcript: `target/workflow-logs/step10-rust-non-lsp-20260905.log`,
343923 bytes, SHA-256 `b119e007ae1704aaac85d6d12e506a90ffe17d511bd30c7bdb93118577ccbbfe`.

Retain the longword clear provisionally: three matched complete B01 pairs exceed
the conservative threshold, host memory/ABI and native capacity/error contracts
pass, and the required Rust/workflow gates pass. B10 remains explicitly incomplete
in both builds and all earlier native debt remains assigned to A-close. The byte
reference switch is retained for later comparisons and rollback. No additional
B10 sampling or unbounded trial was used to obtain this decision.

Measured performance contribution: 40.62% lower B01 case time and 24.00% lower
whole focused-test median cost. The expensive 33-minute Rust qualification is
reported as development cost, not product throughput; it does not diminish or
expand the bounded measured speedup. Next, Step 11 / R0 investigates one Rust
cost before another broad native change, preserving the balanced plan.
