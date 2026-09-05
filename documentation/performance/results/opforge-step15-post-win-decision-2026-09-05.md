# Step15 / Item A-triage: post-win decision

Active AGENTS.md remains binding. Source: the active performance plan and user
request to realize inexpensive gains early while retaining final certainty.
Workflow: `skills/opforge-plan-authoring/SKILL.md`, `run_plan_workflow.sh`.
This is a decision artifact; it changes no production behavior.

## Decision and ordering

First resolve the completed branch rejections (Step16 / A-branch), then perform
one bounded current-B10 localization (Step17 / A-b10-localize). A-close moves to
Step18 and depends on both dispositions plus every resulting repair. LSP-close
remains last, now Step27. This ordering separates confirmed correctness failures
from timeout-only evidence and prevents a speculative broad rerun.

The concrete first source is `examples/mos6502/6502_first_run_artifact_contract.asm`
in the schema binary parity route, which completed pass one and rejected
`beq done` in pass two. Other retained completed failures include `bra skip` in
`65c02_simple.asm`, `bra skip1` in `65c02_allmodes.asm`, and the source-CPU
normalization group. The Rust reference is
`crates/opforge-vm/src/execution_model/encoding_bridge.rs`; initial native
inspection boundaries are `tkpkg_selection_service.asm` and
`opforge-cli/encode_eval_bridge.asm`. Neither a selector defect nor a link to B10
is proven yet. The plan limits reproduction/observation and requires the exact
original case identity, not a superficially similar fixture.

B10 still cannot complete at its unchanged 120s bound. Historical no-I/O
observations reached frontend around 100.7s, with 20,837/20,985 VM opcodes and no
engine statement visits. This establishes progress and a broad owner, not an
elapsed-time share or stationary loop. After the branch decision commit, one
current run using validated existing counters will distinguish ingestion from
statement/session work or explicitly identify an insufficient evidence boundary.
Any resulting implementation receives a separate reviewed item before A-close.

## Performance and remaining evidence

| Work | Recorded result | Limit / consequence |
|---|---|---|
| Step10 native session clear | B01 case median improved 40.62%; whole-test cost 24.00% lower | Same cleared range; B10 remained incomplete |
| Step12 Rust explicit-package path | B01 72.7808% and B10 63.9820% lower host case medians | Rust measurements; no direct native counterpart or native B10 claim |
| Step13 native live statement initialization | B01 17.2050% improvement; capacity513 and early-error proof pass | All B03/B10 attempts remained incomplete |
| Step14 native module buffering | Complete independent module oracle 23.4621% faster; reads16,541 to8 with same16,538 bytes | Four of five candidate timing attempts completed; one timeout and both B10 timeouts remain open |
| Step14 validation | Non-LSP Rust PASS: 1,590 assembler tests in1,855.83s | Suite cost, not product runtime; whole-gate wall duration unavailable |

These percentages have different baselines and workloads and must not be added
or multiplied into a claimed total product gain. B08 still lacks strict stdout
parity; B09 retains its unresolved-label failure. Historical bytes-only B04/B06/
B07 evidence still needs strict current proof. The 13 failed native groups,
29 historical timeout events, four completed branch rejections and wrong TRAP
negative diagnostic remain assigned to A-close. The six-byte initialization
range discrepancy also remains explicitly owned. No failure is deleted by this
reordering. Step14's unexpected full-counter failure on valid input remains
unresolved; a partial or faulty observer cannot authorize B10 performance claims.

## Developer-loop candidate and payback

Read-only inspection found repeated native host assembly in Rust tests:
`native_platform_profile_harness_and_cli_assemble` runs five define compositions
for two roots (ten assembly calls), including five full-CLI assemblies.
Independent full-CLI tests also exist. Different defines are different semantic
inputs, so this count alone does not establish reusable results. The shared test
helper creates a fresh assembly; no safe cross-test reuse or timing benefit has
been demonstrated. Preserve all live Rust oracles and per-test artifact checks.

Host reuse is deferred rather than implemented speculatively. A later bounded
measurement can group exact root/defines/input identities and time preparation
before selecting an immutable cache boundary. Expected saved seconds and remaining
invocations are currently unknown, so numeric payback cannot be calculated.
The branch repair directly unblocks correctness qualification; it has no measured
speedup. B10 observation is limited to one run plus existing build/startup cost,
not a promise to save120s. Re-rank only from new evidence after focused commits.

## Source receipts

The following committed artifacts preserve raw provenance and proof limits.

- `documentation/performance/results/opforge-step09-early-failure-ledger-2026-09-05.md`: 10141 bytes; SHA256 `52dae0ae4af7060b91726b8014a3106bcd9ad0f8658486fd4692cffc20ba6a92`.

- `documentation/performance/results/opforge-native-item0f-attribution-decision-2026-09-04.md`: 16336 bytes; SHA256 `ddb4b13efae8fba996ac81cee4506e435486cc32ebb9125aedd74776ea6f1075`.

- `documentation/performance/results/opforge-step10-session-clear-2026-09-05.md`: 11311 bytes; SHA256 `d76fd56186f7e3a7f2126d68d32af2a8f00410612a4e67f984b209aa4ec9741f`.

- `documentation/performance/results/opforge-step12-explicit-package-performance-2026-09-05.md`: 6805 bytes; SHA256 `a5ef7f6cef607d8319680a0db884a77cafc401f231731ce2c1983c8eff07042e`.

- `documentation/performance/results/opforge-step13-native-statement-performance-2026-09-05.md`: 6907 bytes; SHA256 `7a4d72611a97e189a98c6c006891087dd7af0844f39b707dea3b3e2f10ddf29b`.

- `documentation/performance/results/opforge-step14-native-module-comparison-2026-09-05.json`: 12958 bytes; SHA256 `ee9f884cd23feb53d679eaece44c442fcc4d420c63a5d8833c78e4601d81b352`.

- `documentation/performance/results/opforge-corpus-v1-native-status-2026-09-04.md`: 4100 bytes; SHA256 `39e9dcc7b668f91e50759960fa8a6795c4ae5f71f92c48254c155951761521e2`.

## Validation receipts

- `/private/tmp/opforge-step15-plan.log`: explicit exit0; 1768 bytes; SHA256 `9e5154f88956afd61987dba01a9320cdc7bfa400af18d25302b125509f223fe1`.
- `/private/tmp/opforge-step15-workflow.log`: explicit exit0; 6363 bytes; SHA256 `9b45861a3bad928de7dd71e7a1f6d49a1f7ae10b26d1980c1525df26736892e6`.

#### Progress log

- Production code changed: none; this slice changes ordering and evidence records.
- Behavior now implemented: separately numbered branch repair and one-run B10 localization precede unchanged A-close requirements.
- Validation status: source evidence, unique contiguous Step01–27, plan wrapper and workflow gate pass; independent compliance pending.
- Unresolved issue: branch cause, B10 cause, other non-LSP failures and host-reuse payback remain unproven.
- Next concrete implementation step: after this focused decision commit, compare the exact branch case at the Rust/native request-selection boundary.
