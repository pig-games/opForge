<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->

# Step14 native module buffer performance evidence report

This report records a workload-scoped measured performance result for the
Step14 native module-buffer change; it is not a broad final qualification. The
machine-readable collection is
[`opforge-step14-native-module-comparison-2026-09-05.json`](opforge-step14-native-module-comparison-2026-09-05.json).

## Recorded result

The final six runs passed the fresh FS-UAE protocol, exited zero, and matched
the exact Rust-authoritative output `[34]`: four observer-off timing runs and
two platform-counter runs. The four complete matched pairs (1, 2, 4, and 5)
have byte-mode median START-to-DONE time **65.255084583
s** and buffered-mode median **49.944847938 s**, an observed improvement of
**23.4621359%**. Pair 3 buffered remains a required timeout failure at the
120-second post-start bound and is retained in the evidence. The timing result
is a measured performance result for this workload.

The original comparison’s first byte and buffered rows are retained as invalid
merged-stream harness checks. Its original module-boundaries run passed all
four cases. Both original B10 rows reached the 120-second bound and failed as
timeouts. The corrected all-counter byte and buffered runs used the same valid
exact module oracle. Both completed the fresh guest protocol with exit 1 and
diagnostic `OPC-NCLI027`; the shared failure is attributed to instrumentation
composition, with provenance before HEAD unknown. These are failure evidence,
not timing rows.

## Mechanism and bounded discriminator

The platform profile completed for both modes with exact output and zero guest
exit. The module source payload was **16,538 bytes** in both runs. Byte mode
performed **16,541** module reads; buffered mode performed **8** reads, with
the same **3 module candidates**. The bounded run’s mechanism validation passed
with no counter or identity errors. The platform records are summarized in the
aggregate JSON; the raw records remain in the per-run receipts.

The Level C host helper model ran **7 tests**. It proves buffer refill, EOF,
CRLF, reset, and fail-closed helper behavior, but is not DOS or parser proof.
The Level D FS-UAE cases are the authoritative exact Rust-output checks; each
successful timing case produced output `[34]`, fresh completion, and guest exit
0.

## Identities and receipts

The measured native source identity in the comparison, corrected, and bounded
receipts is SHA-256
`b19587ad3a3427d015876813bde87c2bd58990b5378d5f0e75f6ba1b64cdc2aa`. The
comment-only current-source identity is separately recorded as
`1fccdf6168ff4d68ea660fc551018349be977e02c7de8d981239cffbbb8d40ea`; it must
not replace the measured identity. The shared FS-UAE config identity is
`937a5756808116a2ce5a135b7b7cc64e2278529846fc5862e993812a29052ba6`.
Measured byte and buffered images were stable within their respective matched
pairs (`fnv1a64:27592e80cb8a3113` and `fnv1a64:8f4d610d968907cf`). The
platform probes intentionally have distinct image digests because they include
platform instrumentation.

The final host helper proof passed all seven tests. Root’s comment-only binary
equivalence check passed with exit 0 and byte-identical before/after artifacts,
as recorded in its receipt. The full non-LSP Rust quality gate passed with explicit exit 0: **1,590 assembler tests in 1,855.83s**, plus the other required checks. Reviewer plan review is **GO** for
provisional retention; final commit and review remain pending.

## Provisional disposition

Retain the independently measured module result provisionally: four matched
pair comparisons pass, the bounded pair 4/5 buffered completions and platform
discriminator pass, and exact Level D output is established for eight successful timing runs across four matched
pairs plus two platform-counter runs. Preserve all
failure evidence: the two invalid initial merged-stream harness assertions,
the original four-case boundary pass, both B10 120-second timeouts, corrected
pair 3 buffered timeout, and both all-counter guest-exit-1 `OPC-NCLI027` cases.
Final review, and commit are still required before
final qualification.

## Noise and acceptance threshold

Completed matched byte runs span 64.891421084–65.345264667s (range/median
0.6954915%); buffered runs span 49.806161375–50.093854791s (0.5760222%).
Selected noise is 0.6954915%, so the acceptance threshold is max(5%, noise)
= 5%. The observed 23.4621% improvement exceeds it. This calculation includes
only the four complete pairs; the fifth buffered attempt remains a timeout,
not a timing sample. The aggregate records the CPU/JIT/timer contract and
explicit paths, hashes and sizes for all three drivers and summary indexes.
The assembler suite took 1,855.83s; Rust and workflow gates passed, but their
whole-run wall-clock durations were not recorded. No duration is inferred from polling.

#### Progress log

- Production code changed: module candidate scanning uses an 8KiB refill buffer, with the byte-reader reference retained.
- Behavior now implemented: candidate parsing and exact output remain unchanged while module DOS reads fall from 16,541 to 8 in the measured fixture.
- Validation status: seven focused host tests, complete focused Level D cases, byte-identical comment rebuild and workflow gate pass; full non-LSP Rust gate passed (1,590 assembler tests). The aggregate indexes the workflow exit-0 log and its hash/size.
- Unresolved issue: one buffered timeout, both B10 timeouts and shared full-counter failures remain recorded; final compliance/commit remain pending.
- Next concrete implementation step: finish final review, then commit Step14 before Step15.
