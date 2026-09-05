# Step20 — native include output isolation

Step20 / A-include-stdout restores quiet normal include processing while preserving explicit native debug reporting. The production change adds the established `NativeCliDebugEnabled` guard before the existing include-line report call in `line_processor::invocationPass`. The public report routine, parser path and binary output contract remain intact.

Performance contribution: normal execution avoids formatting and writing the unsolicited include record. No timing percentage or DOS-call reduction is claimed for this repair; Step19’s measured source-buffer gains remain a separate result.

## Evidence and limitations

- Level B source-order contract passes. It proves gate ordering in source, not machine execution.
- Fresh Level D normal refill/include fixture passes with exact live Rust binary `[17, 34]`, empty stdout/stderr, challenge completion and explicit zero guest exit. It also preserves the source refill, CRLF and suspended-parent coverage.
- Fresh Level D explicit debug include passes with exact live Rust binary `[17]`, exactly one `INCLUDE-LINE 1 1` line among established debug output, empty stderr, challenge completion and explicit zero guest exit.
- Frozen B08 passes under the unchanged 120-second bound with exact Rust-authoritative artifacts and empty stdout/stderr. This resolves the reproducible B08 output failure; broader Phase A and any formal finding closure remain separate.
- The required non-LSP Rust gate passed in 261.2439 seconds, including the corrected source-budget contract. LSP remains explicitly deferred. Workflow and native staged checks also pass. Independent plan-compliance review by `step20_review` passed with no findings.
- Native staged checks and formatter pass. No new instrumentation or public ABI change is introduced.
- The two added instructions increase exact source snapshots by 49 bytes and two processed rows. The initial budget test exposed stale row/byte snapshots; they were updated to the measured 92,034 rows and 3,524,289 processed bytes. Hard capacity limits remain unchanged. The original failing log is retained.

## Hypothesis ledger

| ID | Hypothesis | Evidence | Status | Next discriminator |
|---|---|---|---|---|
| S20-1 | Normal include reporting causes the stream mismatch. | Step19 observed `INCLUDE-LINE 1 1`; the sole call lacked the debug gate; fresh normal fixture now has empty streams and exact bytes. | confirmed | Completed B08 and explicit debug confirmation both pass. |

No Phase A closure is claimed. B10 and other outstanding native failure debt remain governed by Step21 / A-close. Historical B08 stream text was not retained and is not reconstructed here. Raw process receipts and log hashes are indexed in the accompanying JSON after validation completes.
