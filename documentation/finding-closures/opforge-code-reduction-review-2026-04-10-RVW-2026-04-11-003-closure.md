# Finding Closure Report

## Finding

- ID: `RVW-2026-04-11-003`
- Original summary: The package codec repeated the same hand-written encode/decode pattern across many chunk families, manually mirroring count handling, owner encoding, ordered field serialization, decode reconstruction, and result-vector pushes.

## Claimed Fix

- Plan item: Work item 12 - migrate tokenizer and contract package chunk codecs onto the shared schema layer.
- Implementation slice or commit: pre-commit Work item 12 remediation slice on `main`, completing the helper seam introduced by Work item 11.
- Changed files:
  - `crates/opforge-package/src/package/codec.rs`
  - `crates/opforge-package/src/package/codec/scoped_schema.rs`
  - `crates/opforge-package/src/package/tests.rs`

## Validation Evidence

- Command or check: `cargo test -p package encode_decode_round_trip_contract_schema_ -- --nocapture`
- Result: PASS; the focused contract-schema suite validated schema-driven round trips for `TokenizerVmProgramDescriptor`, `ParserContractDescriptor`, `ParserVmProgramDescriptor`, `ExprContractDescriptor`, and `ExprParserContractDescriptor`.
- Command or check: `cargo test -p package decode_ -- --nocapture`
- Result: PASS; the existing decode regression suite stayed green while exercising legacy compatibility, malformed payloads, bounded-count rejection, and contract validation paths.
- Command or check: `cargo fmt --all`
- Result: PASS.
- Command or check: `cargo clippy --all-targets --all-features -- -D warnings`
- Result: PASS.
- Command or check: `cargo audit`
- Result: PASS with the existing allowed warnings for `registry` unmaintained (`RUSTSEC-2025-0026`) and `rand` via `proptest` (`RUSTSEC-2026-0097`).
- Command or check: `cargo test --workspace`
- Result: PASS; the full workspace unit, integration, and doc-test suites completed without failures.

## Closure Status

- Status: `fixed`
- Residual risk: low; all review-targeted package chunk families now declare field order once through the shared schema helper, and focused round-trip plus decode-regression tests cover both the new generic seam and the retained validation behavior.

## Notes

- Work item 11 introduced the shared `scoped_schema` helper for `REGS`, `FORM`, `TABL`, and `MSEL`; this slice extends the same seam to `TKVM`, `PARS`, `PRVM`, `EXPR`, and `EXPP` rather than adding a second codec abstraction.
- `codec.rs` now routes every review-targeted chunk family through one schema-driven encode/decode path, while descriptor-specific validation remains attached through per-entry decode hooks for the expression contract families.
- The new `encode_decode_round_trip_contract_schema_` tests lock the remaining families onto the shared schema path so future field-order changes have one source of truth instead of mirrored manual loops.