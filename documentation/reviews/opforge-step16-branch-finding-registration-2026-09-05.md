<!-- workflow-provenance: skill=opforge-review-reporting; entrypoint=new_artifact_from_template.sh -->

# Review Report: Step16 historical branch finding registration

## Scope

Register the existing Step09 ledger finding **E0-1** as
**RVW-2026-09-05-001** for deterministic closure traceability. These identifiers
refer to the same finding. This document normalizes the existing source audit
and evidence; it does not claim a new triple-model code review.

Source: `documentation/performance/results/opforge-step09-early-failure-ledger-2026-09-05.md`.
The original finding and failed receipts remain historical records. Step16's
correction report and evidence JSON identify the exact reproduction and four
completed native confirmations. Independent `branch_boundary_audit` established
the source boundary; `plan_review` approved its focused E0-1 closure.

## Version Impact

- Affected component(s): Native package-selected relative operand encoding.
- Impact class: none
- Owned contract: Review finding identity and evidence traceability.
- Rationale: This registration changes no production behavior or version. The Step16 repair and its required gates are documented separately.

## Findings

### RVW-2026-09-05-001

- Severity: high
- File: native/motorola68000/amigaos/tkpkg/tkpkg_operand_runtime.asm:135
- Issue: Historical E0-1: later-pass rel8 selection discarded a resolved forward target solely because its defining row had not yet refreshed its stability flag. Four completed branch cases consequently failed with OPC-NCLI026 after successful pass one.
- Why it matters: Valid native inputs could not assemble, preventing correctness qualification of the performance work. Rust computes the relative displacement from the available target rather than rejecting that separate stability flag.
- Fix direction: Remove only the later-pass instability rejection; retain pass-one placeholder behavior, expression errors, displacement/range checks and output encoding. This direction is implemented in Step16 and independently approved for E0-1 closure.

## Testing Gaps

The four original cases pass fresh challenge-bound native completion, explicit
zero exit and byte-for-byte in-memory Rust oracle comparison. The full non-LSP
Rust gate passes 1,591 assembler tests and the remaining included packages;
staged native and workflow gates also pass. This finding closure does not prove
all relative ranges or close the original whole native groups.

## Residual Risks

B10 timeouts, aggregate failures, the wrong negative diagnostic and all other
Phase A qualification debt remain open. LSP remains explicitly deferred. No
runtime speedup is measured or claimed for this correctness repair.

## Brief Summary

E0-1 and RVW-2026-09-05-001 are explicit aliases for the same historical finding.
The associated Step16 closure is supported by the existing independent review
and exact case evidence; this registration adds no implementation scope.
