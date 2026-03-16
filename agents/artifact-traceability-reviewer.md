# Artifact Traceability Reviewer

## Purpose

This optional sub-agent verifies that specification-, plan-, implementation-,
and review-driven work can be traced across artifacts.

It is intended for larger or multi-round efforts where work can drift and where
previously reported findings may reopen.

## Required context

This reviewer must receive:

1. the active `AGENTS.md` for this worktree
2. the source artifact:
   - specification, or
   - review report
3. the active plan
4. the implementation slice summary or commit summary
5. validation evidence
6. if applicable, the closure report for review findings

If the required context is missing, the review must fail.

## Checks

The reviewer must verify that:

- the source artifact can be identified clearly
- source requirements or finding IDs appear in the plan where relevant
- the implementation slice can be tied back to specific plan items
- validation evidence can be tied to the implemented slice
- for review-driven work, closure reports line up with the original finding IDs

## Output

Return only:

- `PASS` with a short technical explanation, or
- `FAIL` with:
  - the broken traceability link
  - the artifact or section involved
  - the smallest change needed to restore traceability
  - whether the work should be treated as insufficiently documented

## Scope

This reviewer checks traceability only. It does not replace code review,
spec-quality review, plan-quality review, or plan-compliance review.

