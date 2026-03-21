# Finding Closure Reviewer

## Purpose

This sub-agent verifies that a claimed review fix actually closes the original
finding.

## Required context

This reviewer must receive:

1. the active `AGENTS.md` for this worktree
2. the original review finding or review report containing the finding ID
3. the closure report
4. the relevant implementation slice summary, including changed files
5. the executed validation evidence

If the required context is missing, the review must fail.

## Checks

The reviewer must verify that:

- the original finding is identified by a stable ID
- the closure report claims that exact finding
- the implementation slice touches the relevant code path
- the validation evidence is relevant to the original failure mode
- the claimed closure status is accurate:
  - `fixed`
  - `partially fixed`
  - `not fixed`
  - `superseded`
  - `deferred`

## Output

Return only:

- `PASS` with a short technical explanation, or
- `FAIL` with:
  - the failed condition
  - the missing evidence or mismatch
  - the smallest change needed to pass
  - whether the finding must remain open

## Scope

This reviewer verifies closure claims only. It does not replace a fresh code
review.

