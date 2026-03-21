# Plan Modes Guide

Plans in this workflow system share one structure but may have different modes.

## Supported modes

### `implementation`

Use when the source is:

- a specification
- a clear user-requested feature slice

Typical traceability:

- source requirements -> plan items -> implementation slices -> validation

### `remediation`

Use when the source is:

- a review report
- a known bug list
- a regression report

Additional requirements:

- each relevant plan item should list the review finding IDs it addresses
- closure work should follow implementation and use a closure report

### `migration`

Use when work is primarily about renames, structural transitions, or replacing
old paths with new ones while preserving behavior.

Additional requirements:

- identify the before/after contract explicitly
- call out compatibility expectations
- include migration validation, not only compile success

### `cleanup-only`

Use only when explicitly approved.

This mode is for:

- restructuring
- consistency cleanup
- maintenance work

It should not be used to hide implementation drift.

Additional requirements:

- the reason for cleanup must be explicit
- the done criteria must explain why the cleanup is worth doing now

## Mode selection rule

If multiple modes could apply, choose the mode that best describes the primary
driver of the work. Do not mix several modes in one plan unless the work is
small and the source relationships are still obvious.

