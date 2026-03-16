# Artifact Traceability Guide

Traceability exists to make it possible to answer:

- why is this work item here?
- what requirement or finding does it address?
- what commit or slice implemented it?
- what validation proved it?
- if it came from review, is the finding actually closed?

## Recommended chains

### Spec-driven work

- spec requirement
- plan item
- implementation slice or commit
- validation evidence

### Review-driven work

- review finding ID
- remediation plan item
- implementation slice or commit
- validation evidence
- closure report

## Minimum traceability fields

### In plans

- source
- mode
- source requirement or finding IDs per relevant item

### In implementation updates

- current slice
- changed files
- validation run

### In closure reports

- finding ID
- implementation slice or commit
- validation evidence
- closure status

## When to use the traceability reviewer

The optional reviewer is most useful when:

- the work is large
- several agents contribute over time
- earlier review issues have reappeared in later rounds
- the plan and implementation are drifting apart

