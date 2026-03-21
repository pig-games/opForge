# Plan Slice Rules

All executable plans should follow these rules:

- one active work item at a time
- each work item is commit-sized
- each work item or phase ends in a new commit
- each work item has full quality-gate validation
- each work item requires `plan-compliance-reviewer` before commit
- each work item has checkbox tracking
- no item is marked done before validation is green
- no next item starts if the current item is blocked and the plan was not updated

For remediation plans:

- list the review finding IDs addressed by each relevant work item
- state whether each item is expected to fully or partially close those findings
