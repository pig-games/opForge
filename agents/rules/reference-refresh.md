# Reference and golden refreshes

Regenerate only the affected fixtures with explicit paths; prefer
`scripts/workflow/update_references.sh`. Review the generated diff against the
intended behavior. Investigate unexpected changes before including them.
Do not use a blanket refresh to hide a regression. Discuss materially broader
refreshes when they exceed the agreed scope. No separate approval artifact or
allowlist document is required.
