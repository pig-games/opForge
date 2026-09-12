# Reference and golden refreshes

Use this guide when regenerating expected outputs. Establish why the intended
behavior changes before updating its expected artifacts; do not use regeneration
to hide a regression or turn implementation output into an unexamined oracle.

Regenerate affected fixtures with explicit paths. The optional
[refresh helper](../../scripts/workflow/update_references.sh) takes allowed paths,
then `--` and the actual generation command. It sets `OPFORGE_UPDATE_REFERENCE=1`
and checks the resulting tracked reference diff. Review that diff and any new or
deleted fixtures yourself; the helper is not a complete correctness check.

Preserve pre-existing edits. Investigate unexpected changes before including them;
discuss materially broader refreshes when they exceed the agreed scope. Validate
the affected behavior and examples with the regenerated expectations. No separate
approval artifact or allowlist document is required: explicit command arguments
and the reviewed diff provide the scope.
