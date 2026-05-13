# Reference and Golden Artifact Refresh Rule Pack

Load this only when updating reference or golden artifacts.

- Update only the minimum references or goldens directly affected by the current slice.
- Do not run broad refresh commands unless the user explicitly asked for a full
  refresh or an evidence artifact explicitly approves a bulk refresh.
- Refresh evidence must include an allowlist naming the exact artifacts expected
  to change.
- Prefer `scripts/workflow/update_references.sh` with explicit paths over ad hoc
  broad regeneration commands.
- If unexpected references change, stop and report the mismatch before staging.
