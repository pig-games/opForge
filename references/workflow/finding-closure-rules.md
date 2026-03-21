# Finding Closure Rules

Every closure report must identify:

- the original finding ID
- the original finding summary
- the implementation slice or commit claiming closure
- the validation evidence used
- the closure status

Allowed closure statuses:

- `fixed`
- `partially fixed`
- `not fixed`
- `superseded`
- `deferred`

Closure guidance:

- use `fixed` only when the original failure mode no longer reproduces, or an
  equivalent targeted validation clearly proves the issue is gone
- use `partially fixed` when only part of the original risk is addressed
- use `not fixed` when the original issue still exists
- use `superseded` when the original finding is replaced by a newer, more exact
  finding
- use `deferred` when the issue is intentionally left open

