# Native Parity Failure Triage Rule Pack

Load this rule pack when a native parity test fails or an FS-UAE run returns
unexpected behavior. Load `agents/rules/native-rust-parity-porting.md` for the
boundary workflow and `agents/rules/fs-uae.md` before running FS-UAE.

## Hypothesis ledger

Maintain a ledger for the active slice:

| ID | Hypothesis | Evidence for | Evidence against | Status | Next discriminator |
|---|---|---|---|---|---|

Allowed statuses are `open`, `confirmed`, `fixed`, `falsified`,
`invalid test artifact`, `instrumentation artifact`, and `blocked`.

Prefer the smallest discriminator at the earliest unproven boundary. Do not
accumulate speculative changes across boundaries.

## Reduced fixtures

Before relying on a prefix scan or reduced fixture, answer:

```text
Does the reduced fixture preserve all symbols needed by pass 2?
Does it end on a label-only line?
Does it omit later definitions used by earlier forward references?
Does pass-2 behavior mean the same thing as the full fixture?
Is this proof-level E only?
```

A reduced fixture is Level E unless its semantic completeness is explicitly
documented. A truncated fixture that changes pass-2 meaning cannot prove parity.

## Fix claims

Use this form:

```text
Claimed fixed invariant:
Previous failing evidence:
Production change:
Minimal proving test:
Proof level:
Result:
Remaining failure:
Why the remaining failure is distinct:
Instrumentation removed/kept:
```

Moving the failure frontier is useful localization evidence, but it is not a fix
claim. A claim requires a named invariant and focused proof at that boundary.
