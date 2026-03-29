# opForge Motorola 68000 Family Non-MMU Residual Risks Follow-Up Spec (v0.1)

## Summary
This specification defines the follow-up behavioral contract for four
non-MMU residual risks left open by the 2026-03-29 68000-family remediation
review. It exists to drive a later implementation plan without reopening the
already-closed primary findings.

The scoped residual risks are:
- long divide support on `m68020`, `m68030`, and `m68040`
- `MOVES` effective-address legality validation
- `CAS2` register-field legality validation
- multi-pass branch-size selection safety after the branch displacement fix

This document explicitly excludes the `68030`/`68040` PMMU instruction-set gap.
The active worktree `AGENTS.md` workflow and execution rules remain binding for
any plan or implementation derived from this specification.

## Problem
The current 68000-family remediation plan closes the review's nine primary
findings, but the source review still identifies four non-MMU residual risks
that remain underspecified for future work.

Without an explicit follow-up specification, later implementation work would
have to infer intended behavior from scattered review notes and partially
similar existing handlers. That creates drift risk in three ways.

First, `DIVS.L` and `DIVU.L` on `m68020+` are missing for the same family-vs-CPU
override reason that originally blocked `CHK.L`, but the exact delegation and
diagnostic contract is not yet written down.

Second, two existing encoder paths accept architecturally invalid register or
effective-address combinations without deterministic rejection. The current code
can silently accept source text that should fail assembly, which is worse than
an unimplemented instruction because it can produce incorrect machine code.

Third, the branch encoder's multi-pass width selection behavior still lacks a
source contract describing how unresolved pass-1 placeholders may influence
final instruction size. Without that contract, future fixes could preserve the
displacement math correction while still leaving wrong-size branch decisions
possible in multi-pass assembly.

## Goals
- [ ] `REQ-M68K-RESIDUAL-001`: Define the legal `DIVS.L` and `DIVU.L` surface on
      `m68020`, `m68030`, and `m68040`, including the requirement that baseline
      family handlers defer `.L` forms so later CPU handlers can own legality
      and encoding.
- [ ] `REQ-M68K-RESIDUAL-002`: Require `MOVES` to reject architecturally invalid
      PC-relative effective addresses with deterministic diagnostics on all CPUs
      that otherwise support `MOVES`.
- [ ] `REQ-M68K-RESIDUAL-003`: Require `CAS2` to accept only address registers in
      the architecturally defined `Rn` fields and reject data registers in that
      position with deterministic diagnostics.
- [ ] `REQ-M68K-RESIDUAL-004`: Define a safe multi-pass branch-size selection
      contract so unresolved pass-1 branch targets cannot silently lock the
      final encoding to an undersized form.
- [ ] `REQ-M68K-RESIDUAL-005`: Provide acceptance criteria and validation
      expectations specific enough to drive a follow-up implementation plan
      without hidden scope expansion.

## Non-Goals
- [ ] `NREQ-M68K-RESIDUAL-001`: Implement the residual-risk fixes directly in
      this specification.
- [ ] `NREQ-M68K-RESIDUAL-002`: Cover the `68030`/`68040` PMMU instruction-set
      gap (`PMOVE`, `PLOAD`, `PSTORE`, `PFLUSHA`, `PFLUSHN`, `PTEST`, and
      related surfaces).
- [ ] `NREQ-M68K-RESIDUAL-003`: Redesign the 68000-family parser, assembler
      pass scheduler, registry model, or diagnostics framework beyond what is
      required to close the scoped residual risks.
- [ ] `NREQ-M68K-RESIDUAL-004`: Expand the 68000-family CPU surface beyond the
      already-adopted `m68000`, `m68010`, `m68020`, `m68030`, and `m68040`
      targets.
- [ ] `NREQ-M68K-RESIDUAL-005`: Reopen or re-specify the nine primary findings
      already covered by the 2026-03-29 remediation plan except where their
      shipped behavior constrains the residual-risk fixes.

## Invariants / Constraints
- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times for downstream plan and implementation work.
- The family id remains `motorola68000`, and the baseline `m68000` behavior
  stays authoritative unless a later CPU is explicitly selected.
- `m68000` and `m68010` must not accept `DIVS.L` or `DIVU.L`.
- `m68020`, `m68030`, and `m68040` may widen legality only for architecturally
  valid long-divide forms.
- Parse availability does not imply encode legality. The family layer may parse
  forms that a selected CPU or instruction encoder must later reject.
- Architecturally invalid source forms must fail deterministically and must not
  silently assemble into a different legal encoding.
- Follow-up work for the branch residual risk must preserve the already-fixed
  displacement rule that branch displacements are based on `PC = instr_addr + 2`
  for `.B`, `.W`, and `.L` forms.
- This specification must not be used to justify PMMU/MMU implementation work;
  the `68030`/`68040` PMMU gap remains explicitly deferred.

## Behavioral Contract

### 1. Long divide on `m68020+`
- `DIVS.L` and `DIVU.L` are in scope only for `m68020`, `m68030`, and `m68040`.
- The family-common divide path must defer `.L` forms with a non-fatal
  delegation result so later CPU handlers can decide legality and encoding.
- `m68020` owns the canonical long-divide encode path for later CPUs, and
  `m68030`/`m68040` inherit that behavior unless they need a documented CPU-local
  restriction.
- `m68000` and `m68010` must reject long-divide forms with CPU-level diagnostics
  that truthfully describe the selected CPU as unsupported for that form.
- Long-divide support for follow-up implementation is bounded by this matrix:

| Source form | Status | CPU scope | Notes |
| --- | --- | --- | --- |
| `DIVS.L <ea>,Dq` | Canonical | `m68020+` only | Signed long divide, single quotient destination |
| `DIVU.L <ea>,Dq` | Canonical | `m68020+` only | Unsigned long divide, single quotient destination |
| `DIVS.L <ea>,Dr:Dq` | Canonical | `m68020+` only | Signed long divide, remainder plus quotient |
| `DIVU.L <ea>,Dr:Dq` | Canonical | `m68020+` only | Unsigned long divide, remainder plus quotient |
| `DIVSL.L <ea>,Dr:Dq` | Narrow accepted alias | `m68020+` only | Must assemble identically to `DIVS.L <ea>,Dr:Dq` |
| `DIVUL.L <ea>,Dr:Dq` | Narrow accepted alias | `m68020+` only | Must assemble identically to `DIVU.L <ea>,Dr:Dq` |

- `.L` is mandatory for every long-divide form in scope here. Size omission does
  not imply long divide and continues to select the already-shipped word-divide
  behavior where applicable.
- The accepted positive source forms for follow-up implementation are:
  - `DIVS.L <ea>,Dq`
  - `DIVS.L <ea>,Dr:Dq`
  - `DIVSL.L <ea>,Dr:Dq`
  - `DIVU.L <ea>,Dq`
  - `DIVU.L <ea>,Dr:Dq`
  - `DIVUL.L <ea>,Dr:Dq`
- In all accepted forms, `<ea>` must use an architecturally legal divide source
  effective address for the selected CPU, using the same divide-source
  effective-address legality contract already applied to baseline `DIVS` and
  `DIVU` on that CPU. `Dq` is the quotient destination, and `Dr` is the
  remainder destination when a register pair is present.
- In register-pair forms, `Dr` and `Dq` must both be data registers and must be
  distinct. Pair forms that reuse the same data register for both roles are out
  of scope for this follow-up and must fail deterministically.
- The follow-up implementation must reject malformed long-divide layouts
  deterministically, including:
  - missing explicit `.L`
  - missing destination register operands
  - register-pair layouts that do not use data registers
  - register-pair layouts where `Dr == Dq`
  - operand counts or operand kinds that do not match one of the six accepted
    source forms above

### 2. `MOVES` effective-address legality
- `MOVES` remains legal only on CPUs that already support the instruction.
- `MOVES` must reject PC-relative source or destination effective addresses even
  if those modes pass broader family-level "memory alterable" screening.
- The rejection must occur before bytes are emitted and must identify the
  offending operand span when available.
- The diagnostic must make clear that the specific effective address is invalid
  for `MOVES`, rather than implying that the mnemonic itself is unknown.
- Legal baseline-addressing `MOVES` forms on `m68010` and legal later-family
  forms on `m68020+` must remain accepted.

### 3. `CAS2` register-field legality
- `CAS2` remains in scope only on `m68020`, `m68030`, and `m68040`.
- The two `Rn` fields encoded in the extension words must accept address
  registers only.
- Data registers, special registers, and any non-register operand in either
  `Rn` position must be rejected deterministically before encoding completes.
- The diagnostic must identify that the rejected operand violates `CAS2`'s
  address-register requirement, not a generic parse failure.
- Existing legal `CAS2` operand forms using address registers must remain
  accepted unchanged.

### 4. Multi-pass branch-size selection safety
- When a branch target is unresolved in an early pass, the assembler must not
  permanently commit the instruction to a size that may become too small once
  the target address is known.
- If the current implementation uses placeholders during pass 1, the final pass
  must re-evaluate branch size using resolved addresses before the encoding is
  considered complete.
- A branch whose resolved displacement does not fit the initially chosen short
  form must be widened deterministically to a legal form or fail with a precise
  out-of-range diagnostic if no legal form exists.
- The multi-pass contract must preserve explicit size suffix semantics:
  - explicit `.B` never widens silently
  - explicit `.W` and `.L` retain their declared size and validate range
  - auto-sized branches may widen when the resolved target requires it
- The final emitted displacement must continue to use `target - (instr_addr + 2)`
  regardless of whether the branch size was selected in one pass or multiple
  passes.

## Boundary Cases
- `DIVS.L`/`DIVU.L` on `m68000` or `m68010` must remain rejected even if the
  shared family parser recognizes the mnemonic and operand syntax.
- `DIVS.L`/`DIVU.L` on `m68020+` must not be blocked by a hard baseline-family
  error before the CPU handler can inspect the instruction.
- `DIVSL.L (A0),D2:D3` and `DIVUL.L (A0),D2:D3` must remain accepted aliases for
  the canonical pair forms and must assemble identically to
  `DIVS.L (A0),D2:D3` and `DIVU.L (A0),D2:D3`.
- `DIVS.L (A0),D2:D2` and `DIVU.L (A0),D2:D2` must fail deterministically
  because the pair form requires distinct remainder and quotient registers.
- `MOVES` with `(LABEL,PC)` or any other PC-relative effective address must be
  rejected even if the operand is otherwise memory-shaped.
- `MOVES` with legal non-PC-relative memory operands must remain accepted.
- `CAS2` where one `Rn` field is an address register and the other is a data
  register must fail; partial validity is not sufficient.
- `CAS2` diagnostics must remain stable regardless of whether the invalid `Rn`
  appears in the first or second memory operand.
- Auto-sized branches with unresolved pass-1 targets must not emit a final short
  branch when the resolved displacement requires word or long range.
- Explicit `.B` branches with resolved out-of-range targets must fail with a
  deterministic range diagnostic rather than silently widening.

## Acceptance Criteria
- [ ] `AC-M68K-RESIDUAL-001`: Representative `DIVS.L <ea>,Dq` and
      `DIVU.L <ea>,Dq` forms such as `DIVS.L (A0),D1` and `DIVU.L (A0),D1`
      assemble on `m68020`, and the same encodings remain available on
      `m68030` and `m68040`.
- [ ] `AC-M68K-RESIDUAL-002`: Representative register-pair long-divide forms
      such as `DIVS.L (A0),D2:D3` and `DIVU.L (A0),D2:D3` assemble on `m68020`;
      `DIVSL.L (A0),D2:D3` and `DIVUL.L (A0),D2:D3` assemble identically as
      narrow aliases; malformed pair layouts such as `DIVS.L (A0),D2:D2` fail
      with deterministic operand diagnostics.
- [ ] `AC-M68K-RESIDUAL-003`: `DIVS.L` and `DIVU.L` remain rejected on `m68000`
      and `m68010` with CPU-level diagnostics instead of hard family-layer
      baseline errors that block later CPU overrides, and forms without an
      explicit `.L` continue to use the pre-existing word-divide path rather
      than silently selecting the long-divide behavior.
- [ ] `AC-M68K-RESIDUAL-004`: `MOVES` rejects representative PC-relative source
      and destination effective addresses with deterministic diagnostics, while
      representative legal non-PC-relative `MOVES` forms still assemble on both
      `m68010` and `m68020+`.
- [ ] `AC-M68K-RESIDUAL-005`: `CAS2.W` and `CAS2.L` accept representative legal
      `(An):(Am)` memory-pair operands, and reject a representative mixed
      `(Dn):(Am)` or `(An):(Dm)` form with a diagnostic that states the
      address-register requirement.
- [ ] `AC-M68K-RESIDUAL-006`: An auto-sized branch whose target is unresolved on
      an early pass is re-evaluated after symbol resolution and does not finish
      as a short branch when the resolved displacement requires `.W` or `.L`.
- [ ] `AC-M68K-RESIDUAL-007`: Explicit `.B` branches remain hard range-checked,
      and final emitted branch displacements continue to equal
      `target - (instr_addr + 2)`.
- [ ] `AC-M68K-RESIDUAL-008`: The spec explicitly excludes the
      `68030`/`68040` PMMU instruction-set gap from scope and leaves it for a
      separate spec and planning effort.

## Validation Expectations
- The spec artifact must pass the branch-local spec quality gate.
- The follow-up implementation plan derived from this spec should include at
  least one focused positive and one focused negative validation item for each
  scoped residual risk.
- Long-divide validation should prove:
  - positive encoding for `DIVS.L (A0),D1` and `DIVU.L (A0),D1` on `m68020`
  - positive encoding for `DIVS.L (A0),D2:D3` and `DIVU.L (A0),D2:D3` on
    `m68020`
  - alias parity proving `DIVSL.L (A0),D2:D3` matches `DIVS.L (A0),D2:D3`, and
    `DIVUL.L (A0),D2:D3` matches `DIVU.L (A0),D2:D3`
  - carry-forward behavior on `m68030` and `m68040`
  - CPU-level rejection on `m68000` and `m68010`
  - deterministic rejection of `DIVS.L (A0),D2:D2` or `DIVU.L (A0),D2:D2`
  - confirmation that omitted-size forms stay on the existing word-divide path
- `MOVES` validation should prove:
  - rejection of at least one PC-relative source or destination form
  - continued success of at least one legal `MOVES` form on `m68010`
  - continued success of at least one legal `MOVES` form on `m68020+`
- `CAS2` validation should prove:
  - rejection when either `Rn` field is a data register
  - continued success of a legal address-register form
- Branch multi-pass validation should prove:
  - an auto-sized unresolved branch widens or retries correctly once the target
    resolves
  - explicit `.B` remains a hard range-checked contract
  - final displacement bytes still match `target - (instr_addr + 2)`

## Open Questions
None. This specification is intentionally bounded to the four non-MMU residual
risks already identified by the 2026-03-29 review and explicitly defers the
separate PMMU/MMU surface.
