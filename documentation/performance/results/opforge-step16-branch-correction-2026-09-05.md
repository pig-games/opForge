# Step16 / Item A-branch: resolved forward targets

Active AGENTS.md remains binding. Source: Step16 of the active performance plan.
Workflow: `skills/opforge-plan-authoring/SKILL.md`, `run_plan_workflow.sh`.
The accompanying `opforge-step16-branch-evidence-2026-09-05.json` indexes exact
run identities, receipts and logs. Required code gates passed; E0-1 closure review passed; final compliance and commit remain pending.

## Corrected boundary

The generic package-selected `rel8` operand path rejected an available target
in pass two solely because the symbol's defining statement had not refreshed
its stability flag. This is distinct from an expression evaluation failure.
Rust's `execution_model/selector_encoding.rs::encode_expr_relative` evaluates
the target and calculates `target - (current + instruction_length)`; it does
not reject a resolved later-pass value for that flag.

Native `tkpkg_operand_runtime.asm::tryBranchOffset8` already handles pass one
with a placeholder. The removed later-pass instability check sent a valid
value to `tryUnstablePassOneOperand`, which returns NO_OUTPUT in pass two.
The selection service converts that to an empty candidate, and the assembly
driver reports OPC-NCLI026. The resolver's newer forward-label stability
reporting exposed this older rejection path. Source/blame evidence supports
that interaction; this slice did not run a historical checkout bisection.

The production change removes that extra rejection. Expression evaluation
errors, pass-one behavior, current-PC adjustment, signed displacement limits
and candidate-byte construction are unchanged. No CPU-specific logic, public
ABI, package semantics, instrumentation or general selector refactoring is added.

## Reproduction and confirmation

The single unchanged reproduction used the original schema case
`examples/mos6502/6502_first_run_artifact_contract.asm`, its original command,
embedded default package, CPU68020/max/JIT0 configuration and300s post-start
bound. The fresh guest completed with exit1: pass one succeeded, then pass two
rejected `beq done` with OPC-NCLI026. The host test exited101 in103.440821s.
This is failure evidence, not native parity or a timing baseline.

After the correction, one attempt-all confirmation passed all four original
completed branch cases with their original source/command/CPU/package inputs
and live in-memory Rust oracles:

- `examples/mos6502/6502_first_run_artifact_contract.asm`
- `examples/mos6502/65c02_simple.asm`
- `examples/mos6502/65c02_allmodes.asm`
- `source-cpu-package-aliases`

The existing runner requires fresh challenge-bound start/completion, explicit
zero guest exit and exact artifact equality, and removes per-case guest files.
The confirmation host process exited0 in730.070850s; the test reports729.99s.
These are four-case validation costs, not product-performance measurements.
The original aggregate tests remain intact; their unrelated timeouts and full
51-group requalification remain owned by A-close.

## Proof levels and limits

- Level A: the harness creates each actual source's fresh Rust oracle before
  any possible environment-based native skip. The host-only invocation proves
  Rust expectations, not native behavior; the recorded confirmation then
  executed all four cases at Level D.
- Source boundary audit: independent Rust/native path and blame inspection
  identifies the rejection and verifies unchanged evaluation/range paths. This
  is source evidence, not a native instruction simulator or runtime proof.
- Level D: the four completed confirmation cases prove exact native artifacts
  for those inputs. They do not prove B10 completion, every branch range, other
  instruction plans or the whole schema/source-CPU aggregate.
- Formatter, ownership and proof-contract checks passed. The first full
  non-LSP Rust gate exited 101 after 1,590 tests passed and one obsolete rel8
  source-contract assertion failed; the corrected focused rel8 contract now
  passes both tests. `tests.rs` replaces the obsolete instability requirement
  with eval-success coverage and a contract forbidding later-pass instability
  rejection. The corrected full-gate rerun passed with all 1,591 assembler tests. No
  observation instrumentation was needed or added.

The corrected staged-native contract gate and workflow gate both completed
with exit 0. The staged-native receipt covers the 23 canonical native
contracts, FS-UAE invocation policy, architecture/no-growth, evidence
classification, test ownership, proof contract, and 238-file formatter check.
The workflow receipt additionally records 134 workflow tests and all quality,
reference-scope, and release-note policy checks passing. The corrected full
non-LSP Rust rerun completed with exit 0: 1,591 assembler tests passed; LSP
clippy/tests remain deferred by the gate mode. Its stable receipt is indexed
in the evidence JSON. The assembler suite took 1,819.78s; the complete gate took
1,889.188706s.

## Hypothesis ledger

| ID | Hypothesis | Evidence | Limit | Status | Next discriminator |
|---|---|---|---|---|---|
| E0-1 | Later-pass resolved forward target is discarded for instability | Exact reproduced BEQ failure, source rejection path, four corrected native cases pass | Whole aggregate and Phase A remain open | fixed (independent closure PASS) | Final focused commit |
| A-branch-PC | Placement/current-PC mismatch causes this failure | Considered during source audit | Request, placement and current-PC paths are coherent; exact cases pass with only instability check removed | falsified | No further probe for this slice |
| E0-3 | Branch rejection explains B10 timeout | No direct evidence | Timeout-only cases have no branch diagnostic; no current B10 run here | open | Separate Step17 bounded localization |

Performance contribution: restores correctness needed to qualify the earlier
performance gains. No runtime speedup is measured or claimed for this repair.

The focused public-export snapshot initially failed because a removed label
was counted in the public inventory. Restoring that label preserves the original
6,575 declarations and128,293 name bytes while retaining the four-instruction
removal. Two host builds prove the restored-label image is byte-identical to
the confirmed source variant. An initial host comparison failed because its
backup had an assembler source extension and entered module discovery; renaming
that backup excluded it, and both corrected builds passed. No guest retry was
used for this label-only correction. The focused budget command selected21
tests and exited0; its unavailable FS-UAE cases skipped. The host source, import
and export checks provide the budget proof, not those skipped selections.

#### Progress log

- Production code changed: removed the erroneous later-pass `rel8` instability rejection.
- Behavior now implemented: the four recorded forward-branch cases complete with exact Rust artifacts.
- Validation status: reproduction preserved; all four Level D confirmations and focused structural checks pass; non-LSP Rust and staged native/workflow gates pass; E0-1 closure review passes; final compliance pending.
- Unresolved issue: other Phase A failures, B10 and final qualification remain open.
- Next concrete implementation step: complete final review, then commit this invariant repair before Step17.
