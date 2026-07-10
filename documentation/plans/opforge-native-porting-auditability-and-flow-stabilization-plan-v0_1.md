<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# opForge Native Porting Auditability and Flow Stabilization Plan v0.1

## Metadata

- Source: User request on 2026-07-10 to realize the three remediation proposals in the “Repo Porting Overview” discussion: fail-closed Level D retention, complete slice contracts, and flow-control ownership stabilization.
- Mode: remediation
- Owner: Codex
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.
- Validation status: Pending plan-quality gate. This artifact is authored with `skills/opforge-plan-authoring/SKILL.md` through `scripts/workflow/run_plan_workflow.sh`.

## Goal

Make the native Rust-to-68020 parity programme mechanically auditable while
preserving the existing architecture boundary: Rust remains the authority, the
native CLI remains the Level D execution path, and no CPU-specific semantics
are added to generic Rust/native layers.

This remediation has three outcomes:

1. a fail-closed, retained Level D completion record for the completed native
   expansion slices (Items 5.1–5.6 of the reference-parity plan);
2. a complete, versioned slice-metadata contract enforced by the deterministic
   validator; and
3. an ownership-only decomposition of `opasmDriverApplyFlowControl` before
   additional native semantics are added there.

## Version Impact

- Affected component(s): native parity completion workflow, workflow validators and tests, `documentation/plans/slices/*.toml`, and native opasm flow-control modules.
- Impact class: none
- Owned contract: a native parity slice records its exact fast and Level D proof commands, expected inputs/outputs, known non-equivalences, and retained completion evidence; native flow routes through owned modules without changing statement-index, stack, register, CCR, or output behavior.
- Rationale: current slice claims are well structured but do not yet retain fail-closed Level D receipts as a standard gate, metadata omits execution-critical details, and one dispatcher owns growing independent semantic domains.

## Constraints

- This is a remediation plan, not a mandate to expand native assembler feature coverage. It covers only workflow evidence, slice contracts, and ownership-preserving flow decomposition.
- Do not alter Rust examples, reference fixtures, or reference code. In particular, `.org` must never be stripped on either side.
- Native Level D proof must invoke the real native CLI through configured FS-UAE and compare with live Rust output. Internal harness calls, reduced fixtures, and skipped runs are not completion evidence.
- The deterministic staged native-porting gate remains local and must not launch FS-UAE or use the network.
- A retained completion record must identify the tested source commit/tree, exact test names, exact commands, result, timestamp, and whether the run completed. It must not retrospectively claim that an older commit was tested when only the current aggregate baseline was executed.
- Native implementation items load `agents/rules/native-rust-parity-porting.md`, `agents/rules/native-68000.md`, and `agents/rules/fs-uae.md`; failures additionally load native parity triage rules, and any instrumentation change loads the safe-instrumentation rules.
- The flow refactor is ownership-only. Preserve the ABI, statement table layout, callback status (`D0`), skip flag (`D1`), next index (`D2`), PC behavior, binding/scope state, events, and register/CCR guarantees. Do not combine it with a semantic fix.
- One active work item at a time. Every item ends with one focused commit; no later item starts before its gates and `plan-compliance-reviewer` PASS are recorded.

## Milestones

- Milestone A — auditable Level D evidence: Items 1–2.
- Milestone B — complete machine-checked slice contracts: Items 3–4.
- Milestone C — flow-control ownership stabilization: Items 5–7.

## Work Items

- [x] Item 1: define and validate a retained fail-closed Level D completion-record format
  - Source requirement or finding IDs: RPO-001 (recent Level D completion is not retained as a fail-closed standard gate); fully closes the record-format and fail-closed invocation portion, not historical execution evidence.
  - Expected files: `scripts/workflow/run_native_cli_expansion_completion.sh`; `scripts/workflow/check_native_level_d_manifest.py`; `scripts/workflow/tests/test_native_cli_expansion_completion.py`; `scripts/workflow/tests/test_native_level_d_manifest.py`; a documented `documentation/quality-gates/` manifest convention; narrowly required workflow documentation.
  - Full quality gates: focused workflow Python tests; a negative test for missing FS-UAE configuration, missing required test, skipped outcome, malformed/absent manifest, and source-identity mismatch; `make workflow-gate`; `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`; formatter gate if native assembly changes (not expected).
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for Level D evidence infrastructure only; it must confirm the deterministic staged gate still cannot launch FS-UAE.
  - Commit outcome: a wrapper fails closed unless explicit configured FS-UAE execution completes every declared test and writes a schema-validated retained record naming the tested source identity, commands, tests, results, and timestamp.
  - Definition of done: a current implementation commit can be represented without self-referential SHA claims by an evidence-only follow-up record that names the already-tested source commit/tree; no wrapper success path accepts a skipped Level D test; historical commits remain unmodified.

- [x] Item 2: establish an honest aggregate Level D baseline for native expansion Items 5.1–5.6
  - Source requirement or finding IDs: RPO-001; partially closes historical auditability by recording a new aggregate baseline only, not inventing per-commit proof for prior work.
  - Expected files: the Item 1 completion wrapper configuration; `crates/opforge-asm/src/tests.rs` only if a named Level D test is missing; `documentation/quality-gates/native-cli-expansion-items-5-1-to-5-6-*.json` (or the approved equivalent evidence-only manifest); this plan.
  - Full quality gates: exact configured FS-UAE command for each named Level D test covering column-one directives, counted repetition, sequence assignment, iterable repetition, while, conditionals/match, and scopes; verify every outcome is `Completed` rather than `Skipped`; focused Rust test selection; `python3 scripts/workflow/check_native_level_d_manifest.py <manifest>`; `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`; `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for aggregate Level D closure only and confirms the manifest says “aggregate baseline” rather than claiming historical per-commit execution.
  - Commit outcome: one evidence-only commit retains a completed, exact-command Level D receipt for the currently tested aggregate source identity and lists every included test/result.
  - Definition of done: the required wrapper cannot pass with an omitted, skipped, failing, or source-mismatched test; all named Items 5.1–5.6 have one retained baseline result; no existing implementation commit is rewritten or relabelled as historically proven.

- [ ] Item 3: version the native slice metadata contract and make omitted evidence fields fail validation
  - Source requirement or finding IDs: RPO-002 (machine-enforced slice contract is weaker than the normative contract); fully closes validator-schema enforcement.
  - Expected files: `scripts/workflow/check_native_porting_slice.py`; `scripts/workflow/tests/test_native_porting_slice.py` (or its existing focused test module); any schema/documentation helper consumed by the validator; `documentation/plans/slices/*.toml` only where the new versioned fields are introduced.
  - Full quality gates: focused validator tests for missing/empty expected inputs, expected outputs, known non-equivalences, fast proof command, exact Level D command, and Level D fail-closed declaration; backward-compatibility/reviewed migration test; `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`; `make workflow-gate`; `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh` if Rust tests change.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for schema enforcement only and confirms no validator path launches FS-UAE.
  - Commit outcome: schema versioning makes the required contract fields deterministic for new/modified native parity slices while giving existing artifacts one explicit reviewed migration path.
  - Definition of done: validation rejects an incomplete v2 slice; validation explains the missing field; migration does not silently weaken currently governed artifacts; expected inputs/outputs and known non-equivalences are distinct fields rather than prose inference.

- [ ] Item 4: migrate the active native parity slice records to complete contracts
  - Source requirement or finding IDs: RPO-002; fully closes missing contract detail for the active native parity slices, while leaving unrelated historical artifacts unchanged.
  - Expected files: active `documentation/plans/slices/native-porting-slice-*.toml` records for Items 5.1–5.6 and other currently enforced native parity slices; narrow validator fixtures/tests; this plan.
  - Full quality gates: validate every migrated TOML record; run the fast proof command declared by each migrated slice; run the Item 1 fail-closed Level D completion wrapper for the Items 5.1–5.6 aggregate; `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`; `make workflow-gate`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for metadata migration only and verifies stated known non-equivalences match bounded native behavior rather than aspirational Rust parity.
  - Commit outcome: each active slice exposes concrete expected input/output, limitations, and reproducible fast/Level D commands in machine-readable form.
  - Definition of done: no migrated record relies on an ambiguous “run FS-UAE” instruction; bounded limits (where applicable) are stated as known non-equivalences; no Rust examples, reference fixtures, or implementation behavior changes.

- [ ] Item 5: extract statement navigation and scope-flow ownership from the monolithic native dispatcher
  - Source requirement or finding IDs: RPO-003 (flow-control complexity is concentrating in `opasmDriverApplyFlowControl`); partially closes the architectural hotspot without moving conditional or repetition semantics yet.
  - Expected files: `native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm`; new narrowly named native flow/navigation and scope modules; native module wiring; `crates/opforge-asm/src/tests.rs`; one updated slice metadata record if the native boundary identifier changes.
  - Full quality gates: focused Level B source/order and callback-contract tests; Level C model/ABI preservation tests where a host contract exists; exact Level D aggregate wrapper for Items 5.1–5.6; native formatter; `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`; `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for ownership-only navigation/scope extraction and confirms no new assembler behavior or CPU-specific generic logic.
  - Commit outcome: the root dispatcher delegates navigation and scope directives through explicit module boundaries while retaining its existing callback ABI and observable behavior.
  - Definition of done: statement index/skip status are preserved across every delegated path; scope reset/push/pop and qualified lookup retain canonical bytes; no production test changes merely to accommodate a changed behavior.

- [ ] Item 6: extract conditional and match branch-selection ownership
  - Source requirement or finding IDs: RPO-003; partially closes the dispatcher hotspot by isolating conditional flow after Item 5 provides shared navigation ownership.
  - Expected files: native opasm driver and a dedicated conditional-flow module; module wiring; `crates/opforge-asm/src/tests.rs`; conditional slice metadata only if its native boundary path changes.
  - Full quality gates: existing focused Level A/B/C conditional tests plus new module-boundary/return-contract tests; exact `native_opcore_conditionals_fs_uae` and aggregate Level D wrapper; native formatter; `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`; `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for conditional ownership extraction only and confirms skipped branch, nesting, and failure semantics are unchanged.
  - Commit outcome: `.if`/`.elseif`/`.else` and `.match`/`.case`/`.default` navigation has one named native owner, with the root dispatcher limited to routing and common callback framing.
  - Definition of done: canonical conditional bytes remain identical; selected/unselected statement ranges, depth bookkeeping, and malformed-block failures retain their prior observed behavior; no new conditional feature is added.

- [ ] Item 7: extract repetition ownership and certify the stabilized dispatcher boundary
  - Source requirement or finding IDs: RPO-003; fully closes the planned ownership split for the existing repetition/while surface.
  - Expected files: native opasm driver and dedicated repetition-flow module; module wiring; `crates/opforge-asm/src/tests.rs`; affected sequence/iterable/while slice metadata records if boundary names change; this plan.
  - Full quality gates: existing focused Level A/B/C counted-for, sequence, iterable-for, and while tests plus return-register/CCR preservation checks; exact Level D tests for those slices and the aggregate wrapper; native formatter; `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`; `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`; `make workflow-gate`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for repetition ownership extraction only and confirms no range/list/while semantic expansion is bundled.
  - Commit outcome: the root flow dispatcher is a small router over navigation, scope, conditional, and repetition owners; existing bounded repetition behavior remains unchanged.
  - Definition of done: `.for`, iterable `.for`, sequence assignment support, and `.while` retain current canonical and failure-path behavior; module boundaries document ownership and callback contracts; every completed extraction has its own focused commit.

## Blocking Rules

- active `AGENTS.md` rules remain binding during execution, including the prohibition on changing Rust reference examples/code and stripping `.org`.
- no commit before all quality gates pass.
- `plan-compliance-reviewer` must return `PASS` before commit.
- each work item or phase must end in exactly one new commit before the next item starts.
- no advancing to the next item on failed validation, failed FS-UAE completion, skipped Level D result, or a failed reviewer finding.
- checkbox updates are mandatory bookkeeping and a checkbox is not marked complete until its retained evidence and all named gates are green.
- a Level D receipt may only claim the source identity it actually tested; do not rewrite history or backfill unsupported per-commit claims.
- archive completed plans with `scripts/workflow/archive_completed_plan.sh`.
