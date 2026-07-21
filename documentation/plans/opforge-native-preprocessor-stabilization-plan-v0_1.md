<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# Plan: opForge Native Preprocessor Stabilization v0.1

## Metadata

- Source: User request on 2026-07-18, based on the Codex Planning Brief in the referenced “Repo Porting Overview” conversation; verified against repository head `fc7af953`.
- Amendment source: User-supplied “Scope Amendment: Native Runtime Boundary Stabilization” on 2026-07-21; current runtime concentration verified from the worktree and treated as discovery evidence only.
- Mode: implementation
- Owner: Codex
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.
- Artifact type: remediation and stabilization plan.
- Workflow: `skills/opforge-plan-authoring/SKILL.md`; `scripts/workflow/run_plan_workflow.sh`.
- Validation status: PASS — `plan-quality-reviewer` review completed 2026-07-18; retained evidence in the companion `.quality-gate.txt`.

## Goal

Stabilize the completed native macro/preprocessor foundation and the native
runtime boundaries on which segment, statement, export, diagnostic, and later
layout/output parity will depend. Preserve the Rust-authoritative macro-only
CLI parity slice while keeping preprocessor ownership bounded and
transactional; reduce `opasm_assembly_driver.asm` to assembly-session
orchestration and explicit subsystem dispatch; reduce `tkpkg_service.asm` to
public-service dispatch, request lifecycle, status projection, and stable ABI
adaptation; and narrow `opcore_expr_bridge.asm` to one explicit
expression-service boundary.

This plan does **not** implement `.segment`, `.statement`, statement
invocation, macro recursion beyond the existing one-frame bound, or module
export/import semantics. Those remain deferred follow-on vertical slices.

## Version Impact

- Affected component(s): native AmigaOS CLI preprocessor; native opasm driver and engine boundary; native tkpkg service and package-runtime boundary; native expression-service boundary; native parity and architecture-boundary tests; parent Item 7 readiness requirements.
- Impact class: patch
- Owned contract: ownership changes preserve ABI, accepted source, diagnostics, package selection, selector ordering, emitted artifacts, register/CCR/stack contracts, and all currently green Rust/native proofs.
- Rationale: preprocessor stabilization alone is insufficient while the assembly driver and package service combine orchestration, parsing, evaluation, selection, encoding, diagnostics, and mutable runtime state. Planned segment, statement, export, layout, selector, and diagnostic semantics must not accumulate in transitional god modules.

## Current-State Evidence

- Completed macro authority: `crates/opforge-core/src/macro_processor.rs::MacroProcessor::expand_lines`, `macro_processor_definitions.rs::{parse_macro_invocation,build_macro_args,format_macro_block_start}`, and `macro_processor_args_subst.rs::substitute_line`.
- Completed native boundary: `native/motorola68000/amigaos/opforge-cli/preprocessor.asm`, `line_processor.asm::{opforgeNativeCliProcessExpandedLineV1,opforgeNativeCliProcessExpandedScopeLineV1,opforgeNativeCliExpandActiveMacroV1}`, state/constants, and normal source → tokenizer → PRVM → session routing.
- Existing Level D macro proof: `native_macro_invocation_fixture_fs_uae`, using `examples/opcore/macro_invocation_native.asm`; the declared output is `A5 12 85 34 01 02 03 03 00 09 00`. The slice record is `documentation/plans/slices/native-porting-slice-macro-substitution-reentry.toml`.
- Existing real-68020 negative proof: `native_macro_preprocessor_harness_fs_uae_proves_capture_lookup_and_nested_frame_rejection` proves the one-frame rejection preserves the active caller frame, bound arguments, and caller line.
- Existing limits: eight definitions, eight body lines/definition, nine macro arguments, one invocation/expansion depth, and `SOURCE_LINE_BUFFER_CAPACITY = 512`. The state byte-count formula currently resides in `constants.asm`; state storage resides in `state.asm`.
- Existing Item 7 sequence: the governed reference-parity plan marks Item 7.3 complete and leaves 7.4 segment semantics, 7.5 statement capture, 7.6 statement expansion, and 7.7 exports open. This plan supplies the stabilization prerequisite before 7.4.
- Runtime-boundary amendment provenance: user-supplied “Scope Amendment: Native Runtime Boundary Stabilization” on 2026-07-21. Completed Items 1–5.1 retain their original scope, evidence, commits, and definitions of done; they do not prove the runtime-boundary work added below.
- Runtime concentration is discovery evidence only, not a decomposition criterion: `opasm_assembly_driver.asm` (5,397 lines) and `tkpkg_service.asm` (3,918) are mandatory decomposition targets; `opcore_expr_bridge.asm` (1,397) requires an explicit ownership/narrowing decision; `opasm_engine.asm` (2,607) and `tkpkg_pipeline.asm` (1,277) are conditional targets; `tkpkg_tokenizer_vm.asm` (1,586), `prvm_runtime.asm` (1,236), and `opasm_flow_text_encoding.asm` (1,094) require cohesion audits only unless evidence proves mixed ownership.
- Current concentration evidence: the driver combines pass/session coordination with directive routing, structural flow, expression/operand handling, sizing/layout, data/text emission, package selector requests, and events. `tkpkg_service.asm` combines public ABI dispatch with parser/expression adaptation, selection, operand-plan interpretation, package-table encoding, label lookup, diagnostics, and output formatting. The bridge must be assessed by responsibility rather than its name or line count.

## Constraints

- Every native behavior item loads `agents/rules/native-rust-parity-porting.md`, creates/upgrades a `documentation/plans/slices/*.toml` boundary contract, and uses Levels A–E honestly. Assembly changes also load `agents/rules/native-68000.md`; FS-UAE execution loads `agents/rules/fs-uae.md`; investigation/instrumentation loads the applicable triage/safe-instrumentation rules.
- Every documented FS-UAE `cargo test` invocation is single-instance and uses `--test-threads=1`.
- The macro fixture is a permanent Level D CLI-artifact regression during every preprocessor ownership change. The real-68020 nested-frame harness is a separate Level D focused-negative proof, not CLI-artifact parity; other internal harnesses are B/C/E only and never substitute for CLI parity.
- Ownership-only items must preserve ABI, register/CCR/stack contracts, state layout, capacities, accepted source, diagnostics, and exact Rust/native fixture artifacts. No segment, statement, export, or unrelated macro feature may be bundled into them.
- The preprocessor must not contain CPU/family/dialect, selector, operand-plan, mnemonic encoding, or output-format decisions. Run the CPU-boundary guard; do not mix the independent Rust selector-boundary remediation into this plan.
- A failed expansion must either be rolled back to its pre-call observable state or terminate the current CLI invocation with a deterministic diagnostic before later source/session processing observes partial state. Never silently truncate, fall through to PRVM, or use a test-only product branch.
- One active checkbox item at a time. Each item ends in exactly one focused commit after its gates and `plan-compliance-reviewer` PASS. A failed Level D invariant pauses the item and inserts a separate one-invariant remediation item before continuation.
- This plan is not active for implementation until its `plan-quality-reviewer` PASS sidecar exists and the metadata validation status is updated. A plan-compliance PASS is additionally required before every plan-driven commit.
- Macro-affecting source items use a mandatory two-phase evidence pattern: before the source commit, `run_native_macro_completion.sh --verify` runs both configured Level D tests and fails on missing configuration, skip, or non-exact execution while permitting the staged worktree; after that source commit, the clean-tree `--manifest` mode creates a fresh source-identity receipt and an evidence-only follow-up commit. The next implementation item may not start until its receipt validates. A receipt is baseline evidence only for its recorded source identity; it never proves a later extraction/change.
- Completed Items 1–5.1 are historically immutable except for explicit factual corrections. New runtime work may not reinterpret their evidence.
- `opasm_assembly_driver.asm`, `tkpkg_service.asm`, and `opcore_expr_bridge.asm` are no-growth files until their target ownership is certified. New semantic behavior is prohibited there except a minimal delegation entrypoint, compatibility glue with a named deletion item, an approved ownership-only extraction, or separately reviewed remediation of an existing invariant.
- An extraction commit must not also change or generalize semantics. CPU/family/selector remediation, including any MOS-shaped plan or operand generalization, is a separate semantic-remediation item with fresh parity evidence.
- Do not split cohesive VMs merely to reduce line count. After a neutral context interface exists, no adapter may directly access another subsystem's mutable internal tables. Every extracted module names its owner, public entry surface, mutable state, dependencies, register/CCR/stack contract, focused source-contract proof, and retained Level D regression where the real CLI exposes the path.

## Per-Item Native Boundary Contract Matrix

This matrix supplies the required contract fields for the commit-sized items
below; each item’s own invariant and gates narrow the matrix further. “A/B/C/D/E”
states the required evidence levels, not a claim that every level proves the
same behavior.

| Items | Rust authority | Native boundary | Inputs → outputs / known non-equivalences | Evidence |
|---|---|---|---|---|
| 1, 1.1 | `MacroProcessor::expand_lines`, `substitute_line`, block formatting | macro fixture CLI bridge and completion wrapper | fixture → exact 11 bytes; nested frame → preserved caller; no listing/segment/statement claim | A Rust oracle; B wrapper/validator; C substitution model; D CLI fixture + focused-negative harness; E debug only |
| 2–2.4 | existing macro capture/binding/substitution APIs named in slice records | one extracted preprocessor owner per item | unchanged current macro inputs → unchanged statuses/bytes; no new directives | B structure/ABI; C existing contract model; D macro wrapper; E none |
| 3, 5 | `expand_lines` and block formatting | expansion staging, line processor, session route | staged ordinary/structural line → record or restored caller state; no segment feature | B route contract; C transition/fault model; D macro wrapper + nested-frame harness; E only approved diagnostics |
| 4, 4.1, 5.1 | macro definition parsing and current fixed-limit semantics | definition record, scanner, constants/state | macro structural text/capacity → unchanged result or deterministic pre-mutation failure; inactive segment/statement kinds | A representative Rust acceptance; B layout/ownership; C tables; D macro wrapper; E none |
| 5.2–5.3 | current native behavior and retained Rust parity corpus | all audited runtime boundaries | source inventory → ownership architecture; no semantic change | B inventory/API; C dependency model; no D semantic claim |
| 5.4–5.5 | current service ABI, parser, and Rust expression behavior | tkpkg dispatch/status/parser/expression | identical request → identical status/output/error | B ABI; C transition model; D existing service/CLI fixtures |
| 5.6–5.7 | current selector/package authority | selection, operand, encoding, and context boundary | identical candidate/context → identical selected envelope/bytes | B boundary; C candidate/context model; D encode parity |
| 5.8–5.9 | existing Rust/opasm semantics | driver routing, flow, operand, data/text, layout owners | identical statements → identical traversal, PC, diagnostics, and image | B ownership; C pass/flow models; D affected CLI corpus |
| 5.10–5.12 | existing expression and architecture contracts | bridge, conditional targets, workflow guards | unchanged expressions/dependencies; no new feature | B source/dependency tests; C model; D affected parity |
| 5.13 | full existing parity authority | real CLI and established native paths | pre-amendment corpus → exact retained results | A/B/C as declared; D complete established corpus; E non-authoritative |
| 6, 6.1 | existing Rust test behavior is authority | evidence metadata and Rust test-module boundaries | unchanged test filters/results; no product semantic output | B classification/source list; D wrapper proves retained macro authority; E tools explicitly non-production |
| 7 | completed stabilization records and parent Item 7 scope | governed plan/slice dependency boundary | green evidence → parent prerequisite only; does not activate a feature | B artifact validation; D retained source-identity receipts; E none |

### Item-specific supplements

- Item 2: Rust `MacroProcessor::expand_lines`; native `opforgeNativeCliCaptureMacroDefinitionLineV1`/`opforgeNativeCliFinishMacroDefinitionsV1`; captured macro lines → unchanged stored body/status; approved path is only pre-tokenizer capture.
- Item 2.1: Rust `parse_macro_invocation`, `parse_macro_args`, `build_macro_args`; native `opforgeNativeCliParseMacroInvocationV1`/`opforgeNativeCliBeginMacroInvocationFrameV1`; dotted invocation → unchanged complete frame or deterministic failure; approved path is before source recording/PRVM.
- Item 2.2: Rust `substitute_line`; native `opforgeNativeCliSubstituteMacroBodyLineV1`; captured body/bound frame → unchanged expansion buffer/status; approved path is substitution only, not frontend re-entry.
- Item 2.3: Rust macro token-boundary parsing; native `lineStartsWithMacroDirective`, `lineStartsWithEndmacroDirective`, invocation-name/directive scanners; same source text → same token-boundary decision; approved path is scanner helper replacement only.
- Item 2.4: Rust `expand_lines`/block formatting; native `opforgeNativeCliBeginExpandedLineV1`/`opforgeNativeCliEndExpandedLineV1`; expansion text → same staged/restored caller line; approved path is source staging only. Items 2–2.4 share the declared fixed-capacity and no-new-directive non-equivalences.
- Item 4.1: Rust macro definition parsing; native scanner owner exported by Item 2.3; structural line → exact bounded directive/name classification; approved path is existing macro scanner routing only, no inactive-kind dispatch.
- Item 5.1: Rust macro errors where comparable; native constants/state capacity checks; within-limit request → unchanged state, over-limit request → deterministic pre-mutation failure; approved path is limits/checks only. Native fixed capacities remain the explicit non-equivalence.
- Items 5.2–5.3: current native behavior and existing Rust parity corpus; all eight audited assembly boundaries; source inventory and target architecture only, with no production semantic change.
- Items 5.4–5.7: current tkpkg service ABI, parser/expression, selector, operand, and encoding authority; façade/service/context boundaries; existing requests and contexts retain exact statuses, envelopes, diagnostics, and bytes.
- Items 5.8–5.9: existing opasm flow, expression, data/text, and layout semantics; driver-to-owner dispatch boundaries; source statements retain traversal, PC, diagnostics, package requests, and emitted image exactly.
- Items 5.10–5.12: current expression and architecture contracts; expression bridge, conditional audit targets, and deterministic guards; no semantic behavior is added.
- Item 5.13: established pre-amendment parity corpus; real CLI and existing native paths; evidence-only closure rather than feature implementation.
- Item 6: non-production artifact work; authority is current slice proof declarations; boundary is harness metadata/comment/report classification; no input/output semantics change, and no harness becomes an approved production path.
- Item 6.1: non-production Rust test ownership only; authority is pre-move test function/filter/result set; boundary is test-module declarations; no native production boundary or behavior is changed, and exact test names remain approved completion-wrapper paths.

## Milestones and Dependency Graph

```text
M1 Macro baseline + evidence receipt
  -> M2 ownership-only native extraction
  -> M3 explicit expansion/scanner/transaction contracts
  -> M4 preprocessor resource budget
  -> M5 native responsibility and dependency audit
  -> M6 tkpkg service-boundary decomposition
  -> M7 opasm driver-boundary decomposition
  -> M8 expression boundary and conditional runtime audits
  -> M9 existing-parity closure and no-growth certification
  -> M10 debug-evidence classification, Rust test ownership, and readiness review
  -> follow-on: CLI diagnostic remediation -> 7.4 segment capture -> segment expansion -> canonical segment Level D
               -> 7.5 statement capture -> 7.6 statement match/re-entry -> statement Level D
               -> 7.7 export representation -> module/import integration -> Item 7 closure
```

No follow-on feature starts until M10 passes. CLI diagnostic remediation follows
stabilization and precedes parity features, but is not implemented by this plan.
The follow-on sequence is planning only in this artifact; each feature requires
its own activated item/slice contract.

## Work Items

- [x] Item 1: add a fail-closed macro Level D completion producer and validator
  - Source requirement or finding IDs: SP-001 (macro Level D remains auditable after preprocessor work); existing slice `native-porting-slice-macro-substitution-reentry.toml`.
  - Invariant: the completed macro-only native CLI path continues to produce the live-Rust 11-byte binary, while nested-frame rejection leaves the active frame and caller source intact. Listing parity is not currently a Level D macro claim and is not added by this item.
  - Rust authority / native boundary: the current macro processor functions listed above; macro fixture → `opforgeNativeCliTokenizeCurrentLine` → expansion bridge → session output.
  - Expected inputs/outputs/non-equivalences: the untouched `macro_invocation_native.asm`; exact binary and rejection preservation; segments, statements, recursive success, listing parity, and wider diagnostics remain outside scope.
  - Expected files: `scripts/workflow/run_native_macro_completion.sh`, `scripts/workflow/check_native_macro_level_d_manifest.py`, their focused Python tests, `documentation/quality-gates/README.md`, and macro slice metadata. No retained receipt is created in this implementation commit.
  - Receipt contract: `--verify` requires the same FS-UAE environment and both exact tests in canonical order (`native_macro_invocation_fixture_fs_uae`, `native_macro_preprocessor_harness_fs_uae_proves_capture_lookup_and_nested_frame_rejection`) with exactly-one-test PASS/no `SKIP:` but writes nothing; `--manifest` additionally requires a clean worktree and records HEAD/tree SHA, UTC timestamp, command, and PASS result. Validator rejects a missing/extra/reordered test, malformed receipt, non-PASS result, or source identity mismatch; wrapper tests prove missing configuration and skip fail closed.
  - Proof: A `opcore_macro_invocation_rust_oracle`-style Level A fixture oracle; B wrapper/validator source contract; C existing `native_preprocessor_macro_substitution_and_reentry_are_bounded`; D exact two-test macro completion wrapper; E only retained debug investigation reports. Each test retains its existing “proves/does not prove” declaration in the slice metadata.
  - Full quality gates: `cargo test -p asm native_preprocessor_macro_substitution_and_reentry_are_bounded -- --nocapture`; `cargo test -p asm examples_match_reference_outputs -- --nocapture`; `python3 scripts/workflow/tests/test_native_macro_completion.py`; wrapper `--check-config` and staged-worktree `--verify`; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS limited to baseline certification; no source-semantic change.
  - Commit outcome: one workflow-only producer/validator commit, with clean-tree receipt instructions and no receipt or product semantic change.
  - Definition of done: the producer is proven to fail closed for missing config, skips, missing/reordered tests, dirty tree, and malformed/source-mismatched receipts; SP-001 remains partially open pending Item 1.1.

- [x] Item 1.1: record the macro completion receipt as an evidence-only follow-up
  - Source requirement or finding IDs: SP-001; Item 1’s producer/validator contract; `documentation/quality-gates/README.md`.
  - Invariant: a receipt is generated from a clean post-Item-1 HEAD, identifies that implementation commit/tree, and is committed without changing the tested source.
  - Rust authority / native boundary: Item 1 macro boundary; `run_native_macro_completion.sh` and its JSON validator.
  - Expected files: one `documentation/quality-gates/native-macro-level-d-<tested-short-sha>.json` receipt only.
  - Expected inputs/outputs/non-equivalences: clean post-Item-1 worktree → `documentation/quality-gates/native-macro-level-d-<tested-short-sha>.json`; later committed-receipt validation checks schema/source identity without `--expect-head`, because the evidence-only commit has a new HEAD.
  - Proof: B validator and clean-tree rule; D wrapper executes both exact macro tests. No E evidence closes the item.
  - Full quality gates: clean-tree `scripts/workflow/run_native_macro_completion.sh --manifest documentation/quality-gates/native-macro-level-d-$(git rev-parse --short HEAD).json`; before committing receipt, `python3 scripts/workflow/check_native_macro_level_d_manifest.py <receipt> --expect-head`; after committing it, validator without `--expect-head`; `make workflow-gate`.
  - Plan-compliance review evidence: PASS for evidence-only receipt commit.
  - Commit outcome: one receipt-only follow-up commit naming the exact tested Item-1 source identity.
  - Definition of done: SP-001 fully closes and every later macro-affecting item consumes the retained receipt/wrapper rather than an optional individual Level D run.

- [x] Item 2: extract macro-definition capture into its owner module without semantic change
  - Source requirement or finding IDs: SP-002 (avoid a preprocessor god module); SP-003 (make shared frontend ownership visible).
  - Invariant: exactly the existing macro definitions, binding/substitution, scanner behaviour, expansion bytes, errors, capacity limits, and register/CCR/stack effects remain unchanged while code moves behind stable public entrypoints.
  - Rust authority / native boundary: existing macro processor subset; `preprocessor.asm` and `line_processor.asm` exports.
  - Expected files: `preprocessor.asm`, new `preprocessor_definitions.asm`, native build/module references, focused source-structure tests, and a slice record.
  - Required module ownership: definitions owns macro header/body capture and finish-at-EOF; public names and state layout are unchanged. Do not move invocation, substitution, scan, expansion, or state tables in this commit.
  - Proof: B asserts only capture routes to the new module; C existing definition-boundary model; D Item 1 macro wrapper.
  - Full quality gates: Item 1 macro wrapper/validator; `cargo test -p asm native_preprocessor_macro_definitions_are_consumed_and_bounded -- --nocapture`; native formatter; staged native-porting; Rust quality; CPU boundary; workflow gate.
  - Plan-compliance review evidence: PASS limited to definition-capture ownership and macro regression.
  - Commit outcome: one ownership-only definition module commit.
  - Definition of done: no new directive semantics and capture behavior is unchanged; SP-002 partially closes.

- [x] Item 2.1: extract invocation binding into its owner module without semantic change
  - Source requirement or finding IDs: SP-002; existing invocation-frame and binding slice contracts.
  - Expected files: `preprocessor.asm`, new `preprocessor_invocation.asm`, module references, focused tests/slice metadata.
  - Full quality gates: `cargo test -p asm native_preprocessor_macro_invocation_frame_is_bounded_and_resettable -- --nocapture`; `cargo test -p asm native_preprocessor_macro_invocations_bind_before_prvm_routing -- --nocapture`; Item 1 macro wrapper/validator; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for lookup/argument-binding extraction only.
  - Commit outcome: one ownership-only invocation module commit.
  - Definition of done: public parse result, fixed frame, errors, ABI, and macro Level D outputs are unchanged; SP-002 remains partially closed.

- [x] Item 2.2: extract macro substitution into its owner module without semantic change
  - Source requirement or finding IDs: SP-002; macro substitution/reentry slice contract.
  - Expected files: `preprocessor.asm`, new `preprocessor_substitution.asm`, module references, focused tests/slice metadata.
  - Full quality gates: `cargo test -p asm native_preprocessor_macro_substitution_and_reentry_are_bounded -- --nocapture`; Item 1 macro wrapper/validator; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for byte-for-byte substitution ownership only.
  - Commit outcome: one ownership-only substitution module commit.
  - Definition of done: named/positional/full-list/default behavior and deterministic failures remain identical; SP-002 remains partially closed.

- [x] Item 2.3: extract bounded scanning into its owner module without semantic change
  - Source requirement or finding IDs: SP-002 and SP-005; existing macro definition/invocation tests.
  - Expected files: `preprocessor.asm`, new `preprocessor_scan.asm`, module references, focused tests/slice metadata.
  - Full quality gates: definition and invocation focused commands from Items 2/2.1; Item 1 macro wrapper/validator; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for moving existing scanner helpers only; no scanner behavior expansion.
  - Commit outcome: one ownership-only scanner module commit.
  - Definition of done: current token-boundary acceptance and malformed cases are unchanged; SP-002 remains partially closed.

- [x] Item 2.4: extract expansion staging into its owner module without semantic change
  - Source requirement or finding IDs: SP-002 and SP-003; existing macro expansion slice contract.
  - Expected files: `preprocessor.asm`, new `preprocessor_expansion.asm`, `line_processor.asm` only for changed imports/calls, module references, focused tests/slice metadata.
  - Full quality gates: `cargo test -p asm native_preprocessor_macro_substitution_and_reentry_are_bounded -- --nocapture`; Item 1 macro wrapper/validator; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for staging/restore ownership only.
  - Commit outcome: one ownership-only expansion module commit.
  - Definition of done: source save/restore, depth rejection, ABI, and fixture bytes are unchanged; SP-002/SP-003 fully close.

- [x] Item 3: formalize the expanded-line frontend contract
  - Source requirement or finding IDs: SP-004 (ordinary versus structural expanded-line routing is implicit).
  - Invariant: ordinary substituted lines use tokenizer → PRVM → session; generated structural lines use the documented source/session flow; each route restores caller source on every success/failure return and reports one deterministic status.
  - Rust authority / native boundary: `MacroProcessor::expand_lines` and block formatting; `opforgeNativeCliProcessExpandedLineV1`, `opforgeNativeCliProcessExpandedScopeLineV1`, begin/end staging, and session record APIs.
  - Expected files: focused expansion module from Item 2, `line_processor.asm`, state/slice metadata, and native parity tests; exact naming follows established native module style.
  - Contract requirements: document owner of current/saved line pointer/length, expansion depth, statement-index advancement, tokenizer/PRVM status, output position, scope start/end, error propagation, and caller restoration. Generated `.block`/`.endblock` remains macro-only scope behaviour, not a generic segment feature.
  - Proof: B source-order/ABI contract; C success plus tokenizer/PRVM failure transition model; D macro fixture and real nested-frame rejection; negative cases cover staging length, active depth, body failure, and closing-scope failure.
  - Full quality gates: Item 1 gates plus `cargo test -p asm native_preprocessor_macro_substitution_and_reentry_are_bounded -- --nocapture` and exact Level D macro command.
  - Plan-compliance review evidence: PASS for routing/rollback only, with no changes to macro language acceptance.
  - Commit outcome: one explicit frontend contract can be used by later segments/statements without direct `line_processor` branching or partial-state leakage.
  - Definition of done: SP-004 is fully closed.

- [x] Item 4: define an inert shared structural-definition record contract
  - Source requirement or finding IDs: SP-005 (one-off directive/name loops); SP-006 (macro/segment/statement storage must not diverge accidentally).
  - Invariant: one inert definition header/body contract expresses kind, name/signature, body span/count, owner module, visibility, label attachment, expansion policy, and scope-wrapping policy without enabling segment/statement behavior or changing existing macro storage.
  - Rust authority / native boundary: Rust macro processor definition parsing; native scan/definition modules and fixed state representation.
  - Expected files: definition module, `state.asm`, `constants.asm`, slice metadata, state-layout tests.
  - Proof: B native field-layout and inactive-kind ownership; C record-state model; D Item 1 macro wrapper. This item does not change the scanner or capture/expand segments/statements.
  - Full quality gates: Item 1 macro wrapper/validator; focused state-layout test; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for inert state/layout migration only.
  - Commit outcome: one record-layout commit, or a documented no-layout-change commit that records the existing structure as the shared contract.
  - Definition of done: unimplemented kinds cannot route through normal processing and existing macro state is identical; SP-006 fully closes.

- [x] Item 4.1: replace duplicated structural scanners with the bounded scanner contract
  - Source requirement or finding IDs: SP-005; Item 4’s shared record contract.
  - Expected files: `preprocessor_scan.asm`, affected definition/invocation modules, scanner tests, slice metadata.
  - Invariant: one bounded scanner recognizes complete structural directives and names at token boundaries while preserving current macro acceptance/rejection and leaving segment/statement behavior inactive.
  - Exact proof commands: `cargo test -p asm native_preprocessor_macro_definitions_are_consumed_and_bounded -- --nocapture`; `cargo test -p asm native_preprocessor_macro_invocations_bind_before_prvm_routing -- --nocapture`; Item 1 macro wrapper/validator. New table-driven tests must cover mixed case, prefixes, comments, labels, quotes, truncated/max-length lines, and end-kind matching; each declares Level C and does-not-prove native execution.
  - Full quality gates: exact proof commands; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for scanner consolidation only.
  - Commit outcome: one scanner-migration commit.
  - Definition of done: malformed input fails deterministically, macro CLI output is unchanged, and SP-005 fully closes.

- [x] Item 5: establish transactional expanded-line failure behavior
  - Source requirement or finding IDs: SP-007 (partial-frame/session corruption risk); SP-008 (fixed capacities lack one coherent contract).
  - Invariant: expanded-line staging/re-entry either commits a complete record or restores caller source, invocation frame, expansion depth, scope depth, recorded statement count, and output position before returning a deterministic failure.
  - Rust authority / native boundary: macro processor error behavior where comparable; preprocessor state, expansion route, scope/session state, and reset paths.
  - Expected files: expansion/line-route modules, state, native tests, and slice metadata.
  - Exact proof commands: existing `cargo test -p asm native_preprocessor_macro_substitution_and_reentry_are_bounded -- --nocapture`; new focused tests named `native_preprocessor_expanded_line_failure_restores_caller_state` and `native_preprocessor_generated_scope_failure_is_transactional`; Item 1 macro wrapper/validator. The new tests must observe pre/post caller line, frame, depth, record count, and output-position model on tokenizer/PRVM and generated-scope failure.
  - Proof: B state-transition ownership; C fault-injection model; D macro success plus nested-frame Level D negative proof. Do not add instrumentation here; if needed, add a separately reviewed instrumentation-only item using the safe framework.
  - Full quality gates: exact proof commands; native formatter; staged native-porting; Rust quality; CPU-boundary guard; workflow gate.
  - Plan-compliance review evidence: PASS for transactional route recovery only.
  - Commit outcome: one transactional expansion-route commit.
  - Definition of done: no partial expanded line reaches later CLI processing and SP-007 fully closes for the named observable failures.

- [x] Item 5.1: publish and enforce the native preprocessor resource budget
  - Source requirement or finding IDs: SP-008; Item 5 transaction contract.
  - Expected files: `constants.asm`, `state.asm`, affected owner modules, slice metadata, focused capacity tests/documentation.
  - Invariant: every declared definition/body/header/name/signature/argument/full-list/source/expansion/saved-line/depth capacity has one owner, lifetime/reset policy, and deterministic pre-mutation failure outcome.
  - Exact proof commands: new `cargo test -p asm native_preprocessor_capacity_matrix_is_deterministic -- --nocapture`; existing definition/invocation/substitution focused tests; Item 1 macro wrapper/validator. The matrix explicitly states shared/partitioned/cumulative and session-reset behavior, and its Level C declaration does not claim native 68020 execution.
  - Full quality gates: exact proof commands; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for capacity bookkeeping/diagnostics only.
  - Commit outcome: one capacity-contract commit.
  - Definition of done: capacity failures never truncate/fall through and SP-008 fully closes; no segment/statement capacity is activated.

- [x] Item 5.2: inventory native runtime responsibilities and dependency direction
  - Source requirement or finding IDs: NR-001 through NR-008.
  - Invariant: every routine, mutable-state block, public export, imported subsystem, and diagnostic path in the eight audited files has one declared responsibility without changing production behavior.
  - Expected files: repository-grounded inventories for routines, mutable state, inbound/outbound calls, public/private entries, dependency findings, candidate extraction groups, and explicit retain-as-cohesive decisions.
  - Mandatory findings: distinguish orchestration from semantic implementation; identify direct cross-subsystem state access, duplicated string/scanner/operand helpers, package/CPU/family/dialect/selector/expression/layout/output/diagnostic owners, and safe segment/statement landing points.
  - Proof: Level B source inventory checked against labels and imports; no C/D semantic claim.
  - Full quality gates: focused inventory validator; `make workflow-gate`.
  - Plan-compliance review evidence: PASS for evidence and architecture inventory only. Evidence: `documentation/architecture/native-runtime-boundary-inventory-v0_1.md`; `python3 scripts/workflow/check_native_runtime_boundary_inventory.py --report` complete manifest PASS; `python3 -m unittest scripts/workflow/tests/test_check_native_runtime_boundary_inventory.py` PASS; plan-compliance review PASS after complete routine/import/state/diagnostic manifest audit.
  - Commit outcome: one evidence-and-architecture inventory commit.
  - Definition of done: no mandatory or conditional target proceeds to decomposition without a repository-grounded ownership decision.

- [x] Item 5.3: define the target native runtime boundary contract
  - Source requirement or finding IDs: NR-009; Item 5.2 inventory.
  - Invariant: one-way ownership and stable service contracts are explicit for CLI frontend, preprocessor, opasm driver/engine, tkpkg façade and runtimes, expression service, and diagnostic/event projection.
  - Expected files: runtime-boundary architecture record, affected slice metadata, focused architecture/dependency contract tests, and this plan.
  - Required target responsibilities: driver owns pass/session orchestration and subsystem callback dispatch only; tkpkg service owns ABI dispatch, request validation/lifecycle, output projection, and last-error entry only; expression service owns context adaptation/execution; package runtimes own neutral package-defined selection/encoding; engine owns statement/pass/image state through documented APIs.
  - Required neutral runtime context: current pass, current address, symbol lookup, symbol stability/finalization, and diagnostic sink.
  - Proof: B architecture/API contracts; C dependency model proving prohibited reverse edges.
  - Full quality gates: focused architecture/dependency checks; `make workflow-gate`.
  - Plan-compliance review evidence: PASS for target contracts only. Evidence: `documentation/architecture/native-runtime-boundary-contract-v0_1.md`; `documentation/plans/slices/native-porting-slice-runtime-boundary-contract.toml`; `python3 scripts/workflow/check_native_runtime_boundary_contract.py` PASS; `python3 -m unittest scripts/workflow/tests/test_check_native_runtime_boundary_contract.py` PASS; plan-compliance review PASS for B/C architecture evidence only.
  - Commit outcome: one architecture-contract commit.
  - Definition of done: every later extraction names source owner, destination owner, temporary adapter, and adapter deletion criterion.

- [x] Item 5.4: extract tkpkg service status projection
  - Source requirement or finding IDs: NR-002; Item 5.3 target contract.
  - Invariant: status projection, output-window setup, and stored last-error handling move behind one focused owner without changing a service-ordinal result.
  - Expected files: `tkpkg_service.asm`, `tkpkg_service_status.asm`, or repository-style equivalents. `tkpkg_service.asm` remains the public façade.
  - Scope boundary: no bootstrap, request decoding, expression, selection, encoding, package semantics, or CPU/family/dialect support change. The only production Level D target is the retained native 65C02 CLI path; generic tokenizer debug corpora may confirm status routing only and never establish target-CPU support.
  - Proof: B ABI layout/entrypoint/source-order contracts; C service status-transition model; D retained native 65C02 CLI completion wrapper. The generic 68000 debug-CLI corpus is supplementary route evidence only, not CPU parity.
  - Full quality gates: affected focused contracts; `scripts/workflow/run_native_macro_completion.sh --verify` under the configured FS-UAE environment; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: scoped review PASS for one ownership-only phase. Evidence: `native/motorola68000/amigaos/tkpkg/tkpkg_service_status.asm`; `documentation/plans/slices/native-porting-slice-tkpkg-status-projection.toml`; staged native-porting gate PASS; focused ownership/roundtrip/native-CLI assembly tests PASS; retained 65C02 macro Level D wrapper PASS (both exact tests); full Rust quality gate PASS; CPU/family/dialect scope explicitly unchanged.
  - Commit outcome: one ownership-only status-projection commit.
  - Definition of done: the façade no longer owns reusable status/error implementation.

- [x] Item 5.4.1: extract tkpkg service request lifecycle
  - Source requirement or finding IDs: NR-002; Item 5.4.
  - Invariant: bootstrap, control-block validation, and request bookkeeping move behind one request owner without changing any service ordinal result.
  - Expected files: `tkpkg_service.asm`, `tkpkg_service_request.asm` or repository-style equivalent, and focused ABI/request-transition tests.
  - Proof: B ABI/entrypoint contracts; C request-lifecycle model; D exact affected service fixtures.
  - Full quality gates: focused request contracts and D fixtures; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for request-lifecycle extraction only. Evidence: `native/motorola68000/amigaos/tkpkg/tkpkg_service_request.asm`; `documentation/plans/slices/native-porting-slice-tkpkg-request-lifecycle.toml`; focused header/ownership/last-error/native-CLI assembly tests PASS; staged native-porting gate PASS; retained 65C02 macro Level D wrapper PASS (both exact FS-UAE tests); full Rust quality gate PASS; CPU/family/dialect scope unchanged.
  - Commit outcome: one ownership-only request-lifecycle commit.
  - Definition of done: the façade no longer decodes reusable request/control-block implementation details.

- [ ] Item 5.5: extract the tkpkg parser service adapter
  - Source requirement or finding IDs: NR-003; Items 5.2–5.4.
  - Invariant: parser route adaptation moves to a focused owner while request envelopes, opcode-version handling, output text, status codes, and diagnostics remain exact.
  - Expected files: `tkpkg_parse_service.asm` or repository-style equivalent.
  - Scope boundary: no expression request execution, expression-language, or diagnostic wording change.
  - Proof: B parser request-envelope/register contracts; C parser-adapter model; D existing parser-dependent parity fixtures.
  - Full quality gates: exact focused and established D fixtures; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for parser-adapter extraction only.
  - Commit outcome: one focused ownership-only parser-adapter commit.
  - Definition of done: service dispatch delegates to an independently owned parser adapter.

- [ ] Item 5.5.1: extract the tkpkg expression service adapter
  - Source requirement or finding IDs: NR-003; Items 5.3–5.5.
  - Invariant: expression request execution moves to one focused owner while context, output text, status codes, and diagnostics remain exact.
  - Expected files: `tkpkg_expression_service.asm` or repository-style equivalent, any named transitional context adapter, and focused expression-service tests.
  - Scope boundary: direct opasm mutable-table access is permitted only through the named transitional adapter with a deletion item; no expression-language or diagnostic wording change.
  - Proof: B expression-envelope/register contracts; C expression-adapter model; D existing expression parity fixtures.
  - Full quality gates: focused expression contracts and D fixtures; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for expression-adapter extraction only.
  - Commit outcome: one focused ownership-only expression-adapter commit.
  - Definition of done: service dispatch delegates to an independently owned expression adapter.

- [ ] Item 5.6: extract the tkpkg selection service
  - Source requirement or finding IDs: NR-004; Items 5.2–5.5.
  - Invariant: selected-instruction decoding and candidate traversal move behind a focused owner without changing selector ordering or emitted bytes.
  - Expected files: `tkpkg_selection_service.asm` or repository-style equivalent.
  - Scope boundary: ownership-only movement may retain current plan tags such as `rel8` and `pair_u8_rel8`; hardcoded-plan generalization and all new CPU/family knowledge are deferred to a separate semantic-remediation plan.
  - Proof: B selector/plan-dispatch ownership; C package-candidate model; D current instruction-selection and encode parity corpus.
  - Full quality gates: exact affected parity corpus; native formatter; staged native-porting; Rust quality; CPU-boundary guard; workflow gate.
  - Plan-compliance review evidence: PASS for unchanged selection ownership only.
  - Commit outcome: one ownership-only selection-service commit.
  - Definition of done: `tkpkg_service.asm` has no selected-instruction decoding or candidate-traversal logic.

- [ ] Item 5.6.1: extract the tkpkg operand-plan runtime
  - Source requirement or finding IDs: NR-004; Item 5.6.
  - Invariant: operand-plan interpretation moves behind one focused owner without changing request envelopes or emitted bytes.
  - Expected files: `tkpkg_operand_runtime.asm` or repository-style equivalent and focused plan-dispatch tests.
  - Scope boundary: retain existing plan tags unchanged; no CPU/family knowledge or plan generalization.
  - Proof: B plan-dispatch ownership; C operand-plan model; D affected encode parity corpus.
  - Full quality gates: focused plan contracts and D corpus; native formatter; staged native-porting; Rust quality; CPU-boundary guard; workflow gate.
  - Plan-compliance review evidence: PASS for operand-runtime extraction only.
  - Commit outcome: one ownership-only operand-runtime commit.
  - Definition of done: service façade and selection owner do not interpret operand plans directly.

- [ ] Item 5.6.2: extract the tkpkg encoding service
  - Source requirement or finding IDs: NR-004; Items 5.6–5.6.1.
  - Invariant: package-table lookup, encoding-program execution, and encoded-output construction move to one focused owner without changing selector ordering or emitted bytes.
  - Expected files: `tkpkg_encode_service.asm` or repository-style equivalent and focused encode-service tests.
  - Proof: B encode-dispatch ownership; C encoding-program model; D current encode parity corpus.
  - Full quality gates: focused encoding contracts and D corpus; native formatter; staged native-porting; Rust quality; CPU-boundary guard; workflow gate.
  - Plan-compliance review evidence: PASS for encoding-service extraction only.
  - Commit outcome: one ownership-only encoding-service commit.
  - Definition of done: `tkpkg_service.asm` contains no package-table interpreter, label lookup, or encoding logic.

- [ ] Item 5.7: define the neutral tkpkg runtime-context ABI
  - Source requirement or finding IDs: NR-005; Items 5.3 and 5.6.
  - Invariant: package selection/expression code obtains pass, address, symbol, stability, and diagnostics through one neutral context contract rather than direct opasm-table access.
  - Expected files: runtime-context ABI/module, opasm context adapter, and focused contract tests.
  - Scope boundary: internal calling conventions may change; observable assembler behavior may not.
  - Proof: B ABI/register/context-field contracts; C symbol/pass/address transition models; D affected CLI parity fixtures.
  - Full quality gates: focused context contracts and established D fixtures; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for context introduction and named adapter deletion criteria.
  - Commit outcome: one context-contract commit.
  - Definition of done: the ABI and adapter expose all required neutral fields without migrating consumers.

- [ ] Item 5.7.1: migrate the tkpkg expression consumer to neutral context
  - Source requirement or finding IDs: NR-005; Items 5.5.1 and 5.7.
  - Invariant: expression service uses only the neutral context for pass, address, symbols, stability, and diagnostics.
  - Expected files: tkpkg expression consumer, opasm context adapter, focused context-field contracts, and slice metadata.
  - Proof: B register/context contracts; C expression context-transition model; D affected expression fixtures.
  - Full quality gates: focused contracts and D fixtures; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for expression-consumer migration only.
  - Commit outcome: one context-consumer migration commit.
  - Definition of done: expression service has no direct opasm mutable-table access.

- [ ] Item 5.7.2: migrate the tkpkg selection consumer to neutral context
  - Source requirement or finding IDs: NR-005; Items 5.6–5.7.1.
  - Invariant: selection and encoding services use only the neutral context for pass, address, symbols, stability, and diagnostics.
  - Expected files: tkpkg selection/encoding consumers, opasm context adapter, focused context-field contracts, and slice metadata.
  - Proof: B register/context contracts; C candidate/context model; D affected selection and encode fixtures.
  - Full quality gates: focused contracts and D fixtures; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for selection-consumer migration only.
  - Commit outcome: one context-consumer migration commit.
  - Definition of done: tkpkg modules do not import or address opasm mutable label-table storage directly.

- [ ] Item 5.8: extract opasm assembly-driver directive routing
  - Source requirement or finding IDs: NR-006; Items 5.2–5.3.
  - Invariant: `opasm_assembly_driver.asm` retains session callback orchestration while directive classification moves to one focused owner without changing statement traversal.
  - Expected files: `opasm_directive_router.asm` and shared bounded token/directive comparison utilities only where justified.
  - Scope boundary: do not move structural-flow scans or enable segment/statement semantics.
  - Proof: B handler/dispatch and no-duplicate-route contracts; C directive-dispatch model; D affected directive fixtures.
  - Full quality gates: exact affected D fixtures; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for one driver-routing ownership extraction at a time.
  - Commit outcome: one independently revertible directive-router commit.
  - Definition of done: the driver implements no mnemonic/directive string chains.

- [ ] Item 5.8.1: extract opasm structural-flow coordination
  - Source requirement or finding IDs: NR-006; Item 5.8.
  - Invariant: matching-end and selected-branch scans move to a domain-flow or structural-navigation owner without changing statement traversal.
  - Expected files: flow-owner extension or `opasm_flow_dispatch.asm`, focused navigation contracts, and slice metadata.
  - Scope boundary: do not enable segment or statement semantics.
  - Proof: B no-duplicate-route contract; C nested-flow traversal model; D conditionals, repetitions, scopes, and structs fixtures.
  - Full quality gates: focused flow contracts and D fixtures; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for structural-flow extraction only.
  - Commit outcome: one independently revertible flow-coordination commit.
  - Definition of done: the driver does not scan future statements for domain-specific terminators.

- [ ] Item 5.9: extract opasm operand/evaluation request construction
  - Source requirement or finding IDs: NR-007; Item 5.8.
  - Invariant: operand/expression request construction moves to one focused owner while two-pass size, PC, diagnostics, and package requests remain exact.
  - Expected files: `opasm_operand_eval.asm` or repository-style equivalent.
  - Scope boundary: do not move selector adaptation, data/text emission, or layout ownership.
  - Proof: B ownership/callback contract; C operand request model; D affected CLI parity fixtures.
  - Full quality gates: exact affected D fixtures; native formatter; staged native-porting; Rust quality; CPU-boundary guard; workflow gate.
  - Plan-compliance review evidence: PASS for operand-request extraction only.
  - Commit outcome: one focused operand-request commit.
  - Definition of done: the driver no longer constructs operand/expression requests.

- [ ] Item 5.9.1: extract opasm selector-encode adaptation
  - Source requirement or finding IDs: NR-007; Item 5.9.
  - Invariant: selector-encode adaptation moves to one owner while package requests, diagnostics, and emitted bytes remain exact.
  - Expected files: `opasm_selector_encode.asm` or repository-style equivalent and focused selector-adapter tests.
  - Proof: B callback ownership; C encode request model; D affected encode parity fixtures.
  - Full quality gates: focused contracts and D fixtures; native formatter; staged native-porting; Rust quality; CPU-boundary guard; workflow gate.
  - Plan-compliance review evidence: PASS for selector-adapter extraction only.
  - Commit outcome: one focused selector-adapter commit.
  - Definition of done: the driver does not adapt selector results into encoding operations.

- [ ] Item 5.9.2: extract opasm numeric-data sizing and emission
  - Source requirement or finding IDs: NR-007; Item 5.9.
  - Invariant: numeric-data sizing/emission moves to one owner while two-pass size, PC, image, and diagnostics remain exact.
  - Expected files: `opasm_directive_data.asm` or repository-style equivalent and focused two-pass data tests.
  - Proof: B ownership/callback contract; C pass-one/pass-two data model; D affected reference/CLI fixtures.
  - Full quality gates: focused data contracts and D fixtures; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for data-service extraction only.
  - Commit outcome: one focused data-service commit.
  - Definition of done: the driver does not implement numeric-data sizing or byte emission.

- [ ] Item 5.9.3: extract opasm text sizing and emission
  - Source requirement or finding IDs: NR-007; Item 5.9.
  - Invariant: text sizing/emission moves to one owner while two-pass size, PC, image, and diagnostics remain exact.
  - Expected files: `opasm_directive_text.asm` or repository-style equivalent and focused text-encoding/two-pass tests.
  - Proof: B ownership/callback contract; C pass-one/pass-two text model; D affected text fixtures.
  - Full quality gates: focused text contracts and D fixtures; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for text-service extraction only.
  - Commit outcome: one focused text-service commit.
  - Definition of done: the driver does not implement text sizing or emission.

- [ ] Item 5.9.4: extract opasm layout ownership
  - Source requirement or finding IDs: NR-007; Items 5.2–5.3.
  - Invariant: layout/region/section/place/alignment ownership moves to one owner without adding layout semantics or changing two-pass size, PC, image, or diagnostics.
  - Expected files: `opasm_layout.asm` or repository-style equivalent and focused layout/two-pass tests.
  - Proof: B ownership/callback contract; C pass-one/pass-two layout model; D affected reference/CLI fixtures.
  - Full quality gates: focused layout contracts and D fixtures; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for layout-owner extraction only.
  - Commit outcome: one focused layout-owner commit.
  - Definition of done: the driver does not implement layout, region, section, placement, or alignment behavior.

- [ ] Item 5.10: audit and narrow the expression bridge
  - Source requirement or finding IDs: NR-008; Items 5.2–5.3.
  - Invariant: `opcore_expr_bridge.asm` has one documented responsibility and does not duplicate parser, evaluator, literal, symbol, diagnostic, or service-adapter policy owned elsewhere.
  - Expected files: `opcore_expr_bridge.asm`, any proven destination module or narrow adapter, retained ownership decision, slice metadata, and focused expression-boundary tests.
  - Required decision: retain cohesive, split by proven ownership, or replace with a narrow adapter over an existing expression runtime. Line count alone cannot decide; a semantic correction is a separate remediation item.
  - Proof: B entrypoint/dependency inventory; C expression-service boundary model; D affected expression parity fixtures.
  - Full quality gates: focused bridge/dependency checks and exact affected D fixtures; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for the retained ownership decision or one ownership-only extraction.
  - Commit outcome: one documented no-change decision or independently revertible narrowing/extraction commit.
  - Definition of done: bridge responsibilities, state, imports, and long-term owner are explicit.

- [ ] Item 5.11: conditionally remediate opasm engine and tkpkg pipeline ownership
  - Source requirement or finding IDs: NR-001, NR-004, NR-009; audit findings from Items 5.2–5.3.
  - Invariant: `opasm_engine.asm` and `tkpkg_pipeline.asm` change only if the audit proves mixed ownership or prohibited dependency direction; tokenizer VM, PRVM runtime, and text-encoding flow remain intact unless an independently testable responsibility violation is found.
  - Expected files: only audited targets and retained cohesion/decomposition decisions.
  - Proof: B inventory/owner contract; C dependency model; D affected parity fixtures when source changes.
  - Full quality gates: no-change decision validation or exact affected gates; native formatter if assembly changes; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS limited to the proved condition.
  - Commit outcome: one documented no-change decision or one focused conditional extraction/remediation commit.
  - Definition of done: every audited large module has a retained cohesion/decomposition decision.

- [ ] Item 5.12: enforce no-growth and ownership boundaries
  - Source requirement or finding IDs: NR-009; all prior runtime-boundary items.
  - Invariant: deterministic source guards reject new private semantic routines in the three transitional hotspot files, permit declared façade/delegation entries, detect direct tkpkg access to opasm mutable tables, detect CPU/family terms outside approved owners, and require new semantic modules to name their owner and slice contract.
  - Expected files: deterministic workflow guard(s), positive/negative workflow tests, guard documentation, and this plan.
  - Scope boundary: no simplistic absolute line-count gate is sufficient.
  - Proof: B positive/negative workflow tests; no D semantic claim from the guard itself, followed by the full established Level D corpus.
  - Full quality gates: focused workflow tests; complete established D corpus; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for deterministic guard scope and allowances.
  - Commit outcome: one workflow-guard commit.
  - Definition of done: later segment/statement work cannot silently regrow decomposed hotspots or reverse neutral dependency edges.

- [ ] Item 5.13: run complete existing-parity closure
  - Source requirement or finding IDs: NR-001 through NR-009; all prior runtime-boundary items.
  - Invariant: every observable feature green before this amendment remains green after decomposition.
  - Expected files: retained clean-source completion receipt, affected slice records/evidence, parity closure report, and this plan.
  - Required proof groups: macro completion wrapper and retained receipt; scopes; conditionals/match; loops; structs; compile-time values/expressions; text encoding; module-local symbols; package selection/encoding; CLI/reference artifact corpus. Use exact real-CLI FS-UAE tests where established; internal harnesses never substitute for CLI proof.
  - Proof: A/B/C as declared by each corpus; D complete established CLI corpus; E remains non-authoritative.
  - Full quality gates: all named parity groups; retained macro receipt validation; no-growth/dependency guards; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for evidence-only closure and any remaining adapter deletion schedule.
  - Commit outcome: one clean-source-identity evidence-only completion receipt; no feature implementation.
  - Definition of done: all extraction adapters are removed or have named later deletion items, all no-growth guards pass, and no CPU/package boundary regression remains.

- [ ] Item 6: classify preprocessor debugging evidence
  - Source requirement or finding IDs: SP-009 (debug harnesses must not become parity substitutes); SP-010 (oversized `tests.rs` obscures subsystem ownership).
  - Invariant: each harness declares permanent B/C/D proof, Level E diagnostic tooling, or removal, and the macro CLI fixture remains the only macro artifact parity authority.
  - Expected files: harness comments/metadata, debug reports, macro slice metadata, and focused classification tests if needed.
  - Required classifications: macro preprocessor harness (Level D focused-negative), macro CLI debug-event harness (diagnostic or permanent contract), pipeline-selection helpers, and FS-UAE console-debugger tools/reports (Level E unless separately promoted). Temporary probes have a deletion condition.
  - Proof: source/metadata classification check plus Item 1 macro wrapper; Level E tooling is never cited as production parity.
  - Full quality gates: focused classification check; Item 1 macro wrapper/validator; native formatter if harness assembly changes; staged native-porting where applicable; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for evidence classification only.
  - Commit outcome: one harness-evidence classification commit.
  - Definition of done: production versus debug evidence is unambiguous and SP-009 fully closes.

- [ ] Item 6.1: split native-parity Rust test ownership mechanically
  - Source requirement or finding IDs: SP-010; Item 6 classifications.
  - Expected files: `crates/opforge-asm/src/tests.rs` and extracted subsystem-owned test modules following existing crate conventions.
  - Invariant: test functions, filters, results, and proof declarations are unchanged while preprocessor/macro/FS-UAE/reference-shard tests move to owning modules.
  - Exact proof commands: each pre-move focused macro test filter; `cargo test -p asm examples_match_reference_outputs -- --nocapture`; Item 1 macro wrapper/validator. A source-list assertion records unchanged function names/filters.
  - Full quality gates: exact proof commands; Rust quality; workflow gate; native formatter only if native harness assembly is touched.
  - Plan-compliance review evidence: PASS for test-file ownership only.
  - Commit outcome: one mechanical Rust test-module refactor commit.
  - Definition of done: completion wrappers retain their exact test names and SP-010 fully closes with no product code change.

- [ ] Item 7: certify native runtime stabilization and amend the parent parity plan
  - Source requirement or finding IDs: SP-011; NR-001 through NR-009.
  - Invariant: parent parity Item 7.4 segment work cannot begin until all original preprocessor items, the resource budget, debug evidence, Rust test ownership, native responsibility/dependency audit, mandatory tkpkg-service and opasm-driver decomposition, expression-boundary decision, conditional audit decisions, neutral runtime context, no-growth/dependency guards, established parity, and a clean source-identity completion receipt are green.
  - Expected files: this plan; `documentation/plans/opforge-native-cli-reference-parity-expansion-plan-v0_1.md`; runtime-boundary architecture records; affected slice records; retained receipts; plan-quality and plan-compliance evidence.
  - Required follow-on ordering (not implementation in this item): CLI error-output remediation; segment definition capture; segment invocation/expansion; canonical macro/segment Level D closure; statement definition/signature storage; statement matching/capture; statement substitution and ordinary frontend re-entry; statement Level D closure; preprocessor export representation; module/import visibility and alias integration; Item 7 parity-shard closure.
  - Required parent-plan amendment: Item 7.4 explicitly depends on this plan’s completion; CLI diagnostic remediation becomes the next implementation programme; segment capture no longer follows preprocessor-only stabilization immediately. The separate CPU/selector semantic-remediation programme remains required before Item 8 linker/output expansion, but does not block Item 7 source-preprocessor work once no-growth and ownership boundaries are certified.
  - Required risks and rollback: every ownership-only extraction is independently revertible; every temporary adapter has a deletion criterion; any changed Level D result creates a separate first-divergence remediation item; no decomposition closes merely because a hotspot became shorter.
  - Full quality gates: all named checks from Items 1–6.1; complete established native parity corpus; macro Level D receipt validation; no-growth/dependency guards; `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-native-preprocessor-stabilization-plan-v0_1.md`; `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-native-cli-reference-parity-expansion-plan-v0_1.md`; `python3 scripts/workflow/check_workflow_artifact_bundle.py --root . plan documentation/plans/opforge-native-preprocessor-stabilization-plan-v0_1.md`; `make workflow-gate`; final `plan-quality-reviewer` PASS.
  - Plan-compliance review evidence: PASS confirms all stabilization prerequisites are complete and no segment, statement, export, linker, or new selector semantics were implemented prematurely.
  - Commit outcome: one stabilization-closure and parent-plan amendment commit after preceding items are independently committed and green.
  - Definition of done: the next active programme is CLI error-output remediation; segment capture follows only after that remediation completes.

## Blocking Rules

- No commit before all named quality gates pass and `plan-compliance-reviewer` returns `PASS`.
- Each work item or extraction phase ends in exactly one new focused commit before the next item begins; no feature semantics in Items 1–6.
- Exact FS-UAE commands must fail closed when required configuration is absent; no optional/skipped Level D result may close a macro-affecting item.
- If an ownership change changes macro output, diagnostics, acceptance, state layout, ABI, or a Level D result, stop; restore the macro baseline and add one separate remediation item with the first divergent boundary.
- No new CPU-specific logic, package bypass, selector/encoder logic, direct output encoding, silent truncation, harness-injected shortcut, or unclassified debug probe is permitted.
- No new segment, statement, export, linker, output-format, or selector semantics during this plan. No extraction commit may correct or generalize semantics.
- `opasm_assembly_driver.asm`, `tkpkg_service.asm`, and `opcore_expr_bridge.asm` are no-growth files until their ownership is certified. Reduced line count alone never closes an item; ownership/dependency contracts do.
- Do not create distributed god modules with circular imports or shared mutable state. Every temporary adapter names its owner, reason, allowed callers, deletion criterion, and latest permitted removal milestone.
- Any changed Level D result stops the active item and creates a separate first-divergence remediation item.
- Checkbox updates, slice metadata, retained evidence, required quality-gate sidecars, and plan-compliance review are mandatory bookkeeping. Archive this plan only when all items are complete using `scripts/workflow/archive_completed_plan.sh`.
