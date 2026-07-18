<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# Plan: opForge Native Preprocessor Stabilization v0.1

## Metadata

- Source: User request on 2026-07-18, based on the Codex Planning Brief in the referenced “Repo Porting Overview” conversation; verified against repository head `fc7af953`.
- Mode: implementation
- Owner: Codex
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.
- Artifact type: remediation and stabilization plan.
- Workflow: `skills/opforge-plan-authoring/SKILL.md`; `scripts/workflow/run_plan_workflow.sh`.
- Validation status: PASS — `plan-quality-reviewer` review completed 2026-07-18; retained evidence in the companion `.quality-gate.txt`.

## Goal

Stabilize the completed native macro/preprocessor foundation before any segment,
statement, or preprocessor-export semantics are implemented. Preserve the
Rust-authoritative macro-only CLI parity slice while splitting ownership,
making expanded-line routing and bounded state explicit, and establishing the
failure and capacity contracts on which the remaining Item 7 work will depend.

This plan does **not** implement `.segment`, `.statement`, statement
invocation, macro recursion beyond the existing one-frame bound, or module
export/import semantics. Those remain deferred follow-on vertical slices.

## Version Impact

- Affected component(s): native AmigaOS CLI preprocessor, native parity tests, Item 7 planning and retained Level D evidence.
- Impact class: patch
- Owned contract: preprocessor source-structure handling stays bounded, transactional, CPU-neutral, and re-enters the established frontend without changing macro artifact output.
- Rationale: `preprocessor.asm` owns capture, invocation binding, substitution, scanning, and re-entry after the macro slice, while `line_processor.asm` owns a second portion of the expansion bridge. Extending that split ownership directly to segments/statements would make new semantics hard to localize and unsafe to prove.

## Current-State Evidence

- Completed macro authority: `crates/opforge-core/src/macro_processor.rs::MacroProcessor::expand_lines`, `macro_processor_definitions.rs::{parse_macro_invocation,build_macro_args,format_macro_block_start}`, and `macro_processor_args_subst.rs::substitute_line`.
- Completed native boundary: `native/motorola68000/amigaos/opforge-cli/preprocessor.asm`, `line_processor.asm::{opforgeNativeCliProcessExpandedLineV1,opforgeNativeCliProcessExpandedScopeLineV1,opforgeNativeCliExpandActiveMacroV1}`, state/constants, and normal source → tokenizer → PRVM → session routing.
- Existing Level D macro proof: `native_macro_invocation_fixture_fs_uae`, using `examples/opcore/macro_invocation_native.asm`; the declared output is `A5 12 85 34 01 02 03 03 00 09 00`. The slice record is `documentation/plans/slices/native-porting-slice-macro-substitution-reentry.toml`.
- Existing real-68020 negative proof: `native_macro_preprocessor_harness_fs_uae_proves_capture_lookup_and_nested_frame_rejection` proves the one-frame rejection preserves the active caller frame, bound arguments, and caller line.
- Existing limits: eight definitions, eight body lines/definition, nine macro arguments, one invocation/expansion depth, and `SOURCE_LINE_BUFFER_CAPACITY = 512`. The state byte-count formula currently resides in `constants.asm`; state storage resides in `state.asm`.
- Existing Item 7 sequence: the governed reference-parity plan marks Item 7.3 complete and leaves 7.4 segment semantics, 7.5 statement capture, 7.6 statement expansion, and 7.7 exports open. This plan supplies the stabilization prerequisite before 7.4.

## Constraints

- Every native behavior item loads `agents/rules/native-rust-parity-porting.md`, creates/upgrades a `documentation/plans/slices/*.toml` boundary contract, and uses Levels A–E honestly. Assembly changes also load `agents/rules/native-68000.md`; FS-UAE execution loads `agents/rules/fs-uae.md`; investigation/instrumentation loads the applicable triage/safe-instrumentation rules.
- The macro fixture is a permanent Level D CLI-artifact regression during every preprocessor ownership change. The real-68020 nested-frame harness is a separate Level D focused-negative proof, not CLI-artifact parity; other internal harnesses are B/C/E only and never substitute for CLI parity.
- Ownership-only items must preserve ABI, register/CCR/stack contracts, state layout, capacities, accepted source, diagnostics, and exact Rust/native fixture artifacts. No segment, statement, export, or unrelated macro feature may be bundled into them.
- The preprocessor must not contain CPU/family/dialect, selector, operand-plan, mnemonic encoding, or output-format decisions. Run the CPU-boundary guard; do not mix the independent Rust selector-boundary remediation into this plan.
- A failed expansion must either be rolled back to its pre-call observable state or terminate the current CLI invocation with a deterministic diagnostic before later source/session processing observes partial state. Never silently truncate, fall through to PRVM, or use a test-only product branch.
- One active checkbox item at a time. Each item ends in exactly one focused commit after its gates and `plan-compliance-reviewer` PASS. A failed Level D invariant pauses the item and inserts a separate one-invariant remediation item before continuation.
- This plan is not active for implementation until its `plan-quality-reviewer` PASS sidecar exists and the metadata validation status is updated. A plan-compliance PASS is additionally required before every plan-driven commit.
- Macro-affecting source items use a mandatory two-phase evidence pattern: before the source commit, `run_native_macro_completion.sh --verify` runs both configured Level D tests and fails on missing configuration, skip, or non-exact execution while permitting the staged worktree; after that source commit, the clean-tree `--manifest` mode creates a fresh source-identity receipt and an evidence-only follow-up commit. The next implementation item may not start until its receipt validates. A receipt is baseline evidence only for its recorded source identity; it never proves a later extraction/change.

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
| 6, 6.1 | existing Rust test behavior is authority | evidence metadata and Rust test-module boundaries | unchanged test filters/results; no product semantic output | B classification/source list; D wrapper proves retained macro authority; E tools explicitly non-production |
| 7 | completed stabilization records and parent Item 7 scope | governed plan/slice dependency boundary | green evidence → parent prerequisite only; does not activate a feature | B artifact validation; D current macro receipt; E none |

### Item-specific supplements

- Item 2: Rust `MacroProcessor::expand_lines`; native `opforgeNativeCliCaptureMacroDefinitionLineV1`/`opforgeNativeCliFinishMacroDefinitionsV1`; captured macro lines → unchanged stored body/status; approved path is only pre-tokenizer capture.
- Item 2.1: Rust `parse_macro_invocation`, `parse_macro_args`, `build_macro_args`; native `opforgeNativeCliParseMacroInvocationV1`/`opforgeNativeCliBeginMacroInvocationFrameV1`; dotted invocation → unchanged complete frame or deterministic failure; approved path is before source recording/PRVM.
- Item 2.2: Rust `substitute_line`; native `opforgeNativeCliSubstituteMacroBodyLineV1`; captured body/bound frame → unchanged expansion buffer/status; approved path is substitution only, not frontend re-entry.
- Item 2.3: Rust macro token-boundary parsing; native `lineStartsWithMacroDirective`, `lineStartsWithEndmacroDirective`, invocation-name/directive scanners; same source text → same token-boundary decision; approved path is scanner helper replacement only.
- Item 2.4: Rust `expand_lines`/block formatting; native `opforgeNativeCliBeginExpandedLineV1`/`opforgeNativeCliEndExpandedLineV1`; expansion text → same staged/restored caller line; approved path is source staging only. Items 2–2.4 share the declared fixed-capacity and no-new-directive non-equivalences.
- Item 4.1: Rust macro definition parsing; native scanner owner exported by Item 2.3; structural line → exact bounded directive/name classification; approved path is existing macro scanner routing only, no inactive-kind dispatch.
- Item 5.1: Rust macro errors where comparable; native constants/state capacity checks; within-limit request → unchanged state, over-limit request → deterministic pre-mutation failure; approved path is limits/checks only. Native fixed capacities remain the explicit non-equivalence.
- Item 6: non-production artifact work; authority is current slice proof declarations; boundary is harness metadata/comment/report classification; no input/output semantics change, and no harness becomes an approved production path.
- Item 6.1: non-production Rust test ownership only; authority is pre-move test function/filter/result set; boundary is test-module declarations; no native production boundary or behavior is changed, and exact test names remain approved completion-wrapper paths.

## Milestones and Dependency Graph

```text
M1 Macro baseline + evidence receipt
  -> M2 ownership-only native extraction
  -> M3 explicit expansion/scanner/transaction contracts
  -> M4 capacities, harness classification, Rust test ownership
  -> M5 readiness review
  -> follow-on: 7.4 segment capture -> segment expansion -> canonical segment Level D
               -> 7.5 statement capture -> 7.6 statement match/re-entry -> statement Level D
               -> 7.7 export representation -> module/import integration -> Item 7 closure
```

No follow-on feature starts until M5 passes. The follow-on sequence is planning
only in this artifact; it requires its own activated item/slice contracts.

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

- [ ] Item 2.1: extract invocation binding into its owner module without semantic change
  - Source requirement or finding IDs: SP-002; existing invocation-frame and binding slice contracts.
  - Expected files: `preprocessor.asm`, new `preprocessor_invocation.asm`, module references, focused tests/slice metadata.
  - Full quality gates: `cargo test -p asm native_preprocessor_macro_invocation_frame_is_bounded_and_resettable -- --nocapture`; `cargo test -p asm native_preprocessor_macro_invocations_bind_before_prvm_routing -- --nocapture`; Item 1 macro wrapper/validator; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for lookup/argument-binding extraction only.
  - Commit outcome: one ownership-only invocation module commit.
  - Definition of done: public parse result, fixed frame, errors, ABI, and macro Level D outputs are unchanged; SP-002 remains partially closed.

- [ ] Item 2.2: extract macro substitution into its owner module without semantic change
  - Source requirement or finding IDs: SP-002; macro substitution/reentry slice contract.
  - Expected files: `preprocessor.asm`, new `preprocessor_substitution.asm`, module references, focused tests/slice metadata.
  - Full quality gates: `cargo test -p asm native_preprocessor_macro_substitution_and_reentry_are_bounded -- --nocapture`; Item 1 macro wrapper/validator; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for byte-for-byte substitution ownership only.
  - Commit outcome: one ownership-only substitution module commit.
  - Definition of done: named/positional/full-list/default behavior and deterministic failures remain identical; SP-002 remains partially closed.

- [ ] Item 2.3: extract bounded scanning into its owner module without semantic change
  - Source requirement or finding IDs: SP-002 and SP-005; existing macro definition/invocation tests.
  - Expected files: `preprocessor.asm`, new `preprocessor_scan.asm`, module references, focused tests/slice metadata.
  - Full quality gates: definition and invocation focused commands from Items 2/2.1; Item 1 macro wrapper/validator; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for moving existing scanner helpers only; no scanner behavior expansion.
  - Commit outcome: one ownership-only scanner module commit.
  - Definition of done: current token-boundary acceptance and malformed cases are unchanged; SP-002 remains partially closed.

- [ ] Item 2.4: extract expansion staging into its owner module without semantic change
  - Source requirement or finding IDs: SP-002 and SP-003; existing macro expansion slice contract.
  - Expected files: `preprocessor.asm`, new `preprocessor_expansion.asm`, `line_processor.asm` only for changed imports/calls, module references, focused tests/slice metadata.
  - Full quality gates: `cargo test -p asm native_preprocessor_macro_substitution_and_reentry_are_bounded -- --nocapture`; Item 1 macro wrapper/validator; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for staging/restore ownership only.
  - Commit outcome: one ownership-only expansion module commit.
  - Definition of done: source save/restore, depth rejection, ABI, and fixture bytes are unchanged; SP-002/SP-003 fully close.

- [ ] Item 3: formalize the expanded-line frontend contract
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

- [ ] Item 4: define an inert shared structural-definition record contract
  - Source requirement or finding IDs: SP-005 (one-off directive/name loops); SP-006 (macro/segment/statement storage must not diverge accidentally).
  - Invariant: one inert definition header/body contract expresses kind, name/signature, body span/count, owner module, visibility, label attachment, expansion policy, and scope-wrapping policy without enabling segment/statement behavior or changing existing macro storage.
  - Rust authority / native boundary: Rust macro processor definition parsing; native scan/definition modules and fixed state representation.
  - Expected files: definition module, `state.asm`, `constants.asm`, slice metadata, state-layout tests.
  - Proof: B native field-layout and inactive-kind ownership; C record-state model; D Item 1 macro wrapper. This item does not change the scanner or capture/expand segments/statements.
  - Full quality gates: Item 1 macro wrapper/validator; focused state-layout test; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for inert state/layout migration only.
  - Commit outcome: one record-layout commit, or a documented no-layout-change commit that records the existing structure as the shared contract.
  - Definition of done: unimplemented kinds cannot route through normal processing and existing macro state is identical; SP-006 fully closes.

- [ ] Item 4.1: replace duplicated structural scanners with the bounded scanner contract
  - Source requirement or finding IDs: SP-005; Item 4’s shared record contract.
  - Expected files: `preprocessor_scan.asm`, affected definition/invocation modules, scanner tests, slice metadata.
  - Invariant: one bounded scanner recognizes complete structural directives and names at token boundaries while preserving current macro acceptance/rejection and leaving segment/statement behavior inactive.
  - Exact proof commands: `cargo test -p asm native_preprocessor_macro_definitions_are_consumed_and_bounded -- --nocapture`; `cargo test -p asm native_preprocessor_macro_invocations_bind_before_prvm_routing -- --nocapture`; Item 1 macro wrapper/validator. New table-driven tests must cover mixed case, prefixes, comments, labels, quotes, truncated/max-length lines, and end-kind matching; each declares Level C and does-not-prove native execution.
  - Full quality gates: exact proof commands; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for scanner consolidation only.
  - Commit outcome: one scanner-migration commit.
  - Definition of done: malformed input fails deterministically, macro CLI output is unchanged, and SP-005 fully closes.

- [ ] Item 5: establish transactional expanded-line failure behavior
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

- [ ] Item 5.1: publish and enforce the native preprocessor resource budget
  - Source requirement or finding IDs: SP-008; Item 5 transaction contract.
  - Expected files: `constants.asm`, `state.asm`, affected owner modules, slice metadata, focused capacity tests/documentation.
  - Invariant: every declared definition/body/header/name/signature/argument/full-list/source/expansion/saved-line/depth capacity has one owner, lifetime/reset policy, and deterministic pre-mutation failure outcome.
  - Exact proof commands: new `cargo test -p asm native_preprocessor_capacity_matrix_is_deterministic -- --nocapture`; existing definition/invocation/substitution focused tests; Item 1 macro wrapper/validator. The matrix explicitly states shared/partitioned/cumulative and session-reset behavior, and its Level C declaration does not claim native 68020 execution.
  - Full quality gates: exact proof commands; native formatter; staged native-porting; Rust quality; workflow gate.
  - Plan-compliance review evidence: PASS for capacity bookkeeping/diagnostics only.
  - Commit outcome: one capacity-contract commit.
  - Definition of done: capacity failures never truncate/fall through and SP-008 fully closes; no segment/statement capacity is activated.

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

- [ ] Item 7: certify stabilization readiness and amend the parent parity plan before feature work
  - Source requirement or finding IDs: SP-011 (do not start Item 7.4 on an unreviewed foundation).
  - Invariant: the parent Item 7 sequence cannot begin segment capture until all stabilization contracts, baseline evidence, test ownership, and required receipts are green.
  - Expected files: this plan, `documentation/plans/opforge-native-cli-reference-parity-expansion-plan-v0_1.md`, relevant slice records, retained completion receipt, and plan-quality evidence.
  - Required follow-on sequence (not implementation in this item): (a) segment definition capture, (b) segment invocation/expansion, (c) canonical `macro_syntax.asm` Level D closure, (d) statement definition/signature storage, (e) statement matching/capture, (f) substitution and ordinary-route re-entry, (g) statement Level D closure, (h) preprocessor export representation, (i) module/import visibility/alias injection, (j) Item 7 coverage closure. Each feature is a separate slice/commit with a fresh Rust/native contract.
  - Follow-on proving ground: segment capture is first because it reuses definition storage/scanning but must not use macro scope wrapping; only after its capture proof passes may segment expansion run. The canonical segment fixture is not a substitute for the macro baseline.
  - Required risks and rollback: package/CPU boundary must remain untouched; preserve selector ordering and diagnostics by keeping preprocessor upstream of PRVM/opasm; rollback any extraction by reverting its single commit; do not retain adapters after their explicit deletion criterion is met.
  - Full quality gates: `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-native-preprocessor-stabilization-plan-v0_1.md`; `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-native-cli-reference-parity-expansion-plan-v0_1.md`; `python3 scripts/workflow/check_workflow_artifact_bundle.py --root . plan documentation/plans/opforge-native-preprocessor-stabilization-plan-v0_1.md`; `make workflow-gate`; current macro Level D receipt validation; final `plan-quality-reviewer` PASS.
  - Plan-compliance review evidence: PASS confirms the parent plan is amended only with validated prerequisites and no feature box is checked prematurely.
  - Commit outcome: M1–M4 are independently committed/green, this plan’s closure evidence is retained, and parent Item 7 explicitly depends on it.
  - Definition of done: the next active work is segment capture only and SP-011 fully closes.

## Blocking Rules

- No commit before all named quality gates pass and `plan-compliance-reviewer` returns `PASS`.
- Each work item or extraction phase ends in exactly one new focused commit before the next item begins; no feature semantics in Items 1–6.
- Exact FS-UAE commands must fail closed when required configuration is absent; no optional/skipped Level D result may close a macro-affecting item.
- If an ownership change changes macro output, diagnostics, acceptance, state layout, ABI, or a Level D result, stop; restore the macro baseline and add one separate remediation item with the first divergent boundary.
- No new CPU-specific logic, package bypass, selector/encoder logic, direct output encoding, silent truncation, harness-injected shortcut, or unclassified debug probe is permitted.
- Checkbox updates, slice metadata, retained evidence, required quality-gate sidecars, and plan-compliance review are mandatory bookkeeping. Archive this plan only when all items are complete using `scripts/workflow/archive_completed_plan.sh`.
