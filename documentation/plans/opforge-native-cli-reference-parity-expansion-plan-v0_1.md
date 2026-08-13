<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# opForge Native CLI Reference Parity Expansion Plan v0.1

## Metadata

- Source: User request on 2026-06-19 to create an official plan that expands native CLI FS-UAE parity coverage across the `examples/reference` corpus and makes that coverage mandatory for native implementation work.
- Mode: implementation
- Owner: Codex
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.
- Workflow revision source: Native porting workflow and quality framework implemented by `opforge-native-porting-workflow-spec-plan-v0_1.md`.
- Validation status: Deterministic plan validators and `make workflow-gate` pass; fresh single-agent `plan-quality-reviewer` returned `PASS` on 2026-07-04.

## Goal

Expand the native AmigaOS CLI validation path so every applicable example in the
existing example/reference corpus is represented by a native parity test that:

1. drives the native implementation through the CLI interface as a real user
   would;
2. runs through the existing FS-UAE-backed native test harness;
3. assembles the same source through the Rust implementation as the authority;
4. compares exact emitted outputs and error text between native and Rust; and
5. becomes part of the standard required test surface for native implementation
   work.

The immediate target is broad reference-backed parity, not new assembler
features. This plan exists to turn the existing reference corpus into a durable
native regression net.

The active execution scope of this plan is intentionally narrower than the full
corpus inventory:

- highest priority: `6502` and `65c02`
- next priority after that scope is fully green: opForge Core parity through
  `6502`/`65c02`-backed fixtures
- only after those active items are fully green should native mandatory gating
  be promoted for that active scope

All other CPU families are explicitly on hold in this plan until their native
implementation readiness is established by separate follow-on planning.

## Constraints

- This plan must not become active for plan-driven implementation until the
  plan-quality workflow returns `PASS`.
- Native parity must compare against the Rust implementation as the authority.
  Checked-in `examples/reference/**` artifacts define the corpus and expected
  artifact surfaces, but native success is determined by exact native-vs-Rust
  parity.
- The native path under test must stay the real CLI path. Do not satisfy this
  plan by calling internal native helper routines directly while bypassing the
  native CLI interface.
- Applicability is defined explicitly. Every reference-backed example must be
  either:
  - represented by a native parity case, or
  - represented by a reviewed exclusion entry with a concrete blocking reason.
- CPU-neutral and opForge Core examples that currently depend on 8080/Z80
  spellings may be duplicated into MOS-backed native parity fixtures first.
  Prefer additive copies over mutating the existing canonical examples.
- Error examples are part of parity scope where the native CLI can exercise the
  same failure path. Their native tests must compare deterministic diagnostic
  text against the Rust result, and keep checked-in `.err` references aligned
  when behavior intentionally changes.
- Items that add or refresh `.hex`, `.lst`, `.map`, or `.err` files must load
  `agents/rules/reference-refresh.md` and use the available
  `opforge-golden-reference-maintainer` implementation skill.
- Native 68000 implementation items must load `agents/rules/native-68000.md`
  and run the native formatter gate before completion.
- FS-UAE-backed items must load `agents/rules/fs-uae.md` and use the one-shot
  known-good invocation (`opt-in-allowed`) style documented there; the explicit
  opt-in is permitted only for these configured fail-closed Level D commands.
- Native Rust-to-68000 parity items must load
  `agents/rules/native-rust-parity-porting.md`. When a parity failure is under
  investigation they must also load
  `agents/rules/native-parity-failure-triage.md`, and before adding temporary or
  permanent instrumentation they must load
  `agents/rules/native-68000-safe-instrumentation.md`.
- Every native behavior fix must be one named invariant in one focused commit.
  Before production edits, record the required Rust/native boundary contract
  and add a machine-readable `native-rust-parity` slice metadata file under
  `documentation/plans/slices/`.
- Every test or observation used as parity evidence must declare proof Level
  A–E and state “This test proves” and “This test does not prove.” Level A–C
  evidence is the mandatory fast proof where technically possible; Level D is
  the real FS-UAE confirmation; Level E is localization only.
- A reduced or truncated fixture is a localization probe with proof Level E unless its semantic completeness is
  explicitly justified. Prefix progress, moved failures, and temporary probes
  do not establish parity.
- The deterministic staged native-porting gate must remain local and must not
  launch FS-UAE or use the network. Required Level D execution is a separate
  named, one-shot completion gate and must fail rather than silently skip when
  the configured native completion workflow requires it.
- Full native parity gating introduced by this plan must become mandatory for
  native implementation work, not an opt-in follow-up.
- `6502` and `65c02` are the only active family targets in this plan. No work
  should advance into other family corpora until the active `6502`/`65c02`
  parity items and their required gates are green.
- `motorola68000` parity remains on hold here until a separate full native
  implementation plan defines when the native CLI path is complete enough for
  honest reference-corpus parity.
- `motorola6800`/`6809` and `intel8080`/`z80` family parity remain optional and
  on hold here. They are intentionally last in priority order and should not be
  activated through this plan.

## Current Baseline

The current repository already has:

- a Rust-side reference harness in
  `crates/opforge-asm/src/tests.rs` via
  `examples_match_reference_outputs()`;
- an FS-UAE-backed native CLI harness in
  `crates/opforge-asm/src/fs_uae_smoke.rs`;
- focused native parity tests for selected MOS first-run and artifact slices;
- a much broader checked-in corpus than the native harness currently exercises.

Current example source counts under `examples/`:

- `mos6502`: 39
- `opcore`: 91
- `motorola68000`: 48
- `motorola6800`: 5
- `z80`: 2

Current checked-in reference artifact counts under `examples/reference/`:

- `mos6502`: 79
- `opcore`: 148
- `motorola68000`: 91
- `motorola6800`: 10
- `z80`: 4

The plan therefore focuses on test-system shape first, then `6502`/`65c02`
coverage, then opForge Core coverage adapted onto that active family scope, and
finally mandatory gating for that active scope only.

## Version Impact

- Affected component(s): native CLI parity harness, FS-UAE native tests, example/reference coverage policy, native-quality gate workflow
- Impact class: patch
- Owned contract: native CLI must produce Rust-authoritative exact outputs across the declared applicable example/reference corpus
- Rationale: native implementation work currently relies on a narrower set of focused tests than the existing example/reference corpus can support; this plan closes that gap and promotes the resulting parity net into the standard required gate for native work

## Current Status

- Items 1 through 3 are historically implemented in commits `9d29ed41` and
  `2cd2378a`, but they are not closed under the current native-porting quality
  framework.
- Item 4 is complete: RQ-001 through RQ-010 classify the retrospective gaps,
  and Items 4.1 through 4.8 provide the ordered closure path.
- Item 4.1 is complete: manifest evidence is classified at Level A and its
  duplicate, unknown-scope, prefix-precedence, and broad-prefix limitations are
  protected.
- Item 4.2 is complete: the Rust CLI receives exact example source bytes and
  generates the live binary/PRG oracle used by Level D parity. Item 4.3 is the
  next active item.
- The remaining opForge Core expansion has been decomposed into Items 5 through
  9 so each coverage commit owns one coherent corpus surface. Item 10 promotes
  only the framework-closed active scope into the mandatory native completion
  gate.
- If an Item 6–9 coverage shard reveals a native/Rust divergence, that coverage
  item pauses. A new one-invariant remediation item must be inserted into this
  plan and completed under the native porting workflow before the coverage
  shard resumes.

## Native Porting Execution Contract

For every remaining item that changes native behavior:

1. Create the boundary contract required by
   `agents/rules/native-rust-parity-porting.md`, naming the Rust reference
   functions, native boundary, inputs, outputs, known non-equivalences, fast
   proof, and FS-UAE proof.
2. Create one slice metadata file under `documentation/plans/slices/` with the
   named invariant, approved production paths, and every evidence-bearing test
   classified at proof Level A–E.
3. Reproduce a real failure once at Level D, then maintain a hypothesis ledger
   and locate the first divergent boundary in the prescribed source-to-output
   order.
4. Add the mandatory Level B or C host-side contract regression before patching
   the first divergent native boundary. If a host proof is technically
   impossible, stop and amend this plan with the concrete reason and replacement
   proof.
5. Use only the safe debug-contract framework for instrumentation. Temporary
   probes remain Level E and must be removed; stable assertions must use a
   canonical contract ID and document preservation of registers, SR/CCR, stack,
   branch behavior, and non-overlapping buffers.
6. Run focused host proofs, applicable negative or boundary cases, the native
   formatter gate, the full Rust quality gate, the staged native-porting gate,
   and the exact named FS-UAE test with `--test-threads=1`.
7. Obtain `plan-compliance-reviewer` `PASS`, commit exactly the named invariant,
   and only then resume corpus expansion.

Coverage-only commits may batch already-green manifest cases within one
coherent shard. They must not contain speculative native fixes. Any discovered
behavior fix follows the seven-step contract above as a separate plan item and
commit.

## Retrospective Items 1–3 Closure Matrix

The statuses below describe evidence under the current framework. “Historically
landed” is not equivalent to “framework-closed.”

| ID | Historical claim | Current evidence | Status | Required closure |
|---|---|---|---|---|
| RQ-001 | Item 1 has an explicit applicability manifest and deterministic completeness guard. | Level A `native_reference_manifest_*` and module tests exercise uniqueness, seed retention, and corpus accounting with explicit proof limitations. | Closed by Item 4.1. | No further retrospective work. |
| RQ-002 | Item 1 exclusions are concrete and every current example is accounted for. | Level A tests cover unknown new scope, duplicate paths, overlapping-prefix precedence, and the fact that broad-prefix accounting is not semantic applicability proof. | Closed by Item 4.1. | Semantic applicability remains a reviewed planning decision, not an inferred test result. |
| RQ-003 | Item 2 compares native outputs with Rust as the authority. | Level A/B tests build every schema case from exact example source bytes, and the named Level D test compares native CLI binary/PRG artifacts with artifacts emitted by the live Rust CLI in the same run. | Closed for successful binary/PRG cases by Item 4.2. | Text, map, and deterministic failures remain RQ-004. |
| RQ-004 | Item 2’s generic runner covers bytes, text artifacts, maps, and deterministic errors. | Item 4.3 adds live-Rust listing parity. Item 4.4 adds a live-Rust unknown-mnemonic oracle, stable semantic diagnostic normalization, negative comparator coverage, and Level D native status/diagnostic parity. The native CLI has no map-output mode. | Closed for the implemented binary, PRG, listing, and deterministic-error surface; maps explicitly on hold. | Map parity requires a separately authorized native map-output feature plan. |
| RQ-005 | Item 3 covers the applicable `6502`/`65c02` corpus through the real CLI. | Seven manifest cases execute real CLI commands under FS-UAE against live Rust artifacts. Item 4.7 records that this applicable set is limited to the listed `6502`/`65c02` cases; other families and mixed-CPU input remain excluded because the native CLI slice does not implement them. | Closed for the declared applicable set. | Broader families require separate native implementation scope. |
| RQ-006 | Item 3’s native changes preserve expression operand fallback semantics. | Item 4.5 records the boundary, locks both fallback branches, covers missing/malformed metadata decisions at Level C, and confirms exact-source output parity under FS-UAE. | Closed. | No runtime assertion is used because the recovery condition must remain non-fatal. |
| RQ-007 | Item 3’s source `.cpu` and parser-routing changes match Rust. | Item 4.6 corrected bootstrap quote normalization/tail preservation, locks the native routing order, covers quote/trailing boundaries at Level C, and confirms quoted success plus malformed rejection in FS-UAE. | Closed. | The Rust authority and examples remain unchanged. |
| RQ-008 | Item 3 suppresses implementation/debug progress during normal CLI use without changing control flow. | Item 4.7 inventories all progress sites, locks their debug-flag dominance, models enabled/disabled markers, and proves identical native bytes with isolated output under paired FS-UAE runs. | Closed. | Product help/version/error diagnostics remain intentionally ungated. |
| RQ-009 | Items 2 and 3 ended in separate focused commits with plan-compliance evidence. | Both landed together in `2cd2378a`; no matching retained plan-compliance artifact was found. Item 4.8 permanently records this immutable exception; Items 4.1–4.8 each end in their own reviewed commit. | Closed as a recorded historical exception. | Do not rewrite history or treat the combined commit as precedent. |
| RQ-010 | Required Level D evidence cannot silently skip. | `scripts/workflow/run_native_reference_retrospective_completion.sh` requires explicit FS-UAE opt-in, binary, config, and arguments, then runs every retrospective Level D proof serially. Its focused test proves missing configuration fails. | Closed. | Optional individual tests may still skip; the completion wrapper may not. |

## Work Items

- [x] Item 1: add a governed native reference applicability manifest and completeness guard
  - Historical implementation status: landed in `9d29ed41`; current-framework closure remains blocked on Item 4 and its resulting remediation items
  - Source requirement or finding IDs: user request for “each applicable test” coverage; current `examples_match_reference_outputs()` corpus definition in `crates/opforge-asm/src/tests.rs`; existing native CLI FS-UAE tests in `crates/opforge-asm/src/tests.rs`; existing helper surface in `crates/opforge-asm/src/fs_uae_smoke.rs`
  - Expected files:
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
    - `crates/opforge-asm/src/native_reference_parity.rs` or equivalent extracted helper module if needed
  - Full quality gates:
    - `cargo test -p asm native_reference_ -- --nocapture`
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-native-cli-reference-parity-expansion-plan-v0_1.md`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to manifest structure, applicability accounting, and deterministic completeness enforcement with no family coverage expansion yet
  - Commit outcome:
    - the repo has one explicit native parity manifest for reference-backed examples, plus a failing completeness guard for any applicable example that is neither covered nor explicitly excluded
  - Definition of done:
    - every applicable example/reference entry must be represented by a `NativeReferenceCase`-style record or a reviewed exclusion record
    - exclusions carry concrete blocking reasons rather than “future work” placeholders
    - the guard fails deterministically when new reference-backed examples are added without native parity accounting

- [x] Item 2: generalize the FS-UAE native CLI harness into a schema-driven native-vs-Rust parity runner
  - Historical implementation status: landed together with Item 3 in `2cd2378a`; current-framework closure remains blocked on Item 4 and its resulting remediation items
  - Source requirement or finding IDs: user requirement that native tests behave “just like a user would do it” through the CLI; existing focused helpers in `crates/opforge-asm/src/fs_uae_smoke.rs`; Rust authority in `run_with_cli_with_context` and `examples_match_reference_outputs()`
  - Expected files:
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/native_reference_parity.rs` or equivalent helper module if extracted
  - Full quality gates:
    - `cargo test -p asm external_fs_uae_opforge_native_cli_ -- --nocapture --test-threads=1`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to harness generalization, Rust oracle comparison, and exact artifact/error comparison without adding broad corpus shards yet
  - Commit outcome:
    - the native harness can execute manifest-driven parity cases and compare exact native-vs-Rust payloads for bytes, text artifacts, map files, and deterministic error output
  - Definition of done:
    - one generic parity path replaces new ad hoc item-specific native helpers for future reference-backed expansion
    - listing normalization matches the current Rust reference-harness policy where banner/profile-only differences are already intentionally normalized
    - the native runner can stage full file trees, module roots, and artifact outputs as needed per case

- [x] Item 3: cover the full applicable `6502` and `65c02` reference corpus through native CLI parity shards
  - Historical implementation status: landed together with Item 2 in `2cd2378a`; current-framework closure remains blocked on Item 4 and its resulting remediation items
  - Source requirement or finding IDs: current `6502` and `65c02` examples and references under `examples/mos6502/**` and `examples/reference/mos6502/**`; existing first-run/native CLI focused tests; user request to add as much reference-backed native coverage as possible while prioritizing `6502` first; user requirement that parity must be measured from actual `opforge_cli` CLI-argument invocations issued from a script or command prompt, with emulation allowed, and that only the artifacts written by `opforge_cli` itself are valid comparison inputs against the Rust outputs for this scope
  - Expected files:
    - `native/motorola68000/amigaos/opforge-cli/**`
    - `native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
    - `crates/opforge-asm/src/native_reference_parity.rs` or equivalent helper module if extracted
  - Full quality gates:
    - `cargo test -p asm native_reference_6502_ -- --nocapture`
    - `cargo test -p asm native_reference_65c02_ -- --nocapture`
    - `cargo test -p asm external_fs_uae_opforge_native_cli_ -- --nocapture --test-threads=1`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to `6502`/`65c02` parity expansion with no expansion into `65816`, `45gs02`, `motorola68000`, `motorola6800`/`6809`, or `intel8080`/`z80`
  - Commit outcome:
    - all applicable `6502` and `65c02` examples are represented by native CLI parity tests or reviewed exclusions, the native CLI accepts and honors the Rust CLI argument surface needed by that shard when invoked through actual CLI arguments from a script or command prompt under FS-UAE or equivalent emulation, and that shard becomes the stable baseline native gate candidate
  - Definition of done:
    - success examples compare exact native-vs-Rust payloads for the declared artifact types
    - error examples compare exact native-vs-Rust deterministic diagnostics where the native CLI can reach the same path
    - first-run, artifact-matrix, and broader `6502`/`65c02` examples live under one shard structure instead of a loose mix of one-off tests
    - prompt-driven or script-driven native CLI invocations can assemble the active shard examples under emulation when explicit output arguments are supplied, rather than only through harness-injected source cases
    - the native CLI accepts and honors the Rust CLI argument surface required by the active shard scope, including input selection, CPU override, include/module roots, and declared output arguments for byte and text artifacts
    - parity comparisons for this item use only the files and text artifacts written by `opforge_cli` from those actual CLI-argument invocations as the native side of the comparison
    - `65816` and `45gs02` examples remain explicit on-hold exclusions in the applicability manifest rather than being silently folded into the active shard

- [x] Item 4: audit and decompose the retrospective quality closure for Items 1–3
  - Source requirement or finding IDs: historical commits `9d29ed41` and `2cd2378a`; current `agents/rules/native-rust-parity-porting.md`; current `agents/rules/native-parity-failure-triage.md`; missing proof-level declarations and slice metadata; schema-driven Level D test currently sourcing expected payloads from checked-in `.hex` references; Items 2 and 3 sharing one historical commit
  - Expected files:
    - this plan
    - `documentation/plans/opforge-native-cli-reference-parity-expansion-plan-v0_1.md.quality-gate.txt`
  - Full quality gates:
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-native-cli-reference-parity-expansion-plan-v0_1.md`
    - `python3 scripts/workflow/check_workflow_artifact_bundle.py plan documentation/plans/opforge-native-cli-reference-parity-expansion-plan-v0_1.md`
    - `make workflow-gate`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a documentation-only audit that makes no retroactive proof claims and maps each gap to one bounded remediation item
  - Commit outcome:
    - Items 1–3 have an explicit claim-by-claim closure matrix and the plan contains one ordered, commit-sized remediation item for every unmet current-framework requirement
  - Definition of done:
    - each original Item 1–3 definition-of-done claim is classified as proven, partially proven, unproven, or superseded
    - ordinary Rust assertions are distinguished from native debug-contract assertions
    - the audit identifies where a live Rust CLI oracle must replace or supplement checked-in reference payloads
    - every evidence-bearing test is assigned a target proof level and required “This test proves” / “This test does not prove” text
    - every historical native behavior change is mapped to a Rust/native boundary contract and a focused host proof, or to a concrete documented reason that such proof is impossible
    - stable `DEBUG_ASSERT_*` or `DEBUG_EVENT_*` additions are requested only where they protect a named boundary invariant; assertions are not added decoratively
    - the historical combined Item 2/3 commit is recorded as immutable history and is not treated as precedent for future multi-item commits
    - no code, native assembly, fixture, or reference change is included in this audit commit

- [x] Item 4.1: classify Item 1 manifest evidence and harden its accounting boundaries
  - Source requirement or finding IDs: RQ-001 and RQ-002; fully closes proof-declaration and accounting-boundary gaps while preserving manifest behavior
  - Expected files:
    - `crates/opforge-asm/src/native_reference_parity.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - `cargo test -p asm native_reference_ -- --nocapture`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for Item 1 evidence classification and accounting boundary tests only
  - Commit outcome:
    - manifest tests declare Level A, what they prove, and what they do not prove; precedence, duplicate, newly added unaccounted path, and broad-prefix limitations are explicit
  - Definition of done:
    - no native production path or FS-UAE helper changes
    - current corpus accounting remains green
    - semantic applicability is not inferred merely from a prefix match

- [x] Item 4.2: replace reference-derived schema expectations with a live Rust CLI binary oracle
  - Source requirement or finding IDs: RQ-003 and the binary portion of RQ-005; fully closes live-oracle parity for successful binary/PRG schema cases
  - Expected files:
    - `crates/opforge-asm/src/native_reference_parity.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/fs_uae_smoke.rs` only if the existing result surface cannot carry the required CLI artifacts
    - `crates/opforge-asm/tests/fixtures/native_cli_reference_parity_schema.json`
  - Full quality gates:
    - focused Level A Rust-oracle test
    - focused Level B schema contract test
    - exact named Level D FS-UAE schema binary test with `--nocapture --test-threads=1`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for live Rust CLI oracle replacement on successful binary/PRG cases only
  - Commit outcome:
    - each native schema binary is compared with output produced during the same test by the Rust CLI authority; checked-in references remain an independent corpus check
  - Definition of done:
    - tests declare Levels A, B, and D and their limitations
    - no checked-in `.hex` decoding is used as the native-vs-Rust oracle
    - exact files under `examples/**` and `examples/reference/**` remain unchanged and are passed unmodified to the Rust CLI
    - FS-UAE still exercises actual native CLI argument strings and CLI-written artifacts

- [x] Item 4.3: add schema contracts and native remediation for listing-text parity
  - Source requirement or finding IDs: listing-text portion of RQ-004; map parity is explicitly on hold because the native CLI exposes no map-output mode
  - Expected files:
    - `crates/opforge-asm/src/tests.rs`
    - `native/motorola68000/amigaos/opasm/opasm_engine.asm`
    - `native/motorola68000/amigaos/opasm/opasm_output_artifacts.asm`
    - `native/motorola68000/amigaos/opforge-cli/assembly_session.asm`
    - `native/motorola68000/amigaos/opforge-cli/constants.asm`
    - `documentation/plans/slices/native-porting-slice-cli-listing-parity.toml`
  - Full quality gates:
    - focused Level A/B artifact-schema tests
    - exact named Level D FS-UAE listing artifact test with `--nocapture --test-threads=1`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for listing schema support and the bounded native listing remediation
  - Commit outcome:
    - the generic schema runner supports exact normalized listing comparison using live Rust output
  - Definition of done:
    - every normalization is named and justified
    - missing, extra, and mismatched artifacts fail deterministically
    - native comparison inputs are only artifacts written by `opforge_cli`
    - exact files under `examples/**` and `examples/reference/**` remain unchanged
    - `.org` remains present in both authority input and native listing output
    - map parity is not claimed or simulated; it remains on hold until native map output is separately authorized

- [x] Item 4.4: add schema contracts for deterministic failure parity
  - Source requirement or finding IDs: diagnostic portion of RQ-004; fully closes the advertised error surface
  - Expected files:
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
    - `crates/opforge-asm/tests/fixtures/native_cli_reference_parity_schema.json`
  - Full quality gates:
    - focused Level A/B diagnostic-schema tests
    - exact named Level D FS-UAE diagnostic schema test with `--nocapture --test-threads=1`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for deterministic status/stdout/stderr comparison without a native behavior fix
  - Commit outcome:
    - an expected-failure schema case compares Rust/native failure status and the normalized unknown-mnemonic semantic class
  - Definition of done:
    - host launcher failures remain distinguishable from guest CLI failures
    - at least one positive and one intentionally mismatched schema unit test protect the comparator
    - any discovered native divergence pauses this item for a new one-invariant remediation item

- [x] Item 4.5: close the expression metadata fallback invariant from Item 3
  - Source requirement or finding IDs: RQ-006; fully closes only stored-expression metadata fallback parity
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - `native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm`
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - focused Level B/C fallback contract tests including missing and malformed metadata boundaries
    - exact named Level D FS-UAE expression fallback confirmation with `--nocapture --test-threads=1`
    - `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for one expression fallback invariant and no parser, selector, or output expansion
  - Commit outcome:
    - the historical fallback behavior has a Rust/native boundary contract, focused fast proof, justified assertion decision, and Level D confirmation
  - Definition of done:
    - one first divergent boundary is named
    - any stable assertion uses a canonical contract ID and proves preservation; otherwise the metadata records why no runtime assertion is appropriate
    - no unrelated expression grammar support is added

- [x] Item 4.6: close the source CPU normalization and parser-routing invariant from Item 3
  - Source requirement or finding IDs: RQ-007; fully closes quoted `.cpu` token normalization and routing to the package-backed parser
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - narrowly required files under `native/motorola68000/amigaos/opforge-cli/`
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - focused Level B/C source CPU and directive-routing contract tests
    - exact named Level D FS-UAE source CPU test with `--nocapture --test-threads=1`
    - `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for one source CPU/parser-routing invariant and no output or module behavior changes
  - Commit outcome:
    - quoted and bare source CPU forms reach the same Rust-authoritative pipeline selection with malformed/trailing-token boundaries protected
  - Definition of done:
    - the boundary contract names tokenizer/parser/session transitions
    - tests declare proof levels and limitations
    - assertion placement, if any, protects the normalized token or route result without encoding CPU semantics in generic paths

- [x] Item 4.7: close normal-output isolation from native debug progress
  - Source requirement or finding IDs: RQ-008; fully closes debug-output isolation while recognizing `9ea4b98e` as partial superseding evidence
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - narrowly required files under `native/motorola68000/amigaos/opforge-cli/`
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - focused enabled/disabled Level B/C preservation tests
    - exact named Level D FS-UAE normal-output and `--native-debug` confirmation with `--nocapture --test-threads=1`
    - `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for output isolation only, with no CLI feature or parity expansion
  - Commit outcome:
    - normal successful CLI output contains no implementation progress records; debug mode uses only the approved safe framework where structured events are retained
  - Definition of done:
    - all historical debug-gating sites are inventoried
    - release and debug paths preserve registers, SR/CCR, stack balance, and following branch behavior
    - free-form probes are removed or documented as product diagnostics rather than instrumentation

- [x] Item 4.8: record fail-closed Level D closure for Items 1–3
  - Source requirement or finding IDs: RQ-005, RQ-009, and RQ-010; closes retrospective evidence only after Items 4.1–4.7 pass
  - Expected files:
    - this plan
    - the repository-native completion wrapper or workflow evidence selected for fail-closed Level D execution
  - Full quality gates:
    - all focused tests from Items 4.1–4.7
    - exact configured Level D schema and boundary tests with `--test-threads=1`
    - `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
    - `make workflow-gate`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for retrospective closure evidence and gate wiring only
  - Commit outcome:
    - Items 1–3 are explicitly framework-closed, the historical combined commit exception is recorded, and missing FS-UAE configuration fails the required completion command
    - required command: `scripts/workflow/run_native_reference_retrospective_completion.sh`
  - Definition of done:
    - every RQ finding is marked closed with named evidence
    - no historical commit is rewritten
    - Item 5 remains blocked until this item commits

- [x] Item 5: classify the opForge Core corpus into commit-sized native parity shards
  - Source requirement or finding IDs: user note that some opForge Core cases currently use 8080/Z80 spellings and may need `6502`-adapted copies first; existing `examples/opcore/**` and `examples/reference/opcore/**` corpora; current Rust-side reference harness behavior; native porting workflow requirement that one active slice own one coherent invariant
  - Expected files:
    - `crates/opforge-asm/src/native_reference_parity.rs`
    - this plan, if the inventory requires narrower shard boundaries or new one-invariant remediation items
  - Full quality gates:
    - `cargo test -p asm native_reference_ -- --nocapture`
    - `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-native-cli-reference-parity-expansion-plan-v0_1.md`
    - `python3 scripts/workflow/check_workflow_artifact_bundle.py plan documentation/plans/opforge-native-cli-reference-parity-expansion-plan-v0_1.md`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
    - `make workflow-gate`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for inventory, applicability accounting, and shard assignment only, with no fixture adaptation or native behavior changes
  - Commit outcome:
    - every applicable opcore source/reference entry is assigned to exactly one of Items 6–9 or has a reviewed exclusion with a concrete blocker
  - Definition of done:
    - the manifest distinguishes direct CPU-neutral staging from additive MOS-backed adaptation
    - no entry is assigned through a prefix-only assumption
    - each shard has a bounded artifact surface and named Level D test
    - any already-known red parity case is represented by a separate inserted remediation item rather than hidden inside a coverage shard
  - Inventory outcome:
    - every checked-in `.asm` and `.inc` path is assigned explicitly; support files name their owning root and must share its shard
    - every checked-in reference artifact is owned by one root; `README.md` and the shared diagnostic schema are exact-path exclusions with concrete reasons
    - the four bounded Level D tests are named `native_reference_opcore_syntax_expression_fs_uae`, `native_reference_opcore_module_macro_statement_fs_uae`, `native_reference_opcore_layout_output_fs_uae`, and `native_reference_opcore_diagnostic_fs_uae`
    - additive adaptation is required where a canonical source embeds non-MOS CPU selection, mnemonics, or operand syntax; canonical Rust examples and references remain immutable

- [x] Item 5.1: remediate column-one native directive routing
  - Source requirement or finding IDs: Item 6 Level D red case on `examples/opcore/for_counter_basic.asm`; native fallback recorded `.cpu` as a label when PRVM also exposed its operand expression
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - `native/motorola68000/amigaos/opforge-cli/assembly_session.asm`
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - focused Level B source-lock test
    - exact named Level D `native_column_one_directive_routing_fs_uae` with `--nocapture --test-threads=1`
    - `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for column-one directive routing only
  - Commit outcome:
    - dot-prefixed column-one tokens route as directive mnemonics before expression-bearing bare-label heuristics
  - Definition of done:
    - the canonical opcore source and all Rust examples/references remain unchanged
    - Level D no longer emits `LABEL .cpu`
    - no directive-specific semantics are added to generic native paths

- [x] Item 5.2: add native counted-repetition source expansion
  - Source requirement or finding IDs: Item 6 Level D failure on canonical `for_counter_basic.asm`; Rust authority in `repetition.rs::evaluate_for_plan` and `repetition_driver.rs`; native first divergence is between parsed statement storage and pass-one execution because `.for` blocks remain unexpanded
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - narrowly required native source-expansion/session files under `native/motorola68000/amigaos/opforge-cli/`
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - focused Level A/B/C tests for zero, one, and bounded counted repetition
    - exact Level D `native_opcore_counted_for_fs_uae` with `--nocapture --test-threads=1`
    - `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for counted `.for` expansion only
  - Commit outcome:
    - native source expansion consumes a matched `.for <count>` / `.endfor` block and presents the repeated body to the ordinary statement pipeline
  - Definition of done:
    - zero-count and nested-boundary behavior match Rust
    - the iteration limit fails closed
    - `.org` and body source text are never stripped or rewritten

- [x] Item 5.3a: add native compile-time sequence assignment storage
  - Source requirement or finding IDs: Level E localization of `for_collection_basic.asm` showed `nums = {1, 3, 5, 7}` stored as label `nums` plus unknown mnemonic `=` before iterable planning
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - a bounded generic native compile-time-value module and session wiring
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - focused Level B/C assignment parsing, capacity, and session-reset tests
    - exact Level D `native_opcore_sequence_assignment_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for sequence assignment storage only
  - Commit outcome:
    - native list assignments are consumed as compile-time value definitions rather than pass-engine mnemonics
  - Definition of done:
    - identifier and numeric element bounds fail closed
    - stored values reset once per session and remain stable across both passes
    - no iterable control, indexing, or `.len` behavior is bundled

- [x] Item 5.3b: add native iterable repetition values and loop-variable binding
  - Source requirement or finding IDs: Item 6 assignments `for_collection_basic.asm` and `ranges_lists_basic.asm`; Rust authority in `repetition.rs::evaluate_for_plan`, `AsmValue::List`, and `AsmValue::Range`
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - narrowly required native opcore expression/value and source-expansion files
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - focused Level A/B/C list, inclusive-range, stepped-range, and loop-binding tests
    - exact Level D `native_opcore_iterable_for_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, and full Rust quality gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for iterable `.for` values and binding only
  - Commit outcome:
    - native `.for <name> in <list-or-range>` binds each scalar iteration value and expands the complete body
  - Definition of done:
    - list indexing, `.len`, inclusive ranges, and explicit steps used by the assigned sources match Rust
    - zero steps and direction mismatches fail deterministically
    - no CPU semantics enter the generic value layer

- [x] Item 5.4: add native condition-based repetition
  - Source requirement or finding IDs: Item 6 assignment `while_basic.asm`; Rust authority in `repetition.rs::evaluate_while_condition` and `repetition_driver.rs`
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - native source-expansion and expression-session files
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - focused Level A/B/C false-first, current-address, and iteration-limit tests
    - exact Level D `native_opcore_while_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, and full Rust quality gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for `.while` expansion only
  - Commit outcome:
    - native `.while` reevaluates its condition at each body boundary against current session state
  - Definition of done:
    - `$` observes the same current address as Rust
    - false-first loops emit nothing
    - the iteration limit prevents non-terminating expansion

- [x] Item 5.5: add native conditional and match branch selection
  - Source requirement or finding IDs: Item 6 assignment `cond_syntax.asm`; Rust authority in `asmline_conditionals.rs`
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - native conditional/source-routing files
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - focused Level A/B/C nested `.if`/`.elseif`/`.else` and `.match`/`.case`/`.default` tests
    - exact Level D `native_opcore_conditionals_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, and full Rust quality gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for conditional branch selection only
  - Commit outcome:
    - native processing records and assembles only the selected conditional branches while preserving nesting
  - Definition of done:
    - skipped branches cannot define symbols or emit bytes
    - labelled conditional directives preserve Rust label behavior
    - unmatched/unterminated blocks fail deterministically

- [x] Item 5.6: add native block and namespace symbol qualification
  - Source requirement or finding IDs: Item 6 assignments `scopes.asm` and `scopes_namespace.asm`; Rust authority in `asmline_directives_scope.rs::route_scope_directive_ast`
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - native symbol/session and directive-routing files
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - focused Level A/B/C nested qualification, shadowing, `.bend`, and `.endn` tests
    - exact Level D `native_opcore_scopes_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, and full Rust quality gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for generic scope qualification only
  - Commit outcome:
    - native symbol definition and lookup use the same active block/namespace qualification order as Rust
  - Definition of done:
    - local shadowing and fully qualified lookup match Rust bytes
    - close aliases are equivalent
    - scope state is reset between sessions and passes

- [x] Item 5.7: add native struct values, instances, and scoped repetition
  - Source requirement or finding IDs: Item 6 assignments `struct_literal_instance_basic.asm`, `struct_var_instance_basic.asm`, and `bfor_labeled_struct_basic.asm`; Rust authority in `asmline_directives_scope.rs::process_struct_mode_statement_ast` and scoped repetition handling
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - native generic value/symbol/source-expansion files
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - focused Level A/B/C field layout, literal instance, mutable instance, member access, and `.bfor` scope tests
    - exact Level D `native_opcore_structs_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, and full Rust quality gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for struct/value semantics and `.bfor` ownership only
  - Commit outcome:
    - native compile-time values represent struct definitions and instances with deterministic field offsets and scoped repeated labels
  - Definition of done:
    - `.byte ?`/`.word ?` fields affect layout without emitting definition bytes
    - `.const`, `.var`, and `.set` instance behavior matches Rust
    - indexed `.bfor` member access matches the canonical artifact

- [x] Item 5.8: add native text-encoding definition and emission semantics
  - Source requirement or finding IDs: Item 6 assignments `text_encoding.asm` and `text_encoding_definitions.asm`; Rust authority in `asmline_directives_text.rs`
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - native text-encoding/value/output files
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - focused Level A/B/C built-in selection, `.encode` cloning, `.cdef`, `.tdef`, `.edef`, `.text`, `.null`, and `.ptext` tests
    - exact Level D `native_opcore_text_encoding_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, and full Rust quality gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for text encoding only
  - Commit outcome:
    - native text emission and source-defined encoding tables match Rust byte-for-byte
  - Definition of done:
    - ASCII and PETSCII switching matches Rust
    - cloned encodings and escape sequences are session-local
    - unknown encodings and malformed definitions fail deterministically

- [x] Item 5.9: add complete additive MOS adaptations for mixed-CPU syntax roots
  - Source requirement or finding IDs: Item 5 `AdditiveMosAdaptation` assignments `expr_syntax.asm`, `grouping.asm`, `syntax.asm`, and `testexpr.asm`; user prohibition on modifying Rust examples/reference code
  - Expected files:
    - additive fixtures outside `examples/**`
    - `crates/opforge-asm/src/native_reference_parity.rs`
    - focused tests in `crates/opforge-asm/src/tests/native_opcore_adapted_syntax.rs`
  - Full quality gates:
    - Level A semantic-completeness accounting for every canonical statement
    - Level B additive-fixture artifact checks
    - separate exact Level D `native_opcore_adapted_{expr_syntax,grouping,syntax,testexpr}_fs_uae` tests with `--nocapture --test-threads=1`
    - separate exact Level D `native_mos_forward_ref_stability_fs_uae` against the unchanged canonical mixed-CPU source and live Rust CLI artifact
    - reference-scope validator, native formatter, staged native porting gate, and full Rust quality gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for additive adaptation only, with no native parity fix bundled
  - Commit outcome:
    - reviewed MOS-backed copies preserve the complete CPU-neutral syntax/expression intent while replacing only non-MOS instruction and operand spellings
  - Definition of done:
    - every canonical line is mapped, retained unchanged, or explicitly justified as CPU-spelling adaptation
    - no canonical file or canonical reference changes
    - every `.org` remains present and semantically equivalent
  - Dependency note:
    - fixture adaptation and source-mapping evidence may be prepared in this item, but Item 5.9 is finalized only after Item 6 supplies the required native expression-semantics parity for the retained CPU-neutral expressions and its Level D shard is green

- [x] Item 5.9.1: restore native numeric suffix-literal expression parity
  - Source requirement or finding IDs: Item 5.9 Level D failure on retained `0a6h`, `1010b`, and `17o` literals; Item 6 native/Rust divergence rule
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - `native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm`
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - Level B native request-shape/literal-cursor proof
    - exact Level D FS-UAE suffix-literal proof with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, and full Rust quality gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for numeric suffix-literal parity only
  - Commit outcome:
    - native ExprVM consumes `h`, `b`, `o`/`q`, and `d` suffix literals without consuming adjacent expression text
  - Definition of done:
    - suffix literal values and surrounding additive cursor semantics match Rust for the supported scalar bridge grammar
    - remaining Item 6 operator-precedence gaps remain separate remediation items

- [x] Item 5.9.2: restore native multiplicative expression parity
  - Source requirement or finding IDs: Item 6 native/Rust divergence rule; Item 6 `expr_syntax.asm` multiplication, division, and modulo expressions
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - native ExprVM parser/evaluator files
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - Level A Rust operator oracle
    - exact Level D native multiplicative FS-UAE proof with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, and full Rust quality gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for multiplicative expression parity only
  - Commit outcome:
    - native scalar expressions preserve Rust multiplication, division, and modulo precedence beneath unary operators and above addition/subtraction
  - Definition of done:
    - `*`, `/`, and `%` evaluate with Rust-compatible values and token boundaries
    - later shift, comparison, bitwise, logical, and ternary gaps remain separate remediation items

- [x] Item 5.9.3: restore native shift expression parity
  - Source requirement or finding IDs: Item 6 native/Rust divergence rule; `runtime_expr_parser.rs::parse_shift`; `opforge-core/src/expr.rs::apply_binary`
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - native ExprVM parser/evaluator files
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - Level A Rust shift oracle
    - exact Level D native shift FS-UAE proof with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, and full Rust quality gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for shift expression parity only
  - Commit outcome:
    - native scalar expressions preserve Rust left/right shift precedence, logical right shift, and 32-bit shift-count masking
  - Definition of done:
    - `<<` and `>>` evaluate with Rust-compatible values and token boundaries above addition/subtraction and beneath comparisons
    - comparison, bitwise, logical, and ternary gaps remain separate remediation items

- [x] Item 5.9.4: restore native complete-source statement capacity
  - Source requirement or finding IDs: Item 5.9 Level D failure on complete additive `expr_syntax.asm`; valid guest artifacts report `OPC-NCLI013`/`OPC-NCLI010` after the native statement table reaches its fixed 160-record limit
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - native CLI and opasm capacity declarations
    - focused tests in `crates/opforge-asm/src/tests.rs`
    - the audited native-runtime boundary inventory snapshot
  - Full quality gates:
    - Level B capacity-boundary/source-lock proof
    - exact Level D `native_statement_capacity_over_160_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for statement-record capacity only
  - Commit outcome:
    - native statement storage accepts the complete bounded source-record domain instead of failing at the former 160-statement transitional limit
  - Definition of done:
    - a real native source containing more than 160 ordinary statements assembles byte-for-byte with Rust
    - statement storage remains bounded by the existing 512-record source/session limit and rejects overflow
    - label, expression, image, and source-line capacities remain separate invariants

- [x] Item 5.9.5: restore native digit-separator expression parity
  - Source requirement or finding IDs: Item 5.9 valid Level D failure in complete additive `expr_syntax.asm` at the first retained underscore-separated literal, `1_000_000`
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - native ExprVM parser/compiler files
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - Level A Rust digit-separator oracle
    - exact Level D `native_expression_digit_separators_fs_uae` with `--nocapture --test-threads=1`
    - exact negative Level D `native_expression_digit_separator_invalid_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for digit separators only
  - Commit outcome:
    - native scalar literal scanning accepts Rust-compatible underscores without changing the literal value or consuming adjacent expression text
  - Definition of done:
    - decimal, prefixed hexadecimal/binary, and suffix-form binary separators used by `expr_syntax.asm` match Rust
    - tokenizer-invalid underscore-only literals still fail deterministically; placements accepted by the Rust tokenizer remain accepted
    - later expression and pass-layout failures remain separate remediation items

- [x] Item 5.9.6: restore native direct forward-reference sizing stability
  - Source requirement or finding IDs: user-reported actual-Amiga divergence in canonical `examples/mos6502/mos_forward_ref_stability.asm`; standalone Level D emitted `AD 00 01` and `9C 00 02` where source comments and Rust authority require `AD 01 01` and `9C 01 02`
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - narrowly required native opcore unresolved-symbol, opasm shape-inference, and tkpkg selector-stability files
    - the standalone canonical-source proof in `crates/opforge-asm/src/tests/native_mos_forward_ref_stability.rs`
  - Full quality gates:
    - focused Level B pass-one unresolved-symbol and unstable-widen contract
    - exact Level D `native_mos_forward_ref_stability_fs_uae` with `--nocapture --test-threads=1`
    - exact Level D `native_mos_unstable_widen_no_wider_fallback_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for direct forward-reference pass-one sizing only
  - Commit outcome:
    - pass one marks an absent forward symbol unstable and defers a narrowing selector when package metadata requires widening, matching the emitted pass-two byte count
  - Definition of done:
    - the unchanged mixed-CPU canonical source emits `AD 01 01 EA 60` and `9C 01 02 EA 60` on real AmigaOS
    - its live Rust HEX remains equal to the checked-in reference and source comments
    - BIN gap-layout behavior and other sizing forms remain separate invariants

- [x] Item 5.9.7: remove the native 16-label transitional limit
  - Source requirement or finding IDs: Item 5.9 valid Level D failure in complete additive `expr_syntax.asm` when defining `num_sep_suf`, the seventeenth retained label
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - native opasm label storage and dependent read-only symbol snapshot files
    - focused tests in `crates/opforge-asm/src/tests/native_label_capacity.rs`
  - Full quality gates:
    - focused Level B label/snapshot capacity contract
    - exact Level D `native_label_capacity_over_16_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for label and symbol-snapshot capacity only
  - Commit outcome:
    - native label storage and dependent read-only snapshots cover the complete existing 512-record source/session domain instead of stopping at sixteen labels
  - Definition of done:
    - a real native source defining more than sixteen labels resolves its final label byte-for-byte with Rust
    - scoped alias storage remains bounded for the corresponding source-label domain
    - behavior beyond the existing 512-record source/session bound remains a separate invariant

- [x] Item 5.9.8: restore native exponentiation expression parity
  - Source requirement or finding IDs: Item 5.9 valid Level D failure in complete additive `expr_syntax.asm` at the first retained exponentiation expression, `pow1 .const 2 ** 3`
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-expression-power.toml`
    - native opcore expression compiler and ExprVM runtime files
    - focused tests in `crates/opforge-asm/src/tests/native_expression_power.rs`
    - refreshed complete native runtime boundary inventory hash without expanding the certified opcore routine surface
  - Full quality gates:
    - focused Level A Rust value/precedence/right-associativity oracle
    - focused Level B parser/runtime source contract
    - exact Level D `native_expression_power_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for exponentiation parsing/evaluation only
  - Commit outcome:
    - native scalar expressions parse `**` above multiplication, associate it to the right, and evaluate nonnegative exponents with wrapping 32-bit multiplication
  - Definition of done:
    - `2 ** 3`, `3 ** 2 ** 2`, multiplication/power precedence on both sides, and `0 ** 0` emit the same bytes as Rust under real AmigaOS execution
    - negative exponents fail through the existing checked evaluator path
    - every later failure in the complete additive fixture remains a separate remediation item rather than being excluded

- [x] Item 5.9.9: restore native comparison expression parity
  - Source requirement or finding IDs: Item 5.9 valid Level D failure in complete additive `expr_syntax.asm` at the first retained comparison, `cmp_eq .const (3 == 3)`
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-expression-comparison.toml`
    - native opcore expression compiler and ExprVM runtime files
    - focused tests in `crates/opforge-asm/src/tests/native_expression_comparison.rs`
    - refreshed complete native runtime boundary inventory hash without expanding the certified opcore routine surface
  - Full quality gates:
    - focused Level A Rust comparison value/precedence oracle
    - focused Level B parser/runtime source contract
    - exact Level D `native_expression_comparison_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for comparison parsing/evaluation only
  - Commit outcome:
    - native scalar expressions parse all retained equality and ordering spellings above shifts and evaluate signed native-domain values to canonical zero/one results
  - Definition of done:
    - `==`, `!=`, `<>`, `<=`, `<`, `>=`, and `>` emit the same bytes as Rust under real AmigaOS execution
    - true and false results, signed ordering, and shift/comparison precedence are proven
    - later complete-fixture failures remain separate remediation items rather than being excluded

- [x] Item 5.9.10: restore native bitwise expression parity
  - Source requirement or finding IDs: Item 5.9 valid Level D failure in complete additive `expr_syntax.asm` at the first retained bitwise expression, `bit_and .const ($f0 & $0f)`
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-expression-bitwise.toml`
    - native opcore expression compiler and ExprVM runtime files
    - focused tests in `crates/opforge-asm/src/tests/native_expression_bitwise.rs`
    - refreshed complete native runtime boundary inventory hash without expanding the certified opcore routine surface
  - Full quality gates:
    - focused Level A Rust bitwise value/precedence oracle
    - focused Level B parser/runtime source contract
    - exact Level D `native_expression_bitwise_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for bitwise parsing/evaluation only
  - Commit outcome:
    - native scalar expressions parse AND, XOR, and OR in the Rust precedence order and yield doubled tokens to the logical tier
  - Definition of done:
    - `&`, `^`, and `|` values and mixed precedence emit the same bytes as Rust under real AmigaOS execution
    - comparison-before-bitwise precedence and logical-token yielding are proven
    - later complete-fixture failures remain separate remediation items rather than being excluded

- [x] Item 5.9.11: restore native logical expression parity
  - Source requirement or finding IDs: Item 5.9 valid Level D failure in complete additive `expr_syntax.asm` at the first retained logical expression, `log_and .const (2 && 3)`
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-expression-logical.toml`
    - native opcore expression compiler and ExprVM runtime files
    - focused tests in `crates/opforge-asm/src/tests/native_expression_logical.rs`
    - refreshed complete native runtime boundary inventory hash without expanding the certified opcore routine surface
  - Full quality gates:
    - focused Level A Rust logical value/precedence oracle
    - focused Level B parser/runtime source contract
    - exact Level D `native_expression_logical_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for logical parsing/evaluation only
  - Commit outcome:
    - native scalar expressions parse logical AND, OR, and XOR with Rust truthiness, precedence, and associativity
  - Definition of done:
    - all three logical operators emit canonical zero/one bytes matching Rust under real AmigaOS execution
    - logical-AND precedence, shared left-associative OR/XOR precedence, and bitwise-before-logical precedence are proven with discriminating cases
    - all nine focused cases execute; later complete-fixture failures remain separate remediation items rather than being excluded

- [x] Item 5.9.13: restore native ternary arm selection parity
  - Source requirement or finding IDs: Item 5.9.12 expanded Level D proof showed `0 ? >$1234 : <$1234` selecting the true arm; the earlier complete `expr_syntax.asm` trace likewise recorded `ternary1 .const 0 ? 1 : 2` as `1`
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-expression-ternary.toml`
    - native ExprVM runtime
    - focused tests in `crates/opforge-asm/src/tests/native_expression_ternary.rs`
  - Full quality gates:
    - focused Level A Rust true/false/nested ternary oracle
    - focused Level B runtime register/condition-code preservation contract
    - exact Level D `native_expression_ternary_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for ternary selection only
  - Commit outcome:
    - native ExprVM preserves the false arm across condition pop and branches before arm restoration changes condition codes
  - Definition of done:
    - zero and nonzero conditions, nested right-associative ternaries, and logical-condition precedence match all five Rust bytes under real AmigaOS execution
    - the focused test executes every case; unary-in-arm coverage remains in Item 5.9.12 rather than being excluded

- [x] Item 5.9.12: restore native unary expression entry parity
  - Source requirement or finding IDs: Item 5.9 valid Level D failure in complete additive `expr_syntax.asm` after ternary evaluation, where `lda #>PAGE` is rejected as `OTR901: selected operand unexpected text`
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-expression-unary.toml`
    - native selected-operand runtime and opcore expression compiler files
    - focused tests in `crates/opforge-asm/src/tests/native_expression_unary.rs`
    - refreshed complete native runtime boundary inventory hashes without expanding either certified routine surface
  - Full quality gates:
    - focused Level A Rust unary/grouping value and precedence oracle
    - focused Level B selected-operand/compiler source contract
    - exact Level D `native_expression_unary_fs_uae` with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for unary entry/parsing only
  - Commit outcome:
    - native selected operands admit valid unary/grouping starts and compile recursive unary and outer low/high semantics into the existing ExprVM unary operations
  - Definition of done:
    - all fifteen focused instruction cases match Rust bytes under real AmigaOS execution
    - recursive unary including plus, outer and parenthesized high/low, unary-before-power, parenthesized immediate behavior, and high/low in both ternary arms are proven with discriminating cases
    - later complete-fixture failures, including string literals, remain separate remediation items rather than being excluded

- [x] Item 5.9.14: restore native scalar string-literal expression parity
  - Source requirement or finding IDs: Item 5.9 complete `expr_syntax.asm` Level D artifact records `char_a` and `char_ab` as zero although Rust and the retained source require `$41` and `$4142`
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-expression-string-literal.toml`
    - native selected-operand runtime and opcore expression compiler files
    - focused tests in `crates/opforge-asm/src/tests/native_expression_string_literal.rs`
    - refreshed complete native runtime boundary inventory hashes without expanding either certified routine surface
  - Full quality gates:
    - focused Level A Rust scalar string-literal oracle
    - focused Level B selected-operand/compiler source contract
    - exact positive and negative Level D `native_expression_string_literal_` tests with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for scalar string-literal expression parity only
  - Commit outcome:
    - native scalar expressions decode one/two-byte quoted literals and pack them with Rust-compatible values
  - Definition of done:
    - single and double quotes, simple and hexadecimal escapes, direct selected operands, low/high extraction, and packed word emission match Rust under real AmigaOS execution
    - empty, unterminated, and wider literals fail through the checked compiler path
    - later complete-fixture failures remain separate remediation items rather than being excluded

- [x] Item 5.9.15: restore native zero-valued data-expression parity
  - Source requirement or finding IDs: Item 5.9 valid Level D failure in complete additive `expr_syntax.asm` at `.byte (TABLE & $ff)`, where the expression service succeeds with zero but the native comma-list path reclassifies that value as unresolved
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-data-expression-zero.toml`
    - native opasm comma-operand evaluation path
    - focused tests in `crates/opforge-asm/src/tests/native_data_expression_zero.rs`
    - refreshed complete native runtime boundary inventory hash without expanding the certified routine surface
  - Full quality gates:
    - focused Level A Rust zero-valued data-expression oracle
    - focused Level B expression-service status/value contract
    - exact positive and unresolved-negative Level D `native_data_expression_` tests with `--nocapture --test-threads=1`
    - complete unrestricted Level D `native_opcore_adapted_expr_syntax_fs_uae` rerun
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for successful zero-valued comma-list expressions only
  - Commit outcome:
    - native numeric data-list evaluation uses expression-service status, rather than result truthiness, to distinguish success from failure
  - Definition of done:
    - bitwise and logical expressions resolving to zero in first, middle, and final list positions match Rust under real AmigaOS execution
    - an unresolved expression still produces a completed nonzero guest result with diagnostics
    - complete `expr_syntax.asm` matches Rust without a fixture-count or prefix limit
    - symbolic subtraction-to-zero and long string data remain separate remediation items rather than being bundled or excluded

- [x] Item 5.9.16: restore native string-data directive parity
  - Source requirement or finding IDs: Item 5.9 valid Level D failures in complete additive `testexpr.asm` at `.byte 'abcd'` and `syntax.asm` at an exactly-64-byte quoted operand whose closing delimiter was truncated from the native statement snapshot
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-data-string.toml`
    - native opasm statement-text and string-data parsing paths
    - focused tests in `crates/opforge-asm/src/tests/native_data_string.rs`
    - refreshed complete native runtime boundary inventory hashes without expanding either certified routine surface
  - Full quality gates:
    - focused Level A Rust string-data byte oracle
    - focused Level B source-span and quote/escape parser contract
    - exact positive and malformed-negative Level D `native_data_string_` tests with `--nocapture --test-threads=1`
    - independent complete-source Level D reruns for `syntax.asm` and `testexpr.asm` to classify the next boundaries
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for string-data operands only
  - Commit outcome:
    - native `.byte`/`.db` parsing retains full source-backed operands when the compact snapshot fills, accepts both quote styles, and decodes hexadecimal escapes exactly like Rust
  - Definition of done:
    - single/double-quoted long strings, an exactly-64-byte operand, simple and hexadecimal escapes, and mixed numeric/string lists match Rust under real AmigaOS execution
    - malformed, bad-hex, and short-hex strings each produce a completed nonzero guest result with diagnostics
    - `testexpr.asm` emits its final `abcd` bytes before exposing only the separate default-origin mismatch
    - `syntax.asm` advances beyond all string-data sections before exposing a later expression boundary
    - default-origin and later expression failures remain separate remediation items rather than being bundled or excluded

- [x] Item 5.9.17: make Level D FS-UAE parity proof fail closed and ephemeral
  - Source requirement or finding IDs: user requirement that crashes, timeouts, stale outputs, mismatched case evidence, and evidence-name oracle aliases can never pass as native parity
  - Expected files:
    - `crates/opforge-asm/src/fs_uae_smoke.rs` and all native parity callers
    - the canonical native parity and FS-UAE rule packs plus active `AGENTS.md`
    - `scripts/workflow/check_native_fs_uae_proof_contract.py` and focused validator tests
  - Full quality gates:
    - adversarial host tests for absent/wrong/stale markers, absent/nonzero exit, missing output, byte mismatch, case-identity mismatch, and cleanup-on-drop
    - exact real Level D `native_mos_forward_ref_stability_fs_uae` under the fortified protocol
    - deterministic FS-UAE proof-contract validator, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` only when the final staged state has no persistent last-green evidence and no MOS parity case can omit its proof mode
  - Commit outcome:
    - one focused workflow/harness commit makes the proof contract intrinsic to native parity execution rather than a caller-side optional assertion
  - Definition of done:
    - the actual CPU/source/command/package case carries its Rust oracle directly in memory; no evidence filename or case-name alias resolves the oracle
    - exact fresh start/done challenge responses, explicit guest exit, and byte-for-byte Rust equivalence are mandatory for positive parity
    - all on-disk case evidence is cleared before launch and removed before the runner returns on success, failure, timeout, crash, or unwind
    - non-parity FS-UAE smoke and diagnostic runs also require guest completion plus explicit exit status and remove their artifact trees; launcher success alone never passes
    - one failed case or subcase cannot poison the serial coordinator or prevent every later discovered case from being attempted; failures are aggregated only after the supplied case set runs
    - legacy persistent FS-UAE artifacts and last-green records are removed, and the workflow gate rejects their reintroduction

- [x] Item 5.9.18: restore native implicit-origin parity
  - Source requirement or finding IDs: Item 5.9.16 complete `testexpr.asm` Level D artifact has exactly two differing bytes, offsets 1 and 5, where the native hard-coded `$0800` implicit origin produces `$08` and Rust's zero implicit origin produces `$00`
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-default-origin.toml`
    - native opasm session initialization
    - focused tests in `crates/opforge-asm/src/tests/native_default_origin.rs`
    - refreshed complete native runtime boundary inventory hash without expanding the certified routine surface
  - Full quality gates:
    - focused Level A live Rust implicit-origin and explicit-`.org` byte oracle
    - focused Level B native pass-one/pass-two origin initialization contract
    - exact Level D `native_default_origin_` tests with `--nocapture --test-threads=1`
    - independent complete-source Level D `native_opcore_adapted_testexpr_fs_uae`
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for implicit session origin only
  - Commit outcome:
    - native sessions without `.org` begin both passes at zero like Rust, while explicit `.org` remains authoritative
  - Definition of done:
    - label arithmetic and absolute instruction operands before any `.org` match live Rust bytes under real AmigaOS execution
    - explicit nonzero `.org` behavior remains byte-for-byte unchanged
    - complete additive `testexpr.asm` matches Rust with no mismatch
    - the later `$-label` expression failure in complete `syntax.asm` remains a separate remediation item rather than being bundled or excluded

- [x] Item 5.9.19: preserve parenthesized label-term boundaries in native expressions
  - Source requirement or finding IDs: Item 5.9.16 complete `syntax.asm` Level D failure at `entries .const ($-jump_tab) / 3`; native `termLength` consumes the closing parenthesis as part of `jump_tab)` and the expression bridge reports `OTR922`
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-expression-label-boundary.toml`
    - native opcore expression compiler label-term scanner
    - focused tests in `crates/opforge-asm/src/tests/native_expression_label_boundary.rs`
    - refreshed complete native runtime boundary inventory hash without expanding the certified routine surface
  - Full quality gates:
    - focused Level A live Rust current-address/label arithmetic oracle
    - focused Level B native label-token boundary contract
    - exact positive and unresolved-negative Level D `native_expression_label_boundary_` tests with `--nocapture --test-threads=1`
    - independent complete-source Level D `native_opcore_adapted_syntax_fs_uae` rerun
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for label-term delimiter ownership only
  - Commit outcome:
    - the native expression compiler stops a label term before `)` so the enclosing parser can close the group and continue with following operators
  - Definition of done:
    - `($-label) / 3` and a label followed by `)` evaluate with Rust-compatible values under real AmigaOS execution
    - an actually unresolved parenthesized label still produces a completed nonzero guest result with diagnostics
    - complete additive `syntax.asm` advances beyond `entries .const ($-jump_tab) / 3`; every later failure remains a separate remediation item rather than being bundled or excluded

- [x] Item 5.9.20: match Rust quoted `.word` byte emission
  - Source requirement or finding IDs: Item 5.9.19 complete `syntax.asm` Level D rerun; the first byte mismatch is at `.word 'AB'`, where Rust emits decoded source bytes `41 42` while native incorrectly numerically packs `$4142` as `42 41`
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-word-string-data.toml`
    - native opasm assembly-driver quoted-data sizing and emission path
    - focused tests in `crates/opforge-asm/src/tests/native_word_string_data.rs`
    - refreshed complete native runtime boundary inventory hash without expanding the certified routine surface
  - Full quality gates:
    - focused Level A live Rust quoted-word and numeric-expression oracle
    - focused Level B native branch-order contract
    - exact positive and malformed-negative Level D `native_word_string_data_` tests with `--nocapture --test-threads=1`
    - independent complete-source Level D `native_opcore_adapted_syntax_fs_uae` rerun
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for quoted `.word` ownership only
  - Commit outcome:
    - quoted-only `.word` operands use decoded source-byte emission while numeric word expressions retain MOS little-endian packing
  - Definition of done:
    - the four complete `.word` controls from `syntax.asm` emit `42 41 41 42 41 42 42 41` under Rust and real AmigaOS execution
    - malformed quoted `.word` input remains a completed diagnostic failure
    - complete additive `syntax.asm` advances beyond byte offset 106; every later failure remains a separate remediation item rather than being bundled or excluded

- [x] Item 5.9.21: keep native `.ds` reservations out of emitted artifacts
  - Source requirement or finding IDs: exhaustive all-19 precedence-expression FS-UAE matrix after Item 5.9.20; every completed case has a common 128-byte native surplus from `buffer .ds StrSize * 4`, while the live Rust entry oracle advances the PC without emitting reservation bytes
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-ds-reservation.toml`
    - native opasm pass-two `.ds` emission branch
    - focused tests in `crates/opforge-asm/src/tests/native_ds_reservation.rs`
    - refreshed complete native runtime boundary inventory hash without expanding the certified routine surface
  - Full quality gates:
    - focused Level A live Rust sparse-entry/label-address oracle
    - focused Level B native advance-versus-emit branch contract
    - exact positive and unresolved-negative Level D `native_ds_reservation_` tests with `--nocapture --test-threads=1`
    - independent complete-source Level D `native_opcore_adapted_syntax_fs_uae` rerun
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for `.ds` artifact ownership only
  - Commit outcome:
    - `.ds` retains its PC reservation in the sizing path and emits no bytes in the artifact path
  - Definition of done:
    - a reservation changes a following label address but does not add bytes to the exact Rust/native artifact
    - an unresolved reservation remains a completed diagnostic failure
    - complete additive `syntax.asm` no longer has the 128-byte reservation surplus; every later failure remains separate rather than being bundled or excluded

- [x] Item 5.9.22: preserve suffixed-number boundaries before bitwise operators
  - Source requirement or finding IDs: exhaustive all-19 precedence-expression FS-UAE matrix; only `23H & 0FH` and `23H | 0FH ^ 0FFH` fail compilation after earlier byte-parity corrections because `parseSuffixedNumber` neither delimits `&`/`^` nor restores remaining length when backing up over any delimiter
  - Expected files:
    - `documentation/plans/slices/native-porting-slice-suffixed-bitwise-boundary.toml`
    - native opcore suffixed-number scanner
    - expanded focused tests in `crates/opforge-asm/src/tests/native_expression_bitwise.rs`
    - refreshed complete native runtime boundary inventory hash without expanding the certified routine surface
  - Full quality gates:
    - expanded Level A live Rust bitwise oracle with `H`-suffix operands
    - expanded Level B native delimiter/tier contract
    - exact Level D `native_expression_bitwise_` tests with `--nocapture --test-threads=1`
    - independent complete-source Level D `native_opcore_adapted_syntax_fs_uae` rerun
    - native formatter, staged native porting gate, full Rust quality gate, and workflow gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for suffixed-number delimiter ownership only
  - Commit outcome:
    - the suffixed-number scanner backs up its pointer and remaining length together, then yields `&`, `|`, and `^` to the established bitwise precedence tiers
  - Definition of done:
    - both suffixed-hex bitwise expressions match live Rust under real AmigaOS execution
    - the existing `$`-prefixed bitwise and logical-yield controls remain exact
    - complete additive `syntax.asm` advances beyond both bitwise expressions; every later failure remains separate rather than being bundled or excluded

- [x] Item 6: add the CPU-neutral syntax and expression opcore parity shard
  - Source requirement or finding IDs: Item 5 assignments for parsing, expression, conditional, range/list, grouping, scope, and text-encoding examples that can run directly or through additive `6502`/`65c02` fixtures
  - Expected files:
    - additive opcore MOS-backed fixtures and matching references assigned by Item 4
    - `crates/opforge-asm/src/native_reference_parity.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
  - Full quality gates:
    - focused Level A/B tests named by the Item 5 shard
    - `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - exact named Level D FS-UAE shard test with `--nocapture --test-threads=1`
    - `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for one syntax/expression coverage shard, classified proof levels, complete fixture semantics, and no bundled native parity fix
  - Commit outcome:
    - the assigned CPU-neutral syntax/expression cases compare exact native CLI artifacts with the Rust authority
  - Definition of done:
    - every assigned case is green or the item pauses for a separately inserted one-invariant remediation item
    - additive fixtures preserve the complete semantics of their canonical source and document why adaptation is necessary
    - tests state what they prove and do not prove

- [x] Item 6.1: restore native module-local symbol scoping parity
  - Source requirement or finding IDs: Item 7 Level D `module_basics.asm` failure: native pass one rejects the second module-local `VALUE` as a duplicate label
  - Expected files:
    - one `documentation/plans/slices/*.toml` metadata record
    - native module/symbol storage and lookup files
    - focused tests in `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - Level A full Rust CLI/module-preprocessor oracle for `module_basics.asm`
    - exact Level D FS-UAE module-local symbol proof with `--nocapture --test-threads=1`
    - native formatter, staged native porting gate, and full Rust quality gate
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for module-local symbol scoping only
  - Commit outcome:
    - identical local labels in distinct modules do not collide in native CLI passes and match Rust output
  - Definition of done:
    - `module_basics.asm` matches Rust through the real native CLI
    - macro tokenizer and statement-parser divergences remain separate remediation items

- [ ] Item 7: add the module, macro, and statement opcore parity shard
  - Source requirement or finding IDs: Item 5 assignments for modules, imports, visibility, macros, statement definitions/expansion, and multi-file roots
  - Expected files:
    - additive opcore MOS-backed fixtures and matching references assigned by Item 4
    - `crates/opforge-asm/src/native_reference_parity.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
  - Full quality gates:
    - focused Level A/B tests named by the Item 5 shard
    - `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - exact named Level D FS-UAE shard test with `--nocapture --test-threads=1`
    - `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for one module/macro/statement coverage shard, including complete staged file trees and no bundled native parity fix
  - Discovery evidence and pause condition:
    - the uncapped `native_reference_opcore_module_macro_statement_fs_uae` discovery run on 2026-08-09 attempted all 17 assigned roots, produced 7 passes and 10 fresh completed-guest failures, and classified those failures as `I7-D-01` through `I7-D-04`
    - Item 7 remains paused until Items 7.8–7.11 are each committed and green; Item 8 must not begin while Item 7 is paused
    - after Items 7.8–7.11, `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' RUST_TEST_THREADS=1 cargo test -p asm native_reference_opcore_module_macro_statement_fs_uae -- --nocapture --test-threads=1` must attempt and pass all 17 assigned roots before this parent can close
  - Commit outcome:
    - assigned single- and multi-file module, macro, and statement cases compare only CLI-written native artifacts with Rust
  - Definition of done:
    - staged module roots and auxiliary files match real CLI usage
    - every assigned case is green or pauses for a separate remediation item
    - no harness-injected shortcut substitutes for the CLI path

### Native preprocessor subsystem boundary

The native preprocessor owns source-structure directives only: `.macro`,
`.segment`, `.statement`, their matching end directives, argument substitution,
and expansion back into the existing source-line → tokenizer → PRVM → session
path. PRVM and opasm remain owners of ordinary statement parsing, expression
evaluation, selection, encoding, scopes, and output. Native limits (fixed
tables, bounded text, recursion depth, and AmigaOS calling conventions) must
produce deterministic diagnostics rather than silently truncate. The active
`AGENTS.md` remains binding during every item below.

- [x] Item 7.1: establish bounded native preprocessor storage and source re-entry
  - Source requirement or finding IDs: Item 7 macro and statement Level D failures; Rust `MacroProcessor::expand_lines` ownership boundary.
  - Expected files: `native/motorola68000/amigaos/opforge-cli/{state,session_init,line_processor,preprocessor}.asm`; one slice record; focused host tests.
  - Full quality gates: focused Level B/C storage and re-entry contract tests; native formatter; staged native-porting gate; full Rust quality gate.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for bounded state/reset/re-entry only.
  - Commit outcome: definition and expansion buffers reset per CLI run; an expanded ordinary line re-enters the existing pipeline without harness injection.
  - Definition of done: no macro or statement semantics are implemented yet; capacity/depth failure is deterministic and no source line is silently dropped.

- [x] Item 7.2: consume and store native macro definitions
  - Source requirement or finding IDs: `macro_syntax.asm` tokenizer failure at macro-only `@` body text.
  - Expected files: native preprocessor/state/source routing; one slice record; focused macro-definition tests.
  - Full quality gates: Level A Rust macro-definition oracle; Level B/C native definition-boundary tests; native formatter; staged native-porting gate; full Rust quality gate.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for `.macro/.endmacro` definition consumption only.
  - Commit outcome: validated macro definition bodies never reach tokenizer or PRVM before invocation.
  - Definition of done: names, parameter declarations, body order, comments, mismatch/unterminated errors, and capacity bounds are retained; invocation remains a later item.

- [x] Item 7.3: expand macro invocations with Rust-compatible substitution and scope wrapping
  - Source requirement or finding IDs: `macro_syntax.asm` COPY/PAIR/TEXT/LOCAL forms; Rust `parse_macro_invocation`, `build_macro_args`, and `substitute_line`.
  - Expected files: a new macro-only MOS fixture and reference artifact that contains COPY/PAIR/TEXT/LOCAL but no `.segment`; native preprocessor/state/line routing; focused macro artifact and contract tests; one slice record.
  - Execution phases (each phase is one focused commit; do not begin the next phase until the current phase has its focused proof and plan-compliance review):
    - Phase 7.3a: add bounded invocation-frame state and reset semantics
      - Rust reference: `MacroInvocation`, `MacroArgs`, and `MacroProcessor::expand_lines` invocation-frame lifetime.
      - Native boundary: `opforge.cli.state` and `opforgeNativeCliResetPreprocessorV1`.
      - Implement fixed-capacity selected-definition, body cursor, positional argument, full-list, and label buffers. Size the reset range exactly, initialize sentinel fields, and reject an active/nested frame rather than overwriting it.
      - Focused proof: Level B state-layout/reset contract, including byte-count coverage and a no-silent-truncation capacity assertion.
      - Commit outcome: every invocation attempt has one resettable, bounded frame; no macro lookup or expansion is added yet.
    - Phase 7.3b: parse macro invocations and bind arguments
      - Rust reference: `parse_macro_invocation`, `parse_macro_args`, `parse_macro_params`, and `build_macro_args`.
      - Native boundary: a preprocessor-only invocation parser called before source recording/tokenization.
      - Implement label-attached and indented `.NAME` forms, case-insensitive lookup of captured definitions, parenthesized and comma-leading argument lists, parameter defaults, named parameter slots, `.1` through `.9` positional slots, and a canonical full-list buffer. Preserve nested expression/quoted comma handling or fail deterministically before expansion; never let a recognized macro invocation fall through to PRVM.
      - Focused proof: Level C host substitution-model cases for COPY, PAIR default binding, empty/extra/malformed arguments, and recognized-versus-ordinary directive routing.
      - Commit outcome: a complete validated invocation frame is available to the expansion step, without emitting source lines.
    - Phase 7.3c: substitute one captured macro body line into a bounded expansion buffer
      - Rust reference: `substitute_line` in `macro_processor_args_subst.rs`.
      - Native boundary: preprocessor-owned byte scanner and `NativeCliPreprocessExpansionLine`.
      - Implement `.name`, `.{name}`, `.1` through `.9`, `@1` through `@9`, and `.@` replacement with Rust-compatible case-insensitive name lookup. Preserve non-matching text byte-for-byte; reject unterminated braced names, unknown required bindings, and output overflow deterministically.
      - Focused proof: Level C table-driven substitution model covering named, braced, positional, at-positional, full-list, defaults, quoted commas, and capacity failure.
      - Commit outcome: every stored body line can become one bounded ordinary source line, but is not yet routed through the frontend.
    - Phase 7.3d: expand body lines through the ordinary native CLI path and wrap labeled calls
      - Rust reference: `format_macro_block_start`, macro-body expansion, and nested `expand_lines` behavior.
      - Native boundary: `opforgeNativeCliProcessExpandedLineV1` only; do not add macro semantics to PRVM, opasm, or the generic CLI parser.
      - Emit `.block`/`.endblock` around macro expansion, attaching a caller label to `.block` exactly as Rust does. Route each substituted body line through the existing line processor, restore the caller line after each route, and enforce the declared recursion limit with a deterministic failure status.
      - Focused proof: Level B/C re-entry and label-scope contract for LOCAL; recursion/second-frame failure must prove the caller frame and session state remain intact.
      - Commit outcome: a recognized macro call is consumed and its ordinary expanded lines enter the existing source → tokenizer → PRVM → session path.
    - Phase 7.3e: add and prove the macro-only MOS fixture
      - Fixture: `examples/opcore/macro_invocation_native.asm`, containing COPY, PAIR, TEXT, and LOCAL only; `.segment` remains excluded for Item 7.4.
      - References: matching `examples/reference/opcore/macro_invocation_native.{hex,lst}` generated solely from the live Rust CLI/reference workflow after a baseline reference-gate failure identifies the missing fixture files.
      - Tests: Level A live Rust artifact oracle, exact named Level D FS-UAE test using the macro-only fixture and CLI-written native artifact comparison, with `--nocapture --test-threads=1`.
      - Commit outcome: the isolated fixture produces byte-for-byte native CLI output matching Rust; this proof does not claim canonical `macro_syntax.asm` or segment support.
    - Phase 7.3f: run closure gates and commit only the completed invocation slice
      - Required gates: focused Level A/B/C tests; `cargo test -p asm examples_match_reference_outputs -- --nocapture`; exact Item 7.3 Level D FS-UAE test; `scripts/workflow/run_native_68000_format_gate.sh`; `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`; `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`.
      - Review evidence: `plan-compliance-reviewer` returns `PASS` for lookup, defaults, positional/named/full-list substitution, label wrapping, recursion failure, macro-only fixture scope, and no segment/statement/module-export semantics.
      - Commit outcome: one focused Item 7.3 commit, excluding the canonical segment fixture, statement work, module/import export work, unrelated reference rewrites, and any harness-injected shortcut.
  - Full quality gates: Level A live Rust artifact oracle for the macro-only fixture; Level C substitution model; exact Level D FS-UAE proof for the macro-only fixture; native formatter; staged native-porting gate; full Rust quality gate.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for invocation, defaults, positional/named/full-list substitution, and `.block` wrapping only.
  - Commit outcome: macro expansions re-enter ordinary native CLI processing and the macro-only fixture matches Rust.
  - Definition of done: `.name`, `.1`…`.9`, `@1`…`@9`, `.@`, label-attached calls, default values, recursion bounds, and deterministic errors match the declared Rust subset; canonical `macro_syntax.asm` remains reserved for Item 7.4.

- [x] Item 7.3g: remediate native CLI error-output routing before new preprocessor semantics
  - Source requirement or finding IDs: stabilization-plan Item 7 ordering; deterministic CLI failure parity assigned by this plan.
  - Invariant: native failure text, exit status, and stdout/stderr routing match the declared live Rust CLI and checked-in `.err` authority; successful output routing remains unchanged.
  - Expected files: focused native DOS/error-sink adapter and migrated error call sites; Level A/B/D tests; one slice record.
  - Activation dependency: the archived native preprocessor stabilization plan is the required baseline; record the first divergent diagnostic boundary before changing behavior.
  - Full quality gates: focused Level A Rust diagnostic oracle; Level B routing contract; exact Level D native CLI failure proof; native formatter; staged native-porting gate; full Rust quality gate.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for error-output routing and exit-status parity only.
  - Commit outcome: one independently revertible CLI diagnostic-routing remediation commit.
  - Definition of done: deterministic failure output reaches the Rust-authoritative sink with matching normalized text and exit status; no segment, statement, export, linker, output-format, CPU, or selector semantics are added.

- [x] Item 7.4: add native segment definition and expansion semantics
  - Source requirement or finding IDs: `macro_syntax.asm` INLINE `.segment` form; Rust segment branch in `MacroProcessor::expand_lines`.
  - Activation dependency: the native preprocessor stabilization plan is archived and Item 7.3g CLI error-output remediation is complete and green.
  - Expected files: native preprocessor and tests; one slice record.
  - Full quality gates: Level A live Rust `macro_syntax.asm` artifact oracle; focused Level C source model; exact Level D `macro_syntax.asm` FS-UAE proof; native formatter; staged native-porting gate; full Rust quality gate.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for segment expansion only.
  - Commit outcome: `.segment/.endsegment` expands without macro scope wrapping and preserves label attachment semantics.
  - Definition of done: the canonical INLINE segment output matches Rust; no statement-definition behavior is added.

- [x] Item 7.5: consume and store native statement definitions
  - Source requirement or finding IDs: `statement_expansion.asm` parser failure at `.statement`.
  - Expected files: native preprocessor/state; statement-signature representation; focused tests; one slice record.
  - Full quality gates: Level A Rust statement-definition oracle; Level B/C signature-storage tests; native formatter; staged native-porting gate; full Rust quality gate.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for definition storage only.
  - Commit outcome: `.statement/.endstatement` definitions are consumed before PRVM and retain literal/boundary/capture signature data.
  - Definition of done: no invocation matching yet; malformed, nested, mismatched, unterminated, and capacity cases are deterministic.

- [x] Item 7.6: match and expand native statement invocations
  - Source requirement or finding IDs: canonical LOAD, bracketed lda, move, and addi statement forms; Rust `asm_expand_statement_invocation`.
  - Expected files: native preprocessor/statement matcher/routing; focused tests; one slice record.
  - Full quality gates: Level A live Rust statement artifact oracle; Level C signature/capture model; exact Level D statement FS-UAE proof; native formatter; staged native-porting gate; full Rust quality gate.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for longest-keyword selection, literal/boundary/capture matching, substitution, and re-entry only.
  - Commit outcome: matching statement invocations expand to ordinary native source statements with exact Rust bytes.
  - Definition of done: `statement_expansion.asm` matches Rust through the real native CLI; unsupported signatures fail deterministically without corrupting the session.

- [x] Item 7.7: integrate preprocessor exports with native module/import flow
  - Source requirement or finding IDs: Item 7 module/import/visibility and multi-file requirements; Rust `AsmMacroProcessor::{take_native_exports,inject_*}`.
  - Expected files: native preprocessor, module/use flow, source graph tests, slice record; deletion of `tkpkg_engine_context_adapter.asm` and its inventory/no-growth allowance once neutral context is supplied without a tkpkg-to-opasm import.
  - Full quality gates: Level A multi-file Rust oracle; Level C export/import model; exact Level D multi-file FS-UAE proof; native formatter; staged native-porting gate; full Rust quality gate.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for public/private export injection and aliases only.
  - Validation evidence: six independent fail-closed FS-UAE roots complete and match their exact live Rust oracle bytes; wildcard-requested private macro, segment, and statement names resolve to later local shadows; the separately requested `mos_forward_ref_stability.asm` FS-UAE proof, staged native-porting gate, and full Rust quality gate pass.
  - Commit outcome: module macro and statement exports are injected according to native `.use` selection and visibility rules.
  - Definition of done: Item 7’s declared multi-file roots run through real native CLI usage; no generic CLI path gains CPU-specific semantics.

- [x] Item 7.8: accept directive-first native macro and segment definitions
  - Source requirement or finding IDs: `I7-D-01`, parent Item 7 complete-corpus Level D failure for `macro_segment_syntax.asm`; the native preprocessor currently recognizes name-first definitions but lets `.macro FILL(value)` and `.segment INLINE(v)` reach tokenization. This item fully closes `I7-D-01`.
  - Invariant: directive-first and name-first definition headers produce the same bounded native definition record and consume the complete definition before tokenizer/PRVM routing.
  - Expected files: native preprocessor definition/header parsing only; focused Level A/C header model; exact `macro_segment_syntax.asm` Level D proof; one slice record; this plan’s checkbox/evidence only after closure.
  - Full quality gates: live Rust directive-first oracle; focused native header-routing and bound tests; `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' RUST_TEST_THREADS=1 cargo test -p asm native_macro_segment_directive_first_fs_uae -- --nocapture --test-threads=1`; native formatter; staged native-porting gate; full Rust quality gate; `make workflow-gate`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for directive-first definition recognition only, with no module visibility, autoload, conditional, linker, output, or CPU behavior.
  - Validation evidence: focused Level B/C header tests pass; the permanent 68020 harness rejects a wrong leading dot directive and proves exact dot/dollar definition capture, invocation, binding, and substitution with fresh guest completion and zero exit; all five exact authoritative FS-UAE cases complete and match their same-case live Rust CLI bytes; the native formatter, staged native-porting gate, full Rust quality gate, and `make workflow-gate` pass. The plan-compliance reviewer returned `PASS` for the exact staged Item 7.8 index on 2026-08-11.
  - Commit outcome: one independently revertible definition-header remediation commit.
  - Definition of done: `macro_segment_syntax.asm` completes through the real native CLI and matches the exact same-case live Rust bytes; malformed directive-first headers fail deterministically without falling through.

- [x] Item 7.9: consume ordinary native module visibility directives
  - Source requirement or finding IDs: `I7-D-02`, parent Item 7 complete-corpus Level D failures for `module_use.asm`, `module_use_include.asm`, `module_visibility.asm`, and `use_wildcard_import.asm`; native pass one currently reports `.pub/.priv` as unknown mnemonics outside preprocessor-export capture. This item fully closes `I7-D-02`.
  - Activation dependency: Item 7.8 is committed and green.
  - Invariant: `.pub` and `.priv` are consumed as module visibility state before ordinary statement dispatch, and visibility applies consistently to constants/labels and preprocessor exports without emitting bytes.
  - Expected files: native module/use visibility owner and the narrow opasm/source-routing seam; focused Level A/C visibility model; exact affected-root Level D proof; one slice record; this plan’s checkbox/evidence only after closure.
  - Full quality gates: live Rust public/private symbol oracle; focused native routing/state/reset tests including private non-leakage; `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' RUST_TEST_THREADS=1 cargo test -p asm native_module_visibility_roots_fs_uae -- --nocapture --test-threads=1`; native formatter; staged native-porting gate; full Rust quality gate; `make workflow-gate`.
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for ordinary module visibility directives only, with no autoload search, conditional, linker, output, or CPU behavior.
  - Validation evidence: both focused Level C source/behavior contracts pass;
    the four canonical 6502/65C02 visibility roots each complete a fresh
    fail-closed Level D run and match the exact same-case live Rust CLI bytes;
    six independent 65C02 focused cases prove ordinary, segment, macro,
    statement, and selected compound-expression wildcard imports together with
    full `lib.math.VALUE` and alias-qualified `M.VALUE` ordinary imports; and
    the permanent 68020 harness proves public selective/wildcard lookup plus
    private ordinary-name rejection. The focused reference comparison, native
    formatter, staged native-porting gate, full serial Rust quality gate, and
    `make workflow-gate` pass. The plan-compliance reviewer returned `PASS` for
    the exact staged Item 7.9 implementation on 2026-08-11.
  - Reference-refresh evidence: intentional golden refresh after the user-directed
    rewrite of the wildcard-import corpus root as an explicit 65C02 opcore case
    (`.cpu 65c02`, `lda #VAL`, `brk`). Update mode changed only
    `examples/reference/opcore/use_wildcard_import.hex` and
    `examples/reference/opcore/use_wildcard_import.lst`; no `.map` or `.err`
    artifact changed. The focused non-update reference comparison passes, and
    the same canonical source passes the exact same-case Level D FS-UAE proof.
  - Commit outcome: one independently revertible ordinary-visibility remediation commit.
  - Definition of done: all four visibility-bearing Item 7 roots complete through the real native CLI and match their exact same-case live Rust bytes; private ordinary symbols cannot be imported by wildcard, selective, qualified, or aliased access.

- [x] Item 7.10: resolve native module declarations from configured module roots
  - Source requirement or finding IDs: `I7-D-03`, parent Item 7 complete-corpus Level D failures for `macro_cross_module_ok.asm`, `module_use_autoload.asm`, `project_root/main.asm`, and `statement_cross_module_ok.asm`; each fresh guest run reports `OPC-NCLI018` for a module whose exact support file is present under a configured `-M` root. This item fully closes `I7-D-03`.
  - Activation dependency: Item 7.9 is committed and green.
  - Invariant: native `.use` resolution searches configured module roots deterministically for source files whose declared `.module` name matches the requested name, while preserving explicit root order, cycle handling, and bounded storage.
  - Expected files: native CLI module resolver/source graph boundary only; focused Level A/C root-order and declaration-name model; exact four-root Level D proof; one slice record; this plan’s checkbox/evidence only after closure.
  - Full quality gates: live Rust multi-root oracle; focused native search-order, missing-module, duplicate-name, and capacity tests; `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' RUST_TEST_THREADS=1 cargo test -p asm native_module_autoload_roots_fs_uae -- --nocapture --test-threads=1`; native formatter; staged native-porting gate; full Rust quality gate; `make workflow-gate`.
  - Validation evidence: the focused native module-discovery contract, the 30 runner unit tests, the non-update reference comparison, the exact post-reset four-case canonical Level D FS-UAE test, and the four-case executable boundary test pass. The boundary proof extracts only the requested declaration from a multi-module file, proves a later unrelated source cannot inherit candidate-local match state, and executes native missing, ambiguous, and depth-capacity failures. Each FS-UAE case completed its fresh challenge/start/done/explicit-exit protocol; positive cases matched their same-case live in-memory Rust oracle byte for byte, negative cases completed with a nonzero guest exit and the expected diagnostic, and no `target/fs-uae-*` artifact tree remained. The native formatter, staged native-porting gate, complete serial Rust quality gate, and workflow gate also pass.
  - Plan-compliance review evidence: exact-index reviewer `Hooke` returns `PASS` after confirming all prior code, scope, metadata, parent-expansion, and 65C02-only corpus findings are closed and both final Level D proof sets pass fail-closed with no residual artifacts.
  - Reference-refresh evidence: the user-directed removal of Intel `mvi` runtime adaptation rewrites the four affected canonical roots as stored 65C02 examples. Update mode changed only `examples/reference/opcore/macro_cross_module_ok.hex`, `examples/reference/opcore/macro_cross_module_ok.lst`, `examples/reference/opcore/macro_segment_syntax.lst`, `examples/reference/opcore/statement_boundary_span.lst`, `examples/reference/opcore/statement_cross_module_ok.hex`, and `examples/reference/opcore/statement_cross_module_ok.lst`; no `.map` or `.err` artifact changed. The non-update reference comparison passes.
  - Commit outcome: one independently revertible module-resolution remediation commit.
  - Definition of done: all four autoloaded/cross-file Item 7 roots resolve only their staged support trees and match exact same-case live Rust bytes; missing and ambiguous modules remain deterministic failures.

- [x] Item 7.11: consume native `.ifdef/.ifndef` conditionals before statement dispatch
  - Source requirement or finding IDs: `I7-D-04`, parent Item 7 complete-corpus Level D failure for `preproc_syntax.asm`; native pass one currently reports `.ifdef` as an unknown mnemonic after include expansion. This item fully closes `I7-D-04`.
  - Activation dependency: Item 7.10 is committed and green.
  - Invariant: `.ifdef/.ifndef` select branches from the same command-line/preprocessor symbol environment as Rust, using the existing bounded conditional stack and never dispatching structural directives as ordinary statements.
  - Expected files: native conditional/source-routing boundary only; focused Level A/C defined/undefined branch model; exact `preproc_syntax.asm` Level D proof; one slice record; this plan’s checkbox/evidence only after closure.
  - Full quality gates: live Rust defined/undefined oracle; focused native nesting, else, mismatch, reset, and capacity tests; `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' RUST_TEST_THREADS=1 cargo test -p asm native_preprocessor_conditionals_stored_65c02_fs_uae -- --nocapture --test-threads=1`; native formatter; staged native-porting gate; full Rust quality gate; `make workflow-gate`.
  - Reference-refresh evidence: the user-directed stored-corpus rewrite changes `examples/opcore/preproc_syntax.asm` and `examples/opcore/preproc_syntax.inc` into explicit 65C02 sources with no Intel `mvi` syntax. Update mode changes only `examples/reference/opcore/preproc_syntax.hex` and `examples/reference/opcore/preproc_syntax.lst`; no `.map` or `.err` artifact changes. The non-update `examples_match_reference_outputs` comparison passes.
  - Validation evidence: the exact nine-case Level D test `native_preprocessor_conditionals_stored_65c02_fs_uae` passes in 198.17s. Five independent positive guests prove stored empty/`VAL`/`UNKNOWN` selection, reset after a defined run, and nested selection with explicit zero exits and exact same-case live Rust bytes. Four independent negative guests prove duplicate `.else`, conditional-depth overflow, command-line define-table overflow, and unclosed nesting with completed nonzero exits and the required `OPC-NCLI015`/`OPC-NCLI030` diagnostics. Every case used the fresh challenge/start/done/exit protocol, all nine cases were attempted, and no `target/fs-uae-*` artifact tree remained.
  - Plan-compliance review evidence: exact-index reviewer `Hooke` returns `PASS` after confirming the plan and slice invoke the real nine-case Level D test; executable 65C02 coverage proves stored defined/undefined selection, immediate per-run reset, nested routing, duplicate-else rejection, conditional/define capacity failures, and unclosed-state rejection; scope remains conditional preprocessing plus the intentional stored 65C02 fixture/reference/docs updates, with no unrelated RISC-V staging.
  - Commit outcome: one independently revertible preprocessor-conditional remediation commit.
  - Definition of done: `preproc_syntax.asm` and both defined/undefined focused variants complete through the real native CLI and match exact same-case live Rust bytes; malformed nesting fails deterministically.

- [ ] Item 8: add the section, region, linker, and output opcore parity shard
  - Source requirement or finding IDs: Item 5 assignments for sections, segments, regions, maps, metadata, alignment, linker placement, and CLI-selected output artifacts
  - Activation dependency: a separate CPU/selector semantic-remediation programme is complete and green. That programme does not block Items 7.3g–7.7 source-preprocessor work.
  - Expected files:
    - additive opcore MOS-backed fixtures and matching references assigned by Item 4
    - `crates/opforge-asm/src/native_reference_parity.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
  - Full quality gates:
    - focused Level A/B artifact-contract tests named by the Item 5 shard
    - `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - exact named Level D FS-UAE shard test with `--nocapture --test-threads=1`
    - `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for one section/linker/output coverage shard and exact declared artifact comparisons without bundled native fixes
  - Commit outcome:
    - assigned layout and output cases compare exact CLI-written payloads, listings, maps, metadata, and other declared artifacts with Rust
  - Definition of done:
    - normalization is limited to already-reviewed nondeterministic banner/profile fields
    - every assigned artifact is checked for both presence and exact normalized content
    - every red case pauses for a separate remediation item

- [ ] Item 9: add the deterministic opcore diagnostic parity shard
  - Source requirement or finding IDs: Item 5 assignments for applicable `*_error.asm` and deterministic CLI failure cases
  - Expected files:
    - additive opcore MOS-backed error fixtures and matching `.err` references assigned by Item 4
    - `crates/opforge-asm/src/native_reference_parity.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
  - Full quality gates:
    - focused Level A/B diagnostic-contract tests named by the Item 5 shard
    - `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - exact named Level D FS-UAE diagnostic shard test with `--nocapture --test-threads=1`
    - `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for one deterministic diagnostic shard, exact Rust/native failure equivalence, and no bundled native behavior fix
  - Commit outcome:
    - every assigned reachable error case compares deterministic native CLI diagnostics with Rust and its checked-in `.err` contract
  - Definition of done:
    - host launch failures are distinguished from Amiga-side diagnostic failures
    - error ordering, exit status, and normalized text are compared
    - unreachable error paths remain reviewed exclusions with concrete native blockers

- [ ] Item 10: promote the completed active native reference scope into the mandatory native completion gate
  - Source requirement or finding IDs: user request that these tests “must become part of the standard test run for any native implementation work”; the explicit user reprioritization to make `6502`/`65c02` the only active family scope before anything else advances; existing `scripts/workflow/run_rust_quality_gate.sh`; native rule-pack requirements from `AGENTS.md`
  - Expected files:
    - `scripts/workflow/run_native_porting_quality_gate.py`
    - the repository-native completion wrapper or CI workflow selected for required Level D execution
    - `scripts/workflow/render_quality_gate_preset.py` if needed
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
    - documentation or workflow notes only if the gate surface changes need explicit repo-local guidance
  - Full quality gates:
    - `python3 scripts/workflow/check_workflow_artifact_bundle.py plan documentation/plans/opforge-native-cli-reference-parity-expansion-plan-v0_1.md`
    - `cargo test -p asm native_reference_6502_ -- --nocapture`
    - `cargo test -p asm native_reference_65c02_ -- --nocapture`
    - `cargo test -p asm native_reference_opcore_ -- --nocapture`
    - exact named Level D FS-UAE shard tests with `--nocapture --test-threads=1`
    - `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
    - `make workflow-gate`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for gate promotion only, with deterministic staged checks separated from configured Level D completion execution and no corpus or native behavior changes
  - Commit outcome:
    - native implementation work can no longer bypass the declared `6502`/`65c02` and opcore-on-`6502`/`65c02` parity shards during the standard required workflow
  - Definition of done:
    - the standard native-quality path runs the active native reference parity surface for native-relevant changes
    - the deterministic staged gate does not launch FS-UAE or use the network
    - the configured native completion gate fails on missing Level D
      configuration or parity regressions instead of silently skipping them
    - no on-hold family scope is pulled into the required gate through this plan
    - optional local filtering for iteration remains possible without weakening the required gate

## On-Hold Follow-On Scope

The following scope is intentionally not active in this plan:

- `motorola68000` reference parity:
  on hold until a separate full native implementation plan defines when the
  native CLI path is complete enough to make broad `.srec`/`.lst`/`.err`
  reference parity honest and maintainable
- `motorola6800`/`6809` reference parity:
  optional and on hold; these families are last in priority order here
- `intel8080`/`z80` reference parity:
  optional and on hold; these families are last in priority order here
- `65816`, `45gs02`, and other non-`6502`/non-`65c02` MOS-family expansion:
  on hold in this plan even though they live under `examples/mos6502/**`

Any activation of those scopes should happen in separate follow-on plans once
the native implementation is ready enough for honest CLI-driven reference parity
rather than speculative test scaffolding.

## Blocking Rules

- the active worktree `AGENTS.md` rules remain binding during execution
- the plan must retain a fresh plan-quality `PASS` matching the current
  structure; the 2026-07-04 single-agent review authorizes Item 4
- only one work item or inserted one-invariant remediation item may be active at a time
- no commit before all quality gates pass
- `plan-compliance-reviewer` must return `PASS` before commit
- each work item or phase must end in exactly one new commit before the next item starts
- native behavior edits require approved slice metadata and
  `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`
- Level D FS-UAE evidence must use the exact named one-shot test with
  `--test-threads=1`; broad filters and silent skips do not satisfy completion
- a failing coverage case pauses its shard and becomes a separate
  one-invariant remediation item before any production fix
- Item 5 cannot begin until Items 4.1–4.8 close every RQ finding
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- archive completed plans with `scripts/workflow/archive_completed_plan.sh`
