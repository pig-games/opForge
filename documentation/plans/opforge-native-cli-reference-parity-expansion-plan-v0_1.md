<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# opForge Native CLI Reference Parity Expansion Plan v0.1

## Metadata

- Source: User request on 2026-06-19 to create an official plan that expands native CLI FS-UAE parity coverage across the `examples/reference` corpus and makes that coverage mandatory for native implementation work.
- Mode: implementation
- Owner: Codex
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.

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
- Native 68000 implementation items must load `agents/rules/native-68000.md`
  and run the native formatter gate before completion.
- FS-UAE-backed items must load `agents/rules/fs-uae.md` and use the one-shot
  `cargo test` invocation style documented there.
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

- Item 1 is complete: implementation landed in a focused manifest/completeness
  slice, all listed executable quality gates pass locally, and
  `plan-compliance-reviewer` returned `PASS` for the Item 1 boundary.
- Items 2 through 5 remain in progress or blocked by the still-red real
  FS-UAE native CLI failure-path shard, so they are not eligible to be checked
  off yet.

## Work Items

- [x] Item 1: add a governed native reference applicability manifest and completeness guard
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

- [ ] Item 4: create `6502`/`65c02`-backed opForge Core parity fixtures and cover the applicable opcore reference corpus
  - Source requirement or finding IDs: user note that some opForge Core cases currently use 8080/Z80 spellings and may need `6502`-adapted copies first; existing `examples/opcore/**` and `examples/reference/opcore/**` corpora; current Rust-side reference harness behavior
  - Expected files:
    - `examples/opcore/**` or a parallel native-targeted fixture directory if additive copies are used
    - `examples/reference/opcore/**` for any newly introduced native-targeted copies
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
  - Full quality gates:
    - `cargo test -p asm native_reference_opcore_ -- --nocapture`
    - `cargo test -p asm examples_match_reference_outputs -- --nocapture`
    - `cargo test -p asm external_fs_uae_opforge_native_cli_ -- --nocapture --test-threads=1`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to opForge Core fixture adaptation and native parity coverage, with additive `6502`/`65c02` copies where required and no unrelated corpus migration
  - Commit outcome:
    - opForge Core behavior covered by the Rust example/reference corpus is also represented by native CLI parity tests, using `6502`/`65c02`-backed copies where CPU-specific original fixtures are not directly applicable
  - Definition of done:
    - CPU-neutral opcore cases run without unnecessary duplication
    - CPU-bound 8080/Z80 opcore cases that are still relevant to core functionality gain explicit MOS-backed parity fixtures first
    - original canonical examples remain intact unless a reviewed change is clearly safer than additive copies

- [ ] Item 5: promote `6502`/`65c02` native reference parity into the standard mandatory gate for native implementation work
  - Source requirement or finding IDs: user request that these tests “must become part of the standard test run for any native implementation work”; the explicit user reprioritization to make `6502`/`65c02` the only active family scope before anything else advances; existing `scripts/workflow/run_rust_quality_gate.sh`; native rule-pack requirements from `AGENTS.md`
  - Expected files:
    - `scripts/workflow/run_rust_quality_gate.sh`
    - `scripts/workflow/render_quality_gate_preset.py` if needed
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
    - documentation or workflow notes only if the gate surface changes need explicit repo-local guidance
  - Full quality gates:
    - `python3 scripts/workflow/check_workflow_artifact_bundle.py plan documentation/plans/opforge-native-cli-reference-parity-expansion-plan-v0_1.md`
    - `cargo test -p asm native_reference_6502_ -- --nocapture`
    - `cargo test -p asm native_reference_65c02_ -- --nocapture`
    - `cargo test -p asm native_reference_opcore_ -- --nocapture`
    - `cargo test -p asm external_fs_uae_opforge_native_cli_ -- --nocapture --test-threads=1`
    - `scripts/workflow/run_native_68000_format_gate.sh`
    - `RUST_TEST_THREADS=1 scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to gate promotion, native change-scope enforcement, and deterministic mandatory-parity execution rules for the active `6502`/`65c02` plus opcore-on-`6502`/`65c02` scope only
  - Commit outcome:
    - native implementation work can no longer bypass the declared `6502`/`65c02` and opcore-on-`6502`/`65c02` parity shards during the standard required workflow
  - Definition of done:
    - the standard native-quality path runs the active native reference parity surface for native-relevant changes
    - the repo’s required workflow clearly fails on parity regressions instead of silently skipping them
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
- no commit before all quality gates pass
- `plan-compliance-reviewer` must return `PASS` before commit
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- archive completed plans with `scripts/workflow/archive_completed_plan.sh`
