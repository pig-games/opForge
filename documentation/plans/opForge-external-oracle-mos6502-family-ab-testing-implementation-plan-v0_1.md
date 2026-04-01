# opForge MOS 6502 Family External-Oracle A/B Implementation Plan

## Metadata

- Source: `documentation/opForge-external-oracle-mos6502-family-ab-testing-spec-v0_1.md` (`REQ-EXTAB6502-001` through `REQ-EXTAB6502-011`, `AC-EXTAB6502-001` through `AC-EXTAB6502-009`)
- Mode: `implementation`
- Owner: Codex

## Objective

Extend the existing Rust-native external-oracle harness so opForge can run an
opt-in, skip-safe `64tass`-backed A/B workflow for the shipped `mos6502`
family surface. Execution should start with one narrow `m6502` success-path
vertical slice, then expand to the other governed CPUs, bounded negative-path
comparison, documented divergence handling, and explicit workflow integration
without affecting the default `examples/reference` or workspace test paths.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- The existing `examples/reference` golden-reference workflow remains the
  primary oracle for opForge-owned examples and must not be widened or replaced.
- External-oracle fixtures for this effort must live only under
  `examples/ab/mos6502/64tass/` and must not be merged into
  `examples/reference`.
- The governed family id and CPU ids remain fixed to `mos6502`, `m6502`,
  `65c02`, `65816`, and `45gs02`.
- The plan must stay implementation-first and slice-first: prove one working
  `m6502` byte-parity path before broadening to the rest of the family.
- Default local and CI/workspace test paths must remain dependency-free and
  must not require `64tass`; the workflow stays opt-in and skip-safe.
- The adapter boundary and metadata model must remain reusable for non-`mos6502`
  families; do not redesign the generic harness when a bounded extension is
  sufficient.
- Shared-subset fixtures must be curated from existing `examples/mos6502/`
  sources wherever possible rather than copied wholesale or replaced with ad
  hoc synthetic coverage.
- Success comparison remains semantic-bytes-first; error comparison remains
  normalized-error-class-first; listings, maps, symbol tables, and other
  non-byte artifacts stay out of scope for this plan.
- Intentional `opForge` versus `64tass` differences must remain visible through
  documented-divergence fixtures instead of silent skips.

## Planning Decisions Captured Up Front

- Reuse the existing `crates/opforge-asm/src/external_oracle.rs` harness and
  `crates/opforge-asm/src/oracle/` adapter boundary; add a dedicated `64tass`
  adapter module instead of redesigning the harness.
- Keep the first landed slice as small as possible: `m6502`, positive fixtures,
  flat comparable bytes, explicit `64tass` opt-in gating, and clean skip
  behavior when the binary is unavailable.
- Expand family coverage one CPU at a time so `65c02`, `65816`, and `45gs02`
  each land as their own commit-sized corpus and adapter extension.
- Treat `65816` width/bank-state assumptions and `45gs02` target selection as
  explicit adapter/fixture concerns, not ambient tool defaults.
- Defer developer-command and workflow-polish changes until the core `64tass`
  path, negative-path normalization, and documented divergence handling are all
  proven.

## Work Items

- [x] Work item 1: land the first end-to-end `64tass` success-path vertical slice for `m6502`
  - Source requirement or finding IDs: `REQ-EXTAB6502-001`, `REQ-EXTAB6502-002`, `REQ-EXTAB6502-004`, `REQ-EXTAB6502-005`, `REQ-EXTAB6502-008`, `REQ-EXTAB6502-009`, `REQ-EXTAB6502-010`, `AC-EXTAB6502-001`, `AC-EXTAB6502-003`, `AC-EXTAB6502-006`, `AC-EXTAB6502-008`, `AC-EXTAB6502-009`
  - Validation: add and run a focused `64tass` success-path test filter, verify skip-safe behavior when `64tass` is absent, and rerun `cargo test -p asm examples_match_reference_outputs`.
  - Definition of done: one opt-in, skip-safe `m6502` path assembles curated shared-subset fixtures through `64tass` and compares normalized semantic bytes end to end.
  - Expected files:
    - `crates/opforge-asm/src/oracle/mod.rs`
    - `crates/opforge-asm/src/oracle/tass64.rs`
    - `crates/opforge-asm/src/external_oracle.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/ab/mos6502/64tass/m6502/fixtures.toml`
    - `examples/ab/mos6502/64tass/m6502/positive/*.asm`
  - Full quality gates:
    - add adapter-availability tests for `OPFORGE_EXTERNAL_ORACLE_64TASS` and `OPFORGE_64TASS_BIN`
    - prove `m6502` command construction and flat-binary artifact discovery for `64tass`
    - derive the initial positive fixtures from existing `examples/mos6502/6502_simple.asm`, `examples/mos6502/6502_allmodes.asm`, or narrower shared-subset extracts from them
    - verify mismatch reporting includes fixture id, family, cpu, oracle id, compare mode, output paths, and concrete byte-difference summary
    - run `cargo test -p asm external_oracle_`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the minimum new adapter, manifest, fixture set, and harness wiring required for one working `m6502` success path
    - compliance note explicitly states that `65c02`, `65816`, `45gs02`, error-class comparison, documented divergence, and workflow integration remain deferred
  - Commit outcome:
    - one real `mos6502`/`64tass` shared-subset path works end to end and proves the first family-specific external-oracle slice without disturbing default test paths
  - Definition of done:
    - the repository contains a new dedicated `examples/ab/mos6502/64tass/m6502/` corpus root
    - a dedicated `64tass` adapter exists and is opt-in plus skip-safe
    - the harness can compare opForge and `64tass` success output by normalized semantic bytes for `m6502`
    - default reference behavior remains unchanged when `64tass` is absent

- [x] Work item 2: extend the proven `64tass` path to the curated `65c02` success corpus
  - Source requirement or finding IDs: `REQ-EXTAB6502-003`, `REQ-EXTAB6502-004`, `REQ-EXTAB6502-005`, `REQ-EXTAB6502-009`, `REQ-EXTAB6502-010`, `AC-EXTAB6502-002`, `AC-EXTAB6502-003`
  - Validation: run the `64tass` external-oracle suite with configured `65c02` coverage and rerun `cargo test -p asm examples_match_reference_outputs`.
  - Definition of done: the workflow contains a separate `65c02` manifest and can compare curated shared-subset `65c02` success fixtures by normalized semantic bytes.
  - Expected files:
    - `crates/opforge-asm/src/oracle/tass64.rs`
    - `crates/opforge-asm/src/external_oracle.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/ab/mos6502/64tass/65c02/fixtures.toml`
    - `examples/ab/mos6502/64tass/65c02/positive/*.asm`
  - Full quality gates:
    - prove explicit `65c02` target selection in the adapter rather than falling back to a baseline 6502 mode
    - seed fixtures from existing `examples/mos6502/65c02_simple.asm`, `examples/mos6502/65c02_allmodes.asm`, or shared-subset extracts from them
    - verify deterministic multi-manifest discovery/execution ordering across `m6502` and `65c02`
    - run `cargo test -p asm external_oracle_`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to `65c02` adapter support and corpus seeding only
    - compliance note explicitly states that `65816`, `45gs02`, negative fixtures, and divergence handling remain deferred
  - Commit outcome:
    - the `mos6502` family external-oracle corpus now covers the first carry-forward CPU beyond baseline `m6502`
  - Definition of done:
    - a separate `65c02` manifest exists and runs
    - `65c02` shared-subset integer/addressing coverage is visible in the corpus
    - the family harness still remains deterministic and skip-safe

- [x] Work item 3: extend the success corpus to `65816` with explicit state-sensitive fixture handling
  - Source requirement or finding IDs: `REQ-EXTAB6502-003`, `REQ-EXTAB6502-004`, `REQ-EXTAB6502-005`, `REQ-EXTAB6502-009`, `REQ-EXTAB6502-010`, `AC-EXTAB6502-002`, `AC-EXTAB6502-003`
  - Validation: run the `64tass` suite with configured `65816` fixtures that make width/bank-state assumptions explicit, then rerun `cargo test -p asm examples_match_reference_outputs`.
  - Definition of done: the workflow contains a separate `65816` manifest and can compare curated shared-subset `65816` success fixtures without relying on ambient width or bank defaults.
  - Expected files:
    - `crates/opforge-asm/src/oracle/tass64.rs`
    - `crates/opforge-asm/src/external_oracle.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/ab/mos6502/64tass/65816/fixtures.toml`
    - `examples/ab/mos6502/64tass/65816/positive/*.asm`
  - Full quality gates:
    - prove explicit `65816` target selection and any required profile or fixture-metadata plumbing needed to keep width/bank-state behavior deterministic
    - seed fixtures from existing `examples/mos6502/65816_simple.asm`, `examples/mos6502/65816_allmodes.asm`, and explicit-state examples such as `examples/mos6502/65816_assume_state.asm`
    - verify the loader rejects missing state-sensitive metadata when the shared subset requires it
    - run `cargo test -p asm external_oracle_`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to `65816` adapter/profile support and corpus seeding only
    - compliance note explicitly states that `45gs02`, negative fixtures, and divergence handling remain deferred
  - Commit outcome:
    - the external-oracle corpus now covers `65816` with explicit, reviewable shared-subset state handling
  - Definition of done:
    - a separate `65816` manifest exists and runs
    - state-sensitive shared-subset assumptions are explicit in source or metadata
    - the harness remains generic rather than adding `65816`-only special cases outside the adapter/metadata boundary

- [x] Work item 4: extend the success corpus to `45gs02` with explicit oracle target mapping
  - Source requirement or finding IDs: `REQ-EXTAB6502-003`, `REQ-EXTAB6502-004`, `REQ-EXTAB6502-005`, `REQ-EXTAB6502-009`, `REQ-EXTAB6502-010`, `AC-EXTAB6502-002`, `AC-EXTAB6502-003`
  - Validation: run the `64tass` suite with configured `45gs02` fixtures, verify the adapter selects the intended `45GS02` target rather than a narrower fallback, and rerun `cargo test -p asm examples_match_reference_outputs`.
  - Definition of done: the workflow contains a separate `45gs02` manifest and can compare curated shared-subset `45gs02` success fixtures by normalized semantic bytes using an explicit `45GS02` oracle target.
  - Expected files:
    - `crates/opforge-asm/src/oracle/tass64.rs`
    - `crates/opforge-asm/src/external_oracle.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/ab/mos6502/64tass/45gs02/fixtures.toml`
    - `examples/ab/mos6502/64tass/45gs02/positive/*.asm`
  - Full quality gates:
    - prove adapter command construction for the `45GS02` target and reject narrower fallback modes
    - seed fixtures from existing `examples/mos6502/45gs02_simple.asm`, `examples/mos6502/45gs02_extensions.asm`, and other curated shared-subset `45gs02_*` examples
    - verify four-manifest discovery/execution ordering across `m6502`, `65c02`, `65816`, and `45gs02`
    - run `cargo test -p asm external_oracle_`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to `45gs02` adapter support and corpus seeding only
    - compliance note explicitly states that negative fixtures, documented divergence, and workflow polish remain deferred
  - Commit outcome:
    - the governed `mos6502` CPU surface is covered end to end for the success-path shared subset
  - Definition of done:
    - a separate `45gs02` manifest exists and runs
    - the adapter no longer leaves `45gs02` target selection implicit
    - all four governed CPU manifests can execute through the same generic harness boundary

- [x] Work item 5: add bounded negative-fixture support and `64tass` error-class normalization for deterministic shared-subset failures
  - Source requirement or finding IDs: `REQ-EXTAB6502-006`, `REQ-EXTAB6502-009`, `REQ-EXTAB6502-010`, `REQ-EXTAB6502-011`, `AC-EXTAB6502-004`, `AC-EXTAB6502-007`, `AC-EXTAB6502-009`
  - Validation: add and run normalization-focused tests plus curated negative-fixture tests, then rerun `cargo test -p asm examples_match_reference_outputs`.
  - Definition of done: `expected_outcome = error` fixtures compare bounded normalized error classes for deterministic MOS-family shared-subset failures instead of raw stderr text.
  - Expected files:
    - `crates/opforge-asm/src/normalization.rs`
    - `crates/opforge-asm/src/oracle/tass64.rs`
    - `crates/opforge-asm/src/external_oracle.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/ab/mos6502/64tass/**/fixtures.toml`
    - `examples/ab/mos6502/64tass/**/negative/*.asm`
  - Full quality gates:
    - add `64tass` stderr normalization coverage for the bounded taxonomy named in the spec such as `unknown-mnemonic`, `illegal-addressing-mode`, `unsupported-cpu-feature`, `branch-out-of-range`, `value-out-of-range`, `syntax-error`, `missing-operand`, and `wrong-operand-count`
    - verify `expected_outcome = error` requires both tools to fail and mismatches when statuses or normalized classes diverge
    - seed curated negative fixtures only for CPUs where cross-tool failure behavior is deterministic and reviewable
    - verify structured mismatch output includes both normalized classes plus short diagnostic excerpts
    - run `cargo test -p asm external_oracle_`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to bounded negative-path comparison and deterministic negative-fixture seeding
    - compliance note explicitly states that the taxonomy remains intentionally bounded rather than mirroring raw tool diagnostics
  - Commit outcome:
    - the `mos6502`/`64tass` workflow can validate deterministic shared-subset failures by normalized error class
  - Definition of done:
    - a bounded shared error taxonomy exists and is tested for `64tass`
    - negative fixtures are represented only where the shared subset makes cross-tool failure comparison stable
    - error mismatches are actionable without requiring raw stderr equality

- [x] Work item 6: add documented-divergence fixtures and spec-complete divergence contract enforcement for MOS-family gaps
  - Source requirement or finding IDs: `REQ-EXTAB6502-007`, `REQ-EXTAB6502-009`, `REQ-EXTAB6502-010`, `REQ-EXTAB6502-011`, `AC-EXTAB6502-005`, `AC-EXTAB6502-007`, `AC-EXTAB6502-009`
  - Validation: add and run documented-divergence fixture coverage, verify non-failing behavior only when the observed mismatch still matches the declared divergence contract, then rerun `cargo test -p asm examples_match_reference_outputs`.
  - Definition of done: documented divergences are first-class, machine-checkable MOS-family fixtures and remain visible without becoming silent skips.
  - Expected files:
    - `crates/opforge-asm/src/external_oracle.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/ab/mos6502/64tass/**/fixtures.toml`
    - `examples/ab/mos6502/64tass/**/documented_divergence/*.asm`
  - Full quality gates:
    - enforce the required divergence metadata fields from the spec, including `documented_divergence_kind`, expected per-tool status, required normalized error classes when applicable, and `documented_divergence_reason`
    - support and test the divergence kinds required by the spec: `opforge_error_oracle_success`, `opforge_success_oracle_error`, `byte_mismatch`, and `error_class_mismatch`
    - add at least one MOS-family documented-divergence fixture proving the path is visible but non-failing while the contract still matches observed behavior
    - verify a fully matching divergence fixture is reported as a reclassification candidate
    - run `cargo test -p asm external_oracle_`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to documented-divergence contract enforcement and fixture seeding
    - compliance note explicitly states that default workflow wiring still remains unchanged in this commit
  - Commit outcome:
    - intentional `opForge` versus `64tass` differences remain explicit, reviewable, and machine-checked instead of being hidden
  - Definition of done:
    - divergence fixtures are first-class metadata rather than ad hoc skips
    - the harness enforces the spec’s divergence kinds and metadata requirements
    - divergence reporting stays actionable and visible to contributors

- [ ] Work item 7: wire the `64tass` path into the explicit opt-in developer workflow and preserve default workspace safety
  - Source requirement or finding IDs: `REQ-EXTAB6502-008`, `REQ-EXTAB6502-009`, `REQ-EXTAB6502-011`, `AC-EXTAB6502-006`, `AC-EXTAB6502-007`, `AC-EXTAB6502-008`, `AC-EXTAB6502-009`
  - Validation: verify the explicit external-oracle command exercises the `64tass` suite when enabled, skips cleanly when `64tass` is unavailable, and leaves default workspace/reference tests dependency-free.
  - Definition of done: the finished `mos6502`/`64tass` workflow is runnable through an explicit opt-in command and remains harmless when not enabled.
  - Expected files:
    - `Makefile`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/oracle/tass64.rs`
    - related workflow notes only if command-surface behavior changes need direct user-facing documentation
  - Full quality gates:
    - pass through `OPFORGE_EXTERNAL_ORACLE_64TASS` and `OPFORGE_64TASS_BIN` in the explicit external-oracle command path without forcing them into default test targets
    - verify the dedicated command can run the full `mos6502`/`64tass` manifest set end to end when `64tass` is configured
    - verify the same command reports a structured skip instead of a regression when `64tass` is unavailable
    - run `cargo test --workspace` or the repository’s current default test equivalent without `64tass`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to explicit workflow wiring and environment-variable pass-through
    - compliance note explicitly states that default local and CI execution remain dependency-free and the `64tass` path stays opt-in
  - Commit outcome:
    - contributors and CI can run the `mos6502`/`64tass` workflow intentionally without disturbing the normal opForge reference workflow
  - Definition of done:
    - one explicit developer command exists for the `64tass` path
    - default workspace and reference paths still pass without `64tass`
    - mismatch and skip output remain actionable under the explicit workflow entry point

## Milestones

- [x] Milestone 1: the first `64tass` baseline path is landed for `m6502` and `65c02` (`Work item 1` and `Work item 2`)
- [x] Milestone 2: the governed success-path CPU surface is landed through `65816` and `45gs02` (`Work item 3` and `Work item 4`)
- [x] Milestone 3: bounded negative-path and documented-divergence handling are landed (`Work item 5` and `Work item 6`)
- [ ] Milestone 4: explicit workflow integration is landed without disturbing default test paths (`Work item 7`)

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- do not widen the first slice beyond `m6502`, curated shared-subset positive fixtures, normalized semantic-byte comparison, and skip-safe `64tass` adapter bring-up
- do not merge external-oracle fixtures into `examples/reference` or let `64tass` availability affect default workspace or reference test success
- do not hide MOS-family gaps through silent skips; use deterministic negative fixtures or documented-divergence fixtures as required by the spec
- do not allow `45gs02` to silently run against a narrower oracle target than the intended `45GS02` surface
