# Option B external-oracle A/B harness implementation plan

## Metadata

- Source: `documentation/opForge-external-assembler-ab-testing-proposal-v0_1.md` (`Option B`, `REQ-EXTAB-001` through `REQ-EXTAB-010`, `DC-EXTAB-001` through `DC-EXTAB-007`)
- Mode: `implementation`
- Owner: Copilot

## Objective

Implement Option B as a Rust-native, metadata-driven external-oracle A/B harness inside the existing `opforge-asm` test infrastructure. The first shipped slice should add an environment-gated `vasm` adapter for Motorola 68000/68010 shared-subset fixtures, compare success cases by flat output bytes, skip cleanly when the external tool is unavailable, and leave the existing `examples/reference` workflow untouched. Follow-on slices should add normalized negative-path comparison, documented-divergence handling, actionable mismatch reporting, and opt-in workflow integration.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at all times during plan execution.
- The normal opForge golden-reference workflow remains the primary oracle for `examples/reference` and must not be widened or replaced.
- External-oracle fixtures must live in a dedicated corpus separate from `examples/reference`.
- The default local test path and default CI path must not require `vasm` or any other third-party assembler to be installed.
- The first vertical slice must stay narrow: `68000` and `68010`, curated shared-subset fixtures, flat-binary success comparison first.
- The first implementation should live in Rust code under `crates/opforge-asm`, not in a shell-script-only workflow and not in an `xtask` CLI.
- The first adapter supports flat-binary output only; object-file, listing, map, and relocation comparison remain out of scope for this plan.
- Error comparison must normalize both opForge and oracle failures into a bounded shared taxonomy instead of matching raw stderr text.
- Intentional differences must be represented as documented-divergence fixtures, not ad hoc skips.

## Planning decisions captured up front

- Fixture metadata shape for the first slice: one manifest-style TOML file under `examples/ab/motorola68000/vasm/` that indexes fixture files and shared metadata fields. This keeps the first slice simple while preserving a generic metadata model.
- First implementation location: reusable Rust helper modules in `crates/opforge-asm/src/` plus one focused test entry point in `crates/opforge-asm/src/tests.rs`.
- Documented-divergence behavior: divergence fixtures remain non-failing only while their observed mismatch still matches the documented divergence contract; a fully matching result should be reported as a reclassification candidate.
- Validation baseline: every slice must prove it did not disturb `make reference-test` and must keep the external harness opt-in and skip-safe when `vasm` is unavailable.

## Work Items

- [x] Work item 1: deliver the first end-to-end `vasm` success-path vertical slice for curated `68000`/`68010` fixtures
  - Source requirement or finding IDs: `REQ-EXTAB-001`, `REQ-EXTAB-002`, `REQ-EXTAB-003`, `REQ-EXTAB-004`, `REQ-EXTAB-007`, `REQ-EXTAB-009`, `REQ-EXTAB-010`, `DC-EXTAB-001`, `DC-EXTAB-002`, `DC-EXTAB-004`, `DC-EXTAB-005`, `DC-EXTAB-007`
  - Validation: run the focused opt-in `vasm` success-path test, confirm skip-safe behavior when `vasm` is unavailable, and rerun `cargo test -p asm examples_match_reference_outputs`.
  - Definition of done: one opt-in, skip-safe, end-to-end `vasm` byte-parity path works for curated `68000`/`68010` success fixtures.
  - Expected files:
    - `examples/ab/motorola68000/vasm/fixtures.toml`
    - `examples/ab/motorola68000/vasm/positive/*.asm`
    - `crates/opforge-asm/src/external_oracle.rs`
    - `crates/opforge-asm/src/oracle/vasm.rs`
    - `crates/opforge-asm/src/lib.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - add one opt-in focused test entry point, e.g. `cargo test -p asm external_oracle_vasm_68000_`
    - verify the harness skips cleanly with an explicit message when `vasm` is not configured
    - with `vasm` available, verify curated `68000` and `68010` success fixtures compare by final flat bytes
    - verify mismatch reporting includes fixture id, cpu, oracle id, compare mode, output paths, and byte-length/offset summary
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the minimum manifest, loader logic, harness code, adapter logic, and success-fixture execution required for one working `vasm` path
    - compliance note explicitly states that negative-path comparison, documented divergence, and broader workflow wiring are deferred
  - Commit outcome:
    - one opt-in, skip-safe, end-to-end external-oracle path works for a curated `68000`/`68010` shared subset and proves Option B on real code
  - Definition of done:
    - the repository contains the new `examples/ab/...` corpus separate from `examples/reference`
    - a minimal manifest and loader can drive a curated positive fixture set
    - the thin `vasm` adapter can run the first flat-binary profile
    - the harness compares opForge and `vasm` success results by normalized flat bytes
    - failures surface actionable byte mismatch summaries
    - default test execution remains green when `vasm` is absent

- [x] Work item 2: harden the proven path into a bounded generic harness boundary and stronger metadata validation
  - Source requirement or finding IDs: `REQ-EXTAB-003`, `REQ-EXTAB-007`, `REQ-EXTAB-008`, `REQ-EXTAB-010`, `DC-EXTAB-004`, `DC-EXTAB-005`, `DC-EXTAB-006`
  - Validation: run `cargo test -p asm external_oracle_` plus `cargo test -p asm examples_match_reference_outputs`, and confirm the success fixtures from Work item 1 still pass unchanged.
  - Definition of done: the proven `vasm` path is extracted into a bounded reusable harness boundary without widening scope or behavior.
  - Expected files:
    - `crates/opforge-asm/src/external_oracle.rs`
    - `crates/opforge-asm/src/oracle/mod.rs`
    - `crates/opforge-asm/src/oracle/vasm.rs`
    - `crates/opforge-asm/src/lib.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/ab/motorola68000/vasm/fixtures.toml`
  - Full quality gates:
    - add unit tests covering malformed metadata, adapter availability checks, fixture dispatch, temp-output management, and structured mismatch/skip result objects
    - verify the end-to-end success fixtures from Work item 1 still pass unchanged after the extraction
    - run `cargo test -p asm external_oracle_`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to extracting only the reusable boundary already proven by Work item 1
    - compliance note explicitly states that no new families, no new oracle types, and no object/listing comparison are introduced here
  - Commit outcome:
    - the first working path is hardened into a reusable but still bounded generic harness boundary without widening behavior
  - Definition of done:
    - a reusable adapter trait/boundary exists because the first concrete path proved the required shape
    - metadata validation is stricter and tested
    - the same curated success fixtures still pass
    - the normal reference workflow remains unaffected

- [x] Work item 3: add bounded error-class normalization and negative-fixture comparison
  - Source requirement or finding IDs: `REQ-EXTAB-005`, `REQ-EXTAB-007`, `REQ-EXTAB-008`, `REQ-EXTAB-010`, `DC-EXTAB-003`, `DC-EXTAB-004`, `DC-EXTAB-005`, `DC-EXTAB-006`
  - Validation: run normalization-focused tests, the external-oracle negative-path tests, and `cargo test -p asm examples_match_reference_outputs`.
  - Definition of done: curated negative fixtures compare by bounded normalized error class rather than raw stderr text.
  - Expected files:
    - `crates/opforge-asm/src/normalization.rs`
    - `crates/opforge-asm/src/oracle/vasm.rs`
    - `crates/opforge-asm/src/external_oracle.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/ab/motorola68000/vasm/fixtures.toml`
    - `examples/ab/motorola68000/vasm/negative/*.asm`
  - Full quality gates:
    - add normalization tests for both opForge diagnostics and representative `vasm` stderr text
    - verify both tools must fail for `expected_outcome = error`
    - verify mismatched failure classes fail with a structured summary naming both normalized classes and short diagnostic excerpts
    - run `cargo test -p asm external_oracle_`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the bounded taxonomy, negative fixtures, and error-class comparison
    - compliance note explicitly states that the taxonomy remains intentionally bounded and documented rather than mirroring raw tool-specific diagnostics
  - Commit outcome:
    - negative shared-subset fixtures can be compared by normalized error class instead of raw stderr text
  - Definition of done:
    - a bounded shared error taxonomy exists and is tested
    - opForge and `vasm` error paths are normalized independently into that taxonomy
    - `expected_outcome = error` fixtures fail only when tool outcomes or normalized classes diverge

- [x] Work item 4: add documented-divergence handling and reporting polish
  - Source requirement or finding IDs: `REQ-EXTAB-006`, `REQ-EXTAB-007`, `REQ-EXTAB-010`, `DC-EXTAB-004`, `DC-EXTAB-005`
  - Validation: run documented-divergence fixture coverage under `cargo test -p asm external_oracle_` and confirm `cargo test -p asm examples_match_reference_outputs` still passes.
  - Definition of done: intentional differences are represented as first-class divergence fixtures and reported visibly without becoming silent skips.
  - Expected files:
    - `crates/opforge-asm/src/external_oracle.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/ab/motorola68000/vasm/fixtures.toml`
    - `examples/ab/motorola68000/vasm/documented_divergence/*.asm`
  - Full quality gates:
    - add at least one documented-divergence fixture and verify it is visible but non-failing when its observed mismatch matches the documented contract
    - verify a divergence fixture that becomes fully comparable is reported as a reclassification candidate
    - run `cargo test -p asm external_oracle_`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to documented divergence visibility and reporting polish only
    - compliance note explicitly states that default workflow wiring remains unchanged in this commit
  - Commit outcome:
    - the first Option B harness can represent intentional differences explicitly and report them without turning them into silent skips
  - Definition of done:
    - documented divergences are first-class metadata, not silent skips
    - mismatch reports contain the fields promised by the proposal
    - the workflow remains harmless when not enabled
    - `make reference-test` remains clean without requiring `vasm`

- [x] Work item 5: add explicit opt-in developer and CI workflow integration
  - Source requirement or finding IDs: `REQ-EXTAB-007`, `REQ-EXTAB-010`, `DC-EXTAB-004`, `DC-EXTAB-005`
  - Validation: verify the explicit Makefile or CI entry point runs end to end when enabled, skips cleanly when `vasm` is unavailable, and leaves `cargo test --workspace` dependency-free by default.
  - Definition of done: the finished harness has explicit opt-in local and CI entry points without disturbing the default test path.
  - Expected files:
    - `Makefile`
    - optional CI workflow file if a dedicated opt-in job is added during this slice
    - `crates/opforge-asm/src/tests.rs`
    - related documentation only if command or entry-point behavior needs direct user-facing notes
  - Full quality gates:
    - add and validate an opt-in developer command such as `make test-external-oracle` or equivalent
    - verify `cargo test --workspace` still passes without requiring `vasm`
    - verify the dedicated external-oracle target skips cleanly when `vasm` is unavailable
    - when `vasm` is available, run the dedicated external-oracle target end to end
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to opt-in workflow wiring
    - compliance note explicitly states that default CI remains dependency-free and the external job stays gated behind explicit configuration
  - Commit outcome:
    - the first Option B implementation is runnable through an explicit developer/CI entry point without disturbing the default test path
  - Definition of done:
    - a clear opt-in command exists for local and CI use
    - default local and CI execution remain dependency-free
    - the external-oracle path is runnable when explicitly enabled and harmless when not enabled

## Milestones

- [x] Milestone 1: first end-to-end `vasm`-backed `68000`/`68010` success-path byte parity slice is landed (`Work item 1`)
- [x] Milestone 2: the proven path is hardened into a bounded reusable harness and negative-path comparison is added (`Work item 2` and `Work item 3`)
- [x] Milestone 3: documented divergence and explicit workflow integration are landed (`Work item 4` and `Work item 5`)

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- do not widen the first shipped slice beyond `68000`/`68010`, curated shared-subset fixtures, flat-byte success comparison, dedicated corpus separation, skip-safe opt-in execution, and the byte-mismatch reporting needed for that concrete path
- do not merge external-oracle fixtures into `examples/reference` or let external-tool availability affect default test success
