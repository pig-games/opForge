# Amiga Hunk output component implementation plan

## Metadata

- Source: `documentation/opForge-amiga-hunk-output-component-spec-v0_1.md` (`Goals`, `Invariants / Constraints`, `Behavioral Contract`, `Boundary Cases`, and `Acceptance Criteria`)
- Mode: `implementation`
- Owner: Copilot

## Objective

Implement the first internal output-component seam for `.output format=...` and ship a v0.1 AmigaDOS `format=hunk` executable writer that is honest about its limits. The shipped slice must preserve existing `bin` and `prg` behavior, emit exact regular-executable Hunk headers and segment records for explicitly relocation-free selected sections, reject unsupported flat-image options and unsafe relocation cases with deterministic diagnostics, and keep optional FS-UAE execution outside the generic writer path.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at all times during plan execution.
- The plan must not widen scope beyond the reviewed v0.1 spec.
- Existing `bin` and `prg` `.output` behavior must remain behaviorally stable.
- The output-component registry stays internal and static in v0.1.
- `format=hunk` must not silently emit payloads when relocation safety is unproven.
- `HUNK_RELOC32`, object-file output, overlays, debug hunks, symbol hunks, and Amiga memory-attribute customization remain out of scope for this plan.
- The default workspace quality gate must remain green without FS-UAE installed.
- One active work item at a time; each completed work item ends in exactly one new commit before the next item begins.

## Planning decisions captured up front

- The first parser or model slice changes the `.output` directive data shape before it adds any Hunk payload generation so format-specific validation can actually move behind components.
- The first component registry slice migrates `bin` and `prg` behind the new seam before `hunk` is added so compatibility regressions are visible in isolation.
- The first Hunk writer slice supports exact regular-executable headers, preserved `sections=` order, `HUNK_CODE`/`HUNK_DATA`/`HUNK_BSS`, padded payload words, allocation-size versus payload-size distinction, and deterministic rejection of unsupported options.
- The first relocation slice is deliberately narrow: `crates/opforge-asm/src/engine.rs` introduces the explicit relocation-free disposition that is carried through `crates/opforge-vm/src/output_model.rs` into the Hunk component input model, and the Hunk writer accepts only inputs that carry that explicit disposition; relocation-hunk emission is deferred.
- The v0.1 Hunk subset makes the remaining boundary choices explicit: empty non-BSS sections are omitted deterministically in the component writer, while selected sections without assigned bases fail with deterministic `format=hunk` diagnostics in the live `.output` path.
- Optional FS-UAE execution is a final opt-in validation slice only after byte-level writer correctness is already proven in focused tests.

## Work Items

- [x] Work item 1: refactor `.output` parsing and stored metadata into a format-id plus raw option bag contract
  - Source requirement or finding IDs: spec `Behavioral Contract` parser contract; spec `Acceptance Criteria` bullets for parser acceptance and raw option preservation
  - Validation: focused parser and stored-metadata coverage plus the full repo quality gate before commit
  - Definition of done: `.output` stores a component-ready format id plus raw option bag without regressing existing `bin` or `prg` parsing behavior
  - Expected files:
    - `crates/opforge-asm/src/asmline_directives_metadata.rs`
    - `crates/opforge-vm/src/output_model.rs`
    - `crates/opforge-asm/src/output.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - add focused parser and stored-metadata tests covering raw format-id preservation, raw option-bag preservation, shared syntax rejection, and duplicate-key rejection that is format-independent
    - run `cargo test -p asm root_metadata_linker_output_`
    - run `cargo fmt --all`
    - run `cargo clippy --workspace --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to parser and model reshaping only
    - the compliance note explicitly states that no output payload behavior change is intended in this commit
  - Commit outcome:
    - `.output` metadata is stored in a component-ready form without yet introducing Hunk emission behavior
  - Definition of done:
    - `LinkerOutputDirective` no longer requires the parser to normalize all format-specific semantics up front
    - the selected format identifier survives parsing as data that can be resolved later against a component registry
    - the raw option bag needed for component-side validation is preserved
    - existing `bin` and `prg` parsing tests still pass

- [x] Work item 2: introduce the internal output-component registry and migrate `bin` and `prg` behind it without behavior drift
  - Source requirement or finding IDs: spec `Goals` for registry plus preserved `bin`/`prg`; spec `Behavioral Contract` component operations; spec `Acceptance Criteria` bullet for component-based resolution
  - Validation: focused component-resolution and legacy linker-output coverage plus the full repo quality gate before commit
  - Definition of done: `bin` and `prg` resolve through the new internal registry with no intentional behavior drift
  - Expected files:
    - `crates/opforge-vm/src/output_artifacts.rs`
    - `crates/opforge-vm/src/output_model.rs`
    - one new internal output-component module under `crates/opforge-vm/src/`
    - `crates/opforge-asm/src/output.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-engine/src/lib.rs`
  - Full quality gates:
    - add focused component-resolution tests for built-in `bin` and `prg`
    - verify all existing linker-output bundle, image, and PRG prefix tests still pass with only wording updates where intentionally changed
    - run `cargo test -p asm linker_output_`
    - run `cargo fmt --all`
    - run `cargo clippy --workspace --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the registry seam and bin/prg migration only
    - the compliance note explicitly states that Hunk payload generation and relocation handling are still deferred
  - Commit outcome:
    - all current linker-output behavior resolves through a shared internal component registry instead of one open-coded payload builder
  - Definition of done:
    - `bin` and `prg` are resolved through the new registry
    - current `bin` image-span, contiguous-bundle, and `prg` load-address behavior is preserved
    - unknown output formats now fail at the registry boundary with supported-format diagnostics

- [x] Work item 3: add the first Hunk component input model and exact byte-writer for the relocation-free subset
  - Source requirement or finding IDs: spec `Invariants / Constraints` Hunk executable structure; spec `Behavioral Contract` Hunk mapping, input-model clauses, and deterministic empty non-BSS handling; spec `Boundary Cases` for BSS, payload padding, allocation-size versus payload-size distinction, and non-code-first rejection; spec `Acceptance Criteria` bullets for exact header words, BSS, payload-size distinction, and section-order handling
  - Validation: byte-level Hunk payload tests, negative option-ordering tests, and the full repo quality gate before commit
  - Definition of done: the repository can emit exact regular-executable Hunk bytes for explicitly relocation-free collected segments and reject unsupported Hunk semantics deterministically
  - Expected files:
    - `crates/opforge-vm/src/output_model.rs`
    - `crates/opforge-vm/src/output_artifacts.rs`
    - one new internal Hunk output module under `crates/opforge-vm/src/`
    - `crates/opforge-asm/src/output.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - add byte-level tests covering a one-segment `HUNK_CODE` executable, `HUNK_BSS`, `HUNK_DATA`, big-endian header words, default `MEMF_ANY`, padded payload longword counts, and allocation-size versus payload-size distinction
    - add negative tests covering rejected `image`, `fill`, `loadaddr`, and `contiguous` options for Hunk output
    - add focused tests covering the chosen v0.1 empty non-BSS behavior, with empty non-BSS sections omitted deterministically rather than emitted as zero-sized non-BSS segments
    - add negative tests covering preserved `sections=` order and rejection when the first emitted segment is not `HUNK_CODE`
    - run `cargo test -p asm linker_output_hunk_`
    - run `cargo fmt --all`
    - run `cargo clippy --workspace --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the Hunk model and byte writer for explicitly relocation-free inputs
    - the compliance note explicitly states that relocation hunks, memory-type customization, and object-style features remain out of scope
  - Commit outcome:
    - the repository can build exact regular-executable Hunk bytes for explicitly relocation-free collected segments and reject unsupported format semantics deterministically
  - Definition of done:
    - the Hunk writer emits `HUNK_HEADER`, zero resident-library count, `segment_count`, `0`, `segment_count - 1`, per-segment table words, `HUNK_CODE`/`HUNK_DATA`/`HUNK_BSS`, and `HUNK_END`
    - section order follows the user-declared `sections=` order rather than base-address sorting
    - the writer distinguishes reserved allocation size from initialized payload length
    - empty non-BSS sections are omitted deterministically in v0.1 and covered by focused tests
    - the first emitted segment must be code and is rejected otherwise

- [x] Work item 4: wire assembler-facing relocation disposition and end-to-end Hunk diagnostics into the live `.output` path
  - Source requirement or finding IDs: spec `Goals` relocation explicitness; spec `Behavioral Contract` relocation-capability clauses; spec `Boundary Cases` absolute-relocation rejection and unplaced-section rejection; spec `Acceptance Criteria` bullets for parser acceptance and relocation-required rejection
  - Validation: focused live-path Hunk tests, legacy linker-output regression coverage, and the full repo quality gate before commit
  - Definition of done: Hunk output is reachable through the normal `.output` path and succeeds only for explicitly relocation-free inputs while all other unsafe cases fail deterministically
  - Expected files:
    - `crates/opforge-asm/src/asmline_directives_metadata.rs`
    - `crates/opforge-asm/src/engine.rs`
    - `crates/opforge-vm/src/output_model.rs`
    - `crates/opforge-vm/src/output_artifacts.rs`
    - the internal Hunk output module from Work item 3
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-engine/src/lib.rs`
  - Full quality gates:
    - add end-to-end `.output ..., format=hunk` tests for pass1 acceptance, component resolution, and deterministic diagnostics when relocation disposition is not explicitly proven free
    - add the narrowest safe positive end-to-end test that proves the explicit relocation-free disposition is produced in `crates/opforge-asm/src/engine.rs`, carried through `crates/opforge-vm/src/output_model.rs`, and consumed by the Hunk component without heuristic inference from bytes-only artifacts
    - add an end-to-end negative test that a selected Hunk section without an assigned base fails with a deterministic diagnostic rather than falling through generic flat-image behavior
    - verify existing non-Hunk linker-output tests still pass unchanged
    - run `cargo test -p asm linker_output_hunk_`
    - run `cargo test -p asm linker_output_`
    - run `cargo fmt --all`
    - run `cargo clippy --workspace --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to live-path wiring and deterministic relocation diagnostics
    - the compliance note explicitly states that unsupported relocation-bearing inputs must fail rather than triggering any silent fallback to flat-image semantics
  - Commit outcome:
    - Hunk output is reachable through the normal `.output` pipeline with explicit relocation-safety gating and stable diagnostics
  - Definition of done:
    - `.output "build/out.hunk", format=hunk, sections=...` is parsed and resolved through the live component registry
    - the explicit relocation-free disposition is produced by `crates/opforge-asm/src/engine.rs`, carried through `crates/opforge-vm/src/output_model.rs`, and consumed by the Hunk component without inferring safety from bytes-only artifacts
    - end-to-end Hunk output succeeds only when the component input explicitly says relocation-free
    - end-to-end Hunk output fails deterministically for unproven or relocation-required inputs
    - selected sections without assigned bases fail deterministically for `format=hunk`
    - existing `bin` and `prg` live output behavior remains intact

- [ ] Work item 5: add opt-in FS-UAE smoke validation without coupling it to the default quality gate
  - Source requirement or finding IDs: spec `Goals` FS-UAE as consumer only; spec `Boundary Cases` FS-UAE-not-installed rule; spec `Acceptance Criteria` bullet for optional external FS-UAE execution
  - Validation: opt-in external smoke validation plus confirmation that the default repo quality gate stays dependency-free before commit
  - Definition of done: an explicit external Hunk smoke path exists, skips cleanly when FS-UAE is absent, and does not widen the default quality gate
  - Expected files:
    - one new opt-in test or harness file under `crates/opforge-asm/src/` or `scripts/`
    - related test registration or command wiring
    - `documentation/opForge-amiga-hunk-output-component-spec-v0_1.md` only if command or harness usage wording must be clarified after implementation
  - Full quality gates:
    - add an opt-in external smoke path that runs only when FS-UAE is configured
    - verify the opt-in path skips cleanly with an explicit reason when FS-UAE is unavailable
    - verify the default workspace test path remains green without FS-UAE
    - run the focused opt-in Hunk smoke validation when the environment is configured
    - run `cargo fmt --all`
    - run `cargo clippy --workspace --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to optional external validation wiring
    - the compliance note explicitly states that FS-UAE is not added to the default required toolchain and that the generic output writer remains emulator-agnostic
  - Commit outcome:
    - the repository has an explicit opt-in Hunk executable smoke path without affecting the default quality gate
  - Definition of done:
    - an explicit external validation entry point exists for generated Hunk executables
    - the path is skipped or gated cleanly when FS-UAE is not installed
    - default local and CI validation remains dependency-free with respect to FS-UAE

## Milestones

- [x] Milestone 1: the `.output` model and internal component registry are landed without changing `bin` or `prg` behavior (`Work item 1` and `Work item 2`)
- [x] Milestone 2: exact v0.1 Hunk byte generation and live-path relocation-safety gating are landed (`Work item 3` and `Work item 4`)
- [ ] Milestone 3: optional external FS-UAE smoke validation is available without widening the default quality gate (`Work item 5`)

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- do not widen the relocation slice into relocation-hunk emission, object-file writing, memory-type customization, overlay support, or loader-debug features
- do not re-sort Hunk segments by base address; preserve the user-declared `sections=` order
- do not allow any fallback that emits Hunk output when relocation safety is unknown