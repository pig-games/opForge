# opForge Code-Reduction Follow-Up Cleanup Plan v0.1

## Metadata

- Source: `User request on 2026-04-12 plus the hotspot register below, derived from post-remediation LOC analysis of origin/main..HEAD`
- Mode: `cleanup-only`
- Owner: GitHub Copilot

## Objective

Reduce the highest-confidence follow-up boilerplate that remained after the
April 11 remediation series without collapsing the extracted seams back into
monoliths. Execution must land as narrow, behavior-preserving cleanup commits
that lower control-plane and codec LOC while keeping package bytes, runtime
bootstrap behavior, FFI projection results, and M68k encoder behavior stable.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- This is a cleanup-only follow-up plan explicitly authorized by the user on
  2026-04-12; scope is limited to post-remediation boilerplate reduction in the
  package codec, assembler runtime bootstrap plumbing, FFI portable projections,
  and top-level M68k dispatch.
- No public behavior changes are allowed in package encoding, diagnostic text,
  runtime artifact fallback behavior, FFI ABI or report layout, or M68k opcode
  emission.
- Do not collapse extracted M68k helper modules back into `m68k/handler.rs`.
- One active work item at a time.
- Each work item or phase must end in exactly one new commit before the next
  item begins.
- Full quality gates are mandatory before each commit.
- `plan-compliance-reviewer` must pass before each plan-driven commit.
- No existing review finding is marked fixed or reopened as part of this
  cleanup-only plan unless a later execution slice explicitly adds a closure
  artifact and passes the required closure gate.
- Characterization tests are allowed only where they directly lock the active
  cleanup seam.

## Planning Decisions Captured Up Front

- The package codec lands first because it has the clearest remaining generic
  seam and the largest low-risk LOC reduction outside the M68k handler family.
- The `TOKS` chunk stays separate from the simpler descriptor chunks because its
  legacy-extension and defaulting behavior carries materially higher drift risk.
- The assembler runtime-bootstrap wrapper lands before M68k dispatcher cleanup
  so the plan banks a small, low-risk control-plane reduction before touching
  the highest-change-surface dispatch function.
- FFI portable projection cleanup stays limited to internal adapter factoring;
  it must not widen into ABI changes or public report-shape changes.
- M68k dispatcher cleanup is constrained to orchestration boilerplate in
  `m68k/handler.rs`; extracted submodule instruction logic is not reworked in
  this plan.
- The LSP test-client wait-loop duplication is intentionally deferred because
  its payoff is smaller than the four cleanup targets above.

## Source Hotspots

- `HS-2026-04-12-001`: simple package chunk boilerplate in
  `crates/opforge-package/src/package/codec.rs` at `encode_fams_chunk`,
  `decode_fams_chunk`, `encode_meta_chunk`, `decode_meta_chunk`,
  `encode_strs_chunk`, `decode_strs_chunk`, `encode_diag_chunk`,
  `decode_diag_chunk`, `encode_cpus_chunk`, `decode_cpus_chunk`,
  `encode_dial_chunk`, and `decode_dial_chunk`
- `HS-2026-04-12-002`: token-policy chunk boilerplate in
  `crates/opforge-package/src/package/codec.rs` at `encode_toks_chunk` and
  `decode_toks_chunk`
- `HS-2026-04-12-003`: assembler runtime-wrapper branching in
  `crates/opforge-asm/src/runtime_model.rs` overlapping the shared helpers in
  `crates/opforge-vm/src/runtime_bootstrap.rs`
- `HS-2026-04-12-004`: repeated portable token and expression projection
  helpers in `crates/opforge-ffi/src/portable_adapter.rs`
- `HS-2026-04-12-005`: fixed-instruction and unsupported-placeholder
  boilerplate in `crates/opforge-families/src/m68k/handler.rs` inside
  `encode_instruction_impl`
- `HS-2026-04-12-006`: remaining direct submodule delegation boilerplate in
  `crates/opforge-families/src/m68k/handler.rs` inside `encode_instruction_impl`

## Work Items

- [x] Work item 1: migrate the remaining simple package chunk codecs onto a declarative helper seam
  - Source requirement or finding IDs: `HS-2026-04-12-001`
  - Validation: focused simple-chunk round trips plus full quality gates
  - Definition of done:
    - `FAMS`, `META`, `STRS`, `DIAG`, `CPUS`, and `DIAL` no longer rely on
      bespoke mirrored encode and decode loops in `codec.rs`
    - one shared helper or trait definition owns field order, bounded-count
      handling, and decode reconstruction for those chunk families
    - metadata snapshots, TOC layout, and legacy-container behavior stay stable
  - Validation details:
    - add focused `simple_chunk_schema_` round-trip coverage for the moved chunk
      families and preserve the existing snapshot checks
    - run `cargo test -p package simple_chunk_schema_ -- --nocapture`
    - run `cargo test -p package metadata_snapshot_is_stable -- --nocapture`
    - run `cargo test -p package toc_snapshot_is_stable -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-package/src/package/codec.rs`
    - `crates/opforge-package/src/package/codec/scoped_schema.rs`
    - `crates/opforge-package/src/package/tests.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the six
      simple chunk families, shared helper wiring, and direct round-trip or
      snapshot validation
  - Commit outcome:
    - the simplest remaining package chunk codecs stop duplicating mirrored
      encode or decode control flow, and the more complex `TOKS` slice can land
      next without mixing concerns

- [x] Work item 2: migrate the `TOKS` chunk onto the same declarative codec family
  - Source requirement or finding IDs: `HS-2026-04-12-002`
  - Validation: focused token-policy compatibility tests plus full quality gates
  - Definition of done:
    - `TOKS` no longer keeps a bespoke local encode and decode implementation in
      `codec.rs`
    - owner encoding, lexical defaulting, extension-marker parsing, ASCII
      escape validation, and multi-operator list handling all flow through one
      shared declarative seam
    - legacy `TOKS` payload compatibility and current validation failures remain
      behaviorally stable
  - Validation details:
    - preserve and extend focused token-policy coverage for round-trip,
      legacy-defaulting, and invalid-case-rule rejection behavior
    - run `cargo test -p package encode_decode_round_trip_preserves_toks_policy -- --nocapture`
    - run `cargo test -p package decode_legacy_toks_entries_default_extended_fields -- --nocapture`
    - run `cargo test -p package decode_rejects_invalid_toks_case_rule -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-package/src/package/codec.rs`
    - `crates/opforge-package/src/package/codec/scoped_schema.rs`
    - `crates/opforge-package/src/package/tests.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to `TOKS`
      codec consolidation, legacy-compatibility locking tests, and no unrelated
      chunk-family movement
  - Commit outcome:
    - the final remaining hand-written package chunk codec joins the declarative
      family without widening into unrelated package canonicalization work

- [x] Work item 3: collapse the assembler runtime-model wrapper onto the shared bootstrap seam
  - Source requirement or finding IDs: `HS-2026-04-12-003`
  - Validation: focused bootstrap-path parity tests plus full quality gates
  - Definition of done:
    - `crates/opforge-asm/src/runtime_model.rs` no longer duplicates the shared
      artifact-path and fallback-routing decision tree that already exists in
      the VM bootstrap helpers
    - any remaining assembler-side wrapper is thinner and no longer owns the
      branching policy itself
    - artifact loading, fallback package-bytes behavior, and no-host-pipeline
      outcomes remain stable under the current feature matrix
  - Validation details:
    - add focused `runtime_model_` parity coverage for artifact-path,
      fallback-package, no-host-pipeline, and unbundled no-runtime-model
      branches across the current feature gates
    - run `cargo test -p asm runtime_model_ -- --nocapture`
    - run `cargo test -p asm --features vm-runtime-opasm-artifact runtime_model_ -- --nocapture`
    - run `cargo test -p asm --features vm-runtime-only runtime_model_ -- --nocapture`
    - run `cargo test -p asm --features "vm-runtime-only vm-runtime-opasm-artifact" runtime_model_ -- --nocapture`
    - run `cargo test -p asm --features vm-runtime-opasm-unbundled runtime_model_ -- --nocapture`
    - run `cargo test -p asm --features "vm-runtime-only vm-runtime-opasm-unbundled" runtime_model_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-asm/src/runtime_model.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-vm/src/runtime_bootstrap.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to shared
      bootstrap routing, assembler wrapper simplification, and the exact
      feature-lane parity tests named above for the affected branches
  - Commit outcome:
    - the assembler-side runtime model becomes a thinner control-plane wrapper
      around the already-shared bootstrap seam instead of a second branching
      implementation

- [x] Work item 4: reduce internal FFI portable-projection boilerplate without changing the ABI surface
  - Source requirement or finding IDs: `HS-2026-04-12-004`
  - Validation: focused FFI projection parity plus full quality gates
  - Definition of done:
    - the internal portable-token and portable-expression projection helpers no
      longer repeat avoidable parallel variant handling across multiple local
      helper functions
    - token-kind, token-text, expression span, node kind, node text, display
      text, and child traversal remain behaviorally stable through the public
      FFI surface
    - no public C ABI, report layout, or enum numeric values change
  - Validation details:
    - keep the adapter-backed parity tests focused on token and expression group
      projections through the FFI boundary
    - run `cargo test -p ffi ffi_opforge_opcore_tokenize_group_ -- --nocapture`
    - run `cargo test -p ffi ffi_opforge_opcore_expr_group_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-ffi/src/lib.rs`
    - `crates/opforge-ffi/src/portable_adapter.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to internal
      projection-helper reduction plus existing FFI parity validation
  - Commit outcome:
    - the FFI portable adapter keeps one stable external behavior path while its
      internal variant-projection boilerplate is reduced

- [x] Work item 5: compress fixed-instruction and unsupported-placeholder boilerplate in the M68k dispatcher
  - Source requirement or finding IDs: `HS-2026-04-12-005`
  - Validation: focused fixed-routing dispatch characterization plus full quality gates
  - Definition of done:
    - `m68k/handler.rs` reduces the fixed-instruction and unsupported-placeholder
      control-plane boilerplate inside `encode_instruction_impl`
    - the slice stays local to `handler.rs` unless one tiny helper adjacent to
      that dispatcher is strictly required
    - fixed-instruction routing and selected unsupported-mnemonic `NotFound`
      boundaries remain stable
  - Validation details:
    - add focused `m68k_dispatch_fixed_` characterization coverage for fixed
      instructions and selected unsupported mnemonics that must continue
      returning `NotFound`
    - run `cargo test -p families m68k_dispatch_fixed_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-families/src/m68k/handler.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to
      fixed-instruction or unsupported-case consolidation plus direct dispatch
      characterization tests
  - Commit outcome:
    - the top-level M68k orchestrator loses one bounded class of dispatch
      repetition without touching the extracted encoder seams

- [x] Work item 6: compress the remaining direct submodule delegation boilerplate in the M68k dispatcher
  - Source requirement or finding IDs: `HS-2026-04-12-006`
  - Validation: focused delegation dispatch characterization plus full quality gates
  - Definition of done:
    - `m68k/handler.rs` reduces only the remaining direct delegation boilerplate
      inside `encode_instruction_impl` after Work item 5
    - extracted instruction-group modules remain separate and are changed only if
      a narrow signature adjustment is strictly required to support the local
      delegation cleanup
    - representative delegated instruction paths, branch or `DBcc` routing, and
      long-divide gating remain stable
  - Validation details:
    - add focused `m68k_dispatch_delegate_` characterization coverage for
      representative delegated encoder paths and the dispatcher predicates that
      still gate branch, `DBcc`, and long-divide routing
    - run `cargo test -p families m68k_dispatch_delegate_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-families/src/m68k/handler.rs`
    - `crates/opforge-families/src/m68k.rs`
    - one small helper module under `crates/opforge-families/src/m68k/handler/` only if the delegation cleanup cannot stay local to `handler.rs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to
      delegation cleanup, any necessary tiny signature adjustment, and direct
      dispatch characterization tests
  - Commit outcome:
    - the top-level M68k orchestrator loses the remaining delegation-oriented
      repetition without reopening the extracted handler split

## Milestones

- [x] Milestone 1: the remaining package codec boilerplate is consolidated onto
  declarative helpers (`Work item 1` and `Work item 2`)
- [x] Milestone 2: shared utility wrappers are reduced without semantic drift
  (`Work item 3` and `Work item 4`)
- [x] Milestone 3: the top-level M68k dispatch layer becomes slimmer while the
  extracted handler split stays intact (`Work item 5` and `Work item 6`)

## To Be Planned Later

- LSP test-client wait-loop reduction if post-execution LOC metrics still show
  meaningful duplication after the higher-value cleanup items land
- deeper portable-schema code generation only if Work item 4 still leaves a
  meaningful amount of parallel variant boilerplate in the FFI adapter

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping