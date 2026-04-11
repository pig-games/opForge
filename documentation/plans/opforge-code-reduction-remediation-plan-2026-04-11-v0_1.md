# opForge Code-Reduction Remediation Plan v0.1

## Metadata

- Source: `documentation/reviews/opforge_code_reduction_review_2026-04-10.md`
- Mode: `remediation`
- Owner: GitHub Copilot

## Objective

Reduce the highest-value duplication and oversized-module hotspots identified in
the April 10 code-reduction review without changing behavior. Execution must
land as narrow, reviewable commits that preserve current diagnostics, runtime
bootstrap behavior, package codec stability, and LSP framing semantics while
making the affected seams easier to maintain and reduce further later.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Scope is limited to findings `RVW-2026-04-11-001` through
  `RVW-2026-04-11-007` from the source review.
- One active work item at a time.
- Each work item or phase must end in exactly one new commit before the next
  item begins.
- Full quality gates are mandatory before each commit.
- `plan-compliance-reviewer` must pass before each plan-driven commit.
- When a work item is expected to fully close a review finding, that finding is
  not marked fixed until a closure artifact exists and `finding-closure-reviewer`
  passes for the same commit.
- Do not widen scope into new ISA support, new FPU implementation, public C ABI
  changes, or unrelated crate cleanup.
- Characterization tests are allowed only where they directly lock the seam for
  the active reduction slice.

## Planning Decisions Captured Up Front

- The M68k reductions land first because `RVW-2026-04-11-005` and
  `RVW-2026-04-11-001` are the strongest change-surface multipliers in the
  review.
- The M68k monolith split is phased into wrapper-policy deduplication,
  compatibility extraction, operand parsing extraction, effective-address
  extraction, move or control-flow encoder extraction, arithmetic or branch
  encoder extraction, and specialized control-register or later-family encoder
  extraction so each commit stays reviewable.
- Cross-crate runtime bootstrap consolidation will prefer a shared module in an
  already-common dependency path rather than introducing a new top-level crate
  unless dependency direction makes that unavoidable.
- Each work item must prove its local behavior with focused tests before the
  always-required repo-wide quality gates run.
- Existing public outputs, diagnostics, package bytes, and protocol framing stay
  behaviorally stable unless the active item explicitly centralizes equivalent
  formatting from a shared helper.

## Work Items

- [x] Work item 1: centralize M68k FPU capability validation across CPU wrappers
  - Source requirement or finding IDs: `RVW-2026-04-11-001`
  - Finding closure expectation: fully closes `RVW-2026-04-11-001`
  - Validation: focused cross-CPU `.fpu` characterization plus full quality gates
  - Definition of done:
    - one shared M68k helper owns target-name lookup, legal-target validation,
      and deferred diagnostic formatting for wrapper-level FPU gating
    - `m68020`, `m68030`, `m68040`, and `m68080` handlers delegate to that
      helper instead of carrying local copies of the same policy code
    - wrapper diagnostics remain byte-for-byte stable for currently covered
      accepted and rejected cases
    - a closure artifact can be written for `RVW-2026-04-11-001` without
      claiming any progress on the larger `M68KFamilyHandler` split
  - Validation details:
    - add a table-driven cross-CPU characterization suite that locks legal `.fpu`
      target matrices and deferred "recognized but not yet implemented"
      diagnostics for `m68020`, `m68030`, `m68040`, and `m68080`
    - run `cargo test -p families fpu_mnemonics_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-families/src/m68020/handler.rs`
    - `crates/opforge-families/src/m68030/handler.rs`
    - `crates/opforge-families/src/m68040/handler.rs`
    - `crates/opforge-families/src/m68080/handler.rs`
    - new shared helper module under `crates/opforge-families/src/m68k/`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to wrapper
      policy deduplication, shared diagnostics, and direct characterization tests
  - Commit outcome:
    - M68k wrapper-only FPU policy duplication is removed in one small commit,
      and execution can move to the monolithic-family split next

- [x] Work item 2: extract M68k compatibility validation from the monolithic family handler
  - Source requirement or finding IDs: `RVW-2026-04-11-005`
  - Finding closure expectation: partially closes `RVW-2026-04-11-005`
  - Validation: focused `68080` compatibility seam tests plus full quality gates
  - Definition of done:
    - register-compatibility collection and validation logic no longer live in
      the giant `m68k/handler.rs` implementation body
    - `M68KFamilyHandler` delegates compatibility checks through a stable helper
      boundary without changing current diagnostics
    - no operand parsing or instruction encoding logic is moved in this commit
  - Validation details:
    - add focused tests for `68080`-only register detection across nested
      operand shapes so compatibility scanning is locked before further moves
    - run `cargo test -p families 68080_register -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-families/src/m68k/handler.rs`
    - `crates/opforge-families/src/m68k.rs`
    - new compatibility or validation module under `crates/opforge-families/src/m68k/`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to
      compatibility-validation extraction and seam-locking tests only
  - Commit outcome:
    - the first structural seam is carved out of `M68KFamilyHandler` without
      widening into operand or opcode movement

- [x] Work item 3: extract M68k operand parsing and normalization helpers
  - Source requirement or finding IDs: `RVW-2026-04-11-005`
  - Finding closure expectation: partially closes `RVW-2026-04-11-005`
  - Validation: focused operand-parsing seam tests plus full quality gates
  - Definition of done:
    - operand parsing and normalization helpers move behind a dedicated module
      boundary instead of staying embedded in `m68k/handler.rs`
    - `M68KFamilyHandler` retains orchestration responsibility only for the
      active operand-parsing path
    - existing operand spans and current parse-time error behavior remain stable
  - Validation details:
    - add focused tests covering data, address, control, special, FPU,
      register-list, and full-extension operand parsing paths that move in this
      slice
    - run `cargo test -p families m68k_operand -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-families/src/m68k/handler.rs`
    - `crates/opforge-families/src/m68k/operand.rs`
    - `crates/opforge-families/src/m68k.rs`
    - new operand-parsing module under `crates/opforge-families/src/m68k/`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to operand
      parsing or normalization extraction plus direct seam tests
  - Commit outcome:
    - operand parsing is no longer part of the monolithic handler body, and the
      next slice can target effective-address encoding without mixing concerns

- [ ] Work item 4: extract M68k effective-address encoding into a dedicated seam
  - Source requirement or finding IDs: `RVW-2026-04-11-005`
  - Finding closure expectation: partially closes `RVW-2026-04-11-005`
  - Validation: representative effective-address characterization plus full quality gates
  - Definition of done:
    - effective-address encoding and supporting data structures live in a
      dedicated helper module rather than remaining embedded in the core handler
    - `M68KFamilyHandler` invokes that helper through a stable interface
    - no instruction-dispatch grouping is changed in this commit beyond the
      minimum needed to route through the extracted encoder seam
  - Validation details:
    - add characterization tests for representative encoded effective-address
      bit patterns and extension bytes across data, address, PC-relative,
      absolute, immediate, and full-extension forms
    - run `cargo test -p families effective_address -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-families/src/m68k/handler.rs`
    - `crates/opforge-families/src/m68k.rs`
    - new effective-address module under `crates/opforge-families/src/m68k/`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to
      effective-address extraction and representative encode-path tests
  - Commit outcome:
    - the core addressing encoder becomes an isolated reduction seam and the
      remaining monolith can be cut by instruction-group boundaries next

- [ ] Work item 5: extract M68k move, transfer, and control-flow encoder groups
  - Source requirement or finding IDs: `RVW-2026-04-11-005`
  - Finding closure expectation: partially closes `RVW-2026-04-11-005`
  - Validation: focused move/control-flow characterization plus full quality gates
  - Definition of done:
    - encoder logic for `MOVE`, `MOVEA`, `MOVEP`, `LEA`, `PEA`, `JMP`, `JSR`,
      `LINK`, `UNLK`, `EXG`, `SWAP`, `EXT`, `TRAP`, `STOP`, and fixed system
      instructions moves into stable instruction-group submodules under
      `crates/opforge-families/src/m68k/`
    - `M68KFamilyHandler` delegates those paths through stable module
      boundaries without changing current diagnostics or encoded bytes
    - arithmetic, branch, control-register, `MOVEM`, bitfield, and later-family
      encoder logic is not moved in this commit
  - Validation details:
    - add focused characterization coverage for move/control-flow, system, and
      register-transfer paths moved in this slice
    - run `cargo test -p families m68k_move_control_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-families/src/m68k/handler.rs`
    - `crates/opforge-families/src/m68k.rs`
    - new move/control-flow instruction-group modules under `crates/opforge-families/src/m68k/`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to
      move/control-flow group extraction, dispatcher rewiring, and seam tests
      only
  - Commit outcome:
    - the first instruction-group seam lands as one bounded commit without
      mixing in arithmetic or later-family work

- [ ] Work item 6: extract M68k arithmetic, unary, quick, and branch encoder groups
  - Source requirement or finding IDs: `RVW-2026-04-11-005`
  - Finding closure expectation: partially closes `RVW-2026-04-11-005`
  - Validation: focused arithmetic/branch characterization plus full quality gates
  - Definition of done:
    - encoder logic for integer arithmetic, compare, logic, immediate, unary,
      quick, shift, condition-code, and branch families moves into stable
      instruction-group submodules under `crates/opforge-families/src/m68k/`
    - `M68KFamilyHandler` delegates those paths through stable module
      boundaries without changing current diagnostics or encoded bytes
    - control-register, `MOVEM`, bitfield, later-family, and deferred FPU paths
      remain in `m68k/handler.rs` for the next slice
  - Validation details:
    - add focused characterization coverage for representative arithmetic,
      immediate, shift, condition-code, and branch paths moved in this slice
    - run `cargo test -p families m68k_alu_branch_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-families/src/m68k/handler.rs`
    - `crates/opforge-families/src/m68k.rs`
    - new arithmetic/branch instruction-group modules under `crates/opforge-families/src/m68k/`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to
      arithmetic/branch group extraction, dispatcher rewiring, and seam tests
      only
  - Commit outcome:
    - core integer encoder groups are isolated, leaving only specialized
      control-register, `MOVEM`, bitfield, and later-family paths for the final
      orchestration slice

- [ ] Work item 7: extract M68k control-register, MOVEM, bitfield, and later-family groups and reduce the family handler to orchestration
  - Source requirement or finding IDs: `RVW-2026-04-11-005`
  - Finding closure expectation: fully closes `RVW-2026-04-11-005`
  - Validation: specialized-group characterization plus full quality gates
  - Definition of done:
    - encoder logic for control-register transfers or immediates, `MOVEM` and
      `FMOVEM`, bitfield instructions, `68020` long multiply/divide helpers,
      `68080` extended short-branch handling, and remaining deferred FPU
      dispatch glue lives in dedicated instruction-group modules under
      `crates/opforge-families/src/m68k/`
    - `M68KFamilyHandler` becomes an orchestration layer for parse, validation,
      effective-address services, and submodule dispatch rather than an
      all-in-one instruction encoder
    - no new instruction support or FPU encoding implementation is introduced as
      part of the split
    - a closure artifact can be written for `RVW-2026-04-11-005` because the
      mixed-responsibility structural blocker has been reduced to bounded seams
  - Validation details:
    - add focused characterization coverage for control-register, `MOVEM`,
      bitfield, and later-family dispatch paths moved in this slice
    - run `cargo test -p families m68k_specialized_groups_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-families/src/m68k/handler.rs`
    - `crates/opforge-families/src/m68k.rs`
    - new specialized instruction-group modules under `crates/opforge-families/src/m68k/`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to
      control-register, `MOVEM`, bitfield, and later-family extraction plus
      final dispatcher rewiring and seam tests only
  - Commit outcome:
    - the M68k family core is split into bounded submodules and the largest
      review-surface hotspot is retired in one closure-ready commit

- [ ] Work item 8: consolidate runtime-model bootstrap policy into one shared module
  - Source requirement or finding IDs: `RVW-2026-04-11-007`
  - Finding closure expectation: fully closes `RVW-2026-04-11-007`
  - Validation: cross-crate runtime-bootstrap parity tests plus full quality gates
  - Definition of done:
    - one shared bootstrap helper in a common dependency path owns runtime-model
      artifact path resolution, package-byte fallback, artifact loading, and
      artifact persistence policy used by both `asm` and `engine`
    - existing feature-gated behavior remains intact across editor-side and
      assembler-side callers
    - the shared helper is proven with parity tests rather than duplicated local
      tests that can drift apart later
  - Validation details:
    - add cross-crate parity tests for artifact-path resolution, load-from-path
      behavior, package-byte fallback, and artifact persistence under the
      feature combinations already covered by `asm` and `engine`
    - run `cargo test -p asm --features vm-runtime-opasm-artifact vm_runtime_artifact_ -- --nocapture`
    - run `cargo test -p engine --features vm-runtime-only,vm-runtime-opasm-artifact runtime_model -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-asm/src/runtime_model.rs`
    - `crates/opforge-engine/src/lib.rs`
    - tests in `crates/opforge-asm/src/tests.rs`
    - tests in `crates/opforge-engine/src/lib.rs`
    - new shared runtime-bootstrap module in a crate already depended on by both
      `asm` and `engine`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to runtime
      bootstrap consolidation and explicit cross-crate parity validation
  - Commit outcome:
    - editor-side and assembler-side runtime-model bootstrap behavior shares one
      implementation and one parity contract

- [ ] Work item 9: replace duplicated pass-specific repetition walkers with one traversal driver
  - Source requirement or finding IDs: `RVW-2026-04-11-002`
  - Finding closure expectation: fully closes `RVW-2026-04-11-002`
  - Validation: focused repetition-driver parity tests plus full quality gates
  - Definition of done:
    - one traversal driver owns directive matching, nesting, and iteration
      bookkeeping for repetition constructs
    - pass-specific hooks are limited to regular-line execution, listing output,
      and pass-consistency checks
    - current repetition diagnostics and line-processing behavior remain stable
  - Validation details:
    - add focused repetition tests covering `.for`, `.while`, nested matching,
      conditional skips, scoped-label restrictions, loop bookkeeping, and pass
      parity for the shared traversal driver
    - run `cargo test -p asm repetition_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-asm/src/engine.rs`
    - `crates/opforge-asm/src/tests.rs`
    - optional new helper module under `crates/opforge-asm/src/`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to
      repetition traversal deduplication and focused behavior-locking tests
  - Commit outcome:
    - complex repetition semantics are implemented once and both passes delegate
      through the same traversal contract

- [ ] Work item 10: add a portable-schema adapter layer inside the FFI crate
  - Source requirement or finding IDs: `RVW-2026-04-11-006`
  - Finding closure expectation: fully closes `RVW-2026-04-11-006`
  - Validation: FFI portable-schema parity tests plus full quality gates
  - Definition of done:
    - the FFI crate owns one shared adapter for portable token and expression
      surface mapping instead of several hand-maintained mirrors
    - `push_expr`, token or expression text reconstruction, and token-kind
      remapping route through that shared adapter
    - the public C ABI and exported report layout remain unchanged
  - Validation details:
    - add parity tests that fail if portable expression node kind, display text,
      child enumeration, or token-kind mapping diverge across the FFI boundary
    - run `cargo test -p ffi ffi_opforge_opcore_expr_group_ -- --nocapture`
    - run `cargo test -p ffi ffi_opforge_opcore_tokenize_group_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-ffi/src/lib.rs`
    - new portable-schema adapter module under `crates/opforge-ffi/src/`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to internal
      adapter extraction and FFI parity validation only
  - Commit outcome:
    - the public FFI boundary keeps the same behavior while collapsing the
      portable-schema mirror logic behind one source of truth

- [ ] Work item 11: introduce shared package-codec schema helpers for register, form, table, and selector chunks
  - Source requirement or finding IDs: `RVW-2026-04-11-003`
  - Finding closure expectation: partially closes `RVW-2026-04-11-003`
  - Validation: first migrated codec-schema contract tests plus full quality gates
  - Definition of done:
    - a shared schema-helper layer exists in the package codec path and owns
      field order for `ScopedRegisterDescriptor`, `ScopedFormDescriptor`,
      `VmProgramDescriptor`, and `ModeSelectorDescriptor`
    - those four encode/decode paths use the helper without changing bytes, and
      stable snapshot tests still prove deterministic encoding and TOC layout
    - the remaining repeated families targeted by the review,
      `TokenizerVmProgramDescriptor`, `ParserContractDescriptor`,
      `ParserVmProgramDescriptor`, `ExprContractDescriptor`, and
      `ExprParserContractDescriptor`, are explicitly deferred to the next work
      item rather than mixed into this commit
  - Validation details:
    - migrate exactly `ScopedRegisterDescriptor`, `ScopedFormDescriptor`,
      `VmProgramDescriptor`, and `ModeSelectorDescriptor` onto shared schema
      helpers and add generic contract tests over that exact set
    - run `cargo test -p package encode_decode_round_trip_scoped_schema_ -- --nocapture`
    - run `cargo test -p package metadata_snapshot_is_stable -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-package/src/package/codec.rs`
    - `crates/opforge-package/src/package/tests.rs`
    - optional new schema-helper module under `crates/opforge-package/src/package/`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to schema
      helper introduction plus the named register/form/table/selector chunk
      group only
  - Commit outcome:
    - package-codec reduction has a live shared seam over the low-risk
      register/form/table/selector chunk families only

- [ ] Work item 12: migrate tokenizer and contract package chunk codecs onto the shared schema layer
  - Source requirement or finding IDs: `RVW-2026-04-11-003`
  - Finding closure expectation: fully closes `RVW-2026-04-11-003`
  - Validation: full codec-schema migration contract tests plus full quality gates
  - Definition of done:
    - the remaining repeated chunk families targeted by the review,
      `TokenizerVmProgramDescriptor`, `ParserContractDescriptor`,
      `ParserVmProgramDescriptor`, `ExprContractDescriptor`, and
      `ExprParserContractDescriptor`, declare field order once through the
      shared schema helper layer
    - serializer and deserializer paths share the same declared field order for
      each of those named chunk families
    - a closure artifact can be written for `RVW-2026-04-11-003` because the
      duplicated encode or decode boilerplate is no longer the maintenance model
  - Validation details:
    - migrate exactly `TokenizerVmProgramDescriptor`,
      `ParserContractDescriptor`, `ParserVmProgramDescriptor`,
      `ExprContractDescriptor`, and `ExprParserContractDescriptor`, and extend a
      generic round-trip contract to iterate those named families through the
      same schema-driven encode and decode flow
    - run `cargo test -p package encode_decode_round_trip_contract_schema_ -- --nocapture`
    - run `cargo test -p package decode_ -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-package/src/package/codec.rs`
    - `crates/opforge-package/src/package/tests.rs`
    - shared schema-helper module under `crates/opforge-package/src/package/`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to finishing
      the named tokenizer/contract chunk migration and generic round-trip
      validation
  - Commit outcome:
    - the package codec uses one schema-driven reduction path instead of manual
      mirrored encode or decode blocks across many chunk types

- [ ] Work item 13: share one LSP framing implementation between production and the integration client
  - Source requirement or finding IDs: `RVW-2026-04-11-004`
  - Finding closure expectation: fully closes `RVW-2026-04-11-004`
  - Validation: shared framing-path tests plus full quality gates
  - Definition of done:
    - one shared LSP frame helper is used by both `crates/opforge-lsp/src/protocol.rs`
      and `crates/opforge-lsp/tests/common/lsp_client.rs`
    - production framing tests stay green and the integration client no longer
      validates a stale local copy of the framing rules
    - a closure artifact can be written for `RVW-2026-04-11-004` because the
      duplicated framing logic is removed at the exact protocol seam
  - Validation details:
    - extract one frame reader or writer module and add tests that prove both
      the stdio server path and the integration test client use the same size
      cap, header parsing, JSON decoding, and frame writing behavior
    - run `cargo test -p lsp protocol -- --nocapture`
    - run `cargo test -p lsp -- --nocapture`
    - run `cargo fmt --all`
    - run `cargo clippy --all-targets --all-features -- -D warnings`
    - run `cargo audit`
    - run `cargo test --workspace`
  - Expected files:
    - `crates/opforge-lsp/src/protocol.rs`
    - `crates/opforge-lsp/tests/common/lsp_client.rs`
    - new shared framing module under `crates/opforge-lsp/src/`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to shared
      framing extraction and production-plus-client framing validation
  - Commit outcome:
    - LSP framing logic is implemented once and both the server and the test
      client rely on the same code path

## Milestones

- [ ] Milestone 1: M68k wrapper-policy duplication is removed and the structural
  split foundation is in place (`Work item 1` through `Work item 4`)
- [ ] Milestone 2: the `M68KFamilyHandler` monolith is reduced to orchestration
  and is closure-ready (`Work item 5` through `Work item 7`)
- [ ] Milestone 3: runtime bootstrap and repetition traversal duplication are
  removed from the assembler and engine path (`Work item 8` and `Work item 9`)
- [ ] Milestone 4: the FFI portable-schema mirror and package codec boilerplate
  are centralized behind shared adapters (`Work item 10` through `Work item 12`)
- [ ] Milestone 5: production and test-client LSP framing share one
  implementation (`Work item 13`)

## To Be Planned Later

- larger reduction work outside the cited findings, including possible later
  consolidation in `crates/opforge-lib/src/lib.rs`, `crates/opforge-cli-core/src/cli.rs`,
  and other residual large surfaces named in the source review
- any new Motorola FPU encoding implementation work beyond wrapper-policy
  centralization and structural splitting
- public API redesign, ABI changes, or output-format changes that are not
  required to remove the identified duplication seams

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- when a work item fully closes a finding, no checkbox completion and no
  finding-fixed claim before a closure artifact exists and
  `finding-closure-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- do not widen this remediation plan into unrelated feature work or general
  cleanup outside `RVW-2026-04-11-001` through `RVW-2026-04-11-007`